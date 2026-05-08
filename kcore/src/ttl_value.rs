use std::clone::Clone;
use std::future::Future;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::sync::RwLock;
use std::time::Duration;
use std::time::Instant;

use async_singleflight::Group;

use anyhow::anyhow;
use anyhow::Result as AResult;

fn calc_expiry(base: Instant, duration: Duration) -> u64 {
    (Instant::now() + duration)
        .saturating_duration_since(base)
        .as_secs()
}

#[derive(Clone)]
pub(crate) struct TtlValue<V: Clone> {
    base: Instant,
    inner: Arc<TtlInner<V>>,
}

struct TtlInner<V: Clone> {
    expiry: AtomicU64,
    value: RwLock<V>,
    group: Group<String, V, anyhow::Error>,
}

impl<V: Clone + Send + Sync> TtlValue<V> {
    pub(crate) fn new(v: V, duration: Duration) -> Self {
        let base = Instant::now();
        let expiry = calc_expiry(base, duration);

        Self {
            base,
            inner: Arc::new(TtlInner {
                expiry: AtomicU64::new(expiry),
                value: RwLock::new(v),
                group: Group::new(),
            }),
        }
    }

    fn is_expired(&self) -> bool {
        self.inner.expiry.load(Ordering::Acquire) <= calc_expiry(self.base, Duration::ZERO)
    }

    pub(crate) async fn get_or_insert(
        &self,
        duration: Duration,
        with: impl Future<Output = AResult<V>> + Send,
    ) -> AResult<V> {
        if !self.is_expired() {
            return Ok(self.inner.value.read().expect("Poisoned").clone());
        }

        self.inner
            .group
            .work("key", async {
                if !self.is_expired() {
                    return Ok(self.inner.value.read().expect("Poisoned").clone());
                }
                let val = with.await?;
                {
                    *self.inner.value.write().expect("Poisoned") = val.clone();
                }
                self.inner
                    .expiry
                    .store(calc_expiry(self.base, duration), Ordering::Release);
                Ok(val)
            })
            .await
            .map_err(|e| e.unwrap_or_else(|| anyhow!("Leader Failed")))
    }
}
