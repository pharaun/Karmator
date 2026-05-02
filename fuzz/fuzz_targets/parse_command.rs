#![no_main]
use arbitrary::{Arbitrary, Unstructured};
use libfuzzer_sys::fuzz_target;

extern crate kcore;

// Dummy struct to get a string
#[derive(Arbitrary)]
struct Dummy {
    dat: String,
}

fuzz_target!(|data: &[u8]| {
    let mut unstructured = Unstructured::new(data);

    if let Ok(value) = Dummy::arbitrary(&mut unstructured) {
        let cmd = value.dat;

        // TODO: for now focus on just crashes
        let _ = kcore::parse_command(&cmd);
    }
});
