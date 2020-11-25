#![no_main]
use libfuzzer_sys::fuzz_target;
use arbitrary::{Arbitrary, Unstructured};

extern crate karmator_rust;

// Dummy struct to get a string
#[derive(Arbitrary)]
struct Dummy {
    dat: String
}

fuzz_target!(|data: &[u8]| {
    let mut unstructured = Unstructured::new(data);

    if let Ok(value) = Dummy::arbitrary(&mut unstructured) {
        let cmd = value.dat;

        // TODO: for now focus on just crashes
        karmator_rust::parser::karma::parse(&cmd).unwrap();
    }
});
