#![no_main]
use libfuzzer_sys::fuzz_target;
use arbitrary::{Arbitrary, Unstructured};

extern crate karmator_rust;
use karmator_rust::parser::test_all_token as kt;

// Dummy struct to get a string
#[derive(Arbitrary)]
struct Dummy {
    dat: Vec<kt::KarmaToken>,
}

fuzz_target!(|data: &[u8]| {
    let mut unstructured = Unstructured::new(data);

    if let Ok(value) = Dummy::arbitrary(&mut unstructured) {
        let cmd = value.dat;
        let dat = cmd.iter().map(|k| k.to_string()).collect::<Vec<String>>().join("");

        let new_dat = kt::all_token(&dat);

        assert_eq!(new_dat, Ok(("", cmd)));
    }
});
