use derive_builder::Builder;

#[test]
fn tests() {
    let mut v: Vec<u32> = Vec::new();
    let x = vec![2];
    v.extend_from_slice(&x);

    #[derive(Builder)]
    pub struct Command {
        executable: String,
        #[builder(each = "arg")]
        args: Vec<String>,
        // #[builder(each = "env")]
        // env: Vec<String>,
        current_dir: Option<String>,
    }
    let command = Command::builder()
        .executable("cargo".to_owned())
        // .args(vec!["build".to_owned(), "--release".to_owned()])
        // .env(vec![])
        .build()
        .unwrap();

    let t = trybuild::TestCases::new();
    // t.pass("tests/01-parse.rs");
    // t.pass("tests/02-create-builder.rs");
    // t.pass("tests/03-call-setters.rs");
    // t.pass("tests/04-call-build.rs");
    // t.pass("tests/05-method-chaining.rs");
    t.pass("tests/06-optional-field.rs");
    // t.pass("tests/07-repeated-field.rs");
    //t.compile_fail("tests/08-unrecognized-attribute.rs");
    //t.pass("tests/09-redefined-prelude-types.rs");
}
