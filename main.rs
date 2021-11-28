// use derive_builder::Builder;

// #[derive(Builder)]
// pub struct Command {
//     executable: String,
//     #[builder(each = "arg")]
//     args: Vec<String>,
//     #[builder(each = "env")]
//     env: Vec<String>,
//     current_dir: Option<String>,
// }

// fn main() {
//     let command = Command::builder()
//         .executable("cargo".to_owned())
//         .arg("build".to_owned())
//         .arg("--release".to_owned())
//         .build()
//         .unwrap();

//     assert_eq!(command.executable, "cargo");
//     assert_eq!(command.args, vec!["build", "--release"]);
// }

use derive_builder::Builder;

#[derive(Builder)]
pub struct Command {
    executable: String,
    args: Vec<String>,
    env: Vec<String>,
    current_dir: String,
}

// struct A {
//     a: i32,
// }

fn main() {

}

