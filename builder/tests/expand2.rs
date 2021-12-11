#![feature(prelude_import)]
#[prelude_import]
use std::prelude::rust_2018::*;
#[macro_use]
extern crate std;
use derive_builder::Builder;
pub struct Command {
    executable: String,
    #[builder(each = "arg")]
    args: Vec<String>,
    #[builder(each = "env")]
    env: Vec<String>,
    current_dir: Option<String>,
}
pub struct CommandBuilder {
    executable: std::option::Option<String>,
    args: std::vec::Vec<String>,
    env: std::vec::Vec<String>,
    current_dir: std::option::Option<String>,
}
impl Command {
    pub fn builder() -> CommandBuilder {
        CommandBuilder {
            executable: std::option::Option::None,
            args: std::vec::Vec::<String>::new(),
            env: std::vec::Vec::<String>::new(),
            current_dir: std::option::Option::None,
        }
    }
}
impl CommandBuilder {
    pub fn build(
        &mut self,
    ) -> std::result::Result<Command, std::boxed::Box<dyn std::error::Error>> {
        if self.executable.is_none() {
            let err = {
                let res = ::alloc::fmt::format(::core::fmt::Arguments::new_v1(
                    &["", " is missing"],
                    &match (&"executable",) {
                        _args => [::core::fmt::ArgumentV1::new(
                            _args.0,
                            ::core::fmt::Display::fmt,
                        )],
                    },
                ));
                res
            };
            return std::result::Result::Err(err.into());
        }
        std::result::Result::Ok(Command {
            executable: self.executable.clone().unwrap(),
            args: self.args.clone(),
            env: self.env.clone(),
            current_dir: self.current_dir.clone(),
        })
    }
    fn executable(&mut self, executable: String) -> &mut Self {
        self.executable = std::option::Option::Some(executable);
        self
    }
    fn arg(&mut self, arg: String) -> &mut Self {
        self.args.push(arg);
        self
    }
    fn args(&mut self, args: Vec<String>) -> &mut Self {
        self.args = args;
        return self;
    }
    fn env(&mut self, env: Vec<String>) -> &mut Self {
        self.env = env;
        return self;
    }
    fn current_dir(&mut self, current_dir: String) -> &mut Self {
        self.current_dir = std::option::Option::Some(current_dir);
        self
    }
}
fn main() {
    let command = Command::builder()
        .executable("cargo".to_owned())
        .arg("build".to_owned())
        .arg("--release".to_owned())
        .build()
        .unwrap();
    {
        match (&command.executable, &"cargo") {
            (left_val, right_val) => {
                if !(*left_val == *right_val) {
                    let kind = ::core::panicking::AssertKind::Eq;
                    ::core::panicking::assert_failed(
                        kind,
                        &*left_val,
                        &*right_val,
                        ::core::option::Option::None,
                    );
                }
            }
        }
    };
    {
        match (&command.args, &<[_]>::into_vec(box ["build", "--release"])) {
            (left_val, right_val) => {
                if !(*left_val == *right_val) {
                    let kind = ::core::panicking::AssertKind::Eq;
                    ::core::panicking::assert_failed(
                        kind,
                        &*left_val,
                        &*right_val,
                        ::core::option::Option::None,
                    );
                }
            }
        }
    };
}