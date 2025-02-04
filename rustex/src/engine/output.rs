use ansi_term::Color::{Black, Blue, Green, Red, White, Yellow};
use std::{any::Any, fmt::Display};
use tex_engine::engine::utils::outputs::Outputs;

pub trait OutputCont: Any {
    fn message(&self, text: String);
    fn errmessage(&self, text: String);
    fn file_open(&self, text: String);
    fn file_close(&self, text: String);
    fn write_18(&self, text: String);
    fn write_17(&self, text: String);
    fn write_16(&self, text: String);
    fn write_neg1(&self, text: String);
    fn write_other(&self, text: String);
    fn as_any(self: Box<Self>) -> Box<dyn Any>;
}

pub enum RusTeXOutput {
    Log(tex_engine::engine::utils::outputs::LogOutputs),
    Print(bool),
    None,
    Cont(Box<dyn OutputCont>),
}
impl RusTeXOutput {
    pub fn errmessage<D: Display>(&self, text: D) {
        match self {
            Self::Log(_) => log::info!(target:"errmessage::?","{}",text),
            Self::Print(_) => {
                println!("\n\n{}", Red.paint(text.to_string()));
            }
            Self::None => {}
            Self::Cont(b) => b.errmessage(text.to_string()),
        }
    }
}
impl Outputs for RusTeXOutput {
    fn new() -> Self {
        Self::None
    }

    fn message<D: Display>(&self, text: D) {
        match self {
            Self::Log(l) => l.message(text),
            Self::Print(_) => print!("{}", Yellow.paint(text.to_string())),
            Self::None => {}
            Self::Cont(b) => b.message(text.to_string()),
        }
    }

    fn file_open<D: Display>(&self, text: D) {
        match self {
            Self::Log(l) => l.file_open(text),
            Self::Print(_) => print!("\n({}", text),
            Self::None => {}
            Self::Cont(b) => b.file_open(text.to_string()),
        }
    }

    fn file_close<D: Display>(&self, text: D) {
        match self {
            Self::Log(l) => l.file_close(""),
            Self::Print(_) => print!(")"),
            Self::None => {}
            Self::Cont(b) => b.file_close(text.to_string()),
        }
    }

    fn write_18<D: Display>(&self, text: D) {
        match self {
            Self::Log(l) => l.write_18(text),
            Self::Print(_) => (),
            Self::None => (),
            Self::Cont(b) => b.write_18(text.to_string()),
        }
    }

    fn write_17<D: Display>(&self, text: D) {
        match self {
            Self::Log(l) => l.write_17(text),
            Self::Print(_) => print!("{}", text),
            Self::None => {}
            Self::Cont(b) => b.write_17(text.to_string()),
        }
    }

    fn write_16<D: Display>(&self, text: D) {
        match self {
            Self::Log(l) => l.write_16(text),
            Self::Print(_) => print!("\n{}", White.bold().paint(text.to_string())),
            Self::None => {}
            Self::Cont(b) => b.write_16(text.to_string()),
        }
    }

    fn write_neg1<D: Display>(&self, text: D) {
        match self {
            Self::Log(l) => l.write_neg1(text),
            Self::Print(true) => print!("\n{}", Black.on(Blue).paint(text.to_string())),
            Self::None | Self::Print(_) => {}
            Self::Cont(b) => b.write_neg1(text.to_string()),
        }
    }

    fn write_other<D: Display>(&self, text: D) {
        match self {
            Self::Log(l) => l.write_other(text),
            Self::Print(_) => print!("\n{}", Black.on(Green).paint(text.to_string())),
            Self::None => {}
            Self::Cont(b) => b.write_other(text.to_string()),
        }
    }
}
