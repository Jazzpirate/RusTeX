use std::fmt::Display;

pub trait Outputs {
    fn new() -> Self;

    fn message<D:Display>(&self,text:D) {
        log::info!(target:"message","{}",text);
    }

    fn file_open<D:Display>(&self,text:D) {
        log::info!(target:"file","({}",text);
    }

    fn file_close<D:Display>(&self,_text:D) {
        log::info!(target:"file",")");
    }

    fn write_18<D:Display>(&self,text:D) {
        log::info!(target:"write::18","{}",text);
    }

    fn write_17<D:Display>(&self,text:D) {
        log::info!(target:"write::17","{}",text);
    }

    fn write_16<D:Display>(&self,text:D) {
        log::info!(target:"write::16","{}",text);
    }

    fn write_neg1<D:Display>(&self,text:D) {
        log::info!(target:"write::-1","{}",text);
    }

    fn write_other<D:Display>(&self,text:D) {
        log::info!(target:"write::?","{}",text);
    }
}

pub struct LogOutputs;
impl Outputs for LogOutputs {
    fn new() -> Self { Self }
}