use std::fmt::Display;

pub trait Outputs {
    fn new() -> Self;
    #[inline(always)]
    fn message<D:Display>(&self,text:D) {
        log::info!(target:"message","{}",text);
    }
    #[inline(always)]
    fn errmessage<D:Display>(&self,text:D) {
        log::error!(target:"errmessage","{}",text);
        crate::throw!("run aborted")
    }
    #[inline(always)]
    fn file_open<D:Display>(&self,text:D) {
        log::info!(target:"file","({}",text);
    }
    #[inline(always)]
    fn file_close<D:Display>(&self,text:D) {
        log::info!(target:"file",")");
    }
    #[inline(always)]
    fn write_18<D:Display>(&self,text:D) {
        log::info!(target:"write::18","{}",text);
    }
    #[inline(always)]
    fn write_17<D:Display>(&self,text:D) {
        log::info!(target:"write::17","{}",text);
    }
    #[inline(always)]
    fn write_16<D:Display>(&self,text:D) {
        log::info!(target:"write::16","{}",text);
    }
    #[inline(always)]
    fn write_neg1<D:Display>(&self,text:D) {
        log::info!(target:"write::-1","{}",text);
    }
    #[inline(always)]
    fn write_other<D:Display>(&self,text:D) {
        log::info!(target:"write::?","{}",text);
    }
}

pub struct LogOutputs;
impl Outputs for LogOutputs {
    fn new() -> Self { Self }
}