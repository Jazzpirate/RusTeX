use std::borrow::Cow;
use std::fmt::{Display, Formatter};
use tex_engine::engine::filesystem::{File, FileSystem};
use crate::engine::{Font, Types};
use std::fmt::Write;
use tex_engine::engine::fontsystem::Font as FontT;
use tex_engine::pdflatex::nodes::{NumOrName, PDFColor};
use tex_engine::tex::nodes::boxes::{HBoxInfo, ToOrSpread, VBoxInfo};
use tex_engine::tex::nodes::math::MathClass;
use tex_engine::utils::HMap;
use tex_glyphs::fontstyles::FontModifier;
use crate::engine::extension::CSS;
use crate::RUSTEX_CSS_URL;
use crate::shipout::state::{Alignment, CharOrStr, Common, FontData, ShipoutNodeH, ShipoutNodeHRow, ShipoutNodeM, ShipoutNodeSVG, ShipoutNodeTable, ShipoutNodeV, SourceRef};
use crate::utils::{Flex, VecMap};

#[derive(Default)]
pub enum ImageOptions {
    #[default] AsIs,
    ModifyURL(Box<dyn Fn(String) -> String>),
    Embed
}

pub(crate) struct CompilationDisplay<'a,'b> {
    pub(crate) width:i32,
    pub(crate) indent:u8,
    pub(crate) color: PDFColor,
    pub(crate) font: Font,
    pub(crate) font_data:&'a HMap<Box<str>,FontData>,
    pub(crate) attrs:VecMap<Cow<'static,str>,Cow<'static,str>>,
    pub(crate) styles:VecMap<Cow<'static,str>,Cow<'static,str>>,
    pub(crate) sourcerefs:bool,
    pub(crate) image:&'a ImageOptions,
    pub(crate) f:&'a mut Formatter<'b>
}

macro_rules! node {
    ($self:ident !$($tk:tt)* ) => {{
        $self.do_indent()?;$self.indent +=1;
        node!(@START $self;$($tk)* IND );
    }};
    ($self:ident $($tk:tt)* ) => {
        node!(@START $self;$($tk)*)
    };
    (@START $self:ident;<<$tag:expr; $($tk:tt)*) => {
        write!($self.f,"<{}",$tag)?;
        node!(@ATTRS $self;$tag; $($tk)*);
    };
    (@START $self:ident;<$tag:ident $($tk:tt)*) => {{
        write!($self.f,"<{}",stringify!($tag))?;
        node!(@ATTRS $self;stringify!($tag); $($tk)*);
    }};
    (@ATTRS $self:ident;$tag:expr; class=$cls:literal?($b:expr) $($tk:tt)*) => {
        if $b {write!($self.f," class=\"{}\"",$cls)?;}
        node!(@ATTRS $self;$tag; $($tk)*);
    };
    (@ATTRS $self:ident;$tag:expr; class=$cls:literal $($tk:tt)*) => {
        write!($self.f," class=\"{}\"",$cls)?;
        node!(@ATTRS $self;$tag; $($tk)*);
    };
    (@ATTRS $self:ident;$tag:expr; class=$cls:expr; $($tk:tt)*) => {
        write!($self.f," class=\"{}\"",$cls)?;
        node!(@ATTRS $self;$tag; $($tk)*);
    };
    (@ATTRS $self:ident;$tag:expr; ref=$r:ident $($tk:tt)*) => {
        if $self.sourcerefs { write!($self.f," data-rustex-sourceref=\"{}\"",$r)? }
        node!(@ATTRS $self;$tag; $($tk)*);
    };
    (@ATTRS $self:ident;$tag:expr; $a:literal=$v:expr; $($tk:tt)*) => {
        write!($self.f," {}=\"{}\"",$a,$v)?;
        node!(@ATTRS $self;$tag; $($tk)*);
    };
    (@ATTRS $self:ident;$tag:expr; $($tk:tt)*) => {
        for (k,v) in std::mem::take(&mut $self.attrs) {
            write!($self.f," {}=\"{}\"",k,v)?
        }
        node!(@STYLES? $self;$tag; $($tk)*);
    };
    (@STYLES? $self:ident;$tag:expr; style:$style:block $($tk:tt)*) => {
        let mut in_style = false;
        let mut needs_span:Option<i32> = None;
        macro_rules! style {
            (!$s:ident; ($a:expr)=$v:expr) => {{
                if !in_style {
                    write!($s.f," style=\"")?;
                    in_style = true;
                }
                write!($s.f,"{}:{};",$a,$v)?;
            }};
            ($a:literal=$v:expr) => {{
                if !in_style {
                    write!($self.f," style=\"")?;
                    in_style = true;
                }
                write!($self.f,"{}:{};",$a,$v)?;
            }};
        }
        macro_rules! width {
            ($v:expr) => {
                if $v == 0 {style!("width"="0");} else if $v != $self.width {
                    needs_span = Some($self.width);
                    let pctg = $v as f32 / ($self.width as f32);
                    $self.width = $v;
                    style!("--rustex-scale-width"=format_args!("{:.2}",pctg));
                }
            }
        }
        $style
        if in_style {
            if let Some(w) = needs_span {
                node!(@STYLES! $self;$tag; {$($tk)*} WIDTH=w;);
            } else {
                node!(@STYLES! $self;$tag; {$($tk)*});
            }
        }
        else { node!(@STYLES? $self;$tag; $($tk)*); }
    };
    (@STYLES? $self:ident;$tag:expr; style:$a:literal=$v:expr; $($tk:tt)*) => {
        write!($self.f," style=\"{}:{};",$a,$v)?;
        node!(@STYLES! $self;$tag; {$($tk)*});
    };
    (@STYLES? $self:ident;$tag:expr; $($tk:tt)*) => {
        if !$self.styles.is_empty() {
            write!($self.f," style=\"")?;
            $self.do_styles()?;
            $self.f.write_char('"')?;
        }
        node!(@BODY $self;$tag; $($tk)*);
    };
    (@STYLES! $self:ident;$tag:expr; {style:$a:literal=$v:expr; $($tk:tt)*} $(WIDTH=$w:expr;)?) => {
        write!($self.f,"{}:{};",$a,$v)?;
        node!(@STYLES! $self;$tag; {$($tk)*} $(WIDTH=$w;)?);
    };
    (@STYLES! $self:ident;$tag:expr; {$($tk:tt)*} $(WIDTH=$w:expr;)?) => {
        $self.do_styles()?;
        $self.f.write_char('"')?;
        node!(@BODY $self;$tag; $($tk)* $(WIDTH=$w;)?);
    };
    (@BODY $self:ident;$tag:expr; /> $(WIDTH=$w:expr;)?) => {
        write!($self.f,"></{}>",$tag)?;
        $( $self.width = $w; )?
    };
    (@BODY $self:ident;$tag:expr; /> IND $(WIDTH=$w:expr;)?) => {
        write!($self.f,"></{}>",$tag)?;
        $self.indent -= 1;
        $( $self.width = $w; )?
    };
    (@BODY $self:ident;$tag:expr; $b:block/>) => {
        $self.f.write_char('>')?;
        $b
        write!($self.f,"</{}>",$tag)?;
    };
    (@BODY $self:ident;$tag:expr; $b:block/> IND) => {
        $self.f.write_char('>')?;
        $b
        $self.indent -= 1;
        $self.do_indent()?;
        write!($self.f,"</{}>",$tag)?;
    };
    (@BODY $self:ident;$tag:expr; $b:block/> WIDTH=$w:expr;) => {
        $self.f.write_str("><span>")?;
        $b
        $self.width = $w;
        write!($self.f,"</span></{}>",$tag)?;
    };
    (@BODY $self:ident;$tag:expr; $b:block/> IND WIDTH=$w:expr;) => {
        $self.f.write_str("><span>")?;
        $b
        $self.indent -= 1;
        $self.do_indent()?;
        $self.width = $w;
        write!($self.f,"</span></{}>",$tag)?;
    };
}

impl CompilationDisplay<'_,'_> {
    pub fn display(&mut self,
        metas:&Vec<VecMap<String,String>>,
        top:&VecMap<String,String>,
        css:&Vec<CSS>,
        page_width:i32,
        out:&Vec<ShipoutNodeV>
    ) -> std::fmt::Result {
        self.f.write_str("<!DOCTYPE html>\n<html lang=\"en\"")?;
        for (k,v) in top.iter() {
            write!(self.f," {}=\"{}\"",k,v)?;
        }
        self.f.write_str(">\n<head>\t<meta charset=\"UTF-8\">\n")?;
        for m in metas.into_iter() {
            write!(self.f,"\t<meta")?;
            for (k,v) in m.iter() {
                write!(self.f," {}=\"{}\"",k,v)?;
            }
            self.f.write_str(">\n")?;
        }
        write!(self.f,"\t<link rel=\"stylesheet\" type=\"text/css\" href=\"{}\">\n",RUSTEX_CSS_URL)?;
        for c in css.iter() {
            match c {
                CSS::File(s) => write!(self.f,"\t<link rel=\"stylesheet\" type=\"text/css\" href=\"{}\">\n",s)?,
                CSS::Literal(s) => write!(self.f,"\t<style>\n{}</style>\n",s)?
            }
        }
        for (name,d) in self.font_data.iter(){//.filter_map(|d| d.1.web.as_ref().map(|s| s.as_ref().ok()).flatten()) {
            match &d.web {
                Some((l,_))=> write!(self.f,"\t<link rel=\"stylesheet\" href=\"{l}\">\n")?,
                None => write!(self.f,"\t<!-- Missing web font for {name} -->\n")?,
            }
        }
        self.f.write_str("</head>\n<body>")?;
        write!(self.f,"<page class=\"rustex-body\" style=\"--rustex-text-width:{};--rustex-page-width:{};",
            Self::dim_to_num(self.width),
            Self::dim_to_num(page_width)
        )?;
        if let Some(font) = self.font_data.get(self.font.filename()) {
            if let Some((_,css)) = font.web.as_ref() {
                write!(self.f,"font-family:{css};")?;
            }
            write!(self.f,"font-size:{};",Self::dim_to_string(self.font.get_at().0))?;
        }
        self.f.write_str("\">")?;
        for c in out.iter() {
            self.do_v(c,true)?;
        }
        self.f.write_str("\n</page></body></html>")
    }

    #[inline(always)]
    fn dim_to_px(d:i32) -> f32{
        d as f32 / 65536.0 * 1.5
    }
    #[inline(always)]
    fn dim_to_num(d:i32) -> String {
        format!("{:.5}",Self::dim_to_px(d)).trim_end_matches('0').trim_end_matches('.').to_string()
    }
    #[inline(always)]
    fn dim_to_string(d:i32) -> String {
        Self::dim_to_num(d) + "px"
    }
    #[inline(always)]
    fn mu_to_string(d:i32) -> String {
        format!("{:.5}",(d as f32) / 18.0 / 65536.0).trim_end_matches('0').trim_end_matches('.').to_string() + "em"
    }
    #[inline(always)]
    fn do_indent(&mut self) -> std::fmt::Result {
        self.f.write_char('\n')?;
        for _ in 0..(self.indent + 1) {
            self.f.write_str("\t")?;
        }
        Ok(())
    }
    fn do_styles(&mut self) -> std::fmt::Result {
        for (k,v) in std::mem::take(&mut self.styles) {
            write!(self.f,"{}:{};",k,v)?
        }
        Ok(())
    }

    fn font_attrs(&mut self,old:&Font,mut style:impl FnMut(&mut Self,&'static str,Cow<'static,str>) -> std::fmt::Result) -> std::fmt::Result {
        let oldd = self.font_data.get(old.filename()).unwrap();
        let newd = self.font_data.get(self.font.filename()).unwrap();
        let oldcss = oldd.web.as_ref().map(|(_,c)| c.as_str());
        let newcss = newd.web.as_ref().map(|(_,c)| c.as_str());
        if oldcss != newcss {
            if let Some(c) = newcss {
                style(self,"font-family",c.to_string().into())?;
            }
        }
        let size = ((self.font.get_at().0 as f32 / (old.get_at().0 as f32)) * 100.0).round();
        if size != 100.0 {
            style(self,"font-size",format!("{}%",size).into())?;
        }
        let old = oldd.modifiers.unwrap_or_default();
        let new = newd.modifiers.unwrap_or_default();
        if new.has(FontModifier::Capitals) && !old.has(FontModifier::Capitals) {
            style(self,"font-variant","small-caps".into())?;
        } else if old.has(FontModifier::Capitals) && !new.has(FontModifier::Capitals) {
            style(self,"font-variant","normal".into())?;
        }
        if new.has(FontModifier::Bold) && !old.has(FontModifier::Bold) {
            style(self,"font-weight","bold".into())?;
        } else if old.has(FontModifier::Bold) && !new.has(FontModifier::Bold) {
            style(self,"font-weight","normal".into())?;
        }
        if new.has(FontModifier::Italic) && !old.has(FontModifier::Italic) {
            style(self,"font-style","italic".into())?;
        } else if new.has(FontModifier::Oblique) && !old.has(FontModifier::Oblique) {
            style(self,"font-style","oblique".into())?;
        } else if (old.has(FontModifier::Italic) || old.has(FontModifier::Oblique)) && !(new.has(FontModifier::Italic) || new.has(FontModifier::Oblique)) {
            style(self,"font-style","normal".into())?;
        }
        Ok(())
    }

    fn do_color<N>(&mut self,node:&'static str,color:&PDFColor,children:&Vec<N>,mut f:impl FnMut(&mut Self,&N) -> std::fmt::Result) -> std::fmt::Result {
        if *color == self.color {
            for c in children { f(self,c)? }
            Ok(())
        } else {
            let old = self.color;
            self.color = *color;
            if children.len() == 1 {
                self.styles.insert("color".into(),format!("{}",color).into());
                for c in children { f(self,c)? }
            } else {
                node!(self <<node; class="rustex-contents"?(node!="mrow" && node !="g") style:"color"=color; {
                    for c in children { f(self,c)? }
                }/>);
            }
            self.color = old;
            Ok(())
        }
    }

    fn do_font<N>(&mut self,node:&'static str,font:&Font,children:&Vec<N>,mut f:impl FnMut(&mut Self,&N) -> std::fmt::Result) -> std::fmt::Result {
        if *font == self.font {
            for c in children { f(self,c)? }
            Ok(())
        } else {
            let old = std::mem::replace(&mut self.font,font.clone());
            if children.len() == 1 {
                self.font_attrs(&old,|s,a,b| Ok(s.styles.insert(a.into(),b.into())))?;
                for c in children { f(self,c)? }
            } else {
                node!(self <<node; class="rustex-contents"?(node!="mrow" && node !="g") style:{
                    self.font_attrs(&old,|s,k,v| Ok(style!(!s; (k)=v)))?;
                } {
                    for c in children { f(self,c)? }
                }/>);
            }
            self.font = old;
            Ok(())
        }
    }
    fn do_annotations<N>(&mut self,node:&'static str,
        attrs:&VecMap<Cow<'static,str>,Cow<'static,str>>,
        styles:&VecMap<Cow<'static,str>,Cow<'static,str>>,
        children:&Vec<N>,mut f:impl FnMut(&mut Self,&N) -> std::fmt::Result) -> std::fmt::Result {
        if !attrs.iter().any(|(k,_)| self.attrs.get(k).is_some()) {
            for (k,v) in attrs.iter() {
                self.attrs.insert(k.clone(),v.clone());
            }
            for (k,v) in styles.iter() {
                self.styles.insert(k.clone(),v.clone());
            }
            node!(self <<node; class="rustex-contents"?(node!="mrow" && node !="g") {
                for c in children { f(self,c)? }
            }/>);
        } else {
            node!(self <<node; class="rustex-contents"?(node!="mrow" && node !="g") {
                for (k,v) in attrs.iter() {
                    self.attrs.insert(k.clone(),v.clone());
                }
                for (k,v) in styles.iter() {
                    self.styles.insert(k.clone(),v.clone());
                }
                node!(self <<node; class="rustex-contents"?(node!="mrow" && node !="g") {
                    for c in children { f(self,c)? }
                }/>);
            }/>);
        }
        Ok(())
    }

    fn do_v(&mut self,c:&ShipoutNodeV,top:bool) -> std::fmt::Result { match c {
        ShipoutNodeV::Common(Common::WithColor {color,children,..}) =>
            self.do_color("span",color,children,|s,n| s.do_v(n,top)),
        ShipoutNodeV::Common(Common::WithFont {font,children,..}) =>
            self.do_font("span",font,children,|s,n| s.do_v(n,top)),
        ShipoutNodeV::Common(Common::WithAnnotation {attrs,styles,children,..}) => {
            self.do_annotations("span",attrs,styles,children,|s,n| s.do_v(n,top))
        }
        ShipoutNodeV::Common(Common::Literal(s)) => self.f.write_str(s),
        ShipoutNodeV::Common(Common::WithLink {href,children,..}) => {
            node!(self <a "href"=href;{
                for c in children { self.do_v(c,top)? }
            }/>);
            Ok(())
        }
        ShipoutNodeV::Common(Common::WithMatrix {scale,rotate,skewx,skewy,children,..}) => {
            node!(self <span class="rustex-pdfmatrix" style:"transform"=format_args!("matrix({scale},{rotate},{skewx},{skewy},0,0)"); {
                for c in children { self.do_v(c,top)? }
            }/>);
            Ok(())
        }
        ShipoutNodeV::Common(Common::PDFDest(n)) => {
            match n {
                NumOrName::Name(s) =>
                    node!(self !<a "id"=s; "name"=s;/>),
                NumOrName::Num(n) =>
                    node!(self !<a "id"=format_args!("NUM_{}",n); "name"=format_args!("NUM_{}",n);/>)
            }
            Ok(())
        }
        ShipoutNodeV::KernSkip(m) => {
            node!(self !<div class="rustex-vskip" style:{
                if m.base.is_positive() {
                    style!("height"=Self::dim_to_string(m.base))
                } else {
                    style!("margin-bottom"=Self::dim_to_string(m.base))
                }
                match m.stretch {
                    Flex::Fil(_) | Flex::Fill(_) | Flex::Filll(_) =>
                        style!("margin-top"="auto"),
                    _ => ()
                }
            }/>);Ok(())
        }
        ShipoutNodeV::Common(Common::VBox {sref,info:info@VBoxInfo::VBox {..},children,..}) => {
            self.do_indent()?;self.do_vbox(sref,info,children,top)
        }
        ShipoutNodeV::Common(Common::VBox {sref,info:info@VBoxInfo::VTop {..},children,..}) => {
            self.do_vtop(sref,info,children,false)
        }
        ShipoutNodeV::Common(Common::HBox {sref,info:info@HBoxInfo::HBox {..},children,..}) => {
            self.do_indent()?;self.do_hbox(sref,info,children)
        }
        ShipoutNodeV::HRule {width,height,depth} => {
            let ht = height.map(|h| h.0).unwrap_or(26214) + depth.map(|d| d.0).unwrap_or(0);
            if ht <= 0 { return Ok(()) }
            let bottom = match depth {
                None => None,
                Some(d) if d.0 == 0 => None,
                Some(d) => Some(-d.0)
            };
            let color = self.color;

            node!(self !<div class="rustex-hrule" style:{
                style!("height"=Self::dim_to_string(ht));
                match width {
                    None => style!("min-width"="100%"),
                    Some(w) => style!("--rustex-scale-width"=(w.0 as f32) / (self.width as f32))
                }
            }{node!(self !<div style:{
                style!("background"=color);
                style!("height"=Self::dim_to_string(ht));
                if let Some(b) = bottom {
                    style!("margin-bottom"=Self::dim_to_string(b));
                }
            }/>)}/>);
            Ok(())
        }
        ShipoutNodeV::Paragraph { children, alignment,
            parskip, line_skip, left_skip, right_skip,
            width, sref,..} => {
            self.do_indent()?;
            self.indent += 1;
            let cls = match width {
                i if *i != 0 && *i != self.width =>
                    "rustex-paragraph rustex-scalewidth",
                0 => todo!(),
                _ => "rustex-paragraph rustex-withwidth",
            };
            node!(self <div class=cls;ref=sref style:{
                if !left_skip.is_zero() {
                    style!("margin-left"=Self::dim_to_string(left_skip.base))
                }
                if !right_skip.is_zero() {
                    style!("margin-right"=Self::dim_to_string(right_skip.base))
                }
                match alignment {
                    Alignment::L => style!("text-align"="left"),
                    Alignment::C => style!("text-align"="center"),
                    Alignment::R => style!("text-align"="right"),
                    _ => ()
                }
                width!(*width);
            }{for c in children {
                self.do_h(c,false)?
            }}/>);
            self.indent -= 1;
            Ok(())
        }
        ShipoutNodeV::HAlign {children,num_cols,..} => {
            node!(self !<table class="rustex-halign" style:"--rustex-align-num"=num_cols;{
                node!(self <tbody {
                    for c in children {
                        self.do_row(c)?
                    }
                }/>);
            }/>);
            Ok(())
        }
        _ => todo!("{c:?}")
    } }

    fn do_h(&mut self,c:&ShipoutNodeH,escape:bool) -> std::fmt::Result { match c {
        ShipoutNodeH::Common(Common::WithColor {color,children,..}) =>
            self.do_color("span",color,children,|s,n| s.do_h(n,escape)),
        ShipoutNodeH::Common(Common::WithFont {font,children,..}) =>
            self.do_font("span",font,children,|s,n| s.do_h(n,escape)),
        ShipoutNodeH::Common(Common::WithAnnotation {attrs,styles,children,..}) =>
            self.do_annotations("span",attrs,styles,children,|s,n| s.do_h(n,escape)),
        ShipoutNodeH::Common(Common::Literal(s)) => self.f.write_str(s),
        ShipoutNodeH::Common(Common::WithLink {href,children,..}) => {
            node!(self <a "href"=href;{
                for c in children { self.do_h(c,escape)? }
            }/>);
            Ok(())
        }
        ShipoutNodeH::Common(Common::WithMatrix {scale,rotate,skewx,skewy,children,..}) => {
            node!(self <span class="rustex-pdfmatrix" style:"transform"=format_args!("matrix({scale},{rotate},{skewx},{skewy},0,0)"); {
                for c in children { self.do_h(c,escape)? }
            }/>);
            Ok(())
        }
        ShipoutNodeH::Common(Common::PDFDest(n)) => {
            match n {
                NumOrName::Name(s) =>
                    node!(self !<a "id"=s; "name"=s;/>),
                NumOrName::Num(n) =>
                    node!(self !<a "id"=format_args!("NUM_{}",n); "name"=format_args!("NUM_{}",n);/>)
            }
            Ok(())
        }
        ShipoutNodeH::KernSkip(m) => {
            node!(self <div class="rustex-hskip" style:{
                if m.base.is_positive() {
                    style!("width"=Self::dim_to_string(m.base))
                } else {
                    style!("margin-left"=Self::dim_to_string(m.base))
                }
                match m.stretch {
                    Flex::Fil(_) | Flex::Fill(_) | Flex::Filll(_) =>
                        style!("margin-right"="auto"),
                    _ => ()
                }
            }/>);
            Ok(())
        }
        ShipoutNodeH::Common(Common::SVG {sref, minx,maxx,miny,maxy,children,..}) => {
            self.do_svg(sref,*minx,*maxx,*miny,*maxy,children)
        }
        ShipoutNodeH::Common(Common::HBox {sref,info:info@HBoxInfo::HBox {..},children,..}) => {
            self.do_hbox(sref,info,children)
        }
        ShipoutNodeH::Common(Common::VBox {sref,info:info@VBoxInfo::VBox {..},children,..}) => {
            self.do_vbox(sref,info,children,false)
        }
        ShipoutNodeH::Common(Common::VBox {sref,info:info@VBoxInfo::VTop {..},children,..}) => {
            self.do_vtop(sref,info,children,false)
        }
        ShipoutNodeH::VRule {width,height,depth} => {
            let wd = match width {
                Some(w) if w.0 <= 0 => return Ok(()),
                Some(w) => w.0,
                None => 26214
            };
            let color = self.color;
            if height.is_none() && depth.is_none() {
                node!(self <div class="rustex-vrule" style:{
                    style!("--rustex-this-width"=Self::dim_to_string(wd));
                    style!("background"=color);
                    if escape {
                        style!("align-self"="stretch");
                    } else {
                        let font_size = self.font.get_at().0;
                        style!("min-height"=Self::dim_to_string(font_size));
                    }
                }/>);
            } else {
                let ht = height.map(|h| h.0).unwrap_or(0) + depth.map(|d| d.0).unwrap_or(0);
                let dp = if depth.is_none() && !escape {None} else {Some(depth.map(|d| d.0).unwrap_or(0))};
                node!(self <div class="rustex-vrule-container"
                    style:"height"=Self::dim_to_string(ht);
                    style:"--rustex-this-width"=Self::dim_to_string(wd);
                    {node!(self <div style:{
                        style!("background"=color);
                        match dp {
                            Some(dp) => style!("margin-bottom"=Self::dim_to_string(-dp)),
                            None => {
                                style!("height"=format_args!("calc(0.5ex + {})",Self::dim_to_string(ht)));
                                style!("margin-bottom"="-0.5ex");
                            }
                        }
                    }/>)}
                />);
            }
            Ok(())
        }
        ShipoutNodeH::Char(c) => {
            if self.attrs.is_empty() && self.styles.is_empty() {
                Display::fmt(&Escaped(c), self.f)?
            } else {
                node!(self <span {Display::fmt(&Escaped(c), self.f)?}/>)
            }
            Ok(())
        },
        ShipoutNodeH::Space if !escape => {
            if self.attrs.is_empty() && self.styles.is_empty() {
                self.f.write_str(" ")?
            } else {
                node!(self <span class="rustex-contents" {self.f.write_char(' ')?}/>)
            }
            Ok(())
        },
        ShipoutNodeH::Space => {
            node!(self <div class="rustex-space-in-hbox" />);
            Ok(())
        }
        ShipoutNodeH::Math {display,sref,children,..} => {
            let inner = move |s:&mut Self| {
                node!(s <math class="rustex-math" ref=sref {
                    node!(s !<mrow {
                        for c in children {
                           s.do_indent()?;s.do_math(c,None,false)?
                        }
                    }/>);
                }/>);
                Ok(())
            };

            match display {
                Some((above,below)) => {
                    node!(self <div class="rustex-display"
                        style:"margin-top"=Self::dim_to_string(above.base);
                        style:"margin-bottom"=Self::dim_to_string(below.base);{
                        inner(self)?
                    }/>);
                    Ok(())
                }
                None => inner(self)
            }
        }
        ShipoutNodeH::Img(img) => {
            match &self.image {
                ImageOptions::AsIs => {
                    let width = img.width().0;
                    let height = img.height().0;
                    node!(self <img "src"=img.filepath.display();
                        "width"=Self::dim_to_string(width);
                        "height"=Self::dim_to_string(height);
                    />);
                    Ok(())
                }
                _ => todo!()
            }
        }
        ShipoutNodeH::Indent(i) => {
            if *i != 0 {
                node!(self <div class="rustex-parindent" style:"margin-left"=Self::dim_to_string(*i);/>);
            }
            Ok(())
        }
        ShipoutNodeH::LineBreak => self.f.write_str("<br/>"),
        ShipoutNodeH::MissingGlyph {char,font_name,..} => {
            node!(self <span class="rustex-missing-glyph" "title"=format_args!("Missing Glyph {char} in {font_name}");/>);
            Ok(())
        }
        _ => todo!("{c:?}")
    }}

    fn cls(cls:MathClass) -> (&'static str,&'static str,&'static str) {
        match cls {
            MathClass::Ord => ("rustex-math-ord","0","0"),
            MathClass::Op => ("rustex-math-op","0.15em","0.15em"),
            MathClass::Bin => ("rustex-math-bin","0.15em","0.15em"),
            MathClass::Rel => ("rustex-math-rel","0.2em","0.2em"),
            MathClass::Open => ("rustex-math-open","0","0"),
            MathClass::Close => ("rustex-math-close","0","0"),
            MathClass::Punct => ("rustex-math-punct","0","0.15em")
        }
    }

    fn do_math(&mut self,c:&ShipoutNodeM,cls:Option<MathClass>,cramped:bool) -> std::fmt::Result { match c {
        ShipoutNodeM::Common(Common::Literal(s)) => self.f.write_str(s),
        ShipoutNodeM::Common(Common::WithLink {href,children,..}) => {
            node!(self !<mrow "href"=href; {
                for c in children { self.do_math(c,cls,cramped)? }
            }/>);
            Ok(())
        }
        ShipoutNodeM::Common(Common::WithColor {color,children,..}) =>
            self.do_color("mrow",color,children,|s,n| s.do_math(n,cls,cramped)),
        ShipoutNodeM::Common(Common::WithAnnotation {attrs,styles,children,..}) => {
            self.do_annotations("mrow",attrs,styles,children,|s,n| s.do_math(n,cls,cramped))
        }
        ShipoutNodeM::MSkip{base,mu} if !*mu => {
            self.do_indent()?;
            node!(self !<mspace class="rustex-mkern" "width"=Self::dim_to_string(*base);/>);
            Ok(())
        }
        ShipoutNodeM::MSkip{base,..} => {
            self.do_indent()?;
            node!(self !<mspace class="rustex-mkern" "width"=Self::mu_to_string(*base);/>);
            Ok(())
        }
        ShipoutNodeM::WithClass {class,children,..} => {
            if children.len() == 1 {
                self.do_math(children.first().unwrap(),Some(*class),cramped)
            } else {
                let (cls,lspace,rspace) = Self::cls(*class);
                node!(self !<mrow class=cls; "lspace"=lspace; "rspace"=rspace; {
                    for c in children {
                        self.do_math(c, Some(*class),cramped)?
                    }
                }/>);
                Ok(())
            }
        }
        ShipoutNodeM::Common(Common::HBox {sref,info:info@HBoxInfo::HBox {..},children,..}) => {
            let oldwd = self.width;
            let wd = info.assigned_width().unwrap_or_else(|| info.computed_width().unwrap_or_default()).0;
            node!(self !<mtext class="rustex-math-escape" ref=sref style:{
                if wd>0 {
                    self.width = wd;
                    let wd = Self::dim_to_string(wd);
                    style!("width"=wd);
                    style!("--rustex-curr-width"=wd);
                } else if wd == 0 {
                    style!("width"="0");
                }
            } {
                self.do_hbox(sref,info,children)?
            }/>);
            self.width = oldwd;
            Ok(())
        }
        ShipoutNodeM::Common(Common::VBox {sref,info,children,..}) => {
            let oldwd = self.width;
            let wd = info.assigned_width().unwrap_or_else(|| info.computed_width().unwrap_or_default()).0;
            node!(self !<mtext class="rustex-math-escape" ref=sref style:{
                if wd>0 {
                    self.width = wd;
                    let wd = Self::dim_to_string(wd);
                    style!("width"=wd);
                    style!("--rustex-curr-width"=wd);
                } else if wd == 0 {
                    style!("width"="0");
                }
            } {
                self.do_vbox(sref,info,children,false)?;
            }/>);
            self.width = oldwd;
            Ok(())
        }
        ShipoutNodeM::VCenter {sref,width,children,..} => {
            let oldwd = self.width;
            node!(self !<mtext class="rustex-math-escape" ref=sref style:{
                if *width>0 {
                    self.width = *width;
                    let wd = Self::dim_to_string(*width);
                    style!("width"=wd);
                    style!("--rustex-curr-width"=wd);
                } else if *width == 0 {
                    style!("width"="0");
                }
            } {
                node!(self <div class="rustex-vcenter-container" {
                    node!(self <div {
                        for c in children {
                            self.do_v(c,false)?;
                        }
                    } />)
                }/>)
            }/>);
            self.width = oldwd;
            Ok(())
        }
        ShipoutNodeM::VRule {width,height,depth} => {
            let wd = match width {
                Some(w) if w.0 <= 0 => return Ok(()),
                Some(w) => w.0,
                None => 26214
            };
            let color = self.color;
            self.do_indent()?;
            node!(self <mspace "background"=color;
                "width"=Self::dim_to_string(wd);
                "height"=Self::dim_to_string(height.unwrap_or_default().0);
                "depth"=Self::dim_to_string(depth.unwrap_or_default().0);
            />);
            Ok(())
        }
        ShipoutNodeM::Glyph {char,cramped} => {
            // TODO cramped
            let (tag,cls,lspace,rspace) = match cls {
                Some(MathClass::Ord)| None => {
                    let (cls,lspace,rspace) = Self::cls(MathClass::Ord);
                    ("mi",cls,lspace,rspace)
                }
                Some(o) => {
                    let (cls,lspace,rspace) = Self::cls(o);
                    ("mo",cls,lspace,rspace)
                }
            };
            node!(self <<tag; class=cls; "lspace"=lspace; "rspace"=rspace;{Display::fmt(&Escaped(char), self.f)?}/>);
            Ok(())
        }
        ShipoutNodeM::Space => {
            node!(self !<mspace class="rustex-mkern" "width"="0.25em";/>);
            Ok(())
        }
        ShipoutNodeM::Sup{base, sup,limits} => {
            node!(self !<<if *limits {"mover"} else {"msup"}; {
                self.do_math(base,None,cramped)?;
                if sup.len() == 1 {
                    self.do_math(sup.first().unwrap(),None,cramped)?;
                } else {
                    node!(self !<mrow {
                        for c in sup.iter() {
                            self.do_math(c,None,cramped)?;
                        }
                    }/>);
                }
            } />);
            Ok(())
        }
        ShipoutNodeM::Sub{base, sub,limits} => {
            node!(self !<<if *limits {"munder"} else {"msub"}; {
                self.do_math(base,None,cramped)?;
                if sub.len() == 1 {
                    self.do_math(sub.first().unwrap(),None,cramped)?;
                } else {
                    node!(self !<mrow {
                        for c in sub.iter() {
                            self.do_math(c,None,cramped)?;
                        }
                    }/>);
                }
            } />);
            Ok(())
        }
        ShipoutNodeM::LeftRight { sref,left,right,children,..} => {
            node!(self !<mrow ref=sref {
                if let Some(Ok(c)) = left {
                    node!(self !<mo class="rustex-math-open" "stretchy"="true"; {Display::fmt(&Escaped(c),self.f)?} />);
                }
                for c in children {
                    self.do_math(c,None,cramped)?
                }
                if let Some(Ok(c)) = right {
                    node!(self !<mo class="rustex-math-close" "stretchy"="true"; {Display::fmt(&Escaped(c),self.f)?} />);
                }
            }/>);
            Ok(())
        }
        ShipoutNodeM::Middle(r) => {
            match r {
                Ok(c) =>
                    node!(self <mo "stretchy"="true"; {Display::fmt(&Escaped(c),self.f)?}/>),
                Err((_,char,font_name)) =>
                    node!(self <mtext class="rustex-missing" "title"=format_args!("Missing Glyph {char} in {font_name}");/>)
            }
            Ok(())
        }
        ShipoutNodeM::SubSup{base, sub,sup,limits} => {
            node!(self !<<if *limits {"munderover"} else {"msubsup"}; {
                self.do_math(base,None,cramped)?;
                if sub.len() == 1 {
                    self.do_math(sub.first().unwrap(),None,cramped)?;
                } else {
                    node!(self !<mrow {
                        for c in sub.iter() {
                            self.do_math(c,None,cramped)?;
                        }
                    }/>);
                }
                if sup.len() == 1 {
                    self.do_math(sup.first().unwrap(),None,cramped)?;
                } else {
                    node!(self !<mrow {
                        for c in sup.iter() {
                            self.do_math(c,None,cramped)?;
                        }
                    }/>);
                }
            } />);
            Ok(())
        }
        ShipoutNodeM::Phantom {width,height,depth} => {
            self.do_indent()?;
            node!(self !<mspace
                "width"=Self::dim_to_string(*width);
                "height"=Self::dim_to_string(*height);
                "depth"=Self::dim_to_string(*depth);
            />);
            Ok(())
        }
        ShipoutNodeM::Over{sref,sep,left,right,top,bottom,..} => {
            let inner = move |s:&mut Self| {
                node!(s !<mfrac ref=sref "linethickness"=sep.map(|s| Self::dim_to_string(s)).unwrap_or_default(); {
                    node!(s !<mrow {
                        for c in top.iter() {
                            s.do_math(c,None,cramped)?;
                        }
                    }/>);
                    node!(s !<mrow {
                        for c in bottom.iter() {
                            s.do_math(c,None,cramped)?;
                        }
                    }/>);
                }/>);
                Ok(())
            };
            match (left,right) {
                (None,None) => inner(self)?,
                _ => node!(self !<mrow {
                    if let Some(Ok(c)) = left {
                        node!(self !<mo class="rustex-math-open" "stretchy"="true"; {Display::fmt(&Escaped(c),self.f)?} />);
                    }
                    inner(self)?;
                    if let Some(Ok(c)) = right {
                        node!(self !<mo class="rustex-math-close" "stretchy"="true"; {Display::fmt(&Escaped(c),self.f)?} />);
                    }
                }/>)
            }
            Ok(())
        }
        ShipoutNodeM::Underline {children,..} => {
            if children.len() == 1 {
                self.styles.insert("text-decoration".into(),"underline".into());
                self.do_math(children.first().unwrap(),cls,cramped)
            } else {
                node!(self !<mrow style:"text-decoration"="underline"; {
                    for c in children {
                        self.do_math(c,cls,cramped)?;
                    }
                }/>);
                Ok(())
            }
        }
        ShipoutNodeM::Overline {children,..} => {
            if children.len() == 1 {
                self.styles.insert("text-decoration".into(),"overline".into());
                self.do_math(children.first().unwrap(),cls,cramped)
            } else {
                node!(self !<mrow style:"text-decoration"="overline"; {
                    for c in children {
                        self.do_math(c,cls,cramped)?;
                    }
                }/>);
                Ok(())
            }
        }
        ShipoutNodeM::Accent { accent,children,..} => {
            node!(self !<mover {
                if children.len() == 1 {
                    self.do_math(children.first().unwrap(),None,cramped)?
                } else {
                    node!(self !<mrow {
                        for c in children {
                            self.do_math(c,None,cramped)?;
                        }
                    }/>);
                }
                match accent {
                    Ok(c) => node!(self !<mo {Display::fmt(&Escaped(c),self.f)?}/>),
                    Err((_,c,fnt)) =>
                        node!(self !<mtext class="rustex-missing" "title"=format_args!("Missing Glyph {c} in {fnt}");/>)
                }
            }/>);
            Ok(())
        }
        ShipoutNodeM::MissingGlyph {char,font_name,..} => {
            node!(self !<mtext class="rustex-missing" "title"=format_args!("Missing Glyph {char} in {font_name}");/>);
            Ok(())
        }
        ShipoutNodeM::Radical {children,..} => {
            node!(self <msqrt {
                for c in children {
                    self.do_math(c,None,cramped)?;
                }
            }/>);
            Ok(())
        }
        _ => todo!("{c:?}")
    } }


    fn do_row(&mut self,c:&ShipoutNodeTable) -> std::fmt::Result { match c {
        ShipoutNodeTable::Common(Common::Literal(s)) => self.f.write_str(s),
        ShipoutNodeTable::Common(Common::WithAnnotation {children,attrs,styles,..}) => {
            for c in children {
                for (k,v) in attrs.iter() { self.attrs.insert(k.clone(),v.clone())}
                for (k,v) in styles.iter() { self.styles.insert(k.clone(),v.clone())}
                self.do_row(c)?;
            }
            Ok(())
        }
        ShipoutNodeTable::Common(Common::WithFont {children,font,..}) => {
            for c in children {
                // TODO insert font
                self.do_row(c)?;
            }
            Ok(())
        }
        ShipoutNodeTable::Row {children,..} => {
            node!(self !<tr {
                for c in children {
                    self.do_cell(c)?;
                }
            }/>);
            Ok(())
        }
        ShipoutNodeTable::NoAlign {children,..} => {
            node!(self !<tr {node!(self <td class="rustex-noalign" {
                for c in children {
                    self.do_v(c,false)?;
                }
            }/>);}/>);
            Ok(())
        }
        _ => todo!("{c:?}")
    } }

    fn do_cell(&mut self,c:&ShipoutNodeHRow) -> std::fmt::Result { match c {
        ShipoutNodeHRow::Common(Common::Literal(s)) => self.f.write_str(s),
        ShipoutNodeHRow::Common(Common::WithAnnotation {children,attrs,styles,..}) => {
            for c in children {
                for (k,v) in attrs.iter() { self.attrs.insert(k.clone(),v.clone())}
                for (k,v) in styles.iter() { self.styles.insert(k.clone(),v.clone())}
                self.do_cell(c)?;
            }
            Ok(())
        }
        ShipoutNodeHRow::Cell {children,spans,..} => {
            node!(self !<td class="rustex-halign-cell" style:{
                if *spans > 1 {
                    style!("grid-column"=format_args!("span {}",spans))
                }
            }{
                for c in children {
                    self.do_h(c,true)?;
                }
            }/>);
            Ok(())
        }
        _ => todo!("{c:?}")
    }}

    fn do_svg(&mut self,sref:&SourceRef,minx:i32,maxx:i32,miny:i32,maxy:i32,children:&Vec<ShipoutNodeSVG>) -> std::fmt::Result {
        node!(self <div class="rustex-svg" {node!(self <svg ref=sref
            "width"=Self::dim_to_string(maxx - minx);
            "height"=Self::dim_to_string(maxy - miny);
            "viewBox"=format_args!("{} {} {} {}",
                Self::dim_to_num(minx),
                Self::dim_to_num(miny),
                Self::dim_to_num(maxx - minx),
                Self::dim_to_num(maxy - miny)
            );{node!(self !<g "transform"=format_args!("translate(0,{})",Self::dim_to_num(maxy + miny)); {
                for c in children {
                    self.do_svg_node(c)?
                }
            }/>);}
        />)}/>);
        Ok(())
    }

    fn do_svg_node(&mut self,c:&ShipoutNodeSVG) -> std::fmt::Result { match c {
        ShipoutNodeSVG::Common(Common::Literal(s)) => self.f.write_str(s),
        ShipoutNodeSVG::SVGNode {tag,attrs,children,..} => {
            for (k,v) in attrs.iter() { self.attrs.insert((*k).into(),v.clone().into())}
            node!(self !<<tag; {for c in children {
                self.do_svg_node(c)?
            }} />);
            Ok(())
        }
        ShipoutNodeSVG::Common(Common::WithColor {color,children,..}) =>
            self.do_color("g",color,children,|s,n| s.do_svg_node(n)),
        ShipoutNodeSVG::Common(Common::WithAnnotation {attrs,styles,children,..}) => {
            self.do_annotations("g",attrs,styles,children,|s,n| s.do_svg_node(n))
        }
        ShipoutNodeSVG::Common(Common::HBox {sref,info:info@HBoxInfo::HBox {..},children,..}) => {
            let wd = info.computed_width().map(|d| d.0).unwrap_or_default();
            let ht = info.computed_height().map(|d| d.0).unwrap_or_default()
                + info.assigned_depth().map(|d| d.0).unwrap_or_default();
            self.do_indent()?;
            node!(self !<foreignObject class="rustex-foreign"
                style:"width"=Self::dim_to_string(wd);
                style:"height"=Self::dim_to_string(ht);
                style:"translate"=format_args!("0 {}",Self::dim_to_string(-ht));
                {node!(self <div
                    {self.do_hbox(sref,info,children)?;}
                />)}
            />);
            Ok(())
        }
        _ => todo!("{c:?}")
    }}

    fn do_hbox(&mut self,sref:&SourceRef,info:&HBoxInfo<Types>,children:&Vec<ShipoutNodeH>) -> std::fmt::Result {
        let (wd,ht,bottom,to) = get_box_dims_h(info);
        let cls = match wd {
            Some(i) if i != 0 && i != self.width =>
                "rustex-hbox-container rustex-scalewidth",
            //Some(0) => todo!(),
            Some(i) if i == self.width =>
                "rustex-hbox-container rustex-withwidth",
            _ => "rustex-hbox-container",
        };
        let inner = move |s:&mut Self| {
            node!(s <div class=cls; ref=sref style:{
                if let Some(bottom) = bottom {
                    style!("margin-bottom"=Self::dim_to_string(bottom))
                }
                if let Some(ht) = ht {
                    style!("height"=Self::dim_to_string(ht))
                }
                if let Some(wd) = wd { width!(wd) }
            } {
                let cls = match to {
                    Some(i) if i != 0 && i != s.width =>
                        "rustex-hbox rustex-scalewidth",
                    Some(i) if i == s.width =>
                        "rustex-hbox rustex-withwidth",
                    _ => "rustex-hbox"
                };
                node!(s <div class=cls; style:{ if let Some(w) = to {width!(w)}} {
                    for c in children {
                        s.do_h(c,true)?;
                    }
                }/>)
            } />);
            Ok(())
        };
        match (info.raised(),info.moved_left()) {
            (Some(r),None) => {
                node!(self <div class="rustex-raise" style:"--rustex-raise"=Self::dim_to_string(r.0);{
                    inner(self)?
                }/>);
            }
            (None,Some(ml)) => {
                node!(self <div class="rustex-moveleft" style:"--rustex-moveleft"=Self::dim_to_string(ml.0);{
                    inner(self)?
                }/>);
            }
            _ => inner(self)?, // both are impossible anyway
        }
        Ok(())
    }

    fn do_vbox(&mut self,sref:&SourceRef,info:&VBoxInfo<Types>,children:&Vec<ShipoutNodeV>,top:bool) -> std::fmt::Result {
        let (wd,ht,bottom,to) = get_box_dims_v(top,info);
        let cls = match wd {
            Some(i) if i != 0 && i != self.width =>
                "rustex-vbox-container rustex-scalewidth",
            //Some(0) => todo!(),
            Some(i) if i == self.width =>
                "rustex-vbox-container rustex-withwidth",
            _ => "rustex-vbox-container",
        };
        let inner = move |slf:&mut Self| {
            node!(slf <div class=cls; ref=sref style:{
                if let Some(bottom) = bottom {
                    style!("margin-bottom"=Self::dim_to_string(bottom))
                }
                if let Some(wd) = wd { width!(wd) }
            } {
                let inner = move |slf:&mut Self| {
                    node!(slf <div class="rustex-vbox" style:{
                        match to {
                            Some(i) if i < 0 => {
                                style!("height"="0");
                                style!("margin-bottom"=Self::dim_to_string(i))
                            }
                            Some(i) => style!("height"=Self::dim_to_string(i)),
                            _ => ()
                        }
                    } {
                        for c in children {
                            slf.do_v(c,false)?;
                        }
                    }/>);
                    Ok(())
                };
                if let Some(h) = ht {
                    node!(slf <div class="rustex-vbox-height-container" style:"height"=Self::dim_to_string(h);{
                        inner(slf)?
                    }/>);
                } else { inner(slf)?}
            }/>);
            Ok(())
        };
        match (info.raised(),info.moved_left()) {
            (Some(r),None) => {
                node!(self <div class="rustex-raise" style:"--rustex-raise"=Self::dim_to_string(r.0);{
                    inner(self)?
                }/>);
            }
            (None,Some(ml)) => {
                node!(self <div class="rustex-moveleft" style:"--rustex-moveleft"=Self::dim_to_string(ml.0);{
                    inner(self)?
                }/>);
            }
            _ => inner(self)?, // both are impossible anyway
        }
        Ok(())
    }

    fn do_vtop(&mut self,sref:&SourceRef,info:&VBoxInfo<Types>,children:&Vec<ShipoutNodeV>,top:bool) -> std::fmt::Result {
        let (wd,ht,bottom,to) = get_box_dims_v(top,info);
        let cls = match wd {
            Some(i) if i != 0 && i != self.width =>
                "rustex-vtop-container rustex-scalewidth",
            //Some(0) => todo!(),
            Some(i) if i == self.width =>
                "rustex-vtop-container rustex-withwidth",
            _ => "rustex-vtop-container",
        };
        let inner = move |slf:&mut Self| {
            node!(slf <div class=cls; style:{
                if let Some(bottom) = bottom {
                    style!("margin-bottom"=Self::dim_to_string(bottom))
                }
                if let Some(wd) = wd { width!(wd) }
            } {
                let mut inner = move |slf:&mut Self| {
                    node!(slf <div class="rustex-vtop" style:{
                        match to {
                            Some(i) if i < 0 => {
                                style!("height"=0);
                                style!("margin-bottom"=Self::dim_to_string(i));
                            }
                            Some(i) => style!("height"=Self::dim_to_string(i)),
                            _ => ()
                        }
                    } {
                        for c in children {
                            slf.do_v(c,false)?;
                        }
                    }/>);
                    Ok(())
                };
                if let Some(h) = ht {
                    node!(slf <div class="rustex-vtop-height-container" style:"height"=Self::dim_to_string(h); {
                        inner(slf)?
                    }/>);
                } else { inner(slf)?}
            }/>);
            Ok(())
        };


        match (info.raised(),info.moved_left()) {
            (Some(r),None) => {
                node!(self <div class="rustex-raise" style:"--rustex-raise"=Self::dim_to_string(r.0);{
                    inner(self)?
                }/>);
            }
            (None,Some(ml)) => {
                node!(self <div class="rustex-moveleft" style:"--rustex-moveleft"=Self::dim_to_string(ml.0);{
                    inner(self)?
                }/>);
            }
            _ => inner(self)?, // both are impossible anyway
        }
        Ok(())
    }

}


fn get_box_dims_v(top:bool,info:&VBoxInfo<Types>) -> (Option<i32>,Option<i32>,Option<i32>,Option<i32>) {
    let wd = match info.assigned_width() {
        Some(w) =>  Some(w.0),
        _ if top => match info.computed_width() {
            Some(w) if w.0 < 0 => Some(0),
            _ => None
        }
        _ => None
    };
    let (ht,mut bottom) = match info.assigned_height() {
        Some(h) if h.0 < 0 => (Some(0),Some(h.0)),
        Some(h) => (Some(h.0),None),
        _ if top && info.computed_height().map(|d| d.0 < 0).unwrap_or(false) => (Some(0),None),
        _ => (None,None)
    };
    match (bottom,info.assigned_depth()) {
        (Some(b),Some(d)) => {
            let s = b + d.0;
            if s != 0 { bottom = Some(b);
            } else { bottom = None; }
        }
        (_,Some(d)) if d.0 != 0 => bottom = Some(d.0),
        _ => ()
    }
    let to = match info.to_or_scaled() {
        Some(ToOrSpread::To(d)) => Some(d.0),
        Some(ToOrSpread::Spread(s)) => Some(s.0 + info.computed_height().map(|d| d.0).unwrap_or(0)),
        _ => None
    };
    (wd,ht,bottom,to)
}

fn get_box_dims_h(info:&HBoxInfo<Types>) -> (Option<i32>,Option<i32>,Option<i32>,Option<i32>) {
    let wd = match info.assigned_width() {
        Some(w) =>  Some(w.0),
        _  => match info.computed_width() {
            Some(w) if w.0 < 0 => Some(0),
            _ => None
        }
    };
    let (ht,mut bottom) = match info.assigned_height() {
        Some(h) if h.0 < 0 => (Some(0),Some(h.0)),
        Some(h) => (Some(h.0),None),
        _ => (None,None)
    };
    match (bottom,info.assigned_depth()) {
        (Some(b),Some(d)) => {
            let s = b + d.0;
            if s != 0 { bottom = Some(b);
            } else { bottom = None; }
        }
        (_,Some(d)) if d.0 != 0 => bottom = Some(d.0),
        _ => ()
    }
    let to = match info.to_or_scaled() {
        Some(ToOrSpread::To(d)) => Some(d.0),
        Some(ToOrSpread::Spread(s)) => Some(s.0 + info.computed_width().map(|d| d.0).unwrap_or(0)),
        _ => None
    };
    (wd,ht,bottom,to)
}

struct Escaped<'a>(&'a CharOrStr);
impl Display for Escaped<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use CharOrStr::*;
        const TRIGGER: [char; 3] = ['<','>','&'];
        match self.0 {
            Char('<') => f.write_str("&lt;"),
            Char('>') => f.write_str("&gt;"),
            Char('&') => f.write_str("&amp;"),
            //'"' => self.f.write_str("&quot;"),
            //'\'' => self.f.write_str("&apos;"),
            Char(c) => f.write_char(*c),
            Str(s) if s.contains(&TRIGGER) => {
                f.write_str(&s.replace('<',"&lt;").replace('>',"&gt;").replace('&',"&amp;"))
            }
            Str(s) => f.write_str(s)
        }
    }
}

/*

fn dim_to_px(d:i32) -> f32{
    d as f32 / 65536.0 * 1.5
}


pub(crate) fn dim_to_num(d:i32) -> String {
    format!("{:.5}",dim_to_px(d)).trim_end_matches('0').trim_end_matches('.').to_string()
}


pub(crate) fn dim_to_string(d:Dim32) -> String {
    dim_to_num(d.0) + "px"
}


pub(crate) fn mudim_to_string(d:Mu) -> String {
    format!("{:.5}",(d.0 as f32) / 18.0 / 65536.0).trim_end_matches('0').trim_end_matches('.').to_string() + "em"
}

type Ref = SourceReference<<<Types as EngineTypes>::File as File>::SourceRefID>;

pub struct Escaper<'a,W:Write>(&'a mut W);
impl<'a,W:Write> Write for Escaper<'a,W> {
    fn write_char(&mut self, c: char) -> std::fmt::Result {
        match c {
            '<' => self.0.write_str("&lt;"),
            '>' => self.0.write_str("&gt;"),
            '&' => self.0.write_str("&amp;"),
            //'"' => self.f.write_str("&quot;"),
            //'\'' => self.f.write_str("&apos;"),
            _ => self.0.write_char(c)
        }
    }
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        for c in s.chars() { self.write_char(c)? }
        Ok(())
    }
}

#[derive(Debug)]
pub enum HTMLChild {
    Node(HTMLNode),
    Text(String),
    Comment(String),
    HRule{width:Option<Dim32>,height:Dim32,bottom:Option<Dim32>,color:PDFColor,start:SRef,end:SRef},
    VRuleS {width:Dim32,font_size:Option<Dim32>,color:PDFColor,start:SRef,end:SRef},
    VRuleC {width:Dim32,height:Dim32,depth:Option<Dim32>,color:PDFColor,start:SRef,end:SRef},
    Image {width:Dim32,height:Dim32,filepath:PathBuf}
    //Glyph(Glyph),Space,EscapedSpace
}
impl HTMLChild {
    pub fn display_fmt(&self,store:&FontStore,files:&RusTeXFileSystem,missings:&mut HashSet<String>,indent:usize,curr_width:Dim32,in_font:&Font,do_refs:bool,f:&mut Formatter<'_>) -> std::fmt::Result {
        match self {
            HTMLChild::Node(n) => n.display_fmt(store, files, missings,indent, curr_width, in_font,do_refs, f),
            HTMLChild::Text(string) => f.write_str(string),
            HTMLChild::Comment(s) => f.write_str(s),
            HTMLChild::HRule { width, height, bottom, color, start, end } => {
                write!(f, "<div class=\"rustex-hrule\" style=\"height:{};{}\"><div style=\"background:{};height:{};{}\" {}></div></div>",
                       dim_to_string(*height),
                       match width {
                           None => "min-width:100%".to_string(),
                           Some(w) => format!("--rustex-scale-width:{};", (w.0 as f32) / (curr_width.0 as f32))
                       },
                       color,
                       dim_to_string(*height),
                       match bottom {
                           None => "".to_string(),
                           Some(b) => format!("margin-bottom:{};", dim_to_string(*b))
                       },
                       SourceRange(start, end, files,do_refs)
                )
            },
            HTMLChild::VRuleS { width, font_size, color, start, end } => {
                write!(f,"<div class=\"rustex-vrule\" style=\"--rustex-this-width:{};{};background:{};\" {}></div>",
                       dim_to_string(*width),
                    match font_size {
                        Some(font_size) => format!("min-height:{}",dim_to_string(*font_size)),
                        _ => "align-self:stretch".to_string()
                    },
                    color,
                    SourceRange(start,end,files,do_refs)
                )
            }
            HTMLChild::VRuleC { width, height, depth, color, start, end } => {
                write!(f,
                       "<div class=\"rustex-vrule-container\" style=\"height:{};--rustex-this-width:{};\" {}><div style=\"{}background:{};\"></div></div>",
                        dim_to_string(*height),
                       dim_to_string(*width),
                        SourceRange(start,end,files,do_refs),
                       /*match depth {
                           None => "-0.5ex".to_string(),
                           Some(d) => dim_to_string(-*d)
                       },*/
                       match depth {
                           None => format!("height:calc(0.5ex + {});margin-bottom:-0.5ex;", dim_to_string(*height)),
                           Some(d) => format!("margin-bottom:{};", dim_to_string(-*d))
                       },
                       color
                )
            }
            HTMLChild::Image { width, height, filepath } => {
                write!(f, "<img src=\"{}\" width=\"{}\" height=\"{}\"/>",
                       filepath.display(),
                       dim_to_string(*width),
                       dim_to_string(*height),
                )
            }
        }
    }
}

#[derive(Debug)]
pub struct HTMLNode {
    pub tag:HTMLTag,
    pub children:Vec<HTMLChild>,
    uses_font:bool,
    pub(crate) uses_color:bool,
    pub color:Option<PDFColor>,
    pub font:Option<(Font,bool)>, // true = absolute, false = relative
    pub width:Option<(Dim32,bool)>, // true = absolute, false = relative
    pub classes:Vec<Cow<'static,str>>,
    pub attrs:VecMap<Cow<'static,str>,String>,
    pub styles:VecMap<Cow<'static,str>,Cow<'static,str>>,
    pub sourceref:Option<(Ref,Ref)>,
}

impl HTMLNode {
    pub fn reopened(&self) -> Self {
        HTMLNode {
            tag:self.tag.clone(),
            children:Vec::new(),
            uses_font:self.uses_font,
            uses_color:self.uses_color,
            color:self.color.clone(),
            font:self.font.clone(),
            width:self.width.clone(),
            classes:self.classes.clone(),
            attrs:self.attrs.clone(),
            styles:self.styles.clone(),
            sourceref:self.sourceref.clone()
        }
    }
    pub fn new(tag:HTMLTag) -> Self {
        HTMLNode {
            tag,
            children:Vec::new(),
            uses_font:false,
            uses_color:false,
            color:None,font:None,
            width:None,
            classes:Vec::new(),
            attrs:VecMap::default(),
            styles:VecMap::default(),
            sourceref:None
        }
    }
    pub fn page() -> Self {
        HTMLNode {
            tag:HTMLTag::Page,
            children:Vec::new(),
            uses_font:false,
            uses_color:false,
            color:None,font:None,
            width:None,
            classes:vec!("rustex-page".into()),
            attrs:VecMap::default(),
            styles:VecMap::default(),
            sourceref:None
        }
    }
    pub fn push_glyph(&mut self,mode:ShipoutMode,g:Glyph) {
        match self.children.last_mut() {
            Some(HTMLChild::Text(string)) => {
                write!(Escaper(string),"{}",g).unwrap();
            }
            _ => {
                let mut s = String::new();
                write!(Escaper(&mut s),"{}",g).unwrap();
                self.push_child(mode,HTMLChild::Text(s));
            }
        }
    }
    pub fn push_space(&mut self,mode:ShipoutMode) {
        match self.children.last_mut() {
            Some(HTMLChild::Text(string)) if string.ends_with(char::is_whitespace) => {
                string.push_str("&nbsp;");
            }
            Some(HTMLChild::Text(string)) => {
                string.push(' ');
            }
            _ => {
                self.push_child(mode,HTMLChild::Text(" ".to_string()));
            }
        }
    }
    pub fn paragraph(start:SRef,end:SRef,width:Dim32) -> Self {
        HTMLNode {
            tag:HTMLTag::Paragraph,
            children:Vec::new(),
            uses_font:false,
            uses_color:false,
            color:None,font:None,
            sourceref:Some((start,end)),
            width:Some((width,false)),
            classes:vec!("rustex-paragraph".into()),
            attrs:VecMap::default(),
            styles:VecMap::default(),
        }
    }
    pub fn push_comment<D:Display>(&mut self,d:D) {
        match self.children.last_mut() {
            Some(HTMLChild::Comment(string)) => {
                write!(string,"{}",d).unwrap();
            }
            _ => {
                let mut s = String::new();
                write!(s,"{}",d).unwrap();
                self.children.push(HTMLChild::Comment(s));
            }
        }
    }
    pub fn push_string<D:Display>(&mut self,d:D,mode:ShipoutMode,escape:bool) {
        match self.children.last_mut() {
            Some(HTMLChild::Text(string)) => {
                if escape {
                    write!(Escaper(string),"{}",d).unwrap();
                } else {
                    write!(string,"{}",d).unwrap();
                }
            }
            _ => {
                let mut s = String::new();
                if escape {
                    write!(Escaper(&mut s),"{}",d).unwrap();
                } else {
                    write!(s,"{}",d).unwrap();
                }
                self.push_child(mode,HTMLChild::Text(s));
            }
        }
    }
    pub fn push_child(&mut self,mode:ShipoutMode,ch:HTMLChild) {
        if mode == ShipoutMode::SVG {
            self.children.push(ch)
        } else {
            match ch {
                t @ HTMLChild::Text { .. } => {
                    if mode != ShipoutMode::Math && self.font.is_none() { self.uses_font = true }
                    if self.color.is_none() { self.uses_color = true }
                    self.children.push(t);
                }
                HTMLChild::Node(n) => return self.push_open_node(mode, n),
                o => self.children.push(o)
            }
        }
    }

    pub fn push_open_node(&mut self,mode:ShipoutMode,n:HTMLNode) {
        if self.color.is_none() {
            self.uses_color = self.uses_color || n.uses_color;
        }
        if self.font.is_none() {
            self.uses_font = self.uses_font || n.uses_font;
        }
        n.close(mode,&mut self.children);
    }
    pub fn close(self,mode:ShipoutMode,parent:&mut Vec<HTMLChild>) {
        match self.tag {
            HTMLTag::FontChange(_) => close_font(self,mode,parent),
            HTMLTag::ColorChange(_) => close_color(self,mode,parent),
            HTMLTag::Annot(_,None) => close_annot(self,mode,parent),
            HTMLTag::Invisible(_) => close_invisible(self,mode,parent),
            _ if self.children.len() == 1 => match &self.children[0] {
                HTMLChild::Node(n) if matches!(n.tag,HTMLTag::FontChange(_)|HTMLTag::ColorChange(_)|HTMLTag::Invisible(_))
                    => merge_annotation(self, mode, parent),
                _ => parent.push(HTMLChild::Node(self))
            },
            _ => parent.push(HTMLChild::Node(self))
        }
    }
    pub fn displayable<'a>(&'a self,store:&'a FontStore,files:&'a RusTeXFileSystem,width:Dim32,font:&'a Font,do_refs:bool) -> DisplayableNode<'a> {
        DisplayableNode {node:self,files,store,width,font,do_refs,missing_fonts:Mutex::new(HashSet::new())}
    }
    pub fn display_fmt(&self,store:&FontStore,files:&RusTeXFileSystem,missings:&mut HashSet<String>,indent:usize,curr_width:Dim32,in_font:&Font,do_refs:bool,f:&mut Formatter<'_>) -> std::fmt::Result {
        write!(f,"<{}",self.tag)?;
        let (widthcls,widthsstyle,wd,scaled) = if let Some((wd,abs)) = self.width {
            if wd == Dim32(0) {
                (None,Some("max-width:0;".to_string()),curr_width,false)
            } else if wd < Dim32(0) {
                (None,Some(format!("max-width:0;margin-right:{};",dim_to_string(wd))),curr_width,false)
            } else if abs {
                (Some("rustex-withwidth"),Some(format!("--rustex-this-width:{};",dim_to_num(wd.0))),wd,false)
            } else {
                let pctg = wd.0 as f32 / (curr_width.0 as f32);
                (Some("rustex-scalewidth"),Some(format!("--rustex-scale-width:{};",format!("{:.2}",pctg))),wd,true)
            }
        } else { (None,None,curr_width,false) };

        if !self.classes.is_empty() || widthcls.is_some() {
            f.write_str(" class=\"")?;
            let mut i = self.classes.iter();
            if let Some(c) = widthcls {
                f.write_str(c)?
            } else {
                f.write_str(i.next().unwrap())?;
            }
            for c in i {
                write!(f," {}",c)?;
            }
            f.write_char('"')?;
        }
        if !self.attrs.is_empty() {
            for (k,v) in self.attrs.iter() {
                write!(f," {}=\"{}\"",k,v)?;
            }
        }
        let (font,font_style,missing) = if let Some((ref f,abs)) = self.font {
            if !abs && in_font == f {
                (in_font,None,None)
            } else {
                let (a, b) = font_attributes(store, &f, missings,if abs { None } else { Some(in_font) });
                (f, a, b)
            }
        } else { (in_font,None,None) };

        if !self.styles.is_empty() || widthsstyle.is_some() || font_style.is_some() || self.color.is_some() {
            f.write_str(" style=\"")?;
            if let Some(s) = font_style {
                f.write_str(&s)?;
            }
            if let Some(wd) = widthsstyle {
                f.write_str(&wd)?;
            }
            if let Some(c) = self.color {
                write!(f,"color:{};",c)?;
            }
            for (k,v) in self.styles.iter() {
                write!(f,"{}:{};",k,v)?;
            }
            f.write_char('"')?;
        }
        if do_refs {
            if let Some((start, end)) = &self.sourceref {
                f.write_char(' ')?;
                do_sourceref(files, start, end, f)?;
            }
        }
        f.write_char('>')?;
        if scaled {
            f.write_str("<span>")?;
        }
        if let Some(missing) = missing {
            f.write_str(&missing)?;
        }
        for c in &self.children {
            if self.tag.allow_linebreak() {
                do_indent(indent+2,f)?;
            }
            c.display_fmt(store,files,missings,indent+2,wd,font,do_refs,f)?;
        }
        if scaled {
            f.write_str("</span>")?;
        }
        if self.tag.allow_linebreak() {
            do_indent(indent,f)?;
        }
        write!(f,"</{}>",self.tag)
    }
}

fn close_invisible(mut node:HTMLNode,mode:ShipoutMode, parent:&mut Vec<HTMLChild>) {
    node.tag = HTMLTag::Invisible(mode);
    node.styles.insert("visibility".into(),"hidden".into());
    if node.children.len() == 1 {
        match node.children.first().unwrap() {
            HTMLChild::Node(_) => {
                if let Some(HTMLChild::Node(mut n)) = node.children.pop() {
                    if node.attrs.iter().any(|(k,_)| {
                        n.attrs.contains_key(k)
                    }) {
                        node.children.push(HTMLChild::Node(n));
                        parent.push(HTMLChild::Node(node));
                        return
                    }
                    for (k,v) in node.attrs {
                        n.attrs.insert(k,v);
                    }
                    if let (Some(c),None) = (node.color,n.color) {
                        n.color = Some(c);
                        n.uses_color = false;
                    }
                    if let (Some(c),None) = (node.font,&n.font) {
                        n.font = Some(c);
                        n.uses_font = false;
                    }
                    for s in node.styles {
                        n.styles.insert(s.0,s.1);
                    }
                    n.styles.insert("visibility".into(),"hidden".into());
                    parent.push(HTMLChild::Node(n));
                } else {unreachable!()}
            }
            _ => parent.push(HTMLChild::Node(node))
        }
    } else {
        parent.push(HTMLChild::Node(node));
    }
}

fn close_annot(mut node:HTMLNode,mode:ShipoutMode, parent:&mut Vec<HTMLChild>) {
    if node.children.len() == 1 {
        match node.children.first().unwrap() {
            HTMLChild::Node(_) => {
                if let Some(HTMLChild::Node(mut n)) = node.children.pop() {
                    if node.attrs.iter().any(|(k,_)| {
                        n.attrs.contains_key(k)
                    }) {
                        node.children.push(HTMLChild::Node(n));
                        parent.push(HTMLChild::Node(node));
                        return
                    }
                    for (k,v) in node.attrs {
                        n.attrs.insert(k,v);
                    }
                    if let (Some(c),None) = (node.color,n.color) {
                        n.color = Some(c);
                        n.uses_color = false;
                    }
                    if let (Some(c),None) = (node.font,&n.font) {
                        n.font = Some(c);
                        n.uses_font = false;
                    }
                    for s in node.styles {
                        n.styles.insert(s.0,s.1);
                    }
                    parent.push(HTMLChild::Node(n));
                } else {unreachable!()}
            }
            _ => parent.push(HTMLChild::Node(node))
        }
    } else {
        match &mut node.tag {
            HTMLTag::Annot(m,_) => {
                *m = mode;
                parent.push(HTMLChild::Node(node));
            }
            _ => unreachable!()
        }
    }
}

fn merge_annotation(mut node:HTMLNode, _mode:ShipoutMode, parent:&mut Vec<HTMLChild>) {
    match node.children.pop() {
        Some(HTMLChild::Node(n)) if matches!(n.tag,HTMLTag::ColorChange(_)|HTMLTag::FontChange(_)) => {
            if n.attrs.iter().any(|(k,_)| {
                node.attrs.contains_key(k)
            }) {
                node.children.push(HTMLChild::Node(n));
                parent.push(HTMLChild::Node(node));
                return
            }
            for (k,v) in n.attrs {
                node.attrs.insert(k,v);
            }
            if n.color.is_some() {
                node.color = n.color;
                node.uses_color = false;
            }
            if n.font.is_some() {
                node.font = n.font;
                node.uses_font = false;
            }
            for s in n.styles {
                node.styles.insert(s.0,s.1);
            }
            node.children = n.children;
            parent.push(HTMLChild::Node(node));
        }
        Some(HTMLChild::Node(n)) if matches!(n.tag,HTMLTag::Invisible(_)) => {
            node.styles.insert("visibility".into(),"hidden".into());
            node.children = n.children;
            parent.push(HTMLChild::Node(node));
        }
        _ => unreachable!()
    }
}

fn close_font(mut annot:HTMLNode,mode:ShipoutMode,parent:&mut Vec<HTMLChild>) {
    let mut has_text = false;
    let num = annot.children.iter().filter(|c| match c {
        HTMLChild::Node(n) => n.uses_font,
        HTMLChild::Text {..} => {has_text = true ;true},
        _ => false
    }).count();
    match num {
        0 => parent.extend(annot.children),
        1 if !has_text => {
            if let Some(c) = annot.children.iter_mut().find_map(|c| match c {
                HTMLChild::Node(n) if n.uses_font => Some(n),
                _ => None
            }) {
                assert!(c.font.is_none());
                c.font = annot.font;
                c.uses_font = false;
            }
            parent.extend(annot.children);
        }
        _ => match &mut annot.tag {
            HTMLTag::FontChange(m) => {
                *m = mode;
                parent.push(HTMLChild::Node(annot));
            }
            _ => unreachable!()
        }
    }
}

fn close_color(mut annot:HTMLNode,mode:ShipoutMode,parent:&mut Vec<HTMLChild>) {
    let mut has_text = false;
    let num = annot.children.iter().filter(|c| match c {
        HTMLChild::Node(n) => n.uses_color,
        HTMLChild::Text {..} => {has_text = true ;true},
        _ => false
    }).count();
    match num {
        0 => parent.extend(annot.children),
        1 if !has_text => {
            if let Some(c) = annot.children.iter_mut().find_map(|c| match c {
                HTMLChild::Node(n) if n.uses_color => Some(n),
                _ => None
            }) {
                assert!(c.color.is_none());
                c.uses_color = false;
                c.color = annot.color;
            }
            parent.extend(annot.children);
        }
        _ => match &mut annot.tag {
            HTMLTag::ColorChange(m) => {
                *m = mode;
                parent.push(HTMLChild::Node(annot));
            }
            _ => unreachable!()
        }
    }
}


pub struct DisplayableNode<'a> {
    node:&'a HTMLNode,
    files:&'a RusTeXFileSystem,
    store:&'a FontStore,
    width:Dim32,
    font:&'a Font,
    missing_fonts:Mutex<HashSet<String>>,
    do_refs:bool
}
impl<'a> DisplayableNode<'a> {
    pub fn get_missings(mut self) -> HashSet<String> {
        std::mem::take(self.missing_fonts.get_mut().unwrap())
    }
}
impl<'a> Display for DisplayableNode<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut hs = self.missing_fonts.lock().unwrap();
        self.node.display_fmt(self.store,self.files,&mut *hs,0,self.width,self.font,self.do_refs,f)
    }
}

#[derive(Debug,PartialEq,Eq,Clone)]
pub enum HTMLTag {
    Page,Paragraph,
    VBoxContainer,VBoxHeight,VBox,
    VTopContainer,VTopHeight,VTop,
    VCenterContainer,VCenter,
    HBoxContainer,HBox,Display,Raise,MoveLeft,
    HAlign,HBody,HRow,HCell,NoAlignH,
    FontChange(ShipoutMode),ColorChange(ShipoutMode),Link(ShipoutMode),
    Annot(ShipoutMode,Option<String>),
    Invisible(ShipoutMode),
    Matrix(ShipoutMode),
    Dest(ShipoutMode),
    Math,MathGroup,Mo,Mi,MUnderOver,MUnder,MOver,MSubSup,MSub,MSup,MFrac,MSqrt,
    MathEscape,
    SvgWrap,SvgG(String),SvgTop,SvgForeign,EscapeSvg
}
impl HTMLTag {
    fn allow_linebreak(&self) -> bool {
        use HTMLTag::*;
        match self {
            HBoxContainer | HBox | Paragraph | Mi | Mo => false,
            FontChange(m) | ColorChange(m) | Link(m) |
            Matrix(m) | Annot(m,_) | Invisible(m) => !m.is_h(),
            _ => true
        }
    }
    /*fn is_math(&self) -> bool {
        use HTMLTag::*;
        match self {
            Math | MathGroup | Mo | Mi | MUnderOver | MUnder | MOver | MSubSup | MSub | MSup | MathEscape | MFrac | MSqrt => true,
            _ => false
        }
    }*/
}
impl Display for HTMLTag {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use HTMLTag::*;
        match self {
            Page => f.write_str("article"),
            Paragraph | VBoxContainer | VBoxHeight | VBox | VTopContainer | VTopHeight | VTop |
            HBoxContainer | HBox | Display | Raise | MoveLeft | VCenterContainer | VCenter |
            SvgWrap | EscapeSvg => f.write_str("div"),
            Math => f.write_str("math"),
            MathGroup => f.write_str("mrow"),
            MathEscape => f.write_str("mtext"),
            MFrac => f.write_str("mfrac"),
            HAlign => f.write_str("table"),
            HBody => f.write_str("tbody"),
            HRow => f.write_str("tr"),
            HCell => f.write_str("td"),
            NoAlignH => f.write_str("td"),
            SvgG(s) => f.write_str(s),
            SvgTop => f.write_str("svg"),
            SvgForeign => f.write_str("foreignObject"),
            Mo => f.write_str("mo"),
            Mi => f.write_str("mi"),
            MUnderOver => f.write_str("munderover"),
            MUnder => f.write_str("munder"),
            MOver => f.write_str("mover"),
            MSubSup => f.write_str("msubsup"),
            MSub => f.write_str("msub"),
            MSup => f.write_str("msup"),
            MSqrt => f.write_str("msqrt"),
            FontChange(mode) | ColorChange(mode) | Annot(mode,None) | Matrix(mode) | Invisible(mode) => {
                if *mode == ShipoutMode::Math { f.write_str("mrow") }
                else if *mode == ShipoutMode::SVG { f.write_str("g") }
                else { f.write_str("span") }
            }
            Annot(_,Some(s)) => f.write_str(s),
            Link(mode) => {
                if *mode == ShipoutMode::Math { f.write_str("mrow") }
                else if *mode == ShipoutMode::SVG { f.write_str("g") }
                else { f.write_str("a") }
            }
            Dest(mode) => {
                if *mode == ShipoutMode::Math { f.write_str("mspace") }
                else if *mode == ShipoutMode::SVG { f.write_str("g") }
                else { f.write_str("a") }
            }
        }
    }
}

fn do_indent(indent:usize,f:&mut Formatter<'_>) -> std::fmt::Result {
    f.write_char('\n')?;
    for _ in 0..indent { f.write_str("  ")?;}
    Ok(())
}

fn font_attributes(store:&FontStore, font:&Font,missings:&mut HashSet<String>,parent:Option<&Font>) -> (Option<String>,Option<String>) {
    match parent {
        Some(f) if !f.filename().ends_with("nullfont") => {
            let old = store.get_info(f.filename());
            let old = if let Some(old) = old { old } else { return simple_font(store,missings,font) };

            let mut first = String::new();
            let size = ((font.get_at().0 as f32 / (f.get_at().0 as f32)) * 100.0).round();
            if size != 100.0 {
                write!(first,"font-size:{}%;",size).unwrap();
            }

            let new = store.get_info(font.filename());
            let new = if let Some(new) = new { new } else {
                missings.insert(font.filename().to_string());
                return (if first.is_empty() {None} else {Some(first)},Some(format!("<!-- Unknown web font for {} -->",font.filename())))
            };
            if new.styles.has(FontModifier::Capitals) && !old.styles.has(FontModifier::Capitals) {
                first.push_str("font-variant:small-caps;");
            } else if old.styles.has(FontModifier::Capitals) && !new.styles.has(FontModifier::Capitals) {
                first.push_str("font-variant:normal;");
            }
            if new.styles.has(FontModifier::Bold) && !old.styles.has(FontModifier::Bold) {
                first.push_str("font-weight:bold;");
            } else if old.styles.has(FontModifier::Bold) && !new.styles.has(FontModifier::Bold) {
                first.push_str("font-weight:normal;");
            }
            if new.styles.has(FontModifier::Italic) && !old.styles.has(FontModifier::Italic) {
                first.push_str("font-style:italic;");
            } else if new.styles.has(FontModifier::Oblique) && !old.styles.has(FontModifier::Oblique) {
                first.push_str("font-style:oblique;");
            } else if (old.styles.has(FontModifier::Italic) || old.styles.has(FontModifier::Oblique)) && !(new.styles.has(FontModifier::Italic) || new.styles.has(FontModifier::Oblique)) {
                first.push_str("font-style:normal;");
            }
            let second = match (old.weblink,new.weblink) {
                (Some((name,_)),Some((newname,_))) if name == newname => None,
                (_,Some((name,_))) => {
                    write!(first,"font-family:{};",name).unwrap();
                    None
                }
                (_,None) => {
                    missings.insert(font.filename().to_string());
                    Some(format!("<!-- Unknown web font for {} -->",font.filename()))
                }
            };
            (if first.is_empty() { None } else { Some(first) },second)
        },
        _ => simple_font(store,missings,font)
    }
}

fn simple_font(store:&FontStore,missings:&mut HashSet<String>, font:&Font) -> (Option<String>,Option<String>) {
    match store.get_info(font.filename()) {
        None => (None,if font.filename().ends_with("nullfont") {None} else {
            missings.insert(font.filename().to_string());
            Some(format!("<!-- Unknown web font for {} -->",font.filename()))
        }),
        Some(info) => {
            let mut s = String::new();
            write!(s,"font-size:{};",dim_to_string(font.get_at())).unwrap();
            if info.styles.has(FontModifier::Capitals){ s.push_str("font-variant:small-caps;"); }
            else { s.push_str("font-variant:normal;");}
            if info.styles.has(FontModifier::Bold) { s.push_str("font-weight:bold;"); }
            else { s.push_str("font-weight:normal;"); }
            if info.styles.has(FontModifier::Italic) { s.push_str("font-style:italic;"); }
            else if info.styles.has(FontModifier::Oblique) { s.push_str("font-style:oblique;"); }
            else { s.push_str("font-style:normal;");}
            match info.weblink {
                None if s.is_empty() => (None,if font.filename().ends_with("nullfont") {None} else {
                    missings.insert(font.filename().to_string());
                    Some(format!("<!-- Unknown web font for {} -->",font.filename()))
                }),
                None => (Some(s),if font.filename().ends_with("nullfont") {None} else {
                    missings.insert(font.filename().to_string());
                    Some(format!("<!-- Unknown web font for {} -->",font.filename()))
                }),
                Some((name,_)) => {
                    write!(s,"font-family:{};",name).unwrap();
                    (Some(s),None)
                }
            }
        }
    }
}

fn do_sourceref(files:&RusTeXFileSystem,start:&SRef,end:&SRef,f:&mut Formatter<'_>) -> std::fmt::Result {
    write!(f,"data-rustex-sourceref=\"{}#({};{}):({};{})\"",files.ref_str(start.file),start.line,start.column,end.line,end.column)
}

struct SourceRange<'a>(&'a SRef,&'a SRef,&'a RusTeXFileSystem,bool);
impl<'a> Display for SourceRange<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.3 {do_sourceref(self.2,self.0,self.1,f) } else {Ok(())}
    }
}

 */