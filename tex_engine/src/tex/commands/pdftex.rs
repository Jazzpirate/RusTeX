use crate::{cmtodo, debug_log, register_conditional, register_dim_assign, register_gullet, register_int, register_int_assign};
use crate::engine::EngineType;
use crate::engine::filesystem::{File, FileSystem};
use crate::engine::gullet::Gullet;
use crate::engine::gullet::methods::{string_to_tokens, tokens_to_string};
use crate::engine::state::State;
use crate::engine::mouth::Mouth;
use crate::engine::stomach::Stomach;
use crate::tex::catcodes::CategoryCode;
use crate::tex::commands::GulletCommand;
use crate::tex::numbers::{Int,NumSet, Dim};
use crate::tex::token::{BaseToken, Token};
use crate::utils::errors::{catch_prim, ErrorInPrimitive};
use crate::utils::strings::CharType;
use crate::utils::Ptr;

/*
pub fn ifpdfabsnum<T:Token,Gu:Gullet<T>>(state:&mut Gu::S,gullet:&mut Gu,cmd:GulletCommand<T>) -> Result<bool,ErrorInPrimitive<T>> {
    debug_log!(trace=>"ifpdfabsnum");
    let i1 = catch_prim!(gullet.get_int(state) => ("ifpdfabsnum",cmd));
    let rel = match catch_prim!(gullet.get_keywords(state,vec!["<",">","="]) => ("ifpdfabsnum",cmd)) {
        None => return Err(ErrorInPrimitive{name:"ifpdfabsnum",msg:Some("Expected one of '<','>','='".to_string()),cause:Some(cmd.cause),source:None}),
        Some(r) => r
    };
    let i2 = catch_prim!(gullet.get_int(state) => ("ifpdfabsnum",cmd));
    match rel {
        "<" => Ok(i1.to_i64().abs() < i2.to_i64().abs()),
        ">" => Ok(i1.to_i64().abs()>i2.to_i64().abs()),
        "=" => Ok(i1.to_i64().abs()==i2.to_i64().abs()),
        _ => unreachable!()
    }
}

pub fn ifpdfabsdim<T:Token,Gu:Gullet<T>>(state:&mut Gu::S,gullet:&mut Gu,cmd:GulletCommand<T>) -> Result<bool,ErrorInPrimitive<T>> {
    debug_log!(trace=>"ifpdfabsdim");
    let i1 = catch_prim!(gullet.get_dim(state) => ("ifpdfabsdim",cmd));
    let rel = match catch_prim!(gullet.get_keywords(state,vec!["<",">","="]) => ("ifpdfabsdim",cmd)) {
        None => return Err(ErrorInPrimitive{name:"ifpdfabsdim",msg:Some("Expected one of '<','>','='".to_string()),cause:Some(cmd.cause),source:None}),
        Some(r) => r
    };
    let i2 = catch_prim!(gullet.get_dim(state) => ("ifpdfabsdim",cmd));
    match rel {
        "<" => Ok(i1.to_sp() < i2.to_sp().abs()),
        ">" => Ok(i1.to_sp().abs()>i2.to_sp().abs()),
        "=" => Ok(i1.to_sp().abs()==i2.to_sp().abs()),
        _ => unreachable!()
    }
}

fn pdffilesize<T:Token,Gu:Gullet<T>>(state:&mut Gu::S,gullet:&mut Gu,cmd:GulletCommand<T>) -> Result<Vec<T>,ErrorInPrimitive<T>> {
    debug_log!(trace=>"pdffilesize");
    let ret = catch_prim!(gullet.get_expanded_group(state,false,false,true) => ("pdffilesize",cmd));
    let filename = tokens_to_string(ret,state.get_escapechar(),state.get_catcode_scheme());
    let f = state.filesystem().get(&filename);
    let ret = f.content_string().len().to_string();
    Ok(string_to_tokens(ret.as_bytes()))
}

fn pdfstrcmp<T:Token,Gu:Gullet<T>>(state:&mut Gu::S,gullet:&mut Gu,cmd:GulletCommand<T>) -> Result<Vec<T>,ErrorInPrimitive<T>> {
    debug_log!(trace=>"pdfstrcmp");
    let str1 = String::from_utf8(catch_prim!(gullet.get_braced_string(state) => ("pdfstrcmp",cmd))).unwrap();
    let str2 = String::from_utf8(catch_prim!(gullet.get_braced_string(state) => ("pdfstrcmp",cmd))).unwrap();
    debug_log!(trace=>"pdfstrcmp: {}=={}?",str1,str2);
    let ret = if str1==str2 {vec!(Token::new(BaseToken::Char(T::Char::from(b'0'),CategoryCode::Other),None))}
        else if str1 < str2 { string_to_tokens("-1".as_bytes())}
        else {vec!(Token::new(BaseToken::Char(T::Char::from(b'1'),CategoryCode::Other),None))};
    Ok(ret)
}

fn pdftexversion<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,_gullet:&mut Gu,cmd:GulletCommand<T>) -> Result<<S::NumSet as NumSet>::Int,ErrorInPrimitive<T>> {
    Ok(catch_prim!(<S::NumSet as NumSet>::Int::from_i64(140) => ("pdftexversion",cmd)))
}

fn pdfmajorversion<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,_gullet:&mut Gu,cmd:GulletCommand<T>) -> Result<<S::NumSet as NumSet>::Int,ErrorInPrimitive<T>> {
    Ok(catch_prim!(<S::NumSet as NumSet>::Int::from_i64(1) => ("pdfmajorversion",cmd)))
}

pub fn pdftexrevision<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,_gullet:&mut Gu,_cmd:GulletCommand<T>) -> Result<Vec<T>,ErrorInPrimitive<T>> {
    Ok(T::from_str("25".to_string()))
}

 */

pub fn initialize_pdftex_primitives<ET:EngineType>(state:&mut ET::State,stomach:&mut ET::Stomach,gullet:&mut ET::Gullet) {
    /*
    register_conditional!(ifpdfabsdim,state,stomach,gullet,(s,gu,cmd) =>ifpdfabsdim(s,gu,cmd));
    register_conditional!(ifpdfabsnum,state,stomach,gullet,(s,gu,cmd) =>ifpdfabsnum(s,gu,cmd));
    register_int_assign!(pdfcompresslevel,state,stomach,gullet);
    register_int_assign!(pdfdecimaldigits,state,stomach,gullet);
    register_gullet!(pdffilesize,state,stomach,gullet,(s,gu,cmd) =>pdffilesize(s,gu,cmd));
    register_dim_assign!(pdfhorigin,state,stomach,gullet);
    register_int_assign!(pdfoutput,state,stomach,gullet);
    register_int!(pdfmajorversion,state,stomach,gullet,(s,g,c) => pdfmajorversion(s,g,c));
    register_int_assign!(pdfminorversion,state,stomach,gullet);
    register_int_assign!(pdfobjcompresslevel,state,stomach,gullet);
    register_dim_assign!(pdfpageheight,state,stomach,gullet);
    register_dim_assign!(pdfpagewidth,state,stomach,gullet);
    register_int_assign!(pdfpkresolution,state,stomach,gullet);
    register_gullet!(pdfstrcmp,state,stomach,gullet,(s,gu,cmd) =>pdfstrcmp(s,gu,cmd));
    register_gullet!(pdftexrevision,state,stomach,gullet,(s,gu,cmd) =>pdftexrevision(s,gu,cmd));
    register_int!(pdftexversion,state,stomach,gullet,(s,g,c) => pdftexversion(s,g,c));
    register_dim_assign!(pdfvorigin,state,stomach,gullet);
    register_int_assign!(tracingstacklevels,state,stomach,gullet);

     */

    cmtodo!(state,stomach,gullet,efcode);
    cmtodo!(state,stomach,gullet,knaccode);
    cmtodo!(state,stomach,gullet,knbccode);
    cmtodo!(state,stomach,gullet,knbscode);
    cmtodo!(state,stomach,gullet,lpcode);
    cmtodo!(state,stomach,gullet,pdfadjustinterwordglue);
    cmtodo!(state,stomach,gullet,pdfadjustspacing);
    cmtodo!(state,stomach,gullet,pdfappendkern);
    cmtodo!(state,stomach,gullet,pdfdraftmode);
    cmtodo!(state,stomach,gullet,pdfforcepagebox);
    cmtodo!(state,stomach,gullet,pdfgamma);
    cmtodo!(state,stomach,gullet,pdfgentounicode);
    cmtodo!(state,stomach,gullet,pdfimageapplygamma);
    cmtodo!(state,stomach,gullet,pdfimagegamma);
    cmtodo!(state,stomach,gullet,pdfimagehicolor);
    cmtodo!(state,stomach,gullet,pdfimageresolution);
    cmtodo!(state,stomach,gullet,pdfinclusioncopyfonts);
    cmtodo!(state,stomach,gullet,pdfinclusionerrorlevel);
    cmtodo!(state,stomach,gullet,pdfinfoomitdate);
    cmtodo!(state,stomach,gullet,pdfomitcharset);
    cmtodo!(state,stomach,gullet,pdfomitinfodict);
    cmtodo!(state,stomach,gullet,pdfomitprocset);
    cmtodo!(state,stomach,gullet,pdfpagebox);
    cmtodo!(state,stomach,gullet,pdfprependkern);
    cmtodo!(state,stomach,gullet,pdfprotrudechars);
    cmtodo!(state,stomach,gullet,pdfsuppressptexinfo);
    cmtodo!(state,stomach,gullet,pdfsuppresswarningdupdest);
    cmtodo!(state,stomach,gullet,pdfsuppresswarningdupmap);
    cmtodo!(state,stomach,gullet,pdfsuppresswarningpagegroup);
    cmtodo!(state,stomach,gullet,pdftracingfonts);
    cmtodo!(state,stomach,gullet,pdfuniqueresname);
    cmtodo!(state,stomach,gullet,rpcode);
    cmtodo!(state,stomach,gullet,shbscode);
    cmtodo!(state,stomach,gullet,showstream);
    cmtodo!(state,stomach,gullet,stbscode);
    cmtodo!(state,stomach,gullet,tagcode);
    cmtodo!(state,stomach,gullet,pdelapsedtime);
    cmtodo!(state,stomach,gullet,pdflastannot);
    cmtodo!(state,stomach,gullet,pdflastlink);
    cmtodo!(state,stomach,gullet,pdflastobj);
    cmtodo!(state,stomach,gullet,pdflastxform);
    cmtodo!(state,stomach,gullet,pdflastximage);
    cmtodo!(state,stomach,gullet,pdflastximagecolordepth);
    cmtodo!(state,stomach,gullet,pdflastximagepages);
    cmtodo!(state,stomach,gullet,pdflastxpos);
    cmtodo!(state,stomach,gullet,pdflastypos);
    cmtodo!(state,stomach,gullet,pdfrandomseed);
    cmtodo!(state,stomach,gullet,pdfretval);
    cmtodo!(state,stomach,gullet,pdfshellescape);
    cmtodo!(state,stomach,gullet,pdfdestmargin);
    cmtodo!(state,stomach,gullet,pdfeachlinedepth);
    cmtodo!(state,stomach,gullet,pdfeachlineheight);
    cmtodo!(state,stomach,gullet,pdffirstlineheight);
    cmtodo!(state,stomach,gullet,pdfignoreddimen);
    cmtodo!(state,stomach,gullet,pdflastlinedepth);
    cmtodo!(state,stomach,gullet,pdflinkmargin);
    cmtodo!(state,stomach,gullet,pdfpxdimen);
    cmtodo!(state,stomach,gullet,pdfthreadmargin);
    cmtodo!(state,stomach,gullet,pdfpageattr);
    cmtodo!(state,stomach,gullet,pdfpageresources);
    cmtodo!(state,stomach,gullet,pdfpagesattr);
    cmtodo!(state,stomach,gullet,pdfpkmode);
    cmtodo!(state,stomach,gullet,ifincsname);
    cmtodo!(state,stomach,gullet,ifpdfprimitive);
    cmtodo!(state,stomach,gullet,leftmarginkern);
    cmtodo!(state,stomach,gullet,pdfcolorstackinit);
    cmtodo!(state,stomach,gullet,pdfcreationdate);
    cmtodo!(state,stomach,gullet,pdfescapehex);
    cmtodo!(state,stomach,gullet,pdfescapename);
    cmtodo!(state,stomach,gullet,pdfescapestring);
    cmtodo!(state,stomach,gullet,pdffiledump);
    cmtodo!(state,stomach,gullet,pdffilemoddate);
    cmtodo!(state,stomach,gullet,pdffontname);
    cmtodo!(state,stomach,gullet,pdffontobjnum);
    cmtodo!(state,stomach,gullet,pdffontsize);
    cmtodo!(state,stomach,gullet,pdfincludechars);
    cmtodo!(state,stomach,gullet,pdfinsertht);
    cmtodo!(state,stomach,gullet,pdflastmatch);
    cmtodo!(state,stomach,gullet,pdfmatch);
    cmtodo!(state,stomach,gullet,pdfmdfivesum);
    cmtodo!(state,stomach,gullet,pdfnormaldeviate);
    cmtodo!(state,stomach,gullet,pdfpageref);
    cmtodo!(state,stomach,gullet,pdftexbanner);
    cmtodo!(state,stomach,gullet,pdfunescapehex);
    cmtodo!(state,stomach,gullet,pdfuniformdeviate);
    cmtodo!(state,stomach,gullet,pdfxformname);
    cmtodo!(state,stomach,gullet,pdfximagebbox);
    cmtodo!(state,stomach,gullet,rightmarginkern);

    cmtodo!(state,stomach,gullet,letterspacefont);
    cmtodo!(state,stomach,gullet,partokenname);
    cmtodo!(state,stomach,gullet,pdfannot);
    cmtodo!(state,stomach,gullet,pdfcatalog);
    cmtodo!(state,stomach,gullet,pdfcolorstack);
    cmtodo!(state,stomach,gullet,pdfcopyfont);
    cmtodo!(state,stomach,gullet,pdfdest);
    cmtodo!(state,stomach,gullet,pdfendlink);
    cmtodo!(state,stomach,gullet,pdfendthread);
    cmtodo!(state,stomach,gullet,pdffakespace);
    cmtodo!(state,stomach,gullet,pdffontattr);
    cmtodo!(state,stomach,gullet,pdffontexpand);
    cmtodo!(state,stomach,gullet,pdfglyphtounicode);
    cmtodo!(state,stomach,gullet,pdfinfo);
    cmtodo!(state,stomach,gullet,pdfinterwordspaceoff);
    cmtodo!(state,stomach,gullet,pdfinterwordspaceon);
    cmtodo!(state,stomach,gullet,pdfliteral);
    cmtodo!(state,stomach,gullet,pdfmapfile);
    cmtodo!(state,stomach,gullet,pdfmapline);
    cmtodo!(state,stomach,gullet,pdfnames);
    cmtodo!(state,stomach,gullet,pdfnobuiltintounicode);
    cmtodo!(state,stomach,gullet,pdfnoligatures);
    cmtodo!(state,stomach,gullet,pdfobj);
    cmtodo!(state,stomach,gullet,pdfoutline);
    cmtodo!(state,stomach,gullet,pdfprimitive);
    cmtodo!(state,stomach,gullet,pdfrefobj);
    cmtodo!(state,stomach,gullet,pdfrefxform);
    cmtodo!(state,stomach,gullet,pdfrefximage);
    cmtodo!(state,stomach,gullet,pdfresettimer);
    cmtodo!(state,stomach,gullet,pdfrestore);
    cmtodo!(state,stomach,gullet,pdfrunninglinkoff);
    cmtodo!(state,stomach,gullet,pdfrunninglinkon);
    cmtodo!(state,stomach,gullet,pdfsave);
    cmtodo!(state,stomach,gullet,pdfsavepos);
    cmtodo!(state,stomach,gullet,pdfsetmatrix);
    cmtodo!(state,stomach,gullet,pdfsetrandomseed);
    cmtodo!(state,stomach,gullet,pdfspacefont);
    cmtodo!(state,stomach,gullet,pdfstartlink);
    cmtodo!(state,stomach,gullet,pdfthread);
    cmtodo!(state,stomach,gullet,pdftrailer);
    cmtodo!(state,stomach,gullet,pdftrailerid);
    cmtodo!(state,stomach,gullet,pdfstartthread);
    cmtodo!(state,stomach,gullet,pdfxform);
    cmtodo!(state,stomach,gullet,pdfximage);
    cmtodo!(state,stomach,gullet,quitvmode);

}