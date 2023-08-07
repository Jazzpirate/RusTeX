//! Implementations for the pdfTeX-specific commands.
//! Use [`initialize_pdftex_primitives`] to register all of these.

use crate::{cmtodo, debug_log, register_conditional, register_dim_assign, register_int, register_int_assign, register_unexpandable, register_expandable, catch_prim, throw};
use crate::engine::EngineType;
use crate::engine::filesystem::{File, FileSystem};
use crate::engine::gullet::Gullet;
use crate::engine::gullet::methods::{string_to_tokens, tokens_to_string};
use crate::engine::state::State;
use crate::engine::mouth::Mouth;
use crate::engine::stomach::Stomach;
use crate::tex::catcodes::CategoryCode;
use crate::tex::numbers::{Int,Dim};
use crate::tex::token::{BaseToken, Token};
use crate::tex::commands::{Command, CommandSource, ResolvedToken, TokenCont};
use crate::utils::errors::TeXError;
use crate::utils::strings::CharType;
use crate::utils::Ptr;


/// The version number returned by [`\pdftexversion`](pdftexversion) (140).
pub static PDF_TEX_VERSION: i64 = 140;
/// The version number returned by [`\pdfmajorversion`](pdfmajorversion) (1).
pub static PDF_MAJOR_VERSION: i64 = 1;
/// The version number returned by [`\pdftexrevision`](pdftexrevision) (25).
pub static PDFTEX_REVISION: i64 = 25;

// --------------------------------------------------------------------------------------------------

/// "ifpdfabsnum"
pub static IFPDFABSNUM : &str = "ifpdfabsnum";
/// `\ifpdfabsnum`: Compare the absolute values of two numbers.
pub fn ifpdfabsnum<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>) -> Result<bool,TeXError<ET::Token>> {
    debug_log!(trace=>"ifpdfabsnum");
    let i1 = catch_prim!(gullet.get_int(state) => (IFPDFABSNUM,cmd));
    let rel = match catch_prim!(gullet.get_keywords(state,vec!["<",">","="]) => (IFPDFABSNUM,cmd)) {
        None => throw!("Expected one of '<','>','='" => cmd.cause),
        Some(r) => r
    };
    let i2 = catch_prim!(gullet.get_int(state) => (IFPDFABSNUM,cmd));
    match rel {
        "<" => Ok(i1.to_i64().abs() < i2.to_i64().abs()),
        ">" => Ok(i1.to_i64().abs()>i2.to_i64().abs()),
        "=" => Ok(i1.to_i64().abs()==i2.to_i64().abs()),
        _ => unreachable!()
    }
}

/// "ifpdfabsdim"
pub static IFPDFABSDIM : &str = "ifpdfabsdim";
/// `\ifpdfabsdim`: Compare the absolute values of two dimensions.
pub fn ifpdfabsdim<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>) -> Result<bool,TeXError<ET::Token>> {
    debug_log!(trace=>"ifpdfabsdim");
    let i1 = catch_prim!(gullet.get_dim(state) => (IFPDFABSDIM,cmd));
    let rel = match catch_prim!(gullet.get_keywords(state,vec!["<",">","="]) => (IFPDFABSDIM,cmd)) {
        None => throw!("Expected one of '<','>','='" => cmd.cause),
        Some(r) => r
    };
    let i2 = catch_prim!(gullet.get_dim(state) => (IFPDFABSDIM,cmd));
    match rel {
        "<" => Ok(i1.to_sp() < i2.to_sp().abs()),
        ">" => Ok(i1.to_sp().abs()>i2.to_sp().abs()),
        "=" => Ok(i1.to_sp().abs()==i2.to_sp().abs()),
        _ => unreachable!()
    }
}

/// "pdffilesize"
pub static PDFFILESIZE : &str = "pdffilesize";
/// `\pdffilesize`: Get the size of a file (in bytes).
pub fn pdffilesize<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,fun:TokenCont<ET>) -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"pdffilesize");
    let filename = catch_prim!(gullet.get_braced_string(state) => (PDFFILESIZE,cmd));
    //gullet.get_expanded_group(state,false,false,true, &mut |_,t| Ok(ret.push(t))) => (PDFFILESIZE,cmd));
    // let filename = tokens_to_string(ret,state.get_escapechar(),state.get_catcode_scheme());
    let f = state.filesystem().get(&filename);
    let x = f.content_string();
    match &*x {
        None => Ok(()),
        Some(v) =>{
            string_to_tokens::<ET>(v.len().to_string().as_bytes(),state,fun)
        }
    }
}

/// "pdfglyphtounicode"
pub static PDFGLYPHTOUNICODE : &str = "pdfglyphtounicode";
/// `\pdfglyphtounicode`: Register the unicode codepoint of a glyph.
pub fn pdfglyphtounicode<ET:EngineType>(state: &mut ET::State, gullet:&mut ET::Gullet, cmd:CommandSource<ET>)
                                  -> Result<(), TeXError<ET::Token>> {
    debug_log!(trace=>"\\pdfglyphtounicode");
    // TODO
    catch_prim!(gullet.mouth().read_argument::<ET>(state, &mut |_,_|Ok(())) => (PDFGLYPHTOUNICODE,cmd));
    catch_prim!(gullet.mouth().read_argument::<ET>(state,&mut |_,_|Ok(())) => (PDFGLYPHTOUNICODE,cmd));
    Ok(())
}

/// "pdfstrcmp"
pub static PDFSTRCMP : &str = "pdfstrcmp";
/// `\pdfstrcmp`: Compare two strings; return -1, 0, or 1.
pub fn pdfstrcmp<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,f:TokenCont<ET>) -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"pdfstrcmp");
    let str1 = catch_prim!(gullet.get_braced_string(state) => (PDFSTRCMP,cmd));
    let str2 = catch_prim!(gullet.get_braced_string(state) => (PDFSTRCMP,cmd));
    debug_log!(trace=>"pdfstrcmp: {}=={}?",str1,str2);
    if str1==str2 {f(state,Token::new(BaseToken::Char(ET::Char::from(b'0'),CategoryCode::Other),None))}
        else if str1 < str2 { string_to_tokens::<ET>("-1".as_bytes(),state,f)}
        else {f(state,Token::new(BaseToken::Char(ET::Char::from(b'1'),CategoryCode::Other),None))}
}

/// "pdftexversion"
pub static PDFTEXVERSION : &str = "pdftexversion";
/// ` \pdftexversion`: Return the [`PDF_TEX_VERSION`] as [`Int`].
pub fn pdftexversion<ET:EngineType>(cmd:CommandSource<ET>)
    -> Result<ET::Int,TeXError<ET::Token>> {
    Ok(catch_prim!(ET::Int::from_i64(PDF_TEX_VERSION) => (PDFTEXVERSION,cmd)))
}

/// "pdfmajorversion"
pub static PDFMAJORVERSION : &str = "pdfmajorversion";
/// `\pdfmajorversion`: Return the [`PDF_MAJOR_VERSION`] as [`Int`].
pub fn pdfmajorversion<ET:EngineType>(cmd:CommandSource<ET>)
    -> Result<ET::Int,TeXError<ET::Token>> {
    Ok(catch_prim!(ET::Int::from_i64(PDF_MAJOR_VERSION) => (PDFMAJORVERSION,cmd)))
}


/// "pdftexrevision"
pub static PDFTEXREVISION : &str = "pdftexrevision";
/// `\pdftexrevision`: expands to the [`PDFTEX_REVISION`] (`25`).
pub fn pdftexrevision<ET:EngineType>(state:&mut ET::State,f:TokenCont<ET>)
    -> Result<(),TeXError<ET::Token>> {
    string_to_tokens::<ET>(PDFTEX_REVISION.to_string().as_bytes(),state,f)
}

/// Initialize a TeX engine with default implementations for all pdfTeX primitives.
pub fn initialize_pdftex_primitives<ET:EngineType>(state:&mut ET::State,stomach:&mut ET::Stomach,gullet:&mut ET::Gullet) {
    register_conditional!(ifpdfabsdim,state,stomach,gullet,(s,gu,cmd) =>ifpdfabsdim::<ET>(s,gu,cmd));
    register_conditional!(ifpdfabsnum,state,stomach,gullet,(s,gu,cmd) =>ifpdfabsnum::<ET>(s,gu,cmd));
    register_int_assign!(pdfcompresslevel,state,stomach,gullet);
    register_int_assign!(pdfdecimaldigits,state,stomach,gullet);
    register_expandable!(pdffilesize,state,stomach,gullet,(s,gu,cmd,f) =>pdffilesize::<ET>(s,gu,cmd,f));
    register_int_assign!(pdfgentounicode,state,stomach,gullet);
    register_unexpandable!(pdfglyphtounicode,state,stomach,gullet,(s,gu,cmd) =>pdfglyphtounicode::<ET>(s,gu,cmd));
    register_dim_assign!(pdfhorigin,state,stomach,gullet);
    register_int_assign!(pdfoutput,state,stomach,gullet);
    register_int!(pdfmajorversion,state,stomach,gullet,(s,g,c) => pdfmajorversion::<ET>(c));
    register_int_assign!(pdfminorversion,state,stomach,gullet);
    register_int_assign!(pdfobjcompresslevel,state,stomach,gullet);
    register_dim_assign!(pdfpageheight,state,stomach,gullet);
    register_dim_assign!(pdfpagewidth,state,stomach,gullet);
    register_int_assign!(pdfpkresolution,state,stomach,gullet);
    register_expandable!(pdfstrcmp,state,stomach,gullet,(s,gu,cmd,f) =>pdfstrcmp::<ET>(s,gu,cmd,f));
    register_expandable!(pdftexrevision,state,stomach,gullet,(s,gu,cmd,f) =>pdftexrevision::<ET>(s,f));
    register_int!(pdftexversion,state,stomach,gullet,(s,g,c) => pdftexversion::<ET>(c));
    register_dim_assign!(pdfvorigin,state,stomach,gullet);
    register_int_assign!(tracingstacklevels,state,stomach,gullet);

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