//! Implementations for the pdfTeX-specific commands.
//! Use [`initialize_pdftex_primitives`] to register all of these.

use crate::{cmtodo, debug_log, register_conditional, register_dim_assign, register_int, register_int_assign, register_unexpandable, register_expandable, catch_prim, throw};
use crate::engine::{EngineMut, EngineType};
use crate::engine::filesystem::{File, FileSystem};
use crate::engine::gullet::Gullet;
use crate::engine::gullet::methods::{tokens_to_string};
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
pub fn ifpdfabsnum<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>) -> Result<bool,TeXError<ET>> {
    debug_log!(trace=>"ifpdfabsnum");
    let i1 = catch_prim!(engine.get_int() => (IFPDFABSNUM,cmd));
    let rel = match catch_prim!(engine.is_next_char_one_of(&super::tex::LGE) => (IFPDFABSNUM,cmd)) {
        None => throw!("Expected one of '<','>','='" => cmd.cause),
        Some(r) => r
    };
    let i2 = catch_prim!(engine.get_int() => (IFPDFABSNUM,cmd));
    match rel {
        b'<' => Ok(i1.to_i64().abs() < i2.to_i64().abs()),
        b'>' => Ok(i1.to_i64().abs()>i2.to_i64().abs()),
        b'=' => Ok(i1.to_i64().abs()==i2.to_i64().abs()),
        _ => unreachable!()
    }
}

/// "ifpdfabsdim"
pub static IFPDFABSDIM : &str = "ifpdfabsdim";
/// `\ifpdfabsdim`: Compare the absolute values of two dimensions.
pub fn ifpdfabsdim<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>) -> Result<bool,TeXError<ET>> {
    debug_log!(trace=>"ifpdfabsdim");
    let i1 = catch_prim!(engine.get_dim() => (IFPDFABSDIM,cmd));
    let rel = match catch_prim!(engine.is_next_char_one_of(&super::tex::LGE) => (IFPDFABSDIM,cmd)) {
        None => throw!("Expected one of '<','>','='" => cmd.cause),
        Some(r) => r
    };
    let i2 = catch_prim!(engine.get_dim() => (IFPDFABSDIM,cmd));
    match rel {
        b'<' => Ok(i1.to_sp() < i2.to_sp().abs()),
        b'>' => Ok(i1.to_sp().abs()>i2.to_sp().abs()),
        b'=' => Ok(i1.to_sp().abs()==i2.to_sp().abs()),
        _ => unreachable!()
    }
}

/// "pdffilesize"
pub static PDFFILESIZE : &str = "pdffilesize";
/// `\pdffilesize`: Get the size of a file (in bytes).
pub fn pdffilesize<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, fun:TokenCont<ET>) -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"pdffilesize");
    let mut filename = engine.memory.get_string();
    catch_prim!(engine.get_braced_string(&mut filename) => (PDFFILESIZE,cmd));
    //gullet.get_expanded_group(state,false,false,true, &mut |_,t| Ok(ret.push(t))) => (PDFFILESIZE,cmd));
    // let filename = tokens_to_string(ret,state.get_escapechar(),state.get_catcode_scheme());
    let f = engine.state.filesystem().get(&filename);
    engine.memory.return_string(filename);
    let x = f.content_string();
    match &*x {
        None => Ok(()),
        Some(v) =>{
            engine.string_to_tokens(v.len().to_string().as_bytes(),fun)
        }
    }
}

/// "pdfglyphtounicode"
pub static PDFGLYPHTOUNICODE : &str = "pdfglyphtounicode";
/// `\pdfglyphtounicode`: Register the unicode codepoint of a glyph.
pub fn pdfglyphtounicode<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>)
                                        -> Result<(), TeXError<ET>> {
    debug_log!(trace=>"\\pdfglyphtounicode");
    // TODO
    catch_prim!(engine.get_argument(&mut |_,_|Ok(())) => (PDFGLYPHTOUNICODE,cmd));
    catch_prim!(engine.get_argument(&mut |_,_|Ok(())) => (PDFGLYPHTOUNICODE,cmd));
    Ok(())
}

/// "pdfstrcmp"
pub static PDFSTRCMP : &str = "pdfstrcmp";
/// `\pdfstrcmp`: Compare two strings; return -1, 0, or 1.
    pub fn pdfstrcmp<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, f:TokenCont<ET>) -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"pdfstrcmp");
    let mut str1 = engine.memory.get_string();
    let mut str2 = engine.memory.get_string();
    catch_prim!(engine.get_braced_string(&mut str1) => (PDFSTRCMP,cmd));
    catch_prim!(engine.get_braced_string(&mut str2) => (PDFSTRCMP,cmd));
    debug_log!(trace=>"pdfstrcmp: {}=={}?",str1,str2);
    if str1==str2 {f(engine,Token::new(BaseToken::Char(ET::Char::from(b'0'),CategoryCode::Other),None))?}
        else if str1 < str2 { engine.string_to_tokens("-1".as_bytes(),f)?}
        else {f(engine,Token::new(BaseToken::Char(ET::Char::from(b'1'),CategoryCode::Other),None))?}
    engine.memory.return_string(str1);
    engine.memory.return_string(str2);
    Ok(())
}

/// "pdftexversion"
pub static PDFTEXVERSION : &str = "pdftexversion";
/// ` \pdftexversion`: Return the [`PDF_TEX_VERSION`] as [`Int`].
pub fn pdftexversion<ET:EngineType>(cmd:CommandSource<ET>)
    -> Result<ET::Int,TeXError<ET>> {
    Ok(catch_prim!(ET::Int::from_i64(PDF_TEX_VERSION) => (PDFTEXVERSION,cmd)))
}

/// "pdfmajorversion"
pub static PDFMAJORVERSION : &str = "pdfmajorversion";
/// `\pdfmajorversion`: Return the [`PDF_MAJOR_VERSION`] as [`Int`].
pub fn pdfmajorversion<ET:EngineType>(cmd:CommandSource<ET>)
    -> Result<ET::Int,TeXError<ET>> {
    Ok(catch_prim!(ET::Int::from_i64(PDF_MAJOR_VERSION) => (PDFMAJORVERSION,cmd)))
}


/// "pdftexrevision"
pub static PDFTEXREVISION : &str = "pdftexrevision";
/// `\pdftexrevision`: expands to the [`PDFTEX_REVISION`] (`25`).
pub fn pdftexrevision<ET:EngineType>(engine:&mut EngineMut<ET>,f:TokenCont<ET>)
    -> Result<(),TeXError<ET>> {
    engine.string_to_tokens(PDFTEX_REVISION.to_string().as_bytes(),f)
}

/// Initialize a TeX engine with default implementations for all pdfTeX primitives.
pub fn initialize_pdftex_primitives<ET:EngineType>(engine:&mut EngineMut<ET>) {
    register_conditional!(ifpdfabsdim,engine,(e,cmd) =>ifpdfabsdim::<ET>(e,cmd));
    register_conditional!(ifpdfabsnum,engine,(e,cmd) =>ifpdfabsnum::<ET>(e,cmd));
    register_int_assign!(pdfcompresslevel,engine);
    register_int_assign!(pdfdecimaldigits,engine);
    register_expandable!(pdffilesize,engine,(e,cmd,f) =>pdffilesize::<ET>(e,cmd,f));
    register_int_assign!(pdfgentounicode,engine);
    register_unexpandable!(pdfglyphtounicode,engine,false,(e,cmd) =>pdfglyphtounicode::<ET>(e,cmd));
    register_dim_assign!(pdfhorigin,engine);
    register_int_assign!(pdfoutput,engine);
    register_int!(pdfmajorversion,engine,(_,c) => pdfmajorversion::<ET>(c));
    register_int_assign!(pdfminorversion,engine);
    register_int_assign!(pdfobjcompresslevel,engine);
    register_dim_assign!(pdfpageheight,engine);
    register_dim_assign!(pdfpagewidth,engine);
    register_int_assign!(pdfpkresolution,engine);
    register_expandable!(pdfstrcmp,engine,(e,cmd,f) =>pdfstrcmp::<ET>(e,cmd,f));
    register_expandable!(pdftexrevision,engine,(e,_,f) =>pdftexrevision::<ET>(e,f));
    register_int!(pdftexversion,engine,(_,c) => pdftexversion::<ET>(c));
    register_dim_assign!(pdfvorigin,engine);
    register_int_assign!(tracingstacklevels,engine);

    cmtodo!(engine,efcode);
    cmtodo!(engine,knaccode);
    cmtodo!(engine,knbccode);
    cmtodo!(engine,knbscode);
    cmtodo!(engine,lpcode);
    cmtodo!(engine,pdfadjustinterwordglue);
    cmtodo!(engine,pdfadjustspacing);
    cmtodo!(engine,pdfappendkern);
    cmtodo!(engine,pdfdraftmode);
    cmtodo!(engine,pdfforcepagebox);
    cmtodo!(engine,pdfgamma);
    cmtodo!(engine,pdfimageapplygamma);
    cmtodo!(engine,pdfimagegamma);
    cmtodo!(engine,pdfimagehicolor);
    cmtodo!(engine,pdfimageresolution);
    cmtodo!(engine,pdfinclusioncopyfonts);
    cmtodo!(engine,pdfinclusionerrorlevel);
    cmtodo!(engine,pdfinfoomitdate);
    cmtodo!(engine,pdfomitcharset);
    cmtodo!(engine,pdfomitinfodict);
    cmtodo!(engine,pdfomitprocset);
    cmtodo!(engine,pdfpagebox);
    cmtodo!(engine,pdfprependkern);
    cmtodo!(engine,pdfprotrudechars);
    cmtodo!(engine,pdfsuppressptexinfo);
    cmtodo!(engine,pdfsuppresswarningdupdest);
    cmtodo!(engine,pdfsuppresswarningdupmap);
    cmtodo!(engine,pdfsuppresswarningpagegroup);
    cmtodo!(engine,pdftracingfonts);
    cmtodo!(engine,pdfuniqueresname);
    cmtodo!(engine,rpcode);
    cmtodo!(engine,shbscode);
    cmtodo!(engine,showstream);
    cmtodo!(engine,stbscode);
    cmtodo!(engine,tagcode);
    cmtodo!(engine,pdelapsedtime);
    cmtodo!(engine,pdflastannot);
    cmtodo!(engine,pdflastlink);
    cmtodo!(engine,pdflastobj);
    cmtodo!(engine,pdflastxform);
    cmtodo!(engine,pdflastximage);
    cmtodo!(engine,pdflastximagecolordepth);
    cmtodo!(engine,pdflastximagepages);
    cmtodo!(engine,pdflastxpos);
    cmtodo!(engine,pdflastypos);
    cmtodo!(engine,pdfrandomseed);
    cmtodo!(engine,pdfretval);
    cmtodo!(engine,pdfshellescape);
    cmtodo!(engine,pdfdestmargin);
    cmtodo!(engine,pdfeachlinedepth);
    cmtodo!(engine,pdfeachlineheight);
    cmtodo!(engine,pdffirstlineheight);
    cmtodo!(engine,pdfignoreddimen);
    cmtodo!(engine,pdflastlinedepth);
    cmtodo!(engine,pdflinkmargin);
    cmtodo!(engine,pdfpxdimen);
    cmtodo!(engine,pdfthreadmargin);
    cmtodo!(engine,pdfpageattr);
    cmtodo!(engine,pdfpageresources);
    cmtodo!(engine,pdfpagesattr);
    cmtodo!(engine,pdfpkmode);
    cmtodo!(engine,ifincsname);
    cmtodo!(engine,ifpdfprimitive);
    cmtodo!(engine,leftmarginkern);
    cmtodo!(engine,pdfcolorstackinit);
    cmtodo!(engine,pdfcreationdate);
    cmtodo!(engine,pdfescapehex);
    cmtodo!(engine,pdfescapename);
    cmtodo!(engine,pdfescapestring);
    cmtodo!(engine,pdffiledump);
    cmtodo!(engine,pdffilemoddate);
    cmtodo!(engine,pdffontname);
    cmtodo!(engine,pdffontobjnum);
    cmtodo!(engine,pdffontsize);
    cmtodo!(engine,pdfincludechars);
    cmtodo!(engine,pdfinsertht);
    cmtodo!(engine,pdflastmatch);
    cmtodo!(engine,pdfmatch);
    cmtodo!(engine,pdfmdfivesum);
    cmtodo!(engine,pdfnormaldeviate);
    cmtodo!(engine,pdfpageref);
    cmtodo!(engine,pdftexbanner);
    cmtodo!(engine,pdfunescapehex);
    cmtodo!(engine,pdfuniformdeviate);
    cmtodo!(engine,pdfxformname);
    cmtodo!(engine,pdfximagebbox);
    cmtodo!(engine,rightmarginkern);

    cmtodo!(engine,letterspacefont);
    cmtodo!(engine,partokenname);
    cmtodo!(engine,pdfannot);
    cmtodo!(engine,pdfcatalog);
    cmtodo!(engine,pdfcolorstack);
    cmtodo!(engine,pdfcopyfont);
    cmtodo!(engine,pdfdest);
    cmtodo!(engine,pdfendlink);
    cmtodo!(engine,pdfendthread);
    cmtodo!(engine,pdffakespace);
    cmtodo!(engine,pdffontattr);
    cmtodo!(engine,pdffontexpand);
    cmtodo!(engine,pdfinfo);
    cmtodo!(engine,pdfinterwordspaceoff);
    cmtodo!(engine,pdfinterwordspaceon);
    cmtodo!(engine,pdfliteral);
    cmtodo!(engine,pdfmapfile);
    cmtodo!(engine,pdfmapline);
    cmtodo!(engine,pdfnames);
    cmtodo!(engine,pdfnobuiltintounicode);
    cmtodo!(engine,pdfnoligatures);
    cmtodo!(engine,pdfobj);
    cmtodo!(engine,pdfoutline);
    cmtodo!(engine,pdfprimitive);
    cmtodo!(engine,pdfrefobj);
    cmtodo!(engine,pdfrefxform);
    cmtodo!(engine,pdfrefximage);
    cmtodo!(engine,pdfresettimer);
    cmtodo!(engine,pdfrestore);
    cmtodo!(engine,pdfrunninglinkoff);
    cmtodo!(engine,pdfrunninglinkon);
    cmtodo!(engine,pdfsave);
    cmtodo!(engine,pdfsavepos);
    cmtodo!(engine,pdfsetmatrix);
    cmtodo!(engine,pdfsetrandomseed);
    cmtodo!(engine,pdfspacefont);
    cmtodo!(engine,pdfstartlink);
    cmtodo!(engine,pdfthread);
    cmtodo!(engine,pdftrailer);
    cmtodo!(engine,pdftrailerid);
    cmtodo!(engine,pdfstartthread);
    cmtodo!(engine,pdfxform);
    cmtodo!(engine,pdfximage);
    cmtodo!(engine,quitvmode);

}