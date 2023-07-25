use crate::{register_dim_assign, register_int_assign};
use crate::engine::gullet::Gullet;
use crate::engine::state::State;
use crate::engine::stomach::Stomach;
use crate::tex::token::Token;
use crate::utils::strings::CharType;
use crate::utils::Ptr;

/* TODO

efcode
knaccode
knbccode
knbscode
lpcode
pdfadjustinterwordglue
pdfadjustspacing
pdfappendkern
pdfdraftmode
pdfforcepagebox
pdfgamma
pdfgentounicode
pdfimageapplygamma
pdfimagegamma
pdfimagehicolor
pdfimageresolution
pdfinclusioncopyfonts
pdfinclusionerrorlevel
pdfinfoomitdate
pdfmajorversion
pdfomitcharset
pdfomitinfodict
pdfomitprocset
pdfpagebox
pdfprependkern
pdfprotrudechars
pdfsuppressptexinfo
pdfsuppresswarningdupdest
pdfsuppresswarningdupmap
pdfsuppresswarningpagegroup
pdftracingfonts
pdfuniqueresname
rpcode
shbscode
showstream
stbscode
tagcode
tracinglostchars
tracingstacklevels

pdfelapsedtime
pdflastannot
pdflastlink
pdflastobj
pdflastxform
pdflastximage
pdflastximagecolordepth
pdflastximagepages
pdflastxpos
pdflastypos
pdfrandomseed
pdfretval
pdfshellescape
pdftexversion

pdfdestmargin
pdfeachlinedepth
pdfeachlineheight
pdffirstlineheight
pdfignoreddimen
pdflastlinedepth
pdflinkmargin
pdfpxdimen
pdfthreadmargin

pdfpageattr
pdfpageresources
pdfpagesattr
pdfpkmode

expanded
ifincsname
ifpdfabsdim
ifpdfabsnum
ifpdfprimitive
leftmarginkern
pdfcolorstackinit
pdfcreationdate
pdfescapehex
pdfescapename
pdfescapestring
pdffiledump
pdffilemoddate
pdffilesize
pdffontname
pdffontobjnum
pdffontsize
pdfincludechars
pdfinsertht
pdflastmatch
pdfmatch
pdfmdfivesum
pdfnormaldeviate
pdfpageref
pdfstrcmp
pdftexbanner
pdftexrevision
pdfunescapehex
pdfuniformdeviate
pdfxformname
pdfximagebbox
rightmarginkern

letterspacefont
partokenname
pdfannot
pdfcatalog
pdfcolorstack
pdfcopyfont
pdfdest
pdfendlink
pdfendthread
pdffakespace
pdffontattr
pdffontexpand
pdfglyphtounicode
pdfinfo
pdfinterwordspaceoff
pdfinterwordspaceon
pdfliteral
pdfmapfile
pdfmapline
pdfnames
pdfnobuiltintounicode
pdfnoligatures
pdfobj
pdfoutline
pdfprimitive
pdfrefobj
pdfrefxform
pdfrefximage
pdfresettimer
pdfrestore
pdfrunninglinkoff
pdfrunninglinkon
pdfsave
pdfsavepos
pdfsetmatrix
pdfsetrandomseed
pdfspacefont
pdfstartlink
pdfthread
pdftrailer
pdftrailerid
pdfstartthread
pdfxform
pdfximage
quitvmode
 */

//TODO: pdffilesize

pub fn initialize_pdftex_primitives<T:Token,S:State<T>,Gu:Gullet<T,S=S>,Sto:Stomach<T,S=S,Gu=Gu>>(state:&mut S,stomach:&mut Sto,gullet:&mut Gu) {
    register_int_assign!(pdfcompresslevel,state,stomach,gullet);
    register_int_assign!(pdfdecimaldigits,state,stomach,gullet);
    register_dim_assign!(pdfhorigin,state,stomach,gullet);
    register_int_assign!(pdfoutput,state,stomach,gullet);
    register_int_assign!(pdfminorversion,state,stomach,gullet);
    register_int_assign!(pdfobjcompresslevel,state,stomach,gullet);
    register_dim_assign!(pdfpageheight,state,stomach,gullet);
    register_dim_assign!(pdfpagewidth,state,stomach,gullet);
    register_int_assign!(pdfpkresolution,state,stomach,gullet);
    register_dim_assign!(pdfvorigin,state,stomach,gullet);
}