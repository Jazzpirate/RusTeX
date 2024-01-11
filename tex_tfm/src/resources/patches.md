# Patches for font-glyph-maps

This crate tries to map codepoints in `.tfm` files to actual unicode characters. It does
so by parsing the `pdftex.map` file, which contains a list of properties for each 
`.tfm` file; in particular, it lists a `.enc` file and/or a `.pfa` or `.pfb` file.
These contain (ideally) glyph names for each glyph in the font, which is then mapped to
a unicode character by using the `glyphmap.txt` file, which contains a list of
glyph names and their unicode character (one per line, separated by a space).

Ideally, that would always yield the correct unicode character, but glyphnames are
not perfectly standardized, some of the glyph names in the `.pfb` or `.enc` files
might be wrong, or the `.map` file may yield a (similar but) wrong `.enc` file.

This document allows us to patch those problems by manually specifying the correct
values where the automatic process fails, in a conveniently human-readable and editable
format.

Additionally, for some purposes - such as HTML conversion - we also want to know
the "**font modifiers**" (bold, italic, smallcaps, etc.) of a `.tfm` font. 
These are largely inferred from the name and/or potentially listed properties in the 
`.pfa` or `.pfb` file, but this is based on heuristics and might be wrong.

This very file is parsed *at compile time* to generate the relevant code reflecting
the patches. Syntactic errors in this file will therefore lead to compilation errors already.

### Font Information

The following table maps `.tfm` filenames to:
- a sequence of modifiers, identified via the following characters:
  - `b`: bold
  - `i`: italic
  - `o`: oblique/slanted
  - `s`: sans serif
  - `m`: monospaced
  - `c`: small-caps
  - `S`: script
  - `B`: blackboard
  - `f`: fraktur
- A glyph table name listed below (optional), if we want to override the one
  generated from the `.pfb` or `.enc` file listed in the `.map` file
- A font name and link to an external URL providing the font, e.g. for usage in 
  HTML (optional)

Multiple `.tfm` filenames are allowed; separated by `,`; leading whitespaces are ignored.

For example, the first line in the table below tells us that the`.tfm` files `cmssbx12`, `cmssbx17`, 
`cmssbx8`, and `cmssbx9` are all bold sans-serif fonts (`bs`), use the glyph table `computer modern 1`
(see next section), and are available as the web-compatible font `Latin Modern Sans` at the given URL.


<!--- START -->
| `.tfm` name                                                                                                                                                                                                                                                     | Modifiers | Table (Optional)  | External Font (Optional)                                                                                                                                           |
|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-----------|-------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| cmr5, cmr6, cmr7, cmr8, cmr9, cmr10, cmr12, cmr17                                                                                                                                                                                                               |           | computer modern 1 | Computer Modern Serif https://cdn.jsdelivr.net/gh/dreampulse/computer-modern-web-font@master/font/Serif/cmun-serif.css                                             |
| cmsy5, cmsy6, cmsy7, cmsy8, cmsy9, cmsy10, tcrm1000,ecrm0500,ecrm0600,ecrm0700,ecrm0800,ecrm0900,ecrm1000,ecrm1095, ecrm1200, ecrm1440,ecrm1728,ecrm2074,ecrm2488,ecrm2986,ecrm3583                                                                             |           |                   | Computer Modern Serif https://cdn.jsdelivr.net/gh/dreampulse/computer-modern-web-font@master/font/Serif/cmun-serif.css                                             |
| cmti7, cmti8, cmti9, cmti10, cmti12                                                                                                                                                                                                                             | i         |                   | Computer Modern Serif https://cdn.jsdelivr.net/gh/dreampulse/computer-modern-web-font@master/font/Serif/cmun-serif.css                                             |
| cmb10, cmbx5, cmbx6, cmbx7, cmbx8, cmbx9, cmbx10, cmbx12                                                                                                                                                                                                        | b         |                   | Computer Modern Serif https://cdn.jsdelivr.net/gh/dreampulse/computer-modern-web-font@master/font/Serif/cmun-serif.css                                             |
| tcbx1000,ecbx0500,ecbx0600,ecbx0700,ecbx0800,ecbx0900,ecbx1000,ecbx1095, ecbx1200, ecbx1440,ecbx1728,ecbx2074,ecbx2488,ecbx2986,ecbx3583                                                                                                                        | b         |                   | Computer Modern Serif https://cdn.jsdelivr.net/gh/dreampulse/computer-modern-web-font@master/font/Serif/cmun-serif.css                                             |
| ecti0500,ecti0600,ecti0700,ecti0800,ecti0900,ecti1000,ecti1095, ecti1200, ecti1440,ecti1728,ecti2074,ecti2488,ecti2986,ecti3583                                                                                                                                 | i         |                   | Computer Modern Serif https://cdn.jsdelivr.net/gh/dreampulse/computer-modern-web-font@master/font/Serif/cmun-serif.css                                             |
| eccc0500,eccc0600,eccc0700,eccc0800,eccc0900,eccc1000,eccc1095, eccc1200, eccc1440,eccc1728,eccc2074,eccc2488,eccc2986,eccc3583                                                                                                                                 | c         |                   | Computer Modern Serif https://cdn.jsdelivr.net/gh/dreampulse/computer-modern-web-font@master/font/Serif/cmun-serif.css                                             |
| cmbxti10                                                                                                                                                                                                                                                        | bi        |                   | Computer Modern Serif https://cdn.jsdelivr.net/gh/dreampulse/computer-modern-web-font@master/font/Serif/cmun-serif.css                                             |
| cmcsc10                                                                                                                                                                                                                                                         | c         |                   | Computer Modern Serif https://cdn.jsdelivr.net/gh/dreampulse/computer-modern-web-font@master/font/Serif/cmun-serif.css                                             |
| cmsl8, cmsl9, cmsl10, cmsl12                                                                                                                                                                                                                                    | o         |                   | Computer Modern Serif Slanted https://cdn.jsdelivr.net/gh/dreampulse/computer-modern-web-font@master/font/Serif%20Slanted/cmun-serif-slanted.css                   |
| cmbxsl10                                                                                                                                                                                                                                                        | bo        |                   | Computer Modern Serif Slanted https://cdn.jsdelivr.net/gh/dreampulse/computer-modern-web-font@master/font/Serif%20Slanted/cmun-serif-slanted.css                   |
| cmtt8, cmtt9, cmtt10, cmtt12                                                                                                                                                                                                                                    | m         |                   | Computer Modern Typewriter https://cdn.jsdelivr.net/gh/dreampulse/computer-modern-web-font@master/font/Typewriter/cmun-typewriter.css                              |
| cmitt10                                                                                                                                                                                                                                                         | mi        |                   | Computer Modern Typewriter https://cdn.jsdelivr.net/gh/dreampulse/computer-modern-web-font@master/font/Typewriter/cmun-typewriter.css                              |
| cmsltt10                                                                                                                                                                                                                                                        | mo        |                   | Computer Modern Typewriter https://cdn.jsdelivr.net/gh/dreampulse/computer-modern-web-font@master/font/Typewriter/cmun-typewriter.css                              |
| cmtcsc10                                                                                                                                                                                                                                                        | mc        |                   | Computer Modern Typewriter https://cdn.jsdelivr.net/gh/dreampulse/computer-modern-web-font@master/font/Typewriter/cmun-typewriter.css                              |
| cmvtt10                                                                                                                                                                                                                                                         |           |                   | Computer Modern Typewriter Variable https://cdn.jsdelivr.net/gh/dreampulse/computer-modern-web-font@master/font/Typewriter%20Variable/cmun-typewriter-variable.css |
| cmss8, cmss9, cmss10, cmss12                                                                                                                                                                                                                                    | s         |                   | Computer Modern Sans https://cdn.jsdelivr.net/gh/dreampulse/computer-modern-web-font@master/font/Sans/cmun-sans.css                                                |
| cmssdc10                                                                                                                                                                                                                                                        | s         |                   | Computer Modern Sans Demi-Condensed https://cdn.jsdelivr.net/gh/dreampulse/computer-modern-web-font@master/font/Sans%20Demi-Condensed/cmun-sans-demicondensed.css  |
| cmssbx8, cmssbx9, cmssbx10, cmssbx12, cmssbx17                                                                                                                                                                                                                  | sb        |                   | Computer Modern Sans https://cdn.jsdelivr.net/gh/dreampulse/computer-modern-web-font@master/font/Sans/cmun-sans.css                                                |
| ecss0500,ecss0600,ecss0700,ecss0800,ecss0900,ecss1000,ecss1095, ecss1200, ecss1440,ecss1728,ecss2074,ecss2488,ecss2986,ecss3583,tcss0500,tcss0600,tcss0700,tcss0800,tcss0900,tcss1000,tcss1095, tcss1200, tcss1440,tcss1728,tcss2074,tcss2488,tcss2986,tcss3583 | s         |                   | Computer Modern Sans https://cdn.jsdelivr.net/gh/dreampulse/computer-modern-web-font@master/font/Sans/cmun-sans.css                                                |
| ecsx0500,ecsx0600,ecsx0700,ecsx0800,ecsx0900,ecsx1000,ecsx1095, ecsx1200, ecsx1440,ecsx1728,ecsx2074,ecsx2488,ecsx2986,ecsx3583                                                                                                                                 | sb        |                   | Computer Modern Sans https://cdn.jsdelivr.net/gh/dreampulse/computer-modern-web-font@master/font/Sans/cmun-sans.css                                                |
| ecsi0500,ecsi0600,ecsi0700,ecsi0800,ecsi0900,ecsi1000,ecsi1095, ecsi1200, ecsi1440,ecsi1728,ecsi2074,ecsi2488,ecsi2986,ecsi3583                                                                                                                                 | si        |                   | Computer Modern Sans https://cdn.jsdelivr.net/gh/dreampulse/computer-modern-web-font@master/font/Sans/cmun-sans.css                                                |
| ectt0500,ectt0600,ectt0700,ectt0800,ectt0900,ectt1000,ectt1095, ectt1200, ectt1440,ectt1728,ectt2074,ectt2488,ectt2986,ectt3583                                                                                                                                 | m         |                   | Computer Modern Typewriter https://cdn.jsdelivr.net/gh/dreampulse/computer-modern-web-font@master/font/Typewriter/cmun-typewriter.css                              |
| ecsl0500,ecsl0600,ecsl0700,ecsl0800,ecsl0900,ecsl1000,ecsl1095, ecsl1200, ecsl1440,ecsl1728,ecsl2074,ecsl2488,ecsl2986,ecsl3583,tcsl0500,tcsl0600,tcsl0700,tcsl0800,tcsl0900,tcsl1000,tcsl1095, tcsl1200, tcsl1440,tcsl1728,tcsl2074,tcsl2488,tcsl2986,tcsl3583 | o         |                   | Computer Modern Serif Slanted https://cdn.jsdelivr.net/gh/dreampulse/computer-modern-web-font@master/font/Serif%20Slanted/cmun-serif-slanted.css                   |
| ecso0500,ecso0600,ecso0700,ecso0800,ecso0900,ecso1000,ecso1095, ecso1200, ecso1440,ecso1728,ecso2074,ecso2488,ecso2986,ecso3583,tcso0500,tcso0600,tcso0700,tcso0800,tcso0900,tcso1000,tcso1095, tcso1200, tcso1440,tcso1728,tcso2074,tcso2488,tcso2986,tcso3583 | sbo       |                   | Computer Modern Sans https://cdn.jsdelivr.net/gh/dreampulse/computer-modern-web-font@master/font/Sans/cmun-sans.css                                                |
| ecbi0500,ecbi0600,ecbi0700,ecbi0800,ecbi0900,ecbi1000,ecbi1095, ecbi1200, ecbi1440,ecbi1728,ecbi2074,ecbi2488,ecbi2986,ecbi3583,tcbi0500,tcbi0600,tcbi0700,tcbi0800,tcbi0900,tcbi1000,tcbi1095, tcbi1200, tcbi1440,tcbi1728,tcbi2074,tcbi2488,tcbi2986,tcbi3583 | bi        |                   | Computer Modern Serif https://cdn.jsdelivr.net/gh/dreampulse/computer-modern-web-font@master/font/Serif/cmun-serif.css                                             |
| ec-lmr5, ec-lmr6, ec-lmr7, ec-lmr8, ec-lmr9, ec-lmr10, ec-lmr12, ec-lmr17                                                                                                                                                                                       |           |                   | LMRoman10 https://fonts.cdnfonts.com/css/latin-modern-roman                                                                                                        |
| ts1-lmr5, ts1-lmr6, ts1-lmr7, ts1-lmr8, ts1-lmr9, ts1-lmr10, ts1-lmr12, ts1-lmr17                                                                                                                                                                               |           |                   | LMRoman10 https://fonts.cdnfonts.com/css/latin-modern-roman                                                                                                        |
| ec-lmri5, ec-lmri6, ec-lmri7, ec-lmri8, ec-lmri9, ec-lmri10, ec-lmri12, ec-ilmr17                                                                                                                                                                               | i         |                   | LMRoman10 https://fonts.cdnfonts.com/css/latin-modern-roman                                                                                                        |
| ec-lmbx5, ec-lmbx6, ec-lmbx7, ec-lmbx8, ec-lmbx9, ec-lmbx10, ec-lmbx12, ec-lmbx17                                                                                                                                                                               | b         |                   | LMRoman10 https://fonts.cdnfonts.com/css/latin-modern-roman                                                                                                        |
| ec-lmss8, ec-lmss9, ec-lmss10, ec-lmss12, ec-lmss17                                                                                                                                                                                                             | s         |                   | LMSans10 https://fonts.cdnfonts.com/css/latin-modern-sans                                                                                                          |
| ec-lmcsc10                                                                                                                                                                                                                                                      | c         |                   | LMRomanCaps10 https://fonts.cdnfonts.com/css/latin-modern-roman                                                                                                    |
| ec-lmcsco10                                                                                                                                                                                                                                                     | co        |                   | LMRomanCaps10 https://fonts.cdnfonts.com/css/latin-modern-roman                                                                                                    |
| ec-lmtt8, ec-lmtt9, ec-lmtt10, ec-lmtt12                                                                                                                                                                                                                        | m         |                   | LMMono10 https://fonts.cdnfonts.com/css/latin-modern-mono                                                                                                          |
| ec-lmtti10                                                                                                                                                                                                                                                      | mi        |                   | LMMonoSlant10 https://fonts.cdnfonts.com/css/latin-modern-mono                                                                                                     |
| MnSymbolS5, MnSymbolS6, MnSymbolS7, MnSymbolS8, MnSymbolS9, MnSymbolS10, MnSymbolS12                                                                                                                                                                            | S         |                   |                                                                                                                                                                    |
| MnSymbolS-Bold5, MnSymbolS-Bold6, MnSymbolS-Bold7, MnSymbolS-Bold8, MnSymbolS-Bold9, MnSymbolS-Bold10, MnSymbolS-Bold12                                                                                                                                         | bS        |                   |                                                                                                                                                                    |
| cmmi5, cmmi6, cmmi7, cmmi8, cmmi9, cmmi10, cmmi12                                                                                                                                                                                                               | i         |                   |                                                                                                                                                                    |
<!--- END -->

### Glyph Maps

A glyph map is given as a 16x16 table of unicode characters. Each entry is either:
- a unicode codepoint denoted by `\uXXXX`, where `XXXX` is a 4-digit hexadecimal number, or
- a string enclosed in backticks, provided that *somewhere* in the `glyphmap.txt`, there is a named glyph mapping to that string
  (likely always the case if it's a single character or a common ligature), or
- the name of a glyph in the existing `glyphmap.txt` prefixed with `/`; e.g. `/Omega`, or
- empty; signifying that there is no glyph at that position. This is eqivalent to `/.notdef`.

Before each table, a name is expected as a bullet point in a separate line, i.e. `- <name>`.

<!--- START -->
- computer modern 1

| \_x\_  | 0   | 1   | 2    | 3   | 4   | 5   | 6   | 7   | 8   | 9   | A   | B    | C    | D    | E     | F     |
|--------|-----|-----|------|-----|-----|-----|-----|-----|-----|-----|-----|------|------|------|-------|-------|
| **0x** | `Γ` | `∆` | `Θ`  | `Λ` | `Ξ` | `Π` | `Σ` | `Υ` | `Φ` | `Ψ` | `Ω` | `ff` | `fi` | `fl` | `ffi` | `ffl` |
| **1x** | `ı` | `ȷ` | `\`` | `́` | `ˇ` | `̆` | `̄` | `̊` | `̧` | `ß` | `æ` | `œ`  | `ø`  | `Æ`  | `Œ`   | `Ø`   |
| **2x** | ` ` | `!` | `”`  | `#` | `$` | `%` | `&` | `’` | `(` | `)` | `*` | `+`  | `,`  | `-`  | `.`   | `/`   |
| **3x** | `0` | `1` | `2`  | `3` | `4` | `5` | `6` | `7` | `8` | `9` | `:` | `;`  | `¡`  | `=`  | `¿`   | `?`   |
| **4x** | `@` | `A` | `B`  | `C` | `D` | `E` | `F` | `G` | `H` | `I` | `J` | `K`  | `L`  | `M`  | `N`   | `O`   |
| **5x** | `P` | `Q` | `R`  | `S` | `T` | `U` | `V` | `W` | `X` | `Y` | `Z` | `[`  | `“`  | `]`  | `^`   | `̇`   |
| **6x** | `‘` | `a` | `b`  | `c` | `d` | `e` | `f` | `g` | `h` | `i` | `j` | `k`  | `l`  | `m`  | `n`   | `o`   |
| **7x** | `p` | `q` | `r`  | `s` | `t` | `u` | `v` | `w` | `x` | `y` | `z` | `–`  | `—`  | `̋`  | `~`   | `̈`   |
| **8x** |     |     |      |     |     |     |     |     |     |     |     |      |      |      |       |       |
| **9x** |     |     |      |     |     |     |     |     |     |     |     |      |      |      |       |       |
| **Ax** |     |     |      |     |     |     |     |     |     |     |     |      |      |      |       |       |
| **Bx** |     |     |      |     |     |     |     |     |     |     |     |      |      |      |       |       |
| **Cx** |     |     |      |     |     |     |     |     |     |     |     |      |      |      |       |       |
| **Dx** |     |     |      |     |     |     |     |     |     |     |     |      |      |      |       |       |
| **Ex** |     |     |      |     |     |     |     |     |     |     |     |      |      |      |       |       |
| **Fx** |     |     |      |     |     |     |     |     |     |     |     |      |      |      |       |       |

- PostScript Standard Encoding

| \_x\_  | 0      | 1      | 2      | 3      | 4      | 5      | 6      | 7      | 8      | 9      | A      | B      | C      | D      | E      | F      |
|--------|--------|--------|--------|--------|--------|--------|--------|--------|--------|--------|--------|--------|--------|--------|--------|--------| 
| **0x** | \u0000 | \u0001 | \u0002 | \u0003 | \u0004 | \u0005 | \u0006 | \u0007 | \u0008 | \u0009 | \u000A | \u000B | \u000C | \u000D | \u000E | \u000F |
| **1x** | \u0010 | \u0011 | \u0012 | \u0013 | \u0014 | \u0015 | \u0016 | \u0017 | \u0018 | \u0019 | \u001A | \u001B | \u001C | \u001D | \u001E | \u001F |
| **2x** | ` `    | `!`    | `"`    | `#`    | `$`    | `%`    | `&`    | `'`    | `(`    | `)`    | `*`    | `+`    | `,`    | `-`    | `.`    | `/`    |
| **3x** | `0`    | `1`    | `2`    | `3`    | `4`    | `5`    | `6`    | `7`    | `8`    | `9`    | `:`    | `;`    | `<`    | `=`    | `>`    | `?`    |
| **4x** | `@`    | `A`    | `B`    | `C`    | `D`    | `E`    | `F`    | `G`    | `H`    | `I`    | `J`    | `K`    | `L`    | `M`    | `N`    | `O`    |
| **5x** | `P`    | `Q`    | `R`    | `S`    | `T`    | `U`    | `V`    | `W`    | `X`    | `Y`    | `Z`    | `[`    | `\\`   | `]`    | `^`    | `_`    |
| **6x** | `\``   | `a`    | `b`    | `c`    | `d`    | `e`    | `f`    | `g`    | `h`    | `i`    | `j`    | `k`    | `l`    | `m`    | `n`    | `o`    |
| **7x** | `p`    | `q`    | `r`    | `s`    | `t`    | `u`    | `v`    | `w`    | `x`    | `y`    | `z`    | `{`    | `\|`   | `}`    | `~`    | \u007F |
| **8x** |        |        |        |        |        |        |        |        |        |        |        |        |        |        |        |        |
| **9x** |        |        |        |        |        |        |        |        |        |        |        |        |        |        |        |        |
| **Ax** |        | `¡`    | `¢`    | `£`    | `∕`    | `¥`    | `ƒ`    | `§`    | `¤`    | `'`    | `“`    | `«`    | `‹`    | `›`    | `fi`   | `fl`   |
| **Bx** |        | `–`    | `†`    | `‡`    | `·`    |        | `¶`    | `•`    | `‚`    | `„`    | `”`    | `»`    | `…`    | `‰`    |        | `¿`    |
| **Cx** |        | `ˋ`    | `´`    | `ˆ`    | `˜`    | `ˉ`    | `˘`    | `˙`    | `¨`    |        | `˚`    | `¸`    |        | `˝`    | `˛`    | `ˇ`    |
| **Dx** | `—`    |        |        |        |        |        |        |        |        |        |        |        |        |        |        |        |
| **Ex** |        | `Æ`    |        | `ª`    |        |        |        |        | `Ł`    | `Ø`    | `Œ`    | `º`    |        |        |        |        |
| **Fx** |        | `æ`    |        |        |        | `ı`    |        |        | `ł`    | `ø`    | `œ`    | `ß`    |        |        |        |        |
<!--- END -->