unit Html4Ent;

{ Portions (C) International Organization for Standardization 1986
     Permission to copy in any form is granted for use with
     conforming SGML systems and applications as defined in
     ISO 8879, provided this notice is included in all copies.
}

interface

function Html4EntDecode(const InStr: String): String;

implementation

function Html4EntDecode(const InStr: String): String;
var
  OutStr: String;
begin
  { Kind of messy, as just some regexp search-and-replace from the entity files }
  if InStr = 'nbsp' then OutStr := '#160' { no-break space = non-breaking space,
                                  U+00A0 ISOnum }
  else if InStr = 'iexcl' then OutStr := '#161' { inverted exclamation mark, U+00A1 ISOnum }
  else if InStr = 'cent' then OutStr := '#162' { cent sign, U+00A2 ISOnum }
  else if InStr = 'pound' then OutStr := '#163' { pound sign, U+00A3 ISOnum }
  else if InStr = 'curren' then OutStr := '#164' { currency sign, U+00A4 ISOnum }
  else if InStr = 'yen' then OutStr := '#165' { yen sign = yuan sign, U+00A5 ISOnum }
  else if InStr = 'brvbar' then OutStr := '#166' { broken bar = broken vertical bar,
                                  U+00A6 ISOnum }
  else if InStr = 'sect' then OutStr := '#167' { section sign, U+00A7 ISOnum }
  else if InStr = 'uml' then OutStr := '#168' { diaeresis = spacing diaeresis,
                                  U+00A8 ISOdia }
  else if InStr = 'copy' then OutStr := '#169' { copyright sign, U+00A9 ISOnum }
  else if InStr = 'ordf' then OutStr := '#170' { feminine ordinal indicator, U+00AA ISOnum }
  else if InStr = 'laquo' then OutStr := '#171' { left-pointing double angle quotation mark
                                  = left pointing guillemet, U+00AB ISOnum }
  else if InStr = 'not' then OutStr := '#172' { not sign, U+00AC ISOnum }
  else if InStr = 'shy' then OutStr := '#173' { soft hyphen = discretionary hyphen,
                                  U+00AD ISOnum }
  else if InStr = 'reg' then OutStr := '#174' { registered sign = registered trade mark sign,
                                  U+00AE ISOnum }
  else if InStr = 'macr' then OutStr := '#175' { macron = spacing macron = overline
                                  = APL overbar, U+00AF ISOdia }
  else if InStr = 'deg' then OutStr := '#176' { degree sign, U+00B0 ISOnum }
  else if InStr = 'plusmn' then OutStr := '#177' { plus-minus sign = plus-or-minus sign,
                                  U+00B1 ISOnum }
  else if InStr = 'sup2' then OutStr := '#178' { superscript two = superscript digit two
                                  = squared, U+00B2 ISOnum }
  else if InStr = 'sup3' then OutStr := '#179' { superscript three = superscript digit three
                                  = cubed, U+00B3 ISOnum }
  else if InStr = 'acute' then OutStr := '#180' { acute accent = spacing acute,
                                  U+00B4 ISOdia }
  else if InStr = 'micro' then OutStr := '#181' { micro sign, U+00B5 ISOnum }
  else if InStr = 'para' then OutStr := '#182' { pilcrow sign = paragraph sign,
                                  U+00B6 ISOnum }
  else if InStr = 'middot' then OutStr := '#183' { middle dot = Georgian comma
                                  = Greek middle dot, U+00B7 ISOnum }
  else if InStr = 'cedil' then OutStr := '#184' { cedilla = spacing cedilla, U+00B8 ISOdia }
  else if InStr = 'sup1' then OutStr := '#185' { superscript one = superscript digit one,
                                  U+00B9 ISOnum }
  else if InStr = 'ordm' then OutStr := '#186' { masculine ordinal indicator,
                                  U+00BA ISOnum }
  else if InStr = 'raquo' then OutStr := '#187' { right-pointing double angle quotation mark
                                  = right pointing guillemet, U+00BB ISOnum }
  else if InStr = 'frac14' then OutStr := '#188' { vulgar fraction one quarter
                                  = fraction one quarter, U+00BC ISOnum }
  else if InStr = 'frac12' then OutStr := '#189' { vulgar fraction one half
                                  = fraction one half, U+00BD ISOnum }
  else if InStr = 'frac34' then OutStr := '#190' { vulgar fraction three quarters
                                  = fraction three quarters, U+00BE ISOnum }
  else if InStr = 'iquest' then OutStr := '#191' { inverted question mark
                                  = turned question mark, U+00BF ISOnum }
  else if InStr = 'Agrave' then OutStr := '#192' { latin capital letter A with grave
                                  = latin capital letter A grave,
                                  U+00C0 ISOlat1 }
  else if InStr = 'Aacute' then OutStr := '#193' { latin capital letter A with acute,
                                  U+00C1 ISOlat1 }
  else if InStr = 'Acirc' then OutStr := '#194' { latin capital letter A with circumflex,
                                  U+00C2 ISOlat1 }
  else if InStr = 'Atilde' then OutStr := '#195' { latin capital letter A with tilde,
                                  U+00C3 ISOlat1 }
  else if InStr = 'Auml' then OutStr := '#196' { latin capital letter A with diaeresis,
                                  U+00C4 ISOlat1 }
  else if InStr = 'Aring' then OutStr := '#197' { latin capital letter A with ring above
                                  = latin capital letter A ring,
                                  U+00C5 ISOlat1 }
  else if InStr = 'AElig' then OutStr := '#198' { latin capital letter AE
                                  = latin capital ligature AE,
                                  U+00C6 ISOlat1 }
  else if InStr = 'Ccedil' then OutStr := '#199' { latin capital letter C with cedilla,
                                  U+00C7 ISOlat1 }
  else if InStr = 'Egrave' then OutStr := '#200' { latin capital letter E with grave,
                                  U+00C8 ISOlat1 }
  else if InStr = 'Eacute' then OutStr := '#201' { latin capital letter E with acute,
                                  U+00C9 ISOlat1 }
  else if InStr = 'Ecirc' then OutStr := '#202' { latin capital letter E with circumflex,
                                  U+00CA ISOlat1 }
  else if InStr = 'Euml' then OutStr := '#203' { latin capital letter E with diaeresis,
                                  U+00CB ISOlat1 }
  else if InStr = 'Igrave' then OutStr := '#204' { latin capital letter I with grave,
                                  U+00CC ISOlat1 }
  else if InStr = 'Iacute' then OutStr := '#205' { latin capital letter I with acute,
                                  U+00CD ISOlat1 }
  else if InStr = 'Icirc' then OutStr := '#206' { latin capital letter I with circumflex,
                                  U+00CE ISOlat1 }
  else if InStr = 'Iuml' then OutStr := '#207' { latin capital letter I with diaeresis,
                                  U+00CF ISOlat1 }
  else if InStr = 'ETH' then OutStr := '#208' { latin capital letter ETH, U+00D0 ISOlat1 }
  else if InStr = 'Ntilde' then OutStr := '#209' { latin capital letter N with tilde,
                                  U+00D1 ISOlat1 }
  else if InStr = 'Ograve' then OutStr := '#210' { latin capital letter O with grave,
                                  U+00D2 ISOlat1 }
  else if InStr = 'Oacute' then OutStr := '#211' { latin capital letter O with acute,
                                  U+00D3 ISOlat1 }
  else if InStr = 'Ocirc' then OutStr := '#212' { latin capital letter O with circumflex,
                                  U+00D4 ISOlat1 }
  else if InStr = 'Otilde' then OutStr := '#213' { latin capital letter O with tilde,
                                  U+00D5 ISOlat1 }
  else if InStr = 'Ouml' then OutStr := '#214' { latin capital letter O with diaeresis,
                                  U+00D6 ISOlat1 }
  else if InStr = 'times' then OutStr := '#215' { multiplication sign, U+00D7 ISOnum }
  else if InStr = 'Oslash' then OutStr := '#216' { latin capital letter O with stroke
                                  = latin capital letter O slash,
                                  U+00D8 ISOlat1 }
  else if InStr = 'Ugrave' then OutStr := '#217' { latin capital letter U with grave,
                                  U+00D9 ISOlat1 }
  else if InStr = 'Uacute' then OutStr := '#218' { latin capital letter U with acute,
                                  U+00DA ISOlat1 }
  else if InStr = 'Ucirc' then OutStr := '#219' { latin capital letter U with circumflex,
                                  U+00DB ISOlat1 }
  else if InStr = 'Uuml' then OutStr := '#220' { latin capital letter U with diaeresis,
                                  U+00DC ISOlat1 }
  else if InStr = 'Yacute' then OutStr := '#221' { latin capital letter Y with acute,
                                  U+00DD ISOlat1 }
  else if InStr = 'THORN' then OutStr := '#222' { latin capital letter THORN,
                                  U+00DE ISOlat1 }
  else if InStr = 'szlig' then OutStr := '#223' { latin small letter sharp s = ess-zed,
                                  U+00DF ISOlat1 }
  else if InStr = 'agrave' then OutStr := '#224' { latin small letter a with grave
                                  = latin small letter a grave,
                                  U+00E0 ISOlat1 }
  else if InStr = 'aacute' then OutStr := '#225' { latin small letter a with acute,
                                  U+00E1 ISOlat1 }
  else if InStr = 'acirc' then OutStr := '#226' { latin small letter a with circumflex,
                                  U+00E2 ISOlat1 }
  else if InStr = 'atilde' then OutStr := '#227' { latin small letter a with tilde,
                                  U+00E3 ISOlat1 }
  else if InStr = 'auml' then OutStr := '#228' { latin small letter a with diaeresis,
                                  U+00E4 ISOlat1 }
  else if InStr = 'aring' then OutStr := '#229' { latin small letter a with ring above
                                  = latin small letter a ring,
                                  U+00E5 ISOlat1 }
  else if InStr = 'aelig' then OutStr := '#230' { latin small letter ae
                                  = latin small ligature ae, U+00E6 ISOlat1 }
  else if InStr = 'ccedil' then OutStr := '#231' { latin small letter c with cedilla,
                                  U+00E7 ISOlat1 }
  else if InStr = 'egrave' then OutStr := '#232' { latin small letter e with grave,
                                  U+00E8 ISOlat1 }
  else if InStr = 'eacute' then OutStr := '#233' { latin small letter e with acute,
                                  U+00E9 ISOlat1 }
  else if InStr = 'ecirc' then OutStr := '#234' { latin small letter e with circumflex,
                                  U+00EA ISOlat1 }
  else if InStr = 'euml' then OutStr := '#235' { latin small letter e with diaeresis,
                                  U+00EB ISOlat1 }
  else if InStr = 'igrave' then OutStr := '#236' { latin small letter i with grave,
                                  U+00EC ISOlat1 }
  else if InStr = 'iacute' then OutStr := '#237' { latin small letter i with acute,
                                  U+00ED ISOlat1 }
  else if InStr = 'icirc' then OutStr := '#238' { latin small letter i with circumflex,
                                  U+00EE ISOlat1 }
  else if InStr = 'iuml' then OutStr := '#239' { latin small letter i with diaeresis,
                                  U+00EF ISOlat1 }
  else if InStr = 'eth' then OutStr := '#240' { latin small letter eth, U+00F0 ISOlat1 }
  else if InStr = 'ntilde' then OutStr := '#241' { latin small letter n with tilde,
                                  U+00F1 ISOlat1 }
  else if InStr = 'ograve' then OutStr := '#242' { latin small letter o with grave,
                                  U+00F2 ISOlat1 }
  else if InStr = 'oacute' then OutStr := '#243' { latin small letter o with acute,
                                  U+00F3 ISOlat1 }
  else if InStr = 'ocirc' then OutStr := '#244' { latin small letter o with circumflex,
                                  U+00F4 ISOlat1 }
  else if InStr = 'otilde' then OutStr := '#245' { latin small letter o with tilde,
                                  U+00F5 ISOlat1 }
  else if InStr = 'ouml' then OutStr := '#246' { latin small letter o with diaeresis,
                                  U+00F6 ISOlat1 }
  else if InStr = 'divide' then OutStr := '#247' { division sign, U+00F7 ISOnum }
  else if InStr = 'oslash' then OutStr := '#248' { latin small letter o with stroke,
                                  = latin small letter o slash,
                                  U+00F8 ISOlat1 }
  else if InStr = 'ugrave' then OutStr := '#249' { latin small letter u with grave,
                                  U+00F9 ISOlat1 }
  else if InStr = 'uacute' then OutStr := '#250' { latin small letter u with acute,
                                  U+00FA ISOlat1 }
  else if InStr = 'ucirc' then OutStr := '#251' { latin small letter u with circumflex,
                                  U+00FB ISOlat1 }
  else if InStr = 'uuml' then OutStr := '#252' { latin small letter u with diaeresis,
                                  U+00FC ISOlat1 }
  else if InStr = 'yacute' then OutStr := '#253' { latin small letter y with acute,
                                  U+00FD ISOlat1 }
  else if InStr = 'thorn' then OutStr := '#254' { latin small letter thorn,
                                  U+00FE ISOlat1 }
  else if InStr = 'yuml' then OutStr := '#255' { latin small letter y with diaeresis,
                                    U+00FF ISOlat1 }{ Special characters for HTML }

{ C0 Controls and Basic Latin }
  else if InStr = 'quot' then OutStr := '#34'   { quotation mark = APL quote,
                                    U+0022 ISOnum }
  else if InStr = 'amp' then OutStr := '#38'   { ampersand, U+0026 ISOnum }
  else if InStr = 'lt' then OutStr := '#60'   { less-than sign, U+003C ISOnum }
  else if InStr = 'gt' then OutStr := '#62'   { greater-than sign, U+003E ISOnum }

{ Latin Extended-A }
  else if InStr = 'OElig' then OutStr := '#338'  { latin capital ligature OE,
                                    U+0152 ISOlat2 }
  else if InStr = 'oelig' then OutStr := '#339'  { latin small ligature oe, U+0153 ISOlat2 }
{ ligature is a misnomer, this is a separate character in some languages }
  else if InStr = 'Scaron' then OutStr := '#352'  { latin capital letter S with caron,
                                    U+0160 ISOlat2 }
  else if InStr = 'scaron' then OutStr := '#353'  { latin small letter s with caron,
                                    U+0161 ISOlat2 }
  else if InStr = 'Yuml' then OutStr := '#376'  { latin capital letter Y with diaeresis,
                                    U+0178 ISOlat2 }

{ Spacing Modifier Letters }
  else if InStr = 'circ' then OutStr := '#710'  { modifier letter circumflex accent,
                                    U+02C6 ISOpub }
  else if InStr = 'tilde' then OutStr := '#732'  { small tilde, U+02DC ISOdia }

{ General Punctuation }
  else if InStr = 'ensp' then OutStr := '#8194' { en space, U+2002 ISOpub }
  else if InStr = 'emsp' then OutStr := '#8195' { em space, U+2003 ISOpub }
  else if InStr = 'thinsp' then OutStr := '#8201' { thin space, U+2009 ISOpub }
  else if InStr = 'zwnj' then OutStr := '#8204' { zero width non-joiner,
                                    U+200C NEW RFC 2070 }
  else if InStr = 'zwj' then OutStr := '#8205' { zero width joiner, U+200D NEW RFC 2070 }
  else if InStr = 'lrm' then OutStr := '#8206' { left-to-right mark, U+200E NEW RFC 2070 }
  else if InStr = 'rlm' then OutStr := '#8207' { right-to-left mark, U+200F NEW RFC 2070 }
  else if InStr = 'ndash' then OutStr := '#8211' { en dash, U+2013 ISOpub }
  else if InStr = 'mdash' then OutStr := '#8212' { em dash, U+2014 ISOpub }
  else if InStr = 'lsquo' then OutStr := '#8216' { left single quotation mark,
                                    U+2018 ISOnum }
  else if InStr = 'rsquo' then OutStr := '#8217' { right single quotation mark,
                                    U+2019 ISOnum }
  else if InStr = 'sbquo' then OutStr := '#8218' { single low-9 quotation mark, U+201A NEW }
  else if InStr = 'ldquo' then OutStr := '#8220' { left double quotation mark,
                                    U+201C ISOnum }
  else if InStr = 'rdquo' then OutStr := '#8221' { right double quotation mark,
                                    U+201D ISOnum }
  else if InStr = 'bdquo' then OutStr := '#8222' { double low-9 quotation mark, U+201E NEW }
  else if InStr = 'dagger' then OutStr := '#8224' { dagger, U+2020 ISOpub }
  else if InStr = 'Dagger' then OutStr := '#8225' { double dagger, U+2021 ISOpub }
  else if InStr = 'permil' then OutStr := '#8240' { per mille sign, U+2030 ISOtech }
  else if InStr = 'lsaquo' then OutStr := '#8249' { single left-pointing angle quotation mark,
                                    U+2039 ISO proposed }
{ lsaquo is proposed but not yet ISO standardized }
  else if InStr = 'rsaquo' then OutStr := '#8250' { single right-pointing angle quotation mark,
                                    U+203A ISO proposed }
{ rsaquo is proposed but not yet ISO standardized }
  else if InStr = 'euro' then OutStr := '#8364'  { euro sign, U+20AC NEW }{ Mathematical, Greek and Symbolic characters for HTML }

{ Latin Extended-B }
  else if InStr = 'fnof' then OutStr := '#402' { latin small f with hook = function
                                    = florin, U+0192 ISOtech }

{ Greek }
  else if InStr = 'Alpha' then OutStr := '#913' { greek capital letter alpha, U+0391 }
  else if InStr = 'Beta' then OutStr := '#914' { greek capital letter beta, U+0392 }
  else if InStr = 'Gamma' then OutStr := '#915' { greek capital letter gamma,
                                    U+0393 ISOgrk3 }
  else if InStr = 'Delta' then OutStr := '#916' { greek capital letter delta,
                                    U+0394 ISOgrk3 }
  else if InStr = 'Epsilon' then OutStr := '#917' { greek capital letter epsilon, U+0395 }
  else if InStr = 'Zeta' then OutStr := '#918' { greek capital letter zeta, U+0396 }
  else if InStr = 'Eta' then OutStr := '#919' { greek capital letter eta, U+0397 }
  else if InStr = 'Theta' then OutStr := '#920' { greek capital letter theta,
                                    U+0398 ISOgrk3 }
  else if InStr = 'Iota' then OutStr := '#921' { greek capital letter iota, U+0399 }
  else if InStr = 'Kappa' then OutStr := '#922' { greek capital letter kappa, U+039A }
  else if InStr = 'Lambda' then OutStr := '#923' { greek capital letter lambda,
                                    U+039B ISOgrk3 }
  else if InStr = 'Mu' then OutStr := '#924' { greek capital letter mu, U+039C }
  else if InStr = 'Nu' then OutStr := '#925' { greek capital letter nu, U+039D }
  else if InStr = 'Xi' then OutStr := '#926' { greek capital letter xi, U+039E ISOgrk3 }
  else if InStr = 'Omicron' then OutStr := '#927' { greek capital letter omicron, U+039F }
  else if InStr = 'Pi' then OutStr := '#928' { greek capital letter pi, U+03A0 ISOgrk3 }
  else if InStr = 'Rho' then OutStr := '#929' { greek capital letter rho, U+03A1 }
{ there is no Sigmaf, and no U+03A2 character either }
  else if InStr = 'Sigma' then OutStr := '#931' { greek capital letter sigma,
                                    U+03A3 ISOgrk3 }
  else if InStr = 'Tau' then OutStr := '#932' { greek capital letter tau, U+03A4 }
  else if InStr = 'Upsilon' then OutStr := '#933' { greek capital letter upsilon,
                                    U+03A5 ISOgrk3 }
  else if InStr = 'Phi' then OutStr := '#934' { greek capital letter phi,
                                    U+03A6 ISOgrk3 }
  else if InStr = 'Chi' then OutStr := '#935' { greek capital letter chi, U+03A7 }
  else if InStr = 'Psi' then OutStr := '#936' { greek capital letter psi,
                                    U+03A8 ISOgrk3 }
  else if InStr = 'Omega' then OutStr := '#937' { greek capital letter omega,
                                    U+03A9 ISOgrk3 }

  else if InStr = 'alpha' then OutStr := '#945' { greek small letter alpha,
                                    U+03B1 ISOgrk3 }
  else if InStr = 'beta' then OutStr := '#946' { greek small letter beta, U+03B2 ISOgrk3 }
  else if InStr = 'gamma' then OutStr := '#947' { greek small letter gamma,
                                    U+03B3 ISOgrk3 }
  else if InStr = 'delta' then OutStr := '#948' { greek small letter delta,
                                    U+03B4 ISOgrk3 }
  else if InStr = 'epsilon' then OutStr := '#949' { greek small letter epsilon,
                                    U+03B5 ISOgrk3 }
  else if InStr = 'zeta' then OutStr := '#950' { greek small letter zeta, U+03B6 ISOgrk3 }
  else if InStr = 'eta' then OutStr := '#951' { greek small letter eta, U+03B7 ISOgrk3 }
  else if InStr = 'theta' then OutStr := '#952' { greek small letter theta,
                                    U+03B8 ISOgrk3 }
  else if InStr = 'iota' then OutStr := '#953' { greek small letter iota, U+03B9 ISOgrk3 }
  else if InStr = 'kappa' then OutStr := '#954' { greek small letter kappa,
                                    U+03BA ISOgrk3 }
  else if InStr = 'lambda' then OutStr := '#955' { greek small letter lambda,
                                    U+03BB ISOgrk3 }
  else if InStr = 'mu' then OutStr := '#956' { greek small letter mu, U+03BC ISOgrk3 }
  else if InStr = 'nu' then OutStr := '#957' { greek small letter nu, U+03BD ISOgrk3 }
  else if InStr = 'xi' then OutStr := '#958' { greek small letter xi, U+03BE ISOgrk3 }
  else if InStr = 'omicron' then OutStr := '#959' { greek small letter omicron, U+03BF NEW }
  else if InStr = 'pi' then OutStr := '#960' { greek small letter pi, U+03C0 ISOgrk3 }
  else if InStr = 'rho' then OutStr := '#961' { greek small letter rho, U+03C1 ISOgrk3 }
  else if InStr = 'sigmaf' then OutStr := '#962' { greek small letter final sigma,
                                    U+03C2 ISOgrk3 }
  else if InStr = 'sigma' then OutStr := '#963' { greek small letter sigma,
                                    U+03C3 ISOgrk3 }
  else if InStr = 'tau' then OutStr := '#964' { greek small letter tau, U+03C4 ISOgrk3 }
  else if InStr = 'upsilon' then OutStr := '#965' { greek small letter upsilon,
                                    U+03C5 ISOgrk3 }
  else if InStr = 'phi' then OutStr := '#966' { greek small letter phi, U+03C6 ISOgrk3 }
  else if InStr = 'chi' then OutStr := '#967' { greek small letter chi, U+03C7 ISOgrk3 }
  else if InStr = 'psi' then OutStr := '#968' { greek small letter psi, U+03C8 ISOgrk3 }
  else if InStr = 'omega' then OutStr := '#969' { greek small letter omega,
                                    U+03C9 ISOgrk3 }
  else if InStr = 'thetasym' then OutStr := '#977' { greek small letter theta symbol,
                                    U+03D1 NEW }
  else if InStr = 'upsih' then OutStr := '#978' { greek upsilon with hook symbol,
                                    U+03D2 NEW }
  else if InStr = 'piv' then OutStr := '#982' { greek pi symbol, U+03D6 ISOgrk3 }

{ General Punctuation }
  else if InStr = 'bull' then OutStr := '#8226' { bullet = black small circle,
                                     U+2022 ISOpub  }
{ bullet is NOT the same as bullet operator, U+2219 }
  else if InStr = 'hellip' then OutStr := '#8230' { horizontal ellipsis = three dot leader,
                                     U+2026 ISOpub  }
  else if InStr = 'prime' then OutStr := '#8242' { prime = minutes = feet, U+2032 ISOtech }
  else if InStr = 'Prime' then OutStr := '#8243' { double prime = seconds = inches,
                                     U+2033 ISOtech }
  else if InStr = 'oline' then OutStr := '#8254' { overline = spacing overscore,
                                     U+203E NEW }
  else if InStr = 'frasl' then OutStr := '#8260' { fraction slash, U+2044 NEW }

{ Letterlike Symbols }
  else if InStr = 'weierp' then OutStr := '#8472' { script capital P = power set
                                     = Weierstrass p, U+2118 ISOamso }
  else if InStr = 'image' then OutStr := '#8465' { blackletter capital I = imaginary part,
                                     U+2111 ISOamso }
  else if InStr = 'real' then OutStr := '#8476' { blackletter capital R = real part symbol,
                                     U+211C ISOamso }
  else if InStr = 'trade' then OutStr := '#8482' { trade mark sign, U+2122 ISOnum }
  else if InStr = 'alefsym' then OutStr := '#8501' { alef symbol = first transfinite cardinal,
                                     U+2135 NEW }
{ alef symbol is NOT the same as hebrew letter alef,
     U+05D0 although the same glyph could be used to depict both characters }

{ Arrows }
  else if InStr = 'larr' then OutStr := '#8592' { leftwards arrow, U+2190 ISOnum }
  else if InStr = 'uarr' then OutStr := '#8593' { upwards arrow, U+2191 ISOnum}
  else if InStr = 'rarr' then OutStr := '#8594' { rightwards arrow, U+2192 ISOnum }
  else if InStr = 'darr' then OutStr := '#8595' { downwards arrow, U+2193 ISOnum }
  else if InStr = 'harr' then OutStr := '#8596' { left right arrow, U+2194 ISOamsa }
  else if InStr = 'crarr' then OutStr := '#8629' { downwards arrow with corner leftwards
                                     = carriage return, U+21B5 NEW }
  else if InStr = 'lArr' then OutStr := '#8656' { leftwards double arrow, U+21D0 ISOtech }
{ ISO 10646 does not say that lArr is the same as the 'is implied by' arrow
    but also does not have any other character for that function. So ? lArr can
    be used for 'is implied by' as ISOtech suggests }
  else if InStr = 'uArr' then OutStr := '#8657' { upwards double arrow, U+21D1 ISOamsa }
  else if InStr = 'rArr' then OutStr := '#8658' { rightwards double arrow,
                                     U+21D2 ISOtech }
{ ISO 10646 does not say this is the 'implies' character but does not have
     another character with this function so ?
     rArr can be used for 'implies' as ISOtech suggests }
  else if InStr = 'dArr' then OutStr := '#8659' { downwards double arrow, U+21D3 ISOamsa }
  else if InStr = 'hArr' then OutStr := '#8660' { left right double arrow,
                                     U+21D4 ISOamsa }

{ Mathematical Operators }
  else if InStr = 'forall' then OutStr := '#8704' { for all, U+2200 ISOtech }
  else if InStr = 'part' then OutStr := '#8706' { partial differential, U+2202 ISOtech  }
  else if InStr = 'exist' then OutStr := '#8707' { there exists, U+2203 ISOtech }
  else if InStr = 'empty' then OutStr := '#8709' { empty set = null set = diameter,
                                     U+2205 ISOamso }
  else if InStr = 'nabla' then OutStr := '#8711' { nabla = backward difference,
                                     U+2207 ISOtech }
  else if InStr = 'isin' then OutStr := '#8712' { element of, U+2208 ISOtech }
  else if InStr = 'notin' then OutStr := '#8713' { not an element of, U+2209 ISOtech }
  else if InStr = 'ni' then OutStr := '#8715' { contains as member, U+220B ISOtech }
{ should there be a more memorable name than 'ni'? }
  else if InStr = 'prod' then OutStr := '#8719' { n-ary product = product sign,
                                     U+220F ISOamsb }
{ prod is NOT the same character as U+03A0 'greek capital letter pi' though
     the same glyph might be used for both }
  else if InStr = 'sum' then OutStr := '#8721' { n-ary sumation, U+2211 ISOamsb }
{ sum is NOT the same character as U+03A3 'greek capital letter sigma'
     though the same glyph might be used for both }
  else if InStr = 'minus' then OutStr := '#8722' { minus sign, U+2212 ISOtech }
  else if InStr = 'lowast' then OutStr := '#8727' { asterisk operator, U+2217 ISOtech }
  else if InStr = 'radic' then OutStr := '#8730' { square root = radical sign,
                                     U+221A ISOtech }
  else if InStr = 'prop' then OutStr := '#8733' { proportional to, U+221D ISOtech }
  else if InStr = 'infin' then OutStr := '#8734' { infinity, U+221E ISOtech }
  else if InStr = 'ang' then OutStr := '#8736' { angle, U+2220 ISOamso }
  else if InStr = 'and' then OutStr := '#8743' { logical and = wedge, U+2227 ISOtech }
  else if InStr = 'or' then OutStr := '#8744' { logical or = vee, U+2228 ISOtech }
  else if InStr = 'cap' then OutStr := '#8745' { intersection = cap, U+2229 ISOtech }
  else if InStr = 'cup' then OutStr := '#8746' { union = cup, U+222A ISOtech }
  else if InStr = 'int' then OutStr := '#8747' { integral, U+222B ISOtech }
  else if InStr = 'there4' then OutStr := '#8756' { therefore, U+2234 ISOtech }
  else if InStr = 'sim' then OutStr := '#8764' { tilde operator = varies with = similar to,
                                     U+223C ISOtech }
{ tilde operator is NOT the same character as the tilde, U+007E,
     although the same glyph might be used to represent both  }
  else if InStr = 'cong' then OutStr := '#8773' { approximately equal to, U+2245 ISOtech }
  else if InStr = 'asymp' then OutStr := '#8776' { almost equal to = asymptotic to,
                                     U+2248 ISOamsr }
  else if InStr = 'ne' then OutStr := '#8800' { not equal to, U+2260 ISOtech }
  else if InStr = 'equiv' then OutStr := '#8801' { identical to, U+2261 ISOtech }
  else if InStr = 'le' then OutStr := '#8804' { less-than or equal to, U+2264 ISOtech }
  else if InStr = 'ge' then OutStr := '#8805' { greater-than or equal to,
                                     U+2265 ISOtech }
  else if InStr = 'sub' then OutStr := '#8834' { subset of, U+2282 ISOtech }
  else if InStr = 'sup' then OutStr := '#8835' { superset of, U+2283 ISOtech }
{ note that nsup, 'not a superset of, U+2283' is not covered by the Symbol
     font encoding and is not included. Should it be, for symmetry?
     It is in ISOamsn  }
  else if InStr = 'nsub' then OutStr := '#8836' { not a subset of, U+2284 ISOamsn }
  else if InStr = 'sube' then OutStr := '#8838' { subset of or equal to, U+2286 ISOtech }
  else if InStr = 'supe' then OutStr := '#8839' { superset of or equal to,
                                     U+2287 ISOtech }
  else if InStr = 'oplus' then OutStr := '#8853' { circled plus = direct sum,
                                     U+2295 ISOamsb }
  else if InStr = 'otimes' then OutStr := '#8855' { circled times = vector product,
                                     U+2297 ISOamsb }
  else if InStr = 'perp' then OutStr := '#8869' { up tack = orthogonal to = perpendicular,
                                     U+22A5 ISOtech }
  else if InStr = 'sdot' then OutStr := '#8901' { dot operator, U+22C5 ISOamsb }
{ dot operator is NOT the same character as U+00B7 middle dot }

{ Miscellaneous Technical }
  else if InStr = 'lceil' then OutStr := '#8968' { left ceiling = apl upstile,
                                     U+2308 ISOamsc  }
  else if InStr = 'rceil' then OutStr := '#8969' { right ceiling, U+2309 ISOamsc  }
  else if InStr = 'lfloor' then OutStr := '#8970' { left floor = apl downstile,
                                     U+230A ISOamsc  }
  else if InStr = 'rfloor' then OutStr := '#8971' { right floor, U+230B ISOamsc  }
  else if InStr = 'lang' then OutStr := '#9001' { left-pointing angle bracket = bra,
                                     U+2329 ISOtech }
{ lang is NOT the same character as U+003C 'less than'
     or U+2039 'single left-pointing angle quotation mark' }
  else if InStr = 'rang' then OutStr := '#9002' { right-pointing angle bracket = ket,
                                     U+232A ISOtech }
{ rang is NOT the same character as U+003E 'greater than'
     or U+203A 'single right-pointing angle quotation mark' }

{ Geometric Shapes }
  else if InStr = 'loz' then OutStr := '#9674' { lozenge, U+25CA ISOpub }

{ Miscellaneous Symbols }
  else if InStr = 'spades' then OutStr := '#9824' { black spade suit, U+2660 ISOpub }
{ black here seems to mean filled as opposed to hollow }
  else if InStr = 'clubs' then OutStr := '#9827' { black club suit = shamrock,
                                     U+2663 ISOpub }
  else if InStr = 'hearts' then OutStr := '#9829' { black heart suit = valentine,
                                     U+2665 ISOpub }
  else if InStr = 'diams' then OutStr := '#9830' { black diamond suit, U+2666 ISOpub }
  else OutStr := InStr;
  Html4EntDecode := OutStr
end;

end.
