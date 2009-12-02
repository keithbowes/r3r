
unit expas;
interface

{$CALLING cdecl}
{$MODE DELPHI}

{
  Automatically converted by H2Pas 1.0.0 from xmlparse.h
  The following command line parameters were used:
    -D
    -l
    expat
    -u
    expas
    -v
    xmlparse.h
}

Type
  //Pchar  = ^char;
  PPChar = ^PChar;
  PPointer = ^pointer;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


  {
  Copyright (c) 1998, 1999, 2000 Thai Open Source Software Center Ltd
  See the file copying.txt for copying permission.
   }
{$ifndef XmlParse_INCLUDED}

  const
     XmlParse_INCLUDED = 1;

  type

     XML_Parser = pointer;
{$ifdef XML_UNICODE_WCHAR_T}
  { XML_UNICODE_WCHAR_T will work only if sizeof(wchar_t) == 2 and wchar_t
  uses Unicode.  }
  { Information is UTF-16 encoded as wchar_ts  }
{$ifndef XML_UNICODE}
{$define XML_UNICODE}  
{$endif}
{$include <stddef.h>}

  type

     XML_Char = wchar_t;

     XML_LChar = wchar_t;
{$else}
  { not XML_UNICODE_WCHAR_T  }
{$ifdef XML_UNICODE}
  { Information is UTF-16 encoded as unsigned shorts  }

  type

     XML_Char = word;

     XML_LChar = char;
{$else}
  { not XML_UNICODE  }
  { Information is UTF-8 encoded.  }

  type

     XML_Char = char;

     XML_LChar = char;
{$endif}
  { not XML_UNICODE  }
{$endif}

  PXML_LChar = ^XML_LChar;
     
  { not XML_UNICODE_WCHAR_T  }
  { Constructs a new parser; encoding is the encoding specified by the external
  protocol or null if there is none specified.  }
(* Const before type ignored *)

  function XML_ParserCreate(const encoding:PChar):XML_Parser; external 'expat' name 'XML_ParserCreate';

  { Constructs a new parser and namespace processor.  Element type names
  and attribute names that belong to a namespace will be expanded;
  unprefixed attribute names are never expanded; unprefixed element type
  names are expanded only if there is a default namespace. The expanded
  name is the concatenation of the namespace URI, the namespace separator character,
  and the local part of the name.  If the namespace separator is '\0' then
  the namespace URI and the local part will be concatenated without any
  separator.  When a namespace is not declared, the name and prefix will be
  passed through without expansion.  }
(* Const before type ignored *)
  function XML_ParserCreateNS(encoding:PChar; namespaceSeparator:XML_Char):XML_Parser; external 'expat' name 'XML_ParserCreateNS';

  { atts is array of name/value pairs, terminated by 0;
     names and values are 0 terminated.  }
(* Const before type ignored *)
(* Const before type ignored *)

  type

     XML_StartElementHandler = procedure (userData:pointer; name:PChar; atts:PPChar);
(* Const before type ignored *)

     XML_EndElementHandler = procedure (userData:pointer; name:PChar);
  { s is not 0 terminated.  }
(* Const before type ignored *)

     XML_CharacterDataHandler = procedure (userData:pointer; s:PChar; len:integer);
  { target and data are 0 terminated  }
(* Const before type ignored *)
(* Const before type ignored *)

     XML_ProcessingInstructionHandler = procedure (userData:pointer; target:PChar; data:PChar);
  { data is 0 terminated  }
(* Const before type ignored *)

     XML_CommentHandler = procedure (userData:pointer; data:PChar);

     XML_StartCdataSectionHandler = procedure (userData:pointer);

     XML_EndCdataSectionHandler = procedure (userData:pointer);
  { This is called for any characters in the XML document for
  which there is no applicable handler.  This includes both
  characters that are part of markup which is of a kind that is
  not reported (comments, markup declarations), or characters
  that are part of a construct which could be reported but
  for which no handler has been supplied. The characters are passed
  exactly as they were in the XML document except that
  they will be encoded in UTF-8.  Line boundaries are not normalized.
  Note that a byte order mark character is not passed to the default handler.
  There are no guarantees about how characters are divided between calls
  to the default handler: for example, a comment might be split between
  multiple calls.  }
(* Const before type ignored *)

     XML_DefaultHandler = procedure (userData:pointer; s:PChar; len:integer);
  { This is called for the start of the DOCTYPE declaration when the
  name of the DOCTYPE is encountered.  }
(* Const before type ignored *)

     XML_StartDoctypeDeclHandler = procedure (userData:pointer; doctypeName:PChar);
  { This is called for the start of the DOCTYPE declaration when the
  closing > is encountered, but after processing any external subset.  }

     XML_EndDoctypeDeclHandler = procedure (userData:pointer);
  { This is called for a declaration of an unparsed (NDATA)
  entity.  The base argument is whatever was set by XML_SetBase.
  The entityName, systemId and notationName arguments will never be null.
  The other arguments may be.  }
(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)

     XML_UnparsedEntityDeclHandler = procedure (userData:pointer; entityName:PChar; base:PChar; systemId:PChar; publicId:PChar; 
                   notationName:PChar);
  { This is called for a declaration of notation.
  The base argument is whatever was set by XML_SetBase.
  The notationName will never be null.  The other arguments can be.  }
(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)

     XML_NotationDeclHandler = procedure (userData:pointer; notationName:PChar; base:PChar; systemId:PChar; publicId:PChar);
(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)

     XML_ExternalParsedEntityDeclHandler = procedure (userData:pointer; entityName:PChar; base:PChar; systemId:PChar; publicId:PChar);
(* Const before type ignored *)
(* Const before type ignored *)

     XML_InternalParsedEntityDeclHandler = procedure (userData:pointer; entityName:PChar; replacementText:PChar; replacementTextLength:integer);
  { When namespace processing is enabled, these are called once for
  each namespace declaration. The call to the start and end element
  handlers occur between the calls to the start and end namespace
  declaration handlers. For an xmlns attribute, prefix will be null.
  For an xmlns="" attribute, uri will be null.  }
(* Const before type ignored *)
(* Const before type ignored *)

     XML_StartNamespaceDeclHandler = procedure (userData:pointer; prefix:PChar; uri:PChar);
(* Const before type ignored *)

     XML_EndNamespaceDeclHandler = procedure (userData:pointer; prefix:PChar);
  { This is called if the document is not standalone (it has an
  external subset or a reference to a parameter entity, but does not
  have standalone="yes"). If this handler returns 0, then processing
  will not continue, and the parser will return a
  XML_ERROR_NOT_STANDALONE error.  }

     XML_NotStandaloneHandler = function (userData:pointer):integer;
  { This is called for a reference to an external parsed general entity.
  The referenced entity is not automatically parsed.
  The application can parse it immediately or later using
  XML_ExternalEntityParserCreate.
  The parser argument is the parser parsing the entity containing the reference;
  it can be passed as the parser argument to XML_ExternalEntityParserCreate.
  The systemId argument is the system identifier as specified in the entity declaration;
  it will not be null.
  The base argument is the system identifier that should be used as the base for
  resolving systemId if systemId was relative; this is set by XML_SetBase;
  it may be null.
  The publicId argument is the public identifier as specified in the entity declaration,
  or null if none was specified; the whitespace in the public identifier
  will have been normalized as required by the XML spec.
  The context argument specifies the parsing context in the format
  expected by the context argument to
  XML_ExternalEntityParserCreate; context is valid only until the handler
  returns, so if the referenced entity is to be parsed later, it must be copied.
  The handler should return 0 if processing should not continue because of
  a fatal error in the handling of the external entity.
  In this case the calling parser will return an XML_ERROR_EXTERNAL_ENTITY_HANDLING
  error.
  Note that unlike other handlers the first argument is the parser, not userData.  }
(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)

     XML_ExternalEntityRefHandler = function (parser:XML_Parser; context:PChar; base:PChar; systemId:PChar; publicId:PChar):integer;
  { This structure is filled in by the XML_UnknownEncodingHandler
  to provide information to the parser about encodings that are unknown
  to the parser.
  The map[b] member gives information about byte sequences
  whose first byte is b.
  If map[b] is c where c is >= 0, then b by itself encodes the Unicode scalar value c.
  If map[b] is -1, then the byte sequence is malformed.
  If map[b] is -n, where n >= 2, then b is the first byte of an n-byte
  sequence that encodes a single Unicode scalar value.
  The data member will be passed as the first argument to the convert function.
  The convert function is used to convert multibyte sequences;
  s will point to a n-byte sequence where map[(unsigned char)*s] == -n.
  The convert function must return the Unicode scalar value
  represented by this byte sequence or -1 if the byte sequence is malformed.
  The convert function may be null if the encoding is a single-byte encoding,
  that is if map[b] >= -1 for all bytes b.
  When the parser is finished with the encoding, then if release is not null,
  it will call release passing it the data member;
  once release has been called, the convert function will not be called again.
  
  Expat places certain restrictions on the encodings that are supported
  using this mechanism.
  
  1. Every ASCII character that can appear in a well-formed XML document,
  other than the characters
  
    $@\^`~
  
  must be represented by a single byte, and that byte must be the
  same byte that represents that character in ASCII.
  
  2. No character may require more than 4 bytes to encode.
  
  3. All characters encoded must have Unicode scalar values <= 0xFFFF,
  (ie characters that would be encoded by surrogates in UTF-16
  are  not allowed).  Note that this restriction doesn't apply to
  the built-in support for UTF-8 and UTF-16.
  
  4. No Unicode character may be encoded by more than one distinct sequence
  of bytes.  }
(* Const before type ignored *)

     XML_Encoding = record
          map : array[0..255] of integer;
          data : pointer;
          convert : function (var data:pointer; s:pchar):integer;
          release : procedure (var data:pointer);
       end;
  { This is called for an encoding that is unknown to the parser.
  The encodingHandlerData argument is that which was passed as the
  second argument to XML_SetUnknownEncodingHandler.
  The name argument gives the name of the encoding as specified in
  the encoding declaration.
  If the callback can provide information about the encoding,
  it must fill in the XML_Encoding structure, and return 1.
  Otherwise it must return 0.
  If info does not describe a suitable encoding,
  then the parser will return an XML_UNKNOWN_ENCODING error.  }
(* Const before type ignored *)

     XML_UnknownEncodingHandler = function (var encodingHandlerData:pointer; name:PChar; var info:XML_Encoding):integer;

  procedure XML_SetElementHandler(parser:XML_Parser; startEl:XML_StartElementHandler; endEl:XML_EndElementHandler); external 'expat' name 'XML_SetElementHandler';

  procedure XML_SetCharacterDataHandler(parser:XML_Parser; handler:XML_CharacterDataHandler); external 'expat' name 'XML_SetCharacterDataHandler';

  procedure XML_SetProcessingInstructionHandler(parser:XML_Parser; handler:XML_ProcessingInstructionHandler); external 'expat' name 'XML_SetProcessingInstructionHandler';

  procedure XML_SetCommentHandler(parser:XML_Parser; handler:XML_CommentHandler); external 'expat' name 'XML_SetCommentHandler';

  procedure XML_SetCdataSectionHandler(parser:XML_Parser; startEl:XML_StartCdataSectionHandler; endEl:XML_EndCdataSectionHandler); external 'expat' name 'XML_SetCdataSectionHandler';

  { This sets the default handler and also inhibits expansion of internal entities.
  The entity reference will be passed to the default handler.  }
  procedure XML_SetDefaultHandler(parser:XML_Parser; handler:XML_DefaultHandler); external 'expat' name 'XML_SetDefaultHandler';

  { This sets the default handler but does not inhibit expansion of internal entities.
  The entity reference will not be passed to the default handler.  }
  procedure XML_SetDefaultHandlerExpand(parser:XML_Parser; handler:XML_DefaultHandler); external 'expat' name 'XML_SetDefaultHandlerExpand';

  procedure XML_SetDoctypeDeclHandler(parser:XML_Parser; startEl:XML_StartDoctypeDeclHandler; endEl:XML_EndDoctypeDeclHandler); external 'expat' name 'XML_SetDoctypeDeclHandler';

  procedure XML_SetUnparsedEntityDeclHandler(parser:XML_Parser; handler:XML_UnparsedEntityDeclHandler); external 'expat' name 'XML_SetUnparsedEntityDeclHandler';

  procedure XML_SetNotationDeclHandler(parser:XML_Parser; handler:XML_NotationDeclHandler); external 'expat' name 'XML_SetNotationDeclHandler';

  procedure XML_SetExternalParsedEntityDeclHandler(parser:XML_Parser; handler:XML_ExternalParsedEntityDeclHandler); external 'expat' name 'XML_SetExternalParsedEntityDeclHandler';

  procedure XML_SetInternalParsedEntityDeclHandler(parser:XML_Parser; handler:XML_InternalParsedEntityDeclHandler); external 'expat' name 'XML_SetInternalParsedEntityDeclHandler';

  procedure XML_SetNamespaceDeclHandler(parser:XML_Parser; startEl:XML_StartNamespaceDeclHandler; endEl:XML_EndNamespaceDeclHandler); external 'expat' name 'XML_SetNamespaceDeclHandler';

  procedure XML_SetNotStandaloneHandler(parser:XML_Parser; handler:XML_NotStandaloneHandler); external 'expat' name 'XML_SetNotStandaloneHandler';

  procedure XML_SetExternalEntityRefHandler(parser:XML_Parser; handler:XML_ExternalEntityRefHandler); external 'expat' name 'XML_SetExternalEntityRefHandler';

  { If a non-null value for arg is specified here, then it will be passed
  as the first argument to the external entity ref handler instead
  of the parser object.  }
  procedure XML_SetExternalEntityRefHandlerArg(_para1:XML_Parser; var arg:pointer); external 'expat' name 'XML_SetExternalEntityRefHandlerArg';

  procedure XML_SetUnknownEncodingHandler(parser:XML_Parser; handler:XML_UnknownEncodingHandler; var encodingHandlerData:pointer); external 'expat' name 'XML_SetUnknownEncodingHandler';

  { This can be called within a handler for a start element, end element,
  processing instruction or character data.  It causes the corresponding
  markup to be passed to the default handler.  }
  procedure XML_DefaultCurrent(parser:XML_Parser); external 'expat' name 'XML_DefaultCurrent';

  { This value is passed as the userData argument to callbacks.  }
  procedure XML_SetUserData(parser:XML_Parser; userData:pointer); external 'expat' name 'XML_SetUserData';

  { Returns the last value set by XML_SetUserData or null.  }
  function XML_GetUserData(parser: XML_Parser): Pointer;
  { This is equivalent to supplying an encoding argument
  to XML_ParserCreate. It must not be called after XML_Parse
  or XML_ParseBuffer.  }
(* Const before type ignored *)
  function XML_SetEncoding(parser:XML_Parser; encoding:PChar):integer; external 'expat' name 'XML_SetEncoding';

  { If this function is called, then the parser will be passed
  as the first argument to callbacks instead of userData.
  The userData will still be accessible using XML_GetUserData.  }
  procedure XML_UseParserAsHandlerArg(parser:XML_Parser); external 'expat' name 'XML_UseParserAsHandlerArg';

  { Sets the base to be used for resolving relative URIs in system identifiers in
  declarations.  Resolving relative identifiers is left to the application:
  this value will be passed through as the base argument to the
  XML_ExternalEntityRefHandler, XML_NotationDeclHandler
  and XML_UnparsedEntityDeclHandler. The base argument will be copied.
  Returns zero if out of memory, non-zero otherwise.  }
(* Const before type ignored *)
  function XML_SetBase(parser:XML_Parser; base:PChar):integer; external 'expat' name 'XML_SetBase';

(* Const before type ignored *)
  function XML_GetBase(parser:XML_Parser):PChar; external 'expat' name 'XML_GetBase';

  { Returns the number of the attribute/value pairs passed in last call
  to the XML_StartElementHandler that were specified in the start-tag
  rather than defaulted. Each attribute/value pair counts as 2; thus
  this correspondds to an index into the atts array passed to the
  XML_StartElementHandler.  }
  function XML_GetSpecifiedAttributeCount(parser:XML_Parser):integer; external 'expat' name 'XML_GetSpecifiedAttributeCount';

  { Returns the index of the ID attribute passed in the last call to
  XML_StartElementHandler, or -1 if there is no ID attribute.  Each
  attribute/value pair counts as 2; thus this correspondds to an index
  into the atts array passed to the XML_StartElementHandler.  }
  function XML_GetIdAttributeIndex(parser:XML_Parser):integer; external 'expat' name 'XML_GetIdAttributeIndex';

  { Parses some input. Returns 0 if a fatal error is detected.
  The last call to XML_Parse must have isFinal true;
  len may be zero for this call (or any other).  }
(* Const before type ignored *)
  function XML_Parse(parser:XML_Parser; s:pchar; len:integer; isFinal:integer):integer; external 'expat' name 'XML_Parse';

  function XML_GetBuffer(parser:XML_Parser; len:integer):pointer; external 'expat' name 'XML_GetBuffer';

  function XML_ParseBuffer(parser:XML_Parser; len:integer; isFinal:integer):integer; external 'expat' name 'XML_ParseBuffer';

  { Creates an XML_Parser object that can parse an external general entity;
  context is a '\0'-terminated string specifying the parse context;
  encoding is a '\0'-terminated string giving the name of the externally specified encoding,
  or null if there is no externally specified encoding.
  The context string consists of a sequence of tokens separated by formfeeds (\f);
  a token consisting of a name specifies that the general entity of the name
  is open; a token of the form prefix=uri specifies the namespace for a particular
  prefix; a token of the form =uri specifies the default namespace.
  This can be called at any point after the first call to an ExternalEntityRefHandler
  so longer as the parser has not yet been freed.
  The new parser is completely independent and may safely be used in a separate thread.
  The handlers and userData are initialized from the parser argument.
  Returns 0 if out of memory.  Otherwise returns a new XML_Parser object.  }
(* Const before type ignored *)
(* Const before type ignored *)
  function XML_ExternalEntityParserCreate(parser:XML_Parser; context:PChar; encoding:PChar):XML_Parser; external 'expat' name 'XML_ExternalEntityParserCreate';


  type
     XML_ParamEntityParsing = (XML_PARAM_ENTITY_PARSING_NEVER,XML_PARAM_ENTITY_PARSING_UNLESS_STANDALONE,
       XML_PARAM_ENTITY_PARSING_ALWAYS);

  { Controls parsing of parameter entities (including the external DTD
  subset). If parsing of parameter entities is enabled, then references
  to external parameter entities (including the external DTD subset)
  will be passed to the handler set with
  XML_SetExternalEntityRefHandler.  The context passed will be 0.
  Unlike external general entities, external parameter entities can only
  be parsed synchronously.  If the external parameter entity is to be
  parsed, it must be parsed during the call to the external entity ref
  handler: the complete sequence of XML_ExternalEntityParserCreate,
  XML_Parse/XML_ParseBuffer and XML_ParserFree calls must be made during
  this call.  After XML_ExternalEntityParserCreate has been called to
  create the parser for the external parameter entity (context must be 0
  for this call), it is illegal to make any calls on the old parser
  until XML_ParserFree has been called on the newly created parser.  If
  the library has been compiled without support for parameter entity
  parsing (ie without XML_DTD being defined), then
  XML_SetParamEntityParsing will return 0 if parsing of parameter
  entities is requested; otherwise it will return non-zero.  }

  function XML_SetParamEntityParsing(parser:XML_Parser; parsing:XML_ParamEntityParsing):integer; external 'expat' name 'XML_SetParamEntityParsing';


  type
     XML_Error = (XML_ERROR_NONE,XML_ERROR_NO_MEMORY,XML_ERROR_SYNTAX,
       XML_ERROR_NO_ELEMENTS,XML_ERROR_INVALID_TOKEN,
       XML_ERROR_UNCLOSED_TOKEN,XML_ERROR_PARTIAL_CHAR,
       XML_ERROR_TAG_MISMATCH,XML_ERROR_DUPLICATE_ATTRIBUTE,
       XML_ERROR_JUNK_AFTER_DOC_ELEMENT,XML_ERROR_PARAM_ENTITY_REF,
       XML_ERROR_UNDEFINED_ENTITY,XML_ERROR_RECURSIVE_ENTITY_REF,
       XML_ERROR_ASYNC_ENTITY,XML_ERROR_BAD_CHAR_REF,
       XML_ERROR_BINARY_ENTITY_REF,XML_ERROR_ATTRIBUTE_EXTERNAL_ENTITY_REF,
       XML_ERROR_MISPLACED_XML_PI,XML_ERROR_UNKNOWN_ENCODING,
       XML_ERROR_INCORRECT_ENCODING,XML_ERROR_UNCLOSED_CDATA_SECTION,
       XML_ERROR_EXTERNAL_ENTITY_HANDLING,
       XML_ERROR_NOT_STANDALONE);

  { If XML_Parse or XML_ParseBuffer have returned 0, then XML_GetErrorCode
  returns information about the error.  }
  function XML_GetErrorCode(parser: XML_Parser): XML_Error; external 'expat' name 'XML_GetCurrentLineNumber';
  { These functions return information about the current parse location.
  They may be called when XML_Parse or XML_ParseBuffer return 0;
  in this case the location is the location of the character at which
  the error was detected.
  They may also be called from any other callback called to report
  some parse event; in this the location is the location of the first
  of the sequence of characters that generated the event.  }

  function XML_GetCurrentLineNumber(parser:XML_Parser):integer; external 'expat' name 'XML_GetCurrentLineNumber';

  function XML_GetCurrentColumnNumber(parser:XML_Parser):integer; external 'expat' name 'XML_GetCurrentColumnNumber';

  function XML_GetCurrentByteIndex(parser:XML_Parser):integer; external 'expat' name 'XML_GetCurrentByteIndex';

  { Return the number of bytes in the current event.
  Returns 0 if the event is in an internal entity.  }
  function XML_GetCurrentByteCount(parser:XML_Parser):integer; external 'expat' name 'XML_GetCurrentByteCount';

  { For backwards compatibility with previous versions.  }

  function XML_GetErrorLineNumber(parser: XML_Parser): integer;  external 'expat' name 'XML_GetCurrentLineNumber';
  function XML_GetErrorColumnNumber(parser: XML_Parser): integer;  external 'expat' name 'XML_GetCurrentColumnNumber';
  function XML_GetErrorByteIndex(parser: XML_Parser): integer;  external 'expat' name 'XML_GetCurrentByteIndex';   
  { Frees memory used by the parser.  }

  procedure XML_ParserFree(parser:XML_Parser); external 'expat' name 'XML_ParserFree';

  { Returns a string describing the error.  }
(* Const before type ignored *)
  function XML_ErrorString(code:integer):PXML_LChar; external 'expat' name 'XML_ErrorString';

{ C++ end of extern C conditionnal removed }
{$endif}
  { not XmlParse_INCLUDED  }

implementation

function XML_GetUserData(parser: XML_Parser): Pointer;
begin
  XML_GetUserData := PPointer(parser)^
end;

end.
