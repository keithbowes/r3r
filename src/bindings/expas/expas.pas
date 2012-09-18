unit expas;
interface

{ $DEFINE EXPAT_2_0}
{ $DEFINE LINK_DYNAMIC}

{$IFDEF EXPAT_2_1}
{$DEFINE EXPAT_2_0}
{$ENDIF}

{$IFDEF EXPAT_2_0}
{$DEFINE EXPAT_1_2}
{$ENDIF}

{$IFDEF EXPAT_1_2}
{$DEFINE EXPAT_1_1}
{$ENDIF}

{$IFDEF EXPAT_1_1}
{$DEFINE EXPAT_1_0}
{$ENDIF}

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
  PPointer = ^pointer;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

{$ifndef __GPC__}
const
{$ifdef MSWINDOWS}
{$ifdef XML_UNICODE}
{$ifdef EXPAT_2_0}
  ExpatLib = 'libexpatw-1';
{$else}
  ExpatLib = 'expatw';
{$endif}
{$else}
{$ifdef EXPAT_2_0}
  ExpatLib = 'libexpat-1';
{$else}
  ExpatLib = 'expat';
{$endif}
{$endif}
{$else}
{$ifdef XML_UNICODE}
  ExpatLib = 'expatw';
{$else}
  ExpatLib = 'expat';
{$endif}
{$endif}
{$else}
{$ifndef ExpatLib}
{$ifdef XML_UNICODE}
  {$define ExpatLib 'expatw'}
{$else}
  {$define ExpatLib 'expat'}
{$endif}
{$endif}
{$endif}

{$IFNDEF LINK_DYNAMIC}
{$linklib libexpat.a}
{$IFDEF MSWINDOWS}
{$linklib msvcrt}
{$ELSE}
{$IFDEF UNIX}
{$linklib c}
{$ENDIF}
{$ENDIF}
{$ENDIF}

type
{$ifdef EXPAT_2_0}
  XML_Bool = ByteBool;
  XML_Index = PtrInt;
{$ifndef __GPC__}
  size_t = PtrUInt;
  XML_Size = PtrUInt;
{$else}
  size_t = SizeType;
  XML_Size = PtrCard;
{$endif}
{$else}
  XML_Bool = integer;
  XML_Index = integer;
  XML_Size = integer;
{$endif}

const
{$ifdef EXPAT_2_0}
  XML_FALSE = false;
  XML_TRUE = true;
{$else}
  XML_FALSE = 0;
  XML_TRUE = 1;
{$endif}

  type
  XML_Parser = pointer;
{$ifdef XML_UNICODE}
     XML_Char = widechar;
     XML_LChar = widechar;
     PXML_Char = ^XML_Char;
     PXML_LChar = ^XML_LChar;
  const
     XML_NsSeparator = WideChar($FFFF);
{$else}
  { not XML_UNICODE  }
  { Information is UTF-8 encoded.  }
     XML_Char = char;
     XML_LChar = char;
     PXML_Char = PChar;
     PXML_LChar = PChar;

  const
     XML_NsSeparator = Chr($FF);
{$endif}
  { not XML_UNICODE  }

type
  PPXML_Char = ^PXML_Char;
     
  { Constructs a new parser; encoding is the encoding specified by the external
  protocol or null if there is none specified.  }

  function XML_ParserCreate(const encoding:PXML_Char):XML_Parser; external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_ParserCreate';

{$IFDEF EXPAT_1_1}
  { Constructs a new parser and namespace processor.  Element type names
  and attribute names that belong to a namespace will be expanded;
  unprefixed attribute names are never expanded; unprefixed element type
  names are expanded only if there is a default namespace. The expanded
  name is the concatenation of the namespace URI, the namespace separator character,
  and the local part of the name.  If the namespace separator is '\0' then
  the namespace URI and the local part will be concatenated without any
  separator.  When a namespace is not declared, the name and prefix will be
  passed through without expansion.  }
  function XML_ParserCreateNS(encoding:PXML_Char; namespaceSeparator:XML_Char):XML_Parser; external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_ParserCreateNS';
{$ENDIF}

  { atts is array of name/value pairs, terminated by 0;
     names and values are 0 terminated.  }

  type

     XML_StartElementHandler = procedure (userData:pointer; name:PXML_Char; atts:PPXML_Char);

     XML_EndElementHandler = procedure (userData:pointer; name:PXML_Char);
  { s is not 0 terminated.  }

     XML_CharacterDataHandler = procedure (userData:pointer; s:PXML_Char; len:integer);
  { target and data are 0 terminated  }

     XML_ProcessingInstructionHandler = procedure (userData:pointer; target:PXML_Char; data:PXML_Char);
  { data is 0 terminated  }

{$IFDEF EXPAT_1_1}
     XML_CommentHandler = procedure (userData:pointer; data:PXML_Char);

     XML_StartCdataSectionHandler = procedure (userData:pointer);

     XML_EndCdataSectionHandler = procedure (userData:pointer);
{$ENDIF}
     
{$IFDEF EXPAT_1_0}
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

     XML_DefaultHandler = procedure (userData:pointer; s:PXML_Char; len:integer);
{$ENDIF}

{$IFDEF EXPAT_1_2}
  { This is called for the start of the DOCTYPE declaration when the
  name of the DOCTYPE is encountered.  }
     XML_StartDoctypeDeclHandler = procedure (userData:pointer; doctypeName:PXML_Char);
  { This is called for the start of the DOCTYPE declaration when the
  closing > is encountered, but after processing any external subset.  }

     XML_EndDoctypeDeclHandler = procedure (userData:pointer);
{$ENDIF}

{$IFDEF EXPAT_1_0}
  { This is called for a declaration of an unparsed (NDATA)
  entity.  The base argument is whatever was set by XML_SetBase.
  The entityName, systemId and notationName arguments will never be null.
  The other arguments may be.  }

     XML_UnparsedEntityDeclHandler = procedure (userData:pointer; entityName:PXML_Char; base:PXML_Char; systemId:PXML_Char; publicId:PXML_Char; 
                   notationName:PXML_Char);
  { This is called for a declaration of notation.
  The base argument is whatever was set by XML_SetBase.
  The notationName will never be null.  The other arguments can be.  }

     XML_NotationDeclHandler = procedure (userData:pointer; notationName:PXML_Char; base:PXML_Char; systemId:PXML_Char; publicId:PXML_Char);
{$ENDIF}

{$IFDEF EXPAT_1_2}
     XML_ExternalParsedEntityDeclHandler = procedure (userData:pointer; entityName:PXML_Char; base:PXML_Char; systemId:PXML_Char; publicId:PXML_Char);

     XML_InternalParsedEntityDeclHandler = procedure (userData:pointer; entityName:PXML_Char; replacementText:PXML_Char; replacementTextLength:integer);
{$ENDIF}

{$IFDEF EXPAT_1_1}
  { When namespace processing is enabled, these are called once for
  each namespace declaration. The call to the start and end element
  handlers occur between the calls to the start and end namespace
  declaration handlers. For an xmlns attribute, prefix will be null.
  For an xmlns="" attribute, uri will be null.  }
     XML_StartNamespaceDeclHandler = procedure (userData:pointer; prefix:PXML_Char; uri:PXML_Char);

     XML_EndNamespaceDeclHandler = procedure (userData:pointer; prefix:PXML_Char);
  { This is called if the document is not standalone (it has an
  external subset or a reference to a parameter entity, but does not
  have standalone="yes"). If this handler returns 0, then processing
  will not continue, and the parser will return a
  XML_ERROR_NOT_STANDALONE error.  }

     XML_NotStandaloneHandler = function (userData:pointer):integer;
{$ENDIF}

{$IFDEF EXPAT_1_0}
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

     XML_ExternalEntityRefHandler = function (parser:XML_Parser; context:PXML_Char; base:PXML_Char; systemId:PXML_Char; publicId:PXML_Char):integer;

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

     PXML_Encoding = ^XML_Encoding;
     XML_Encoding = record
          map : array[0..255] of integer;
          data : pointer;
          convert : function (data:pointer; s:pchar):integer;
          release : procedure (data:pointer);
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

     XML_UnknownEncodingHandler = function (encodingHandlerData:pointer; name:PXML_Char; info:PXML_Encoding):integer;
{$ENDIF}

     XML_Error = (XML_ERROR_NONE,XML_ERROR_NO_MEMORY,XML_ERROR_SYNTAX,
       XML_ERROR_NO_ELEMENTS,XML_ERROR_INVALID_TOKEN,
       XML_ERROR_UNCLOSED_TOKEN,XML_ERROR_PARTIAL_CHAR,
       XML_ERROR_TAG_MISMATCH,XML_ERROR_DUPLICATE_ATTRIBUTE,
       XML_ERROR_JUNK_AFTER_DOC_ELEMENT,XML_ERROR_PARAM_ENTITY_REF,
       XML_ERROR_UNDEFINED_ENTITY,XML_ERROR_RECURSIVE_ENTITY_REF,
       XML_ERROR_ASYNC_ENTITY,XML_ERROR_BAD_CHAR_REF,
       XML_ERROR_BINARY_ENTITY_REF,XML_ERROR_ATTRIBUTE_EXTERNAL_ENTITY_REF,
       XML_ERROR_MISPLACED_XML_PI,XML_ERROR_UNKNOWN_ENCODING,
       XML_ERROR_INCORRECT_ENCODING
       {$IFDEF EXPAT_1_0}
       ,XML_ERROR_UNCLOSED_CDATA_SECTION,
       XML_ERROR_EXTERNAL_ENTITY_HANDLING
       {$ENDIF}
       {$IFDEF EXPAT_1_1}
       ,XML_ERROR_NOT_STANDALONE
       {$ENDIF}
       {$IFDEF EXPAT_2_0}
       ,XML_ERROR_UNEXPECTED_STATE,XML_ERROR_DECLARED_IN_PE,
       XML_ERROR_FEATURE_REQUIRES_XML_DTD,XML_ERROR_CHANGE_FEATURE_ONCE_PARSING,
       { Added in 1.95.7 }
       XML_ERROR_UNBOUND_PREFIX,
       { Added in 1.95.8 }
       XML_ERROR_UNDECLARING_PREFIX,XML_ERROR_INCOMPLETE_PE,
       XML_ERROR_XML_DECL,XML_ERROR_TEXT_DECL,XML_ERROR_PUBLICID,
       XML_ERROR_SUSPENDED,XML_ERROR_NOTSUSPENDED,XML_ERROR_ABORTED,
       XML_ERROR_FINISHED,XML_ERROR_SUSPEND_PE,
       { Added in 2.0 }
       XML_ERROR_RESERVED_PREFIX_XML,XML_ERROR_RESERVED_PREFIX_XMLNS,
       XML_ERROR_RESERVED_NAMESPACE_URI
       {$ENDIF});
       
  XML_Status = integer;
  const
     XML_STATUS_ERROR = 0;
     XML_STATUS_OK = 1;
     XML_STATUS_SUSPENDED = 2;

  procedure XML_SetElementHandler(parser:XML_Parser; startEl:XML_StartElementHandler; endEl:XML_EndElementHandler); external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_SetElementHandler';

  procedure XML_SetCharacterDataHandler(parser:XML_Parser; handler:XML_CharacterDataHandler); external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_SetCharacterDataHandler';

  procedure XML_SetProcessingInstructionHandler(parser:XML_Parser; handler:XML_ProcessingInstructionHandler); external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_SetProcessingInstructionHandler';

{$IFDEF EXPAT_1_1}
  procedure XML_SetCommentHandler(parser:XML_Parser; handler:XML_CommentHandler); external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_SetCommentHandler';

  procedure XML_SetCdataSectionHandler(parser:XML_Parser; startEl:XML_StartCdataSectionHandler; endEl:XML_EndCdataSectionHandler); external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_SetCdataSectionHandler';
{$ENDIF}

{$IFDEF EXPAT_1_0}
  { This sets the default handler and also inhibits expansion of internal entities.
  The entity reference will be passed to the default handler.  }
  procedure XML_SetDefaultHandler(parser:XML_Parser; handler:XML_DefaultHandler); external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_SetDefaultHandler';
{$ENDIF}

{$IFDEF EXPAT_1_1}
  { This sets the default handler but does not inhibit expansion of internal entities.
  The entity reference will not be passed to the default handler.  }
  procedure XML_SetDefaultHandlerExpand(parser:XML_Parser; handler:XML_DefaultHandler); external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_SetDefaultHandlerExpand';
{$ENDIF}

{$IFDEF EXPAT_1_2}
  procedure XML_SetDoctypeDeclHandler(parser:XML_Parser; startEl:XML_StartDoctypeDeclHandler; endEl:XML_EndDoctypeDeclHandler); external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_SetDoctypeDeclHandler';
{$ENDIF}

{$IFDEF EXPAT_1_0}
  procedure XML_SetUnparsedEntityDeclHandler(parser:XML_Parser; handler:XML_UnparsedEntityDeclHandler); external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_SetUnparsedEntityDeclHandler';

  procedure XML_SetNotationDeclHandler(parser:XML_Parser; handler:XML_NotationDeclHandler); external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_SetNotationDeclHandler';
{$ENDIF}

{$IFDEF EXPAT_1_2}
  procedure XML_SetExternalParsedEntityDeclHandler(parser:XML_Parser; handler:XML_ExternalParsedEntityDeclHandler); external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_SetExternalParsedEntityDeclHandler';

  procedure XML_SetInternalParsedEntityDeclHandler(parser:XML_Parser; handler:XML_InternalParsedEntityDeclHandler); external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_SetInternalParsedEntityDeclHandler';
{$ENDIF}

{$IFDEF EXPAT_1_1}
  procedure XML_SetNamespaceDeclHandler(parser:XML_Parser; startEl:XML_StartNamespaceDeclHandler; endEl:XML_EndNamespaceDeclHandler); external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_SetNamespaceDeclHandler';
{$ENDIF}

{$IFDEF EXPAT_2_0}
  procedure XML_SetStartNamespaceDeclHandler(parser:XML_Parser; startEl:XML_StartNamespaceDeclHandler); external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_SetStartNamespaceDeclHandler';

  procedure XML_SetEndNamespaceDeclHandler(parser:XML_Parser; endEl:XML_EndNamespaceDeclHandler); external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_SetEndNamespaceDeclHandler';
{$ENDIF}

{$IFDEF EXPAT_1_1}
  procedure XML_SetNotStandaloneHandler(parser:XML_Parser; handler:XML_NotStandaloneHandler); external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_SetNotStandaloneHandler';
{$ENDIF}

{$IFDEF EXPAT_1_0}
  procedure XML_SetExternalEntityRefHandler(parser:XML_Parser; handler:XML_ExternalEntityRefHandler); external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_SetExternalEntityRefHandler';
{$ENDIF}

{$IFDEF EXPAT_1_1}
  { If a non-null value for arg is specified here, then it will be passed
  as the first argument to the external entity ref handler instead
  of the parser object.  }
  procedure XML_SetExternalEntityRefHandlerArg(_para1:XML_Parser; arg:pointer); external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_SetExternalEntityRefHandlerArg';
{$ENDIF}

{$IFDEF EXPAT_1_0}
  procedure XML_SetUnknownEncodingHandler(parser:XML_Parser; handler:XML_UnknownEncodingHandler; encodingHandlerData:pointer); external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_SetUnknownEncodingHandler';

  { This can be called within a handler for a start element, end element,
  processing instruction or character data.  It causes the corresponding
  markup to be passed to the default handler.  }
  procedure XML_DefaultCurrent(parser:XML_Parser); external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_DefaultCurrent';
{$ENDIF}

{$IFDEF EXPAT_2_0}
  procedure XML_SetReturnNSTriplet(parser: XML_Parser; do_nst: integer); external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_SetReturnNSTriplet';
{$ENDIF}

  { This value is passed as the userData argument to callbacks.  }
  procedure XML_SetUserData(parser:XML_Parser; userData:pointer); external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_SetUserData';

{$IFDEF EXPAT_1_0}
  { Returns the last value set by XML_SetUserData or null.  }
  function XML_GetUserData(parser: XML_Parser): Pointer;
{$ENDIF}

{$IFDEF EXPAT_1_1}
  { This is equivalent to supplying an encoding argument
  to XML_ParserCreate. It must not be called after XML_Parse
  or XML_ParseBuffer.  }
  function XML_SetEncoding(parser:XML_Parser; encoding:PXML_Char):XML_Status; external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_SetEncoding';
{$ENDIF}

{$IFDEF EXPAT_1_0}
  { If this function is called, then the parser will be passed
  as the first argument to callbacks instead of userData.
  The userData will still be accessible using XML_GetUserData.  }
  procedure XML_UseParserAsHandlerArg(parser:XML_Parser); external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_UseParserAsHandlerArg';
{$ENDIF}

{$IFDEF EXPAT_2_0}
  function XML_UseForeignDTD(parser: XML_Parser; useDTD: XML_Bool): XML_Error; external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_UseForeignDTD';
{$ENDIF}

{$IFDEF EXPAT_1_0}
  { Sets the base to be used for resolving relative URIs in system identifiers in
  declarations.  Resolving relative identifiers is left to the application:
  this value will be passed through as the base argument to the
  XML_ExternalEntityRefHandler, XML_NotationDeclHandler
  and XML_UnparsedEntityDeclHandler. The base argument will be copied.
  Returns zero if out of memory, non-zero otherwise.  }
  function XML_SetBase(parser:XML_Parser; base:PXML_Char):XML_Status; external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_SetBase';

  function XML_GetBase(parser:XML_Parser):PXML_Char; external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_GetBase';
{$ENDIF}

{$IFDEF EXPAT_1_1}
  { Returns the number of the attribute/value pairs passed in last call
  to the XML_StartElementHandler that were specified in the start-tag
  rather than defaulted. Each attribute/value pair counts as 2; thus
  this correspondds to an index into the atts array passed to the
  XML_StartElementHandler.  }
  function XML_GetSpecifiedAttributeCount(parser:XML_Parser):integer; external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_GetSpecifiedAttributeCount';
{$ENDIF}

{$IFDEF EXPAT_1_2}
  { Returns the index of the ID attribute passed in the last call to
  XML_StartElementHandler, or -1 if there is no ID attribute.  Each
  attribute/value pair counts as 2; thus this correspondds to an index
  into the atts array passed to the XML_StartElementHandler.  }
  function XML_GetIdAttributeIndex(parser:XML_Parser):integer; external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_GetIdAttributeIndex';
{$ENDIF}

{$IFDEF EXPAT_2_1}
{$IFDEF XML_ATTR_INFO}
  type
    PXML_AttrInfo = ^XML_AttrInfo;
    XML_AttrInfo = record
      nameStart: XML_Index;
      nameEnd: XML_Index;
      valueStart: XML_Index;
      valueEnd: XML_Index;
    end;

  function XML_GetAttributeInfo(parser: XML_Parser): PXML_AttrInfo; external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_GetAttributeInfo';
{$ENDIF}
{$ENDIF}

  { Parses some input. Returns 0 if a fatal error is detected.
  The last call to XML_Parse must have isFinal true;
  len may be zero for this call (or any other).  }
  function XML_Parse(parser:XML_Parser; s:pchar; len:integer; isFinal:XML_Bool):XML_Status; external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_Parse';

  function XML_GetBuffer(parser:XML_Parser; len:integer):pointer; external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_GetBuffer';

  function XML_ParseBuffer(parser:XML_Parser; len:integer; isFinal:XML_Bool):XML_Status; external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_ParseBuffer';

{$IFDEF EXPAT_2_0}
  type
    TXML_Parsing = (XML_INITIALIZED, XML_PARSING, XML_FINISHED, XML_SUSPENDED);
  XML_ParsingStatus = record
    parsing: TXML_Parsing;
    finalBuffer: XML_Bool;
  end;

  function XML_StopParser(parser: XML_Parser; resumable: XML_Bool): XML_Status; external {$IFDEF LYNX_DYNAMIC}ExpatLib{$ENDIF} name 'XML_StopParser';
  function XML_ResumeParser(parser: XML_Parser): XML_Status; external {$IFDEF LYNX_DYNAMIC}ExpatLib{$ENDIF} name 'XML_ResumeParser';
  procedure XML_GetParsingStatus(parser: XML_parser; var status: XML_ParsingStatus); external {$IFDEF LYNX_DYNAMIC}ExpatLib{$ENDIF} name 'XML_GetParsingStatus';
{$ENDIF}

{$IFDEF EXPAT_1_1}
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
  function XML_ExternalEntityParserCreate(parser:XML_Parser; context:PXML_Char; encoding:PXML_Char):XML_Parser; external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_ExternalEntityParserCreate';
{$ENDIF}

{$IFDEF EXPAT_1_2}
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

  function XML_SetParamEntityParsing(parser:XML_Parser; parsing:XML_ParamEntityParsing):integer; external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_SetParamEntityParsing';
{$ENDIF}

{$IFDEF EXPAT_2_1}
  function XML_SetHashSalt(parser:XML_Parser; hash_salt: cardinal):integer; external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_SetHashSalt';
{$ENDIF}

{$IFDEF EXPAT_2_0}
  type
    XML_Content_Type = (XML_CTYPE_NONE, XML_CTYPE_EMPTY, XML_CTYPE_ANY, XML_CTYPE_MIXED,
      XML_CTYPE_NAME, XML_CTYPE_CHOICE, XML_CTYPE_SEQ);

    XML_Content_Quant = (XML_CQUANT_NONE, XML_CQUANT_OPT, XML_CQUANT_REP, XML_CQUANT_PLUS);

  PXML_Content = ^XML_Content;
  XML_Content = record
    thetype: XML_Content_Type;
    quant: XML_Content_Quant;
    name: PXML_Char;
    numchildren: word;
    children: PXml_Content;
  end;

  PXML_Memory_Handling_Suite = record
    malloc_fcn: function(size: size_t): Pointer;
    realloc_fcn: function(ptr: Pointer; size: size_t): Pointer;
    free_fcn: procedure(ptr: Pointer);
  end;

  XML_ElementDeclHandler = procedure(userData: Pointer; name: PXML_Char; model: PXML_Content);
  XML_AttlistDeclHandler = procedure(userData: Pointer; elname, attname, att_type, dflt: PXML_Char; isrequired: integer);
  XML_XmlDeclHandler = procedure(userData: Pointer; version, encoding: PXML_Char; standalone: integer);
  XML_EntityDeclHandler = procedure(userData: Pointer; entityName: PXML_Char; is_parameter_entity: integer; value: PXML_Char; value_length: integer; base, systemId, publicId, notationName: PXML_Char);

  procedure XML_SetElementDeclHandler(parser: XML_Parser; eldecl: XML_ElementDeclHandler); external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_SetElementDeclHandler';
  procedure XML_SetAttlistDeclHandler(parser: XML_Parser; attdecl: XML_AttlistDeclHandler); external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_SetElementDeclHandler';
  procedure XML_SetXmlDeclHandler(parser: XML_Parser; xmldecl: XML_XmlDeclHandler); external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_SetXmlDeclHandler';
  procedure XML_SetEntityDeclHandler(parser: XML_Parser; handler: XML_EntityDeclHandler); external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_SetXmlDeclHandler';

  procedure XML_ParserCreate_MM(encoding: PXML_Char; memsuite: PXML_Memory_Handling_Suite; namespaceSeparator: PXML_Char); external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_ParserCreate_MM';
{$ENDIF}


  { If XML_Parse or XML_ParseBuffer have returned 0, then XML_GetErrorCode
  returns information about the error.  }
  function XML_GetErrorCode(parser: XML_Parser): XML_Error; external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_GetErrorCode';

{$IFDEF EXPAT_1_0}
  { These functions return information about the current parse location.
  They may be called when XML_Parse or XML_ParseBuffer return 0;
  in this case the location is the location of the character at which
  the error was detected.
  They may also be called from any other callback called to report
  some parse event; in this the location is the location of the first
  of the sequence of characters that generated the event.  }

  function XML_GetCurrentLineNumber(parser:XML_Parser):XML_Size; external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_GetCurrentLineNumber';

  function XML_GetCurrentColumnNumber(parser:XML_Parser):XML_Size; external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_GetCurrentColumnNumber';

  function XML_GetCurrentByteIndex(parser:XML_Parser):XML_Index; external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_GetCurrentByteIndex';
{$ELSE}
  function XML_GetErrorLineNumber(parser:XML_Parser):XML_Size; external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_GetErrorLineNumber';
  function XML_GetErrorColumnNumber(parser:XML_Parser):XML_Size; external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_GetErrorColumnNumber';
  function XML_GetErrorByteIndex(parser:XML_Parser):XML_Index; external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_GetErrorByteIndex';
{$ENDIF}

{$IFDEF EXPAT_1_1}
  { Return the number of bytes in the current event.
  Returns 0 if the event is in an internal entity.  }
  function XML_GetCurrentByteCount(parser:XML_Parser):integer; external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_GetCurrentByteCount';
{$ENDIF}

{$IFDEF EXPAT_2_0}
  function XML_GetInputContext(parser: XML_Parser; var offset, size: integer): pchar; external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_GetInputContext';
  procedure XML_FreeContentModel(parser: XML_Parser; model: PXML_Content); external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_FreeContentModel';
  function XML_MemMalloc(parser: XML_Parser; size: size_t): Pointer; external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_MemMalloc';
  function XML_MemRealloc(parser: XML_Parser; ptr: Pointer; size: size_t): Pointer; external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_MemRealloc';
  procedure XML_MemFree(parser: XML_Parser; ptr: Pointer); external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_MemFree';
{$ENDIF}

  { Frees memory used by the parser.  }

  procedure XML_ParserFree(parser:XML_Parser); external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_ParserFree';

  { Returns a string describing the error.  }
  function XML_ErrorString(code:XML_Error):PXML_LChar; external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_ErrorString';

{$IFDEF EXPAT_1_0}
const
  { For backwards compatibility with previous versions.  }
  XML_GetErrorLineNumber: function(parser:XML_Parser):XML_Size   = XML_GetCurrentLineNumber;
  XML_GetErrorColumnNumber: function(parser:XML_Parser):XML_Size = XML_GetCurrentColumnNumber;
  XML_GetErrorByteIndex: function(parser:XML_Parser):XML_Index   = XML_GetCurrentByteIndex;   
{$ENDIF}

{$IFDEF EXPAT_2_0}
type
  XML_Expat_Version = record
    major: integer;
    minor: integer;
    micro: integer;
  end;

  XML_FeatureEnum = (XML_FEATURE_END, XML_FEATURE_UNICODE,
    XML_FEATURE_UNICODE_WCHAR_T, XML_FEATURE_DTD,
    XML_FEATURE_CONTEXT_BYTES, XML_FEATURE_MIN_SIZE,
    XML_FEATURE_SIZEOF_XML_CHAR, XML_FEATURE_SIZEOF_XML_LCHAR,
    XML_FEATURE_NS, XML_FEATURE_LARGE_SIZE, XML_FEATURE_ATTR_INFO);
  PXML_Feature = ^XML_Feature;
  XML_Feature = record
    feature: XML_FeatureEnum;
    name: PXML_LChar;
    value: longint;
  end;

  function XML_ExpatVersion: PXML_LChar; external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_ExpatVersion';
  function XML_ExpatVersionInfo: XML_Expat_Version; external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_ExpatVersionInfo';

  function XML_GetFeatureList: PXml_Feature; external {$IFDEF LINK_DYNAMIC}ExpatLib{$ENDIF} name 'XML_GetFeatureList';
{$ENDIF}

implementation

{$IFDEF EXPAT_1_0}
function XML_GetUserData(parser: XML_Parser): Pointer;
begin
  XML_GetUserData := PPointer(parser)^
end;
{$ENDIF}

end.
