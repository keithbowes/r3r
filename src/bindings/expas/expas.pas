{
                            __  __            _
                         ___\ \/ /_ __   __ _| |_
                        / _ \\  /| '_ \ / _` | __|
                       |  __//  \| |_) | (_| | |_
                        \___/_/\_\ .__/ \__,_|\__|
                                 |_| XML parser

   Copyright (c) 1997-2000 Thai Open Source Software Center Ltd
   Copyright (c) 2000-2017 Expat development team
   Licensed under the MIT license:

   Permission is  hereby granted,  free of charge,  to any  person obtaining
   a  copy  of  this  software   and  associated  documentation  files  (the
   "Software"),  to  deal in  the  Software  without restriction,  including
   without  limitation the  rights  to use,  copy,  modify, merge,  publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons  to whom  the Software  is  furnished to  do so,  subject to  the
   following conditions:

   The above copyright  notice and this permission notice  shall be included
   in all copies or substantial portions of the Software.

   THE  SOFTWARE  IS  PROVIDED  "AS  IS",  WITHOUT  WARRANTY  OF  ANY  KIND,
   EXPRESS  OR IMPLIED,  INCLUDING  BUT  NOT LIMITED  TO  THE WARRANTIES  OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR  COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR  OTHER LIABILITY, WHETHER  IN AN  ACTION OF CONTRACT,  TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE.
}

unit Expas;

interface

{ $DEFINE EXPAT_1_2}

{$IFDEF EXPAT_2_2_1}
{$DEFINE EXPAT_2_1}
{$ENDIF}

{$IFDEF EXPAT_2_1}
{$DEFINE EXPAT_2_0_1}
{$ENDIF}

{$IFDEF EXPAT_2_0_1}
{$DEFINE EXPAT_2_0}
{$ENDIF}

{$IFDEF EXPAT_2_0}
{$DEFINE EXPAT_1_2}
{$ENDIF}

{$IFDEF EXPAT_1_2}
{$DEFINE EXPAT_1_1}
{ Exactly 1.2; use features of this version that were removed in 2.0+ }
{$IFNDEF EXPAT_2_0}
{$DEFINE EXPAT_1_2_EXACT}
{$ENDIF}
{$ENDIF}

{$IFDEF EXPAT_1_1}
{$DEFINE EXPAT_1_0}
{$ENDIF}

{$CALLING cdecl}

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

{$IFDEF FPC}
{$modeswitch classicprocvars}
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

type
{$ifdef EXPAT_2_0}
  XML_Bool = WordBool;
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
  function XML_ParserCreate(const encoding:PXML_Char):XML_Parser; external ExpatLib name 'XML_ParserCreate';

{$IFDEF EXPAT_1_1}
  { Constructs a new parser and namespace processor.  Element type
   names and attribute names that belong to a namespace will be
   expanded; unprefixed attribute names are never expanded; unprefixed
   element type names are expanded only if there is a default
   namespace. The expanded name is the concatenation of the namespace
   URI, the namespace separator character, and the local part of the
   name.  If the namespace separator is '\0' then the namespace URI
   and the local part will be concatenated without any separator.
   It is a programming error to use the separator '\0' with namespace
   triplets (see XML_SetReturnNSTriplet).  }
  function XML_ParserCreateNS(encoding:PXML_Char; namespaceSeparator:XML_Char):XML_Parser; external ExpatLib name 'XML_ParserCreateNS';
{$ENDIF}

  type
  { atts is array of name/value pairs, terminated by 0;
     names and values are 0 terminated.  }
     XML_StartElementHandler = procedure (userData:pointer; name:PXML_Char; atts:PPXML_Char);
     XML_EndElementHandler = procedure (userData:pointer; name:PXML_Char);

  { s is not 0 terminated.  }
     XML_CharacterDataHandler = procedure (userData:pointer; s:PXML_Char; len:integer);

  { target and data are 0 terminated  }
     XML_ProcessingInstructionHandler = procedure (userData:pointer; target:PXML_Char; data:PXML_Char);

{$IFDEF EXPAT_1_1}
  { data is 0 terminated  }
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
  { This is called for the start of the DOCTYPE declaration, before
   any DTD or internal subset is parsed.  }
     XML_StartDoctypeDeclHandler = procedure (userData:pointer; doctypeName:PXML_Char{$IFDEF EXPAT_2_0}; pubid, sysid: PXML_Char; has_internal_subset: integer{$ENDIF});

  { This is called for the start of the DOCTYPE declaration when the
  closing > is encountered, but after processing any external subset.  }
     XML_EndDoctypeDeclHandler = procedure (userData:pointer);
{$ENDIF}

{$IFDEF EXPAT_1_0}
  { OBSOLETE -- OBSOLETE -- OBSOLETE
   This handler has been superseded by the EntityDeclHandler above.
   It is provided here for backward compatibility.
   This is called for a declaration of an unparsed (NDATA) entity.
   The base argument is whatever was set by XML_SetBase. The
   entityName, systemId and notationName arguments will never be
   NULL. The other arguments may be.  }
     XML_UnparsedEntityDeclHandler = procedure (userData:pointer; entityName:PXML_Char; base:PXML_Char; systemId:PXML_Char; publicId:PXML_Char;
                   notationName:PXML_Char);

     { This is called for a declaration of notation.  The base argument is
      whatever was set by XML_SetBase. The notationName will never be
      NULL.  The other arguments can be.  }
     XML_NotationDeclHandler = procedure (userData:pointer; notationName:PXML_Char; base:PXML_Char; systemId:PXML_Char; publicId:PXML_Char);
{$ENDIF}

{$IFDEF EXPAT_1_2_EXACT}
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

{$IFDEF EXPAT_2_0}
  {  This is called in two situations:
   1) An entity reference is encountered for which no declaration
      has been read *and* this is not an error.
   2) An internal entity reference is read, but not expanded, because
      XML_SetDefaultHandler has been called.
   Note: skipped parameter entities in declarations and skipped general
         entities in attribute values cannot be reported, because
         the event would be out of sync with the reporting of the
         declarations or attribute values
  }
  XML_SkippedEntityHandler = procedure(userData: Pointer; entityName: XML_Char; is_parameter_entity: integer);
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
       { Added in 1.95.4 }
       ,XML_ERROR_UNEXPECTED_STATE,XML_ERROR_DECLARED_IN_PE,
       { Added in 1.95.5 }
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
       {$IFDEF EXPAT_2_2_1}
       { Added in 2.2.1 }
       ,XML_ERROR_INVALID_ARGUMENT
       {$ENDIF}
       {$ENDIF});

  XML_Status = (XML_STATUS_ERROR, XML_STATUS_OK{$IFDEF EXPAT_2_0}, XML_STATUS_SUSPENDED{$ENDIF});

  procedure XML_SetElementHandler(parser:XML_Parser; startEl:XML_StartElementHandler; endEl:XML_EndElementHandler); external ExpatLib name 'XML_SetElementHandler';

  procedure XML_SetCharacterDataHandler(parser:XML_Parser; handler:XML_CharacterDataHandler); external ExpatLib name 'XML_SetCharacterDataHandler';

  procedure XML_SetProcessingInstructionHandler(parser:XML_Parser; handler:XML_ProcessingInstructionHandler); external ExpatLib name 'XML_SetProcessingInstructionHandler';

{$IFDEF EXPAT_1_1}
  procedure XML_SetCommentHandler(parser:XML_Parser; handler:XML_CommentHandler); external ExpatLib name 'XML_SetCommentHandler';

  procedure XML_SetCdataSectionHandler(parser:XML_Parser; startEl:XML_StartCdataSectionHandler; endEl:XML_EndCdataSectionHandler); external ExpatLib name 'XML_SetCdataSectionHandler';
{$ENDIF}

{$IFDEF EXPAT_1_0}
  { This sets the default handler and also inhibits expansion of
   internal entities. These entity references will be passed to the
   default handler, or to the skipped entity handler, if one is set.  }
  procedure XML_SetDefaultHandler(parser:XML_Parser; handler:XML_DefaultHandler); external ExpatLib name 'XML_SetDefaultHandler';
{$ENDIF}

{$IFDEF EXPAT_1_1}
  { This sets the default handler but does not inhibit expansion of internal entities.
  The entity reference will not be passed to the default handler.  }
  procedure XML_SetDefaultHandlerExpand(parser:XML_Parser; handler:XML_DefaultHandler); external ExpatLib name 'XML_SetDefaultHandlerExpand';
{$ENDIF}

{$IFDEF EXPAT_1_2}
  procedure XML_SetDoctypeDeclHandler(parser:XML_Parser; startEl:XML_StartDoctypeDeclHandler; endEl:XML_EndDoctypeDeclHandler); external ExpatLib name 'XML_SetDoctypeDeclHandler';
{$ENDIF}

{$IFDEF EXPAT_1_0}
  procedure XML_SetUnparsedEntityDeclHandler(parser:XML_Parser; handler:XML_UnparsedEntityDeclHandler); external ExpatLib name 'XML_SetUnparsedEntityDeclHandler';

  procedure XML_SetNotationDeclHandler(parser:XML_Parser; handler:XML_NotationDeclHandler); external ExpatLib name 'XML_SetNotationDeclHandler';
{$ENDIF}

{$IFDEF EXPAT_1_2_EXACT}
  procedure XML_SetExternalParsedEntityDeclHandler(parser:XML_Parser; handler:XML_ExternalParsedEntityDeclHandler); external ExpatLib name 'XML_SetExternalParsedEntityDeclHandler';

  procedure XML_SetInternalParsedEntityDeclHandler(parser:XML_Parser; handler:XML_InternalParsedEntityDeclHandler); external ExpatLib name 'XML_SetInternalParsedEntityDeclHandler';
{$ENDIF}

{$IFDEF EXPAT_1_1}
  procedure XML_SetNamespaceDeclHandler(parser:XML_Parser; startEl:XML_StartNamespaceDeclHandler; endEl:XML_EndNamespaceDeclHandler); external ExpatLib name 'XML_SetNamespaceDeclHandler';
{$ENDIF}

{$IFDEF EXPAT_2_0}
  procedure XML_SetStartNamespaceDeclHandler(parser:XML_Parser; startEl:XML_StartNamespaceDeclHandler); external ExpatLib name 'XML_SetStartNamespaceDeclHandler';

  procedure XML_SetEndNamespaceDeclHandler(parser:XML_Parser; endEl:XML_EndNamespaceDeclHandler); external ExpatLib name 'XML_SetEndNamespaceDeclHandler';
{$ENDIF}

{$IFDEF EXPAT_1_1}
  procedure XML_SetNotStandaloneHandler(parser:XML_Parser; handler:XML_NotStandaloneHandler); external ExpatLib name 'XML_SetNotStandaloneHandler';
{$ENDIF}

{$IFDEF EXPAT_1_0}
  procedure XML_SetExternalEntityRefHandler(parser:XML_Parser; handler:XML_ExternalEntityRefHandler); external ExpatLib name 'XML_SetExternalEntityRefHandler';
{$ENDIF}

{$IFDEF EXPAT_1_1}
  { If a non-null value for arg is specified here, then it will be passed
  as the first argument to the external entity ref handler instead
  of the parser object.  }
  procedure XML_SetExternalEntityRefHandlerArg(_para1:XML_Parser; arg:pointer); external ExpatLib name 'XML_SetExternalEntityRefHandlerArg';
{$ENDIF}

{$IFDEF EXPAT_1_0}
  { This is called for an encoding that is unknown to the parser.
   The encodingHandlerData argument is that which was passed as the
   second argument to XML_SetUnknownEncodingHandler.
   The name argument gives the name of the encoding as specified in
   the encoding declaration.
   If the callback can provide information about the encoding, it must
   fill in the XML_Encoding structure, and return XML_STATUS_OK.
   Otherwise it must return XML_STATUS_ERROR.
   If info does not describe a suitable encoding, then the parser will
   return an XML_UNKNOWN_ENCODING error.  }
  procedure XML_SetUnknownEncodingHandler(parser:XML_Parser; handler:XML_UnknownEncodingHandler; encodingHandlerData:pointer); external ExpatLib name 'XML_SetUnknownEncodingHandler';

  { This can be called within a handler for a start element, end
   element, processing instruction or character data.  It causes the
   corresponding markup to be passed to the default handler.  }
  procedure XML_DefaultCurrent(parser:XML_Parser); external ExpatLib name 'XML_DefaultCurrent';
{$ENDIF}

{$IFDEF EXPAT_2_0}
  { If do_nst is non-zero, and namespace processing is in effect, and
   a name has a prefix (i.e. an explicit namespace qualifier) then
   that name is returned as a triplet in a single string separated by
   the separator character specified when the parser was created: URI
   + sep + local_name + sep + prefix.
   If do_nst is zero, then namespace information is returned in the
   default manner (URI + sep + local_name) whether or not the name
   has a prefix.
   Note: Calling XML_SetReturnNSTriplet after XML_Parse or
     XML_ParseBuffer has no effect.  }
  procedure XML_SetReturnNSTriplet(parser: XML_Parser; do_nst: integer); external ExpatLib name 'XML_SetReturnNSTriplet';
{$ENDIF}

  { This value is passed as the userData argument to callbacks.  }
  procedure XML_SetUserData(parser:XML_Parser; userData:pointer); external ExpatLib name 'XML_SetUserData';

{$IFDEF EXPAT_1_0}
  { Returns the last value set by XML_SetUserData or null.  }
  function XML_GetUserData(parser: XML_Parser): Pointer;
{$ENDIF}

{$IFDEF EXPAT_1_1}
  { This is equivalent to supplying an encoding argument to
   XML_ParserCreate. On success XML_SetEncoding returns non-zero,
   zero otherwise.
   Note: Calling XML_SetEncoding after XML_Parse or XML_ParseBuffer
     has no effect and returns XML_STATUS_ERROR.  }
  function XML_SetEncoding(parser:XML_Parser; encoding:PXML_Char):XML_Status; external ExpatLib name 'XML_SetEncoding';
{$ENDIF}

{$IFDEF EXPAT_1_0}
  { If this function is called, then the parser will be passed
  as the first argument to callbacks instead of userData.
  The userData will still be accessible using XML_GetUserData.  }
  procedure XML_UseParserAsHandlerArg(parser:XML_Parser); external ExpatLib name 'XML_UseParserAsHandlerArg';
{$ENDIF}

{$IFDEF EXPAT_2_0}
  {  If useDTD == XML_TRUE is passed to this function, then the parser
   will assume that there is an external subset, even if none is
   specified in the document. In such a case the parser will call the
   externalEntityRefHandler with a value of NULL for the systemId
   argument (the publicId and context arguments will be NULL as well).
   Note: For the purpose of checking WFC: Entity Declared, passing
     useDTD == XML_TRUE will make the parser behave as if the document
     had a DTD with an external subset.
   Note: If this function is called, then this must be done before
     the first call to XML_Parse or XML_ParseBuffer, since it will
     have no effect after that.  Returns
     XML_ERROR_CANT_CHANGE_FEATURE_ONCE_PARSING.
   Note: If the document does not have a DOCTYPE declaration at all,
     then startDoctypeDeclHandler and endDoctypeDeclHandler will not
     be called, despite an external subset being parsed.
   Note: If XML_DTD is not defined when Expat is compiled, returns
     XML_ERROR_FEATURE_REQUIRES_XML_DTD.
   Note: If parser == NULL, returns XML_ERROR_INVALID_ARGUMENT.  }
  function XML_UseForeignDTD(parser: XML_Parser; useDTD: XML_Bool): XML_Error; external ExpatLib name 'XML_UseForeignDTD';
{$ENDIF}

{$IFDEF EXPAT_1_0}
  { Sets the base to be used for resolving relative URIs in system identifiers in
  declarations.  Resolving relative identifiers is left to the application:
  this value will be passed through as the base argument to the
  XML_ExternalEntityRefHandler, XML_NotationDeclHandler
  and XML_UnparsedEntityDeclHandler. The base argument will be copied.
  Returns zero if out of memory, non-zero otherwise.
  Note: If parser = nil, returns XML_ERROR_INVALID_ARGUMENT.  }
  function XML_SetBase(parser:XML_Parser; base:PXML_Char):XML_Status; external ExpatLib name 'XML_SetBase';
  function XML_GetBase(parser:XML_Parser):PXML_Char; external ExpatLib name 'XML_GetBase';
{$ENDIF}

{$IFDEF EXPAT_1_1}
  { Returns the number of the attribute/value pairs passed in last call
  to the XML_StartElementHandler that were specified in the start-tag
  rather than defaulted. Each attribute/value pair counts as 2; thus
  this correspondds to an index into the atts array passed to the
  XML_StartElementHandler.  Returns -1 if parser = nil.   }
  function XML_GetSpecifiedAttributeCount(parser:XML_Parser):integer; external ExpatLib name 'XML_GetSpecifiedAttributeCount';
{$ENDIF}

{$IFDEF EXPAT_1_2}
  { Returns the index of the ID attribute passed in the last call to
   XML_StartElementHandler, or -1 if there is no ID attribute or
   parser = nil.  Each attribute/value pair counts as 2; thus this
   correspondds to an index into the atts array passed to the
   XML_StartElementHandler.  }
  function XML_GetIdAttributeIndex(parser:XML_Parser):integer; external ExpatLib name 'XML_GetIdAttributeIndex';
{$ENDIF}

{$IFDEF EXPAT_2_1}
{$IFDEF XML_ATTR_INFO}
  type
  { Source file byte offsets for the start and end of attribute names and values.
   The value indices are exclusive of surrounding quotes; thus in a UTF-8 source
   file an attribute value of "blah" will yield:
   info^.valueEnd - info^.valueStart = 4 bytes.
   }
    PXML_AttrInfo = ^XML_AttrInfo;
    XML_AttrInfo = record
      nameStart: XML_Index;
      nameEnd: XML_Index;
      valueStart: XML_Index;
      valueEnd: XML_Index;
    end;

  { Returns an array of XML_AttrInfo structures for the attribute/value pairs
   passed in last call to the XML_StartElementHandler that were specified
   in the start-tag rather than defaulted. Each attribute/value pair counts
   as 1; thus the number of entries in the array is
   XML_GetSpecifiedAttributeCount(parser) / 2.  }
  function XML_GetAttributeInfo(parser: XML_Parser): PXML_AttrInfo; external ExpatLib name 'XML_GetAttributeInfo';
{$ENDIF}
{$ENDIF}

  { Parses some input. Returns XML_STATUS_ERROR if a fatal error is
   detected.  The last call to XML_Parse must have isFinal true; len
   may be zero for this call (or any other).
   Though the return values for these functions has always been
   described as a Boolean value, the implementation, at least for the
   1.95.x series, has always returned exactly one of the XML_Status
   values.  }
  function XML_Parse(parser:XML_Parser; s:pchar; len:integer; isFinal:XML_Bool):XML_Status; external ExpatLib name 'XML_Parse';

  function XML_GetBuffer(parser:XML_Parser; len:integer):pointer; external ExpatLib name 'XML_GetBuffer';

  function XML_ParseBuffer(parser:XML_Parser; len:integer; isFinal:XML_Bool):XML_Status; external ExpatLib name 'XML_ParseBuffer';

{$IFDEF EXPAT_2_0}
  type
    TXML_Parsing = (XML_INITIALIZED, XML_PARSING, XML_FINISHED, XML_SUSPENDED);
  XML_ParsingStatus = record
    parsing: TXML_Parsing;
    finalBuffer: XML_Bool;
  end;

  { Stops parsing, causing XML_Parse() or XML_ParseBuffer() to return.
   Must be called from within a call-back handler, except when aborting
   (resumable = 0) an already suspended parser. Some call-backs may
   still follow because they would otherwise get lost. Examples:
   - endElementHandler() for empty elements when stopped in
     startElementHandler(), 
   - endNameSpaceDeclHandler() when stopped in endElementHandler(), 
   and possibly others.
   Can be called from most handlers, including DTD related call-backs,
   except when parsing an external parameter entity and resumable != 0.
   Returns XML_STATUS_OK when successful, XML_STATUS_ERROR otherwise.
   Possible error codes: 
   - XML_ERROR_SUSPENDED: when suspending an already suspended parser.
   - XML_ERROR_FINISHED: when the parser has already finished.
   - XML_ERROR_SUSPEND_PE: when suspending while parsing an external PE.
   When resumable != 0 (true) then parsing is suspended, that is, 
   XML_Parse() and XML_ParseBuffer() return XML_STATUS_SUSPENDED. 
   Otherwise, parsing is aborted, that is, XML_Parse() and XML_ParseBuffer()
   return XML_STATUS_ERROR with error code XML_ERROR_ABORTED.
   *Note*:
   This will be applied to the current parser instance only, that is, if
   there is a parent parser then it will continue parsing when the
   externalEntityRefHandler() returns. It is up to the implementation of
   the externalEntityRefHandler() to call XML_StopParser() on the parent
   parser (recursively), if one wants to stop parsing altogether.
   When suspended, parsing can be resumed by calling XML_ResumeParser().  }
  function XML_StopParser(parser: XML_Parser; resumable: XML_Bool): XML_Status; external ExpatLib name 'XML_StopParser';

  { Resumes parsing after it has been suspended with XML_StopParser().
   Must not be called from within a handler call-back. Returns same
   status codes as XML_Parse() or XML_ParseBuffer().
   Additional error code XML_ERROR_NOT_SUSPENDED possible.   
   *Note*:
   This must be called on the most deeply nested child parser instance
   first, and on its parent parser only after the child parser has finished,
   to be applied recursively until the document entity's parser is restarted.
   That is, the parent parser will not resume by itself and it is up to the
   application to call XML_ResumeParser() on it at the appropriate moment.  }
  function XML_ResumeParser(parser: XML_Parser): XML_Status; external ExpatLib name 'XML_ResumeParser';

  { Returns status of parser with respect to being initialized, parsing,
   finished, or suspended and processing the final buffer.
   XXX XML_Parse() and XML_ParseBuffer() should return XML_ParsingStatus,
   XXX with XML_FINISHED_OK or XML_FINISHED_ERROR replacing XML_FINISHED }
  procedure XML_GetParsingStatus(parser: XML_Parser; var status: XML_ParsingStatus); external ExpatLib name 'XML_GetParsingStatus';
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
  function XML_ExternalEntityParserCreate(parser:XML_Parser; context:PXML_Char; encoding:PXML_Char):XML_Parser; external ExpatLib name 'XML_ExternalEntityParserCreate';
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
  entities is requested; otherwise it will return non-zero.
  Note: If parser = nil, the function will do nothing and return 0.  }

  function XML_SetParamEntityParsing(parser:XML_Parser; parsing:XML_ParamEntityParsing):integer; external ExpatLib name 'XML_SetParamEntityParsing';
{$ENDIF}

{$IFDEF EXPAT_2_1}
  { Sets the hash salt to use for internal hash calculations.
   Helps in preventing DoS attacks based on predicting hash
   function behavior. This must be called before parsing is started.
   Returns 1 if successful, 0 when called after parsing has started.
   Note: If parser == NULL, the function will do nothing and return 0.  }
  function XML_SetHashSalt(parser:XML_Parser; hash_salt: cardinal):integer; external ExpatLib name 'XML_SetHashSalt';
{$ENDIF}

{$IFDEF EXPAT_2_0}
  { If type == XML_CTYPE_EMPTY or XML_CTYPE_ANY, then quant will be
   XML_CQUANT_NONE, and the other fields will be zero or NULL.
   If type == XML_CTYPE_MIXED, then quant will be NONE or REP and
   numchildren will contain number of elements that may be mixed in
   and children point to an array of XML_Content cells that will be
   all of XML_CTYPE_NAME type with no quantification.
   If type == XML_CTYPE_NAME, then the name points to the name, and
   the numchildren field will be zero and children will be NULL. The
   quant fields indicates any quantifiers placed on the name.
   CHOICE and SEQ will have name NULL, the number of children in
   numchildren and children will point, recursively, to an array
   of XML_Content cells.
   The EMPTY, ANY, and MIXED types will only occur at top level.  }
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

  { This is called for an element declaration. See above for
   description of the model argument. It's the caller's responsibility
   to free model when finished with it.  }
  XML_ElementDeclHandler = procedure(userData: Pointer; name: PXML_Char; model: PXML_Content);

  { The Attlist declaration handler is called for *each* attribute. So
   a single Attlist declaration with multiple attributes declared will
   generate multiple calls to this handler. The "default" parameter
   may be NULL in the case of the "#IMPLIED" or "#REQUIRED"
   keyword. The "isrequired" parameter will be true and the default
   value will be NULL in the case of "#REQUIRED". If "isrequired" is
   true and default is non-NULL, then this is a "#FIXED" default.  }
  XML_AttlistDeclHandler = procedure(userData: Pointer; elname, attname, att_type, dflt: PXML_Char; isrequired: integer);

  { The XML declaration handler is called for *both* XML declarations
   and text declarations. The way to distinguish is that the version
   parameter will be NULL for text declarations. The encoding
   parameter may be NULL for XML declarations. The standalone
   parameter will be -1, 0, or 1 indicating respectively that there
   was no standalone parameter in the declaration, that it was given
   as no, or that it was given as yes.  }
  XML_XmlDeclHandler = procedure(userData: Pointer; version, encoding: PXML_Char; standalone: integer);
  XML_EntityDeclHandler = procedure(userData: Pointer; entityName: PXML_Char; is_parameter_entity: integer; value: PXML_Char; value_length: integer; base, systemId, publicId, notationName: PXML_Char);

  procedure XML_SetElementDeclHandler(parser: XML_Parser; eldecl: XML_ElementDeclHandler); external ExpatLib name 'XML_SetElementDeclHandler';
  procedure XML_SetAttlistDeclHandler(parser: XML_Parser; attdecl: XML_AttlistDeclHandler); external ExpatLib name 'XML_SetElementDeclHandler';
  procedure XML_SetXmlDeclHandler(parser: XML_Parser; xmldecl: XML_XmlDeclHandler); external ExpatLib name 'XML_SetXmlDeclHandler';
  procedure XML_SetEntityDeclHandler(parser: XML_Parser; handler: XML_EntityDeclHandler); external ExpatLib name 'XML_SetXmlDeclHandler';

  { Constructs a new parser using the memory management suite referred to
    by memsuite. If memsuite is NULL, then use the standard library memory
    suite. If namespaceSeparator is non-NULL it creates a parser with
    namespace processing as described above. The character pointed at
    will serve as the namespace separator.
    All further memory operations used for the created parser will come from
    the given suite. }
  function  XML_ParserCreate_MM(encoding: PXML_Char; memsuite: PXML_Memory_Handling_Suite; namespaceSeparator: PXML_Char): XML_Parser; external ExpatLib name 'XML_ParserCreate_MM';
  { Prepare a parser object to be re-used.  This is particularly
    valuable when memory allocation overhead is disproportionately high,
    such as when a large number of small documnents need to be parsed.
    All handlers are cleared from the parser, except for the
    unknownEncodingHandler. The parser's external state is re-initialized
    except for the values of ns and ns_triplets.
    Added in Expat 1.95.3. }
  function XML_ParserReset(parser: XML_Parser; encoding: PXML_Char): XML_Bool; external ExpatLib name 'XML_ParserReset';
{$ENDIF}

  { If XML_Parse or XML_ParseBuffer have returned XML_STATUS_ERROR, then
   XML_GetErrorCode returns information about the error.  }
  function XML_GetErrorCode(parser: XML_Parser): XML_Error; external ExpatLib name 'XML_GetErrorCode';

{$IFDEF EXPAT_1_0}
   { These functions return information about the current parse
   location.  They may be called from any callback called to report
   some parse event; in this case the location is the location of the
   first of the sequence of characters that generated the event.  When
   called from callbacks generated by declarations in the document
   prologue, the location identified isn't as neatly defined, but will
   be within the relevant markup.  When called outside of the callback
   functions, the position indicated will be just past the last parse
   event (regardless of whether there was an associated callback).

   They may also be called after returning from a call to XML_Parse
   or XML_ParseBuffer.  If the return value is XML_STATUS_ERROR then
   the location is the location of the character at which the error
   was detected; otherwise the location is the location of the last
   parse event, as described above.

   Note: XML_GetCurrentLineNumber and XML_GetCurrentColumnNumber return 0 to indicate an error.
   Note: XML_GetCurrentByteIndex returns -1 to indicate an error. }

  function XML_GetCurrentLineNumber(parser:XML_Parser):XML_Size; external ExpatLib name 'XML_GetCurrentLineNumber';

  function XML_GetCurrentColumnNumber(parser:XML_Parser):XML_Size; external ExpatLib name 'XML_GetCurrentColumnNumber';

  function XML_GetCurrentByteIndex(parser:XML_Parser):XML_Index; external ExpatLib name 'XML_GetCurrentByteIndex';
{$ELSE}
  function XML_GetErrorLineNumber(parser:XML_Parser):XML_Size; external ExpatLib name 'XML_GetErrorLineNumber';
  function XML_GetErrorColumnNumber(parser:XML_Parser):XML_Size; external ExpatLib name 'XML_GetErrorColumnNumber';
  function XML_GetErrorByteIndex(parser:XML_Parser):XML_Index; external ExpatLib name 'XML_GetErrorByteIndex';
{$ENDIF}

{$IFDEF EXPAT_1_1}
  { Return the number of bytes in the current event.
  Returns 0 if the event is in an internal entity.  }
  function XML_GetCurrentByteCount(parser:XML_Parser):integer; external ExpatLib name 'XML_GetCurrentByteCount';
{$ENDIF}

{$IFDEF EXPAT_2_0}
  { If XML_CONTEXT_BYTES is defined, returns the input buffer, sets
   the integer pointed to by offset to the offset within this buffer
   of the current parse position, and sets the integer pointed to by size
   to the size of this buffer (the number of input bytes). Otherwise
   returns a NULL pointer. Also returns a NULL pointer if a parse isn't
   active.
   NOTE: The character pointer returned should not be used outside
   the handler that makes the call.  }
  function XML_GetInputContext(parser: XML_Parser; var offset, size: integer): pchar; external ExpatLib name 'XML_GetInputContext';

  { Frees the content model passed to the element declaration handler  }
  procedure XML_FreeContentModel(parser: XML_Parser; model: PXML_Content); external ExpatLib name 'XML_FreeContentModel';

  { Exposing the memory handling functions used in Expat  }
  function XML_MemMalloc(parser: XML_Parser; size: size_t): Pointer; external ExpatLib name 'XML_MemMalloc';
  function XML_MemRealloc(parser: XML_Parser; ptr: Pointer; size: size_t): Pointer; external ExpatLib name 'XML_MemRealloc';
  procedure XML_MemFree(parser: XML_Parser; ptr: Pointer); external ExpatLib name 'XML_MemFree';
{$ENDIF}

  { Frees memory used by the parser.  }
  procedure XML_ParserFree(parser:XML_Parser); external ExpatLib name 'XML_ParserFree';

  { Returns a string describing the error.  }
  function XML_ErrorString(code:XML_Error):PXML_LChar; external ExpatLib name 'XML_ErrorString';

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

  { Added in Expat 1.95.5.  }
  XML_FeatureEnum = (XML_FEATURE_END, XML_FEATURE_UNICODE,
    XML_FEATURE_UNICODE_WCHAR_T, XML_FEATURE_DTD,
    XML_FEATURE_CONTEXT_BYTES, XML_FEATURE_MIN_SIZE,
    XML_FEATURE_SIZEOF_XML_CHAR, XML_FEATURE_SIZEOF_XML_LCHAR,
    XML_FEATURE_NS{$IFDEF EXPAT_2_0_1}, XML_FEATURE_LARGE_SIZE{$ENDIF}
    {$IFDEF EXPAT_2_1}, XML_FEATURE_ATTR_INFO{$ENDIF});
  PXML_Feature = ^XML_Feature;
  XML_Feature = record
    feature: XML_FeatureEnum;
    name: PXML_LChar;
    value: integer;
  end;

  { Return a string containing the version number of this expat  }
  function XML_ExpatVersion: PXML_LChar; external ExpatLib name 'XML_ExpatVersion';

  { Return an XML_Expat_Version record containing numeric version
   number information for this version of expat.  }
  function XML_ExpatVersionInfo: XML_Expat_Version; external ExpatLib name 'XML_ExpatVersionInfo';

  function XML_GetFeatureList: PXml_Feature; external ExpatLib name 'XML_GetFeatureList';

  function XML_MAJOR: integer;
  function XML_MINOR: integer;
  function XML_MICRO: integer;

{$ENDIF}

implementation

{$IFDEF EXPAT_1_0}
function XML_GetUserData(parser: XML_Parser): Pointer;
type
  PPointer = ^Pointer;
begin
  XML_GetUserData := PPointer(parser)^
end;
{$ENDIF}

{$IFDEF EXPAT_2_0}
{ The exact opposite of what expat.h does :) }
function XML_MAJOR: integer;
begin
  XML_MAJOR := XML_ExpatVersionInfo.major;
end;

function XML_MINOR: integer;
begin
  XML_MINOR := XML_ExpatVersionInfo.minor;
end;

function XML_MICRO: integer;
begin
  XML_MICRO := XML_ExpatVersionInfo.micro;
end;
{$ENDIF}

end.
