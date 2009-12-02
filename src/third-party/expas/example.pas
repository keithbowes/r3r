{
  Simple example.  Works in FPC, GPC, and probably Delphi.
  One of these days, I'd like for my entire program to.
}

program Example(output);

{$CALLING cdecl}
{$MODE DELPHI}

{$H+}
{$X+}

uses
  Expas;

type
  PInteger = ^integer;
  TString = {$ifdef __GPC__}String{$else}PChar{$endif};

var
  f: text;
  i: integer;
  r: pointer;
  p: XML_Parser;
  s: String;

function PCharToString(p: PChar): String;
begin
{$ifdef __GPC__}
  PCharToString := CString2String(p);
{$else}
  PCharToString := String(p);
{$endif}
end;

procedure com(userData: pointer; name: PChar; attrs: PPChar);
begin
  WriteLn('com -start');
  WriteLn('User data: ', PInteger(userData)^);
  WriteLn('Element name: ', PCharToString(name));

  i := 0;
  while (PCharToString((attrs + i)^) <> '') do
  begin
    WriteLn('Attribute ', i div 2 + 1, ': ', PCharToString((attrs + i)^), '=', PCharToString((attrs + i + 1)^));
    Inc(i, 2);
  end;
  WriteLn('com -end');
end;

procedure fin(userData: pointer; name: PChar);
begin
  WriteLn('fin - start');
  WriteLn('Element ', PCharToString(name), ' finished');
  WriteLn('fin - end');
end;

procedure han(userData: pointer; s: PChar; len: integer);
var
  t: String;
begin
  WriteLn('han - start');
  if len > 0 then
  begin
    t := PCharToString(s);
    WriteLn('Data received - ', Copy(t, 1, len));
  end;
  WriteLn('han -end');
end;

begin
  i := 27;

  if ParamCount < 1 then
  begin
    WriteLn('Usage: example <xml-file>');
    Halt(0);
  end;

  p := XML_ParserCreate('UTF-8');
  XML_SetElementHandler(p, com, fin);
  XML_SetCharacterDataHandler(p, han);
  XML_SetUserData(p, @i);
  WriteLn('Initial user data: ', PInteger(XML_GetUserData(p))^);
  Assign(f, ParamStr(1));
  Reset(f);
  while not Eof(f) do
  begin
    ReadLn(f, String(s));
    XML_Parse(p, TString(s), Length(s), 0);
  end;

  Close(f);
  XML_ParserFree(p);
end.
