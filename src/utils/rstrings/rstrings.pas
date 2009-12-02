unit RStrings;

interface

function StrToPChar(const Str: String): PChar;

implementation

uses
  SysUtils;

type
  TPChars = array of PChar;

var
  PChars: TPChars;

procedure InitPChars;
begin
  SetLength(PChars, 1);
end;

procedure FreePChars;
var
  i, l: cardinal;
begin
  l := Length(PChars);

  for i := 0 to l - 1 do
  begin
    StrDispose(PChars[i]);
  end;

  SetLength(PChars, 0);
  PChars := nil;
end;

function StrToPChar(const Str: String): PChar;
var
  Len: cardinal;
begin
  Len := Length(PChars);

  Result := StrAlloc(Length(Str) + 1);
  StrPCopy(Result, Str);

  SetLength(PChars, Len + 1);
  PChars[Len] := Result;
end;

initialization

InitPChars;

finalization

FreePChars;

end.
