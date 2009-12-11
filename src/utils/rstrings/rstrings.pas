unit RStrings;

interface

function StrToPChar(const Str: String): PChar;
function StrToPChar2(const Str: String): PChar;
function GetPChar(const N: cardinal): PChar;
procedure SetPChar(const N: cardinal; const p: PChar);
function GetPCharIndex(const p: PChar): longint;

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

function StrToPChar2(const Str: String): PChar;
begin
  Result := StrToPChar(Str);

  if GetPCharIndex(Result) <> -1 then
  begin
    SetLength(PChars, Length(PChars) - 1);
    Result := GetPChar(Length(PChars) - 1);
  end;
end;

function GetPChar(const N: cardinal): PChar;
begin
  Result := PChars[N];
end;

procedure SetPChar(const N: cardinal; const p: PChar);
begin
  PChars[N] := p;
end;

function GetPCharIndex(const p: PChar): longint;
var
  i, Len: longint;
begin
  i := 0;
  Len := Length(PChars);

  while (i < Len) and (StrComp(p, PChars[i]) <> 0) do
  begin
    Inc(i);
  end;

  Result := i;

  if i = Len - 1 then
  begin
    Result := -1;
  end;
end;

initialization

InitPChars;

finalization

FreePChars;

end.
