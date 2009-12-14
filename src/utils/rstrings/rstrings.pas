unit RStrings;

interface

function StrToPChar(const Str: String): PChar;
function GetPChar(const N: cardinal): PChar;
procedure SetPChar(const N: cardinal; const p: PChar);
function GetPCharIndex(const p: PChar): longint;

var
  RemoveDuplicatePChars: Boolean;

implementation

uses
  RList, SysUtils;

var
  PChars: PRList;

procedure InitPChars;
begin
  New(PChars, Init);
end;

procedure FreePChars;
var
  i: cardinal;
begin
  if PChars^.Count > 0 then
  begin
    for i := 0 to PChars^.Count - 1 do
    begin
      FreeMem(PChars^.GetNth(i));
    end;
  end;

  Dispose(PChars, Done);
end;

function StrToPChar(const Str: String): PChar;
var
  Index: integer;
  p: PChar;
begin
  GetMem(p, Length(Str) + 1);
  StrPCopy(p, Str);

  PChars^.Add(p);

  Index := GetPCharIndex(p);
  if (Index <> - 1) and RemoveDuplicatePChars then
  begin
    PChars^.Delete(Index);
    FreeMem(PChars^.GetNth(Index));
  end;

  StrToPChar := p;
end;

function GetPChar(const N: cardinal): PChar;
begin
  GetPChar := PChars^.GetNth(N);
end;

procedure SetPChar(const N: cardinal; const p: PChar);
begin
  PChars^.Delete(N);
  PChars^.Insert(p, N);
end;

function GetPCharIndex(const p: PChar): longint;
var
  i, Len: longint;
begin
  i := 0;
  Len := PChars^.Count;
;
  while (i < Len) and (StrComp(p, PChars^.GetNth(i)) <> 0) do
  begin
    Inc(i);
  end;

  if i + 1 <> Len then
  begin
    GetPCharIndex := i;
  end
  else
  begin
    GetPCharIndex := -1;
  end;
end;

initialization

InitPChars;
RemoveDuplicatePChars := true;

finalization

FreePChars;

end.
