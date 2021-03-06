unit RStrings;

interface

function StrToPChar(const s: String): PChar;
function StrToPCharAlloc(const s: String): PChar;
function GetPChar(const N: PtrUInt): PChar;
procedure SetPChar(const N: PtrUInt; const p: PChar);
function GetPCharIndex(const p: PChar): PtrInt;

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
  i: PtrUInt;
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

function StrToPChar(const s: String): PChar;
begin
  StrToPChar := PChar(s);
end;

function StrToPCharAlloc(const s: String): PChar;
var
  Index: PtrInt;
  r: PChar;
begin
  GetMem(r, Length(s) + 1);
  StrPCopy(r, s);

  if RemoveDuplicatePChars then
  begin
    Index := GetPCharIndex(r);
    if Index < 1 then
    begin
      PChars^.Add(r);
    end
    else
    begin
      Dispose(r);
      r := PChars^.GetNth(Index);
    end;
  end
  else
  begin
    PChars^.Add(r);
  end;

  StrToPCharAlloc := r;
end;

function GetPChar(const N: PtrUInt): PChar;
begin
  GetPChar := PChars^.GetNth(N);
end;

procedure SetPChar(const N: PtrUInt; const p: PChar);
begin
  PChars^.Delete(N);
  PChars^.Insert(p, N);
end;

function GetPCharIndex(const p: PChar): PtrInt;
var
  i, Len: PtrInt;
  Found: Boolean;
begin
  Len := PChars^.Count;
  i := Len - 1;
;
  Found := false;
  while (i >= 0) and not Found do
  begin
    Found := StrComp(p, PChars^.GetNth(i)) = 0;
    if not Found then
    begin
      Dec(i);
    end
  end;

  if Found then
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
