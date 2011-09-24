unit RStrings;

{$IFDEF HAS_ANSISTRING}
{$H+}
{$ENDIF}

interface

function StrToPChar(const Str: String): PChar;
function GetPChar(const N: cardinal): PChar;
procedure SetPChar(const N: cardinal; const p: PChar);
function GetPCharIndex(const p: PChar): longint;

var
  RemoveDuplicatePChars: Boolean;

implementation

{$IFNDEF HAS_ANSISTRING}
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
{$ENDIF}

function StrToPChar(const Str: String): PChar;
{$IFDEF HAS_ANSISTRING}
begin
  StrToPChar := PChar(Str);
{$ELSE}
var
  Index: integer;
  p, r: PChar;
begin
  GetMem(r, Length(Str) + 1);
  StrPCopy(r, Str);

  PChars^.Add(r);

  Index := GetPCharIndex(r);
  if (Index <> - 1) and RemoveDuplicatePChars then
  begin
    p := PChars^.GetNth(Index);
    PChars^.Delete(Index);
    FreeMem(p);
  end;

  StrToPChar := r;
{$ENDIF}
end;

function GetPChar(const N: cardinal): PChar;
begin
{$IFDEF HAS_ANSISTRING}
  GetPChar := nil;
{$ELSE}
  GetPChar := PChars^.GetNth(N);
{$ENDIF}
end;

procedure SetPChar(const N: cardinal; const p: PChar);
begin
{$IFNDEF HAS_ANSISTRING}
  PChars^.Delete(N);
  PChars^.Insert(p, N);
{$ENDIF}
end;

function GetPCharIndex(const p: PChar): longint;
{$IFDEF HAS_ANSISTRING}
begin
  GetPCharIndex := -1;
{$ELSE}
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
{$ENDIF}
end;

{$IFNDEF HAS_ANSISTRING}
initialization

InitPChars;
RemoveDuplicatePChars := true;

finalization

FreePChars;
{$ENDIF}

end.
