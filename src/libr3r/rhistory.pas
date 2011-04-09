unit RHistory;

interface

type
  PRHistory = ^TRHistory;
  TRHistory = object
  private
    FFile: text;
    IsWriting: Boolean;
  public
    constructor Init;
    destructor Done;
    procedure Add(const Entry: String);
    function IsNext: Boolean;
    function GetNext: String;
  end;

var
  History: PRHistory;

implementation

uses
  RSettings_Routines, SysUtils;

constructor TRHistory.Init;
begin
  Assign(FFile, DataDir + PathDelim + 'history');
  Reset(FFile);
  IsWriting := false;
end;

destructor TRHistory.Done;
const
  MaxEntries = 100;
type
  TEntries = array [1..MaxEntries] of String;
var
  Entries: TEntries;
  EntryCount, EntryStart: integer;
  i: integer;
  s: String;
begin
  Reset(FFile);

  i := 0;
  while IsNext do
  begin
    Inc(i);
    ReadLn(FFile, s);
    EntryCount := i;
  end;

  if EntryCount > MaxEntries then
  begin
    EntryStart := EntryCount;
    while EntryStart > MaxEntries do
    begin
      Dec(EntryStart, MaxEntries);
    end;
  end
  else
  begin
    EntryStart := 0;
  end;

  Reset(FFile);
  for i := 1 to EntryStart + MaxEntries do
  begin
    if IsNext and (i >= EntryStart) then
    begin
      ReadLn(FFile, s);
      EntryCount := i - EntryStart;
      Entries[EntryCount] := s;
    end
    else
    begin
      Break;
    end;
  end;

  Rewrite(FFile);
  for i := 1 to EntryCount do
  begin
    WriteLn(FFile, Entries[i]);
  end;

  Close(FFile);
end;

procedure TRHistory.Add(const Entry: String);
var
  Already: Boolean;
  s: String;
begin
  Already := false;
  Reset(FFile);
  while IsNext and not Already do
  begin
    ReadLn(FFile, s);
    Already := s = Entry;
  end;

  if not Already then
  begin
    IsWriting := true;
    Append(FFile);

    if Length(Entry) > 0 then
    begin
      WriteLn(FFile, Entry);
    end;
  end;
end;

function TRHistory.IsNext: Boolean;
begin
  if IsWriting then
  begin
    IsWriting := false;
    Reset(FFile);
  end;

  IsNext := not Eof(FFile);
end;

function TRHistory.GetNext: String;
var
  s: String;
begin
  if IsWriting then
  begin
    Reset(FFile);
    IsWriting := false;
  end;

  ReadLn(FFile, s);
  GetNext := s;
end;

initialization
New(History, Init);

finalization
Dispose(History, Done);

end.
