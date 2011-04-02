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
  Rewrite(FFile);
end;

destructor TRHistory.Done;
const
  MaxEntries = 100;
type
  TEntries = array [1..MaxEntries] of String;
var
  Entries: TEntries;
  EntryCount: 1..MaxEntries;
  i: 1..MaxEntries;
  s: String;
begin
  Reset(FFile);
  for i := 1 to MaxEntries do
  begin
    if IsNext then
    begin
      ReadLn(FFile, s);
      Entries[i] := s;
      EntryCount := i;
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
begin
  IsWriting := true;
  Append(FFile);
  WriteLn(FFile, Entry);
end;

function TRHistory.IsNext: Boolean;
begin
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
