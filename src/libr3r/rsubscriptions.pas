unit RSubscriptions;

interface

uses
  RList;

type
  PRSubscriptions = ^TRSubscriptions;
  TRSubscriptions = object
  private
    FFile: String;
    FList: PRList;
    FText: text;
  public
    constructor Init;
    destructor Done;
    procedure Add(const Sub: String);
    procedure DeleteIndex(const Index: word);
    procedure DeleteString(const Sub: String);
    function Get(const N: word): String;
    function Count: word;
  end;

var
  Subscriptions: PRSubscriptions;

implementation

uses
  Dos, RSettings_Routines, RStrings;

constructor TRSubscriptions.Init;
var
  f: SearchRec;
  Line: String;
begin
  FFile := SettingsDir + 'subscriptions.txt';
  New(FList, Init);

  Assign(FText, FFile);

  FindFirst(FFile, AnyFile, f);
  if DosError <> 0 then
  begin
    Rewrite(FText);
  end;
  FindClose(f);

  Reset(FText);

  while not Eof(FText) do
  begin
    ReadLn(FText, Line);
    Add(Line);
  end;

  Close(FText);
end;

destructor TRSubscriptions.Done;
var
  i: word;
begin
  Assign(FText, FFile);
  Rewrite(FText);

  if FList^.Count > 0 then
  begin
    for i := 0 to FList^.Count - 1 do
    begin
      WriteLn(FText, Get(i));
    end;
  end;

  Dispose(FList, Done);

  Close(FText);
end;

procedure TRSubscriptions.Add(const Sub: String);
begin
  FList^.Add(StrToPChar(Sub));
end;

procedure TRSubscriptions.DeleteIndex(const Index: word);
begin
  FList^.Delete(Index);
end;

procedure TRSubscriptions.DeleteString(const Sub: String);
begin
  FList^.DeleteObject(StrToPChar(Sub));
end;

function TRSubscriptions.Get(const N: word): String;
begin
  if Count > 0 then
  begin
    Result := StrPas(FList^.GetNth(N));
  end;
end;

function TRSubscriptions.Count: word;
begin
  Result := FList^.Count;
end;

initialization

New(Subscriptions, Init);

finalization

Dispose(Subscriptions, Done);

end.
