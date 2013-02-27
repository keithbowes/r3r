unit RSubscriptions;

interface

uses
  RList;

type
  PRSubscriptions = ^TRSubscriptions;
  TRSubscriptions = object(TRStringList)
  private
    FFile: String;
    FText: text;
  public
    constructor Init;
    destructor Done;
    procedure DeleteIndex(const Index: word);
  end;

var
  Subscriptions: PRSubscriptions;

implementation

uses
  Dos, LibR3RStrings, RSettings_Routines;

constructor TRSubscriptions.Init;
var
  f: SearchRec;
  Line: String;
begin
  inherited Init;
  FFile := DataDir + 'subscriptions.txt';

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

  if Count = 0 then
  begin
    Add(Subscription1);
    Add(Subscription2);
    Add(Subscription3);
    Add(Subscription4);
  end;

  Close(FText);
end;

destructor TRSubscriptions.Done;
var
  i: word;
begin
  Assign(FText, FFile);
  Rewrite(FText);

  if Count > 0 then
  begin
    for i := 0 to Count - 1 do
    begin
      WriteLn(FText, GetNth(i));
    end;
  end;

  Close(FText);
  inherited Done;
end;

procedure TRSubscriptions.DeleteIndex(const Index: word);
begin
  Delete(Index);
end;

initialization

New(Subscriptions, Init);

finalization

Dispose(Subscriptions, Done);

end.
