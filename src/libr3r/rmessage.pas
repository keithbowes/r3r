unit RMessage;

interface

uses
  LibR3R, RSock;

procedure SetMessageObject(const Sender: TLibR3R);
procedure CallMessageEvent(Sender: TObject; IsError: Boolean; MessageName, Extra: String);

implementation

uses
  RSettings_Routines, SysUtils;

var
  MessageObject: TLibR3R;

procedure SetMessageObject(const Sender: TLibR3R);
begin
  MessageObject := Sender;
end;

procedure CallMessageEvent(Sender: TObject; IsError: Boolean; MessageName, Extra: String);
var
  f: text;
  LogFile: String;
  s: String;
begin
  if Assigned(MessageObject) and
    Settings.GetBoolean('show-messages') then
  begin
    MessageObject.HandleMessage(IsError, MessageName, Extra);

    if Sender is TRSock then
    begin
      (Sender as TRSock).ShouldShow := false;
    end;
  end;

  if Settings.GetBoolean('logging') then
  begin
    LogFile := GetCacheDir + 'r3r.log';
    WriteStr(s, '[', Ord(IsError), '] ', MessageName, ' (', Extra, ')');

    Assign(f, LogFile); 

    if not FileExists(LogFile) then
    begin
      Rewrite(f);
    end;

    Append(f);
    WriteLn(f, s);
    Close(f);
  end;
end;

end.
