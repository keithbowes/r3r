unit RMessage;

interface

{$IFDEF __GPC__}
type
  TObject = class
  end;
{$ENDIF}

procedure SetMessageObject(const Sender: TObject);
procedure CallMessageEvent(Sender: TObject; IsError: Boolean; MessageName: String);
procedure CallMessageEventEx(Sender: TObject; IsError: Boolean; MessageName: String; Extra: String);

implementation

uses
  LibR3R, RSock;

var
  MessageObject: TLibR3R;

procedure SetMessageObject(const Sender: TObject);
begin
  MessageObject := TLibR3R(Sender);
end;

procedure CallMessageEvent(Sender: TObject; IsError: Boolean; MessageName: String);
begin
  CallMessageEventEx(Sender, IsError, MessageName, '');
end;

procedure CallMessageEventEx(Sender: TObject; IsError: Boolean; MessageName: String; Extra: String);
begin
  if Assigned(MessageObject) and
    Settings.GetBoolean(Settings.IndexOf('show-messages')) then
  begin
    MessageObject.HandleMessage(Sender, IsError, MessageName, Extra);
    TRSock(Sender).ShouldShow := false;
  end;
end;

end.
