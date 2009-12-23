unit RMessage;

interface

uses
  LibR3R, RSock;

procedure SetMessageObject(const Sender: TLibR3R);
procedure CallMessageEvent(Sender: TRSock; IsError: Boolean; MessageName: String);
procedure CallMessageEventEx(Sender: TRSock; IsError: Boolean; MessageName: String; Extra: String);

implementation

var
  MessageObject: TLibR3R;

procedure SetMessageObject(const Sender: TLibR3R);
begin
  MessageObject := Sender;
end;

procedure CallMessageEvent(Sender: TRSock; IsError: Boolean; MessageName: String);
begin
  CallMessageEventEx(Sender, IsError, MessageName, '');
end;

procedure CallMessageEventEx(Sender: TRSock; IsError: Boolean; MessageName: String; Extra: String);
begin
  if Assigned(MessageObject) and
    Settings.GetBoolean(Settings.IndexOf('show-messages')) then
  begin
    MessageObject.HandleMessage(IsError, MessageName, Extra);
    Sender.ShouldShow := false;
  end;
end;

end.
