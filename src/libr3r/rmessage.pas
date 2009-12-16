unit RMessage;

interface

type
{$IFDEF __GPC__}
  TObject = class
  end;
{$ENDIF}

  TRMessage = procedure(Sender: TObject; Error: Boolean; MessageName, Extra: String){$IFNDEF __GPC__} of object{$ENDIF};

procedure SetMessageEvent(Event: TRMessage);
procedure CallMessageEvent(Sender: TObject; IsError: Boolean; MessageName: String);
procedure CallMessageEventEx(Sender: TObject; IsError: Boolean; MessageName: String; Extra: String);

implementation

uses
  LibR3R, RSock;

var
  MessageEvent: TRMessage;

procedure SetMessageEvent(Event: TRMessage);
begin
  MessageEvent := Event;
end;

procedure CallMessageEvent(Sender: TObject; IsError: Boolean; MessageName: String);
begin
  CallMessageEventEx(Sender, IsError, MessageName, '');
end;

procedure CallMessageEventEx(Sender: TObject; IsError: Boolean; MessageName: String; Extra: String);
begin
  if Assigned(MessageEvent) and
    Settings.GetBoolean(Settings.IndexOf('show-messages')) then
  begin
    MessageEvent(Sender, IsError, MessageName, Extra);
    TRSock(Sender).ShouldShow := false;
  end;
end;

end.
