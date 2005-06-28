unit RMessage;

interface

type
  TRMessage = procedure(Sender: TObject; Error: Boolean; MessageName: String) of object;

procedure SetMessageEvent(Event: TRMessage);
procedure CallMessageEvent(Sender: TObject; Error: Boolean; MessageName: String);

implementation

var
  MessageEvent: TRMessage;

procedure SetMessageEvent(Event: TRMessage);
begin
  MessageEvent := Event;
end;

procedure CallMessageEvent(Sender: TObject; Error: Boolean; MessageName: String);
begin
  if Assigned(MessageEvent) then
  begin
    MessageEvent(Sender, Error, MessageName);
  end;
end;

end.
