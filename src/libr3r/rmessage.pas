unit RMessage;

interface

type
  TRMessage = procedure(Sender: TObject; Error: Boolean; MessageName, Extra: String) of object;

procedure SetMessageEvent(Event: TRMessage);
procedure CallMessageEvent(Sender: TObject; IsError: Boolean; MessageName: String; Extra: String = '');

implementation

uses
  LibR3R;

var
  MessageEvent: TRMessage;

procedure SetMessageEvent(Event: TRMessage);
begin
  MessageEvent := Event;
end;

procedure CallMessageEvent(Sender: TObject; IsError: Boolean; MessageName: String; Extra: String = '');
begin
  if Assigned(MessageEvent) and
    Settings.GetBoolean(Settings.IndexOf('show-messages')) then
  begin
    MessageEvent(Sender, IsError, MessageName, Extra);
  end;
end;

end.
