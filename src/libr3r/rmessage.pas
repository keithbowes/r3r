unit RMessage;

interface

uses
  LibR3R, RSock;

{$IFDEF __GPC__}
type
  TObject = Pointer;
{$ENDIF}

procedure SetMessageObject(const Sender: TLibR3R);
procedure CallMessageEvent(Sender: TObject; IsError: Boolean; MessageName: String);
procedure CallMessageEventEx(Sender: TObject; IsError: Boolean; MessageName, Extra: String);

implementation

var
  MessageObject: TLibR3R;

procedure SetMessageObject(const Sender: TLibR3R);
begin
  MessageObject := Sender;
end;

procedure CallMessageEvent(Sender: TObject; IsError: Boolean; MessageName: String);
begin
  CallMessageEventEx(Sender, IsError, MessageName, '');
end;

procedure CallMessageEventEx(Sender: TObject; IsError: Boolean; MessageName, Extra: String);
begin
  if Assigned(MessageObject) and
    Settings.GetBoolean('show-messages') then
  begin
    MessageObject.HandleMessage(IsError, MessageName, Extra);

{$IFNDEF __GPC__}
    if Sender is TRSock then
    begin
      (Sender as TRSock).ShouldShow := false;
    end;
{$ENDIF}
  end;
end;

end.
