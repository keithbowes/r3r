program R3R;

{$ifdef win32}
  {$r ../../../icons/r3r.res}
{$endif}

uses
  Tui;

begin
  with TTui.Create do
  begin
{$IFNDEF __GPC__}
    Free;
{$ENDIF}
  end;
end.
