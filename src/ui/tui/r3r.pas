program R3R;

{$ifdef MSWINDOWS}
  {$r ../../../icons/r3r.res}
{$endif}

uses
  Tui;

begin
  with TTui.Create do
  begin
    Free;
  end;
end.
