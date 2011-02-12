program R3R;

{$ifdef MSWINDOWS}
  {$r ../../../icons/r3r.res}
{$endif}

uses
  TV;

var
  app: PTVApp;

begin
  New(app, Init);
  app^.Run;
  Dispose(app, Done);
end.
