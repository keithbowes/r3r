program R3R;

{$ifdef win32}
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
