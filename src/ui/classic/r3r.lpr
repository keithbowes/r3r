program r3r;

{$mode objfpc}{$H+}

{$ifdef win32}
  {$r ../../../icons/r3r.rc}
{$endif}

uses
  Interfaces, // this includes the LCL widgetset
  Forms,
  { add your units here }
  classic;

begin
  Application.Initialize;
  Application.CreateForm(TR3RForm, R3RForm);
  Application.Run;
end.

