program r3r;

{$mode objfpc}{$H+}

uses
  {$ifdef UNIX}
    CThreads,
  {$endif}
  Interfaces, // this includes the LCL widgetset
  Forms,
  { add your units here }
  classic;

begin
  Application.Title:='R3R';
  Application.Initialize;
  Application.CreateForm(TR3RForm, R3RForm);
  Application.Run;
end.

