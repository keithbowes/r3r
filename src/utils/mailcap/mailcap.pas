unit MailCap;

interface

{$IFNDEF MSWINDOWS}
uses
  RList;
{$ENDIF}

type
  PMailCap = ^TMailCap;
  TMailCap = object
  private
{$IFNDEF MSWINDOWS}
    FList: PRList;
{$ENDIF}
    procedure StripPost(var prog: String);
  public
    constructor Init;
    destructor Done;
    function GetProg(const mtype: String): String;
    function ExecProg(const mtype, param: String): byte;
  end;

function Cap2Doze(const cap: String): String;
function Doze2Cap(const doze: String): String;

implementation

uses
  Dos, Strings
{$IFDEF MSWINDOWS}
  , Registry
{$ENDIF};

function StringReplace(s: String; const _in, repl: String; const Global: Boolean): String;
var
  l: word;
begin
  repeat
    l := Pos(_in, s);
    if l <> 0 then
    begin
      Delete(s, l, Length(_in));
      Insert(repl, s, l);
    end;
  until not Global or (l = 0);
  StringReplace := s;
end;

{$IFNDEF MSWINDOWS}
{$INCLUDE "mailcapdriver.inc"}
{$ELSE}
{$INCLUDE "winregdriver.inc"}
{$ENDIF}

function Cap2Doze(const cap: String): String;
begin
  Cap2Doze := StringReplace(cap, '%s', '%1', true);
end;

function Doze2Cap(const doze: String): String;
begin
  Doze2Cap := StringReplace(doze, '%1', '%s', true);
end;


function TMailCap.ExecProg(const mtype, param: String): byte;
var
  dl, dop: String;
  expstart: byte;
  fl: String;
  prog: String;
begin
  dl := GetEnv('MAILCAP_DOWNLOADER');
  dop := GetEnv('MAILCAP_DOWNLOADER_OPTIONS');
  fl := GetEnv('TMP') + '/dl';

  if Length(dl) = 0 then
  begin
    dl := 'wget';
    dop := '-O ' + fl;
  end;
  Exec(dl, dop + ' ' + param);

  prog := GetProg(mtype);
  StripPost(prog);

  expstart := Pos(' ', prog);
  prog := Copy(prog, 1, expstart - 1);
  Exec(FSearch(prog, GetEnv('PATH')), dl);

  ExecProg := DosExitCode;
end;

procedure TMailCap.StripPost(var prog: String);
begin
  Delete(prog, Pos('%s', prog) + 2, Length(prog));
end;

end.
