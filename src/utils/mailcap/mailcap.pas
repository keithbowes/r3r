unit MailCap;

interface

uses
  RList;

type
  PMailCap = ^TMailCap;
  TMailCap = object
  private
    FList: PRList;
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
  Dos, Strings;

type
  PMailCapEntry = ^TMailCapEntry;
  TMailCapEntry = record
    MType: String;
    Prog: String;
  end;

function FileExists(const f: String): Boolean;
var
  s: SearchRec;
begin
  FindFirst(f, anyfile, s);
  FileExists := DosError = 0;
  FindClose(s);
end;

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

function Trim(const s: String): String;
const
  WhiteSpace: set of char = [#0, #8, #9, #10, #13, #32];
var
  i, j: cardinal;
  Leading, Trailing: Boolean;
  t: String;
begin
  j := 0;
  Leading := true;
  Trailing := true;

  if Length(s) > 0 then
  begin
    for i := 1 to Length(s) do
    begin
      if (not Trailing) or not (s[i] in WhiteSpace) then
      begin
        Trailing := false;
        Inc(j);
        SetLength(t, j);
        t[j] := s[i];
      end;
    end;

    for i := Length(s) downto 1 do
    begin
      if Leading and not (s[i] in WhiteSpace) then
      begin
        Leading := false
      end
      else if not Leading then
      begin
        Delete(t, i + 1, Length(s) - i - 1);
        Break;
      end;
    end;
  end;

  Trim := t;
end;

function Cap2Doze(const cap: String): String;
begin
  Cap2Doze := StringReplace(cap, '%s', '%1', true);
end;

function Doze2Cap(const doze: String): String;
begin
  Doze2Cap := StringReplace(doze, '%1', '%s', true);
end;

constructor TMailCap.Init;
var
  Com1, Com2, Com3, Com3Part: String;
  Com1End, Com2End, Com3End, i: word;
  f: text;
  mcfile, mcfilelist: String;
  mcfiles: PRList;
  p: PMailCapEntry;
  pc: PChar;
  s: String;
  TestPassed: Boolean;
begin
  New(mcfiles, Init);
  New(FList, Init);
  mcfilelist :=  GetEnv('MAILCAPS');
  if Length(mcfilelist) = 0 then
  begin
    mcfile := GetEnv('HOME') + '/' + '.mailcap';
  end;

  repeat
    com1End := Pos(':', mcfilelist);
    if Com1End > 0 then
    begin
      Com1 := Copy(mcfilelist, 1, Com1End - 1);
      Delete(mcfilelist, 1, Com1End);
      pc := StrAlloc(Length(Com1) + 1);
      StrPCopy(pc, Com1);
      mcfiles^.Add(pc);
    end
  until Com1End = 0;

  if Length(mcfilelist) > 0 then
  begin
    pc := StrAlloc(Length(mcfilelist) + 1);
    StrPCopy(pc, mcfilelist);
    mcfiles^.Add(pc);
  end;

  if mcfiles^.Count > 0 then
  begin
    for i := 0 to mcfiles^.Count - 1 do
    begin
      mcfile := StrPas(mcfiles^.GetNth(i));

      if FileExists(mcfile) then
      begin
        Assign(f, mcfile);
        Reset(f);

        while not Eof(f) do
        begin
          ReadLn(f, s);

          if (Length(s) > 0) and (Pos('#', s) = 0) then
          begin
            TestPassed := true;
            Com1End := Pos(';', s);
            Com1 := Copy(s, 1, Com1End - 1);
            Delete(s, 1, Com1End);
            Com2End := Pos(';', s);
            if Com2End = 0 then
            begin
              Com2End := Length(s) + 1;
            end;
            Com2 := Trim(Copy(s, 1, Com2End - 1));
            StripPost(Com2);

            Com3 := Copy(s, Com2End, Length(s) - Com2End + 1);
            if Length(Com3) > 0 then
            begin
              repeat
                Com2End := 0;
                Com3End := Pos('test', Com3);
                Delete(Com3, 1, Com3End - 1);

                if Com3End <> 0 then
                begin
                  Com2End := Pos(';', Com3);
                  if Com2End = 0 then
                  begin
                    Com2End := Length(Com3) + 1;
                  end;

                  Com3End := 1;
                  Com3Part := Copy(Com3, Com3End, Com2End - 1);
                  Delete(Com3Part, 1, Length('test='));
                  Delete(Com3, Com3End, Com2End);

                  SwapVectors;
                  Exec(FSearch(Com3Part, GetEnv('PATH')), '');
                  SwapVectors;

                  if DosExitCode <> 0 then
                  begin
                    TestPassed := false;
                    Com3End := 0;
                  end;
                end;
              until Com3End = 0;
            end;

            if TestPassed then
            begin
              New(p);
              p^.MType := Com1;
              p^.Prog := Com2;

              FList^.Add(p);
            end;
          end;
        end;

        Close(f);
      end;
    end;
  end;

  if mcfiles^.Count > 0 then
  begin
    for i := 0 to mcfiles^.Count - 1 do
    begin
      pc := mcfiles^.GetNth(i);
      StrDispose(pc);
    end;
  end;

  Dispose(mcfiles, Done);
end;

destructor TMailCap.Done;
var
  i: word;
  p: PMailCapEntry;
begin
  if FList^.Count > 0 then
  begin
    for i := 0 to FList^.Count - 1 do
    begin
      p := PMailCapEntry(FList^.GetNth(i));
      Dispose(p);
    end;
  end;

  Dispose(FList, Done);
end;

function TMailCap.GetProg(const mtype: String): String;
var
  i: word;
begin
  GetProg := '';

  if FList^.Count > 0 then
  begin
    for i := 0 to FList^.Count - 1 do
    begin
      if PMailCapEntry(FList^.GetNth(i))^.MType = mtype then
      begin
        GetProg := PMailCapEntry(FList^.GetNth(i))^.Prog;
        Break;
      end;
    end;
  end;
end;

function TMailCap.ExecProg(const mtype, param: String): byte;
var
  args, prog: String;
  dl, dop: String;
  expstart: byte;
  fl: String;
begin
  dl := GetEnv('DOWNLOADER');
  dop := GetEnv('DOWNLOADEROPTIONS');
  fl := GetEnv('TEMP') + '/dl';

  if Length(dl) = 0 then
  begin
    dl := 'wget';
    dop := '-O ' + fl;
  end;
  Exec(dl, dop + ' ' + param);

  prog := GetProg(mtype);
  StripPost(prog);

  expstart := Pos(' ', prog);
  args := Copy(prog, expstart, Pos(param, prog) + Length(param) - expstart);
  prog := Copy(prog, 1, expstart - 1);
  Exec(FSearch(prog, GetEnv('PATH')), dl);

  ExecProg := DosExitCode;
end;

procedure TMailCap.StripPost(var prog: String);
begin
  Delete(prog, Pos('%s', prog) + 2, Length(prog));
end;

end.
