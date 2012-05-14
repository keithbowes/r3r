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
      GetMem(pc, Length(Com1) + 1);
      StrPCopy(pc, Com1);
      mcfiles^.Add(pc);
    end
  until Com1End = 0;

  if Length(mcfilelist) > 0 then
  begin
    GetMem(pc, Length(mcfilelist) + 1);
    StrPCopy(pc, mcfilelist);
    mcfiles^.Add(pc);
  end;

  if mcfiles^.Count > 0 then
  begin
    for i := 0 to mcfiles^.Count - 1 do
    begin
      WriteStr(mcfile, PChar(mcfiles^.GetNth(i)));

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
      FreeMem(pc);
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
  MSubType, MSuperType: String;
  Res: String;
  Reserv: String;
  SubType, SuperType: String;

procedure ParseMType(const pbl: String; var SuperType, SubType: String);
var
  pst: word;
begin
  pst := Pos('/', pbl);
  SubType := Copy(pbl, pst + 1, Length(mtype) - i);
  SuperType := Copy(pbl, 1, pst - 1);
end;

begin
  Res := '';
  Reserv := '';
  ParseMType(mtype, SuperType, SubType);

  if FList^.Count > 0 then
  begin
    for i := 0 to FList^.Count - 1 do
    begin
      if PMailCapEntry(FList^.GetNth(i))^.MType = mtype then
      begin
        Res := PMailCapEntry(FList^.GetNth(i))^.Prog;
        Break;
      end;
      
      if Reserv = '' then
      begin
        ParseMType(PMailCapEntry(FList^.GetNth(i))^.MType, MSuperType, MSubType);
        if (SuperType = MSuperType) and (MSubType = '*') then
        begin
          Reserv := PMailCapEntry(FList^.GetNth(i))^.Prog;
        end;
      end;
    end;
  end;

  if Res = '' then
  begin
    Res := Reserv;
  end;

  GetProg := Res;
end;

{
vi:ft=pascal
}
