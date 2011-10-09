constructor TMailCap.Init;
begin
end;

destructor TMailCap.Done;
begin
end;

function TMailCap.GetProg(const mtype: String): String;
var
  i: word;
  Res: String;
  SubType: String;
begin
  i := Pos('/', mtype);
  SubType := Copy(mtype, i + 1, Length(mtype) - i);

  with TRegistry.Create do
  begin
    RootKey := HKEY_CLASSES_ROOT;
    OpenKey(SubType + '\shell\open\command', true);
    Res := ReadString('');
    CloseKey;
    if Res = '' then
    begin
      SubType := '.' + LowerCase(SubType);
      RootKey :=  HKEY_CLASSES_ROOT;
      OpenKey(SubType + '\shell\open\command', true);
      Res := ReadString('');
      CloseKey;
    end;

    GetProg := Res;
    Free;
  end;
end;

{
vi:ft=pascal
}
