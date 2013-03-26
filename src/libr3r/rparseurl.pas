unit RParseURL;

interface

type
  TURL = packed record
    Protocol: String;
    User: String;
    Password: String;
    Host: String;
    Port: String;
    Path: String;
    Search: String;
  end;

function ParseURL(URL: String): TURL;

implementation

function ParseURL(URL: String): TURL;
const
  DefaultHost = 'localhost';
var
  ErrPos, Index, Index2: byte;
  Res: TURL;
begin
  Res.Protocol := 'http';
  Res.Port := '80';

  Index := Pos('?', URL);
  if Index <> 0 then
  begin
    Res.Search := Copy(URL, Index + 1, Length(URL) - Index);
    Delete(URL, Index, Length(URL) - Index + 1);
  end;

  Index := Pos('://', URL);
  if Index <> 0 then
  begin
    Res.Protocol := Copy(URL, 1, Index - 1);
    Delete(URL, 1, Index + 2);
  end;

  Index := Pos('@', URL);
  if Index <> 0 then
  begin
    Index2 := Pos(':', URL);
    if (Index2 <> 0) and (Index2 < Index) then
    begin
      Res.User := Copy(URL, 1, Index2 - 1);
      Res.Password := Copy(URL, Index2 + 1, Index - Index2 - 1);
    end
    else
    begin
      Res.User := Copy(URL, 1, Index - 1);
    end;

    Delete(URL, 1, Index);
  end;

  Index := Pos('/', URL);
  if Index <> 0 then
  begin
    Res.Host := Copy(URL, 1, Index - 1);
    Res.Path := Copy(URL, Index, Length(URL) - Index + 1);

    if (Pos('.', Res.Host) = 0) and (Pos(DefaultHost, Res.Host) = 0) then
    begin
      Res.Path := '/' + Res.Host + Res.Path;
      Res.Host := DefaultHost;
    end;

    URL := '';
  end;

  Index := Pos(':', Res.Host);
  if Index <> 0 then
  begin
    Res.Port := Copy(Res.Host, Index + 1, Length(Res.Host) - Index);
    Delete(Res.Host, Index, Length(Res.Host) - Index + 1);

    Val(Res.Port, Index2, ErrPos);
    if ErrPos <> 0 then
    begin
      Res.Protocol := Res.Port;
    end;
  end;

  ParseURL := Res;
end;

end.
