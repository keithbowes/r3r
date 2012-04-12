unit RParseURL;

interface

function ParseURL(URL: String; var Protocol, User, Password, Host, Port, Path, Search: String): String;

implementation

function ParseURL(URL: String; var Protocol, User, Password, Host, Port, Path, Search: String): String;
const
  DefaultHost = 'localhost';
var
  ErrPos, Index, Index2: byte;
  Res: String;
begin
  Protocol := 'http';
  Port := '80';

  Index := Pos('?', URL);
  if Index <> 0 then
  begin
    Search := Copy(URL, Index + 1, Length(URL) - Index);
    Delete(URL, Index, Length(URL) - Index + 1);
  end;

  Index := Pos('://', URL);
  if Index <> 0 then
  begin
    Protocol := Copy(URL, 1, Index - 1);
    Delete(URL, 1, Index + 2);
  end;

  Index := Pos('@', URL);
  if Index <> 0 then
  begin
    Index2 := Pos(':', URL);
    if (Index2 <> 0) and (Index2 < Index) then
    begin
      User := Copy(URL, 1, Index2 - 1);
      Password := Copy(URL, Index2 + 1, Index - Index2 - 1);
    end
    else
    begin
      User := Copy(URL, 1, Index - 1);
    end;

    Delete(URL, 1, Index);
  end;

  Index := Pos('/', URL);
  if Index <> 0 then
  begin
    Host := Copy(URL, 1, Index - 1);
    Path := Copy(URL, Index, Length(URL) - Index + 1);

    if (Pos('.', Host) = 0) and (Host <> DefaultHost) then
    begin
      Path := '/' + Host + Path;
      Host := DefaultHost;
    end;

    URL := '';
  end;

  Index := Pos(':', Host);
  if Index <> 0 then
  begin
    Port := Copy(Host, Index + 1, Length(Host) - Index);
    Delete(Host, Index, Length(Host) - Index + 1);

    Val(Port, Index2, ErrPos);
    if ErrPos <> 0 then
    begin
      Protocol := Port;
    end;
  end;
  
  Res := Path;
  if Search <> '' then
  begin
    Res := Res + '?' + Search;
  end;

  ParseURL := Res;
end;

end.
