unit ColorTable;

interface

type
  TColorTable = record
    FDescBack: byte;
    FDescFore: byte;
    FIndexBack: byte;
    FIndexFore: byte;
    FInfoBack: byte;
    FInfoFore: byte;
    FInfoBarBack: byte;
    FInfoBarFore: byte;
    FSep: byte;
    FStatusBack: byte;
    FStatusFore: byte;
    FTitleBack: byte;
    FTitleFore: byte;
    FUIBack: byte;
    FUIFore: byte;
  end;

var
  AColorTable: TColorTable;

procedure ColorTableInit;

implementation

uses
  DOS, StrTok;

function StrToInt(const s: String): integer;
var
  Err: byte;
  Res: integer;
begin
  Val(s, Res, Err);
  StrToInt := Res;
end;

procedure ColorTableInit;
var
  Table: String;
  TableParts: TStringsList;
begin
  Table := GetEnv('R3R_COLOR_TABLE');

  if Table = '' then
  begin
    {
      0  = black
      1  = blue
      2  = green
      4  = red
      5  = magenta
      6  = brown
      11 = light cyan
      14 = yellow
    }

    with AColorTable do
    begin
      FDescBack := 0;
      FDescFore := 4;
      FIndexBack := 0;
      FIndexFore := 6;
      FInfoBack := 0;
      FInfoFore := 2;
      FInfoBarBack := 1;
      FInfoBarFore := 14;
      FSep := 11;
      FStatusBack := 0;
      FStatusFore := 5;
      FTitleBack := 0;
      FTitleFore := 2;
      FUIBack := 1;
      FUIFore := 14;
    end;
  end
  else
  begin
    TableParts := Split(Table, ',');

    with AColorTable do
    begin
      FDescBack := StrToInt(TableParts.Strings[0]);
      FDescFore := StrToInt(TableParts.Strings[1]);
      FIndexBack := StrToInt(TableParts.Strings[2]);
      FIndexFore := StrToInt(TableParts.Strings[3]);
      FInfoBack := StrToInt(TableParts.Strings[4]);
      FInfoFore := StrToInt(TableParts.Strings[5]);
      FInfoBarBack := StrToInt(TableParts.Strings[6]);
      FInfoBarFore := StrToInt(TableParts.Strings[7]);
      FSep := StrToInt(TableParts.Strings[8]);
      FStatusBack := StrToInt(TableParts.Strings[9]);
      FStatusFore := StrToInt(TableParts.Strings[10]);
      FTitleBack := StrToInt(TableParts.Strings[11]);
      FTitleFore := StrToInt(TableParts.Strings[12]);
      FUIBack := StrToInt(TableParts.Strings[13]);
      FUIFore := StrToInt(TableParts.Strings[14]);
    end;
  end;
end;

end.
