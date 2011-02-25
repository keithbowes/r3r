unit Skin;

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
    FOptionIndexBack: byte;
    FOptionIndexFore: byte;
    FOptionDescBack: byte;
    FOptionDescFore: byte;
    FOptionPromptBack: byte;
    FOptionPromptFore: byte;
    FOptionValueBack: byte;
    FOptionValueFore: byte;
    FSep: byte;
    FStatusBack: byte;
    FStatusFore: byte;
    FTitleBack: byte;
    FTitleFore: byte;
    FUIBack: byte;
    FUIFore: byte;
  end;

  TPageType = (ptNone, ptMain, ptOptions);

var
  SkinColorTable: TColorTable;
  SkinOptionFull: Boolean;

procedure ColorTableInit;

implementation

uses
  DOS, Info, RSettings, RSettings_Routines, StrTok, SysUtils,
  TuiStrings;

procedure ColorTableInit;
var
  f: text;
  IsEmpty: Boolean;
  IsValidSkinFile: Boolean;
  Line: String;
  LineNo: word;
  Screen: TPageType;
  SkinFile: String;
  TableParts: TStringsList;
  TableSubparts: TStringsList;

procedure AddSkinColor(var BElem, FElem: byte; const Table: String);
var
  Back, Fore: byte;
begin
  TableParts := Split(Table, ',');

  if TableParts.Length = 2 then
  begin
    Back := StrToInt(TableParts.Strings[0]);
    Fore := StrToInt(TableParts.Strings[1]);
  end;

  if Back <> Fore then
  begin
    BElem := Back;
    FElem := Fore;
  end
  else { white on black }
  begin
    BElem := 0;
    FElem := 15;
  end;
end;

function GetSkinFrom(const From: String): Boolean;
var
  Orig: String;
  Res: Boolean;
begin
  Orig := SkinFile;
  SkinFile := From + PathDelim + 'skins' + PathDelim + SkinFile;
  if Pos('.skin', SkinFile) = 0 then
  begin
    SkinFile := SkinFile + '.skin';
  end;

  Res := FileExists(SkinFile);
  GetSkinFrom := Res;

  if not Res then
  begin
    SkinFile := Orig;
  end;
end;

begin
  Settings.RegisterString('skin', 'Display', 'default', SkinToUse);
  SkinFile := Settings.GetString(Settings.IndexOf('skin'));

  if not FileExists(SkinFile) then
  begin
    if not GetSkinFrom(GetInstalledPrefix + PathDelim + 'share' + PathDelim + LowerCase(AppName)) then
    begin
      GetSkinFrom(DataDir);
    end;
  end;

  IsValidSkinFile := false;
  LineNo := 0;
  Screen := ptNone;

  if FileExists(SkinFile) then
  begin
    Assign(f, SkinFile);
    Reset(f);
    while not EOF(f) do
    begin
      ReadLn(f, Line);
      IsEmpty := (Length(Line) = 0) or (Copy(Line, 1, 1) = '#');
      if not IsEmpty then
      begin
        if Copy(Line, 1, 1) = ':' then
        begin
          case Copy(Line, 2, 1)[1] of
            'm':
            begin
              IsValidSkinFile := true;
              Screen := ptMain;
            end;
            'o':
            begin
              IsValidSkinFile := true;
              Screen := ptOptions;
              SkinOptionFull := Copy(Line, 3, 1) <> '0';
            end;
          end;
          Continue;
        end
      end
      else
      begin
        Continue;
      end;

      case Screen of
        ptMain:
        begin
          Inc(LineNo);

          case LineNo of
            1:
            begin
              AddSkinColor(SkinColorTable.FUIBack, SkinColorTable.FUIFore, Line);
            end;
            2:
            begin
              TableSubparts := Split(Line, ';');
              AddSkinColor(SKinColorTable.FIndexBack, SkinColorTable.FIndexFore, TableSubparts.Strings[0]);
              AddSkinColor(SkinColorTable.FTitleBack, SkinColorTable.FTitleFore, TableSubparts.Strings[1]);
            end;
            3:
            begin
              SkinColorTable.FSep := StrToInt(Line);
            end;
            4:
            begin
              AddSkinColor(SkinColorTable.FInfoBack, SkinColorTable.FInfoFore, Line);
            end;
            5:
            begin
              AddSkinColor(SkinColorTable.FDescBack, SkinColorTable.FDescFore, Line);
            end;
            6:
            begin
              AddSkinColor(SkinColorTable.FInfoBarBack, SkinColorTable.FInfoBarFore, Line);
            end;
            7:
            begin
              AddSkinColor(SkinColorTable.FStatusBack, SkinColorTable.FStatusFore, Line);
            end;
          end;
        end;
        ptOptions:
        begin
          TableSubparts := Split(Line, ';');
          AddSkinColor(SkinColorTable.FOptionIndexBack, SkinColorTable.FOptionIndexFore, TableSubparts.Strings[0]);
          AddSkinColor(SkinColorTable.FOptionDescBack, SkinColorTable.FOptionDescFore, TableSubparts.Strings[1]);
          AddSkinColor(SkinColorTable.FOptionValueBack, SkinColorTable.FOptionValueFore, TableSubparts.Strings[2]);
          AddSkinColor(SkinColorTable.FOptionPromptBack, SkinColorTable.FOptionPromptFore, TableSubparts.Strings[3]);
        end;
      end;
    end;
  end;
  
  if (not FileExists(SkinFile)) or (not IsValidSkinFile) then
  begin
  { If we can't parse a skin file, make the foregrounds white;
    so that we don't have black on black. }
  { Needed because FPC doesn't support the value keyword for records }
    with SkinColorTable do   
    begin
      FDescFore := 15;
      FIndexFore := 15;
      FInfoFore := 15;
      FInfoBarFore := 15;
      FOptionIndexFore := 15;
      FOptionDescFore := 15;
      FOptionPromptFore := 15;
      FOptionValueFore := 15;
      FStatusFore := 15;
      FTitleFore := 15;
      FUIFore := 15;
    end;
  end;
end;

end.
