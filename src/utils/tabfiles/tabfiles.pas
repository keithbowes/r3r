unit TabFiles;

interface

type
  TTab = record
    Name: String;
    Value: String;
    Section: String;
  end;

  PTabFile = ^TTabFile;
  TTabFile = object
  private
    FAmWriting: Boolean;
    FFile: text;
    FFileName: String;
    FNewFile: Boolean;
    function ParseTab(Line: String): TTab;
    procedure UpdateTabs(const Section, Ident: String; const Keep: byte);
  public
    constructor Init(const FileName: String);
    destructor Done;
    function ReadBool(const Section, Ident: String; const Default: Boolean): Boolean;
    function ReadFloat(const Section, Ident: String; const Default: real): real;
    function ReadInteger(const Section, Ident: String; const Default: integer): integer;
    function ReadString(const Section, Ident, Default: String): String;
    procedure WriteBool(const Section, Ident: String; const Val: Boolean);
    procedure WriteFloat(const Section, Ident: String; const Val: real);
    procedure WriteInteger(const Section, Ident: String; const Val: integer);
    procedure WriteString(const Section, Ident: String; Val: String);
    function ValueExists(const Section, Ident: String): Boolean;
    procedure EraseSection(const Section: String);
    procedure DeleteKey(const Section, Ident: String);
    procedure UpdateFile;
    procedure StartWriting;
    procedure EndWriting;
  end;

implementation

uses
  DOS;

const
  TabChar = #9;
  TabRepl = #130;

  KeepNeither = 0;
  KeepName = 1;
  KeepSection = 2;
  KeepBoth = 3;
  KeepEither = 4;
  KeepAll = 5;

constructor TTabFile.Init(const FileName: String);
var
  Rec: SearchRec;
begin
  FFileName := FileName;

  Assign(FFile, FFileName);
  FindFirst(FileName, AnyFile, Rec);

  FNewFile := DosError <> 0;
  if FNewFile then
  begin
    Rewrite(FFile);
  end;

  FindClose(Rec);
end;

destructor TTabFile.Done;
begin
  Close(FFile);
end;

function TTabFile.ReadBool(const Section, Ident: String; const Default: Boolean): Boolean;
begin
  ReadBool := ReadInteger(Section, Ident, Ord(Default)) <> 0;
end;

function TTabFile.ReadFloat(const Section, Ident: String; const Default: real): real;
var
  ErrPos: byte;
  fValue: real;
  Line: String;
  Tab: TTab;
  ValueFound: Boolean;
begin
  ValueFound := false;
  Reset(FFile);

  while not EOF(FFile) and not ValueFound do
  begin
    ReadLn(FFile, Line);
    Tab := ParseTab(Line);

    if (Tab.Name = Ident) and (Tab.Section = Section) then
    begin
      Val(Tab.Value, fValue, ErrPos);

      if ErrPos = 0 then
      begin
        ReadFloat := fValue;
        ValueFound := true;
      end;
    end;
  end;

  if not ValueFound then
  begin
    ReadFloat := Default;
  end;
end;

function TTabFile.ReadInteger(const Section, Ident: String; const Default: integer): integer;
begin
  ReadInteger := Trunc(ReadFloat(Section, Ident, Default));
end;

function TTabFile.ReadString(const Section, Ident, Default: String): String;
var
  Line: String;
  Tab: TTab;
  ValueFound: Boolean;

function GetTrueValue: String;
var
  PostTab, PreTab: String;
  Val: String;

function GetIndex: byte;
begin
  GetIndex := Pos(TabRepl, Val);
end;

begin
  Val := Tab.Value;

  while GetIndex <> 0 do
  begin
    PreTab := Copy(Val, 1, GetIndex - 1);
    PostTab := Copy(Val, GetIndex + 1, Length(Val) - GetIndex);
    Val := PreTab + TabChar + PostTab;
  end;

  GetTrueValue := Val;
end;

begin
  ValueFound := false;
  Reset(FFile);

  while not EOF(FFile) and not ValueFound do
  begin
    ReadLn(FFile, Line);

    Tab := ParseTab(Line);
    if (Tab.Name = Ident) and (Tab.Section = Section) then
    begin
      ReadString := GetTrueValue;
      ValueFound := true;
    end;
  end;

  if not ValueFound then
  begin
    ReadString := Default;
  end;
end;

procedure TTabFile.WriteBool(const Section, Ident: String; const Val: Boolean);
begin
  WriteInteger(Section, Ident, Ord(Val));
end;

procedure TTabFile.WriteFloat(const Section, Ident: String; const Val: real);
begin
  if not FAmWriting then
  begin
    UpdateTabs(Section, Ident, KeepEither);
  end;

  WriteLn(FFile, 'section:', Section, TabChar, 'name:', Ident, TabChar, 'value:', Val);
end;

procedure TTabFile.WriteInteger(const Section, Ident: String; const Val: integer);
begin
  if not FAmWriting then
  begin
    UpdateTabs(Section, Ident, KeepEither);
  end;

  WriteLn(FFile, 'section:', Section, TabChar, 'name:', Ident, TabChar, 'value:', Val);
end;

procedure TTabFile.WriteString(const Section, Ident: String; Val: String);
var
  PreTab, PostTab: String;

function GetIndex: byte;
begin
  GetIndex := Pos(TabChar, Val);
end;

begin
  if not FAmWriting then
  begin
    UpdateTabs(Section, Ident, KeepEither);
  end;

  while GetIndex <> 0 do
  begin
    PreTab := Copy(Val, 1, GetIndex - 1);
    PostTab := Copy(Val, GetIndex + 1, Length(Val) - GetIndex);

    Val := PreTab + TabRepl + PostTab;
  end;

  WriteLn(FFile, 'section:', Section, TabChar, 'name:', Ident, TabChar, 'value:', Val);
end;

function TTabFile.ValueExists(const Section, Ident: String): Boolean;
var
  Line: String;
  Tab: TTab;
  ValueFound: Boolean;
begin
  ValueFound := false;
  Reset(FFile);

  while not EOF(FFile) and not ValueFound do
  begin
    ReadLn(FFile, Line);

    Tab := ParseTab(Line);
    if (Tab.Name = Ident) and (Tab.Section = Section) then
    begin
      ValueFound := true;
    end;
  end;

  ValueExists := ValueFound;
end;

procedure TTabFile.EraseSection(const Section: String);
begin
  UpdateTabs(Section, '', KeepName);
end;

procedure TTabFile.DeleteKey(const Section, Ident: String);
begin
  UpdateTabs(Section, Ident, KeepNeither);
end;

procedure TTabFile.UpdateFile;
begin
  UpdateTabs('', '', KeepAll);
end;

procedure TTabFile.StartWriting;
begin
  FAmWriting := true;
  Rewrite(FFile);
end;

procedure TTabFile.EndWriting;
begin
  FAmWriting := false;
end;

function TTabFile.ParseTab(Line: String): TTab;
var
  LinePart, Qual, Quan: String;
  Sep, TabPos: byte;
  Tab: TTab;
begin
  repeat
    TabPos := Pos(TabChar, Line);

    if TabPos <> 0 then
    begin
      LinePart := Copy(Line, 1, TabPos - 1);
    end
    else
    begin
      LinePart := Line;
    end;

    Sep := Pos(':', LinePart);
    Qual := Copy(LinePart, 1, Sep - 1);
    Quan := Copy(LinePart, Sep + 1, Length(LinePart) - Sep + 1);

    if Qual = 'name' then
    begin
      Tab.Name := Quan;
    end
    else if Qual = 'section' then
    begin
      Tab.Section := Quan;
    end
    else if Qual = 'value' then
    begin
      Tab.Value := Quan;
    end;

    Delete(Line, 1, TabPos);
  until TabPos = 0;

  ParseTab := Tab;
end;

procedure TTabFile.UpdateTabs(const Section, Ident: String; const Keep: byte);
var
  f: text;
  First: Boolean;
  Line: String;
  Tab: TTab;

function IsKept: Boolean;
begin
  case Keep of
    KeepAll:
    begin
      IsKept := true;
    end;
    KeepBoth:
    begin
      IsKept := (Tab.Name <> Ident) and (Tab.Section <> Section);
    end;
    KeepEither:
    begin
      IsKept := (Tab.Name <> Ident) or (Tab.Section <> Section);
    end;
    KeepName:
    begin
      IsKept := Tab.Section <> Section;
    end;
    KeepNeither:
    begin
      Result := not ((Tab.Name = Ident) and (Tab.Section = Section));
    end;
    KeepSection:
    begin
      IsKept := Tab.Name <> Ident;
    end;
  end;
end;

begin
  First := true;
  Reset(FFile);

  Assign(f, FFileName);

  while not EOF(FFile) do
  begin
    ReadLn(FFile, Line);

    Tab := ParseTab(Line);
    if IsKept then
    begin
      if not First then
      begin
        Append(f);
      end
      else
      begin
        Rewrite(f);
        First := false;
      end;

      WriteLn(f, Line);
    end;
  end;

  if not FNewFile then
  begin
    Close(f);
  end;

  FNewFile := false;
  Append(FFile);
end;

end.
