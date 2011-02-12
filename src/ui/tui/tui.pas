unit Tui;

interface

uses
  LibR3R, RList;

type
  TViewport = record
    FirstItem, LastItem: word;
    PortHeight: word;
  end;

  TWindowDim = record
    LeftEnd, TopEnd: word;
    LeftStart, TopStart: word;
  end;

  TTui = class(TLibR3R)
  private
    FCurrentItem: cardinal;
    FDimInfo: TWindowDim;
    FDimInfoBar: TWindowDim;
    FDimList: TWindowDim;
    FDimSep: TWindowDim;
    FDimStatus: TWindowDim;
    FDimUI: TWindowDim;
    FItems: PRList;
    FScreenHeight: word;
    FScreenWidth: word;
    FTitleOriginal: String;
    FViewPort: TViewport;
    procedure Draw;
    procedure Redraw;
    procedure RedrawConditional;
    procedure DrawFeedInfo;
    procedure DrawFeedList;
    procedure DrawInfoBar;
    procedure DrawSeparator;
    procedure DrawStatus;
    procedure DrawUIString;
    function GetOriginalTitle: String;
    procedure InitWindowDims;
    procedure SetNewTitle(const NewTitle: String);
  protected
    procedure NotifyUpdate; override;
    procedure ShowHelp;
    procedure GoURI;
    procedure QueryItemNumber;
    procedure GoItem;
    procedure OpenBrowser(Link: String);
    procedure SetOptions;
    procedure GoDonate;
    procedure ScrollDown;
    procedure ScrollUp;
    procedure ScrollTo(n: word);
  public
    constructor Create; {$IFDEF __GPC__}override;{$ENDIF}
    destructor Destroy; override;
    procedure DisplayItem(const Item: TFeedItem); override;
    procedure HandleMessage(IsError: Boolean; MessageName, Extra: String); override;
    procedure RetrieveFeed(Resource: String); override;
  end;

implementation

uses
  ColorTable, Crt, Dos, Info, RSettings_Routines, SysUtils, TuiStrings
{$IFDEF MSWINDOWS}
  , Windows
{$ENDIF};

{$IFNDEF HAS_SCREENHEIGHTWIDTH}
function ScreenHeight: word;
begin
{$IFDEF __GPC__}
  ScreenHeight := ScreenSize.Y;
{$ELSE}
  ScreenHeight := 25;
{$ENDIF __GPC__}
end;

function ScreenWidth: word;
begin
{$IFDEF __GPC__}
  ScreenWidth := ScreenSize.X;
{$ELSE}
  ScreenWidth := 80;
{$ENDIF __GPC__}
end;
{$ENDIF HAS_SCREENHEIGHTWIDTH}

procedure FullScreen;
begin
  Window(1, 1, ScreenWidth, ScreenHeight);
end;

function CreateFeedItem: TFeedItem;
begin
  CreateFeedItem := TFeedItem.Create;
end;

constructor TTui.Create;
const
  DownKey = 'j';
  EndKey = Chr(79);
  EnterKey = Chr(13);
  HomeEndKey = '~'; // For some reason, FPC reports both HOME and END as a tilde
  HomeKey = Chr(71);
  NullKey = Chr(0);
  PageDownKey = Chr(81);
  PageUpKey = Chr(73);
  RefreshKey = '-';
  ScrollDownKey = Chr(32);
  ScrollUpKey = Chr(8);
  SearchKey = '/';
  ShellKey = '!';
  UpKey = 'k';
var
  FeedIndex: word;
  KeyChar: char;
begin
  inherited Create;
  New(FItems, Init);
  FCurrentItem := 1;
  FTitleOriginal := GetOriginalTitle;

{$IFDEF __GPC__}
  CRTInit;
{$ENDIF __GPC__}

  FViewPort.FirstItem := FCurrentItem;
  FViewPort.LastItem := ScreenHeight - 5;
  FViewPort.PortHeight := FViewPort.LastItem;
  SetNewTitle(AppName);
  ColorTableInit;
  InitWindowDims;

  Draw;

  for FeedIndex := 1 to ParamCount do
  begin
    RetrieveFeed(ParamStr(FeedIndex));
  end;

  if Settings.GetBoolean(Settings.IndexOf('load-subscriptions-on-startup')) then
  begin
    if Subscriptions^.Count > 0 then
    begin
      for FeedIndex := 0 to Subscriptions^.Count - 1 do
      begin
        RetrieveFeed(Subscriptions^.GetNth(FeedIndex));
      end;
    end;
  end;

  repeat
    KeyChar := ReadKey;

    { Read the scan code of control characters }
    if KeyChar = NullKey then
    begin
      KeyChar := ReadKey;
    end;

    if KeyChar = GoKey then
    begin
      GoURI;
    end
    else if KeyChar = OptionsKey then
    begin
      SetOptions;
    end
    else if KeyChar = SearchKey then
    begin
      QueryItemNumber;
      ScrollTo(FCurrentItem);
    end
    else if KeyChar = DonateKey then
    begin
      GoDonate;
    end
    else if KeyChar = DownKey then
    begin
      if FCurrentItem < FItems^.Count then
      begin
        Inc(FCurrentItem);
      end
      else
      begin
        FCurrentItem := 1;
      end;

      ScrollTo(FCurrentItem);
    end
    else if KeyChar = UpKey then
    begin
      if FCurrentItem > 1 then
      begin
        Dec(FCurrentItem);
      end
      else
      begin
        FCurrentItem := FItems^.Count;
      end;

      ScrollTo(FCurrentItem);
    end
    else if KeyChar = RefreshKey then
    begin
      Redraw;
    end
    else if (KeyChar = ScrollDownKey) or (KeyChar = PageDownKey) then
    begin
      ScrollDown;
    end
    else if (KeyChar = ScrollUpKey) or (KeyChar = PageUpKey) then
    begin
      ScrollUp;
    end
    else if KeyChar = HomeKey then
    begin
      ScrollTo(1);
    end
    else if KeyChar = EndKey then
    begin
      ScrollTo(FItems^.Count);
    end
    else if KeyChar = HomeEndKey then
    begin
      if FCurrentItem > 1 then
      begin
        ScrollTo(1);
      end
      else
      begin
        ScrollTo(FItems^.Count);
      end;
    end
    else if KeyChar = EnterKey then
    begin
      if FCurrentItem > 0 then
      begin
        OpenBrowser(TFeedItem(FItems^.GetNth(FCurrentItem - 1)).MainLink);
      end;
    end
    else if KeyChar = ShellKey then
    begin
      FullScreen;
      ClrScr;
      SwapVectors;
      Exec(GetEnv('SHELL'), '');
      SwapVectors;
      ClrScr;
      Redraw;
    end;
  until KeyChar = QuitKey;
end;

destructor TTui.Destroy;
var
  i: cardinal;
  p: TFeedItem;
begin
  if Assigned(FItems) then
  begin
    if FItems^.Count > 0 then
    begin
      for i := 0 to FItems^.Count - 1 do
      begin
        p := FItems^.GetNth(i);
        p.Free;
      end;
    end;

    Dispose(FItems, Done);
  end;

  SetNewTitle(FTitleOriginal);
  FullScreen;
  NormVideo;
  ClrScr;

{$IFNDEF __GPC__}
  inherited Destroy;
{$ENDIF}
end;

procedure TTui.DisplayItem(const Item: TFeedItem);
var
  AItem: TFeedItem;
  Items: cardinal;
  Title: String;
begin
  AItem := CreateFeedItem;
  AItem.Title := Item.TitleText;
  AItem.Subject := Item.Subject;
  AItem.Created := Item.Created;
  AItem.Description := Item.DescriptionText;
  AItem.Links := Item.Links;
  AItem.MainLink := Item.GetMainLink;
  AItem.Enclosure := Item.Enclosure;
  FItems^.Add(AItem);
  Items := FItems^.Count;

  if Items < cardinal(ScreenHeight - 4) then
  begin
    Title := AItem.TitleText;
    if Length(Title) > (ScreenWidth div 2 - 3 - Length(IntToStr(Items))) then
    begin
      Title := Copy(Title, 1, ScreenWidth div 2 - 3 - Length(IntToStr(Items)) - 4) + '...';
    end;

    TextBackground(AColorTable.FIndexBack);
    TextColor(AColorTable.FIndexFore);
    Write(Items, ': ');

    TextBackground(AColorTable.FTitleBack);
    TextColor(AColorTable.FTitleFore);
    WriteLn(Title);
  end;
end;

procedure TTui.HandleMessage(IsError: Boolean; MessageName, Extra: String);
begin
  DrawStatus;

  if IsError then
  begin
    Write(ErrorError)
  end
  else
  begin
    Write(ErrorWarning);
  end;

  Write(': ', MessageName);

  if Extra <> '' then
  begin
    Write(' (', Extra, ')');
  end;
end;

procedure TTui.RetrieveFeed(Resource: String);
begin
  DrawFeedList;
  inherited RetrieveFeed(Resource);
  GoItem;
end;

procedure TTui.NotifyUpdate;
begin
  DrawStatus;
  WriteLn(UpdateAvailable);
end;

procedure TTui.ShowHelp;
begin
  DrawInfoBar;

  Write(GoURL);
  Write(Options:25 - Length(GoUrl) + Length(Options));
  WriteLn;

  Write(Donate);
  Write(Quit:25 - Length(Donate) + Length(Quit));
end;

procedure TTui.GoURI;
var
  URI: String;
begin
  DrawStatus;

  WriteLn;
  Write(Feed);
  ReadLn(URI);

  if URI <> '' then
  begin
    RetrieveFeed(URI);
  end;
end;

procedure TTui.QueryItemNumber;
var
  ErrPos: word;
  iNo: cardinal;
  No: String;
begin
  DrawStatus;

  WriteLn;
  Write(ItemNo);
  ReadLn(No);

  Val(No, iNo, ErrPos);

  if ErrPos = 0 then
  begin
    FCurrentItem := iNo;
  end
  else
  begin
    WriteLn(InvalidNumber);
  end;
end;

procedure TTui.GoItem;
var
  Desc: String;
  i, j, k: cardinal;
  StatusText: String;
const
  DescLen = 150;
procedure PrintField(const Name, Value: String);
begin
  if Value <> '' then
  begin
    WriteLn(Name, Value)
  end;
end;

begin
  RedrawConditional;

  if FItems^.Count > 0 then
  begin
    with TFeedItem(FItems^.GetNth(FCurrentItem - 1)) do
    begin
      Desc := Description;
      if Length(Desc) > DescLen then
      begin
        Desc := Copy(Desc, 1, DescLen) + '...';
      end;

      DrawStatus;
      if MainLink <> '' then
      begin
        StatusText := MainLink + ActivateLink;
        if Length(StatusText) > ScreenWidth then
        begin
          StatusText := Copy(StatusText, 1, ScreenWidth - 5) + '...';
        end;

        Write(StatusText);
      end;

      DrawFeedInfo;
      WriteLn(ItemNo, FCurrentItem, NextPrevLink);
      PrintField(ItemTitle, Title);
      PrintField(ItemSubject, Subject);
      PrintField(ItemCreated, Created);

      if Desc <> '' then
      begin
        Write(ItemDesc);

        TextBackground(AColorTable.FDescBack);
        TextColor(AColorTable.FDescFore);
        WriteLn(Desc);

        TextBackground(AColorTable.FInfoBack);
        TextColor(AColorTable.FInfoFore);
      end;

      PrintField(ItemEmail, Contact^.Email);
      PrintField(ItemEncl, Enclosure.URL);

      j := 0;
      k := 0;
      for i := 0 to Links^.Count - 1 do
      begin
        if Links^.GetNth(i) = nil then
        begin
          Inc(j);
          Continue;
        end;

        if j = FCurrentItem then
        begin
          Inc(k);
          WriteLn(StringReplace(ItemLink, '%i', IntToStr(k), [rfReplaceAll]),
            StrPas(Links^.GetNth(i)));
        end
        else if j > FCurrentItem then
        begin
          Break;
        end;
      end;
    end;
  end;
end;

procedure TTui.OpenBrowser(Link: String);
var
  Browser, Tmp: String;
  Index: byte;
begin
  Browser := Settings.GetString(Settings.IndexOf('for:http'));
  { Check for the default registry association }
  if Pos('%1', Browser) <> 0 then
  begin
    { Check for the first command-line switch }
    Index := Pos('-', Browser);
    if Index = 0 then
    begin
      Index := Pos('/', Browser);
    end;

    if Index = 0 then
    begin
      Index := Pos('%1', Browser);
    end;

    Tmp := Copy(Browser, Index, Length(Browser) - Index + 1);
    Browser := Copy(Browser, 1, Index - 2);

    Link := StringReplace(Tmp, '%1', Link, [rfReplaceAll]);
  end;

  SwapVectors;
  Exec(FSearch(Browser, GetEnv('PATH')), '"' + Link + '"');
  SwapVectors;
end;

procedure TTui.SetOptions;
const
  NumLen = 2;
  Width = 37;
var
  i, ErrPos, Len: byte;
  HBound, Index, SetInt: integer;
  SetBool: Boolean;
  SetName, SetVal: String;
  SRec: PRList;

function BoolToString(i: Boolean): String;
begin
  if i then
  begin
    BoolToString := TrueString
  end
  else
  begin
    BoolToString := FalseString
  end
end;

{$IFDEF NO_SUPPORTS_UNICODE}
function UTF8Decode(const s: String): String;
begin
  UTF8Decode := s
end;
{$ENDIF NO_SUPPORTS_UNICODE}

begin
  DrawFeedInfo;

  WriteLn;
  TextBackground(AColorTable.FIndexBack);
  TextColor(AColorTable.FIndexFore);
  Write(NumSym:NumLen);

  TextBackground(AColorTable.FTitleBack);
  TextColor(AColorTable.FTitleFore);
  WriteLn(OptionName:NumLen + Length(OptionName), OptionVal:Width - Length(OptionName) div 2 + 3 + NumLen);
  WriteLn;

  with Settings do
  begin
    Enumerate(SRec, HBound);

    for i := 0 to HBound - 1 do
    begin
      TextBackground(AColorTable.FIndexBack);
      TextColor(AColorTable.FIndexFore);

      SetName := PRSetting(SRec^.GetNth(i))^.Description;
      Write((i + 1):NumLen, '.');

      TextBackground(AColorTable.FTitleBack);
      TextColor(AColorTable.FTitleFore);
      Write(SetName:NumLen + Length(SetName));

      case PRSetting(SRec^.GetNth(i))^.ValueType of
        TypeString:
        begin
          Write(PRSetting(SRec^.GetNth(i))^.ValueString:Width - Length(UTF8Decode(SetName)) + Length(PRSetting(SRec^.GetNth(i))^.ValueString));
        end;
        TypeInteger:
        begin
          Index := PRSetting(SRec^.GetNth(i))^.ValueInteger;
          Len := 0;

          while Index > 0 do
          begin
            Inc(Len);
            Index := Index div 10;
          end;

          Write(PRSetting(SRec^.GetNth(i))^.ValueInteger:Width - Length(UTF8Decode(SetName)) + Len);
        end;
        TypeBoolean:
        begin
          SetVal := BoolToString(PRSetting(SRec^.GetNth(i))^.ValueBoolean);
          Len := Length(SetVal);
          Write(SetVal:Width - Length(UTF8Decode(SetName)) + Len);
        end;
      end;

      WriteLn;
    end;

    repeat
      Write(SettingToChange);
      TextBackground(AColorTable.FIndexBack);
      TextColor(AColorTable.FIndexFore);

      ReadLn(SetName);
      Len := Length(SetName);

      Val(SetName, Index, ErrPos);
      if ErrPos = 0 then
      begin
        Dec(Index)
      end
      else
      begin
        Index := 0
      end;

      TextBackground(AColorTable.FTitleBack);
      TextColor(AColorTable.FTitleFore);

      if (Index >= 0) and (Index < HBound) and (Len <> 0) then
      begin
        Write(NewValue);
        ReadLn(SetVal);

        case PRSetting(SRec^.GetNth(Index))^.ValueType of
          TypeString:
          begin
            SetString(Index, SetVal);
          end;
          TypeInteger:
          begin
            Val(SetVal, SetInt, ErrPos);

            if ErrPos = 0 then
            begin
              SetInteger(Index, SetInt);
            end;
          end;
          TypeBoolean:
          begin
            if LowerCase(SetVal) = LowerCase(TrueString) then
            begin
              SetBoolean(Index, True)
            end
            else if LowerCase(SetVal) = LowerCase(FalseString) then
            begin
              SetBoolean(Index, False)
            end
          end;
        end;
      end;
    until (Len = 0) or (Index < 0) or (SetName = QuitKey);

    DrawFeedInfo;
    GoItem;
  end;
end;

procedure TTui.GoDonate;
begin
  OpenBrowser('http://sourceforge.net/donate/index.php?group_id=90897');
end;

procedure TTui.ScrollDown;
var
  i: word;
  Title: String;
begin
  FViewPort.FirstItem := FViewPort.LastItem + 1;
  FViewPort.LastItem := FViewPort.LastItem + FViewPort.PortHeight;
  FCurrentItem := FViewPort.FirstItem;

  if FItems^.Count < FViewPort.PortHeight then
  begin
    FViewPort.FirstItem := 1;
    FViewPort.LastItem := FItems^.Count;
    FCurrentItem := FViewPort.LastItem;
  end
  else if FViewPort.LastItem > FItems^.Count then
  begin
    FViewPort.LastItem := FItems^.Count;
    FViewPort.FirstItem := FViewPort.LastItem - FViewPort.PortHeight + 1;
    FCurrentItem := FViewPort.FirstItem;
  end;

  DrawFeedList;
  ClrScr;

  for i := FViewPort.FirstItem to FViewPort.LastItem do
  begin
    Title := TFeedItem(FItems^.GetNth(i - 1)).Title;
    if Length(Title) > ScreenWidth div 2 - 3 - Length(IntToStr(i)) then
    begin
      Title := Copy(Title, 1, ScreenWidth div 2 - 3 - Length(IntToStr(i)) - 4) + '...';
    end;

    TextBackground(AColorTable.FIndexBack);
    TextColor(AColorTable.FIndexFore);
    Write(i, ': ');

    TextBackground(AColorTable.FTitleBack);
    TextColor(AColorTable.FTitleFore);
    WriteLn(Title);
  end;

  GoItem;
end;

procedure TTui.ScrollUp;
var
  i: word;
  Title: String;
begin
  if FViewPort.FirstItem > FViewPort.PortHeight then
  begin
    FViewPort.FirstItem := FViewPort.FirstItem - FViewPort.PortHeight;
    FViewPort.LastItem := FViewPort.LastItem - FViewPort.PortHeight;
  end
  else if FItems^.Count < FViewPort.PortHeight then
  begin
    FViewPort.FirstItem := 1;
    FViewPort.LastItem := FItems^.Count;
  end
  else
  begin
    FViewPort.FirstItem := 1;
    FViewPort.LastItem := FViewPort.PortHeight;
  end;

  FCurrentItem := FViewPort.FirstItem;
  DrawFeedList;
  ClrScr;

  for i := FViewPort.FirstItem to FViewPort.LastItem do
  begin
    Title := TFeedItem(FItems^.GetNth(i - 1)).Title;
    if Length(Title) > ScreenWidth div 2 - 3 - Length(IntToStr(i)) then
    begin
      Title := Copy(Title, 1, ScreenWidth div 2 - 3 - Length(IntToStr(i)) - 4) + '...';
    end;

    TextBackground(AColorTable.FIndexBack);
    TextColor(AColorTable.FIndexFore);
    Write(i, ': ');

    TextBackground(AColorTable.FTitleBack);
    TextColor(AColorTable.FTitleFore);
    WriteLn(Title);
  end;

  GoItem;
end;

procedure TTui.ScrollTo(n: word);
begin
  if (n <= FViewPort.FirstItem) and (n > 0) then
  begin
    repeat
      ScrollUp;
    until n >= FViewPort.FirstItem;
  end
  else if n > FViewPort.LastItem then
  begin
    repeat
      ScrollDown;
    until n <= FViewPort.LastItem;
  end;

  FCurrentItem := n;
  GoItem;
end;

procedure TTui.DrawFeedInfo;
begin
  Window(FDimInfo.LeftStart, FDimInfo.TopStart, FDimInfo.LeftEnd, FDimInfo.TopEnd);
  TextBackground(AColorTable.FInfoBack);
  TextColor(AColorTable.FInfoFore);
  ClrScr;
end;

procedure TTui.DrawFeedList;
begin
  Window(FDimList.LeftStart, FDimList.TopStart, FDimList.LeftEnd, FDimList.TopEnd);
  TextBackground(AColorTable.FTitleBack);
  TextColor(AColorTable.FTitleFore);

  GotoXY(1, FItems^.Count + 1);
end;

procedure TTui.DrawInfoBar;
begin
  Window(FDimInfoBar.LeftStart, FDimInfoBar.TopStart, FDimInfoBar.LeftEnd, FDimInfoBar.TopEnd);
  TextBackground(AColorTable.FInfoBarBack);
  TextColor(AColorTable.FInfoBarFore);
  ClrScr;
end;

procedure TTui.DrawSeparator;
begin
  Window(FDimSep.LeftStart, FDimSep.TopStart, FDimSep.LeftEnd, FDimSep.TopEnd);
  TextBackground(AColorTable.FSep);
  ClrScr;
end;

procedure TTui.DrawStatus;
begin
  Window(FDimStatus.LeftStart, FDimStatus.TopStart, FDimStatus.LeftEnd, FDimStatus.TopEnd);
  TextBackground(AColorTable.FStatusBack);
  TextColor(AColorTable.FStatusFore);
  ClrScr;
end;

procedure TTui.DrawUIString;
begin
  Window(FDimUI.LeftStart, FDimUI.TopStart, FDimUI.LeftEnd, FDimUI.TopEnd);
  TextBackground(AColorTable.FUIBack);
  TextColor(AColorTable.FUIFore);
  ClrScr;
  Write(UserAgent:75);
end;

procedure TTui.InitWindowDims;
const
  Origin = 1;
  SingleLine = 1;

  InfoBarHeight = 2;
  SepBarWidth = 1;
  StatusHeight = SingleLine;
  UIHeight = SingleLine;

function Half(const n: word): word;
begin
  Half := n div 2;
end;
begin
  FDimInfo.LeftEnd := ScreenWidth;
  FDimInfo.TopEnd := ScreenHeight - (InfoBarHeight + StatusHeight);
  FDimInfo.LeftStart := Half(ScreenWidth) + SepBarWidth;
  FDimInfo.TopStart := UIHeight + SingleLine;

  FDimInfoBar.LeftEnd := ScreenWidth;
  FDimInfoBar.TopEnd := ScreenHeight - StatusHeight;
  FDimInfoBar.LeftStart := Origin;
  FDimInfoBar.TopStart := ScreenHeight - StatusHeight - SingleLine;

  FDimList.LeftEnd := Half(ScreenWidth) - SepBarWidth;
  FDimList.TopEnd := ScreenHeight - (InfoBarHeight + StatusHeight);
  FDimList.LeftStart := Origin;
  FDimList.TopStart := UIHeight + SingleLine;

  FDimSep.LeftEnd := Half(ScreenWidth);
  FDimSep.TopEnd := ScreenHeight - (InfoBarHeight + StatusHeight);
  FDimSep.LeftStart := Half(ScreenWidth);
  FDimSep.TopStart := UIHeight + SepBarWidth;

  FDimStatus.LeftEnd := ScreenWidth;
  FDimStatus.TopEnd := ScreenHeight;
  FDimStatus.LeftStart := Origin;
  FDimStatus.TopStart := ScreenHeight;

  FDimUI.LeftEnd := ScreenWidth;
  FDimUI.TopEnd := UIHeight;
  FDimUI.LeftStart := Origin;
  FDimUI.TopStart := Origin;
end;

function TTui.GetOriginalTitle: String;
var
  Data: Pointer;
  Res: String;
begin
  Res := GetEnv('R3R_DEFAULT_TITLE');
  if Res = '' then
  begin
{$IFDEF MSWINDOWS}
    GetMem(Data, ScreenWidth);
    GetConsoleTitle(Data, ScreenWidth);
    Res := String(Data);
    FreeMem(Data);
{$ELSE}
    Data := nil;

    if GetEnv('DISPLAY') <> '' then
    begin
      Res := GetEnv('USER') + '@' + GetEnv('HOSTNAME') + ': ' + GetCurrentDir;
    end
    else
    begin
      Res := String(Data);
    end;
{$ENDIF MSWINDOWS}
  end;

  GetOriginalTitle := Res;
end;

procedure TTui.SetNewTitle(const NewTitle: String);
var
  Data: Pointer;
begin
{$IFDEF MSWINDOWS}
  Data := PChar(NewTitle);
  SetConsoleTitle(Data);
{$ELSE}
  if GetEnv('DISPLAY') <> '' then
  begin
    SwapVectors;
    Exec(GetInstalledPrefix + '/bin/r3r-settitle', '"' + NewTitle + '"');
    SwapVectors;
  end;
{$ENDIF MSWINDOWS}
end;

procedure TTui.Draw;
begin
  ClrScr;

  DrawUIString;
  DrawFeedInfo;
  DrawInfoBar;
  DrawStatus;
  DrawSeparator;

  DrawFeedList;

  FScreenHeight := ScreenHeight;
  FScreenWidth := ScreenWidth;
  ShowHelp
end;

procedure TTui.Redraw;
var
  iTmp: word;
begin
  Draw;

  iTmp := FCurrentItem;
  ScrollTo(FViewPort.FirstItem);
  ScrollTo(iTmp)
end;

{
  Whether this does anything depends on the capabilities
  of the compiler's CRT unit.
}
procedure TTui.RedrawConditional;
begin
  if (ScreenHeight <> FScreenHeight) or (ScreenWidth <> FScreenWidth) then
  begin
    Redraw;
  end;
end;

end.
