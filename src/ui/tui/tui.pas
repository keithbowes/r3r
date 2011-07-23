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
    FScrollingUp: Boolean;
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
    procedure InitWindowDims;
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
    procedure HandleMessage(IsError: Boolean; MessageName, Extra: String); override;
    procedure RetrieveFeed(Resource: String); override;
  end;

implementation

{$INCLUDE "tuidefs.inc"}

uses
  Dos, Info, Mailcap, RKeys, RSettings_Routines, RTitle, Skin,
  SysUtils, TuiFuncs, TuiStrings
{$IFNDEF USE_NCRT}
  , Crt
{$ELSE}
  , nCrt, nCurses
{$ENDIF USE_NCRT}
  
{$IFDEF USE_READLINE}
  , ReadLine, RStrings
{$ENDIF};

{$IFDEF USE_READLINE}
var
  rl_finished: Boolean;

{$calling cdecl}
procedure read_chars(chars: PChar);
begin
  ClrScr;
  rl_finished := true;

  if StrLen(chars) > 0 then
  begin
    add_history(chars);
  end;
end;

{$calling default}
{$ENDIF}

var
  obj: TTui;

function CreateFeedItem: TFeedItem;
begin
  CreateFeedItem := TFeedItem.Create;
end;

procedure ItemReceived(const Item: TFeedItem);
var
  AItem: TFeedItem;
  Items: cardinal;
  Title: String;
begin
  with obj do
  begin
    AItem := CreateFeedItem;
    AItem.Title := Item.TitleText;
    AItem.Subject := Item.Subject;
    AItem.Created := Item.Created;
    AItem.Description := Item.DescriptionText;
    AItem.Link := Item.Link;
    AItem.Contact.Email := Item.Contact.Email;
    AItem.Enclosure.MimeType := Item.Enclosure.MimeType;
    AItem.Enclosure.URL := Item.Enclosure.URL;
    FItems^.Add(AItem);
    Items := FItems^.Count;

    if Items <= FViewPort.PortHeight then
    begin
      Title := AItem.TitleText;
      if Length(Title) > (FDimList.LeftEnd - 3 - Length(IntToStr(Items))) then
      begin
        Title := Copy(Title, 1, FDimList.LeftEnd - 3 - Length(IntToStr(Items)) - 6) + '...';
      end;

      TextBackground(SkinColorTable.FIndexBack);
      TextColor(SkinColorTable.FIndexFore);
      Write(Items, ': ');

      TextBackground(SkinColorTable.FTitleBack);
      TextColor(SkinColorTable.FTitleFore);
      TuiWriteLn(Title);
    end;
  end;
end;

constructor TTui.Create;
var
  FeedIndex: word;
  KeyChar: char;
  mc: PMailCap;
  prog: String;
begin
  inherited Create;
  New(FItems, Init);
  FCurrentItem := 1;
  FScrollingUp := true;

  RegisterItemCallback(ItemReceived);
  obj := Self;

{$IFDEF __GPC__}
  CRTInit;
{$ENDIF __GPC__}

  Draw;
  SetNewTitle(AppName);

{$IFDEF USE_READLINE}
  while History^.IsNext do
  begin
    add_history(StrToPChar(History^.GetNext));
  end;
{$ENDIF}

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

    if KeyChar <> NullKey then
    begin
      { Case insensitivity a la other programs}
      KeyChar := LowerCase(KeyChar);
    end
    else
    begin
      { Read the scan code of control characters }
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
    else if (KeyChar = DownArrow) or (KeyChar = DownKey) then
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
    else if (KeyChar = UpArrow) or (KeyChar = UpKey) then
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
    else if TranslateControl(KeyChar) = RefreshKey then
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
    else if (KeyChar = HelpKey) or (KeyChar = HelpSymKey) then
    begin
      OpenBrowser(GetInstalledPrefix + PathDelim + 'share' + PathDelim + LowerCase(AppName) + PathDelim + 'docs' + PathDelim + 'keys.html');
    end
    else if (KeyChar = EnterKey) or (KeyChar = RightArrow) or (KeyChar = RightKey) then
    begin
      if FItems^.Count > 0 then
      begin
        OpenBrowser(TFeedItem(FItems^.GetNth(FCurrentItem - 1)).Link);
      end;
    end
    else if KeyChar = EnclosureKey then
    begin
      EndWin;
      New(mc, Init);
      SwapVectors;
      prog := mc^.GetProg(TFeedITem(FItems^.GetNth(FCurrentItem - 1)).Enclosure.MimeType);
      if Prog <> '' then
      begin
        mc^.ExecProg(mc^.GetProg(TFeedITem(FItems^.GetNth(FCurrentItem - 1)).Enclosure.MimeType), TFeedITem(FItems^.GetNth(FCurrentItem - 1)).Enclosure.URL);
      end
      else
      begin
        prog := Settings.GetString(Settings.IndexOf('for:.ogg'));
        Exec(prog, TFeedITem(FItems^.GetNth(FCurrentItem - 1)).Enclosure.URL);
      end;
      SwapVectors;
      Dispose(mc, Done);
      ClrScr;
      Redraw;
    end
    else if KeyChar = ShellKey then
    begin
      EndWin;
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

  EndWin;

{$IFNDEF __GPC__}
  inherited Destroy;
{$ENDIF}
end;

procedure TTui.HandleMessage(IsError: Boolean; MessageName, Extra: String);
begin
  DrawStatus;

  if IsError then
  begin
    TuiWrite(ErrorError)
  end
  else
  begin
    TuiWrite(ErrorWarning);
  end;

  TuiWrite(': ' + MessageName);

  if Extra <> '' then
  begin
    TuiWrite('(' + Extra + ')');
  end;
end;

procedure TTui.RetrieveFeed(Resource: String);
begin
  if FItems^.Count > 0 then
  begin
    ScrollTo(FCurrentItem);
  end;

  DrawFeedList;
  inherited RetrieveFeed(Resource);
  GoItem;

  if FItems^.Count > 0 then
  begin
    ScrollTo(FCurrentItem);
  end;
end;

procedure TTui.NotifyUpdate;
begin
  DrawStatus;
  TuiWrite(UpdateAvailable);
end;

procedure TTui.ShowHelp;
const
  OptSep = '     ';
var
  Opts: String;

procedure AddOption(opt: String);
begin
  if Length(Opts) + Length(OptSep) + Length(opt) < ScreenWidth then
  begin
    if Opts <> '' then
    begin
      Opts := Opts + OptSep;
    end;

    Opts := Opts + opt;
  end;
end;

begin
  Opts := '';

  DrawInfoBar;

  AddOption(GoURL);
  AddOption(Options);
  AddOption(Donate);
  AddOption(Quit);
  AddOption(Help);

  TuiWrite(Opts);
end;

procedure TTui.GoURI;
var
  URI: String;
begin
  DrawStatus;

  WriteLn;
{$IFNDEF USE_READLINE}
  TuiWrite(Feed);
  ReadLn(URI);
{$ELSE}
  rl_finished := false;
  rl_callback_handler_install('', read_chars);
  repeat
    ClrScr;
    TuiWrite(Feed);
    if StrLen(rl_line_buffer) > 0 then
    begin
      URI := StrPas(rl_line_buffer);
      TuiWrite(URI);
    end;
    rl_callback_read_char;
  until rl_finished;
  rl_callback_handler_remove;
{$ENDIF}

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
  TuiWrite(ItemNo);
  ReadLn(No);

  Val(No, iNo, ErrPos);

  if ErrPos = 0 then
  begin
    FCurrentItem := iNo;
  end
  else
  begin
    TuiWriteLn(InvalidNumber);
  end;
end;

procedure TTui.GoItem;
var
  Desc, DescLine: String;
  FreeLines: word;
  Len: word;
  StatusText: String;

function GetPortLength: cardinal;
begin
  GetPortLength := FDimInfo.LeftEnd - FDimInfo.LeftStart;
end;

procedure PrintField(Name, Value: String);
var
  Field: String;
begin
  if (Value <> '') and (FreeLines > 0) then
  begin
    Field := Name + Value;
    Len := GetPortLength;

    if Length(Field) > Len then
    begin
      Field := Copy(Field, 1, Len - 4) + '...';
    end;

    TuiWriteLn(Field);
    Dec(FreeLines);
  end;
end;

begin
  FreeLines := FDimInfo.TopEnd - FDimInfo.TopStart;
  Len := GetPortLength;

  if FItems^.Count > 0 then
  begin
    with TFeedItem(FItems^.GetNth(FCurrentItem - 1)) do
    begin
      DrawStatus;
      if Link <> '' then
      begin
        StatusText := Link + ActivateLink;
        if Length(StatusText) > ScreenWidth then
        begin
          StatusText := Copy(StatusText, 1, ScreenWidth - 5) + '...';
        end;

        TuiWrite(StatusText);
      end;

      DrawFeedInfo;
      PrintField(ItemNo, IntToStr(FCurrentItem) + NextPrevLink);
      PrintField(ItemTitle, Title);
      PrintField(ItemSubject, Subject);
      PrintField(ItemCreated, Created);

      PrintField(ItemEmail, Contact.Email);
      PrintField(ItemEncl, Enclosure.URL);

      Desc := Description;
      if (Desc <> '') and (FreeLines > 1) then
      begin
        TextBackground(SkinColorTable.FDescBack);
        TextColor(SkinColorTable.FDescFore);

        repeat
          Len := GetPortLength - 1;
          if Length(Desc) > Len then
          begin
            repeat
              Dec(Len)
            until (Len = 1) or (Desc[Len] in [#0, #8, #9, #10, #13, #32]);
          end;

          DescLine := Copy(Desc, 1, Len);
          Delete(Desc, 1, Len);
          Dec(FreeLines);
          TuiWriteLn(DescLine);
        until (FreeLines = 1) or (Length(Desc) = 0);

        if Length(Desc) > 0 then
        begin
          TuiWriteLn('...');
        end;

        TextBackground(SkinColorTable.FInfoBack);
        TextColor(SkinColorTable.FInfoFore);
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

  { Windows puts the path in quotes, which Exec() doesn't like }
  if (Copy(Browser, 1, 1) = '"') and (Copy(Browser, Length(Browser), 1) = '"') then
  begin
    Browser := Copy(Browser, 2, Length(Browser) - 2);
  end;

  SwapVectors;
  Exec(FSearch(Browser, GetEnv('PATH')), '"' + Link + '"');
  SwapVectors;
  Redraw;
end;

procedure TTui.SetOptions;
const
  NumLen = 2;
var
  ErrPos, Len: byte;
  HBound, Index, SetInt: integer;
  SetName, SetVal: String;
  SRec: PRList;
  Width: word;

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

procedure DisplayOptions;
var
  i: byte;

function TruncateString(const s: String): String;
begin
  if Length(s) >= Width then
  begin
    TruncateString := Copy(s, 1, Width - 1 - 3) + '...';
  end
  else
  begin
    TruncateString := s;
  end;
end;

begin
  DrawFeedList;

  if SkinOptionFull then
  begin
    Window(1, FDimUI.TopStart + FDimUI.TopEnd, ScreenWidth, ScreenHeight - FDimUI.TopEnd - (FDimStatus.TopStart - FDimStatus.TopEnd));
  end
  else
  begin
    DrawFeedInfo;
  end;

  ClrScr;

  TextBackground(SkinColorTable.FOptionIndexBack);
  TextColor(SkinColorTable.FOptionIndexFore);
  TuiEcho(NumSym, false, NumLen);

  TextBackground(SkinColorTable.FOptionDescBack);
  TextColor(SkinColorTable.FOptionDescFore);
  TuiEcho(OptionDesc, false, NumLen + Length(OptionDesc));
  
  TextBackground(SkinColorTable.FOptionValueBack);
  TextColor(SkinColorTable.FOptionValueFore);
  TuiEcho(OptionVal, true, Width - Length(OptionDesc) div 2 + 3 + NumLen);
  WriteLn;

  with Settings do
  begin
    Enumerate(SRec, HBound);

    for i := 0 to HBound - 1 do
    begin
      TextBackground(SkinColorTable.FOptionIndexBack);
      TextColor(SkinColorTable.FOptionIndexFore);

      SetName := TruncateString(PRSetting(SRec^.GetNth(i))^.Description);
      TuiConvertCodeset(SetName);

      TuiEcho(IntToStr(i + 1), false, NumLen);
      TuiWrite('.');

      TextBackground(SkinColorTable.FOptionDescBack);
      TextColor(SkinColorTable.FOptionDescFore);
      TuiEcho(SetName, false, NumLen + Length(SetName));

      TextBackground(SkinColorTable.FOptionValueBack);
      TextColor(SkinColorTable.FOptionValueFore);

      case PRSetting(SRec^.GetNth(i))^.ValueType of
        TypeString:
        begin
          TuiEcho(PRSetting(SRec^.GetNth(i))^.ValueString, false, Width - Length(UTF8Decode(SetName)) + Length(PRSetting(SRec^.GetNth(i))^.ValueString));
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

          TuiEcho(IntToStr(PRSetting(SRec^.GetNth(i))^.ValueInteger), false, Width - Length(UTF8Decode(SetName)) + Len);
        end;
        TypeBoolean:
        begin
          SetVal := BoolToString(PRSetting(SRec^.GetNth(i))^.ValueBoolean);
          Len := Length(SetVal);
          TuiEcho(SetVal, false, Width - Length(UTF8Decode(SetName)) + Len);
        end;
      end;

      WriteLn;
    end;
  end
end;

begin
  Width := ScreenWidth div 2;
  while Width >= 80 do
  begin
    Width := Width div 2;
  end;

  DisplayOptions;

  with Settings do
  begin
    repeat
      DrawStatus;
      TextBackground(SkinColorTable.FOptionPromptBack);
      TextColor(SkinColorTable.FOptionPromptFore);
      TuiWrite(SettingToChange);
      TextBackground(SkinColorTable.FOptionIndexBack);
      TextColor(SkinColorTable.FOptionIndexFore);

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

      TextBackground(SkinColorTable.FOptionPromptBack);
      TextColor(SkinColorTable.FOptionPromptFore);

      if (Index >= 0) and (Index < HBound) and (Len <> 0) then
      begin
        TuiWrite(NewValue);

        TextBackground(SkinColorTable.FOptionValueBack);
        TextColor(SkinColorTable.FOptionValueFore);
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
        DisplayOptions
      end;
    until (Len = 0) or (Index < 0) or (SetName = QuitKey);

    Redraw;
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
  if FViewPort.LastItem = 0 then
  begin
    FViewPort.FirstItem := 0;
    FViewPort.LastItem := FViewPort.PortHeight - 1;
    FCurrentItem := FViewPort.FirstItem;
  end
  else
  begin
    FViewPort.FirstItem := FViewPort.LastItem;
    FViewPort.LastItem := FViewPort.LastItem + (FDimList.TopEnd - 1) - (FDimUI.TopEnd + 1);
    FCurrentItem := FViewPort.FirstItem;
  end;

  FScrollingUp := false;

  if FItems^.Count < FViewPort.PortHeight then
  begin
    FViewPort.FirstItem := 1;
    FViewPort.LastItem := FItems^.Count;
    FCurrentItem := FViewPort.FirstItem;
  end
  else if FViewPort.LastItem > FItems^.Count then
  begin
    FViewPort.LastItem := FItems^.Count;
    FViewPort.FirstItem := FViewPort.LastItem - FViewPort.PortHeight;
    FCurrentItem := FViewPort.FirstItem + 1;
    FScrollingUp := true;
  end
  else
  begin
    Inc(FViewPort.FirstItem);
    Inc(FViewPort.LastItem);
    Inc(FCurrentItem);
  end;

  DrawFeedList;
  ClrScr;
  GotoXY(1, 1);

  for i := FViewPort.FirstItem to FViewPort.LastItem  do
  begin
    Title := TFeedItem(FItems^.GetNth(i - 1)).Title;
    if Length(Title) > FDimList.LeftEnd - Length(IntToStr(i)) - 3 then
    begin
      Title := Copy(Title, 1, FDimList.LeftEnd - Length(IntToStr(i)) - 6) + '...';
    end;

    TextBackground(SkinColorTable.FIndexBack);
    TextColor(SkinColorTable.FIndexFore);
    Write(i, ': ');

    TextBackground(SkinColorTable.FTitleBack);
    TextColor(SkinColorTable.FTitleFore);
    TuiWriteLn(Title);
  end;

  GoItem;
end;

procedure TTui.ScrollUp;
var
  i: word;
  Title: String;
begin
  FCurrentItem := 1;

  if FViewPort.FirstItem > FViewPort.PortHeight then
  begin
    FViewPort.LastItem := FViewPort.FirstItem;
    FViewPort.FirstItem := FViewPort.LastItem - FViewPort.PortHeight;

    if not FScrollingUp then
    begin
      Dec(FviewPort.FirstItem);
      Dec(FViewPort.LastItem);
    end;
  end
  else if FItems^.Count < FViewPort.PortHeight then
  begin
    FViewPort.FirstItem := 0;
    FViewPort.LastItem := FItems^.Count;
  end
  else
  begin
    FViewPort.FirstItem := 0;
    FViewPort.LastItem := FViewPort.PortHeight;
  end;
  FCurrentItem := FViewPort.FirstItem + 1;

  DrawFeedList;
  ClrScr;
  GotoXY(1, 1);

  for i := FViewPort.FirstItem + 1 to FViewPort.LastItem do
  begin
    Title := TFeedItem(FItems^.GetNth(i - 1)).Title;
    if Length(Title) > FDimList.LeftEnd - 3 - Length(IntToStr(i)) then
    begin
      Title := Copy(Title, 1, FDimList.LeftEnd - 3 - Length(IntToStr(i)) - 6) + '...';
    end;

    TextBackground(SkinColorTable.FIndexBack);
    TextColor(SkinColorTable.FIndexFore);
    Write(i, ': ');

    TextBackground(SkinColorTable.FTitleBack);
    TextColor(SkinColorTable.FTitleFore);
    TuiWriteLn(Title);
  end;

  FScrollingUp := true;
  GoItem;
end;

procedure TTui.ScrollTo(n: word);
begin
  RedrawConditional;

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
  TextBackground(SkinColorTable.FInfoBack);
  TextColor(SkinColorTable.FInfoFore);
  ClrScr;
end;

procedure TTui.DrawFeedList;
begin
  Window(FDimList.LeftStart, FDimList.TopStart, FDimList.LeftEnd, FDimList.TopEnd);
  TextBackground(SkinColorTable.FTitleBack);
  TextColor(SkinColorTable.FTitleFore);
  if FItems^.Count = 0 then
  begin
    ClrScr;
  end;

  GotoXY(1, FItems^.Count + FDimUI.TopEnd);
end;

procedure TTui.DrawInfoBar;
begin
  Window(FDimInfoBar.LeftStart, FDimInfoBar.TopStart, FDimInfoBar.LeftEnd, FDimInfoBar.TopEnd);
  TextBackground(SkinColorTable.FInfoBarBack);
  TextColor(SkinColorTable.FInfoBarFore);
  ClrScr;
end;

procedure TTui.DrawSeparator;
begin
  Window(FDimSep.LeftStart, FDimSep.TopStart, FDimSep.LeftEnd, FDimSep.TopEnd);
  TextBackground(SkinColorTable.FSep);
  ClrScr;
end;

procedure TTui.DrawStatus;
begin
  Window(FDimStatus.LeftStart, FDimStatus.TopStart, FDimStatus.LeftEnd, FDimStatus.TopEnd);
  TextBackground(SkinColorTable.FStatusBack);
  TextColor(SkinColorTable.FStatusFore);
  ClrScr;
end;

procedure TTui.DrawUIString;
begin
  Window(FDimUI.LeftStart, FDimUI.TopStart, FDimUI.LeftEnd, FDimUI.TopEnd);
  TextBackground(SkinColorTable.FUIBack);
  TextColor(SkinColorTable.FUIFore);
  ClrScr;
  TuiEcho(UserAgent, false, ScreenWidth - ((ScreenWidth - Length(UserAgent)) div 2));
end;

procedure TTui.InitWindowDims;
const
  Origin = 1;
  SingleLine = 1;

  InfoBarHeight = 1;
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
  FDimInfoBar.TopStart := ScreenHeight - StatusHeight - (InfoBarHeight - SingleLine);

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

  FViewPort.FirstItem := FCurrentItem + 1;
  FViewPort.LastItem := FDimInfoBar.TopStart - FDimUI.TopEnd - 1;
  FViewPort.PortHeight := FViewPort.LastItem - 1;
end;

procedure TTui.Draw;
begin
  ColorTableInit;
  InitWindowDims;

  FullScreen;

  DrawFeedInfo;
  DrawSeparator;
  DrawFeedList;
  ClrScr;

  DrawInfoBar;
  DrawStatus;
  DrawUIString;
  ShowHelp;

  FScreenHeight := ScreenHeight;
  FScreenWidth := ScreenWidth;
end;

procedure TTui.Redraw;
var
  iTmp: word;
begin
  Draw;

  if FItems^.Count > 0 then
  begin
    iTmp := FCurrentItem;
    ScrollTo(FViewPort.FirstItem);
    ScrollTo(iTmp);
  end;
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
