unit Tui;

interface

uses
  LibR3R, RList;

type
  TViewport = record
    FirstItem, LastItem: word;
    PortHeight: word;
  end;

  TTui = class(TLibR3R)
  private
    FCurrentItem: cardinal;
    FItems: PRList;
    FTitleOriginal: String;
    FViewPort: TViewport;
    procedure DrawFeedInfo;
    procedure DrawFeedList;
    procedure DrawInfoBar;
    procedure DrawSeparator;
    procedure DrawStatus;
    procedure DrawUIString;
    function GetOriginalTitle: String;
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
  Crt, Dos, Info, SysUtils, TuiStrings
{$IFDEF MSWINDOWS}
  , Windows
{$ENDIF};

{$IFNDEF HAS_SCREENHEIGHTWIDTH}
var
  ScreenHeight: word;
  ScreenWidth: word;
{$ENDIF}

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
  UpKey = 'k';
var
  FeedIndex: word;
  iTmp: cardinal;
  KeyChar: char;
begin
  inherited Create;
  New(FItems, Init);
  FCurrentItem := 1;
  FTitleOriginal := GetOriginalTitle;

{$IFNDEF HAS_SCREENHEIGHTWIDTH}
{$IFDEF __GPC__}
  CRTInit;
  ScreenHeight := ScreenSize.Y;
  ScreenWidth := ScreenSize.X;
{$ELSE}
  ScreenHeight := 25;
  ScreenWidth := 80;
{$ENDIF __GPC__}
{$ENDIF HAS_SCREENHEIGHTWIDTH}

  FViewPort.FirstItem := FCurrentItem;
  FViewPort.LastItem := ScreenHeight - 5;
  FViewPort.PortHeight := FViewPort.LastItem;
  SetNewTitle(AppName);

  ClrScr;

  DrawUIString;
  DrawFeedInfo;
  DrawInfoBar;
  DrawStatus;
  DrawSeparator;

  DrawFeedList;
  ClrScr;

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

  ShowHelp;

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
      iTmp := FCurrentItem;
      ScrollTo(FViewPort.FirstItem);
      ScrollTo(iTmp);
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
  Window(1, 1, ScreenWidth, ScreenHeight);
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
  AItem.Title := Item.Title;
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
    Title := AItem.Title;
    if Length(Title) > (ScreenWidth div 2 - 3 - Length(IntToStr(Items))) then
    begin
      Title := Copy(Title, 1, ScreenWidth div 2 - 3 - Length(IntToStr(Items)) - 4) + '...';
    end;

    WriteLn(Items, ': ', Title);
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

  ShowHelp;
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
begin
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
      WriteLn(ItemTitle, Title);
      WriteLn(ItemSubject, Subject);
      WriteLn(ItemCreated, Created);
      WriteLn(ItemDesc, Desc);
      WriteLn(ItemEmail, Contact^.Email);
      WriteLn(ItemEncl, Enclosure.URL);

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
  Width = 30;
var
  i, ErrPos, Len: byte;
  HBound, Index, SetInt: integer;
  SetBool: Boolean;
  SetName, SetVal: String;
  SRec: PRList;
begin
  DrawFeedInfo;

  WriteLn;
  WriteLn(OptionName, OptionVal:Width - Length(OptionName) div 2 + 3);
  WriteLn;

  with Settings do
  begin
    Enumerate(SRec, HBound);

    for i := 0 to HBound - 1 do
    begin
      Write(PRSetting(SRec^.GetNth(i))^.Name);

      case PRSetting(SRec^.GetNth(i))^.ValueType of
        TypeString:
        begin
          Write(PRSetting(SRec^.GetNth(i))^.ValueString:Width - Length(PRSetting(SRec^.GetNth(i))^.Name) + Length(PRSetting(SRec^.GetNth(i))^.ValueString));
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

          Write(PRSetting(SRec^.GetNth(i))^.ValueInteger:Width - Length(PRSetting(SRec^.GetNth(i))^.Name) + Len);
        end;
        TypeBoolean:
        begin
          Write(PRSetting(SRec^.GetNth(i))^.ValueBoolean:Width - Length(PRSetting(SRec^.GetNth(i))^.Name) + 5);
        end;
      end;

      WriteLn;
    end;

    repeat
      Write(SettingToChange);
      ReadLn(SetName);

      Index := IndexOf(SetName);
      Len := Length(SetName);

      if Index <> -1 then
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
            if LowerCase(SetVal) = 'true' then
            begin
              SetBool := true;
            end
            else
            begin
              SetBool := false;
            end;

            SetBoolean(Index, SetBool);
          end;
        end;
      end;
    until (Len = 0) or (SetName = QuitKey);

    DrawFeedInfo;
    GoItem;
    ShowHelp;
  end;
end;

procedure TTui.GoDonate;
begin
  OpenBrowser('http://sourceforge.net/donate/index.php?group_id=90897');
  ShowHelp;
end;

procedure TTui.ScrollDown;
var
  i: word;
  Title: String;
begin
  FViewPort.FirstItem := FViewPort.LastItem;
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
    FViewPort.FirstItem := FViewPort.LastItem - FViewPort.PortHeight;
    Dec(FCurrentItem);
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

    WriteLn(i, ': ', Title);
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

    WriteLn(i, ': ', Title);
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
  Window(ScreenWidth div 2 + 1, 2, ScreenWidth, ScreenHeight - 3);
  TextBackground(Black);
  TextColor(Green);
  ClrScr;
end;

procedure TTui.DrawFeedList;
begin
  Window(1, 2, ScreenWidth div 2 - 1, ScreenHeight - 3);
  TextBackground(Black);
  TextColor(Green);

  GotoXY(1, FItems^.Count + 1);
end;

procedure TTui.DrawInfoBar;
begin
  Window(1, ScreenHeight - 2, ScreenWidth, ScreenHeight - 1);
  TextBackground(Blue);
  TextColor(Yellow);
  ClrScr;
end;

procedure TTui.DrawSeparator;
begin
  Window(ScreenWidth div 2, 2, ScreenWidth div 2, ScreenHeight - 3);
  TextBackground(LightCyan);
  ClrScr;
end;

procedure TTui.DrawStatus;
begin
  Window(1, ScreenHeight, ScreenWidth, ScreenHeight);
  TextBackground(Black);
  TextColor(Magenta);
  ClrScr;
end;

procedure TTui.DrawUIString;
begin
  Window(1, 1, ScreenWidth, 2);
  TextBackground(Blue);
  TextColor(Yellow);
  ClrScr;
  Write(UserAgent:75);
end;

function TTui.GetOriginalTitle: String;
var
  Data: Pointer;
begin
{$IFDEF MSWINDOWS}
  GetMem(Data, ScreenWidth);
  GetConsoleTitle(Data, ScreenWidth);
  GetOriginalTitle := String(Data);
  FreeMem(Data);
{$ELSE}
  Data := nil;

{$IFDEF UNIX}
  GetOriginalTitle := GetEnv('USER') + '@' + GetEnv('HOSTNAME') + ': ' + GetCurrentDir;
{$ELSE}
  GetOriginalTitle := String(Data);
{$ENDIF UNIX}
{$ENDIF MSWINDOWS}
end;

procedure TTui.SetNewTitle(const NewTitle: String);
var
  Data: Pointer;
begin
{$IFDEF MSWINDOWS}
  Data := PChar(NewTitle);
  SetConsoleTitle(Data);
{$ELSE}
{$IFDEF UNIX}
  SwapVectors;
  Exec(Settings.GetString(Settings.IndexOf('installed-prefix')) + '/bin/r3r-settitle', '"' + NewTitle + '"');
  SwapVectors;
{$ENDIF UNIX}
{$ENDIF MSWINDOWS}
end;

end.
