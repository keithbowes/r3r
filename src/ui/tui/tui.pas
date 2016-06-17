unit Tui;

interface

{$INCLUDE "tuidefs.inc"}

uses
  LibR3R, RList;

type
  TProcessingStatus = (psUnstarted, psStarted, psFinished);
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
    FCanAdjustDims: Boolean;
    FCanStart: Boolean;
    FCurrentItem: PtrUInt;
    FDimInfo: TWindowDim;
    FDimInfoBar: TWindowDim;
    FDimList: TWindowDim;
    FDimSep: TWindowDim;
    FDimStatus: TWindowDim;
    FDimUA: TWindowDim;
    FItems: PRList;
    FPrintItems: Boolean;
    FProcessingStatus: TProcessingStatus;
    FScreenHeight: word;
    FScreenWidth: word;
    FScrollingUp: Boolean;
    FViewPort: TViewport;
{$IFDEF USE_NCRT}
    FGotched: Boolean;
    FOnRelease: Boolean;
{$ENDIF}
    procedure Draw;
    procedure Redraw;
    procedure RedrawConditional;
    procedure DrawFeedInfo;
    procedure DrawFeedList;
    procedure DrawInfoBar;
    procedure DrawSeparator;
    procedure DrawStatus;
    procedure DrawUAString;
    procedure InitWindowDims;
  protected
    procedure ShowHelp;
    procedure GoURI;
    function QueryItemNumber: Boolean;
    procedure GoItem;
    procedure OpenProg(Sett, Link: String);
    procedure SetCursorPos;
    procedure SetOptions;
    procedure GoDonate;
    procedure ScrollDown;
    procedure ScrollUp;
    procedure ScrollTo(n: word);
    procedure LoadSubscriptions;
    procedure AddDeleteSubscription;
    procedure PrintFeedItems;
    procedure PrintTitle(Title: String; const Index: PtrUInt);
  public
    constructor Create;
    destructor Destroy; override;
    procedure HandleMessage(IsError: Boolean; MessageName, Extra: String); override;
    procedure QueueURI(Resource: String);
  end;

implementation

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

{$IF DEFINED(USE_READLINE) and not DEFINED(USE_LIBEDIT)}
var
  rl_finished, rl_option_finished: Boolean;

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

procedure read_option_chars(chars: PChar);
begin
  ClrScr;
  rl_option_finished := true;

  { Should this be taken out? }
  if StrLen(chars) > 0 then
  begin
    add_history(chars);
  end;
end;

{$calling default}
{$ENDIF}

procedure ItemReceived(const Item: TFeedItem; const Data: Pointer);
var
  AItem: TFeedItem;
  Items: PtrUInt;
  Title: String;
begin
  with TTui(Data) do
  begin
    AItem := TFeedItem.Create;
    AItem.Title := Item.TitleText;
    AItem.Subject := Item.Subject;
    AItem.Created := Item.Created;
    AItem.Description := Item.DescriptionText;
    AItem.Link := Item.Link;
    AItem.MySelf := Item.MySelf;
    AItem.Contact.Email := Item.Contact.Email;
    AItem.Enclosure.MimeType := Item.Enclosure.MimeType;
    AItem.Enclosure.URL := Item.Enclosure.URL;
    FItems^.Add(AItem);
    Items := FItems^.Count;

    if Settings.GetBoolean('show-incoming-items') then
    begin
      if Items <= FViewPort.FirstItem + FViewPort.PortHeight then
      begin
        Title := AItem.Title;
        PrintTitle(Title, Items);
      end;
    end;
  end;
end;

function GetUserAgentInfo: String;
var
  rv: String;
  t: String;
begin
  t := '';
{$IFDEF USE_NCRT}
  WriteStr(rv, curses_version);
  t := t + StringReplace(rv, ' ', '/', []) + ' ';
{$ENDIF}
{$IFDEF USE_READLINE}
{$IFNDEF USE_LIBEDIT}
  WriteStr(rv, rl_library_version);
  t := t + 'readline/' + rv + ' ';
{$ELSE}
  WriteStr(rv, rl_readline_version);
  t := t + 'libedit/' + rv;
{$ENDIF}
{$ENDIF}

  GetUserAgentInfo := t;
end;

constructor TTui.Create;
{$IFDEF USE_NCRT}
const
  BUTTON_SCROLL_DOWN = REPORT_MOUSE_POSITION;
  BUTTON_SCROLL_UP = BUTTON4_PRESSED;
{$ENDIF}
var
  KeyChar: char;
  i: word;
  mc: PMailCap;
  prog: String;
{$IFDEF USE_NCRT}
  mouse_event: MEVENT;
{$ENDIF}
begin
  inherited Create;

  if ParamCount > 0 then
  begin
    for i := 1 to ParamCount do
    begin
      if Pos('-', ParamStr(i)) = 1 then
      begin
        FCanStart := false;
        WriteLn(StringReplace(Usage, '%s', ParamStr(0), []));
        Exit;
      end;
    end;
  end;
  FCanStart := true;

  New(FItems, Init);
  FCanAdjustDims := true;
  FCurrentItem := 1;
  FPrintItems := false;
  FProcessingStatus := psUnstarted;
  FScrollingUp := false;

{$IFDEF USE_NCRT}
  FGotched := false;
  FOnRelease := false;
{$ENDIF}

  RegisterItemCallback(ItemReceived, Self);
  SetUserAgentInfo(GetUserAgentInfo);
  Settings.RegisterBoolean('show-incoming-items', 'Display', true, ShowIncomingItems);
  Settings.RegisterBoolean('wrap-descriptions', 'Display', true, WrapDescriptions);
  Settings.RegisterInteger('error-seconds', 'Display', 1, ErrorSeconds);
{$IFDEF USE_ICONV}
  Settings.RegisterString('display-encoding', 'Display', 'UTF-8', DescEnc);
{$ENDIF}

  Draw;
  SetNewTitle(AppName);

  if ParamCount > 0 then
  begin
    for i := 1 to ParamCount do
    begin
      if i = ParamCount then
      begin
        FPrintItems := true;
      end;
      QueueURI(ParamStr(i));
    end;
  end
  else if Settings.GetBoolean('load-subscriptions-on-startup') then
  begin
    LoadSubscriptions;
  end;

{$IFDEF USE_READLINE}
  while History^.IsNext do
  begin
    add_history(StrToPChar(History^.GetNext));
  end;
{$ENDIF}

  repeat
{$IFDEF USE_NCRT}
  noecho;

  { Send a key so that the mouse and resizing will work }
  if not FGotched then
  begin
    ungetch(Ord(HomeKey));
    FGotched := true;
  end;
{$ENDIF}
    if KeyPressed then
      begin
{$IFNDEF USE_NCRT}
      KeyChar := ReadKey;

      if KeyChar <> NullKey then
      begin
        { Case insensitivity a la other programs }
        KeyChar := LowerCase(KeyChar);
      end
      else
      begin
        { Read the scan code of control characters }
        KeyChar := ReadKey;
      end;
  {$ELSE}
      try
        mousemask(BUTTON1_PRESSED or BUTTON1_CLICKED or BUTTON1_DOUBLE_CLICKED or BUTTON2_PRESSED or BUTTON2_CLICKED or BUTTON_SCROLL_DOWN or BUTTON_SCROLL_UP, nil);
      finally
        i := getch;
        echo;
      end;

      case i of
        KEY_MOUSE:
        begin
          getmouse(@mouse_event);
          { Allow the mouse to work to feed view }
          if ((FViewPort.LastItem >= FViewPort.PortHeight) or ((FViewPort.LastItem < FViewPort.PortHeight) and ((mouse_event.y - 1 + FDimUA.TopEnd) <= FViewPort.LastItem))) and (mouse_event.y >= FDimList.TopStart - 1) and (mouse_event.y < FDimList.TopEnd) then
          begin
            if mouse_event.x < FDimSep.LeftStart - 1 then
            begin
              if (mouse_event.bstate and BUTTON1_PRESSED = BUTTON1_PRESSED) or (mouse_event.bstate and BUTTON1_CLICKED = BUTTON1_CLICKED) then
              begin
                FCurrentItem := mouse_event.y - 1 + FDimUA.TopEnd;
                FCurrentItem := FCurrentItem + FViewPort.FirstItem;
                FOnRelease := true;
                ScrollTo(FCurrentItem);
              end
              else if (mouse_event.bstate and BUTTON1_DOUBLE_CLICKED = BUTTON1_DOUBLE_CLICKED) or (mouse_event.bstate and BUTTON2_PRESSED = BUTTON2_PRESSED) or (mouse_event.bstate and BUTTON2_CLICKED = BUTTON2_CLICKED) then
              begin
                OpenProg('for:http', TFeedItem(FItems^.GetNth(FCurrentItem - 1)).Link);
              end
              else if mouse_event.bstate and BUTTON_SCROLL_DOWN = BUTTON_SCROLL_DOWN then
              begin
                if (FItems^.Count > FCurrentItem) and not FOnRelease then
                begin
                  Inc(FCurrentItem);
                end;

                ScrollTo(FCurrentItem);
                FOnRelease := false;
              end
              else if mouse_event.bstate and BUTTON_SCROLL_UP = BUTTON_SCROLL_UP then
              begin
                if FCurrentItem > 1 then
                begin
                  Dec(FCurrentItem);
                end;

                ScrollTo(FCurrentItem);
              end
            end
            else
            begin
              ScrollTo(FCurrentItem);
            end;
          end
          { Allow the mouse to work in the info bar }
          else if (mouse_event.y = FDimInfoBar.TopStart - 1) and (mouse_event.bstate and BUTTON1_CLICKED = BUTTON1_CLICKED) then
          begin
            if mouse_event.x < ScreenWidth div 5 then
            begin
              ungetch(Ord(GetBoundKey(QuitKey)));
            end
            else if mouse_event.x < ScreenWidth div 5 * 2 then
            begin
              GoURI;
            end
            else if mouse_event.x < ScreenWidth div 5 * 3 then
            begin
              SetOptions;
            end
            else if mouse_event.x < ScreenWidth div 5 * 4 then
            begin
              GoDonate;
            end
            else
            begin
              ungetch(Ord(GetBoundKey(HelpKey)));
            end;
          end
          else
          begin
            if mouse_event.y <> FDimInfoBar.TopStart - 1 then
            begin
              ScrollTo(FCurrentItem);
            end
            else
            begin
              ungetch(Ord(GetBoundKey(QuitKey)));
            end;
          end;
        end;
        KEY_RESIZE:
        begin
          Redraw;
        end;
      end;

      KeyChar := Chr(byte(i));
      if i < 256 then
      begin
        { Case insensitivity a la other programs }
        KeyChar := LowerCase(KeyChar);
      end;
{$ENDIF}
    end
    else
    begin
      if FProcessingStatus = psStarted then
      begin
        if RetrieveChunk then
        begin
          FProcessingStatus := psStarted;
        end
        else
        begin
          FProcessingStatus := psFinished;
        end;
      end
      else if FPRocessingStatus = psFinished then
      begin
        FProcessingStatus := psUnstarted;

        Redraw;
        ScrollTo(FCurrentItem);
      end;

      KeyChar := NullKey;
    end;

    if KeyChar = GetBoundKey(GoKey) then
    begin
      GoURI;
    end
    else if KeyChar = GetBoundKey(OptionsKey) then
    begin
      SetOptions;
    end
    else if KeyChar = GetBoundKey(SearchKey) then
    begin
      if QueryItemNumber then
      begin
        ScrollTo(FCurrentItem);
      end;
    end
    else if KeyChar = GetBoundKey(DonateKey) then
    begin
      GoDonate;
    end
    else if (KeyChar = DownArrow) or (KeyChar = GetBoundKey(DownKey)) then
    begin
      if FCurrentItem < FItems^.Count then
      begin
        Inc(FCurrentItem);
      end
      else
      begin
        FCurrentItem := 1;
      end;

      if FItems^.Count > 0 then
      begin
        ScrollTo(FCurrentItem);
      end;

      { So when scrolling during downloading feeds, the feed list will display correctly }
      if Settings.GetBoolean('show-incoming-items') and (FProcessingStatus = psStarted) then
      begin
        Redraw;
      end;
    end
    else if (KeyChar = UpArrow) or (KeyChar = GetBoundKey(UpKey)) then
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

      { So when scrolling during downloading feeds, the feed list will display correctly }
      if Settings.GetBoolean('show-incoming-items') and (FProcessingStatus = psStarted) then
      begin
        Redraw;
      end;
    end
    else if KeyChar = GetBoundKey(RefreshKey) then
    begin
      Redraw;
    end
    else if (KeyChar = GetBoundKey(ScrollDownKey)) or (KeyChar = GetBoundKey(PageDownKey)) then
    begin
      ScrollDown;
      PrintFeedItems;
    end
    else if (KeyChar = GetBoundKey(ScrollUpKey)) or (KeyChar = GetBoundKey(PageUpKey)) then
    begin
      ScrollUp;
      ScrollTo(FCurrentItem);
    end
    else if KeyChar = GetBoundKey(HomeKey) then
    begin
      ScrollTo(1);
    end
    else if KeyChar = GetBoundKey(EndKey) then
    begin
      ScrollTo(FItems^.Count);
    end
    else if KeyChar = GetBoundKey(HomeEndKey) then
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
    else if (KeyChar = GetBoundKey(HelpKey)) or (KeyChar = GetBoundKey(HelpSymKey)) then
    begin
      OpenProg('for:http', GetInstalledPrefix + PathDelim + 'share' + PathDelim +  'doc' + PathDelim + LowerCase(AppName) + PathDelim + 'keys.html');
    end
    else if (KeyChar = GetBoundKey(EnterKey)) or (KeyChar = RightArrow) or (KeyChar = GetBoundKey(RightKey)) then
    begin
      if FItems^.Count > 0 then
      begin
        OpenProg('for:http', TFeedItem(FItems^.GetNth(FCurrentItem - 1)).Link);
      end;
    end
    else if KeyChar = GetBoundKey(EmailKey) then
    begin
      prog := TFeedItem(FItems^.GetNth(FCurrentItem - 1)).Contact.Email;
      if prog <> '' then
      begin
        OpenProg('for:mailto', prog);
      end;
    end
    else if KeyChar = GetBoundKey(EnclosureKey) then
    begin
      EndWin;
      New(mc, Init);
      SwapVectors;
      prog := mc^.GetProg(TFeedITem(FItems^.GetNth(FCurrentItem - 1)).Enclosure.MimeType);
      if Prog <> '' then
      begin
        mc^.ExecProg(TFeedItem(FItems^.GetNth(FCurrentItem - 1)).Enclosure.MimeType, TFeedItem(FItems^.GetNth(FCurrentItem - 1)).Enclosure.URL);
      end;
      SwapVectors;
      Dispose(mc, Done);
      ClrScr;
      Redraw;
    end
    else if KeyChar = GetBoundKey(SubscriptionsKey) then
    begin
      LoadSubscriptions;
    end
    else if KeyChar = GetBoundKey(AddKey) then
    begin
      AddDeleteSubscription;
    end
    else if KeyChar = GetBoundKey(ShellKey) then
    begin
      EndWin;
      SwapVectors;
      Exec(GetEnv('SHELL'), '');
      SwapVectors;
      ClrScr;
      Redraw;
    end
    else if KeyChar = GetBoundKey(AbortKey) then
    begin
      UnqueueURI;

      if Settings.GetBoolean('show-incoming-items') then
      begin
        ScrollTo(FCurrentItem);
      end;
    end;
  until KeyChar = GetBoundKey(QuitKey);
end;

destructor TTui.Destroy;
var
  i: PtrUInt;
  p: TFeedItem;
begin
  if Assigned(FItems) then
  begin
    if FItems^.Count > 0 then
    begin
      for i := 0 to FItems^.Count - 1 do
      begin
        p := TFeedItem(FItems^.GetNth(i));
        p.Free;
      end;
    end;

    Dispose(FItems, Done);
  end;

  if FCanStart then
  begin
    EndWin;
  end;

  inherited Destroy;
end;

procedure TTui.HandleMessage(IsError: Boolean; MessageName, Extra: String);
var
  Message: String;
begin
  DrawStatus;

  if IsError then
  begin
    Message := ErrorError
  end
  else
  begin
    Message := ErrorWarning
  end;

  Message := Message + ': ' + MessageName;

  if Extra <> '' then
  begin
    Message := Message + ' (' + Extra + ')'
  end;

  if Length(Message) > FDimStatus.LeftEnd - FDimStatus.LeftStart then
  begin
    Message := Message[FDimStatus.LeftStart..FDimStatus.LeftEnd - 4] + '...';
  end;

  TuiWrite(Message);

  if IsError then
  begin
    Delay(Settings.GetInteger('error-seconds') * 1000);
  end;
end;

procedure TTui.QueueURI(Resource: String);
var
  Message: String;
begin
  DrawStatus;
  Message := StringReplace(Retrieving, '%s', Resource, []);

  if Length(Message) > FDimStatus.LeftEnd - FDimStatus.LeftStart then
  begin
    Message := Message[FDimStatus.LeftStart..FDimStatus.LeftEnd - 4] + '...'
  end;

  TuiWrite(Message);
  inherited QueueURI(Resource);

  if RetrieveChunk then
  begin
    FProcessingStatus := psStarted;
  end
  else
  begin
    FProcessingStatus := psFinished;
  end;

  FPrintItems := false;
end;

procedure TTui.ShowHelp;
var
  OptNum: 1..6 = 1;

procedure AddOption(opt: String);
begin
  Window(FDimInfoBar.LeftStart + ScreenWidth div 5 * (OptNum - 1), FDimInfoBar.TopStart, ScreenWidth div 5 * OptNum, FDimInfoBar.TopEnd);
  TextBackground(SkinColorTable.FInfoBarBack);
  TextColor(SkinColorTable.FInfoBarFore);

  if (ScreenWidth > 20) and (Length(opt) > ScreenWidth div 5) then
  begin
    opt := opt[1..ScreenWidth div 5 - 4];
  end
  else if ScreenWidth < 20 then
  begin
    opt := '';
  end;

  TuiWrite(opt);
  Inc(OptNum);
end;

function ReplaceKey(const s: String; const c: char): String;
begin
  ReplaceKey := StringReplace(s, '%s', GetBoundKey(c), [rfReplaceAll]);
end;

begin
  DrawInfoBar;

  AddOption(ReplaceKey(Quit, 'q'));
  AddOption(ReplaceKey(GoURL, 'g'));
  AddOption(ReplaceKey(Options, 'o'));
  AddOption(ReplaceKey(Donate, 'd'));
  AddOption(ReplaceKey(Help, '?'));
end;

procedure TTui.GoURI;
var
  URI: String;
{$IFDEF USE_LIBEDIT}
  p: PChar;
{$ENDIF}
begin
  DrawStatus;

  WriteLn;
{$IFNDEF USE_READLINE}
  TuiWrite(Feed);
  ReadLn(URI);
{$ELSE}
{$IFNDEF USE_LIBEDIT}
  rl_finished := false;
  rl_callback_handler_install('', read_chars);
  repeat
    ClrScr;
    TuiWrite(Feed);
    if rl_end > 0 then
    begin
      WriteStr(URI, rl_line_buffer);
      if Length(Feed + URI) >= ScreenWidth then
      begin
        URI := Copy(URI, 1, ScreenWidth - Length(UTF8Decode(Feed)) - 3);
      end;
      TuiWrite(URI);
      GotoXY(rl_point + Length(UTF8Decode(Feed)) + 1, 1);
    end
    else
    begin
      URI := '';
    end;
    rl_callback_read_char;
  until rl_finished;
  rl_callback_handler_remove;
{$ELSE}
  p := rl_read(StrToPChar(Feed));
  WriteStr(URI, p);
  URI := TrimRight(URI);
  add_history(p);
  rl_free(p);
  Redraw;
{$ENDIF}
{$ENDIF}

  if URI <> '' then
  begin
    FPrintItems := true;
    QueueURI(URI);
  end;
end;

function TTui.QueryItemNumber: Boolean;
var
  ErrPos: word;
  iNo: PtrUInt;
  No: String;
begin
  DrawStatus;

  WriteLn;
  TuiWrite(ItemNo);
  ReadLn(No);

  Val(No, iNo, ErrPos);

  if (ErrPos = 0) and ((iNo > 0) and (iNo <= FItems^.Count)) then
  begin
    FCurrentItem := iNo;
    QueryItemNumber := true;
  end
  else
  begin
    TuiWrite(InvalidNumber);
    QueryItemNumber := false;
  end;
end;

procedure TTui.GoItem;
var
  Desc: String;
  FreeLines: word;
  ItemText: String;
  Len: word;
  Remaining: PtrInt;
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

      WriteStr(ItemText, FCurrentItem, OutOf, FItems^.Count, StringReplace(StringReplace(NextPrevLink, '%1', GetBoundKey(DownKey), []), '%2', GetBoundKey(UpKey), []));
      DrawFeedInfo;
      PrintField(ItemNo, ItemText);
      PrintField(ItemTitle, Title);
      PrintField(ItemSubject, Subject);
      PrintField(ItemCreated, Created);

      PrintField(ItemEmail, Contact.Email);
      PrintField(ItemEncl, Enclosure.URL);

      Desc := Description;
      if (Desc <> '') and (FreeLines > 1) then
      begin
        TuiWrite(ItemDesc);
        TextBackground(SkinColorTable.FDescBack);
        TextColor(SkinColorTable.FDescFore);

        if not Settings.GetBoolean('wrap-descriptions') then
        begin
          if Length(Desc) > (GetPortLength - 1) * FreeLines then
          begin
            Desc := Copy(Desc, 1, ((GetPortLength - 1) * FreeLines) - 3) + '...';
          end;

          TuiWriteLn(Desc);
        end
        else
        begin
          Remaining := TuiWriteWrapped(Desc, [#0, #8, #9, #10, #13, ' ', '-'], GetPortLength - 1, FreeLines);
          if Remaining > 0 then
          begin
            TuiWriteLn('...');
          end;
        end;

        TextBackground(SkinColorTable.FInfoBack);
        TextColor(SkinColorTable.FInfoFore);
      end;
    end;
  end;

  DrawFeedList;
end;

procedure TTui.OpenProg(Sett, Link: String);
var
  Browser, Tmp: String;
  Index: byte;
begin
  { Do nothing on empty links }
  if Link <> '' then
  begin
    Browser := Settings.GetString(Sett);
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
    end
    else
    begin
      Link := '"' + Link + '"';
    end;

    { Windows puts the path in quotes, which Exec() doesn't like }
    if (Browser[1] = '"') and (Browser[Length(Browser)] = '"') then
    begin
      Browser := Copy(Browser, 2, Length(Browser) - 2);
    end;

    SwapVectors;
    Exec(FSearch(Browser, GetEnv('PATH')), Link);
    SwapVectors;
    Redraw;
  end
end;

procedure TTui.SetCursorPos;
begin
  try
    GotoXY(1, FCurrentItem - FViewPort.FirstItem + Ord(not FScrollingUp));
  except
    GotoXY(1, 1);
  end;
end;

procedure TTui.SetOptions;
const
  NumLen = 2;
var
  ErrPos, Len: byte;
  HBound, Index, SetInt: integer;
  SetDesc, SetName, SetVal: String;
  SRec: PRList;
  Width: word;
{$IFDEF USE_READLINE}
  Backup: PChar;
  InitDesc: String;
{$IFDEF USE_LIBEDIT}
  Prompt: PChar;
{$ENDIF}
{$ENDIF}

procedure ClearOptions;
begin
  DrawUAString;
  DrawFeedInfo;
  if SkinOptionFull then
  begin
    Window(1, FDimUA.TopStart + FDimUA.TopEnd, ScreenWidth, ScreenHeight - (FDimStatus.TopStart - FDimStatus.TopEnd));
  end;

  ClrScr;
end;

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

begin
  DrawFeedList;
  ClearOptions;

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

      SetDesc := TruncateString(PRSetting(SRec^.GetNth(i))^.Description);
      TuiConvertCodeset(SetDesc);

      WriteStr(SetVal, i + 1);
      TuiEcho(SetVal, false, NumLen);
      TuiWrite('.');

      TextBackground(SkinColorTable.FOptionDescBack);
      TextColor(SkinColorTable.FOptionDescFore);
      TuiEcho(SetDesc, false, NumLen + Length(SetDesc));

      TextBackground(SkinColorTable.FOptionValueBack);
      TextColor(SkinColorTable.FOptionValueFore);

      case PRSetting(SRec^.GetNth(i))^.ValueType of
        TypeString:
        begin
          Width := Width - 5;
          SetVal := TruncateString(PRSetting(SRec^.GetNth(i))^.ValueString);
          Width := Width + 5;

          Len := Length(SetVal);
          TuiEcho(SetVal, false, Width - Length(UTF8Decode(SetDesc)) + Len);
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

          WriteStr(SetVal, PRSetting(SRec^.GetNth(i))^.ValueInteger);
          TuiEcho(SetVal, false, Width - Length(UTF8Decode(SetDesc)) + Len);
        end;
        TypeBoolean:
        begin
          SetVal := BoolToString(PRSetting(SRec^.GetNth(i))^.ValueBoolean);
          Len := Length(SetVal);
          TuiEcho(SetVal, false, Width - Length(UTF8Decode(SetDesc)) + Len);
        end;
      end;
{$IFDEF USE_READLINE}
      add_history(StrToPChar(PRSetting(SRec^.GetNth(i))^.Description));
      history_set_pos(history_length div 2);
      if current_history <> nil then
      begin
        WriteStr(InitDesc, current_history^.Line);
      end;
{$ENDIF}

      WriteLn;
    end;
  end;
end;

begin
{$IFDEF USE_READLINE}
{$IFNDEF USE_LIBEDIT}
  rl_callback_handler_install('', read_option_chars);
{$ENDIF}
  Backup := StrToPChar(GetDataDir + '_options_history');
  write_history(Backup);
  clear_history;
{$ENDIF}

  Width := ScreenWidth div 2;
  while Width >= 80 do
  begin
    Width := Width div 2;
  end;

  FCanAdjustDims := false;

  DisplayOptions;

  with Settings do
  begin
    repeat
      DrawStatus;
{$IFDEF USE_READLINE}
{$IFNDEF USE_LIBEDIT}
      rl_option_finished := false;

      repeat
        ClrScr;
        TextBackground(SkinColorTable.FOptionPromptBack);
        TextColor(SkinColorTable.FOptionPromptFore);
        TuiWrite(SettingToChangeReadLine);

        TextBackground(SkinColorTable.FOptionIndexBack);
        TextColor(SkinColorTable.FOptionIndexFore);

        if rl_end > 0 then
        begin
          WriteStr(SetDesc, rl_line_buffer);
          if Length(SettingToChangeReadLine + SetDesc) >= ScreenWidth then
          begin
            SetDesc := Copy(SetDesc, 1, ScreenWidth - Length(SettingToChangeReadLine) - 3);
          end;

          TuiWrite(SetDesc);
        end
        else if InitDesc <> '' then
        begin
          SetDesc := InitDesc;
          if Length(SettingToChangeReadLine + SetDesc) >= ScreenWidth then
          begin
            SetDesc := Copy(SetDesc, 1, ScreenWidth - Length(SettingToChangeReadLine) - 3);
          end;

          TuiWrite(SetDesc);
          InitDesc := '';
        end;

        GotoXY(rl_point + Length(UTF8Decode(SettingToChangeReadLine)) + 1, 1);
        rl_callback_read_char;
      until rl_option_finished;
{$ELSE}
      Prompt := rl_read(StrToPChar(SettingToChangeReadLine));
      WriteStr(SetDesc, Prompt);
      rl_free(Prompt);
      TuiWrite(SetDesc);
{$ENDIF}

      for Len := 0 to SRec^.Count - 1 do
      begin
        if PRSetting(SRec^.GetNth(Len))^.Description = SetDesc then
        begin
          WriteStr(SetDesc, Len + 1);
          Break;
        end;
      end;
{$ELSE}
      TextBackground(SkinColorTable.FOptionPromptBack);
      TextColor(SkinColorTable.FOptionPromptFore);
      TuiWrite(SettingToChange);

      TextBackground(SkinColorTable.FOptionIndexBack);
      TextColor(SkinColorTable.FOptionIndexFore);
      ReadLn(SetDesc);
{$ENDIF}
      Len := Length(SetDesc);

      Val(SetDesc, Index, ErrPos);
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
{$IFDEF USE_READLINE}
{$IFDEF USE_LIBEDIT}
        ClearOptions;
        DrawStatus;
{$ENDIF}
        TuiWrite('(');
        case PRSetting(SRec^.GetNth(Index))^.ValueType of
          TypeString:
          begin
            TuiWrite(TruncateString(PRSetting(SRec^.GetNth(Index))^.ValueString));
          end;
          TypeInteger:
          begin
            Write(PRSetting(SRec^.GetNth(Index))^.ValueInteger);
          end;
          TypeBoolean:
          begin
            TuiWrite(BoolToString(PRSetting(SRec^.GetNth(Index))^.ValueBoolean));
          end
        end;
        TuiWrite(') ');
{$ENDIF}
        TuiWrite(NewValue);

        TextBackground(SkinColorTable.FOptionValueBack);
        TextColor(SkinColorTable.FOptionValueFore);
        ReadLn(SetVal);
        if SetVal <> '' then
        begin
          SetName := PRSetting(SRec^.GetNth(Index))^.Name;
          case PRSetting(SRec^.GetNth(Index))^.ValueType of
            TypeString:
            begin
              SetString(SetName, SetVal);
            end;
            TypeInteger:
            begin
              Val(SetVal, SetInt, ErrPos);

              if ErrPos = 0 then
              begin
                SetInteger(SetName, SetInt);
              end;
            end;
            TypeBoolean:
            begin
              if LowerCase(SetVal) = LowerCase(TrueString) then
              begin
                SetBoolean(SetName, True)
              end
              else if LowerCase(SetVal) = LowerCase(FalseString) then
              begin
                SetBoolean(SetName, False)
              end
            end;
          end;
        end;
        DisplayOptions
      end;
    until (Len = 0) or (Index < 0) or (SetDesc = GetBoundKey(QuitKey));

    Redraw;

    if Settings.GetBoolean('show-incoming-items') then
    begin
      GoItem;
    end;
  end;

{$IFDEF USE_READLINE}
{$IFNDEF USE_LIBEDIT}
  rl_callback_handler_remove;
{$ENDIF}
  clear_history;
  read_history(Backup);
  WriteStr(InitDesc, Backup);
  DeleteFile(InitDesc);
{$ENDIF}
end;

procedure TTui.GoDonate;
begin
  OpenProg('for:http', 'http://sourceforge.net/donate/index.php?group_id=90897');
end;

procedure TTui.ScrollDown;
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
    FViewPort.LastItem := FViewPort.LastItem + (FDimList.TopEnd - 1) - (FDimUA.TopEnd + 1);
    FCurrentItem := FViewPort.FirstItem;
    FPrintItems := true;
  end;

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
    FPrintItems := true;
  end
  else
  begin
    Inc(FViewPort.FirstItem);
    Inc(FViewPort.LastItem);
    Inc(FCurrentItem);
  end;
  FScrollingUp := FViewPort.LastItem = FItems^.Count;
  SetCursorPos;
end;

procedure TTui.ScrollUp;
begin
  FCurrentItem := 1;

  if FViewPort.FirstItem > FViewPort.PortHeight then
  begin
    FViewPort.LastItem := FViewPort.FirstItem;
    FViewPort.FirstItem := FViewPort.LastItem - FViewPort.PortHeight;
    FPrintItems := true;
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
    FPrintItems := true;
  end;
  FCurrentItem := FViewPort.FirstItem + 1;
  FScrollingUp := true;
end;

procedure TTui.ScrollTo(n: word);
begin
  { Don't allow scrolling through items during loading if that feature is disabled }
  if not Settings.GetBoolean('show-incoming-items') and (FProcessingStatus = psStarted) then
  begin
    DrawFeedList;
    GotoXY(1, 1);
    Exit;
  end;

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
  if not FPrintItems then
  begin
    GoItem;
  end
  else
  begin
    PrintFeedItems
  end;

  FPrintItems := false;
  SetCursorPos;
end;

procedure TTui.LoadSubscriptions;
var
  FeedIndex: word;
begin
  if Subscriptions^.Count > 0 then
  begin
    for FeedIndex := 0 to Subscriptions^.Count - 1 do
    begin
      if FeedIndex = Subscriptions^.Count - 1 then
      begin
        FPrintItems := true;
      end;
      QueueURI(Subscriptions^.GetNth(FeedIndex));
    end;
  end;
end;

procedure TTui.AddDeleteSubscription;
var
  My: String;
begin
  if FItems^.Count > 0 then
  begin
    My := TFeedItem(FItems^.GetNth(FCurrentItem - 1)).MySelf;
    if My <> '' then
    begin
      if Subscriptions^.Count > 0 then
      begin
        if Subscriptions^.IndexOf(My) = -1 then
        begin
          Subscriptions^.Add(My)
        end
        else
        begin
          Subscriptions^.Delete(Subscriptions^.IndexOf(My))
        end
      end
    end
    else
    begin
      HandleMessage(true, NoFeedURL, My)
    end
  end
end;

procedure TTui.PrintFeedItems;
var
  i: PtrUInt;
  Item: TFeedItem;
  Title: String;
begin
  ClrScr;
  GotoXY(1, 1);

  for i := FViewPort.FirstItem + Ord(FScrollingUp) to FViewPort.LastItem  do
  begin
    Item := TFeedItem(FItems^.GetNth(i - 1));
    if Item <> nil then
    begin
      Title := Item.Title;
      PrintTitle(Title, i);
    end
    else
    begin
      HandleMessage(true, NoData, '');
    end;
  end;

  GoItem;
end;

procedure TTui.PrintTitle(Title: String; const Index: PtrUInt);
var
  LenStr: String;
begin
    WriteStr(LenStr, Index);
    if Length(Title) > FDimList.LeftEnd - Length(LenStr) - 3 then
    begin
      Title := Copy(Title, 1, FDimList.LeftEnd - Length(LenStr) - 6) + '...';
    end;

    TextBackground(SkinColorTable.FIndexBack);
    TextColor(SkinColorTable.FIndexFore);
    Write(Index, ': ');

    TextBackground(SkinColorTable.FTitleBack);
    TextColor(SkinColorTable.FTitleFore);
    TuiWriteLn(Title);
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

procedure TTui.DrawUAString;
var
  fw: word;
  ua: String;
begin
  Window(FDimUA.LeftStart, FDimUA.TopStart, FDimUA.LeftEnd, FDimUA.TopEnd);
  TextBackground(SkinColorTable.FUIBack);
  TextColor(SkinColorTable.FUIFore);
  ClrScr;

  ua := UserAgent;
  if Length(ua) < ScreenWidth then
  begin
    fw := ScreenWidth - ((ScreenWidth - Length(ua)) div 2);
  end
  else
  begin
    fw := 1;
    ua := Copy(ua, 1, ScreenWidth - 1);
  end;

  TuiEcho(ua, false, fw);
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
  if FCanAdjustDims then
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

    FDimUA.LeftEnd := ScreenWidth;
    FDimUA.TopEnd := UIHeight;
    FDimUA.LeftStart := Origin;
    FDimUA.TopStart := Origin;

    FViewPort.FirstItem := FCurrentItem + 1;
    FViewPort.LastItem := FDimInfoBar.TopStart - FDimUA.TopEnd - 1;
    FViewPort.PortHeight := FViewPort.LastItem - 1;
  end;

  FCanAdjustDims := true;
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

  DrawStatus;
  DrawUAString;
  ShowHelp;

  FScreenHeight := ScreenHeight;
  FScreenWidth := ScreenWidth;
end;

procedure TTui.Redraw;
var
  iTmp, iTop: word;
begin
  iTmp := FCurrentItem;
  iTop := FViewPort.FirstItem;
  FCurrentItem := iTop;

  if iTop = 0 then
  begin
    iTop := 1
  end;

  Draw;

  if FItems^.Count > 0 then
  begin
    ScrollTo(iTop);
    FPrintItems := true;
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
