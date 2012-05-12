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
    FCanStart: Boolean;
    FCurrentItem: cardinal;
    FDimInfo: TWindowDim;
    FDimInfoBar: TWindowDim;
    FDimList: TWindowDim;
    FDimSep: TWindowDim;
    FDimStatus: TWindowDim;
    FDimUI: TWindowDim;
    FItems: PRList;
    FPrintItems: Boolean;
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
    function QueryItemNumber: Boolean;
    procedure GoItem;
    procedure OpenBrowser(Link: String);
    procedure OpenEmail(Address: String);
    procedure SetOptions;
    procedure GoDonate;
    procedure ScrollDown;
    procedure ScrollUp;
    procedure ScrollTo(n: word);
    procedure LoadSubscriptions;
    procedure AddDeleteSubscription;
    procedure PrintFeedItems;
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

var
  obj: TTui;

function CreateFeedItem: TFeedItem;
begin
  CreateFeedItem := TFeedItem.Create;
end;

procedure ItemReceived(const Item: TFeedItem);
var
  AItem: TFeedItem;
  Items: PtrUInt;
  LenStr: String;
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
    AItem.MySelf := Item.MySelf;
    AItem.Contact.Email := Item.Contact.Email;
    AItem.Enclosure.MimeType := Item.Enclosure.MimeType;
    AItem.Enclosure.URL := Item.Enclosure.URL;
    FItems^.Add(AItem);
    Items := FItems^.Count;
  
    if Items <= FViewPort.PortHeight then
    begin
      Title := AItem.TitleText;
      WriteStr(LenStr, Items);
      if Length(Title) > (FDimList.LeftEnd - 3 - Length(LenStr)) then
      begin
        Title := Copy(Title, 1, FDimList.LeftEnd - 3 - Length(LenStr)) + '...';
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
  KeyChar: char;
  i: word;
  mc: PMailCap;
  prog: String;
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
  FCurrentItem := 1;
  FPrintItems := false;
  FScrollingUp := false;

  RegisterItemCallback(ItemReceived);
  obj := Self;
  Settings.RegisterInteger('error-seconds', 'Display', 1, ErrorSeconds);

{$IFDEF __GPC__}
  CRTInit;
{$ENDIF __GPC__}

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
      RetrieveFeed(ParamStr(i));
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
    noecho;
    i := getch;
    echo;
    if i = KEY_RESIZE then
    begin
      Redraw;
      Continue;
    end;

    KeyChar := Chr(byte(i));
    if i < 256 then
    begin
      { Case insensitivity a la other programs }
      KeyChar := LowerCase(KeyChar);
    end;
{$ENDIF}

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
      OpenBrowser(GetInstalledPrefix + PathDelim + 'share' + PathDelim +  'doc' + PathDelim + LowerCase(AppName) + PathDelim + 'keys.html');
    end
    else if (KeyChar = GetBoundKey(EnterKey)) or (KeyChar = RightArrow) or (KeyChar = GetBoundKey(RightKey)) then
    begin
      if FItems^.Count > 0 then
      begin
        OpenBrowser(TFeedItem(FItems^.GetNth(FCurrentItem - 1)).Link);
      end;
    end
    else if KeyChar = GetBoundKey(EmailKey) then
    begin
      prog := TFeedItem(FItems^.GetNth(FCurrentItem - 1)).Contact.Email;
      if prog <> '' then
      begin
        OpenEmail(prog);
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
        mc^.ExecProg(mc^.GetProg(TFeedItem(FItems^.GetNth(FCurrentItem - 1)).Enclosure.MimeType), TFeedItem(FItems^.GetNth(FCurrentItem - 1)).Enclosure.URL);
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
    end;
  until KeyChar = GetBoundKey(QuitKey);
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

  if FCanStart then
  begin
    EndWin;
  end;

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

  if IsError then
  begin
    Delay(Settings.GetInteger('error-seconds') * 1000);
  end;
end;

procedure TTui.RetrieveFeed(Resource: String);
begin
  DrawStatus;
  TuiWrite(StringReplace(Retrieving, '%s', Resource, []));

  DrawFeedList;
  ClrScr;
  GoToXY(1, 1);
  inherited RetrieveFeed(Resource);

  if FPrintItems then
  begin
    Redraw;
    FPrintItems := false;
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

function ReplaceKey(const s: String; const c: char): String;
begin
  ReplaceKey := StringReplace(s, '%s', GetBoundKey(c), [rfReplaceAll]);
end;

begin
  Opts := '';

  DrawInfoBar;

  AddOption(ReplaceKey(Quit, 'q'));
  AddOption(ReplaceKey(GoURL, 'g'));
  AddOption(ReplaceKey(Options, 'o'));
  AddOption(ReplaceKey(Donate, 'd'));
  AddOption(ReplaceKey(Help, '?'));

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
    FPrintItems := true;
    RetrieveFeed(URI);
  end;
end;

function TTui.QueryItemNumber: Boolean;
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
  Desc, DescLine: String;
  FreeLines: word;
  ItemText: String;
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

      WriteStr(ItemText, FCurrentItem, StringReplace(StringReplace(NextPrevLink, '%1', GetBoundKey(DownKey), []), '%2', GetBoundKey(UpKey), []));
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

  DrawFeedList;
end;

procedure TTui.OpenBrowser(Link: String);
var
  Browser, Tmp: String;
  Index: byte;
begin
  { Do nothing on empty links }
  if Link <> '' then
  begin
    Browser := Settings.GetString('for:http');
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
    if (Copy(Browser, 1, 1) = '"') and (Copy(Browser, Length(Browser), 1) = '"') then
    begin
      Browser := Copy(Browser, 2, Length(Browser) - 2);
    end;

    SwapVectors;
    Exec(FSearch(Browser, GetEnv('PATH')), Link);
    SwapVectors;
    Redraw;
  end
end;

procedure TTui.OpenEmail(Address: String);
var
  Client, Tmp: String;
  Index: byte;
begin
  Client := Settings.GetString('for:mailto');
  { Check for the default registry association }
  if Pos('%1', Client) <> 0 then
  begin
    { Check for the first command-line switch }
    Index := Pos('-', Client);
    if Index = 0 then
    begin
      Index := Pos('/', Client);
    end;

    if Index = 0 then
    begin
      Index := Pos('%1', Client);
    end;

    Tmp := Copy(Client, Index, Length(Client) - Index + 1);
    Client := Copy(Client, 1, Index - 2);

    Address := StringReplace(Tmp, '%1', Address, [rfReplaceAll]);
  end;

  SwapVectors;
  Exec(FSearch(Client, GetEnv('PATH')), Address);
  SwapVectors;
  Redraw;
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
{$ENDIF}

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

      SetDesc := TruncateString(PRSetting(SRec^.GetNth(i))^.Description);
      TuiConvertCodeset(SetDesc);

      WriteStr(SetDesc, i + 1);
      TuiEcho(SetDesc, false, NumLen);
      TuiWrite('.');

      TextBackground(SkinColorTable.FOptionDescBack);
      TextColor(SkinColorTable.FOptionDescFore);
      TuiEcho(SetDesc, false, NumLen + Length(SetDesc));

      TextBackground(SkinColorTable.FOptionValueBack);
      TextColor(SkinColorTable.FOptionValueFore);

      case PRSetting(SRec^.GetNth(i))^.ValueType of
        TypeString:
        begin
          TuiEcho(PRSetting(SRec^.GetNth(i))^.ValueString, false, Width - Length(UTF8Decode(SetDesc)) + Length(PRSetting(SRec^.GetNth(i))^.ValueString));
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
        InitDesc := StrPas(current_history^.Line);
      end;
{$ENDIF}

      WriteLn;
    end;
  end;
end;

begin
{$IFDEF USE_READLINE}
  rl_callback_handler_install('', read_option_chars);
  Backup := StrToPChar(DataDir + '_options_history');
  write_history(Backup);
  clear_history;
{$ENDIF}

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
{$IFDEF USE_READLINE}
      rl_option_finished := false;
      repeat
        ClrScr;
        TextBackground(SkinColorTable.FOptionPromptBack);
        TextColor(SkinColorTable.FOptionPromptFore);
        TuiWrite(SettingToChangeReadLine);

        TextBackground(SkinColorTable.FOptionIndexBack);
        TextColor(SkinColorTable.FOptionIndexFore);

        if StrLen(rl_line_buffer) > 0 then
        begin
          SetDesc := StrPas(rl_line_buffer);
          TuiWrite(SetDesc);
        end
        else if InitDesc <> '' then
        begin
          SetDesc := InitDesc;
          TuiWrite(SetDesc);
          InitDesc := '';
        end;
        rl_callback_read_char;
      until rl_option_finished;

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
    GoItem;
  end;
  
{$IFDEF USE_READLINE}
  rl_callback_handler_remove;
  clear_history;
  read_history(Backup);
  DeleteFile(StrPas(Backup));
{$ENDIF}
end;

procedure TTui.GoDonate;
begin
  OpenBrowser('http://sourceforge.net/donate/index.php?group_id=90897');
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
    FViewPort.LastItem := FViewPort.LastItem + (FDimList.TopEnd - 1) - (FDimUI.TopEnd + 1);
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

{$IFNDEF USE_NCRT}
  DrawFeedList;
  GotoXY(1, FCurrentItem - FViewPort.FirstItem + Ord(not FScrollingUp));
{$ELSE}
  move(FCurrentItem - FViewPort.FirstItem - 1 + (FDimList.TopStart - FDimUI.TopEnd) + Ord(not FScrollingUp), 0);
{$ENDIF}
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
      RetrieveFeed(Subscriptions^.GetNth(FeedIndex));
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
          Subscriptions^.DeleteIndex(Subscriptions^.IndexOf(My))
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
  i: word;
  Item: TFeedItem;
  LenStr: String;
  Title: String;
begin
  DrawFeedList;
  ClrScr;
  GotoXY(1, 1);

  for i := FViewPort.FirstItem + Ord(FScrollingUp) to FViewPort.LastItem  do
  begin
    Item := FItems^.GetNth(i - 1);
    if Item <> nil then
    begin
      Title := Item.Title;
      WriteStr(LenStr, i);
      if Length(Title) > FDimList.LeftEnd - Length(LenStr) - 3 then
      begin
        Title := Copy(Title, 1, FDimList.LeftEnd - Length(LenStr) - 6) + '...';
      end;

      TextBackground(SkinColorTable.FIndexBack);
      TextColor(SkinColorTable.FIndexFore);
      Write(i, ': ');

      TextBackground(SkinColorTable.FTitleBack);
      TextColor(SkinColorTable.FTitleFore);
      TuiWriteLn(Title);
    end
    else
    begin
      HandleMessage(true, NoData, '');
    end;
  end;

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
