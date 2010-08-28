unit Tui;

interface

uses
  LibR3R, RList;

type
  TTui = class(TLibR3R)
  private
    FCurrentItem: cardinal;
    FItems: PRList;
    procedure DrawFeedInfo;
    procedure DrawFeedList;
    procedure DrawInfoBar;
    procedure DrawSeparator;
    procedure DrawStatus;
    procedure DrawUIString;
  protected
    procedure NotifyUpdate; override;
    procedure ShowHelp;
    procedure GoURI;
    procedure QueryItemNumber;
    procedure GoItem;
    procedure GoLink;
    procedure OpenBrowser(Link: String);
    procedure SetOptions;
    procedure GoDonate;
    procedure RetrieveFeed(Resource: String); override;
  public
    constructor Create; {$IFDEF __GPC__}override;{$ENDIF}
    destructor Destroy; override;
    procedure DisplayItem(const Item: TFeedItem); override;
    procedure HandleMessage(IsError: Boolean; MessageName, Extra: String); override;
  end;

implementation

uses
  Crt, Dos, Info, SysUtils, TuiStrings;

function CreateFeedItem: TFeedItem;
begin
  CreateFeedItem := TFeedItem.Create;
end;

constructor TTui.Create;
const
  DownKey = 'j';
  EnterKey = Chr(13);
  UpKey = 'k';
var
  FeedIndex: word;
  KeyChar: char;
begin
  inherited Create;
  New(FItems, Init);
  FCurrentItem := 0;

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

    case Pos(KeyChar, GoKey + OptionsKey + AboutKey + OpenKey + DonateKey + DownKey + UpKey + EnterKey) of
      1:
      begin
        GoURI;
      end;
      2:
      begin
        SetOptions;
      end;
      3:
      begin
        QueryItemNumber;
        GoItem;
      end;
      4:
      begin
        GoLink;
      end;
      5:
      begin
        GoDonate;
      end;
      6:
      begin
        if FCurrentItem < FItems.Count then
        begin
          Inc(FCurrentItem);
          GoItem;
        end;
      end;
      7:
      begin
        if FCurrentItem > 1 then
        begin
          Dec(FCurrentItem);
          GoItem;
        end;
      end;
      8:
      begin
        if FCurrentItem > 0 then
        begin
          OpenBrowser(TFeedItem(FItems^.GetNth(FCurrentItem - 1)).MainLink);
        end;
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
begin
  AItem := CreateFeedItem;
  AItem.Title := Item.Title;
  AItem.Subject := Item.Subject;
  AItem.Created := Item.Created;
  AItem.Description := Item.Description;
  AItem.Links := Item.Links;
  AItem.MainLink := Item.GetMainLink;
  AItem.Enclosure := Item.Enclosure;
  FItems^.Add(AItem);
  Items := FItems^.Count;

  Write(Items, ': ', Item.Title);

  if Item.LinksCount > 0 then
  begin
    Write(' <', Item.GetMainLink, '>');
  end;

  if Item.Contact^.Email <> '' then
  begin
    Write(' (', Item.Contact^.Email, ')');
  end;

  WriteLn;
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

  if FItems^.Count > 0 then
  begin
    Write(AboutItem);
    Write(OpenLink:25 - Length(AboutItem) + Length(OpenLink));
    WriteLn;
  end;

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
        Write(MainLink, ActivateLink);
      end;

      DrawFeedInfo;
      WriteLn(ItemNo, FCurrentItem, NextPrevLink);
      WriteLn(ItemTitle, Title);
      WriteLn(ItemSubject, Subject);
      WriteLn(ItemCreated, Created);
      WriteLn(ItemDesc, Desc);
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

  ShowHelp;
end;

procedure TTui.GoLink;
var
  ErrPos: byte;
  iNo: cardinal;
  No: String;
begin
  DrawStatus;

  WriteLn;
  Write(LinkNo);
  ReadLn(No);

  Val(No, iNo, ErrPos);

  if (ErrPos = 0) and (TFeedItem(FItems^.GetNth(iNo - 1)).MainLink <> '') and
    (FItems^.Count > 0) and (iNo <= FItems^.Count) then
  begin
    OpenBrowser(TFeedItem(FItems^.GetNth(iNo - 1)).MainLink);
  end
  else
  begin
    WriteLn(InvalidNumber);
  end;

  ShowHelp;
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

procedure TTui.RetrieveFeed(Resource: String);
begin
  DrawFeedList;
  FCurrentItem := FItems^.Count + 1;
  inherited RetrieveFeed(Resource);
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
  Window(1, 2, ScreenWidth div 2 - 1, ScreenHeight - 4);
  TextBackground(Black);
  TextColor(Green);
  GotoXY(1, ScreenHeight - 6);
end;

procedure TTui.DrawInfoBar;
begin
  Window(1, ScreenHeight - 3, ScreenWidth, ScreenHeight - 1);
  TextBackground(Blue);
  TextColor(Yellow);
  ClrScr;
end;

procedure TTui.DrawSeparator;
begin
  Window(ScreenWidth div 2, 2, ScreenWidth div 2, ScreenHeight - 4);
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

end.
