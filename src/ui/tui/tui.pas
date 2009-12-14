unit Tui;

interface

uses
  LibR3R;

type
  TTui = class(TLibR3R)
  private
    FItems: array of TFeedItem;
  protected
    procedure DisplayItem(const Item: TFeedItem); override;
    procedure HandleMessage(Sender: TObject; Error: Boolean; MessageName, Extra: String); override;
    procedure NotifyUpdate; override;
    procedure ShowHelp;
    procedure GoURI;
    procedure GoItem;
    procedure GoLink;
    procedure OpenBrowser(const Link: String);
    procedure SetOptions;
    procedure GoDonate;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  Crt, Dos, Info, RList, TuiStrings;

constructor TTui.Create;
var
  FeedIndex: word;
  KeyChar: char;
begin
  inherited Create;
  FreeLinks := false;

  SetLength(FItems, 1);

  for FeedIndex := 1 to ParamCount do
  begin
    RetrieveFeed(ParamStr(FeedIndex));
  end;

  if Settings.GetBoolean(Settings.IndexOf('load-subscriptions-on-startup')) then
  begin
    for FeedIndex := 0 to Subscriptions^.Count - 1 do
    begin
      RetrieveFeed(Subscriptions^.GetNth(FeedIndex));
    end;
  end;

  ShowHelp;

  repeat
    KeyChar := ReadKey;

    case Pos(KeyChar, GoKey + OptionsKey + AboutKey + OpenKey + DonateKey) of
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
    end;
  until KeyChar = QuitKey;
end;

destructor TTui.Destroy;
var
  i, Items: cardinal;
begin
  Items := Length(FItems);

  for i := 0 to Items - 1 do
  begin
    FItems[i].Free;
  end;

  inherited Destroy;
end;

procedure TTui.DisplayItem(const Item: TFeedItem);
var
  Items: cardinal;
begin
  Items := Length(FItems);
  SetLength(FItems, Items + 1);

  FItems[Items] := TFeedItem.Create;
  FItems[Items].Title := Item.Title;
  FItems[Items].Subject := Item.Subject;
  FItems[Items].Created := Item.Created;
  FItems[Items].Description := Item.Description;
  FItems[Items].MainLink := Item.MainLink;

  Write(Items, ': ', Item.Title);

  if Item.LinksCount > 0 then
  begin
    Write(' <', Item.MainLink, '>');
  end;

  if Item.Contact^.Address <> '' then
  begin
    Write(' (', Item.Contact^.Address, ')');
  end;

  WriteLn;
end;

procedure TTui.HandleMessage(Sender: TObject; Error: Boolean; MessageName, Extra: String);
begin
  if Error then
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

  WriteLn;
end;

procedure TTui.NotifyUpdate;
begin
  WriteLn(UpdateAvailable);
end;

procedure TTui.ShowHelp;
begin
  WriteLn(UserAgent:75);

  Write(GoURL);
  Write(Options:25 - Length(GoUrl) + Length(Options));
  WriteLn;
  if Length(FItems) > 1 then
  begin
    Write(AboutItem);
    Write(OpenLink:25 - Length(AboutItem) + Length(OpenLink));
    WriteLn;
  end;
  Write(Donate);
  Write(Quit:25 - Length(Donate) + Length(Quit));
  WriteLn;
end;

procedure TTui.GoURI;
var
  URI: String;
begin
  Write(Feed);
  ReadLn(URI);

  RetrieveFeed(URI);
  ShowHelp;
end;

procedure TTui.GoItem;
var
  ErrPos: word;
  iNo: cardinal;
  No: String;
begin
  Write(ItemNo);
  ReadLn(No);

  Val(No, iNo, ErrPos);

  if ErrPos = 0 then
  begin
    with FItems[iNo] do
    begin
      if Length(Description) > 75 then
      begin
        Description := Copy(Description, 1, 75) + '...';
      end;

      WriteLn(ItemTitle, Title);
      WriteLn(ItemSubject, Subject);
      WriteLn(ItemCreated, Created);
      WriteLn(ItemDesc, Description);
      WriteLn(ItemLink + MainLink);
    end;
  end
  else
  begin
    WriteLn(InvalidNumber);
  end;

  ShowHelp;
end;

procedure TTui.GoLink;
var
  ErrPos: byte;
  iNo: cardinal;
  No: String;
begin
  Write(LinkNo);
  ReadLn(No);

  Val(No, iNo, ErrPos);

  if (ErrPos = 0) and (FItems[iNo].MainLink <> '') and
    (iNo < cardinal(Length(FItems))) then
  begin
    OpenBrowser(FItems[iNo].MainLink);
  end;

  ShowHelp;
end;

procedure TTui.OpenBrowser(const Link: String);
var
  Browser: String;
begin
  Browser := Settings.GetString(Settings.IndexOf('browser'));

  SwapVectors;
  Exec(Browser, Link);
  SwapVectors;
end;

procedure TTui.SetOptions;
const
  Width = 30;
var
  i, ErrPos, Len: byte;
  HBound, Index, SetInt: integer;
  SetBool: Boolean;
  SetName, SetVal: ShortString;
  SRec: PRList;
begin
  WriteLn;
  WriteLn(OptionName, OptionVal:Width - Length(OptionName) div 2 + 3);
  WriteLn;

  with Settings do
  begin
    Enumerate(SRec, HBound);

    for i := 0 to HBound - 1 do
    begin
      Write(TRSetting(SRec^.GetNth(i)^).Name);

      case TRSetting(SRec^.GetNth(i)^).ValueType of
        TypeString:
        begin
          Write(TRSetting(SRec^.GetNth(i)^).ValueString:Width - Length(TRSetting(SRec^.GetNth(i)^).Name) + Length(TRSetting(SRec^.GetNth(i)^).ValueString));
        end;
        TypeInteger:
        begin
          Index := TRSetting(SRec^.GetNth(i)^).ValueInteger;
          Len := 0;

          while Index > 0 do
          begin
            Inc(Len);
            Index := Index div 10;
          end;

          Write(TRSetting(SRec^.GetNth(i)^).ValueInteger:Width - Length(TRSetting(SRec^.GetNth(i)^).Name) + Len);
        end;
        TypeBoolean:
        begin
          Write(TRSetting(SRec^.GetNth(i)^).ValueBoolean:Width - Length(TRSetting(SRec^.GetNth(i)^).Name) + 5);
        end;
      end;

      WriteLn;
    end;

    repeat
      Write(SettingToChange);
      ReadLn(SetName);

      Index := IndexOf(SetName);
      Len := Length(SetName);

      if (Len > 1) and (Index <> -1) then
      begin
        Write(NewValue);
        ReadLn(SetVal);

        case TRSetting(SRec^.GetNth(Index)^).ValueType of
          TypeString:
          begin
            SetString(Index, SetVal);
          end;
          TypeInteger:
          begin
            Val(SetVal, SetInt, ErrPos);
            SetInteger(Index, SetInt);
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

    ShowHelp;
  end;
end;

procedure TTui.GoDonate;
begin
  OpenBrowser('http://sourceforge.net/donate/index.php?group_id=90897');
  ShowHelp;
end;

end.
