unit Tui;

interface

uses
  LibR3R, RMessage, RList;

type
  TTui = class(TLibR3R)
  private
    FItems: PRList;
  protected
    procedure NotifyUpdate; override;
    procedure ShowHelp;
    procedure GoURI;
    procedure GoItem;
    procedure GoLink;
    procedure OpenBrowser(const Link: String);
    procedure SetOptions;
    procedure GoDonate;
  public
    constructor Create; {$IFDEF __GPC__}override;{$ENDIF}
    destructor Destroy; override;
    procedure DisplayItem(const Item: TFeedItem); override;
    procedure HandleMessage(Sender: TObject; Error: Boolean; MessageName, Extra: String); reintroduce;
  end;

implementation

uses
  Crt, Dos, Info, SysUtils, TuiStrings;

function CreateFeedItem: TFeedItem;
begin
  CreateFeedItem := TFeedItem.Create;
end;

constructor TTui.Create;
var
  FeedIndex: word;
  KeyChar: char;
begin
  inherited Create;
  New(FItems, Init);

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
  i: cardinal;
  p: TFeedItem;
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
  FItems^.Add(AItem);
  Items := FItems^.Count;

  Write(Items, ': ', Item.Title);

  if Item.LinksCount > 0 then
  begin
    Write(' <', Item.GetMainLink, '>');
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
  if FItems^.Count > 1 then
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

  if URI <> '' then
  begin
    RetrieveFeed(URI);
  end;

  ShowHelp;
end;

procedure TTui.GoItem;
var
  Desc: String;
  ErrPos: word;
  i, j, k,  iNo: cardinal;
  No: String;
begin
  Write(ItemNo);
  ReadLn(No);

  Val(No, iNo, ErrPos);

  if ErrPos = 0 then
  begin
    with TFeedItem(FItems^.GetNth(iNo - 1)) do
    begin
      Desc := Description;
      if Length(Desc) > 75 then
      begin
        Desc := Copy(Desc, 1, 75) + '...';
      end;

      WriteLn(ItemTitle, Title);
      WriteLn(ItemSubject, Subject);
      WriteLn(ItemCreated, Created);
      WriteLn(ItemDesc, Desc);

      j := 0;
      k := 0;
      for i := 0 to Links^.Count - 1 do
      begin
        if Links^.GetNth(i) = nil then
        begin
          Inc(j);
          Continue;
        end;

        if j = iNo then
        begin
          Inc(k);
          WriteLn(StringReplace(ItemLink, '%i', IntToStr(k), [rfReplaceAll]),
            StrPas(Links^.GetNth(i)));
        end
        else if j > iNo then
        begin
          Break;
        end;
      end;
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

  if (ErrPos = 0) and (TFeedItem(FItems^.GetNth(iNo - 1)).GetMainLink <> '') and
    (FItems^.Count > 0) then
  begin
    OpenBrowser(TFeedItem(FItems^.GetNth(iNo - 1)).GetMainLink);
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
  SetName, SetVal: String;
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

      if (Len > 1) and (Index <> -1) then
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

    ShowHelp;
  end;
end;

procedure TTui.GoDonate;
begin
  OpenBrowser('http://sourceforge.net/donate/index.php?group_id=90897');
  ShowHelp;
end;

end.
