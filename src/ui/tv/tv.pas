unit TV;

interface

uses
  App, Editors, Dialogs, Drivers, LibR3R, SysUtils, Objects;

type
  TTVLib = class;

  PTVApp = ^TTVApp;
  TTVApp = object(TApplication)
  private
    FColl: PStrCollection;
    FCategories, FCreated: PInputLine;
    FContentHeight, FContentWidth, FHeight, FTop: integer;
    FDescriptionBox: PMemo;
    FFirstItem: Boolean;
    FGoButton: PButton;
    FGoLine: PInputLine;
    FItems: array of TFeedItem;
    FLib: TTVLib;
    FLinkButton: PButton;
    FList: PListBox;
    FRect: TRect;
  public
    constructor Init;
    destructor Done; virtual;
    procedure Open;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure InitMenuBar; virtual;
    procedure InitStatusLine; virtual;
  end;

  TTVLib = class(TLibR3R)
  public
    AApp: TTVApp;
    ATitle: String;
    procedure DisplayItem(const Item: TFeedItem); override;
  end;

implementation

uses
  FvConsts, Info, Menus, MsgBox, StdDlg, TvStrings, Views;

const
  cmFeedsSettings = 1000;
  cmGoDown = 1001;
  cmHttpSettings = 1002;
  cmLinkDown = 1003;
  cmProgramSettings = 1004;

  sfEnabled = sfVisible or sfActive;

procedure TTVLib.DisplayItem(const Item: TFeedItem);
var
  Items: cardinal;
  Pad: String;
begin
  with AApp do
  begin
    Items := Length(FItems);
    SetLength(FItems, Items + 1);
    FItems[Items] := Item;
  
    if not FFirstItem then
    begin
      Pad := '    '
    end;

    FList^.Insert(NewStr(Pad + Item.Title));
    FList^.DrawView;

    FFirstItem := false;
  end;
end;

constructor TTVApp.Init;
var
  Idx: cardinal;
  MainWin: PWindow;
  ScrollBar: PScrollBar;
  WidgetWidth: byte;
begin
  inherited Init;

  GetExtent(FRect);
  FContentHeight := FHeight - FTop;
  FContentWidth := FRect.B.X - FRect.A.X;
  WidgetWidth := Trunc(FContentWidth * 0.4);

  FRect.Assign(FRect.A.X, FRect.A.Y, FContentWidth, FContentHeight);

  New(MainWin, Init(FRect, 'R3R', wnNoNumber));
  InsertWindow(MainWin);

  FRect.Assign(2, FContentHeight - 3, WidgetWidth, FContentHeight - 2);
  ScrollBar := New(PScrollBar, Init(FRect));
  Insert(ScrollBar);

  FRect.Assign(2, 2, WidgetWidth, FContentHeight - 3);
  FList := New(PListBox, Init(FRect, 1, nil));
  Insert(FList);

  FColl := New(PStrCollection, Init(10, 10));
  FList^.NewList(FColl);

  FRect.Assign(WidgetWidth + 1, 2, FContentWidth - 1, 3);
  FCreated := New(PInputLine, Init(FRect, WidgetWidth));
  Insert(FCreated);

  FRect.Assign(WidgetWidth + 1, 4, FContentWidth - 1, 5);
  FCategories := New(PInputLine, Init(FRect, WidgetWidth));
  Insert(FCategories);

  FRect.Assign(WidgetWidth + 1, 6, FContentWidth - 1, 7);
  FLinkButton := New(PButton, Init(FRect, '', cmLinkDown, bfNormal));
  FLinkButton^.SetState(sfDisabled, true);
  Insert(FLinkButton);

  FRect.Assign(WidgetWidth + 1, 8, FContentWidth - 1, FContentHeight - 3);
  FDescriptionBox := New(PMemo, Init(FRect, nil, nil, nil, 100));
  FDescriptionBox^.IsReadOnly := true;
  FDescriptionBox^.Word_Wrap := true;
  Insert(FDescriptionBox);

  FRect.Assign(2, FContentHeight - 1, FContentWidth - Length(Go) - 6, FContentHeight);
  FGoLine := New(PInputLine, Init(FRect, FContentWidth - Length(Go) - 7));
  FGoLine^.Data := NewStr('http://');
  Insert(FGoLine);

  FRect.Assign(FContentWidth - Length(Go) - 5, FContentHeight - 1, FContentWidth - 1, FContentHeight);
  FGoButton := New(PButton, Init(FRect, '', cmGoDown, bfNormal or bfLeftJust));
  FGoButton^.Title := NewStr(Go);
  Insert(FGoButton);

  FLib := TTVLib.Create;
  FLib.AApp := Self;

  for Idx := 1 to ParamCount do
  begin
    FFirstItem := true;
    FLib.RetrieveFeed(ParamStr(Idx))
  end
end;

destructor TTVApp.Done;
begin
  Dispose(FColl, Done);
  FList^.FreeItem(FList^.Range);
  FLib.Free;
  inherited Done
end;

procedure TTVApp.HandleEvent(var Event: TEvent);
var
  Categories, CreateDate, Desc, Lnk: PString;
begin
  inherited HandleEvent(Event);

  if Event.What = evCommand then
  begin
    case Event.Command of
      cmGoDown:
      begin
        FLib.RetrieveFeed(FGoLine^.Data^);
      end;
      cmLinkDown:
      begin
        MessageBox('Link Button pressed', nil, 2);
      end;
      cmHelpAbout:
      begin
        MessageBox(#3 + UserAgent, nil, mfInformation or mfOkButton)
      end;
      cmOpen:
      begin
        Open
      end;
      else
      begin
        Exit;
      end;
    end
  end;
  
  if Event.What = evKeyDown then
  begin
    if not Assigned(FList^.GetFocusedItem) then
    begin
      Exit;
    end;

    with FItems[FList^.Focused] do
    begin
      Categories := NewStr(Subject);
      CreateDate := NewStr(Created);
      Desc := NewStr(Description);
      Lnk := NewStr(Link);

      FLinkButton^.SetState(sfDisabled, Link = '');
    end;

    FCategories^.Data := Categories;
    FCategories^.DrawView;

    FCreated^.Data := CreateDate;
    FCreated^.DrawView;

    FDescriptionBox^.Buffer := nil;

    if Desc <> nil then
    begin
      FDescriptionBox^.InsertText(Desc, Length(Desc^), false);
    end;

    FDescriptionBox^.DrawView;

    FLinkButton^.Title := Lnk;
    FLinkButton^.DrawView;

    DisposeStr(Categories);
    DisposeStr(CreateDate);
    DisposeStr(Desc);
    DisposeStr(Lnk);
    DrawView;
  end;

  ClearEvent(Event)
end;

procedure TTVApp.InitMenuBar;
  function CreateHelpMenu: PMenuItem;
  begin
    Result := NewSubMenu(Help, hcNoContext, NewMenu(NewItem(About, AboutShortcut, Str2Key(AboutKey), cmHelpAbout, hcHelpAbout, nil)), nil)
  end;

  function CreateToolsMenu: PMenuItem;
    function CreateSettingsSubmenu: PMenuItem;
    begin
      Result := NewSubMenu(Settings, hcNoContext, NewMenu(NewItem(Feeds, FeedsShortcut, Str2Key(FeedsKey), cmFeedsSettings, hcNoContext, NewItem(HTTP, HTTPShortcut, Str2Key(HTTPKey), cmHttpSettings, hcNoContext, NewItem(Programs, ProgramsShortcut, Str2Key(ProgramsKey), cmProgramSettings, hcNoContext, nil)))), nil)
    end;
  begin
    Result := NewSubMenu(Tools, hcNoContext, NewMenu(CreateSettingsSubmenu), CreateHelpMenu)
  end;

  function CreateFileMenu: PMenuItem;
  begin
    Result := NewSubMenu(FileMenu, hcNoContext, NewMenu(NewItem(OpenItem,
      OpenShortcut, Str2Key(OpenKey), cmOpen, hcOpen, NewLine(NewItem(Quit, QuitShortcut,
        Str2Key(QuitKey), cmQuit, hcNoContext, nil)))), CreateToolsMenu)
  end;
begin
  GetExtent(FRect);
  FRect.B.Y := FRect.A.Y + 1;
  FTop := FRect.B.Y;

  MenuBar := New(PMenuBar, Init(FRect, NewMenu(CreateFileMenu)))
end;

procedure TTVApp.InitStatusLine;
  function StatusFactory(const AShortcut: String; ALabel: String): String;
  var
    Tilde: byte;
  begin
    repeat
      Tilde := Pos('~', ALabel);
      System.Delete(ALabel, Tilde, 1);
    until Tilde = 0;

    StatusFactory := '~' + AShortcut + '~ ' + ALabel;
  end;

begin
  GetExtent(FRect);
  FRect.A.Y := FRect.B.Y - 1;
  FHeight := FRect.A.Y;
  New(StatusLine, Init(FRect, NewStatusDef(0, $EFFF,
    NewStatusKey(StatusFactory(AboutShortcut, About), Str2Key(AboutKey), cmHelpAbout,
    NewStatusKey(StatusFactory(OpenShortcut, OpenItem), Str2Key(OpenKey), cmOpen,
    NewStatusKey(StatusFactory(QuitShortcut, Quit), Str2Key(QuitKey), cmQuit, nil))),
    nil)))
end;

procedure TTVApp.Open;
var
  Dialog: PFileDialog;
  FileName: FNameStr;
begin
  FileName := '*.*';
  New(Dialog, Init(FileName, OpenFile, TheFileName, fdOpenButton, 1));

  if ExecuteDialog(Dialog, @FileName) <> cmCancel then
  begin
    FFirstItem := true;
    FLib.RetrieveFeed(FileName)
  end
end;

end.
