unit classic; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Menus,
  ComCtrls, Buttons, StdCtrls, ClassicStrings, LibR3R, Info;

type

  { TR3RForm }

  TR3RForm = class(TForm)
    CloseItem: TMenuItem;
    InfoItem: TMenuItem;
    SettingsItem: TMenuItem;
    OpenItem: TMenuItem;
    UriEdit: TEdit;
    GoBtn: TButton;
    FeedList: TListView;
    DescBox: TGroupBox;
    FileMenu: TMenuItem;
    ToolsMenu: TMenuItem;
    HelpMenu: TMenuItem;
    R3RMenu: TMainMenu;
    StatusBar: TStatusBar;
    procedure Created(Sender: TObject);
    procedure InfoMenuClick(Sender: TObject);
    procedure OpenItemClick(Sender: TObject);
    procedure CloseItemClick(Sender: TObject);
  private
    { private declarations }
    procedure ParseFeed(const Uri: String);
    procedure ItemParsed(Item: TParsedFeedItem);
  public
    { public declarations }
  end; 

var
  R3RForm: TR3RForm;

implementation

{ TR3RForm }

procedure TR3RForm.Created(Sender: TObject);
begin
  FileMenu.Caption := FileMnuCap;
    OpenItem.Caption := OpenItmCap;
    CloseItem.Caption := CloseItmCap;
  ToolsMenu.Caption := ToolsMnuCap;
    SettingsItem.Caption := SettingsItmCap;
  HelpMenu.Caption := HelpMnuCap;
    InfoItem.Caption := InfoItmCap;

  FeedList.Columns[0].Width := 100;
  FeedList.Columns[0].Caption := NameCol;

  FeedList.Columns[1].Width := 200;
  FeedList.Columns[1].Caption := TitleCol;

  FeedList.Columns[2].Width := 67;
  FeedList.Columns[2].Caption := SubjCol;

  FeedList.Columns[3].Caption := CreatedCol;

  GoBtn.Caption := Go;
end;

procedure TR3RForm.InfoMenuClick(Sender: TObject);
begin
  ShowMessage('R3R ' + Version + ' (' + Os + ')');
end;

procedure TR3RForm.OpenItemClick(Sender: TObject);
begin
  { TODO:  File dialog }
  ParseFeed('');
end;

procedure TR3RForm.CloseItemClick(Sender: TObject);
begin
  Close;
end;

procedure TR3RForm.ParseFeed(const Uri: String);
var
  Lib: TLibR3R;
begin
  Lib := TLibR3R.Create(Uri);
  Lib.OnItemParsed := @ItemParsed;
  Lib.Parse;
  Lib.Free;
end;

procedure TR3RForm.ItemParsed(Item: TParsedFeedItem);
begin
  { TODO }
end;

initialization
  {$I classic.lrs}

end.

