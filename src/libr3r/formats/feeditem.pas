unit FeedItem;

{$H+}

interface

uses
  RList;

type
  TFeedItemLink = array of String;

  PEmail = ^TEmail;
  TEmail = record
    Toee: String;
    Address: String;
  end;

  TFeedItem = class
  private
    FTitle: String;
    FLinks: PRList;
    FMainLink: String;
    FDescription: String;
    FSubject: String;
    FCreated: String;
    FContact: PEmail;
    FGenerator: String;
    FLastModified: String;
    FLanguage: String;
    FCopyright: String;
    FId: String;
    FUri: String;
    FMyself: String;

    function GetLinksCount: cardinal;
    function GetMainLink: String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Cleanup;
    property Title: String read FTitle write FTitle;
    property Links: PRList read FLinks write FLinks;
    property LinksCount: cardinal read GetLinksCount;
    property MainLink: String read GetMainLink write FMainLink;
    property Description: String read FDescription write FDescription;
    property Subject: String read FSubject write FSubject;
    property Created: String read FCreated write FCreated;
    property Contact: PEmail read FContact write FContact;
    property Generator: String read FGenerator write FGenerator;
    property LastModified: String read FLastModified write FLastModified;
    property Language: String read FLanguage write FLanguage;
    property Id: String read FId write FId;
    property Copyright: String read FCopyright write FCopyright;
    property Uri: String read FUri write FUri;
    property Myself: String read FMyself write FMyself;

    procedure Clear;
  end;

function CreateEmailRecord(EmailStr: String; const Delim: String; const OffsetEnd: word): TEmail;

implementation

uses
  SysUtils;

var
  AlreadyAllocated: Boolean = false;

constructor TFeedItem.Create;
begin
  inherited Create;
  New(FContact);

  if not AlreadyAllocated then
  begin
    New(FLinks, Init);
    AlreadyAllocated := true;
  end;
end;

destructor TFeedItem.Destroy;
begin
  Dispose(FContact);
  Cleanup;
  inherited Destroy;
end;

procedure TFeedItem.Cleanup;
begin
  if AlreadyAllocated then
  begin
    if FLinks <> nil then
    begin
      Links^.Add(nil); // Hack: Have at least one node to destroy
      Dispose(FLinks, Done);
      AlreadyAllocated := false;
    end;
  end;
end;

procedure TFeedItem.Clear;
begin
  Title := '';
  Description := '';
  MainLink := '';
  Subject := '';
  Created := '';
  Contact^.Toee := '';
  Contact^.Address := '';
  Generator := '';
  LastModified := '';
  Language := '';
  Copyright := '';
  Id := '';
  Uri := '';
  Myself := '';

  if Links^.Count > 0 then
  begin
    Links^.Clear;
  end;
end;

function TFeedItem.GetLinksCount: cardinal;
begin
  Result := Links^.Count;
end;

function TFeedItem.GetMainLink: String;
var
  CurrentLink: cardinal;
  Link: String;
begin
  CurrentLink := 0;
  if (Links <> nil) and (LinksCount > 0) then
  begin
    repeat
      Link := StrPas(Links^.GetNth(CurrentLink));
      Inc(CurrentLink);
    until (Link <> '') or (CurrentLink = LinksCount);
  end
  else
  begin
    Link := '';
  end;

  if Link <> '' then
  begin
    FMainLink := Link;
  end;

  Result := FMainLink;
end;

function CreateEmailRecord(EmailStr: String; const Delim: String; const OffsetEnd: word): TEmail;
var
  BegName: cardinal;
begin
  EmailStr := Trim(EmailStr);

  BegName := Pos(Delim, EmailStr);
  with Result do
  begin
    if BegName <> 0 then
    begin
      Address := Copy(EmailStr, 1, BegName - OffsetEnd - 1);
      Toee := Copy(EmailStr, BegName + 1, Length(EmailStr) - BegName - OffsetEnd);
    end
    else
    begin
      Address := EmailStr;
    end;
  end;
end;

end.
