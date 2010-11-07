unit FeedItem;

{$H+}

interface

uses
  RList, StrTok;

type
  PAuthor = ^TAuthor;
  TAuthor = record
    Email: String;
    Name: String;
    URI: String;
  end;

  TEnclosure = record
    MimeType: String;
    URL: String;
  end;

  TFeedItem = class
  protected
    procedure Cleanup;
  public
    Title: String;
    Links: PRList;
    MainLink: String;
    Description: String;
    Subject: String;
    Created: String;
    Contact: PAuthor;
    Generator: String;
    LastModified: String;
    Language: String;
    Id: String;
    Copyright: String;
    Uri: String;
    Myself: String;
    Enclosure: TEnclosure;

    Finished: Boolean;

    constructor Create;
    destructor Destroy; {$IFNDEF __GPC__}override;{$ENDIF}
    function LinksCount: cardinal;
    function GetMainLink: String;
    function GetPodcast: String;
    function DescriptionText: String;
    procedure Clear;
  end;

function CreateEmailRecord(EmailStr: String; const Delim: String; const OffsetEnd: word): TAuthor;

implementation

uses
  SysUtils;

var
  AlreadyAllocated: Boolean = false;

constructor TFeedItem.Create;
begin
{$IFNDEF __GPC__}
  inherited Create;
{$ENDIF}

  New(Contact);
  Finished := false;

  if not AlreadyAllocated then
  begin
    New(Links, Init);
    AlreadyAllocated := true;
  end;
end;

destructor TFeedItem.Destroy;
begin
  Dispose(Contact);
  Cleanup;

{$IFNDEF __GPC__}
  inherited Destroy;
{$ENDIF}
end;

procedure TFeedItem.Cleanup;
begin
  if AlreadyAllocated then
  begin
    if Links <> nil then
    begin
      Links^.Add(nil); // Hack: Have at least one node to destroy
      Dispose(Links, Done);
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
  Contact^.Email := '';
  Contact^.Name := '';
  Contact^.URI := '';
  Generator := '';
  LastModified := '';
  Language := '';
  Copyright := '';
  Id := '';
  Uri := '';
  Myself := '';
  Enclosure.MimeType := '';
  Enclosure.URL := '';

  Links^.Add(nil);

  Finished := false;
end;

function TFeedItem.LinksCount: cardinal;
begin
  LinksCount := Links^.Count;
end;

function TFeedItem.GetMainLink: String;
var
  CurrentLink: cardinal;
  Link: String;
begin
  CurrentLink := LinksCount;
  if (Links <> nil) and (LinksCount > 0) then
  begin
    repeat
      Link := StrPas(Links^.GetNth(CurrentLink));
      Dec(CurrentLink);
    until (Link <> '') or (CurrentLink <= LinksCount);
  end
  else
  begin
    Link := '';
  end;

  if Link <> '' then
  begin
    MainLink := Link;
  end;

  GetMainLink := MainLink;
end;

function TFeedItem.GetPodcast: String;
begin
  if (Pos('audio', Enclosure.MimeType) = 1) or
    (Pos('video', Enclosure.MimeType) = 1) then
  begin
    GetPodcast := Enclosure.URL;
  end
  else
  begin
    GetPodcast := '';
  end;
end;

{ Get the textual representation (without HTML) of the description. }
function TFeedItem.DescriptionText: String;
var
  i: cardinal;
  InEnt, InHTML: Boolean;
  InStr, OutStr: String;
begin
  InEnt := false;
  InHTML := false;

  InStr := Description;
  OutStr := '';

  for i := 0 to Length(InStr) - 1 do
  begin
    if InStr[i] = '<' then
    begin
      InHTML := true;
    end
    else if InStr[i] = '&' then
    begin
      InEnt := true;
    end
    { This has to go here so that > and ; don't get counted
      as part of the text. }
    else if (not InEnt) and (not InHTML) then
    begin
      OutStr := OutStr + InStr[i];
    end
    else if InStr[i] = '>' then
    begin
      InHTML := false;
    end
    else if InEnt and (InStr[i] = ';') then { we don't want to convert all semicolons to random characters }
    begin
      InEnt := false;
      OutStr :=  OutStr + #164; { random character for replacing numerical and named references }
    end;
  end;

  DescriptionText := OutStr;
end;

function CreateEmailRecord(EmailStr: String; const Delim: String; const OffsetEnd: word): TAuthor;
var
  BegName: cardinal;
  Rec: TAuthor;
begin
  EmailStr := Trim(EmailStr);

  BegName := Pos(Delim, EmailStr);
  with Rec do
  begin
    if BegName <> 0 then
    begin
      Email := Copy(EmailStr, 1, BegName - OffsetEnd - 1);
      Name := Copy(EmailStr, BegName + 1, Length(EmailStr) - BegName - OffsetEnd);
    end
    else
    begin
      Email := EmailStr;
    end;
  end;

  CreateEmailRecord := Rec;
end;

end.
