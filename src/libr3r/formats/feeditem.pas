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
    function TitleText: String;
    procedure Clear;
  end;

function CreateEmailRecord(EmailStr: String; const Delim: String; const OffsetEnd: word): TAuthor;

implementation

uses
  SysUtils;

var
  AlreadyAllocated: Boolean = false;

function StripHtml(const InStr: String): String;
var
  EntNum: integer;
  EntStr: String;
  ErrPos: byte;
  i: cardinal;
  InEnt, InHTML: Boolean;
  OutStr: String;
begin
  EntStr := '';
  OutStr := '';

  InEnt := false;
  InHTML := false;

  for i := 1 to Length(InStr) do
  begin
    if InStr[i] = '<' then
    begin
      InHTML := true;
    end
    else if not InHTML and (InStr[i] = '&') and
      { if the next character is whitespace, it's not an entity }
      not (InStr[i + 1] in [#0, #8, #9, #10, #13, #32]) then
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
    else if InEnt  then
    begin
      if InStr[i] <> ';' then
      begin
        EntStr := EntStr + InStr[i];
      end
      else
      begin
        if EntStr = 'amp' then
        begin
          EntStr := '&';
        end
        else if EntStr = 'gt' then
        begin
          EntStr := '>';
        end
        else if EntStr = 'lt' then
        begin
          EntStr := '<';
        end
        else if EntStr = 'quot' then
        begin
          EntStr := '"';
        end
        else if Copy(EntStr, 1, 1) = '#' then { numerical entity }
        begin
          Val(Copy(EntStr, 2, Length(EntStr) - 1), EntNum, ErrPos);
          if ErrPos = 0 then
          begin
            EntStr := Chr(EntNum)
          end
          else
          begin
            EntStr := #164 { random character for malformed numerical entity }
          end
        end
        else { undefined entity, per the XML spec }
        begin
          EntStr := #164; { random character for unknown named references }
        end;

        OutStr :=  OutStr + EntStr; { random character for replacing numerical and named references }
        EntStr := '';
        InEnt := false;
      end;
    end
  end;

  StripHtml := OutStr;
end;

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

  Finished := false;

  Links^.Add(nil);
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
begin
  DescriptionText := StripHtml(Description);
end;

{ Get the textual representation (without HTML) of the title. }
function TFeedItem.TitleText: String;
begin
  TitleText := StripHtml(Title);
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
