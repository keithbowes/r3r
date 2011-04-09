unit RFilter;

interface

uses
  FeedItem;

procedure FilterItem(var Item: TFeedItem);

implementation

uses
{$IFDEF USE_PCRE}
  PCRE, RStrings,
{$ENDIF}
  RSettings_Routines, SysUtils;

procedure FilterField(var Item: TFeedItem; const FieldName: String);
var
  AFile: text;
  CurDir: String;
  EntryFile: String;
  FieldValue: String;
  FilterEntry: String;
{$IFDEF USE_PCRE}
  errptr: PChar;
  erroffset: integer;
  rc: integer;
  re: ppcre;
{$ENDIF}
begin
  CurDir := GetCurrentDir;
  ChDir(DataDir);

  EntryFile := FieldName + '.filter';
  if FileExists(EntryFile) then
  begin
    Assign(AFile, EntryFile);
    Reset(AFile);
    while not Eof(AFile) do
    begin
      ReadLn(AFile, FilterEntry);
      if FieldName = 'title' then
      begin
        FieldValue := Item.Title;
      end
      else if FieldName = 'description' then
      begin
        FieldValue := Item.Description;
      end
      else if FieldName = 'subject' then
      begin
        FieldValue := Item.Subject;
      end
      else if FieldName = 'author' then
      begin
        FieldValue := Item.Contact.Name;
      end;

{$IFDEF USE_PCRE}
      re := pcre_compile(StrToPChar(FilterEntry), PCRE_CASELESS, @errptr, @erroffset, 0);
      rc := pcre_exec(re, nil, StrToPChar(FieldValue), Length(FieldValue), 0, 0, nil, 0);
      if rc >= 0 then
{$ELSE}
      if Pos(LowerCase(FilterEntry), LowerCase(FieldValue)) <> 0 then
{$ENDIF}
      begin
        Item.Clear;
        Break;
      end;
{$IFDEF USE_PCRE}
      pcre_free(re);
{$ENDIF}
    end;

    Close(AFile);
  end;

  ChDir(CurDir);
end;

procedure FilterItem(var Item: TFeedItem);
begin
  FilterField(Item, 'title');
  FilterField(Item, 'description');
  FilterField(Item, 'subject');
  FilterField(Item, 'author');
end;

end.
