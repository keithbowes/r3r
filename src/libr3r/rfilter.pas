unit RFilter;

interface

uses
  FeedItem;

procedure FilterItem(var Item: TFeedItem);

implementation

uses
{$IFDEF USE_PCRE}
  PCRE, RStrings,
{$ELSE}
{$IFDEF USE_REGEXPR}
  RegExpr,
{$ENDIF}
{$ENDIF}
  RSettings_Routines, SysUtils;

procedure FilterField(var Item: TFeedItem; const FieldName: String);
var
  AFile: text;
  CurDir: String;
  EntryFile: String;
  FieldValue: String;
  FilterEntry: String;
  IsRegExp, ShouldFilter: Boolean;
{$IFDEF USE_PCRE}
  errptr: PChar;
  erroffset: integer;
  opt: integer;
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

      IsRegExp := (Copy(FilterEntry, 1, 1) = '/') and
        ((Copy(FilterEntry, Length(FilterEntry), 1) = '/') or
        (Copy(FilterEntry, Length(FilterEntry) - 1, 2) = '/i'));
      if IsRegExp then
      begin
{$IFDEF USE_PCRE}
        if Copy(FilterEntry, Length(FilterEntry), 1) = 'i' then
        begin
          Delete(FilterEntry, Length(FilterEntry), 1);
          opt := PCRE_CASELESS
        end
        else
        begin
          opt := 0
        end;
        FilterEntry := Copy(FilterEntry, 2, Length(FilterEntry) - 2);

        re := pcre_compile(StrToPChar(FilterEntry), opt, @errptr, @erroffset, nil);
        rc := pcre_exec(re, nil, StrToPChar(FieldValue), Length(FieldValue), 0, 0, nil, 0);

        ShouldFilter := rc >= 0;
        pcre_free(re);
{$ELSE}
{$IFDEF USE_REGEXPR}
        with TRegExpr.Create do
        begin
          ModifierI := Copy(FilterEntry, Length(FilterEntry), 1) = 'i';
          if ModifierI then
          begin
            Delete(FilterEntry, Length(FilterEntry), 1);
          end;
          FilterEntry := Copy(FilterEntry, 2, Length(FilterEntry) - 2);

          Expression := FilterEntry;
          ShouldFilter := Exec(FieldValue);
          Free;
        end;
{$ELSE}
        ShouldFilter := false;
{$ENDIF}
{$ENDIF}
      end
      else
      begin
        ShouldFilter := Pos(LowerCase(FilterEntry), LowerCase(FieldValue)) <> 0
      end;

      if ShouldFilter then
      begin
        Item.Clear;
        Break;
      end;
    end;

    Close(AFile);
  end;

  Item.Filtered := ShouldFilter;
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
