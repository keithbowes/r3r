program R3R_OPML;

{$H+}

uses
  Expas, LibIntl, SysUtils;

const
  DefOPML = 'subscriptionList.opml';
  DefText = 'subscriptions.txt';
  Tab = #9;

var
  InName, OutName: String;
  LocaleDir: String;
  Opt: String;

procedure DoExport;
var
  fi, fo: text;
  s: String;

procedure WritePre;
begin
  WriteLn(fo, '<opml version="2.0">');
  WriteLn(fo, Tab + '<head>');
  WriteLn(fo, Tab + Tab + '<title>' + _('Feed List') + '</title>');
  WriteLn(fo, Tab + '</head>');
  WriteLn(fo, Tab + '<body>');
end;

procedure WritePost;
begin
  WriteLn(fo, Tab + '</body>');
  WriteLn(fo, '</opml>');
end;

begin
  if InName = '' then
  begin
    InName := DefText;
  end;

  if OutName = '' then
  begin
    OutName := DefOPML;
  end;

  Assign(fi, InName);
  Assign(fo, OutName);
  Reset(fi);
  Rewrite(fo);

  WritePre;
  while not EOF(fi) do
  begin
    ReadLn(fi, s);
    WriteLn(fo, Tab + Tab + '<outline type="rss" text="' + _('Feed from ') + s + '" xmlUrl="' + s + '"/>');
  end;
  WritePost;

  Close(fo);
  Close(fi);
end;

{$CALLING cdecl}
procedure ElementStarted(user_data: Pointer; name: PChar; attrs: PPChar);
var
  attr, n: String;
  fo: text;
begin
  n := StrPas(name);
  Assign(fo, OutName);
  Append(fo);

  while (Assigned(attrs^)) do
  begin
    attr := StrPas(attrs^);

    if (n = 'outline') and (attr = 'xmlUrl') then
    begin
      WriteLn(fo, (attrs + 1)^);
    end;

    Inc(attrs, 2);
  end;
  
  Close(fo);
end;
{$CALLING default}

procedure DoImport;
var
  fi, fo: text;
  Parser: XML_PARSER;
  s: String;
begin
  if InName = '' then
  begin
    InName := DefOPML;
  end;

  if OutName = '' then
  begin
    OutName := DefText;
  end;

  Parser := XML_ParserCreate(nil);

  XML_SetElementHandler(Parser, ElementStarted, nil);

  Assign(fo, OutName);
  Rewrite(fo);
  Close(fo);

  Assign(fi, InName);
  Reset(fi);

  while not EOF(fi) do
  begin
    ReadLn(fi, s);
    XML_Parse(Parser, {$IFNDEF __GPC__}PChar(s){$ELSE}s{$ENDIF}, Length(s), 0);
  end;

  XML_ParserFree(Parser);

  Close(fi);
end;

procedure ShowUsage;
begin
  WriteLn(_('Usage: r3r_opml option [source [destination]]'));
  WriteLn(_('Option is one of:'));
  WriteLn(Tab + _('0: export subscriptions to an OPML file'));
  WriteLn(Tab + _('1: import subscriptions from an OPML file'));
  WriteLn;
end;

begin
  LocaleDir := ExpandFileName(ExtractFileDir(ParamStr(0)) + '/../share/locale');
  setlocale(LC_ALL, '');
  textdomain('r3r_opml');
  bindtextdomain('r3r_opml', {$IFNDEF __GPC__}PChar(LocaleDir){$ELSE}LocaleDir{$ENDIF});

  Opt := ParamStr(1);
  InName := ParamStr(2);
  OutName := ParamStr(3);

  if Opt = '0' then
  begin
    DoExport;
  end
  else if Opt = '1' then
  begin
    DoImport;
  end
  else
  begin
    ShowUsage;
  end;
end.