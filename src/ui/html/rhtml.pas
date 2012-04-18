unit RHtml;

interface

uses
  LibR3R;

type
  TRHtml = class(TLibR3R)
  public
    constructor Create; {$IFDEF __GPC__}override;{$ENDIF}
    destructor Destroy; override;
  end;

implementation

uses
  Info, SysUtils;

const
  HTMLProlog = '<!DOCTYPE html PUBLIC "-//W3C//DTD//HTML 4.01//EN"'#13#10'    "http://www.w3.org/TR/html4/strict.dtd">';
  HTMLHeader = #13#10'<html>'#13#10#9'<head>'#13#10#9#9'<title>R3R</title>'#13#10#9#9'<meta http-equiv="Content-Type" content="text/html; charset=utf-8">'#13#10#9#9'<meta name="generator" content="%ua">'#13#10#9'</head>'#13#10#9'<body>'#13#10#9#9'<ul>';
  HTMLFooter = #13#10#9#9'</ul>'#13#10#9'</body>'#13#10'</html>'#10;
  PreElement = #13#10#9#9#9'<li>';
  PostElement = '</li>';
  Sep = ' | ';
  Sep2 = ':';
  DescPre = ': ';

procedure RetrieveItem(const Item: TFeedItem);
begin
  Write(PreElement);

  with Item do
  begin
    if Link <> '' then
    begin
      Write('<a href="', Link, '">', Title, '</a>');
    end
    else
    begin
      Write(Title);
    end;

    Write(Sep);

    if Subject <> '' then
    begin
      Write('(', Subject, ')');
    end;

    Write(Sep);

    if Created <> '' then
    begin
      Write(Created);
    end;

    if LastModified <> '' then
    begin
      Write(Sep2);
      Write(LastModified);
    end;

    if Description <> '' then
    begin
      Write(DescPre);

      if Length(Description) < 80 then
      begin
        Write(Description);
      end
      else
      begin
        Write(Copy(Description, 1, 77), '...');
      end;
    end;

    WriteLn(PostElement);
  end;
end;

constructor TRHtml.Create;
var
  i: cardinal;
begin
  inherited Create;
  RegisterItemCallback(RetrieveItem);

  WriteLn(HTMLProlog);
  WriteLn(StringReplace(HTMLHeader, '%ua', UserAgent, [rfReplaceAll]));

  if Settings.GetBoolean('load-subscriptions-on-startup') then
  begin
    for i := 0 to Subscriptions^.Count - 1 do
    begin
      RetrieveFeed(Subscriptions^.GetNth(i));
    end;
  end;

  if ParamCount > 0 then
  begin
    for i := 1 to ParamCount do
    begin
      RetrieveFeed(ParamStr(i));
    end;
  end;
end;

destructor TRHtml.Destroy;
begin
  WriteLn(HTMLFooter);

{$IFNDEF __GPC__}
  inherited Destroy;
{$ENDIF}
end;

end.
