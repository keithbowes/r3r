unit RHtml;

interface

uses
  LibR3R;

type
  TRHtml = class(TLibR3R)
  private
    FFile: text;
  public
    constructor Create; {$IFDEF __GPC__}override;{$ENDIF}
    destructor Destroy; override;
    procedure DisplayItem(const Item: TFeedItem); override;
  end;

implementation

const
  HTMLHeader = '<!DOCTYPE html PUBLIC "-//W3C//DTD//HTML 4.01//EN"'#13#10'    "http://www.w3.org/TR/html4/strict.dtd">'#13#10'<html>'#13#10#9'<head>'#13#10#9#9'<title>R3R</title>'#13#10#9#9'<meta http-equiv="Content-Type" content="text/html; charset=utf-8">'#13#10#9'</head>'#13#10#9'<body>'#13#10#9#9'<ul>';
  HTMLFooter = #13#10#9#9'</ul>'#13#10#9'</body>'#13#10'</html>'#10;
  PreElement = #13#10#9#9#9'<li>';
  PostElement = '</li>';
  Sep = ' | ';
  Sep2 = ':';
  DescPre = ': ';

constructor TRHtml.Create;
var
  i: cardinal;
begin
  inherited Create;
  Assign(FFile, 'r3r.html');
  Rewrite(FFile);

  WriteLn(FFile, HTMLHeader);

  for i := 0 to Subscriptions^.Count - 1 do
  begin
    RetrieveFeed(Subscriptions^.GetNth(i));
  end;
end;

destructor TRHtml.Destroy;
begin
  WriteLn(FFile, HTMLFooter);
  Close(FFile);

{$IFNDEF __GPC__}
  inherited Destroy;
{$ENDIF}
end;

procedure TRHtml.DisplayItem(const Item: TFeedItem);
begin
  Write(FFile, PreElement);

  with Item do
  begin
    if GetMainLink <> '' then
    begin
      Write(FFile, '<a href="', GetMainLink, '">', Title, '</a>');
    end
    else
    begin
      Write(FFile, Title);
    end;

    Write(FFile, Sep);

    if Subject <> '' then
    begin
      Write(FFile, '(', Subject, ')');
    end;

    Write(FFile, Sep);

    if Created <> '' then
    begin
      Write(FFile, Created);
    end;

    if LastModified <> '' then
    begin
      Write(FFile, Sep2);
      Write(FFile, LastModified);
    end;

    if Description <> '' then
    begin
      Write(FFile, DescPre);

      if Length(Description) < 80 then
      begin
        Write(FFile, Description);
      end
      else
      begin
        Write(FFile, Copy(Description, 1, 77), '...');
      end;
    end;

    WriteLn(FFile, PostElement);
  end;
end;

end.
