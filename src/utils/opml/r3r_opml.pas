program R3R_OPML;

uses
  DOM, RegExpr, RList, XMLWrite;

const
  DefOPML = 'abos.opml';
  DefText = 'subscriptions.txt';
  Tab = #9;

var
  BodyElem, Elem, HeadElem, OPMLElem: TDOMElement;
  Comment: TDOMComment;
  f: text;
  List: PRList;
  OPML: TXMLDocument;
  OutName: String;
  re: TRegExpr;
  s: String;

begin
  OutName := ParamStr(1);
  if OutName = '' then
  begin
    OutName := DefOPML;
  end;

  New(List, Init);
  OPML := TXMLDocument.Create;

  Assign(f, DefText);
  Reset(f);

  OPMLElem := OPML.CreateElement('opml');
  OPMLElem.SetAttribute('version', '2.0');

  HeadElem := OPML.CreateElement('head');
  OPMLElem.AppendChild(HeadElem);

  BodyElem := OPML.CreateElement('body');

  while not Eof(f) do
  begin
    ReadLn(f, s);
    if Length(s) = 0 then
    begin
      Continue
    end
    else if Pos('#', s) = 1 then
    begin
      re := TRegExpr.Create('^#+');
      s := re.Replace(s, '', false);
      re.Free;

      Comment := OPML.CreateComment(s);
      BodyElem.AppendChild(Comment);
      List^.Add(Comment);
    end
    else
    begin
      Elem := OPML.CreateElement('outline');
      Elem.SetAttribute('text', s);
      Elem.SetAttribute('type', 'rss');
      Elem.SetAttribute('xmlUrl', s);
      BodyElem.AppendChild(Elem);
      List^.Add(Elem);
    end;
  end;
  Close(f);

  OPMLELem.AppendChild(BodyElem);

  OPML.AppendChild(OPMLElem);
  WriteXMLFile(OPML, OutName);

  HeadElem.Free;

  Dispose(List, Done);
  BodyElem.Free;

  OPMLElem.Free;
  OPML.Free;
end.
