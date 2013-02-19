program R3RConv;

uses
  AtomWriter, EsfWriter, RssWriter, Rss3Writer,
  Conv, ConvStrings, ItemCallbacks, LibR3R, SysUtils,
  {$IFDEF __GPC__}
  GPC
  {$ELSE}
  GetOpts
  {$ENDIF};

{$IFDEF __GPC__}
{$define OptArg OptionArgument}
{$ENDIF}

var
  Feed: TLibR3R;
  InFile: String;
  IsTopElem: Boolean;
  OutType: String;

procedure ItemReceived(const Item: TFeedItem);
begin
  if OutType = 'esf' then
  begin
    WriteEsf(Item, IsTopElem);
  end
  else if OutType = 'rss3' then
  begin
    WriteRss3(Item);
  end
  else if OUtType = 'rss' then
  begin
    WriteRss(Item, IsTopElem);
  end
  else if OutType = 'atom' then
  begin
    WriteAtom(Item, IsTopElem);
  end
  else
  begin
    WriteLn(WrongType);
  end;

  IsTopElem := false;
end;

procedure ShowHelp(invok: String);
begin
	WriteLn(stderr, StringReplace(Usage, '%s', invok, []));
end;

procedure ProcParams;
const
  OptString = 'hi:o:t:';
var
  i: char;
  invok: String;
begin
  invok := ParamStr(0);
  
  repeat
		i := GetOpt(OptString);
		if i <> EndOfOptions then
		begin
			case i of
				'h':
        begin
					ShowHelp(invok);
					Exit;
        end;
				'i':
        begin
					InFile := OptArg;
        end;
				'o':
        begin
					OutFile := OptArg;
        end;
				't':
        begin
					OutType := OptArg;
        end;
      end;	
    end;
	until i = EndOfOptions;

  if (InFile = '') and (OutType = '') then
  begin
		ShowHelp(invok);
    Exit;
  end;
end;

begin
  ProcParams;

  Feed := TLibR3R.Create;
  Feed.RegisterItemCallback(ItemReceived);
	Feed.RetrieveFeed(InFile);
	Feed.Free;

  if OutType = 'rss' then
  begin
    CloseRss;
  end
  else if OutType = 'atom' then
  begin
    CloseAtom;
  end;
end.
