{ A hopefully portable unit for using managable lists and dynamic arrays }

unit RList;

interface

type
  PRListNode = ^TRListNode;
  TRListNode = record
    Data: Pointer;
    Next: PRListNode
  end;

  PRList = ^TRList;
  TRList = object
  private
    FCount: word;
    FElem: PRListNode;
    FFirst: PRListNode;
    FHasFirst: Boolean;
    FNext: PRListNode;
    function GetNode(Data: Pointer): PRListNode;
    procedure Free(List: PRListNode);
  public
    constructor Init;
    destructor Done;
    procedure Add(Data: Pointer);
    procedure Insert(Data: Pointer; Index: word);
    procedure Delete(N: word);
    procedure DeleteObject(Data: Pointer);
    procedure Clear;
    function Count: word;
    function GetNth(N: word): Pointer;
    function IndexOf(Data: Pointer): integer;
  end;

  PRStringList = ^TRStringList;
  TRStringList = object(TRList)
  private
    FStrings: PRList;
    function StrToPChar(s: String): PChar;
  public
    constructor Init;
    destructor Done;
    procedure Add(Data: String);
    procedure Insert(Data: String; Index: word);
    procedure DeleteString(Data: String);
    function GetNth(N: word): String;
    function IndexOf(Data: String): integer;
  end;

implementation

uses
  Strings;

{ TRList: a simple list object }
constructor TRList.Init;
begin
  FCount := 0;
  FHasFirst := false;
end;

destructor TRList.Done;
begin
  Free(FFirst);
  FCount := 0;
end;

procedure TRList.Add(Data: Pointer);
begin
  if FHasFirst then
  begin
    New(FNext);

    FNext^.Data := Data;
    FNext^.Next := nil;

    FElem^.Next := FNext;

    FElem := FNext;
  end
  else
  begin
    New(FElem);
    FElem^.Data := Data;
    FElem^.Next := nil;

    FFirst := FElem;
    FHasFirst := true;
  end;

  Inc(FCount);
end;

procedure TRList.Insert(Data: Pointer; Index: word);
var
  l, m, o: PRListNode;
  p: Pointer;
begin
  New(l);
  l^.Data := Data;
  m := nil;

  if FCount <> 0 then
  begin
    if Index <> 0 then
    begin
      p := GetNth(Index - 1);
      o := GetNode(p);
      m := o^.Next;
      o^.Next := l;
    end
    else
    begin
      m := FFirst;
      FFirst := l;
    end;
  end
  else
  begin
    Add(Data);
    Dispose(l);
  end;

  if m <> nil then
  begin
    l^.Next := m;
    Inc(FCount);
  end;
end;

procedure TRList.Delete(N: word);
var
  l, m: PRListNode;
  p: Pointer;
begin
  if N <> 0 then
  begin
    if N <> FCount then
    begin
      p := GetNth(N - 1);
      l := GetNode(p);
      m := l^.Next;
    
      l^.Next := l^.Next^.Next;
      Dispose(m);
    end
    else
    begin
      p := GetNth(N);
      p := nil;
    end;
  end
  else
  begin
    m := FFirst;
    FFirst := FFirst^.Next;
    Dispose(m);
  end;

  Dec(FCount);
end;

procedure TRList.DeleteObject(Data: Pointer);
var
  w: integer;
begin
  w := IndexOf(Data);
  
  if w <> -1 then
  begin
    Delete(w);
  end;
end;

function TRList.Count: word;
begin
  Count := FCount;
end;

function TRList.GetNth(N: word): Pointer;
var
  i: word;
  Tmp: PRListNode;
begin
  i := 0;
  Tmp := FFirst;

  if N >= FCount then
  begin
    N := FCount - 1;
  end;

  for i := 1 to N do
  begin
    Tmp := Tmp^.Next;
  end;

  GetNth := Tmp^.Data;
end;

function TRList.IndexOf(Data: Pointer): integer;
var
  i: integer;
  Tmp: PRListNode;
begin
  i := 0;
  Tmp := FFirst;

  while (Tmp^.Next <> nil) and (Tmp^.Data <> Data) do
  begin;
    Tmp := Tmp^.Next;
    Inc(i);
  end;

  if Tmp^.Data = Data then
  begin
    IndexOf := i;
  end
  else
  begin
    IndexOf := -1;
  end;
end;

procedure TRList.Clear;
begin
  FElem := FFirst; 

  Free(FElem^.Next);
  FElem := nil;

  FCount := 0;
  FHasFirst := false;
  Dispose(FFirst);
end;

function TRList.GetNode(Data: Pointer): PRListNode;
var
  Tmp: PRListNode;
begin
  Tmp := FFirst;
  
  while (Tmp^.Next <> nil) and (Tmp^.Data <> Data) do
  begin
    Tmp := Tmp^.Next;
  end;

  GetNode := Tmp;
end;

procedure TRList.Free(List: PRListNode);
begin
  if (List <> nil) and (List^.Next <> nil) then
  begin
    Free(List^.Next);
  end;

  Dispose(List);
end;

{ TRStringList: work with strings }
constructor TRStringList.Init;
begin
  inherited Init;
  New(FStrings, Init);
end;

destructor TRStringList.Done;
var
  i: word;
begin
  if FStrings^.Count > 0 then
  begin
    for i := 0 to FStrings^.Count - 1 do
    begin
      FreeMem(FStrings^.GetNth(i));
    end;
  end;

  Dispose(FStrings, Done);
  inherited Done;
end;

procedure TRStringList.Add(Data: String);
begin
  inherited Add(StrToPChar(Data));
end;

procedure TRStringList.Insert(Data: String; Index: word);
begin
  inherited Insert(StrToPChar(Data), Index);
end;

procedure TRStringList.DeleteString(Data: String);
var
  w: integer;
begin
  w := IndexOf(Data);

  if w <> -1 then
  begin
    Delete(w);
  end;
end;

function TRStringList.GetNth(N: word): String;
begin
  GetNth := StrPas(inherited GetNth(N));
end;

function TRStringList.IndexOf(Data: String): integer;
var
  i, Len: word;
  p: PChar;
begin
  i := 0;
  Len := FStrings^.Count;
  p := StrToPChar(Data);

  while (i < Len) and (StrComp(p, FStrings^.GetNth(i)) <> 0) do
  begin
    Inc(i);
  end;

  if i <> Len then
  begin
    IndexOf := i;
  end
  else
  begin
    IndexOf := -1;

    if Count > 0 then
    begin
      FStrings^.Delete(i);
      FreeMem(p);
    end;
  end;
end;

function TRStringList.StrToPChar(s: String): PChar;
var
  p: PChar;
begin
  GetMem(p, Length(s) + 1);
  StrPCopy(p, s);
  StrToPChar := p;

  FStrings^.Add(p);
end;

end.
