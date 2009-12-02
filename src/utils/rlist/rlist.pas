{ A hopefully portable unit for using managable lists and dynamic arrays }

unit RList;

interface

type
  PRListNode = ^TRListNode;
  TRListNode = record
    Data: Pointer;
    Next: PRListNode;
  end;

  PRList = ^TRList;
  TRList = object
  private
    FCount: word;
    FElem: PRListNode;
    FFirst: PRListNode;
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

implementation

constructor TRList.Init;
begin
  FCount := 0;
end;

destructor TRList.Done;
begin
  Free(FFirst);
  FCount := 0;
end;

procedure TRList.Add(Data: Pointer);
begin
  if FElem <> nil then
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
  Delete(w);
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

end.
