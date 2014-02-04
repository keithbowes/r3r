unit RProp;

interface

uses
  RList;

type
  TEnumObjectPropsProc = function(PropObject: Pointer; PropName: String; PropValue, UserData: Pointer): Boolean;

procedure SetObjectProp(const PropObject: Pointer; const PropName: String; PropValue: Pointer);
function GetObjectProp(const PropObject: Pointer; const PropName: String): Pointer;
procedure RemoveObjectProp(const PropObject: Pointer; const PropName: String);
procedure EnumObjectProps(const PropObject: Pointer; ProcCallback: TEnumObjectPropsProc; UserData: Pointer);
function GetObjectList(const PropName: String): PRList;
procedure FreeObjectList(List: PRList);

{ Legacy Routines }
procedure SetProp(const PropName: String; PropValue: Pointer);
function GetProp(const PropName: String): Pointer;
procedure RemoveProp(const PropName: String);

implementation

type
  PRProp = ^TRProp;
  TRProp = record
    Name: String;
    Obj: Pointer;
    Value: Pointer;
  end;

var
  List: PRList;

procedure SetObjectProp(const PropObject: Pointer; const PropName: String; PropValue: Pointer);
var
  Found: Boolean;
  i: PtrUInt;
  Prop: PRProp;
begin
  Found := false;

  if List^.Count > 0 then
  begin
    for i := 0 to List^.Count - 1 do
    begin
      Prop := List^.GetNth(i);
      if (Prop <> nil) and (Prop^.Name = PropName) and (Prop^.Obj = PropObject) then
      begin
        Prop^.Value := PropValue;
        Found := true;
        Break;
      end;
    end;
  end;

  if not Found then
  begin
    New(Prop);
    if Assigned(Prop) then
    begin
      Prop^.Name := PropName;
      Prop^.Obj := PropObject;
      Prop^.Value := PropValue;
      List^.Add(Prop);
    end;
  end;
end;

function GetObjectProp(const PropObject: Pointer; const PropName: String): Pointer;
var
  i: PtrUInt;
  p: PRProp;
  r: Pointer;
begin
  r := nil;

  if List^.Count > 0 then
  begin
    for i := 0 to List^.Count - 1 do
    begin
      p := PRProp(List^.GetNth(i));
      if (p^.Name = PropName) and (p^.Obj = PropObject) then
      begin
        r := p^.Value;
      end;
    end;
  end;

  GetObjectProp := r;
end;

procedure RemoveObjectProp(const PropObject: Pointer; const PropName: String);
var
  i: PtrUInt;
  p: PRProp;
begin
  if List^.Count > 0 then
  begin
    for i := 0 to List^.Count - 1 do
    begin
      p := PRProp(List^.GetNth(i));
      if (p^.Name = PropName) and (p^.Obj = PropObject) then
      begin
        List^.Delete(i);
      end;
    end;
  end;
end;

procedure EnumObjectProps(const PropObject: Pointer; ProcCallback: TEnumObjectPropsProc; UserData: Pointer);
var
  i: PtrUInt;
  ShouldContinue: Boolean;
begin
  if List^.Count > 0 then
  begin
    i := 0;
    ShouldContinue := true;
    while ShouldContinue and (i < List^.Count) do
    begin
      with PRProp(List^.GetNth(i))^ do
      begin
        if Obj = PropObject then
        begin
          ShouldContinue := ProcCallback(Obj, Name, Value, UserData);
        end
      end;
      Inc(i);
    end;
  end;
end;

function GetObjectList(const PropName: String): PRList;
var
  i: PtrUInt;
  l: PRList;
  p: PRProp;
begin
  New(l, Init);
  if List^.Count > 0 then
  begin
    for i := 0 to List^.Count - 1 do
    begin
      p := PRProp(List^.GetNth(i));
      if (p^.Name = PropName) or (PropName = '*') then
      begin
        l^.Add(p^.Obj);
      end;
    end;
  end;

  GetObjectList := l;
end;

procedure FreeObjectList(List: PRList);
begin
  Dispose(List, Done);
end;

procedure SetProp(const PropName: String; PropValue: Pointer);
begin
  SetObjectProp(nil, PropName, PropValue);
end;

function GetProp(const PropName: String): Pointer;
begin
  GetProp := GetObjectProp(nil, PropName);
end;

procedure RemoveProp(const PropName: String);
begin
  RemoveObjectProp(nil, PropName);
end;

procedure FreeProps;
var
  i: PtrUInt;
  p: PRProp;
begin
  if List^.Count > 0 then
  begin
    for i := 0 to List^.Count - 1 do
    begin
      p := List^.GetNth(i);
      Dispose(p);
    end;
  end;
end;

initialization

New(List, Init);

finalization

FreeProps;
FreeObjectList(List);

end.
