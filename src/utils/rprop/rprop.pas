unit RProp;

interface

procedure SetProp(const PropName: String; PropValue: Pointer);
function GetProp(const PropName: String): Pointer;
procedure RemoveProp(const PropName: String);

implementation

uses
  RList;

type
  PRProp = ^TRProp;
  TRProp = record
    Name: String;
    Value: Pointer;
  end;

var
  List: PRList;

procedure SetProp(const PropName: String; PropValue: Pointer);
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
      if (Prop <> nil) and (Prop^.Name = PropName) then
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
    Prop^.Name := PropName;
    Prop^.Value := PropValue;
    List^.Add(Prop);
  end;
end;

function GetProp(const PropName: String): Pointer;
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
      if p^.Name = PropName then
      begin
        r := p^.Value;
      end;
    end;
  end;

  GetProp := r;
end;

procedure RemoveProp(const PropName: String);
var
  i: PtrUInt;
  p: PRProp;
begin
  if List^.Count > 0 then
  begin
    for i := 0 to List^.Count - 1 do
    begin
      p := PRProp(List^.GetNth(i));
      if p^.Name = PropName then
      begin
        List^.Delete(i);
      end;
    end;
  end;
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
Dispose(List, Done);

end.
