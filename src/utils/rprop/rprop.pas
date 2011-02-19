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
  Prop: PRProp;
begin
  New(Prop);
  Prop^.Name := PropName;
  Prop^.Value := PropValue;

  RemoveProp(PropName);
  List^.Add(Prop);
end;

function GetProp(const PropName: String): Pointer;
var
  i: cardinal;
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
  i: cardinal;
  p: PRProp;
begin
  if List^.Count > 0 then
  begin
    for i := 0 to List^.Count - 1 do
    begin
      p := PRProp(List^.GetNth(i));
      if p^.Name = PropName then
      begin
        List^.DeleteObject(p);
      end;
    end;
  end;
end;

procedure FreeProps;
var
  i: cardinal;
begin
  if List^.Count > 0 then
  begin
    for i := 0 to List^.Count - 1 do
    begin
      Dispose(PRProp(List^.GetNth(i)));
    end;
  end;
end;

initialization

New(List, Init);

finalization

FreeProps;
Dispose(List, Done);

end.
