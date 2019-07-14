unit FMX.Material.ZIndex;

interface

uses
  System.Classes;

const
  // https://material.io/design/environment/elevation.html#default-elevations
  Elevations: array [0 .. 24] of TIdentMapEntry = (
    (Value: 0; Name: 'Elevation 0dp'),
    (Value: 1; Name: 'Elevation 1dp'),
    (Value: 2; Name: 'Elevation 2dp'),
    (Value: 3; Name: 'Elevation 3dp'),
    (Value: 4; Name: 'Elevation 4dp'),
    (Value: 5; Name: 'Elevation 5dp'),
    (Value: 6; Name: 'Elevation 6dp'),
    (Value: 7; Name: 'Elevation 7dp'),
    (Value: 8; Name: 'Elevation 8dp'),
    (Value: 9; Name: 'Elevation 9dp'),
    (Value: 10; Name: 'Elevation 10dp'),
    (Value: 11; Name: 'Elevation 11dp'),
    (Value: 12; Name: 'Elevation 12dp'),
    (Value: 13; Name: 'Elevation 13dp'),
    (Value: 14; Name: 'Elevation 14dp'),
    (Value: 15; Name: 'Elevation 15dp'),
    (Value: 16; Name: 'Elevation 16dp'),
    (Value: 17; Name: 'Elevation 17dp'),
    (Value: 18; Name: 'Elevation 18dp'),
    (Value: 19; Name: 'Elevation 19dp'),
    (Value: 20; Name: 'Elevation 20dp'),
    (Value: 21; Name: 'Elevation 21dp'),
    (Value: 22; Name: 'Elevation 22dp'),
    (Value: 23; Name: 'Elevation 23dp'),
    (Value: 24; Name: 'Elevation 24dp'));

  MAX_ELEVATION = 24;

type
  PElevation = ^TElevation;
  TElevation = type Integer;

  TElevationMapEntry = record
    Value: TElevation;
    Name: string;
  end;

  TElevationHelper = record helper for TElevation
    procedure AdjustValue;
  end;

function ElevationToString(Value: TElevation): string;
function ElevationToIdent(Elevation: Integer; var Ident: string): Boolean;
function StringToElevation(const Value: string): TElevation;
procedure GetElevationValues(Proc: TGetStrProc);


implementation

uses
  System.SysUtils;

procedure GetElevationValues(Proc: TGetStrProc);
var
  LItem: Integer;
begin
  for LItem := Low(Elevations) to High(Elevations) do
    Proc(ElevationToString(TElevation(Elevations[LItem].Value)));
end;

function StringToElevation(const Value: string): TElevation;
var
  LElevationInt: Integer;
  LValue: string;
begin
  if Value.StartsWith('Elevation') then
    LValue := Value
  else if TryStrToInt(Value, LElevationInt) then
    LValue := 'Elevation ' + Value + 'dp';

  if (not IdentToInt(LValue, LElevationInt, Elevations)) then
    Result := TElevation(Value.ToInteger)
  else
    Result := TElevation(LElevationInt);
end;

function ElevationToIdent(Elevation: Integer; var Ident: string): Boolean;
begin
  Result := IntToIdent(Elevation, Ident, Elevations);
  if not Result then
  begin
    if Elevation > MAX_ELEVATION then
      Elevation := MAX_ELEVATION;

    Ident := 'Elevation ' + Elevation.ToString + 'dp';
    Result := True;
  end;
end;

function ElevationToString(Value: TElevation): string;
begin
  ElevationToIdent(Integer(Value), Result);
end;

{ TElevationHelper }

procedure TElevationHelper.AdjustValue;
begin
  if Self > MAX_ELEVATION then
    Self := MAX_ELEVATION;
end;

end.
