unit FMX.Material.ZIndex.Editor;

interface

uses
  FMXVclUtils, FMX.Graphics, VCL.Graphics, DesignEditors, DesignIntf, System.Types, System.Classes, FMX.Forms,
  FMXEditors, VCLEditors, System.SysUtils, System.UITypes, FMX.Types, FMX.Ani, ToolsAPI, PropInspAPI;

type
  TElevationProperty = class(TIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

implementation

uses
  FMX.Material.ZIndex;

{ TElevationProperty }

function TElevationProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paValueList, paRevertable];
end;

function TElevationProperty.GetValue: string;
begin
  try
    Result := ElevationToString(TElevation(GetOrdValue));
  except
  end;
end;

procedure TElevationProperty.GetValues(Proc: TGetStrProc);
begin
  GetElevationValues(Proc);
end;

procedure TElevationProperty.SetValue(const Value: string);
begin
  try
    SetOrdValue(Integer(StringToElevation(Value)));
    Modified;
  except
  end;
end;

end.
