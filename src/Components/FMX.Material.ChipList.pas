unit FMX.Material.ChipList;

interface

uses
  FMX.Material.Card, FMX.Edit, FMX.Layouts, FMX.Material.Chip, System.Classes, System.Generics.Collections;

type
  TMaterialChipListValidate = function(AValue: string): Boolean of object;

  TMaterialChipList = class(TMaterialCard)
  private
    FLayout: TFlowLayout;
    FScroll: TVertScrollBox;
    FEdit: TEdit;
    FChipBase: TMaterialChip;
    FOnValidate: TMaterialChipListValidate;
    FChips: TList<TMaterialChip>;
    procedure DoAddChip(const AText: string);

    procedure InternalEditKeyUp(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
    procedure InternalEditTyping(Sender: TObject);
    procedure SetChipBase(const Value: TMaterialChip);
    procedure SetChips(const Value: TList<TMaterialChip>);
  protected
    procedure Click; override;
    procedure Loaded; override;

  public
    constructor Create(AOwner: TComponent); override;
    procedure Remove(AChip: TMaterialChip);
    procedure Add(AChip: TMaterialChip); overload;
    procedure Add(AChip: string); overload;
    property Chips: TList<TMaterialChip> read FChips write SetChips;

    destructor Destroy; override;

  published
    property ChipBase: TMaterialChip read FChipBase write SetChipBase;
    property OnValidate: TMaterialChipListValidate read FOnValidate write FOnValidate;
  end;

implementation

uses
  System.UITypes, System.SysUtils, FMX.Controls, FMX.Types;

type
  TMaterialChipListFlowLayout = class(TFlowLayout)
  const
    DEFAULT_END_MARGING = 20;
  protected
    procedure DoRealign; override;
  end;

  { TMaterialChipList }

procedure TMaterialChipList.Add(AChip: TMaterialChip);
begin
  FChips.Add(AChip);
  AChip.Parent := FScroll;
end;

procedure TMaterialChipList.Add(AChip: string);
begin
  DoAddChip(AChip);
end;

procedure TMaterialChipList.Click;
begin
  inherited;
  FEdit.SetFocus;
end;

constructor TMaterialChipList.Create(AOwner: TComponent);
begin
  inherited;
  FScroll := TVertScrollBox.Create(Self);
  FScroll.Stored := False;
  FScroll.SetSubComponent(True);
  FScroll.Align := TAlignLayout.Contents;
  FScroll.Parent := Self;
  FScroll.Margins.Top := 10;
  FScroll.Margins.Left := 5;
  FScroll.Margins.Right := 5;
  FScroll.Margins.Bottom := 10;
  FScroll.HitTest := False;

  FLayout := TMaterialChipListFlowLayout.Create(Self);
  FLayout.Stored := False;
  FLayout.SetSubComponent(True);
  FLayout.Align := TAlignLayout.Top;
  FLayout.Parent := FScroll;
  FLayout.HitTest := False;

  FChipBase := TMaterialChip.Create(Self);
  FChipBase.Name := 'TMaterialChipListBase';
  FChipBase.SetSubComponent(True);

  FEdit := TEdit.Create(Self);
  FEdit.Stored := False;
  FEdit.SetSubComponent(True);

  FChipBase.Parent := FLayout;

  if csDesigning in ComponentState then
  begin
    FChipBase.Text := 'Test chip';
  end
  else
  begin
    FEdit.Parent := FChipBase;
    FEdit.Align := TAlignLayout.Client;
    FEdit.Font.Assign(FChipBase.Font);
    FEdit.StylesData['background.Opacity'] := 0;
    FEdit.Opacity := 0;
  end;

  FEdit.OnKeyUp := InternalEditKeyUp;
  FEdit.OnTyping := InternalEditTyping;

  FChips := TList<TMaterialChip>.Create;
end;

destructor TMaterialChipList.Destroy;
begin
  FChips.DisposeOf;
  inherited;
end;

procedure TMaterialChipList.DoAddChip(const AText: string);
var
  LChip: TMaterialChip;
begin
  if Assigned(OnValidate) and not OnValidate(AText) then
    Exit;

  FLayout.BeginUpdate;
  try
    FChipBase.Parent := nil;
    LChip := TMaterialChip.Create(Self);
    LChip.Parent := FLayout;
    LChip.Assign(FChipBase);
    LChip.Text := AText;
    FChips.Add(LChip);
  finally
    FChipBase.Parent := FLayout;
    FLayout.EndUpdate;
  end;
  FScroll.ScrollBy(0, -FLayout.Height);
end;

procedure TMaterialChipList.InternalEditKeyUp(Sender: TObject; var Key: Word; var KeyChar: WideChar;
  Shift: TShiftState);
begin
  if Key = vkReturn then
  begin
    DoAddChip(FEdit.Text);
    FEdit.Text := EmptyStr;
    FChipBase.Text := EmptyStr;
  end;

  if (Key = vkBack) and (FEdit.Text.IsEmpty) and (FChips.Count > 0) then
  begin
    FChips.Last.DisposeOf;
    FChips.Remove(FChips.Last);
  end;
end;

procedure TMaterialChipList.InternalEditTyping(Sender: TObject);
begin
  FChipBase.Text := FEdit.Text;
end;

procedure TMaterialChipList.Loaded;
begin
  inherited;
  FChipBase.Text := '';
end;

procedure TMaterialChipList.Remove(AChip: TMaterialChip);
begin
  FChips.Remove(AChip);
  AChip.DisposeOf;
end;

procedure TMaterialChipList.SetChipBase(const Value: TMaterialChip);
begin
  FChipBase.Assign(Value);
end;

procedure TMaterialChipList.SetChips(const Value: TList<TMaterialChip>);
begin
  FChips := Value;
end;

{ TMaterialChipListFlowLayout }

procedure TMaterialChipListFlowLayout.DoRealign;
var
  LLastControl: TControl;
begin
  inherited;
  LLastControl := Controls[ControlsCount - 1];
  Height := LLastControl.Position.Y + LLastControl.Height + DEFAULT_END_MARGING
end;

end.
