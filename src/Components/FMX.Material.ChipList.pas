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
    FReadOnly: Boolean;
    procedure DoAddChip(const AText: string);

    procedure InternalEditKeyUp(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
    procedure InternalEditTyping(Sender: TObject);


    procedure SetChipBase(const Value: TMaterialChip);
    procedure SetChips(const Value: TList<TMaterialChip>);
    procedure SetReadOnly(const Value: Boolean);
  protected
    procedure Click; override;
    procedure Loaded; override;
    procedure Paint; override;
    procedure DoEnter; override;


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
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
  published
    property HitTest;
    property OnCanFocus;
    property TabStop;
    property CanFocus;
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
  FLayout.BeginUpdate;
  try
    FEdit.Parent := nil;
    AChip.Parent := FScroll;
    FChips.Add(AChip);
  finally
    if not ReadOnly then
    begin
      FEdit.Parent := FLayout;
      FEdit.Width := FLayout.Width - FEdit.Position.X;
    end;
    FLayout.EndUpdate;
  end;
  FScroll.ScrollBy(0, -FLayout.Height);
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
  CanFocus := True;
  TabStop := True;

  FScroll := TVertScrollBox.Create(Self);
  FScroll.Stored := False;
  FScroll.SetSubComponent(True);
  FScroll.Align := TAlignLayout.Contents;
  FScroll.Parent := Self;
  FScroll.HitTest := False;

  FLayout := TMaterialChipListFlowLayout.Create(Self);
  FLayout.Parent := FScroll;
  FLayout.Stored := False;
  FLayout.SetSubComponent(True);
  FLayout.Align := TAlignLayout.Top;
  FLayout.HitTest := False;
  FLayout.Padding.Top := 10;
  FLayout.Padding.Left := 5;
  FLayout.Padding.Right := 5;
  FLayout.Padding.Bottom := 10;

  FChipBase := TMaterialChip.Create(Self);
  FChipBase.Name := 'TMaterialChipListBase';
  FChipBase.SetSubComponent(True);

  FEdit := TEdit.Create(Self);
  FEdit.Stored := False;
  FEdit.SetSubComponent(True);

  if csDesigning in ComponentState then
  begin
    FChipBase.Parent := FLayout;
    FChipBase.Text := 'Test chip';
  end
  else
  begin
    FEdit.Parent := FLayout;
    FEdit.Align := TAlignLayout.Client;
    FEdit.Font.Assign(FChipBase.Font);
    FEdit.StylesData['background.Opacity'] := 0;
    FEdit.Parent := FLayout;
    FEdit.Width := FLayout.Width - FEdit.Position.X;
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
    FEdit.Parent := nil;
    LChip := TMaterialChip.Create(Self);
    LChip.Parent := FLayout;
    LChip.Assign(FChipBase);
    LChip.Text := AText;
    LChip.HitTest := Self.HitTest;
    FChips.Add(LChip);
  finally
    if not ReadOnly then
    begin
      FEdit.Width := 20;
      FEdit.Parent := FLayout;
    end;
    FLayout.EndUpdate;
  end;
  FScroll.ScrollBy(0, -FLayout.Height);
end;

procedure TMaterialChipList.DoEnter;
begin
  FEdit.SetFocus;
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

procedure TMaterialChipList.Paint;
begin
  inherited;
  FEdit.Width := FLayout.Width - FEdit.Position.X - TMaterialChipListFlowLayout.DEFAULT_END_MARGING;
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

procedure TMaterialChipList.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
  if Value then
    FEdit.Parent := nil
  else
    FEdit.Parent := FLayout;
end;

{ TMaterialChipListFlowLayout }

procedure TMaterialChipListFlowLayout.DoRealign;
var
  LLastControl: TControl;
begin
  inherited;

  if ControlsCount = 0 then
    Exit;

  LLastControl := Controls[ControlsCount - 1];
  Height := LLastControl.Position.Y + LLastControl.Height + DEFAULT_END_MARGING
end;

end.
