unit MD.ColorPalette;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, System.UIConsts, FMX.Ani;

const
  MaterialColors: array [0 .. 241] of TIdentMapEntry = (
    (Value: Integer($FFFFEBEE); Name: 'claRed50'),
    (Value: Integer($FFFFCDD2); Name: 'claRed100'),
    (Value: Integer($FFEF9A9A); Name: 'claRed200'),
    (Value: Integer($FFE57373); Name: 'claRed300'),
    (Value: Integer($FFEF5350); Name: 'claRed400'),
    (Value: Integer($FFF44336); Name: 'claRed500'),
    (Value: Integer($FFE53935); Name: 'claRed600'),
    (Value: Integer($FFD32F2F); Name: 'claRed700'),
    (Value: Integer($FFC62828); Name: 'claRed800'),
    (Value: Integer($FFB71C1C); Name: 'claRed900'),
    (Value: Integer($FFFF8A80); Name: 'claRedA100'),
    (Value: Integer($FFFF5252); Name: 'claRedA200'),
    (Value: Integer($FFFF1744); Name: 'claRedA400'),
    (Value: Integer($FFD50000); Name: 'claRedA700'),
    (Value: Integer($FFFCE4EC); Name: 'claPink50'),
    (Value: Integer($FFF8BBD0); Name: 'claPink100'),
    (Value: Integer($FFF48FB1); Name: 'claPink200'),
    (Value: Integer($FFF06292); Name: 'claPink300'),
    (Value: Integer($FFEC407A); Name: 'claPink400'),
    (Value: Integer($FFE91E63); Name: 'claPink500'),
    (Value: Integer($FFD81B60); Name: 'claPink600'),
    (Value: Integer($FFC2185B); Name: 'claPink700'),
    (Value: Integer($FFAD1457); Name: 'claPink800'),
    (Value: Integer($FF880E4F); Name: 'claPink900'),
    (Value: Integer($FFFF80AB); Name: 'claPinkA100'),
    (Value: Integer($FFFF4081); Name: 'claPinkA200'),
    (Value: Integer($FFF50057); Name: 'claPinkA400'),
    (Value: Integer($FFC51162); Name: 'claPinkA700'),
    (Value: Integer($FFF3E5F5); Name: 'claPurple50'),
    (Value: Integer($FFE1BEE7); Name: 'claPurple100'),
    (Value: Integer($FFCE93D8); Name: 'claPurple200'),
    (Value: Integer($FFBA68C8); Name: 'claPurple300'),
    (Value: Integer($FFAB47BC); Name: 'claPurple400'),
    (Value: Integer($FF9C27B0); Name: 'claPurple500'),
    (Value: Integer($FF8E24AA); Name: 'claPurple600'),
    (Value: Integer($FF7B1FA2); Name: 'claPurple700'),
    (Value: Integer($FF6A1B9A); Name: 'claPurple800'),
    (Value: Integer($FF4A148C); Name: 'claPurple900'),
    (Value: Integer($FFEA80FC); Name: 'claPurpleA100'),
    (Value: Integer($FFE040FB); Name: 'claPurpleA200'),
    (Value: Integer($FFD500F9); Name: 'claPurpleA400'),
    (Value: Integer($FFAA00FF); Name: 'claPurpleA700'),
    (Value: Integer($FFEDE7F6); Name: 'claDeepPurple50'),
    (Value: Integer($FFD1C4E9); Name: 'claDeepPurple100'),
    (Value: Integer($FFB39DDB); Name: 'claDeepPurple200'),
    (Value: Integer($FF9575CD); Name: 'claDeepPurple300'),
    (Value: Integer($FF7E57C2); Name: 'claDeepPurple400'),
    (Value: Integer($FF673AB7); Name: 'claDeepPurple500'),
    (Value: Integer($FF5E35B1); Name: 'claDeepPurple600'),
    (Value: Integer($FF512DA8); Name: 'claDeepPurple700'),
    (Value: Integer($FF4527A0); Name: 'claDeepPurple800'),
    (Value: Integer($FF311B92); Name: 'claDeepPurple900'),
    (Value: Integer($FFB388FF); Name: 'claDeepPurpleA100'),
    (Value: Integer($FF7C4DFF); Name: 'claDeepPurpleA200'),
    (Value: Integer($FF651FFF); Name: 'claDeepPurpleA400'),
    (Value: Integer($FF6200EA); Name: 'claDeepPurpleA700'),
    (Value: Integer($FFE8EAF6); Name: 'claIndigo50'),
    (Value: Integer($FFC5CAE9); Name: 'claIndigo100'),
    (Value: Integer($FF9FA8DA); Name: 'claIndigo200'),
    (Value: Integer($FF7986CB); Name: 'claIndigo300'),
    (Value: Integer($FF5C6BC0); Name: 'claIndigo400'),
    (Value: Integer($FF3F51B5); Name: 'claIndigo500'),
    (Value: Integer($FF3949AB); Name: 'claIndigo600'),
    (Value: Integer($FF303F9F); Name: 'claIndigo700'),
    (Value: Integer($FF283593); Name: 'claIndigo800'),
    (Value: Integer($FF1A237E); Name: 'claIndigo900'),
    (Value: Integer($FF8C9EFF); Name: 'claIndigoA100'),
    (Value: Integer($FF536DFE); Name: 'claIndigoA200'),
    (Value: Integer($FF3D5AFE); Name: 'claIndigoA400'),
    (Value: Integer($FF304FFE); Name: 'claIndigoA700'),
    (Value: Integer($FFE3F2FD); Name: 'claBlue50'),
    (Value: Integer($FFBBDEFB); Name: 'claBlue100'),
    (Value: Integer($FF90CAF9); Name: 'claBlue200'),
    (Value: Integer($FF64B5F6); Name: 'claBlue300'),
    (Value: Integer($FF42A5F5); Name: 'claBlue400'),
    (Value: Integer($FF2196F3); Name: 'claBlue500'),
    (Value: Integer($FF1E88E5); Name: 'claBlue600'),
    (Value: Integer($FF1976D2); Name: 'claBlue700'),
    (Value: Integer($FF1565C0); Name: 'claBlue800'),
    (Value: Integer($FF0D47A1); Name: 'claBlue900'),
    (Value: Integer($FF82B1FF); Name: 'claBlueA100'),
    (Value: Integer($FF448AFF); Name: 'claBlueA200'),
    (Value: Integer($FF2979FF); Name: 'claBlueA400'),
    (Value: Integer($FF2962FF); Name: 'claBlueA700'),
    (Value: Integer($FFE1F5FE); Name: 'claLightBlue50'),
    (Value: Integer($FFB3E5FC); Name: 'claLightBlue100'),
    (Value: Integer($FF81D4FA); Name: 'claLightBlue200'),
    (Value: Integer($FF4FC3F7); Name: 'claLightBlue300'),
    (Value: Integer($FF29B6F6); Name: 'claLightBlue400'),
    (Value: Integer($FF03A9F4); Name: 'claLightBlue500'),
    (Value: Integer($FF039BE5); Name: 'claLightBlue600'),
    (Value: Integer($FF0288D1); Name: 'claLightBlue700'),
    (Value: Integer($FF0277BD); Name: 'claLightBlue800'),
    (Value: Integer($FF01579B); Name: 'claLightBlue900'),
    (Value: Integer($FF80D8FF); Name: 'claLightBlueA100'),
    (Value: Integer($FF40C4FF); Name: 'claLightBlueA200'),
    (Value: Integer($FF00B0FF); Name: 'claLightBlueA400'),
    (Value: Integer($FF0091EA); Name: 'claLightBlueA700'),
    (Value: Integer($FFE0F7FA); Name: 'claCyan50'),
    (Value: Integer($FFB2EBF2); Name: 'claCyan100'),
    (Value: Integer($FF80DEEA); Name: 'claCyan200'),
    (Value: Integer($FF4DD0E1); Name: 'claCyan300'),
    (Value: Integer($FF26C6DA); Name: 'claCyan400'),
    (Value: Integer($FF00BCD4); Name: 'claCyan500'),
    (Value: Integer($FF00ACC1); Name: 'claCyan600'),
    (Value: Integer($FF0097A7); Name: 'claCyan700'),
    (Value: Integer($FF00838F); Name: 'claCyan800'),
    (Value: Integer($FF006064); Name: 'claCyan900'),
    (Value: Integer($FF84FFFF); Name: 'claCyanA100'),
    (Value: Integer($FF18FFFF); Name: 'claCyanA200'),
    (Value: Integer($FF00E5FF); Name: 'claCyanA400'),
    (Value: Integer($FF00B8D4); Name: 'claCyanA700'),
    (Value: Integer($FFE0F2F1); Name: 'claTeal50'),
    (Value: Integer($FFB2DFDB); Name: 'claTeal100'),
    (Value: Integer($FF80CBC4); Name: 'claTeal200'),
    (Value: Integer($FF4DB6AC); Name: 'claTeal300'),
    (Value: Integer($FF26A69A); Name: 'claTeal400'),
    (Value: Integer($FF009688); Name: 'claTeal500'),
    (Value: Integer($FF00897B); Name: 'claTeal600'),
    (Value: Integer($FF00796B); Name: 'claTeal700'),
    (Value: Integer($FF00695C); Name: 'claTeal800'),
    (Value: Integer($FF004D40); Name: 'claTeal900'),
    (Value: Integer($FFA7FFEB); Name: 'claTealA100'),
    (Value: Integer($FF64FFDA); Name: 'claTealA200'),
    (Value: Integer($FF1DE9B6); Name: 'claTealA400'),
    (Value: Integer($FF00BFA5); Name: 'claTealA700'),
    (Value: Integer($FFE8F5E9); Name: 'claGreen50'),
    (Value: Integer($FFC8E6C9); Name: 'claGreen100'),
    (Value: Integer($FFA5D6A7); Name: 'claGreen200'),
    (Value: Integer($FF81C784); Name: 'claGreen300'),
    (Value: Integer($FF66BB6A); Name: 'claGreen400'),
    (Value: Integer($FF4CAF50); Name: 'claGreen500'),
    (Value: Integer($FF43A047); Name: 'claGreen600'),
    (Value: Integer($FF388E3C); Name: 'claGreen700'),
    (Value: Integer($FF2E7D32); Name: 'claGreen800'),
    (Value: Integer($FF1B5E20); Name: 'claGreen900'),
    (Value: Integer($FFB9F6CA); Name: 'claGreenA100'),
    (Value: Integer($FF69F0AE); Name: 'claGreenA200'),
    (Value: Integer($FF00E676); Name: 'claGreenA400'),
    (Value: Integer($FF00C853); Name: 'claGreenA700'),
    (Value: Integer($FFF1F8E9); Name: 'claLightGreen50'),
    (Value: Integer($FFDCEDC8); Name: 'claLightGreen100'),
    (Value: Integer($FFC5E1A5); Name: 'claLightGreen200'),
    (Value: Integer($FFAED581); Name: 'claLightGreen300'),
    (Value: Integer($FF9CCC65); Name: 'claLightGreen400'),
    (Value: Integer($FF8BC34A); Name: 'claLightGreen500'),
    (Value: Integer($FF7CB342); Name: 'claLightGreen600'),
    (Value: Integer($FF689F38); Name: 'claLightGreen700'),
    (Value: Integer($FF558B2F); Name: 'claLightGreen800'),
    (Value: Integer($FF33691E); Name: 'claLightGreen900'),
    (Value: Integer($FFCCFF90); Name: 'claLightGreenA100'),
    (Value: Integer($FFB2FF59); Name: 'claLightGreenA200'),
    (Value: Integer($FF76FF03); Name: 'claLightGreenA400'),
    (Value: Integer($FF64DD17); Name: 'claLightGreenA700'),
    (Value: Integer($FFF9FBE7); Name: 'claLime50'),
    (Value: Integer($FFF0F4C3); Name: 'claLime100'),
    (Value: Integer($FFE6EE9C); Name: 'claLime200'),
    (Value: Integer($FFDCE775); Name: 'claLime300'),
    (Value: Integer($FFD4E157); Name: 'claLime400'),
    (Value: Integer($FFCDDC39); Name: 'claLime500'),
    (Value: Integer($FFC0CA33); Name: 'claLime600'),
    (Value: Integer($FFAFB42B); Name: 'claLime700'),
    (Value: Integer($FF9E9D24); Name: 'claLime800'),
    (Value: Integer($FF827717); Name: 'claLime900'),
    (Value: Integer($FFF4FF81); Name: 'claLimeA100'),
    (Value: Integer($FFEEFF41); Name: 'claLimeA200'),
    (Value: Integer($FFC6FF00); Name: 'claLimeA400'),
    (Value: Integer($FFAEEA00); Name: 'claLimeA700'),
    (Value: Integer($FFFFFDE7); Name: 'claYellow50'),
    (Value: Integer($FFFFF9C4); Name: 'claYellow100'),
    (Value: Integer($FFFFF59D); Name: 'claYellow200'),
    (Value: Integer($FFFFF176); Name: 'claYellow300'),
    (Value: Integer($FFFFEE58); Name: 'claYellow400'),
    (Value: Integer($FFFFEB3B); Name: 'claYellow500'),
    (Value: Integer($FFFDD835); Name: 'claYellow600'),
    (Value: Integer($FFFBC02D); Name: 'claYellow700'),
    (Value: Integer($FFF9A825); Name: 'claYellow800'),
    (Value: Integer($FFF57F17); Name: 'claYellow900'),
    (Value: Integer($FFFFFF8D); Name: 'claYellowA100'),
    (Value: Integer($FFFFFF00); Name: 'claYellowA200'),
    (Value: Integer($FFFFEA00); Name: 'claYellowA400'),
    (Value: Integer($FFFFD600); Name: 'claYellowA700'),
    (Value: Integer($FFFFF8E1); Name: 'claAmber50'),
    (Value: Integer($FFFFECB3); Name: 'claAmber100'),
    (Value: Integer($FFFFE082); Name: 'claAmber200'),
    (Value: Integer($FFFFD54F); Name: 'claAmber300'),
    (Value: Integer($FFFFCA28); Name: 'claAmber400'),
    (Value: Integer($FFFFC107); Name: 'claAmber500'),
    (Value: Integer($FFFFB300); Name: 'claAmber600'),
    (Value: Integer($FFFFA000); Name: 'claAmber700'),
    (Value: Integer($FFFF8F00); Name: 'claAmber800'),
    (Value: Integer($FFFF6F00); Name: 'claAmber900'),
    (Value: Integer($FFFFE57F); Name: 'claAmberA100'),
    (Value: Integer($FFFFD740); Name: 'claAmberA200'),
    (Value: Integer($FFFFC400); Name: 'claAmberA400'),
    (Value: Integer($FFFFAB00); Name: 'claAmberA700'),
    (Value: Integer($FFFFF3E0); Name: 'claOrange50'),
    (Value: Integer($FFFFE0B2); Name: 'claOrange100'),
    (Value: Integer($FFFFCC80); Name: 'claOrange200'),
    (Value: Integer($FFFFB74D); Name: 'claOrange300'),
    (Value: Integer($FFFFA726); Name: 'claOrange400'),
    (Value: Integer($FFFF9800); Name: 'claOrange500'),
    (Value: Integer($FFFB8C00); Name: 'claOrange600'),
    (Value: Integer($FFF57C00); Name: 'claOrange700'),
    (Value: Integer($FFEF6C00); Name: 'claOrange800'),
    (Value: Integer($FFE65100); Name: 'claOrange900'),
    (Value: Integer($FFFFD180); Name: 'claOrangeA100'),
    (Value: Integer($FFFFAB40); Name: 'claOrangeA200'),
    (Value: Integer($FFFF9100); Name: 'claOrangeA400'),
    (Value: Integer($FFFF6D00); Name: 'claOrangeA700'),
    (Value: Integer($FFEFEBE9); Name: 'claBrown50'),
    (Value: Integer($FFD7CCC8); Name: 'claBrown100'),
    (Value: Integer($FFBCAAA4); Name: 'claBrown200'),
    (Value: Integer($FFA1887F); Name: 'claBrown300'),
    (Value: Integer($FF8D6E63); Name: 'claBrown400'),
    (Value: Integer($FF795548); Name: 'claBrown500'),
    (Value: Integer($FF6D4C41); Name: 'claBrown600'),
    (Value: Integer($FF5D4037); Name: 'claBrown700'),
    (Value: Integer($FF4E342E); Name: 'claBrown800'),
    (Value: Integer($FF3E2723); Name: 'claBrown900'),
    (Value: Integer($FFFAFAFA); Name: 'claGrey50'),
    (Value: Integer($FFF5F5F5); Name: 'claGrey100'),
    (Value: Integer($FFEEEEEE); Name: 'claGrey200'),
    (Value: Integer($FFE0E0E0); Name: 'claGrey300'),
    (Value: Integer($FFBDBDBD); Name: 'claGrey400'),
    (Value: Integer($FF9E9E9E); Name: 'claGrey500'),
    (Value: Integer($FF757575); Name: 'claGrey600'),
    (Value: Integer($FF616161); Name: 'claGrey700'),
    (Value: Integer($FF424242); Name: 'claGrey800'),
    (Value: Integer($FF212121); Name: 'claGrey900'),
    (Value: Integer($FFECEFF1); Name: 'claBlueGrey50'),
    (Value: Integer($FFCFD8DC); Name: 'claBlueGrey100'),
    (Value: Integer($FFB0BEC5); Name: 'claBlueGrey200'),
    (Value: Integer($FF90A4AE); Name: 'claBlueGrey300'),
    (Value: Integer($FF78909C); Name: 'claBlueGrey400'),
    (Value: Integer($FF607D8B); Name: 'claBlueGrey500'),
    (Value: Integer($FF546E7A); Name: 'claBlueGrey600'),
    (Value: Integer($FF455A64); Name: 'claBlueGrey700'),
    (Value: Integer($FF37474F); Name: 'claBlueGrey800'),
    (Value: Integer($FF263238); Name: 'claBlueGrey900'),
    (Value: Integer($FF000000); Name: 'claBlack'),
    (Value: Integer($FFFFFFFF); Name: 'claWhite')
    );

type

  PMaterialColor = ^TMaterialColor;
  TMaterialColor = type Cardinal;

  PMaterialColorRec = ^TMaterialColorRec;

  TMaterialColorMapEntry = record
    Value: TMaterialColor;
    Name: string;
  end;

  TRTLMaterialColors = class
  strict private
    FMaterialColors: array of TMaterialColorMapEntry;
    procedure GetMaterialColorValuesProc(const AMaterialColorName: string);
    function GetMaterialColor(AIndex: Integer): TMaterialColorMapEntry;
  public
    constructor Create;
    function Count: Integer;
    property MaterialColor[Index: Integer]: TMaterialColorMapEntry read GetMaterialColor;
  end;

  TMaterialColorRec = record
  const
    Alpha = TMaterialColor($FF000000);
    Red50 = Alpha or TMaterialColor($FFFFEBEE);
    Red100 = Alpha or TMaterialColor($FFFFCDD2);
    Red200 = Alpha or TMaterialColor($FFEF9A9A);
    Red300 = Alpha or TMaterialColor($FFE57373);
    Red400 = Alpha or TMaterialColor($FFEF5350);
    Red500 = Alpha or TMaterialColor($FFF44336);
    Red600 = Alpha or TMaterialColor($FFE53935);
    Red700 = Alpha or TMaterialColor($FFD32F2F);
    Red800 = Alpha or TMaterialColor($FFC62828);
    Red900 = Alpha or TMaterialColor($FFB71C1C);
    RedA100 = Alpha or TMaterialColor($FFFF8A80);
    RedA200 = Alpha or TMaterialColor($FFFF5252);
    RedA400 = Alpha or TMaterialColor($FFFF1744);
    RedA700 = Alpha or TMaterialColor($FFD50000);

    Pink50 = Alpha or TMaterialColor($FFFCE4EC);
    Pink100 = Alpha or TMaterialColor($FFF8BBD0);
    Pink200 = Alpha or TMaterialColor($FFF48FB1);
    Pink300 = Alpha or TMaterialColor($FFF06292);
    Pink400 = Alpha or TMaterialColor($FFEC407A);
    Pink500 = Alpha or TMaterialColor($FFE91E63);
    Pink600 = Alpha or TMaterialColor($FFD81B60);
    Pink700 = Alpha or TMaterialColor($FFC2185B);
    Pink800 = Alpha or TMaterialColor($FFAD1457);
    Pink900 = Alpha or TMaterialColor($FF880E4F);
    PinkA100 = Alpha or TMaterialColor($FFFF80AB);
    PinkA200 = Alpha or TMaterialColor($FFFF4081);
    PinkA400 = Alpha or TMaterialColor($FFF50057);
    PinkA700 = Alpha or TMaterialColor($FFC51162);

    Purple50 = Alpha or TMaterialColor($FFF3E5F5);
    Purple100 = Alpha or TMaterialColor($FFE1BEE7);
    Purple200 = Alpha or TMaterialColor($FFCE93D8);
    Purple300 = Alpha or TMaterialColor($FFBA68C8);
    Purple400 = Alpha or TMaterialColor($FFAB47BC);
    Purple500 = Alpha or TMaterialColor($FF9C27B0);
    Purple600 = Alpha or TMaterialColor($FF8E24AA);
    Purple700 = Alpha or TMaterialColor($FF7B1FA2);
    Purple800 = Alpha or TMaterialColor($FF6A1B9A);
    Purple900 = Alpha or TMaterialColor($FF4A148C);
    PurpleA100 = Alpha or TMaterialColor($FFEA80FC);
    PurpleA200 = Alpha or TMaterialColor($FFE040FB);
    PurpleA400 = Alpha or TMaterialColor($FFD500F9);
    PurpleA700 = Alpha or TMaterialColor($FFAA00FF);

    DeepPurple50 = Alpha or TMaterialColor($FFEDE7F6);
    DeepPurple100 = Alpha or TMaterialColor($FFD1C4E9);
    DeepPurple200 = Alpha or TMaterialColor($FFB39DDB);
    DeepPurple300 = Alpha or TMaterialColor($FF9575CD);
    DeepPurple400 = Alpha or TMaterialColor($FF7E57C2);
    DeepPurple500 = Alpha or TMaterialColor($FF673AB7);
    DeepPurple600 = Alpha or TMaterialColor($FF5E35B1);
    DeepPurple700 = Alpha or TMaterialColor($FF512DA8);
    DeepPurple800 = Alpha or TMaterialColor($FF4527A0);
    DeepPurple900 = Alpha or TMaterialColor($FF311B92);
    DeepPurpleA100 = Alpha or TMaterialColor($FFB388FF);
    DeepPurpleA200 = Alpha or TMaterialColor($FF7C4DFF);
    DeepPurpleA400 = Alpha or TMaterialColor($FF651FFF);
    DeepPurpleA700 = Alpha or TMaterialColor($FF6200EA);

    Indigo50 = Alpha or TMaterialColor($FFE8EAF6);
    Indigo100 = Alpha or TMaterialColor($FFC5CAE9);
    Indigo200 = Alpha or TMaterialColor($FF9FA8DA);
    Indigo300 = Alpha or TMaterialColor($FF7986CB);
    Indigo400 = Alpha or TMaterialColor($FF5C6BC0);
    Indigo500 = Alpha or TMaterialColor($FF3F51B5);
    Indigo600 = Alpha or TMaterialColor($FF3949AB);
    Indigo700 = Alpha or TMaterialColor($FF303F9F);
    Indigo800 = Alpha or TMaterialColor($FF283593);
    Indigo900 = Alpha or TMaterialColor($FF1A237E);
    IndigoA100 = Alpha or TMaterialColor($FF8C9EFF);
    IndigoA200 = Alpha or TMaterialColor($FF536DFE);
    IndigoA400 = Alpha or TMaterialColor($FF3D5AFE);
    IndigoA700 = Alpha or TMaterialColor($FF304FFE);

    Blue50 = Alpha or TMaterialColor($FFE3F2FD);
    Blue100 = Alpha or TMaterialColor($FFBBDEFB);
    Blue200 = Alpha or TMaterialColor($FF90CAF9);
    Blue300 = Alpha or TMaterialColor($FF64B5F6);
    Blue400 = Alpha or TMaterialColor($FF42A5F5);
    Blue500 = Alpha or TMaterialColor($FF2196F3);
    Blue600 = Alpha or TMaterialColor($FF1E88E5);
    Blue700 = Alpha or TMaterialColor($FF1976D2);
    Blue800 = Alpha or TMaterialColor($FF1565C0);
    Blue900 = Alpha or TMaterialColor($FF0D47A1);
    BlueA100 = Alpha or TMaterialColor($FF82B1FF);
    BlueA200 = Alpha or TMaterialColor($FF448AFF);
    BlueA400 = Alpha or TMaterialColor($FF2979FF);
    BlueA700 = Alpha or TMaterialColor($FF2962FF);

    LightBlue50 = Alpha or TMaterialColor($FFE1F5FE);
    LightBlue100 = Alpha or TMaterialColor($FFB3E5FC);
    LightBlue200 = Alpha or TMaterialColor($FF81D4FA);
    LightBlue300 = Alpha or TMaterialColor($FF4FC3F7);
    LightBlue400 = Alpha or TMaterialColor($FF29B6F6);
    LightBlue500 = Alpha or TMaterialColor($FF03A9F4);
    LightBlue600 = Alpha or TMaterialColor($FF039BE5);
    LightBlue700 = Alpha or TMaterialColor($FF0288D1);
    LightBlue800 = Alpha or TMaterialColor($FF0277BD);
    LightBlue900 = Alpha or TMaterialColor($FF01579B);
    LightBlueA100 = Alpha or TMaterialColor($FF80D8FF);
    LightBlueA200 = Alpha or TMaterialColor($FF40C4FF);
    LightBlueA400 = Alpha or TMaterialColor($FF00B0FF);
    LightBlueA700 = Alpha or TMaterialColor($FF0091EA);

    Cyan50 = Alpha or TMaterialColor($FFE0F7FA);
    Cyan100 = Alpha or TMaterialColor($FFB2EBF2);
    Cyan200 = Alpha or TMaterialColor($FF80DEEA);
    Cyan300 = Alpha or TMaterialColor($FF4DD0E1);
    Cyan400 = Alpha or TMaterialColor($FF26C6DA);
    Cyan500 = Alpha or TMaterialColor($FF00BCD4);
    Cyan600 = Alpha or TMaterialColor($FF00ACC1);
    Cyan700 = Alpha or TMaterialColor($FF0097A7);
    Cyan800 = Alpha or TMaterialColor($FF00838F);
    Cyan900 = Alpha or TMaterialColor($FF006064);
    CyanA100 = Alpha or TMaterialColor($FF84FFFF);
    CyanA200 = Alpha or TMaterialColor($FF18FFFF);
    CyanA400 = Alpha or TMaterialColor($FF00E5FF);
    CyanA700 = Alpha or TMaterialColor($FF00B8D4);

    Teal50 = Alpha or TMaterialColor($FFE0F2F1);
    Teal100 = Alpha or TMaterialColor($FFB2DFDB);
    Teal200 = Alpha or TMaterialColor($FF80CBC4);
    Teal300 = Alpha or TMaterialColor($FF4DB6AC);
    Teal400 = Alpha or TMaterialColor($FF26A69A);
    Teal500 = Alpha or TMaterialColor($FF009688);
    Teal600 = Alpha or TMaterialColor($FF00897B);
    Teal700 = Alpha or TMaterialColor($FF00796B);
    Teal800 = Alpha or TMaterialColor($FF00695C);
    Teal900 = Alpha or TMaterialColor($FF004D40);
    TealA100 = Alpha or TMaterialColor($FFA7FFEB);
    TealA200 = Alpha or TMaterialColor($FF64FFDA);
    TealA400 = Alpha or TMaterialColor($FF1DE9B6);
    TealA700 = Alpha or TMaterialColor($FF00BFA5);

    Green50 = Alpha or TMaterialColor($FFE8F5E9);
    Green100 = Alpha or TMaterialColor($FFC8E6C9);
    Green200 = Alpha or TMaterialColor($FFA5D6A7);
    Green300 = Alpha or TMaterialColor($FF81C784);
    Green400 = Alpha or TMaterialColor($FF66BB6A);
    Green500 = Alpha or TMaterialColor($FF4CAF50);
    Green600 = Alpha or TMaterialColor($FF43A047);
    Green700 = Alpha or TMaterialColor($FF388E3C);
    Green800 = Alpha or TMaterialColor($FF2E7D32);
    Green900 = Alpha or TMaterialColor($FF1B5E20);
    GreenA100 = Alpha or TMaterialColor($FFB9F6CA);
    GreenA200 = Alpha or TMaterialColor($FF69F0AE);
    GreenA400 = Alpha or TMaterialColor($FF00E676);
    GreenA700 = Alpha or TMaterialColor($FF00C853);

    LightGreen50 = Alpha or TMaterialColor($FFF1F8E9);
    LightGreen100 = Alpha or TMaterialColor($FFDCEDC8);
    LightGreen200 = Alpha or TMaterialColor($FFC5E1A5);
    LightGreen300 = Alpha or TMaterialColor($FFAED581);
    LightGreen400 = Alpha or TMaterialColor($FF9CCC65);
    LightGreen500 = Alpha or TMaterialColor($FF8BC34A);
    LightGreen600 = Alpha or TMaterialColor($FF7CB342);
    LightGreen700 = Alpha or TMaterialColor($FF689F38);
    LightGreen800 = Alpha or TMaterialColor($FF558B2F);
    LightGreen900 = Alpha or TMaterialColor($FF33691E);
    LightGreenA100 = Alpha or TMaterialColor($FFCCFF90);
    LightGreenA200 = Alpha or TMaterialColor($FFB2FF59);
    LightGreenA400 = Alpha or TMaterialColor($FF76FF03);
    LightGreenA700 = Alpha or TMaterialColor($FF64DD17);

    Lime50 = Alpha or TMaterialColor($FFF9FBE7);
    Lime100 = Alpha or TMaterialColor($FFF0F4C3);
    Lime200 = Alpha or TMaterialColor($FFE6EE9C);
    Lime300 = Alpha or TMaterialColor($FFDCE775);
    Lime400 = Alpha or TMaterialColor($FFD4E157);
    Lime500 = Alpha or TMaterialColor($FFCDDC39);
    Lime600 = Alpha or TMaterialColor($FFC0CA33);
    Lime700 = Alpha or TMaterialColor($FFAFB42B);
    Lime800 = Alpha or TMaterialColor($FF9E9D24);
    Lime900 = Alpha or TMaterialColor($FF827717);
    LimeA100 = Alpha or TMaterialColor($FFF4FF81);
    LimeA200 = Alpha or TMaterialColor($FFEEFF41);
    LimeA400 = Alpha or TMaterialColor($FFC6FF00);
    LimeA700 = Alpha or TMaterialColor($FFAEEA00);

    Yellow50 = Alpha or TMaterialColor($FFFFFDE7);
    Yellow100 = Alpha or TMaterialColor($FFFFF9C4);
    Yellow200 = Alpha or TMaterialColor($FFFFF59D);
    Yellow300 = Alpha or TMaterialColor($FFFFF176);
    Yellow400 = Alpha or TMaterialColor($FFFFEE58);
    Yellow500 = Alpha or TMaterialColor($FFFFEB3B);
    Yellow600 = Alpha or TMaterialColor($FFFDD835);
    Yellow700 = Alpha or TMaterialColor($FFFBC02D);
    Yellow800 = Alpha or TMaterialColor($FFF9A825);
    Yellow900 = Alpha or TMaterialColor($FFF57F17);
    YellowA100 = Alpha or TMaterialColor($FFFFFF8D);
    YellowA200 = Alpha or TMaterialColor($FFFFFF00);
    YellowA400 = Alpha or TMaterialColor($FFFFEA00);
    YellowA700 = Alpha or TMaterialColor($FFFFD600);

    Amber50 = Alpha or TMaterialColor($FFFFF8E1);
    Amber100 = Alpha or TMaterialColor($FFFFECB3);
    Amber200 = Alpha or TMaterialColor($FFFFE082);
    Amber300 = Alpha or TMaterialColor($FFFFD54F);
    Amber400 = Alpha or TMaterialColor($FFFFCA28);
    Amber500 = Alpha or TMaterialColor($FFFFC107);
    Amber600 = Alpha or TMaterialColor($FFFFB300);
    Amber700 = Alpha or TMaterialColor($FFFFA000);
    Amber800 = Alpha or TMaterialColor($FFFF8F00);
    Amber900 = Alpha or TMaterialColor($FFFF6F00);
    AmberA100 = Alpha or TMaterialColor($FFFFE57F);
    AmberA200 = Alpha or TMaterialColor($FFFFD740);
    AmberA400 = Alpha or TMaterialColor($FFFFC400);
    AmberA700 = Alpha or TMaterialColor($FFFFAB00);

    Orange50 = Alpha or TMaterialColor($FFFFF3E0);
    Orange100 = Alpha or TMaterialColor($FFFFE0B2);
    Orange200 = Alpha or TMaterialColor($FFFFCC80);
    Orange300 = Alpha or TMaterialColor($FFFFB74D);
    Orange400 = Alpha or TMaterialColor($FFFFA726);
    Orange500 = Alpha or TMaterialColor($FFFF9800);
    Orange600 = Alpha or TMaterialColor($FFFB8C00);
    Orange700 = Alpha or TMaterialColor($FFF57C00);
    Orange800 = Alpha or TMaterialColor($FFEF6C00);
    Orange900 = Alpha or TMaterialColor($FFE65100);
    OrangeA100 = Alpha or TMaterialColor($FFFFD180);
    OrangeA200 = Alpha or TMaterialColor($FFFFAB40);
    OrangeA400 = Alpha or TMaterialColor($FFFF9100);
    OrangeA700 = Alpha or TMaterialColor($FFFF6D00);

    Brown50 = Alpha or TMaterialColor($FFEFEBE9);
    Brown100 = Alpha or TMaterialColor($FFD7CCC8);
    Brown200 = Alpha or TMaterialColor($FFBCAAA4);
    Brown300 = Alpha or TMaterialColor($FFA1887F);
    Brown400 = Alpha or TMaterialColor($FF8D6E63);
    Brown500 = Alpha or TMaterialColor($FF795548);
    Brown600 = Alpha or TMaterialColor($FF6D4C41);
    Brown700 = Alpha or TMaterialColor($FF5D4037);
    Brown800 = Alpha or TMaterialColor($FF4E342E);
    Brown900 = Alpha or TMaterialColor($FF3E2723);

    Grey50 = Alpha or TMaterialColor($FFFAFAFA);
    Grey100 = Alpha or TMaterialColor($FFF5F5F5);
    Grey200 = Alpha or TMaterialColor($FFEEEEEE);
    Grey300 = Alpha or TMaterialColor($FFE0E0E0);
    Grey400 = Alpha or TMaterialColor($FFBDBDBD);
    Grey500 = Alpha or TMaterialColor($FF9E9E9E);
    Grey600 = Alpha or TMaterialColor($FF757575);
    Grey700 = Alpha or TMaterialColor($FF616161);
    Grey800 = Alpha or TMaterialColor($FF424242);
    Grey900 = Alpha or TMaterialColor($FF212121);

    BlueGrey50 = Alpha or TMaterialColor($FFECEFF1);
    BlueGrey100 = Alpha or TMaterialColor($FFCFD8DC);
    BlueGrey200 = Alpha or TMaterialColor($FFB0BEC5);
    BlueGrey300 = Alpha or TMaterialColor($FF90A4AE);
    BlueGrey400 = Alpha or TMaterialColor($FF78909C);
    BlueGrey500 = Alpha or TMaterialColor($FF607D8B);
    BlueGrey600 = Alpha or TMaterialColor($FF546E7A);
    BlueGrey700 = Alpha or TMaterialColor($FF455A64);
    BlueGrey800 = Alpha or TMaterialColor($FF37474F);
    BlueGrey900 = Alpha or TMaterialColor($FF263238);

    Black = Alpha or TMaterialColor($FF000000);
    White = Alpha or TMaterialColor($FFFFFFFF);

    Null = TMaterialColor($00000000);
    constructor Create(const Color: TMaterialColor);

    class var
      ColorToRGB: function(Color: TMaterialColor): Longint;
    case LongWord of
      0:
        (Color: TMaterialColor);
      2:
        (HiWord, LoWord: Word);
      3:
{$IFDEF BIGENDIAN}
        (A, R, G, B: System.Byte);
{$ELSE}
        (B, G, R, A: System.Byte);
{$ENDIF}
  end;

  TMaterialColors = TMaterialColorRec;

var
  MaterialColorsMap: TRTLMaterialColors;

procedure GetMaterialColorValues(Proc: TGetStrProc);
function StringToMaterialColor(const Value: string): TMaterialColor;
function IdentToMaterialColor(const Ident: string; var MaterialColor: Integer): Boolean;
function MaterialColorToIdent(MaterialColor: Integer; var Ident: string): Boolean;
function MaterialColorToString(Value: TMaterialColor): string;

implementation

{ TMaterialColorRec }

procedure GetMaterialColorValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := Low(MaterialColors) to High(MaterialColors) do
    Proc(MaterialColorToString(TMaterialColor(MaterialColors[I].Value)));
end;

function StringToMaterialColor(const Value: string): TMaterialColor;
var
  LValue: string;
  LMaterialColor: Integer;
begin
  LValue := Value;
  if LValue = #0 then
    LValue := '$0'
  else if (LValue <> '') and ((LValue.Chars[0] = '#') or (LValue.Chars[0] = 'x')) then
    LValue := '$' + LValue.SubString(1);

  if (not IdentToMaterialColor('cla' + LValue, LMaterialColor)) and (not IdentToMaterialColor(LValue, LMaterialColor)) then
    Result := TMaterialColor(StrToInt64(LValue))
  else
    Result := TMaterialColor(LMaterialColor);
end;

function IdentToMaterialColor(const Ident: string; var MaterialColor: Integer): Boolean;
var
  LIdent: string;
begin
  LIdent := Ident;
  if (LIdent.Length > 0) and (LIdent.Chars[0] = 'x') then
  begin
    MaterialColor := Integer(StringToMaterialColor(LIdent));
    Result := True;
  end
  else
    Result := IdentToInt(LIdent, MaterialColor, MaterialColors);
  // Allow "clXXXX" constants and convert it to TMaterialColor
  if not Result and (LIdent.Length > 3) then
  begin
    LIdent := LIdent.Insert(2, 'a');
    Result := IdentToInt(LIdent, Integer(MaterialColor), MaterialColors);
  end;
end;

function MaterialColorToIdent(MaterialColor: Integer; var Ident: string): Boolean;
begin
  Result := IntToIdent(MaterialColor, Ident, MaterialColors);
  if not Result then
  begin
    Ident := 'x' + IntToHex(MaterialColor, 8);
    Result := True;
  end;
end;

function MaterialColorToString(Value: TMaterialColor): string;
begin
  MaterialColorToIdent(Integer(Value), Result);
  if Result.Chars[0] = 'x' then
    Result := '#' + Result.SubString(1)
  else
    Result := Result.Remove(0, 3);
end;

constructor TMaterialColorRec.Create(const Color: TMaterialColor);
begin
  Self := TMaterialColorRec(Color);
end;

{ TRTLMaterialColors }

function TRTLMaterialColors.Count: Integer;
begin
  Result := Length(FMaterialColors);
end;

constructor TRTLMaterialColors.Create;
begin
  GetMaterialColorValues(GetMaterialColorValuesProc);
end;

function TRTLMaterialColors.GetMaterialColor(AIndex: Integer): TMaterialColorMapEntry;
begin
  Result := FMaterialColors[AIndex];
end;

procedure TRTLMaterialColors.GetMaterialColorValuesProc(const AMaterialColorName: string);
var
  LNewIndex: Integer;
begin
  LNewIndex := Count;
  SetLength(FMaterialColors, LNewIndex + 1);
  FMaterialColors[LNewIndex].Name := AMaterialColorName;
  FMaterialColors[LNewIndex].Value := StringToMaterialColor(AMaterialColorName);
end;

end.
