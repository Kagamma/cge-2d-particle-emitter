unit Castle2DParticleEmitter;

{$MODE DELPHI}
{$COPERATORS ON}

interface

uses
  Classes, SysUtils,
  Castle3D, CastleSceneCore, Castle2DSceneManager, CastleComponentSerialize,
  CastleVectors, Generics.Collections,
  X3DNodes;

type
  TCastleEmitterType = (etGravity, etRadial);

  { 2D particle struct to hold current particle settings. }
  PCastle2DParticle = ^TCastle2DParticle;
  TCastle2DParticle = record
    TimeToLive: single;
    Position: TVector2;
    Size,
    SizeDelta,
    Rotation,
    RotationDelta: single;
    Color,
    ColorDelta: TVector4;
    { Gravity parameters }
    StartPos,
    Velocity: TVector2;
    RadialAcceleration,
    TangentialAcceleration: single;
    { Radial parameters }
    EmitRadius,
    EmitRadiusDelta,
    EmitRotation,
    EmitRotationDelta: single;
  end;

  TCastle2DParticleList = class(TList<TCastle2DParticle>)
  public
    function Ptr(const APos: integer): PCastle2DParticle;
  end;

  TCastle2DParticleBlendDict = TDictionary<integer, string>;

  { This class acts as a place holder for effects. }
  TCastle2DParticleEffect = class
  private
    FTexture: string;
    FSourcePosition,
    FSourcePositionVariance: TVector2;
    FSpeed,
    FSpeedVariance,
    FParticleLifeSpan,
    FParticleLifeSpanVariance,
    FAngle,
    FAngleVariance: single;
    FGravity: TVector2;
    FRadialAcceleration,
    FTangentialAcceleration,
    FRadialAccelVariance,
    FTangentialAccelVariance: single;
    FStartColor,
    FStartColorVariance,
    FFinishColor,
    FFinishColorVariance: TVector4;
    FMaxParticles: integer;
    FStartParticleSize,
    FStartParticleSizeVariance,
    FFinishParticleSize,
    FFinishParticleSizeVariance,
    FDuration: single;
    FEmitterType: TCastleEmitterType;
    FMaxRadius,
    FMaxRadiusVariance,
    FMinRadius,
    FMinRadiusVariance,
    FRotatePerSecond,
    FRotatePerSecondVariance: single;
    FBlendFuncSource,
    FBlendFuncDestination: integer;
    FRotationStart,
    FRotationStartVariance,
    FRotationEnd,
    FRotationEndVariance: single;
  public
    { Clone all attributes of this effect to another one. Good if we want to
      load the effect from file just once and then apply it to other emitters
      so all emitters have the same effect. }
    procedure Clone(var ATarget: TCastle2DParticleEffect);
    { Load particle attributes in .PEX format. }
    procedure Load(const AURL: string);

    property Texture: string read FTexture write FTexture;
    property SourcePosition: TVector2 read FSourcePosition write FSourcePosition;
    property SourcePositionVariance: TVector2 read FSourcePositionVariance write FSourcePositionVariance;
    property Speed: single read FSpeed write FSpeed;
    property SpeedVariance: single read FSpeedVariance write FSpeedVariance;
    property ParticleLifeSpan: single read FParticleLifeSpan write FParticleLifeSpan;
    property ParticleLifeSpanVariance: single read FParticleLifeSpanVariance write FParticleLifeSpanVariance;
    property Angle: single read FAngle write FAngle;
    property AngleVariance: single read FAngleVariance write FAngleVariance;
    property Gravity: TVector2 read FGravity write FGravity;
    property RadialAcceleration: single read FRadialAcceleration write FRadialAcceleration;
    property TangentialAcceleration: single read FTangentialAcceleration write FTangentialAcceleration;
    property RadialAccelVariance: single read FRadialAccelVariance write FRadialAccelVariance;
    property TangentialAccelVariance: single read FTangentialAccelVariance write FTangentialAccelVariance;
    property StartColor: TVector4 read FStartColor write FStartColor;
    property StartColorVariance: TVector4 read FStartColorVariance write FStartColorVariance;
    property FinishColor: TVector4 read FFinishColor write FFinishColor;
    property FinishColorVariance: TVector4 read FFinishColorVariance write FFinishColorVariance;
    property MaxParticles: integer read FMaxParticles write FMaxParticles;
    property StartParticleSize: single read FStartParticleSize write FStartParticleSize;
    property StartParticleSizeVariance: single read FStartParticleSizeVariance write FStartParticleSizeVariance;
    property FinishParticleSize: single read FFinishParticleSize write FFinishParticleSize;
    property FinishParticleSizeVariance: single read FFinishParticleSizeVariance write FFinishParticleSizeVariance;
    property Duration: single read FDuration write FDuration;
    property EmitterType: TCastleEmitterType read FEmitterType write FEmitterType;
    property MaxRadius: single read FMaxRadius write FMaxRadius;
    property MaxRadiusVariance: single read FMaxRadiusVariance write FMaxRadiusVariance;
    property MinRadius: single read FMinRadius write FMinRadius;
    property MinRadiusVariance: single read FMinRadiusVariance write FMinRadiusVariance;
    property RotatePerSecond: single read FRotatePerSecond write FRotatePerSecond;
    property RotatePerSecondVariance: single read FRotatePerSecondVariance write FRotatePerSecondVariance;
    property BlendFuncSource: integer read FBlendFuncSource write FBlendFuncSource;
    property BlendFuncDestination: integer read FBlendFuncDestination write FBlendFuncDestination;
    property RotationStart: single read FRotationStart write FRotationStart;
    property RotationStartVariance: single read FRotationStartVariance write FRotationStartVariance;
    property RotationEnd: single read FRotationEnd write FRotationEnd;
    property RotationEndVariance: single read FRotationEndVariance write FRotationEndVariance;
  end;

  { 2D particle emitter for CGE. }
  TCastle2DParticleEmitter = class(TCastle2DScene)
  private
    FURL: string;
    FStartEmitting: Boolean;
    FEffect: TCastle2DParticleEffect;
    FCoordNode: TCoordinateNode;
    FColorNode: TColorRGBANode;
    FTexCoordNode: TTextureCoordinateNode;
    FImageTexNode: TImageTextureNode;
    FBlendModeNode: TBlendModeNode;
    FParticleCount: integer;
    FParticleList: TCastle2DParticleList;
    { The value is in miliseconds. Set it to -1 for infinite emitting, 0 to
      stop the emitter and positive value for cooldown. }
    FEmissionTime,
    FEmitParticleTime: single;
    { When this is set to true, the emitter will automatically freed after
      all particles destroyed. }
    FReleaseWhenDone: boolean;

    function EmitParticle: boolean;
    procedure UpdateParticle(const P: PCastle2DParticle; ATimeStep: single);
    procedure InitNodeTree;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: single; var RemoveMe: TRemoveType); override;
    { This method will free the current Effect if any, init a new FEffect and
      load settings from .PEX file. }
    procedure LoadPEX(const AURL: string); overload;
    { If AOwnEffect = false we will remove old Effect and replace it with
      AEffect, otherwise we will clone AEffect attributes to Effect (auto-init
      if needed). }
    procedure LoadPEX(const AEffect: TCastle2DParticleEffect;
        const AOwnEffect: Boolean = true); overload;
    { Refresh the emitter according to the change from effect. Normally we dont
      need to explicitly call it unless we make changes in Effect's Texture,
      Duration, BlendFunc and/or MaxParticles. }
    procedure RefreshEffect;

    property Effect: TCastle2DParticleEffect read FEffect;
    property ParticleCount: integer read FParticleCount;
    property ReleaseWhenDone: boolean read FReleaseWhenDone write FReleaseWhenDone;
    property EmissionTime: single read FEmissionTime write FEmissionTime;
  published
    { URL of a .pex file. This will call LoadPEX to load particle effect }
    property URL: string read FURL write LoadPEX;
    { If true, the emitter will start emitting }
    property StartEmitting: Boolean read FStartEmitting write FStartEmitting default False;
  end;

implementation

uses
  DOM, XMLRead, XPath,
  Math,
  CastleDownload, CastleURIUtils;

const
  TexCoordBatch: array [0..5] of TVector2 =
    ((Data: (0, 1)), (Data: (0, 0)), (Data: (1, 0)),
     (Data: (0, 1)), (Data: (1, 0)), (Data: (1, 1)));

var
  BlendDict: TCastle2DParticleBlendDict;

function TCastle2DParticleList.Ptr(const APos: integer): PCastle2DParticle;
begin
  Result := @FItems[APos];
end;

procedure TCastle2DParticleEffect.Clone(var ATarget: TCastle2DParticleEffect);
begin
  if not Assigned(ATarget) then
    ATarget := TCastle2DParticleEffect.Create;
  ATarget.Texture := Texture;
  ATarget.SourcePosition := SourcePosition;
  ATarget.SourcePositionVariance := SourcePositionVariance;
  ATarget.Speed := Speed;
  ATarget.SpeedVariance := SpeedVariance;
  ATarget.ParticleLifeSpan := ParticleLifeSpan;
  ATarget.ParticleLifeSpanVariance := ParticleLifeSpanVariance;
  ATarget.Angle := Angle;
  ATarget.AngleVariance := AngleVariance;
  ATarget.Gravity := Gravity;
  ATarget.RadialAcceleration := RadialAcceleration;
  ATarget.TangentialAcceleration := TangentialAcceleration;
  ATarget.RadialAccelVariance := RadialAccelVariance;
  ATarget.TangentialAccelVariance := TangentialAccelVariance;
  ATarget.StartColor := StartColor;
  ATarget.StartColorVariance := StartColorVariance;
  ATarget.FinishColor := FinishColor;
  ATarget.FinishColorVariance := FinishColorVariance;
  ATarget.MaxParticles := MaxParticles;
  ATarget.StartParticleSize := StartParticleSize;
  ATarget.StartParticleSizeVariance := StartParticleSizeVariance;
  ATarget.FinishParticleSize := FinishParticleSize;
  ATarget.FinishParticleSizeVariance := FinishParticleSizeVariance;
  ATarget.Duration := Duration;
  ATarget.EmitterType := EmitterType;
  ATarget.MaxRadius := MaxRadius;
  ATarget.MaxRadiusVariance := MaxRadiusVariance;
  ATarget.MinRadius := MinRadius;
  ATarget.MinRadiusVariance := MinRadiusVariance;
  ATarget.RotatePerSecond := RotatePerSecond;
  ATarget.RotatePerSecondVariance := RotatePerSecondVariance;
  ATarget.BlendFuncSource := BlendFuncSource;
  ATarget.BlendFuncDestination := BlendFuncDestination;
  ATarget.RotationStart := RotationStart;
  ATarget.RotationStartVariance := RotationStartVariance;
  ATarget.RotationEnd := RotationEnd;
  ATarget.RotationEndVariance := RotationEndVariance;
end;

procedure TCastle2DParticleEffect.Load(const AURL: string);
var
  Doc: TXMLDocument;
  Stream: TStream;

  function XPath(const AXPath: DOMString; const ADOMNode: TDOMNode): TXPathVariable;
  begin
    Result := EvaluateXPathExpression(AXPath, ADOMNode);
  end;
  function XPathAsNumber(const AXPath: DOMString; const ADOMNode: TDOMNode): Extended;
  var
    variable: TXPathVariable;
  begin
    variable := XPath(AXPath, ADOMNode);
    Result := variable.AsNumber;
    variable.Free;
  end;
  function XPathAsText(const AXPath: DOMString; const ADOMNode: TDOMNode): string;
  var
    variable: TXPathVariable;
  begin
    variable := XPath(AXPath, ADOMNode);
    Result := string(variable.AsText);
    variable.Free;
  end;

begin
  try
    Stream := Download(AURL);
    ReadXMLFile(Doc, Stream);
    FTexture :=
        ExtractURIPath(AURL) + XPathAsText('//texture/@name', Doc);
    { We ignored source position as we dont need it. The scene itself should
      attach to a T3DTransform for positioning. }
    FSourcePosition := Vector2(
        //XPath('//sourcePosition/@x', Doc).AsNumber,
        //XPath('//sourcePosition/@y', Doc).AsNumber
        0, 0
    );
    FSourcePositionVariance := Vector2(
        XPathAsNumber('//sourcePositionVariance/@x', Doc),
        XPathAsNumber('//sourcePositionVariance/@y', Doc)
    );
    FSpeed :=
        XPathAsNumber('//speed/@value', Doc);
    FSpeedVariance :=
        XPathAsNumber('//speedVariance/@value', Doc);
    FParticleLifeSpan :=
        XPathAsNumber('//particleLifeSpan/@value', Doc);
    FParticleLifeSpanVariance :=
        XPathAsNumber('//particleLifespanVariance/@value', Doc);
    FAngle :=
        DegToRad(XPathAsNumber('//angle/@value', Doc));
    FAngleVariance :=
        DegToRad(XPathAsNumber('//angleVariance/@value', Doc));
    FGravity := Vector2(
        XPathAsNumber('//gravity/@x', Doc),
        XPathAsNumber('//gravity/@y', Doc)
    );
    FRadialAcceleration :=
        XPathAsNumber('//radialAcceleration/@value', Doc);
    FRadialAccelVariance :=
        XPathAsNumber('//radialAccelVariance/@value', Doc);
    FTangentialAcceleration :=
        XPathAsNumber('//tangentialAcceleration/@value', Doc);
    FTangentialAccelVariance :=
        XPathAsNumber('//tangentialAccelVariance/@value', Doc);
    FStartColor := Vector4(
        XPathAsNumber('//startColor/@red', Doc),
        XPathAsNumber('//startColor/@green', Doc),
        XPathAsNumber('//startColor/@blue', Doc),
        XPathAsNumber('//startColor/@alpha', Doc)
    );
    FStartColorVariance := Vector4(
        XPathAsNumber('//startColorVariance/@red', Doc),
        XPathAsNumber('//startColorVariance/@green', Doc),
        XPathAsNumber('//startColorVariance/@blue', Doc),
        XPathAsNumber('//startColorVariance/@alpha', Doc)
    );
    FFinishColor := Vector4(
        XPathAsNumber('//finishColor/@red', Doc),
        XPathAsNumber('//finishColor/@green', Doc),
        XPathAsNumber('//finishColor/@blue', Doc),
        XPathAsNumber('//finishColor/@alpha', Doc)
    );
    FFinishColorVariance := Vector4(
        XPathAsNumber('//finishColorVariance/@red', Doc),
        XPathAsNumber('//finishColorVariance/@green', Doc),
        XPathAsNumber('//finishColorVariance/@blue', Doc),
        XPathAsNumber('//finishColorVariance/@alpha', Doc)
    );
    FMaxParticles :=
        Round(XPathAsNumber('//maxParticles/@value', Doc));
    FStartParticleSize :=
        XPathAsNumber('//startParticleSize/@value', Doc);
    FStartParticleSizeVariance :=
        XPathAsNumber('//startParticleSizeVariance/@value', Doc);
    FFinishParticleSize :=
        XPathAsNumber('//finishParticleSize/@value', Doc);
    { Fix for onebyonedesign's particle editor. }
    FFinishParticleSizeVariance :=
        XPathAsNumber('//finishParticleSizeVariance/@value | //FinishParticleSizeVariance/@value', Doc);
    FDuration :=
        XPathAsNumber('//duration/@value', Doc);
    FEmitterType :=
        TCastleEmitterType(Round(XPathAsNumber('//emitterType/@value', Doc)));
    FMaxRadius :=
        XPathAsNumber('//maxRadius/@value', Doc);
    FMaxRadiusVariance :=
        XPathAsNumber('//maxRadiusVariance/@value', Doc);
    FMinRadius :=
        XPathAsNumber('//minRadius/@value', Doc);
    FMinRadiusVariance :=
        XPathAsNumber('//minRadiusVariance/@value', Doc);
    FRotatePerSecond :=
        DegToRad(XPathAsNumber('//rotatePerSecond/@value', Doc));
    FRotatePerSecondVariance :=
        DegToRad(XPathAsNumber('//rotatePerSecondVariance/@value', Doc));
    FBlendFuncSource :=
        Round(XPathAsNumber('//blendFuncSource/@value', Doc));
    FBlendFuncDestination :=
        Round(XPathAsNumber('//blendFuncDestination/@value', Doc));
    FRotationStart :=
        DegToRad(XPathAsNumber('//rotationStart/@value', Doc));
    FRotationStartVariance :=
        DegToRad(XPathAsNumber('//rotationStartVariance/@value', Doc));
    FRotationEnd :=
        DegToRad(XPathAsNumber('//rotationEnd/@value', Doc));
    FRotationEndVariance :=
        DegToRad(XPathAsNumber('//rotationEndVariance/@value', Doc));
  finally
    FreeAndNil(Doc);
    FreeAndNil(Stream);
  end;
end;

constructor TCastle2DParticleEmitter.Create(AOwner: TComponent);
begin
  inherited;
  { No collision for particles, and we also dont need to perform culling. I
    cant imagine how much time CGE would need in order to go through all
    vertices on emitters that emit thousands of particles just for calculating
    the bounding box. I would love to have a way to manually calculate the
    bounding box and then feed to the engine so that it can do frustum culling
    without invoking ssRendering. }
  Self.Spatial := [];
  InitNodeTree;
  FParticleCount := 0;
  FParticleList := TCastle2DParticleList.Create;
  FEmitParticleTime := 0;
  FReleaseWhenDone := false;
end;

destructor TCastle2DParticleEmitter.Destroy;
begin
  if Assigned(FEffect) then
    FEffect.Free;
  FParticleList.Free;
  inherited;
end;

procedure TCastle2DParticleEmitter.LoadPEX(const AURL: string);
begin
  if Assigned(FEffect) then
    FreeAndNil(FEffect);
  FEffect := TCastle2DParticleEffect.Create;
  FEffect.Load(AURL);
  FURL := AURL;
  RefreshEffect;
end;

procedure TCastle2DParticleEmitter.LoadPEX(const AEffect: TCastle2DParticleEffect;
    const AOwnEffect: Boolean = true);
begin
  if AOwnEffect then
  begin
    if Assigned(FEffect) then
      FreeAndNil(FEffect);
    FEffect := AEffect;
  end
  else
    AEffect.Clone(FEffect);
  RefreshEffect;
end;

function TCastle2DParticleEmitter.EmitParticle: boolean;
var
  S, C,
  LifeSpan,
  InvLifeSpan,
  Speed,
  Angle,
  MaxRadius,
  MinRadius,
  StartSize,
  FinishSize,
  EndRotation: single;
  FinishColor: TVector4;
  P: PCastle2DParticle;
begin
  if (not FStartEmitting) or (FParticleCount >= FEffect.MaxParticles) then
    exit(false);
  LifeSpan := FEffect.ParticleLifeSpan + FEffect.ParticleLifeSpanVariance * (Random * 2 - 1);
  if LifeSpan < 0 then
    exit(false);
  InvLifeSpan := 1 / LifeSpan;
  P := FParticleList.Ptr(FParticleCount);
  Inc(FParticleCount);
  P^.TimeToLive := LifeSpan;

  P^.Position[0] :=
      FEffect.SourcePosition[0] + FEffect.SourcePositionVariance[0] * (Random * 2 - 1);
  P^.Position[1] :=
      FEffect.SourcePosition[1] + FEffect.SourcePositionVariance[1] * (Random * 2 - 1);

  Speed := FEffect.Speed + FEffect.SpeedVariance * (Random * 2 - 1);
  Angle := FEffect.Angle + FEffect.AngleVariance * (Random * 2 - 1);
  SinCos(Angle, S, C);
  P^.Velocity := Vector2(Speed * C, Speed * S);

  MaxRadius := Max(0, FEffect.MaxRadius + FEffect.MaxRadiusVariance * (Random * 2 - 1));
  MinRadius := Max(0, FEffect.MinRadius + FEffect.MinRadiusVariance * (Random * 2 - 1));
  P^.EmitRadius := MaxRadius;
  P^.EmitRadiusDelta := (MinRadius - MaxRadius) * InvLifeSpan;

  P^.EmitRotation := FEffect.Angle + FEffect.AngleVariance * (Random * 2 - 1);
  P^.EmitRotationDelta := FEffect.RotatePerSecond + FEffect.RotatePerSecondVariance * (Random * 2 - 1);

  P^.RadialAcceleration := FEffect.RadialAcceleration + FEffect.RadialAccelVariance * (Random * 2 - 1);
  P^.TangentialAcceleration := FEffect.TangentialAcceleration + FEffect.TangentialAccelVariance * (Random * 2 - 1);

  StartSize := Max(0.1, FEffect.StartParticleSize + FEffect.StartParticleSizeVariance * (Random * 2 - 1));
  FinishSize := Max(0.1, FEffect.FinishParticleSize + FEffect.FinishParticleSizeVariance * (Random * 2 - 1));
  P^.Size := StartSize;
  P^.SizeDelta := (FinishSize - StartSize) * InvLifeSpan;

  P^.Color[0] := FEffect.StartColor[0] + FEffect.StartColorVariance[0] * (Random * 2 - 1);
  P^.Color[1] := FEffect.StartColor[1] + FEffect.StartColorVariance[1] * (Random * 2 - 1);
  P^.Color[2] := FEffect.StartColor[2] + FEffect.StartColorVariance[2] * (Random * 2 - 1);
  P^.Color[3] := FEffect.StartColor[3] + FEffect.StartColorVariance[3] * (Random * 2 - 1);
  FinishColor[0] := FEffect.FinishColor[0] + FEffect.FinishColorVariance[0] * (Random * 2 - 1);
  FinishColor[1] := FEffect.FinishColor[1] + FEffect.FinishColorVariance[1] * (Random * 2 - 1);
  FinishColor[2] := FEffect.FinishColor[2] + FEffect.FinishColorVariance[2] * (Random * 2 - 1);
  FinishColor[3] := FEffect.FinishColor[3] + FEffect.FinishColorVariance[3] * (Random * 2 - 1);
  P^.ColorDelta := (FinishColor - P^.Color) * InvLifeSpan;

  P^.Rotation := FEffect.RotationStart + FEffect.RotationStartVariance * (Random * 2 - 1);
  EndRotation := FEffect.RotationEnd + FEffect.RotationEndVariance * (Random * 2 - 1);
  P^.RotationDelta := (EndRotation - P^.Rotation) * InvLifeSpan;

  exit(true);
end;

procedure TCastle2DParticleEmitter.UpdateParticle(const P: PCastle2DParticle; ATimeStep: single);
var
  tmp,
  DistanceX,
  DistanceY,
  DistanceScalar,
  RadialX,
  RadialY,
  TangentialX,
  TangentialY: single;
begin
  if ATimeStep > P^.TimeToLive then
    ATimeStep := P^.TimeToLive;
  P^.TimeToLive -= ATimeStep;
  case FEffect.EmitterType of
    etRadial:
      begin
        P^.EmitRotation -= P^.EmitRotationDelta * ATimeStep;
        P^.EmitRadius += P^.EmitRadiusDelta * ATimeStep;
        P^.Position[0] := P^.StartPos[0] - Cos(P^.EmitRotation) * P^.EmitRadius;
        P^.Position[1] := P^.StartPos[1] + Sin(P^.EmitRotation) * P^.EmitRadius;
      end;
    etGravity:
      begin
        DistanceX := P^.Position[0] - P^.StartPos[0];
        DistanceY := P^.Position[1] - P^.StartPos[1];
        DistanceScalar := Vector2(DistanceX, DistanceY).Length;
        if DistanceScalar < 0.0001 then
          DistanceScalar := 0.0001;

        RadialX := DistanceX / DistanceScalar;
        RadialY := DistanceY / DistanceScalar;

        TangentialX := -RadialX;
        TangentialY := -RadialY;

        tmp := TangentialX;
        TangentialX := -TangentialY * P^.TangentialAcceleration;
        TangentialY := tmp * P^.TangentialAcceleration;

        P^.Velocity[0] := P^.Velocity[0] + (FEffect.Gravity[0] + RadialX - TangentialX) * ATimeStep;
        P^.Velocity[1] := P^.Velocity[1] - (-FEffect.Gravity[1] - RadialY + TangentialY) * ATimeStep;
        P^.Position[0] := P^.Position[0] + P^.Velocity[0] * ATimeStep;
        P^.Position[1] := P^.Position[1] + P^.Velocity[1] * ATimeStep;
      end;
  end;
  P^.Size += P^.SizeDelta * ATimeStep;
  P^.Rotation += P^.RotationDelta * ATimeStep;
  P^.Color += P^.ColorDelta * ATimeStep;
end;

procedure TCastle2DParticleEmitter.Update(const SecondsPassed: single; var RemoveMe: TRemoveType);
var
  Rot,
  C,
  S,
  SAdd,
  SSub,
  TimeBetweenParticles: single;
  V1, V2, V3, V4: TVector3;
  Col: TVector4;
  CoordList: TVector3List;
  TexCoordList: TVector2List;
  ColorList: TVector4List;
  P: PCastle2DParticle;
  i: integer;
  ParticleLifeSpan: single;
begin
  inherited;

  if FParticleList.Count = 0 then
    exit;
  CoordList := FCoordNode.FdPoint.Items;
  TexCoordList := FTexCoordNode.FdPoint.Items;
  ColorList := FColorNode.FdColor.Items;
  { We dont call Clear() here because it will reset Capacity which we dont
    want. We will just set Count = 0 instead. }
  CoordList.Count := 0;
  TexCoordList.Count := 0;
  ColorList.Count := 0;

  i := 0;
  while i < FParticleCount do
  begin
    P := FParticleList.Ptr(i);
    if P^.TimeToLive > 0 then
    begin
      UpdateParticle(P, SecondsPassed);
      Inc(i);
    end
    else
    begin
      if FParticleCount - 1 <> i then
        FParticleList[i] := FParticleList[FParticleCount - 1];
      Dec(FParticleCount);
    end;
  end;

  if (FEmissionTime > 0) or (FEmissionTime = -1) then
  begin
    ParticleLifeSpan := FEffect.ParticleLifeSpan;
    if ParticleLifeSpan = 0.0 then
      // Avoid endless loop when particleLifeSpan = 0
      ParticleLifeSpan := 0.001;
    TimeBetweenParticles := ParticleLifeSpan / FParticleList.Count;
    FEmitParticleTime += SecondsPassed;
    while FEmitParticleTime > 0 do
    begin
      if EmitParticle then
        UpdateParticle(FParticleList.Ptr(FParticleCount - 1), FEmitParticleTime);
      FEmitParticleTime -= TimeBetweenParticles;
    end;
    if FEmissionTime > 0 then
      FEmissionTime := Max(0, FEmissionTime - SecondsPassed);
  end;

  P := FParticleList.Ptr(0);
  for i:= 0 to FParticleCount-1 do
  begin
    Rot := P^.Rotation;
    SinCos(Rot, S, C);
    SAdd := (C + S) * P^.Size * 0.5;
    SSub := (C - S) * P^.Size * 0.5;
    Col := P^.Color;
    V1 := Vector3(P^.Position[0] - SSub, P^.Position[1] - SAdd, 0);
    V2 := Vector3(P^.Position[0] - SAdd, P^.Position[1] + SSub, 0);
    V3 := Vector3(P^.Position[0] + SSub, P^.Position[1] + SAdd, 0);
    V4 := Vector3(P^.Position[0] + SAdd, P^.Position[1] - SSub, 0);
    CoordList.AddRange([V1, V2, V3, V1, V3, V4]);
    ColorList.AddRange([Col, Col, Col, Col, Col, Col]);
    TexCoordList.AddRange(TexCoordBatch);
    Inc(P);
  end;

  { Manually tell the engine we change the data. }
  FCoordNode.FdPoint.Changed;
  FTexCoordNode.FdPoint.Changed;
  FColorNode.FdColor.Changed;

  RemoveMe := rtNone;
  if FReleaseWhenDone then
  begin
    if (FParticleCount = 0) and (FEmissionTime = 0) then
      RemoveMe := rtRemoveAndFree;
  end;
end;

procedure TCastle2DParticleEmitter.InitNodeTree;
var
  Root: TX3DRootNode;
  ColNode: TCollisionNode;
  ShapeNode: TShapeNode;
  TriNode: TTriangleSetNode;
begin
  Root := TX3DRootNode.Create;
  { We dont want the engine to perform collision detection on our particles,
    in case this node belongs to a colliable scene. }
  ColNode := TCollisionNode.Create;
  ColNode.Enabled := false;
  Root.FdChildren.Add(ColNode);

  ShapeNode:= TShapeNode.Create;
  ColNode.FdChildren.Add(ShapeNode);

  ShapeNode.Appearance := TAppearanceNode.Create;
  ShapeNode.Material := TMaterialNode.Create;
  { This is a 2d particle emitter and naturally we dont want it to be affected
    by any 3d light sources, by setting these values we tell the engine's light
    system to ignore the particles, make it very fast unlit. }
  ShapeNode.Material.DiffuseColor := Vector3(0, 0, 0);
  ShapeNode.Material.SpecularColor := Vector3(0, 0, 0);
  ShapeNode.Material.AmbientIntensity := 0;
  ShapeNode.Material.EmissiveColor := Vector3(1, 1, 1);

  FBlendModeNode := TBlendModeNode.Create;
  ShapeNode.Appearance.FdBlendMode.Value := FBlendModeNode;

  FImageTexNode := TImageTextureNode.Create;
  FImageTexNode.RepeatS := false;
  FImageTexNode.RepeatT := false;
  ShapeNode.Texture := FImageTexNode;

  TriNode := TTriangleSetNode.Create;
  TriNode.Solid := false;
  ShapeNode.FdGeometry.Value := TriNode;

  FCoordNode := TCoordinateNode.Create;
  FTexCoordNode := TTextureCoordinateNode.Create;
  FColorNode := TColorRGBANode.Create;

  TriNode.FdCoord.Value := FCoordNode;
  TriNode.FdTexCoord.Value := FTexCoordNode;
  TriNode.FdColor.Value := FColorNode;

  Self.Load(Root, true);
end;

procedure TCastle2DParticleEmitter.RefreshEffect;
begin
  FImageTexNode.FdUrl.Send([FEffect.Texture]);
  FEmissionTime := FEffect.Duration;
  FEmitParticleTime := 0;
  if FParticleCount > FEffect.MaxParticles then
    FParticleCount := Max(0, FEffect.MaxParticles - 1);
  FParticleList.Count := FEffect.MaxParticles;
  { We set the capacity so that the engine doesnt reallocate memory every time
    the number of particles change. }
  FCoordNode.FdPoint.Items.Capacity := FEffect.MaxParticles * 6;
  FTexCoordNode.FdPoint.Items.Capacity := FEffect.MaxParticles * 6;
  FColorNode.FdColor.Items.Capacity := FEffect.MaxParticles * 6;
  FBlendModeNode.FdSrcFactor.Send(BlendDict[FEffect.BlendFuncSource]);
  FBlendModeNode.FdDestFactor.Send(BlendDict[FEffect.BlendFuncDestination]);
end;

initialization
  BlendDict := TCastle2DParticleBlendDict.Create;
  BlendDict.Add(0, 'zero');
  BlendDict.Add(1, 'one');
  BlendDict.Add(768, 'src_color');
  BlendDict.Add(769, 'one_minus_src_color');
  BlendDict.Add(770, 'src_alpha');
  BlendDict.Add(771, 'one_minus_src_alpha');
  BlendDict.Add(772, 'dst_alpha');
  BlendDict.Add(773, 'one_minus_dst_alpha');
  BlendDict.Add(774, 'dst_color');
  BlendDict.Add(775, 'one_minus_dst_color');
  RegisterSerializableComponent(TCastle2DParticleEmitter, '2D Particle Emitter');

finalization
  BlendDict.Free;

end.

