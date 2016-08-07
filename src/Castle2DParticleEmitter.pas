unit Castle2DParticleEmitter;

{$mode objfpc}{$H+}
{$COPERATORS ON}

interface

uses
  Classes, SysUtils,
  Castle3D, CastleSceneCore, Castle2DSceneManager,
  CastleVectors, CastleGenericLists,
  X3DFields, X3DTime, X3DNodes;

type
  TCastleEmitterType = (etGravity, etRadial);

  { 2D particle struct to hold current particle settings. }
  PCastle2DParticle = ^TCastle2DParticle;
  TCastle2DParticle = record
    TimeToLive: single;
    Position: TVector2Single;
    Size,
    SizeDelta,
    Rotation,
    RotationDelta: single;
    Color,
    ColorDelta: TVector4Single;
    { Gravity parameters }
    StartPos,
    Velocity: TVector2Single;
    RadialAcceleration,
    TangentialAcceleration: single;
    { Radial parameters }
    EmitRadius,
    EmitRadiusDelta,
    EmitRotation,
    EmitRotationDelta: single;
  end;
  TCastle2DParticleList = specialize TGenericStructList<TCastle2DParticle>;

  { This class acts as a place holder for effects. }
  TCastle2DParticleEffect = class
  private
    FTexture: String;
    FSourcePosition,
    FSourcePositionVariance: TVector2Single;
    FSpeed,
    FSpeedVariance,
    FParticleLifeSpan,
    FParticleLifeSpanVariance,
    FAngle,
    FAngleVariance: single;
    FGravity: TVector2Single;
    FRadialAcceleration,
    FTangentialAcceleration,
    FRadialAccelVariance,
    FTangentialAccelVariance: single;
    FStartColor,
    FStartColorVariance,
    FFinishColor,
    FFinishColorVariance: TVector4Single;
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
    { We currently ignore blending mode for now, as I dont know how CGE handles
      blending internally. }
    FBlendMode: integer;
    FRotationStart,
    FRotationStartVariance,
    FRotationEnd,
    FRotationEndVariance: single;
  public
    { Clone all attributes of this effect to another one. Good if we want to
      load the effect from file just one and then apply it to other emitters
      so all emitters have the same effect. }
    procedure Clone(var ATarget: TCastle2DParticleEffect);
    { Load particle attributes in .PEX format. }
    procedure Load(const AURL: String);

    property Texture: String read FTexture write FTexture;
    property SourcePosition: TVector2Single read FSourcePosition write FSourcePosition;
    property SourcePositionVariance: TVector2Single read FSourcePositionVariance write FSourcePositionVariance;
    property Speed: single read FSpeed write FSpeed;
    property SpeedVariance: single read FSpeedVariance write FSpeedVariance;
    property ParticleLifeSpan: single read FParticleLifeSpan write FParticleLifeSpan;
    property ParticleLifeSpanVariance: single read FParticleLifeSpanVariance write FParticleLifeSpanVariance;
    property Angle: single read FAngle write FAngle;
    property AngleVariance: single read FAngleVariance write FAngleVariance;
    property Gravity: TVector2Single read FGravity write FGravity;
    property RadialAcceleration: single read FRadialAcceleration write FRadialAcceleration;
    property TangentialAcceleration: single read FTangentialAcceleration write FTangentialAcceleration;
    property RadialAccelVariance: single read FRadialAccelVariance write FRadialAccelVariance;
    property TangentialAccelVariance: single read FTangentialAccelVariance write FTangentialAccelVariance;
    property StartColor: TVector4Single read FStartColor write FStartColor;
    property StartColorVariance: TVector4Single read FStartColorVariance write FStartColorVariance;
    property FinishColor: TVector4Single read FFinishColor write FFinishColor;
    property FinishColorVariance: TVector4Single read FFinishColorVariance write FFinishColorVariance;
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
    { We currently ignore blending mode for now, as I dont know how CGE handles
      blending internal. }
    property BlendMode: integer read FBlendMode write FBlendMode;
    property RotationStart: single read FRotationStart write FRotationStart;
    property RotationStartVariance: single read FRotationStartVariance write FRotationStartVariance;
    property RotationEnd: single read FRotationEnd write FRotationEnd;
    property RotationEndVariance: single read FRotationEndVariance write FRotationEndVariance;
  end;

  { 2D particle emitter for CGE. }
  TCastle2DParticleEmitter = class(T2DScene)
  private
    FEffect: TCastle2DParticleEffect;
    FCoordNode: TCoordinateNode;
    FColorNode: TColorRGBANode;
    FTexCoordNode: TTextureCoordinateNode;
    FImageTexNode: TImageTextureNode;
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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { This method will free the current Effect if any, init a new FEffect and
      load settings from .PEX file. }
    procedure LoadPEX(const AURL: String); overload;
    { If AOwnEffect = false we will remove old Effect and replace it with
      AEffect, otherwise we will clone AEffect attributes to Effect (auto-init
      if needed). }
    procedure LoadPEX(const AEffect: TCastle2DParticleEffect;
        const AOwnEffect: Boolean = true); overload;
    procedure Update(const SecondsPassed: single; var RemoveMe: TRemoveType); override;
    procedure InitNodeTree;
    { Refresh the particle according to the change from effect. Normally we dont
      need to explicitly call it unless we make changes in Effect's Texture,
      Duration and MaxParticles. }
    procedure RefreshEffect;

    property Effect: TCastle2DParticleEffect read FEffect;
    property ParticleCount: integer read FParticleCount;
    property ReleaseWhenDone: boolean read FReleaseWhenDone write FReleaseWhenDone;
    property EmissionTime: single read FEmissionTime write FEmissionTime;
  end;

implementation

uses
  DOM, XMLRead, XPath,
  Math,
  CastleDownload, CastleURIUtils;

const
  TexCoordBatch: array [0..5] of TVector2Single =
    ((0, 0), (1, 0), (1, 1),
     (0, 0), (1, 1), (0, 1));

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
  { We currently ignore blending mode for now, as I dont know how CGE handle
    blending internal. }
  ATarget.BlendMode := BlendMode;
  ATarget.RotationStart := RotationStart;
  ATarget.RotationStartVariance := RotationStartVariance;
  ATarget.RotationEnd := RotationEnd;
  ATarget.RotationEndVariance := RotationEndVariance;
end;

procedure TCastle2DParticleEffect.Load(const AURL: String);
var
  Doc: TXMLDocument;       

  function XPath(const AXPath: DOMString; const ADOMNode: TDOMNode): TXPathVariable;
  begin
    Result := EvaluateXPathExpression(AXPath, ADOMNode);
  end;

begin
  try
    ReadXMLFile(Doc, Download(AURL));
    FTexture :=
        ExtractURIPath(AURL) + XPath('//texture/@name', Doc).AsText;
    { We ignored source position as we dont need it. The scene itself should
      attach to a T3DTransform for position. }
    FSourcePosition := Vector2Single(
        //XPath('//sourcePosition/@x', Doc).AsNumber,
        //XPath('//sourcePosition/@y', Doc).AsNumber
        0, 0
    );
    FSourcePositionVariance := Vector2Single(
        XPath('//sourcePositionVariance/@x', Doc).AsNumber,
        XPath('//sourcePositionVariance/@y', Doc).AsNumber
    );
    FSpeed :=
        XPath('//speed/@value', Doc).AsNumber;
    FSpeedVariance :=
        XPath('//speedVariance/@value', Doc).AsNumber;
    FParticleLifeSpan :=
        XPath('//particleLifeSpan/@value', Doc).AsNumber;
    FParticleLifeSpanVariance :=
        XPath('//particleLifespanVariance/@value', Doc).AsNumber;
    FAngle :=
        DegToRad(XPath('//angle/@value', Doc).AsNumber);
    FAngleVariance :=
        DegToRad(XPath('//angleVariance/@value', Doc).AsNumber);
    FGravity := Vector2Single(
        XPath('//gravity/@x', Doc).AsNumber,
        XPath('//gravity/@y', Doc).AsNumber
    );          
    FRadialAcceleration :=
        XPath('//radialAcceleration/@value', Doc).AsNumber;
    FRadialAccelVariance :=
        XPath('//radialAccelVariance/@value', Doc).AsNumber;
    FTangentialAcceleration :=
        XPath('//tangentialAcceleration/@value', Doc).AsNumber;
    FTangentialAccelVariance :=
        XPath('//tangentialAccelVariance/@value', Doc).AsNumber;
    FStartColor := Vector4Single(
        XPath('//startColor/@red', Doc).AsNumber,
        XPath('//startColor/@green', Doc).AsNumber,
        XPath('//startColor/@blue', Doc).AsNumber,
        XPath('//startColor/@alpha', Doc).AsNumber
    );                                         
    FStartColorVariance := Vector4Single(
        XPath('//startColorVariance/@red', Doc).AsNumber,
        XPath('//startColorVariance/@green', Doc).AsNumber,
        XPath('//startColorVariance/@blue', Doc).AsNumber,
        XPath('//startColorVariance/@alpha', Doc).AsNumber
    );
    FFinishColor := Vector4Single(
        XPath('//finishColor/@red', Doc).AsNumber,
        XPath('//finishColor/@green', Doc).AsNumber,
        XPath('//finishColor/@blue', Doc).AsNumber,
        XPath('//finishColor/@alpha', Doc).AsNumber
    );
    FFinishColorVariance := Vector4Single(
        XPath('//finishColorVariance/@red', Doc).AsNumber,
        XPath('//finishColorVariance/@green', Doc).AsNumber,
        XPath('//finishColorVariance/@blue', Doc).AsNumber,
        XPath('//finishColorVariance/@alpha', Doc).AsNumber
    );         
    FMaxParticles :=
        Round(XPath('//maxParticles/@value', Doc).AsNumber); 
    FStartParticleSize :=
        XPath('//startParticleSize/@value', Doc).AsNumber;
    FStartParticleSizeVariance :=
        XPath('//startParticleSizeVariance/@value', Doc).AsNumber; 
    FFinishParticleSize :=
        XPath('//finishParticleSize/@value', Doc).AsNumber; 
    { Fix for onebyonedesign's particle editor. }
    FFinishParticleSizeVariance :=
        XPath('//finishParticleSizeVariance/@value | //FinishParticleSizeVariance/@value', Doc).AsNumber;
    FDuration :=
        XPath('//duration/@value', Doc).AsNumber;  
    FEmitterType :=
        TCastleEmitterType(Round(XPath('//emitterType/@value', Doc).AsNumber));
    FMaxRadius :=
        XPath('//maxRadius/@value', Doc).AsNumber;
    FMaxRadiusVariance :=
        XPath('//maxRadiusVariance/@value', Doc).AsNumber;
    FMinRadius :=
        XPath('//minRadius/@value', Doc).AsNumber;
    FMinRadiusVariance :=
        XPath('//minRadiusVariance/@value', Doc).AsNumber;  
    FRotatePerSecond :=
        DegToRad(XPath('//rotatePerSecond/@value', Doc).AsNumber);
    FRotatePerSecondVariance :=
        DegToRad(XPath('//rotatePerSecondVariance/@value', Doc).AsNumber);
    FRotationStart :=
        DegToRad(XPath('//rotationStart/@value', Doc).AsNumber);
    FRotationStartVariance :=
        DegToRad(XPath('//rotationStartVariance/@value', Doc).AsNumber);
    FRotationEnd :=
        DegToRad(XPath('//rotationEnd/@value', Doc).AsNumber);
    FRotationEndVariance :=
        DegToRad(XPath('//rotationEndVariance/@value', Doc).AsNumber);
  finally
    FreeAndNil(Doc);
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
  FParticleList.Capacity := 128;
  FEmitParticleTime := 0;
  FReleaseWhenDone := false;
end;

destructor TCastle2DParticleEmitter.Destroy;
begin
  if Assigned(FEffect) then
    FreeAndNil(FEffect);
  FParticleList.Free;
  inherited;
end;

procedure TCastle2DParticleEmitter.LoadPEX(const AURL: String);
begin
  if Assigned(FEffect) then
    FreeAndNil(FEffect);
  FEffect := TCastle2DParticleEffect.Create;
  FEffect.Load(AURL);
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
  LifeSpan,
  InvLifeSpan,
  Speed,
  Angle,
  MaxRadius,
  MinRadius,
  StartSize,
  FinishSize,
  EndRotation: single;
  FinishColor: TVector4Single;
  P: PCastle2DParticle;
begin
  if FParticleCount >= FEffect.MaxParticles then
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
  P^.Velocity := Vector2Single(Speed * Cos(Angle), Speed * Sin(Angle));

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

  P^.Color := FEffect.StartColor + FEffect.StartColorVariance * (Random * 2 - 1);
  FinishColor := FEffect.FinishColor + FEffect.FinishColorVariance * (Random * 2 - 1);
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
        DistanceScalar := VectorLen(Vector2Single(DistanceX, DistanceY));
        if DistanceScalar < 0.0001 then
          DistanceScalar := 0.0001;

        RadialX := DistanceX / DistanceScalar;     
        RadialY := DistanceY / DistanceScalar;

        TangentialX := RadialX;
        TangentialY := RadialY;

        tmp := TangentialX;
        TangentialX := -TangentialY * P^.TangentialAcceleration;
        TangentialY := tmp * P^.TangentialAcceleration;

        P^.Velocity[0] += (FEffect.Gravity[0] + RadialX - TangentialX) * ATimeStep;
        P^.Velocity[1] -= (FEffect.Gravity[1] - RadialY + TangentialY) * ATimeStep;
        P^.Position[0] += P^.Velocity[0] * ATimeStep;
        P^.Position[1] += P^.Velocity[1] * ATimeStep;
      end;
  end;
  P^.Size += P^.SizeDelta * ATimeStep;
  P^.Rotation += P^.RotationDelta * ATimeStep;
  P^.Color += P^.ColorDelta * ATimeStep;
end;

procedure TCastle2DParticleEmitter.Update(const SecondsPassed: single; var RemoveMe: TRemoveType);
var
  Rotation,
  C,
  S,
  Add,
  Sub,
  TimeBetweenParticles: single;
  V1, V2, V3, V4: TVector3Single;
  Col: TVector4Single;
  CoordList: TVector3SingleList;
  TexCoordList: TVector2SingleList;
  ColorList: TVector4SingleList;
  P: PCastle2DParticle;
  i: integer;
begin       
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
    TimeBetweenParticles := FEffect.ParticleLifeSpan / FParticleList.Count;
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
    Rotation := -P^.Rotation;
    C := Cos(Rotation);
    S := Sin(Rotation);
    Add := (C + S) * P^.Size * 0.5;
    Sub := (C - S) * P^.Size * 0.5;
    Col := P^.Color;
    V1 := Vector3Single(P^.Position[0] - Sub, P^.Position[1] - Add, 0);
    V2 := Vector3Single(P^.Position[0] - Add, P^.Position[1] + Sub, 0);
    V3 := Vector3Single(P^.Position[0] + Sub, P^.Position[1] + Add, 0);
    V4 := Vector3Single(P^.Position[0] + Add, P^.Position[1] - Sub, 0);
    CoordList.AddArray([V1, V2, V3, V1, V3, V4]);
    ColorList.AddArray([Col, Col, Col, Col, Col, Col]);
    TexCoordList.AddArray(TexCoordBatch);
    Inc(P);
  end;

  { Manually tell the engine we change the data. }
  FCoordNode.FdPoint.Changed;                     
  FTexCoordNode.FdPoint.Changed;
  FColorNode.FdColor.Changed;
     
  RemoveMe := rtNone;
  if FReleaseWhenDone then
  begin
    if FParticleCount = 0 then
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
  ShapeNode.Material.DiffuseColor := Vector3Single(0, 0, 0);
  ShapeNode.Material.SpecularColor := Vector3Single(0, 0, 0);
  ShapeNode.Material.AmbientIntensity := 0;
  ShapeNode.Material.EmissiveColor := Vector3Single(1, 1, 1);

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
end;

end.

