unit Castle2DParticleEmitterGPU;

{$coperators on}
{$macro on}
{$define nl:=+ LineEnding +}

{$ifdef ANDROID}{$define GLES}{$endif}
{$ifdef iOS}{$define GLES}{$endif}

interface

uses
  Classes, SysUtils,
  {$ifdef GLES}
  CastleGLES20, // This wont work. We need GLES3 header
  {$else}
  GL, GLExt,
  {$endif}
  CastleTransform, CastleSceneCore, CastleComponentSerialize,
  CastleVectors, CastleRenderContext, Generics.Collections, CastleGLImages, CastleLog,
  Castle2DParticleEmitter, CastleUtils, CastleApplicationProperties, CastleGLShaders,
  CastleBoxes, X3DNodes;

type
  TCastle2DParticleInstanceGPU = packed record
    Translation,
    Rotation: TVector2;
  end;
  TCastle2DParticleInstanceGPUArray = packed array of TCastle2DParticleInstanceGPU;

  TCastle2DParticleInstance = record
    Translation: TVector2;
    Rotation: Single;
  end;
  TCastle2DParticleInstanceArray = array of TCastle2DParticleInstance;

  TCastle2DParticleEmitterGPU = class(TCastleSceneCore)
  strict private
    Texture: GLuint;
    FTransformFeedbackProgram,
    FRenderProgram: TGLSLProgram;

    VAOs,
    VBOs: array[0..1] of GLuint;
    VBOInstanced: GLUint;
    CurrentBuffer: GLuint;
    Particles: packed array of TCastle2DParticle;

    FURL: String;
    FStartEmitting: Boolean;
    FEffect: TCastle2DParticleEffect;
    FParticleCount: Integer;
    FSecondsPassed: Single;
    FIsUpdated: Boolean;
    { Countdown before remove the emitter }
    FCountdownTillRemove,
    { The value is in miliseconds. Set it to -1 for infinite emitting, 0 to
      stop the emitter and positive value for cooldown. }
    FEmissionTime: Single;
    { When this is set to true, the emitter will automatically freed after
      all particles destroyed. }
    FReleaseWhenDone: Boolean;
    FPosition: TVector2;
    { Set this to fast-drawing (instancing) the same particles at multiple positions. The position is relative to Translation }
    FInstances: TCastle2DParticleInstanceGPUArray;
    { Bypass GLContext problem }
    FIsGLContextInitialized: Boolean;
    FIsNeedRefresh: Boolean;
    procedure SetStartEmitting(V: Boolean);
    procedure InternalRefreshEffect;
    procedure GLContextOpen;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    procedure LocalRender(const Params: TRenderParams); override;
    procedure GLContextClose; override;

    { This method will free the current Effect if any, init a new FEffect and
      load settings from .PEX file. }
    procedure LoadEffect(const AURL: String); overload;
    procedure LoadEffect(const AEffect: TCastle2DParticleEffect); overload;

    procedure LoadPEX(const AURL: String); overload; deprecated 'Use LoadEffect';
    { AOwnEffect is ignored. }
    procedure LoadPEX(const AEffect: TCastle2DParticleEffect;
        const AOwnEffect: Boolean = True); overload; deprecated 'Use LoadEffect instead. AOwnEffect is ignored.';

    { Refresh the emitter according to the change from effect. Normally we dont
      need to explicitly call it unless we make changes in Effect's Texture,
      Duration, BlendFunc and/or MaxParticles. }
    procedure RefreshEffect;
    procedure SetInstances(const V: TCastle2DParticleInstanceArray);
    function LocalBoundingBox: TBox3D; override;

    property Effect: TCastle2DParticleEffect read FEffect;
    property ParticleCount: Integer read FParticleCount;
    property ReleaseWhenDone: Boolean read FReleaseWhenDone write FReleaseWhenDone;
    property EmissionTime: Single read FEmissionTime write FEmissionTime;
    { Move the position of emitter only }
    property Position: TVector2 read FPosition write FPosition;
  published
    { URL of a .pex file. This will call LoadEffect to load particle effect }
    property URL: String read FURL write LoadEffect;
    { If true, the emitter will start emitting }
    property StartEmitting: Boolean read FStartEmitting write SetStartEmitting default False;
  end;

implementation

uses
  Math;

const
  TransformVertexShaderSource: String =
'#version 330'nl
'layout(location = 0) in vec2 inPosition;'nl
'layout(location = 1) in float inTimeToLive;'nl
'layout(location = 2) in vec2 inSize;'nl
'layout(location = 3) in vec2 inRotation;'nl
'layout(location = 4) in vec4 inColor;'nl
'layout(location = 5) in vec4 inColorDelta;'nl
'layout(location = 6) in vec2 inStartPos;'nl
'layout(location = 7) in vec2 inVelocity;'nl
'layout(location = 8) in vec2 inAcceleration;'nl
'layout(location = 9) in vec2 inEmitRadius;'nl
'layout(location = 10) in vec2 inEmitRotation;'nl

'out vec2 outPosition;'nl
'out float outTimeToLive;'nl
'out vec2 outSize;'nl
'out vec2 outRotation;'nl
'out vec4 outColor;'nl
'out vec4 outColorDelta;'nl
'out vec2 outStartPos;'nl
'out vec2 outVelocity;'nl
'out vec2 outAcceleration;'nl
'out vec2 outEmitRadius;'nl
'out vec2 outEmitRotation;'nl

'struct Effect {'nl
'  vec2 sourcePosition;'nl
'  vec2 sourcePositionVariance;'nl
'  float speed;'nl
'  float speedVariance;'nl
'  float particleLifeSpan;'nl
'  float particleLifeSpanVariance;'nl
'  float angle;'nl
'  float angleVariance;'nl
'  vec2 gravity;'nl
'  float tangentialAcceleration;'nl
'  float tangentialAccelVariance;'nl
'  vec4 startColor;'nl
'  vec4 startColorVariance;'nl
'  vec4 finishColor;'nl
'  vec4 finishColorVariance;'nl
'  int maxParticles;'nl
'  float startParticleSize;'nl
'  float startParticleSizeVariance;'nl
'  float finishParticleSize;'nl
'  float finishParticleSizeVariance;'nl
'  int emitterType;'nl
'  float maxRadius;'nl
'  float maxRadiusVariance;'nl
'  float minRadius;'nl
'  float minRadiusVariance;'nl
'  float rotatePerSecond;'nl
'  float rotatePerSecondVariance;'nl
'  float rotationStart;'nl
'  float rotationStartVariance;'nl
'  float rotationEnd;'nl
'  float rotationEndVariance;'nl
'};'nl
'uniform Effect effect;'nl
'uniform float emissionTime;'nl
'uniform float deltaTime;'nl

'float rnd() {'nl
'  outAcceleration.x = fract(sin(outAcceleration.x + outPosition.x + outVelocity.x + outVelocity.y + outColor.x + deltaTime) * 43758.5453123);'nl
'  return outAcceleration.x;'nl
'}'nl

'void initParticle() {'nl
'  outPosition = inPosition;'nl
'  outColor = inColor;'nl
'  outTimeToLive = inTimeToLive;'nl
'  outSize = inSize;'nl
'  outRotation = inRotation;'nl
'  outColorDelta = inColorDelta;'nl
'  outStartPos = inStartPos;'nl
'  outVelocity = inVelocity;'nl
'  outAcceleration = inAcceleration;'nl
'  outEmitRadius = inEmitRadius;'nl
'  outEmitRotation = inEmitRotation;'nl
'}'nl

'void emitParticle() {'nl
'  outTimeToLive = effect.particleLifeSpan + effect.particleLifeSpanVariance * (rnd() * 2.0 - 1.0);'nl
'  float invLifeSpan = 1.0 / outTimeToLive;'nl
'  outPosition = effect.sourcePosition + effect.sourcePositionVariance * vec2(rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0);'nl
'  outStartPos = effect.sourcePosition;'nl
'  float speed = effect.speed + effect.speedVariance * (rnd() * 2.0 - 1.0);'nl
'  float angle = effect.angle + effect.angleVariance * (rnd() * 2.0 - 1.0);'nl
'  float s = sin(angle);'nl
'  float c = cos(angle);'nl
'  outVelocity = vec2(c, s) * speed;'nl
'  float minRadius = max(0.0, effect.minRadius + effect.minRadiusVariance * (rnd() * 2.0 - 1.0));'nl
'  float maxRadius = max(0.0, effect.maxRadius + effect.maxRadiusVariance * (rnd() * 2.0 - 1.0));'nl
'  outEmitRadius = vec2(maxRadius, (minRadius - maxRadius) * invLifeSpan);'nl
'  outEmitRotation.x = effect.angle + effect.angleVariance * (rnd() * 2.0 - 1.0);'nl
'  outEmitRotation.y = effect.rotatePerSecond + effect.rotatePerSecondVariance * (rnd() * 2.0 - 1.0);'nl
'  outAcceleration.y = effect.tangentialAcceleration + effect.tangentialAccelVariance * (rnd() * 2.0 - 1.0);'nl
'  float startSize = max(0.1, effect.startParticleSize + effect.startParticleSizeVariance * (rnd() * 2.0 - 1.0));'nl
'  float finishSize = max(0.1, effect.finishParticleSize + effect.finishParticleSizeVariance * (rnd() * 2.0 - 1.0));'nl
'  outSize = vec2(startSize, (finishSize - startSize) * invLifeSpan);'nl
'  outColor = effect.startColor + effect.startColorVariance * vec4(rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0);'nl
'  vec4 finishColor = effect.finishColor + effect.finishColorVariance * vec4(rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0);'nl
'  outColorDelta = (finishColor - outColor) * invLifeSpan;'nl
'  outRotation.x = effect.rotationStart + effect.rotationStartVariance * (rnd() * 2.0 - 1.0);'nl
'  float endRotation = effect.rotationEnd + effect.rotationEndVariance * (rnd() * 2.0 - 1.0);'nl
'  outRotation.y = (endRotation - outRotation.x) * invLifeSpan;'nl
'}'nl

'void updateParticle() {'nl
'  float timeBetweenParticle = max(deltaTime, effect.particleLifeSpan / effect.maxParticles);'nl
'  if (outTimeToLive <= 0.0 && emissionTime == 0.0) {'nl
'    outTimeToLive = (rnd() - 1.0) * effect.particleLifeSpan;'nl
'  }'nl
'  if (outTimeToLive == 0.0 && emissionTime != 0.0) {'nl
'    emitParticle();'nl
'  } else if (outTimeToLive < 0.0) {'nl
'    outTimeToLive = min(0.0, outTimeToLive + timeBetweenParticle);'nl
'    return;'nl
'  }'nl
'  outTimeToLive = max(0.0, outTimeToLive - deltaTime);'nl
'  if (effect.emitterType == 0) {'nl
'    vec2 distance = outPosition - outStartPos;'nl
'    float distanceScalar = max(0.0001, length(distance));'nl
'    vec2 radial = distance / distanceScalar;'nl
'    vec2 tangential = vec2(radial.y * outAcceleration.y, -radial.x * outAcceleration.y);'nl
'    outVelocity.x += (effect.gravity.x + radial.x - tangential.x) * deltaTime;'nl
'    outVelocity.y -= (-effect.gravity.y - radial.x + tangential.y) * deltaTime;'nl
'    outPosition += outVelocity * deltaTime;'nl
'  } else {'nl
'    outEmitRotation.x -= outEmitRotation.y * deltaTime;'nl
'    outEmitRadius.x += outEmitRadius.y * deltaTime;'nl
'    outPosition.x = outStartPos.x + cos(outEmitRotation.x) * outEmitRadius.x;'nl
'    outPosition.y = outStartPos.y - sin(outEmitRotation.x) * outEmitRadius.x;'nl
'  }'nl
'  outSize.x += outSize.y * deltaTime;'nl
'  outRotation.x += outRotation.y * deltaTime;'nl
'  outColor += outColorDelta * deltaTime;'nl
'}'nl

'void main() {'nl
'  initParticle();'nl
'  updateParticle();'nl
'}';

  VertexShaderSource: String =
'#version 330'nl
'layout(location = 0) in vec2 inPosition;'nl
'layout(location = 1) in float inTimeToLive;'nl
'layout(location = 2) in vec2 inSize;'nl
'layout(location = 3) in vec2 inRotation;'nl
'layout(location = 4) in vec4 inColor;'nl
'layout(location = 11) in vec4 inInstanced;'nl

'out float geomTimeToLive;'nl
'out vec2 geomSize;'nl
'out vec2 geomRotation;'nl
'out vec4 geomColor;'nl
'out vec4 geomInstanced;'nl

'void main() {'nl
'  gl_Position = vec4(inPosition, 0.0, 1.0);'nl
'  geomTimeToLive = inTimeToLive;'nl
'  geomSize = inSize;'nl
'  geomRotation = inRotation;'nl
'  geomColor = inColor;'nl
'  geomInstanced = inInstanced;'nl
'}';

  GeometryShaderSource: String =
'#version 330'nl
'layout(points) in;'nl
'layout(triangle_strip, max_vertices = 4) out;'nl

'in float geomTimeToLive[];'nl
'in vec2 geomSize[];'nl
'in vec2 geomRotation[];'nl
'in vec4 geomColor[];'nl
'in vec4 geomInstanced[];'nl

'out vec2 fragTexCoord;'nl
'out vec4 fragColor;'nl

'uniform mat4 mvpMatrix;'nl

'void main() {'nl
'  if (geomTimeToLive[0] > 0.0) {'nl
'    mat4 instMatrix = mat4('nl
'      vec4(geomInstanced[0].zw, 0.0, 0.0),'nl
'      vec4(-geomInstanced[0].w, geomInstanced[0].z, 0.0, 0.0),'nl
'      vec4(0.0),'nl
'      vec4(geomInstanced[0].xy, 0.0, 1.0)'nl
'    );'nl
'    mat4 m = mvpMatrix * instMatrix;'nl
'    fragColor = geomColor[0];'nl

'    float s = sin(geomRotation[0].x);'nl
'    float c = cos(geomRotation[0].x);'nl
'    float sadd = (c + s) * geomSize[0].x * 0.5;'nl
'    float ssub = (c - s) * geomSize[0].x * 0.5;'nl
'    vec2 p = gl_in[0].gl_Position.xy;'nl

'    gl_Position = m * vec4(p.x - ssub, p.y - sadd, 0.0, 1.0);'nl
'    fragTexCoord = vec2(0.0, 1.0);'nl
'    EmitVertex();'nl
'    gl_Position = m * vec4(p.x - sadd, p.y + ssub, 0.0, 1.0);'nl
'    fragTexCoord = vec2(0.0, 0.0);'nl
'    EmitVertex();'nl
'    gl_Position = m * vec4(p.x + sadd, p.y - ssub, 0.0, 1.0);'nl
'    fragTexCoord = vec2(1.0, 1.0);'nl
'    EmitVertex();'nl
'    gl_Position = m * vec4(p.x + ssub, p.y + sadd, 0.0, 1.0);'nl
'    fragTexCoord = vec2(1.0, 0.0);'nl
'    EmitVertex();'nl

'    EndPrimitive();'nl
'  }'nl
'}';

  FragmentShaderSource: String =
'#version 330'nl
'precision lowp float;'nl
'in vec2 fragTexCoord;'nl
'in vec4 fragColor;'nl

'out vec4 outColor;'nl

'uniform sampler2D baseColor;'nl

'void main() {'nl
'  outColor = texture(baseColor, fragTexCoord) * fragColor;'nl
'  outColor.rgb *= outColor.a;'nl
'}';

  Varyings: array[0..10] of PChar = (
    'outPosition',
    'outTimeToLive',
    'outSize',
    'outRotation',
    'outColor',
    'outColorDelta',
    'outStartPos',
    'outVelocity',
    'outAcceleration',
    'outEmitRadius',
    'outEmitRotation'
  );

var
  IsCheckedForUsable: Boolean = False;

procedure TCastle2DParticleEmitterGPU.SetStartEmitting(V: Boolean);
begin
  Self.FStartEmitting := V;
  if V and Assigned(FEffect) then
    Self.FCountdownTillRemove := Self.FEffect.ParticleLifeSpan + Self.FEffect.ParticleLifeSpanVariance;
end;

procedure TCastle2DParticleEmitterGPU.GLContextOpen;
var
  V: Integer;
begin
  // Safeguard
  if not ApplicationProperties.IsGLContextOpen then Exit;
  if Self.FIsGLContextInitialized then Exit;

  if not IsCheckedForUsable then
  begin
    // Check maximum number of vertex attributes
    glGetIntegerv(GL_MAX_VERTEX_ATTRIBS, @V);
    WritelnLog('GL_MAX_VERTEX_ATTRIBS: ' + IntToStr(V));
    if V < Length(Varyings) then
      raise Exception.Create(Format('TCastle2DParticleEmitterGPU requires GL_MAX_VERTEX_ATTRIBS at least %d', [Length(Varyings)]));
    IsCheckedForUsable := True;
  end;

  FTransformFeedbackProgram := TGLSLProgram.Create;
  FTransformFeedbackProgram.AttachVertexShader(TransformVertexShaderSource);
  FTransformFeedbackProgram.SetTransformFeedbackVaryings(Varyings);
  FTransformFeedbackProgram.Link;

  FRenderProgram := TGLSLProgram.Create;
  FRenderProgram.AttachVertexShader(VertexShaderSource);
  FRenderProgram.AttachGeometryShader(GeometryShaderSource);
  FRenderProgram.AttachFragmentShader(FragmentShaderSource);
  FRenderProgram.Link;

  glGenBuffers(1, @Self.VBOInstanced);
  glGenBuffers(2, @Self.VBOs);
  glGenVertexArrays(2, @Self.VAOs);
  Self.FIsGLContextInitialized := True;
end;

procedure TCastle2DParticleEmitterGPU.GLContextClose;
begin
  if Self.FIsGLContextInitialized then
  begin
    glDeleteBuffers(1, @Self.VBOInstanced);
    glDeleteBuffers(2, @Self.VBOs);
    glDeleteVertexArrays(2, @Self.VAOs);
    FreeAndNil(Self.FTransformFeedbackProgram);
    FreeAndNil(Self.FRenderProgram);
    glFreeTexture(Self.Texture);
    Self.FIsGLContextInitialized := False;
  end;
  inherited;
end;

constructor TCastle2DParticleEmitterGPU.Create(AOwner: TComponent);
begin
  inherited;
  Self.Texture := 0;
  Self.FSecondsPassed := 0;
  Self.FPosition := Vector2(0, 0);
  Self.Scale := Vector3(1, -1, 1);
  SetLength(FInstances, 1);
  Self.FInstances[0].Translation := Vector2(0, 0);
  Self.FInstances[0].Rotation := Vector2(1, 0);
  Self.FIsGLContextInitialized := False;
  Self.FIsNeedRefresh := False;
  Self.FIsUpdated := False;
end;

destructor TCastle2DParticleEmitterGPU.Destroy;
begin
  inherited;
end;

procedure TCastle2DParticleEmitterGPU.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
  inherited;
  if not Assigned(Self.FEffect) then
    Exit;
  Self.GLContextOpen;
  if Self.FIsNeedRefresh then
    Self.InternalRefreshEffect;
  //if not Self.ProcessEvents then
  //  Exit;

  Self.FSecondsPassed := SecondsPassed;

  if not Self.FIsUpdated then
  begin
    if (FEmissionTime > 0) or (FEmissionTime = -1) then
    begin
      if FEmissionTime > 0 then
        FEmissionTime := Max(0, FEmissionTime - SecondsPassed);
    end;

    if not Self.FStartEmitting then
    begin
      Self.FCountdownTillRemove := Self.FCountdownTillRemove - SecondsPassed;
    end;

      // Update particles
    glEnable(GL_RASTERIZER_DISCARD);
    Self.FTransformFeedbackProgram.Enable;
    Self.FTransformFeedbackProgram.Uniform('deltaTime').SetValue(Self.FSecondsPassed);
    if Self.FStartEmitting then
      Self.FTransformFeedbackProgram.Uniform('emissionTime').SetValue(Self.FEmissionTime)
    else
      Self.FTransformFeedbackProgram.Uniform('emissionTime').SetValue(0);
    Self.FTransformFeedbackProgram.Uniform('effect.sourcePosition').SetValue(Self.FPosition);
    Self.FTransformFeedbackProgram.Uniform('effect.sourcePosition').SetValue(Self.FEffect.SourcePosition);
    Self.FTransformFeedbackProgram.Uniform('effect.sourcePositionVariance').SetValue(Self.FEffect.SourcePositionVariance);
    Self.FTransformFeedbackProgram.Uniform('effect.speed').SetValue(Self.FEffect.Speed);
    Self.FTransformFeedbackProgram.Uniform('effect.speedVariance').SetValue(Self.FEffect.SpeedVariance);
    Self.FTransformFeedbackProgram.Uniform('effect.particleLifeSpan').SetValue(Self.FEffect.ParticleLifeSpan);
    Self.FTransformFeedbackProgram.Uniform('effect.particleLifeSpanVariance').SetValue(Self.FEffect.ParticleLifeSpanVariance);
    Self.FTransformFeedbackProgram.Uniform('effect.angle').SetValue(Self.FEffect.Angle);
    Self.FTransformFeedbackProgram.Uniform('effect.angleVariance').SetValue(Self.FEffect.AngleVariance);
    Self.FTransformFeedbackProgram.Uniform('effect.gravity').SetValue(Self.FEffect.Gravity);
    Self.FTransformFeedbackProgram.Uniform('effect.tangentialAcceleration').SetValue(Self.FEffect.TangentialAcceleration);
    Self.FTransformFeedbackProgram.Uniform('effect.tangentialAccelVariance').SetValue(Self.FEffect.TangentialAccelVariance);
    Self.FTransformFeedbackProgram.Uniform('effect.startColor').SetValue(Self.FEffect.StartColor);
    Self.FTransformFeedbackProgram.Uniform('effect.startColorVariance').SetValue(Self.FEffect.StartColorVariance);
    Self.FTransformFeedbackProgram.Uniform('effect.finishColor').SetValue(Self.FEffect.FinishColor);
    Self.FTransformFeedbackProgram.Uniform('effect.finishColorVariance').SetValue(Self.FEffect.FinishColorVariance);
    Self.FTransformFeedbackProgram.Uniform('effect.maxParticles').SetValue(Self.FEffect.MaxParticles);
    Self.FTransformFeedbackProgram.Uniform('effect.startParticleSize').SetValue(Self.FEffect.StartParticleSize);
    Self.FTransformFeedbackProgram.Uniform('effect.startParticleSizeVariance').SetValue(Self.FEffect.StartParticleSizeVariance);
    Self.FTransformFeedbackProgram.Uniform('effect.finishParticleSize').SetValue(Self.FEffect.FinishParticleSize);
    Self.FTransformFeedbackProgram.Uniform('effect.finishParticleSizeVariance').SetValue(Self.FEffect.FinishParticleSizeVariance);
    Self.FTransformFeedbackProgram.Uniform('effect.emitterType').SetValue(GLint(Self.FEffect.EmitterType));
    Self.FTransformFeedbackProgram.Uniform('effect.maxRadius').SetValue(Self.FEffect.MaxRadius);
    Self.FTransformFeedbackProgram.Uniform('effect.maxRadiusVariance').SetValue(Self.FEffect.MaxRadiusVariance);
    Self.FTransformFeedbackProgram.Uniform('effect.minRadius').SetValue(Self.FEffect.MinRadius);
    Self.FTransformFeedbackProgram.Uniform('effect.minRadiusVariance').SetValue(Self.FEffect.MinRadiusVariance);
    Self.FTransformFeedbackProgram.Uniform('effect.rotatePerSecond').SetValue(Self.FEffect.RotatePerSecond);
    Self.FTransformFeedbackProgram.Uniform('effect.rotatePerSecondVariance').SetValue(Self.FEffect.RotatePerSecondVariance);
    Self.FTransformFeedbackProgram.Uniform('effect.rotationStart').SetValue(Self.FEffect.RotationStart);
    Self.FTransformFeedbackProgram.Uniform('effect.rotationStartVariance').SetValue(Self.FEffect.RotationStartVariance);
    Self.FTransformFeedbackProgram.Uniform('effect.rotationEnd').SetValue(Self.FEffect.RotationEnd);
    Self.FTransformFeedbackProgram.Uniform('effect.rotationEndVariance').SetValue(Self.FEffect.RotationEndVariance);
    Self.FTransformFeedbackProgram.Uniform('deltaTime').SetValue(Self.FSecondsPassed);
    Self.FTransformFeedbackProgram.Uniform('emissionTime').SetValue(Self.FEmissionTime);
    glBindVertexArray(Self.VAOs[CurrentBuffer]);
    glBindBufferBase(GL_TRANSFORM_FEEDBACK_BUFFER, 0, Self.VBOs[(CurrentBuffer + 1) mod 2]);
    glBeginTransformFeedback(GL_POINTS);
    glDrawArrays(GL_POINTS, 0, Self.FEffect.MaxParticles);
    glEndTransformFeedback();
    glDisable(GL_RASTERIZER_DISCARD);
    glBindVertexArray(0); // Just in case :)
    CurrentBuffer := (CurrentBuffer + 1) mod 2;
  end;

  RemoveMe := rtNone;
  if Self.FReleaseWhenDone then
  begin
    if (Self.FEmissionTime = 0) then
    begin
      if not Self.FIsUpdated then
        Self.FCountdownTillRemove := Self.FCountdownTillRemove - SecondsPassed;
      if (Self.FCountdownTillRemove <= 0) then
      begin
        RemoveMe := rtRemoveAndFree;
      end;
    end;
  end;
  Self.FIsUpdated := True;
end;

procedure TCastle2DParticleEmitterGPU.LocalRender(const Params: TRenderParams);
var
  M: TMatrix4;
  InstanceCount: Integer;
  V: TVector3;
  RelativeBBox: TBox3D;
begin
  inherited;
  Self.FIsUpdated := False;
  if not Assigned(Self.FEffect) then
    Exit;
  if not Self.FIsGLContextInitialized then
    Exit;
  if (not Self.Visible) or Params.InShadow or (not Params.Transparent) or (Params.StencilTest > 0) then
    Exit;
  if (not Self.FStartEmitting) and (Self.FCountdownTillRemove <= 0) then
    Exit;
  InstanceCount := Length(Self.FInstances);
  if InstanceCount = 0 then
    Exit;
  Inc(Params.Statistics.ScenesVisible);
  if not Self.FEffect.BBox.IsEmpty then
  begin
    V := Vector3(Self.Position, 0);
    RelativeBBox := Box3D(
      Self.FEffect.BBox.Data[0] + V,
      Self.FEffect.BBox.Data[1] + V
    );
    if not Params.Frustum^.Box3DCollisionPossibleSimple(RelativeBBox) then
      Exit;
  end;
  Inc(Params.Statistics.ShapesVisible);
  Inc(Params.Statistics.ShapesRendered);
  Inc(Params.Statistics.ScenesRendered);

  M := RenderContext.ProjectionMatrix * Params.RenderingCamera.Matrix * Params.Transform^;

  // Draw particles
  glEnable(GL_BLEND);
  glBlendFunc(Self.FEffect.BlendFuncSource, Self.FEffect.BlendFuncDestination);
  Self.FRenderProgram.Enable;
  Self.FRenderProgram.Uniform('mvpMatrix').SetValue(M);
  glBindVertexArray(Self.VAOs[CurrentBuffer]);
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, Self.Texture);
  glDrawArraysInstanced(GL_POINTS, 0, Self.FEffect.MaxParticles, InstanceCount);
  glBindTexture(GL_TEXTURE_2D, 0);
  glBindVertexArray(0);
  glDisable(GL_BLEND);
end;

procedure TCastle2DParticleEmitterGPU.LoadEffect(const AURL: String);
begin
  FEffect := TCastle2DParticleEffect.Create(Self);
  FEffect.Load(AURL);
  FURL := AURL;
  RefreshEffect;
end;

procedure TCastle2DParticleEmitterGPU.LoadEffect(const AEffect: TCastle2DParticleEffect);
begin
  FEffect := AEffect;
  RefreshEffect;
end;

procedure TCastle2DParticleEmitterGPU.LoadPEX(const AURL: String);
begin
  Self.LoadEffect(AURL);
end;

procedure TCastle2DParticleEmitterGPU.LoadPEX(const AEffect: TCastle2DParticleEffect;
  const AOwnEffect: Boolean = True);
begin
  Self.LoadEffect(AEffect);
end;

procedure TCastle2DParticleEmitterGPU.SetInstances(const V: TCastle2DParticleInstanceArray);
var
  I, Len: Integer;
  SX, SY: Single;
  IsEqualSize: Boolean = True;
begin
  Self.GLContextOpen;
  Len := Length(V);
  if Len <> Length(Self.FInstances) then
  begin
    SetLength(Self.FInstances, Len);
    IsEqualSize := False;
  end;
  SX := Sign(Self.Scale.X);
  SY := Sign(Self.Scale.Y);
  for I := 0 to Len - 1 do
  begin
    Self.FInstances[I].Translation.X := SX * V[I].Translation.X;
    Self.FInstances[I].Translation.Y := SY * V[I].Translation.Y;
    Self.FInstances[I].Rotation := Vector2(SX * Cos(V[I].Rotation), SY * Sin(V[I].Rotation));
  end;

  glBindBuffer(GL_ARRAY_BUFFER, Self.VBOInstanced);
  if IsEqualSize then
    glBufferSubData(GL_ARRAY_BUFFER, 0, Len * SizeOf(TCastle2DParticleInstanceGPU), @Self.FInstances[0])
  else
    glBufferData(GL_ARRAY_BUFFER, Len * SizeOf(TCastle2DParticleInstanceGPU), @Self.FInstances[0], GL_STATIC_DRAW);
end;

procedure TCastle2DParticleEmitterGPU.InternalRefreshEffect;
var
  I: Integer;
begin
  Self.FEmissionTime := Self.FEffect.Duration;
  Self.FParticleCount := Self.FEffect.MaxParticles;
  Self.FCountdownTillRemove := Self.FEffect.ParticleLifeSpan + Self.FEffect.ParticleLifeSpanVariance;
  SetLength(Self.Particles, Self.FEffect.MaxParticles);

  if Self.FEffect.ParticleLifeSpan = 0 then
    Self.FEffect.ParticleLifeSpan := 0.001;

  glFreeTexture(Self.Texture);
  Self.Texture := LoadGLTexture(
    Self.FEffect.Texture,
    TextureFilter(minLinear, magLinear),
    Texture2DClampToEdge
  );

  // Generate initial lifecycle
  for I := 0 to Self.FEffect.MaxParticles - 1 do
  begin
    with Self.Particles[I] do
    begin
      TimeToLive := Random * (Self.FEffect.ParticleLifeSpan + Self.FEffect.ParticleLifeSpanVariance);
      Position := Vector2(Random, Random);
      // Take advantage of unused RadialAcceleration for initial seed
      RadialAcceleration := Random;
    end;
  end;
  // Instance VBO
  glBindBuffer(GL_ARRAY_BUFFER, Self.VBOInstanced);
  glBufferData(GL_ARRAY_BUFFER, Length(Self.FInstances) * SizeOf(TCastle2DParticleInstanceGPU), @Self.FInstances[0], GL_STATIC_DRAW);

  // Drawing VAO
  Self.CurrentBuffer := 0;
  for I := 0 to 1 do
  begin
    glBindVertexArray(Self.VAOs[I]);

    glBindBuffer(GL_ARRAY_BUFFER, Self.VBOs[I]);
    glBufferData(GL_ARRAY_BUFFER, Self.FEffect.MaxParticles * SizeOf(TCastle2DParticle), @Self.Particles[0], GL_STATIC_DRAW);

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, SizeOf(TCastle2DParticle), Pointer(0));
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 1, GL_FLOAT, GL_FALSE, SizeOf(TCastle2DParticle), Pointer(8));
    glEnableVertexAttribArray(2);
    glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, SizeOf(TCastle2DParticle), Pointer(12));
    glEnableVertexAttribArray(3);
    glVertexAttribPointer(3, 2, GL_FLOAT, GL_FALSE, SizeOf(TCastle2DParticle), Pointer(20));
    glEnableVertexAttribArray(4);
    glVertexAttribPointer(4, 4, GL_FLOAT, GL_FALSE, SizeOf(TCastle2DParticle), Pointer(28));
    glEnableVertexAttribArray(5);
    glVertexAttribPointer(5, 4, GL_FLOAT, GL_FALSE, SizeOf(TCastle2DParticle), Pointer(44));
    glEnableVertexAttribArray(6);
    glVertexAttribPointer(6, 2, GL_FLOAT, GL_FALSE, SizeOf(TCastle2DParticle), Pointer(60));
    glEnableVertexAttribArray(7);
    glVertexAttribPointer(7, 2, GL_FLOAT, GL_FALSE, SizeOf(TCastle2DParticle), Pointer(68));
    glEnableVertexAttribArray(8);
    glVertexAttribPointer(8, 2, GL_FLOAT, GL_FALSE, SizeOf(TCastle2DParticle), Pointer(76));
    glEnableVertexAttribArray(9);
    glVertexAttribPointer(9, 2, GL_FLOAT, GL_FALSE, SizeOf(TCastle2DParticle), Pointer(84));
    glEnableVertexAttribArray(10);
    glVertexAttribPointer(10, 2, GL_FLOAT, GL_FALSE, SizeOf(TCastle2DParticle), Pointer(92));

    glBindBuffer(GL_ARRAY_BUFFER, Self.VBOInstanced);
    glEnableVertexAttribArray(11);
    glVertexAttribPointer(11, 4, GL_FLOAT, GL_FALSE, SizeOf(TCastle2DParticleInstanceGPU), Pointer(0));
    glVertexAttribDivisor(11, 1);

    glBindVertexArray(0);
  end;
  SetLength(Self.Particles, 0);
  Self.FIsNeedRefresh := False;
end;

procedure TCastle2DParticleEmitterGPU.RefreshEffect;
begin
  Self.FIsNeedRefresh := True;
end;

function TCastle2DParticleEmitterGPU.LocalBoundingBox: TBox3D;
var
  V: TVector3;
begin
  if GetExists then
  begin
    if not Self.FEffect.BBox.IsEmpty then
    begin
      V := Vector3(Self.Position, 0);
      Result := Box3D(
        Self.FEffect.BBox.Data[0] + V,
        Self.FEffect.BBox.Data[1] + V
      );
    end else
      Result := Self.FEffect.BBox;
  end else
    Result := TBox3D.Empty;
  Result.Include(inherited LocalBoundingBox);
end;

initialization
  RegisterSerializableComponent(TCastle2DParticleEmitterGPU, '2D Particle Emitter (GPU)');

end.
