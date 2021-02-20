unit Castle2DParticleEmitterGPU;

{$I castleconf.inc}

{$coperators on}
{$macro on}
{$define nl:=+ LineEnding +}

interface

uses
  Classes, SysUtils,
  {$ifdef CASTLE_OBJFPC}CastleGL,{$else} GL, GLExt,{$endif}
  CastleTransform, CastleScene, CastleSceneCore, Castle2DSceneManager, CastleComponentSerialize,
  CastleVectors, castlerendercontext, Generics.Collections, CastleGLImages, CastleLog,
  Castle2DParticleEmitter,
  X3DNodes;

type
  TCastle2DParticleEmitterGPU = class(TCastleSceneCore)
  public
    class var ShaderVert: GLuint;
    class var ShaderGeom: GLuint;
    class var ShaderFrag: GLuint;
    class var ShaderTFVert: GLuint;
  strict private
    ShaderProg: GLuint;
    ShaderTFProg: GLuint;

    UniformMVPMatrix,
    UniformSourcePosition,
    UniformSourcePositionVariance,
    UniformSpeed,
    UniformSpeedVariance,
    UniformParticleLifeSpan,
    UniformParticleLifeSpanVariance,
    UniformAngle,
    UniformAngleVariance,
    UniformGravity,
    UniformRadialAcceleration,
    UniformTangentialAcceleration,
    UniformRadialAccelVariance,
    UniformTangentialAccelVariance,
    UniformStartColor,
    UniformStartColorVariance,
    UniformFinishColor,
    UniformFinishColorVariance,
    UniformMaxParticles,
    UniformStartParticleSize,
    UniformStartParticleSizeVariance,
    UniformFinishParticleSize,
    UniformFinishParticleSizeVariance,
    UniformEmitterType,
    UniformMaxRadius,
    UniformMaxRadiusVariance,
    UniformMinRadius,
    UniformMinRadiusVariance,
    UniformRotatePerSecond,
    UniformRotatePerSecondVariance,
    UniformRotationStart,
    UniformRotationStartVariance,
    UniformRotationEnd,
    UniformRotationEndVariance,
    UniformEmissionTime,
    UniformDeltaTime: GLuint;
    Texture: GLuint;

    VAOs,
    VBOs: array[0..1] of GLuint;
    CurrentBuffer: GLuint;
    Particles: array of TCastle2DParticle;

    FURL: String;
    FStartEmitting: Boolean;
    FEffect: TCastle2DParticleEffect;
    FParticleCount: Integer;
    FSecondsPassed: Single;
    FIsDrawn: Boolean;
    { The value is in miliseconds. Set it to -1 for infinite emitting, 0 to
      stop the emitter and positive value for cooldown. }
    FEmissionTime,
    FEmitParticleTime: Single;
    { When this is set to true, the emitter will automatically freed after
      all particles destroyed. }
    FReleaseWhenDone: Boolean;
    FPosition: TVector2;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    procedure LocalRender(const Params: TRenderParams); override;

    { This method will free the current Effect if any, init a new FEffect and
      load settings from .PEX file. }
    procedure LoadEffect(const AURL: String); overload;
    { If AOwnEffect = false we will remove old Effect and replace it with
      AEffect, otherwise we will clone AEffect attributes to Effect (auto-init
      if needed). }
    procedure LoadEffect(const AEffect: TCastle2DParticleEffect;
        const AOwnEffect: Boolean = true); overload;
    { Refresh the emitter according to the change from effect. Normally we dont
      need to explicitly call it unless we make changes in Effect's Texture,
      Duration, BlendFunc and/or MaxParticles. }
    procedure RefreshEffect;

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
    property StartEmitting: Boolean read FStartEmitting write FStartEmitting default False;
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
'  float radialAcceleration;'nl
'  float tangentialAcceleration;'nl
'  float radialAccelVariance;'nl
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
'  float duration;'nl
'};'nl
'uniform Effect effect;'nl
'uniform float emissionTime;'nl
'uniform float deltaTime;'nl

'vec2 seed;'nl

'float rnd() {'nl
'  float r = fract(sin(seed.x + seed.y) * 43758.5453123);'nl
'  seed += r;'nl
'  return r;'nl
'}'nl

'void initParticle() {'nl
'  seed = inPosition;'nl
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
'  outAcceleration.x = effect.radialAcceleration + effect.radialAccelVariance * (rnd() * 2.0 - 1.0);'nl
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
'  float timeBetweenParticle = max(0.01, effect.particleLifeSpan / effect.maxParticles);'nl
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

'out float geomTimeToLive;'nl
'out vec2 geomSize;'nl
'out vec2 geomRotation;'nl
'out vec4 geomColor;'nl

'void main() {'nl
'  gl_Position = vec4(inPosition, 0.0, 1.0);'nl
'  geomTimeToLive = inTimeToLive;'nl
'  geomSize = inSize;'nl
'  geomRotation = inRotation;'nl
'  geomColor = inColor;'nl
'}';

  GeometryShaderSource: String =
'#version 330'nl
'layout(points) in;'nl
'layout(triangle_strip, max_vertices = 4) out;'nl

'in float geomTimeToLive[];'nl
'in vec2 geomSize[];'nl
'in vec2 geomRotation[];'nl
'in vec4 geomColor[];'nl

'out vec2 fragTexCoord;'nl
'out vec4 fragColor;'nl

'uniform mat4 mvpMatrix;'nl

'void main() {'nl
'  if (geomTimeToLive[0] > 0) {'nl
'    fragColor = geomColor[0];'nl

'    float s = sin(geomRotation[0].x);'nl
'    float c = cos(geomRotation[0].x);'nl
'    float sadd = (c + s) * geomSize[0].x * 0.5;'nl
'    float ssub = (c - s) * geomSize[0].x * 0.5;'nl
'    vec2 p = gl_in[0].gl_Position.xy;'nl

'    gl_Position = mvpMatrix * vec4(p.x - ssub, p.y - sadd, 0.0, 1.0);'nl
'    fragTexCoord = vec2(0.0, 1.0);'nl
'    EmitVertex();'nl
'    gl_Position = mvpMatrix * vec4(p.x - sadd, p.y + ssub, 0.0, 1.0);'nl
'    fragTexCoord = vec2(0.0, 0.0);'nl
'    EmitVertex();'nl
'    gl_Position = mvpMatrix * vec4(p.x + sadd, p.y - ssub, 0.0, 1.0);'nl
'    fragTexCoord = vec2(1.0, 1.0);'nl
'    EmitVertex();'nl
'    gl_Position = mvpMatrix * vec4(p.x + ssub, p.y + sadd, 0.0, 1.0);'nl
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

function LoadShader(const Kind: GLuint; const Code: PGLchar): GLuint;
var
  Status: GLint = GL_FALSE;
  Len: GLint;
  ErrorMsg: array of GLchar;
  S: String = '';
  I: Integer;
begin
  Result := glCreateShader(Kind);
  glShaderSource(Result, 1, @Code, nil);
  glCompileShader(Result);
  glGetShaderiv(Result, GL_COMPILE_STATUS, @Status);
  glGetShaderiv(Result, GL_INFO_LOG_LENGTH, @Len);
  if Status = GL_FALSE then
  begin
    if Kind = GL_VERTEX_SHADER then
      WritelnLog('TCastle2DParticleEmitterGPU', 'Compile vertex shader failed!')
    else if Kind = GL_GEOMETRY_SHADER then
      WritelnLog('TCastle2DParticleEmitterGPU', 'Compile geometry shader failed!')
    else
      WritelnLog('TCastle2DParticleEmitterGPU', 'Compile fragment shader failed!');
    SetLength(ErrorMsg, Len + 1);
    glGetShaderInfoLog(Result, Len, nil, @ErrorMsg[0]);
    for I := 0 to Len do
      S := S + (String(ErrorMsg[I]));
    WritelnLog('TCastle2DParticleEmitterGPU', S);
  end;
end;

constructor TCastle2DParticleEmitterGPU.Create(AOwner: TComponent);
var
  V: Integer;
begin
  inherited;

  if Self.ShaderVert = 0 then
  begin
    // Check maximum number of vertices
    glGetIntegerv(GL_MAX_VERTEX_ATTRIBS, @V);
    WritelnLog('GL_MAX_VERTEX_ATTRIBS: ' + IntToStr(V));
    if V < 11 then
      WritelnWarning('TCastle2DParticleEmitterGPU', 'TCastle2DParticleEmitterGPU requires GL_MAX_VERTEX_ATTRIBS at least 11');
    // Drawing shader
    Self.ShaderVert := LoadShader(GL_VERTEX_SHADER, PChar(VertexShaderSource));
    Self.ShaderGeom := LoadShader(GL_GEOMETRY_SHADER, PChar(GeometryShaderSource));
    Self.ShaderFrag := LoadShader(GL_FRAGMENT_SHADER, PChar(FragmentShaderSource));

    // Transform & Feedback shader
    Self.ShaderTFVert := LoadShader(GL_VERTEX_SHADER, PChar(TransformVertexShaderSource));
  end;

  Self.ShaderProg := glCreateProgram();
  glAttachShader(Self.ShaderProg, Self.ShaderVert);
  glAttachShader(Self.ShaderProg, Self.ShaderGeom);
  glAttachShader(Self.ShaderProg, Self.ShaderFrag);
  glLinkProgram(Self.ShaderProg);

  Self.ShaderTFProg := glCreateProgram();
  glAttachShader(Self.ShaderTFProg, Self.ShaderTFVert);
  glTransformFeedbackVaryings(Self.ShaderTFProg, Length(Varyings), Varyings, GL_INTERLEAVED_ATTRIBS);
  glLinkProgram(Self.ShaderTFProg);

  // Map uniform
  Self.UniformMVPMatrix := glGetUniformLocation(Self.ShaderProg, 'mvpMatrix');
  Self.UniformSourcePosition := glGetUniformLocation(Self.ShaderTFProg, 'effect.sourcePosition');
  Self.UniformSourcePositionVariance := glGetUniformLocation(Self.ShaderTFProg, 'effect.sourcePositionVariance');
  Self.UniformSpeed := glGetUniformLocation(Self.ShaderTFProg, 'effect.speed');
  Self.UniformSpeedVariance := glGetUniformLocation(Self.ShaderTFProg, 'effect.speedVariance');
  Self.UniformParticleLifeSpan := glGetUniformLocation(Self.ShaderTFProg, 'effect.particleLifeSpan');
  Self.UniformParticleLifeSpanVariance := glGetUniformLocation(Self.ShaderTFProg, 'effect.particleLifeSpanVariance');
  Self.UniformAngle := glGetUniformLocation(Self.ShaderTFProg, 'effect.angle');
  Self.UniformAngleVariance := glGetUniformLocation(Self.ShaderTFProg, 'effect.angleVariance');
  Self.UniformGravity := glGetUniformLocation(Self.ShaderTFProg, 'effect.gravity');
  Self.UniformRadialAcceleration := glGetUniformLocation(Self.ShaderTFProg, 'effect.radialAcceleration');
  Self.UniformTangentialAcceleration := glGetUniformLocation(Self.ShaderTFProg, 'effect.tangentialAcceleration');
  Self.UniformRadialAccelVariance := glGetUniformLocation(Self.ShaderTFProg, 'effect.radialAccelVariance');
  Self.UniformTangentialAccelVariance := glGetUniformLocation(Self.ShaderTFProg, 'effect.tangentialAccelVariance');
  Self.UniformStartColor := glGetUniformLocation(Self.ShaderTFProg, 'effect.startColor');
  Self.UniformStartColorVariance := glGetUniformLocation(Self.ShaderTFProg, 'effect.startColorVariance');
  Self.UniformFinishColor := glGetUniformLocation(Self.ShaderTFProg, 'effect.finishColor');
  Self.UniformFinishColorVariance := glGetUniformLocation(Self.ShaderTFProg, 'effect.finishColorVariance');
  Self.UniformMaxParticles := glGetUniformLocation(Self.ShaderTFProg, 'effect.maxParticles');
  Self.UniformStartParticleSize := glGetUniformLocation(Self.ShaderTFProg, 'effect.startParticleSize');
  Self.UniformStartParticleSizeVariance := glGetUniformLocation(Self.ShaderTFProg, 'effect.startParticleSizeVariance');
  Self.UniformFinishParticleSize := glGetUniformLocation(Self.ShaderTFProg, 'effect.finishParticleSize');
  Self.UniformFinishParticleSizeVariance := glGetUniformLocation(Self.ShaderTFProg, 'effect.finishParticleSizeVariance');
  Self.UniformEmitterType := glGetUniformLocation(Self.ShaderTFProg, 'effect.emitterType');
  Self.UniformMaxRadius := glGetUniformLocation(Self.ShaderTFProg, 'effect.maxRadius');
  Self.UniformMaxRadiusVariance := glGetUniformLocation(Self.ShaderTFProg, 'effect.maxRadiusVariance');
  Self.UniformMinRadius := glGetUniformLocation(Self.ShaderTFProg, 'effect.minRadius');
  Self.UniformMinRadiusVariance := glGetUniformLocation(Self.ShaderTFProg, 'effect.minRadiusVariance');
  Self.UniformRotatePerSecond := glGetUniformLocation(Self.ShaderTFProg, 'effect.rotatePerSecond');
  Self.UniformRotatePerSecondVariance := glGetUniformLocation(Self.ShaderTFProg, 'effect.rotatePerSecondVariance');
  Self.UniformRotationStart := glGetUniformLocation(Self.ShaderTFProg, 'effect.rotationStart');
  Self.UniformRotationStartVariance := glGetUniformLocation(Self.ShaderTFProg, 'effect.rotationStartVariance');
  Self.UniformRotationEnd := glGetUniformLocation(Self.ShaderTFProg, 'effect.rotationEnd');
  Self.UniformRotationEndVariance := glGetUniformLocation(Self.ShaderTFProg, 'effect.rotationEndVariance');
  Self.UniformDeltaTime := glGetUniformLocation(Self.ShaderTFProg, 'deltaTime');
  Self.UniformEmissionTime := glGetUniformLocation(Self.ShaderTFProg, 'emissionTime');
  Self.VAOs[0] := 0;
  Self.Texture := 0;
  Self.FSecondsPassed := 0;
end;

destructor TCastle2DParticleEmitterGPU.Destroy;
begin
  if Self.VAOs[0] <> 0 then
  begin
    glDeleteBuffers(2, @Self.VBOs);
    glDeleteVertexArrays(2, @Self.VAOs);
  end;
  glDeleteProgram(Self.ShaderProg);
  glDeleteProgram(Self.ShaderTFProg);
  glFreeTexture(Self.Texture);
  inherited;
end;

procedure TCastle2DParticleEmitterGPU.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
  inherited;
  Self.FSecondsPassed := SecondsPassed;
  Self.FIsDrawn := False;

  if (FEmissionTime > 0) or (FEmissionTime = -1) then
  begin
    if FEmissionTime > 0 then
      FEmissionTime := Max(0, FEmissionTime - SecondsPassed);
  end;

  RemoveMe := rtNone;
  if Self.FReleaseWhenDone then
  begin
    if (Self.FEmissionTime = 0) then
      RemoveMe := rtRemoveAndFree;
  end;
end;

procedure TCastle2DParticleEmitterGPU.LocalRender(const Params: TRenderParams);
var
  M: TMatrix4;
  PrevShader: GLuint;
begin
  inherited;

  if Self.VAOs[0] = 0 then
    Exit;

  // Why LocalRender get called 2 times in a frame?
  if not Self.FIsDrawn then
    Self.FIsDrawn := True
  else
    Exit;

  M := RenderContext.ProjectionMatrix * Params.RenderingCamera.Matrix * Params.Transform^;

  // Update particles
  glGetIntegerv(GL_CURRENT_PROGRAM, @PrevShader);
  glEnable(GL_RASTERIZER_DISCARD);
  glUseProgram(Self.ShaderTFProg);
  glUniform1f(Self.UniformDeltaTime, Self.FSecondsPassed);
  glUniform1f(Self.UniformEmissionTime, Self.FEmissionTime);
  glUniform2fv(Self.UniformSourcePosition, 1, @Self.FPosition);
  glBindVertexArray(Self.VAOs[(CurrentBuffer + 1) mod 2]);
  glBindBufferBase(GL_TRANSFORM_FEEDBACK_BUFFER, 0, Self.VBOs[CurrentBuffer]);
  glBeginTransformFeedback(GL_POINTS);
  glDrawArrays(GL_POINTS, 0, Self.FEffect.MaxParticles);
  glEndTransformFeedback();
  glDisable(GL_RASTERIZER_DISCARD);

  // Draw particles
  glEnable(GL_BLEND);
  glBlendFunc(Self.FEffect.BlendFuncSource, Self.FEffect.BlendFuncDestination);
  glUseProgram(Self.ShaderProg);
  glUniformMatrix4fv(Self.UniformMVPMatrix, 1, GL_FALSE, @M);
  glBindVertexArray(Self.VAOs[CurrentBuffer]);
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, Self.Texture);
  glDrawArrays(GL_POINTS, 0, Self.FEffect.MaxParticles);
  glBindTexture(GL_TEXTURE_2D, 0);
  glBindVertexArray(0);
  glUseProgram(PrevShader);
  glDisable(GL_BLEND);

  CurrentBuffer := (CurrentBuffer + 1) mod 2;
end;

procedure TCastle2DParticleEmitterGPU.LoadEffect(const AURL: String);
begin
  if Assigned(Self.FEffect) then
    FreeAndNil(Self.FEffect);
  Self.FEffect := TCastle2DParticleEffect.Create;
  Self.FEffect.Load(AURL);
  Self.FURL := AURL;
  Self.RefreshEffect;
end;

procedure TCastle2DParticleEmitterGPU.LoadEffect(const AEffect: TCastle2DParticleEffect;
    const AOwnEffect: Boolean = true);
begin
  if AOwnEffect then
  begin
    if Assigned(Self.FEffect) then
      FreeAndNil(Self.FEffect);
    Self.FEffect := AEffect;
  end else
    AEffect.Clone(Self.FEffect);
  Self.RefreshEffect;
end;

procedure TCastle2DParticleEmitterGPU.RefreshEffect;
var
  I: Integer;
  TimeBetweenParticles,
  Lifespan: Single;
begin
  Self.FEmissionTime := Self.FEffect.Duration;
  Self.FEmitParticleTime := 0;
  Self.FParticleCount := 1;
  SetLength(Self.Particles, Self.FEffect.MaxParticles);

  if Self.FEffect.ParticleLifeSpan = 0 then
    Self.FEffect.ParticleLifeSpan := 0.001;

  glFreeTexture(Self.Texture);
  Self.Texture := LoadGLTexture(
    Self.FEffect.Texture,
    TextureFilter(minNearest, magNearest),
    Texture2DClampToEdge
  );

  // Cleanup current VAOs & VBOs
  if Self.VAOs[0] <> 0 then
  begin
    glDeleteBuffers(2, @Self.VBOs);
    glDeleteVertexArrays(2, @Self.VAOs);
  end;
  // Transfer effect settings to shader
  glUseProgram(Self.ShaderTFProg);
    glUniform2fv(Self.UniformSourcePosition, 1, @Self.FEffect.SourcePosition);
    glUniform2fv(Self.UniformSourcePositionVariance, 1, @Self.FEffect.SourcePositionVariance);
    glUniform1f(Self.UniformSpeed, Self.FEffect.Speed);
    glUniform1f(Self.UniformSpeedVariance, Self.FEffect.SpeedVariance);
    glUniform1f(Self.UniformParticleLifeSpan, Self.FEffect.ParticleLifeSpan);
    glUniform1f(Self.UniformParticleLifeSpanVariance, Self.FEffect.ParticleLifeSpanVariance);
    glUniform1f(Self.UniformAngle, Self.FEffect.Angle);
    glUniform1f(Self.UniformAngleVariance, Self.FEffect.AngleVariance);
    glUniform2fv(Self.UniformGravity, 1, @Self.FEffect.Gravity);
    glUniform1f(Self.UniformRadialAcceleration, Self.FEffect.RadialAcceleration);
    glUniform1f(Self.UniformTangentialAcceleration, Self.FEffect.TangentialAcceleration);
    glUniform1f(Self.UniformRadialAccelVariance, Self.FEffect.RadialAccelVariance);
    glUniform1f(Self.UniformTangentialAccelVariance, Self.FEffect.TangentialAccelVariance);
    glUniform4fv(Self.UniformStartColor, 1, @Self.FEffect.StartColor);
    glUniform4fv(Self.UniformStartColorVariance, 1, @Self.FEffect.StartColorVariance);
    glUniform4fv(Self.UniformFinishColor, 1, @Self.FEffect.FinishColor);
    glUniform4fv(Self.UniformFinishColorVariance, 1, @Self.FEffect.FinishColorVariance);
    glUniform1i(Self.UniformMaxParticles, Self.FEffect.MaxParticles);
    glUniform1f(Self.UniformStartParticleSize, Self.FEffect.StartParticleSize);
    glUniform1f(Self.UniformStartParticleSizeVariance, Self.FEffect.StartParticleSizeVariance);
    glUniform1f(Self.UniformFinishParticleSize, Self.FEffect.FinishParticleSize);
    glUniform1f(Self.UniformFinishParticleSizeVariance, Self.FEffect.FinishParticleSizeVariance);
    glUniform1i(Self.UniformEmitterType, LongInt(Self.FEffect.EmitterType));
    glUniform1f(Self.UniformMaxRadius, Self.FEffect.MaxRadius);
    glUniform1f(Self.UniformMaxRadiusVariance, Self.FEffect.MaxRadiusVariance);
    glUniform1f(Self.UniformMinRadius, Self.FEffect.MinRadius);
    glUniform1f(Self.UniformMinRadiusVariance, Self.FEffect.MinRadiusVariance);
    glUniform1f(Self.UniformRotatePerSecond, Self.FEffect.RotatePerSecond);
    glUniform1f(Self.UniformRotatePerSecondVariance, Self.FEffect.RotatePerSecondVariance);
    glUniform1f(Self.UniformRotationStart, Self.FEffect.RotationStart);
    glUniform1f(Self.UniformRotationStartVariance, Self.FEffect.RotationStartVariance);
    glUniform1f(Self.UniformRotationEnd, Self.FEffect.RotationEnd);
    glUniform1f(Self.UniformRotationEndVariance, Self.FEffect.RotationEndVariance);
  glUseProgram(0);
  // Generate initial lifecycle
  TimeBetweenParticles := Self.FEffect.ParticleLifeSpan / Self.FEffect.MaxParticles;
  LifeSpan := 0;
  for I := 0 to Self.FEffect.MaxParticles - 1 do
  begin
    with Self.Particles[I] do
    begin
      TimeToLive := LifeSpan;
      Position := Vector2(Random, Random);
    end;
    LifeSpan := LifeSpan - TimeBetweenParticles;
  end;
  // Drawing VAO
  Self.CurrentBuffer := 0;
  glGenVertexArrays(2, @Self.VAOs);
  glGenBuffers(2, @Self.VBOs);
  for I := 0 to 1 do
  begin
    glBindVertexArray(Self.VAOs[I]);

    glBindBuffer(GL_ARRAY_BUFFER, Self.VBOs[I]);
    glBufferData(GL_ARRAY_BUFFER, Self.FEffect.MaxParticles * SizeOf(TCastle2DParticle), @Self.Particles[0], GL_STATIC_DRAW);

    glEnableVertexAttribArray(0);
    glEnableVertexAttribArray(1);
    glEnableVertexAttribArray(2);
    glEnableVertexAttribArray(3);
    glEnableVertexAttribArray(4);
    glEnableVertexAttribArray(5);
    glEnableVertexAttribArray(6);
    glEnableVertexAttribArray(7);
    glEnableVertexAttribArray(8);
    glEnableVertexAttribArray(9);
    glEnableVertexAttribArray(10);
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, SizeOf(TCastle2DParticle), Pointer(0));
    glVertexAttribPointer(1, 1, GL_FLOAT, GL_FALSE, SizeOf(TCastle2DParticle), Pointer(8));
    glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, SizeOf(TCastle2DParticle), Pointer(12));
    glVertexAttribPointer(3, 2, GL_FLOAT, GL_FALSE, SizeOf(TCastle2DParticle), Pointer(20));
    glVertexAttribPointer(4, 4, GL_FLOAT, GL_FALSE, SizeOf(TCastle2DParticle), Pointer(28));
    glVertexAttribPointer(5, 4, GL_FLOAT, GL_FALSE, SizeOf(TCastle2DParticle), Pointer(44));
    glVertexAttribPointer(6, 2, GL_FLOAT, GL_FALSE, SizeOf(TCastle2DParticle), Pointer(60));
    glVertexAttribPointer(7, 2, GL_FLOAT, GL_FALSE, SizeOf(TCastle2DParticle), Pointer(68));
    glVertexAttribPointer(8, 2, GL_FLOAT, GL_FALSE, SizeOf(TCastle2DParticle), Pointer(76));
    glVertexAttribPointer(9, 2, GL_FLOAT, GL_FALSE, SizeOf(TCastle2DParticle), Pointer(84));
    glVertexAttribPointer(10, 2, GL_FLOAT, GL_FALSE, SizeOf(TCastle2DParticle), Pointer(92));

    glBindVertexArray(0);
  end;
end;

initialization
  TCastle2DParticleEmitterGPU.ShaderVert := 0;
  RegisterSerializableComponent(TCastle2DParticleEmitterGPU, '2D Particle Emitter (GPU)');

end.