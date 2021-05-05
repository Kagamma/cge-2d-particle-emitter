{$mode objfpc}{$H+}

unit Game;

interface

implementation

uses
  SysUtils, CastleWindow, CastleScene, CastleControls,
  CastleFilesUtils, CastleSceneCore, CastleKeysMouse, CastleColors,
  CastleViewport, CastleUIControls,
  CastleVectors, CastleTransform,
  X3DTime, X3DFields,
  Castle2DParticleEmitter, Castle2DParticleEmitterGPU;

var
  Window: TCastleWindowBase;
  Viewport: TCastleViewport;
  MenuScene: TCastleScene;
  Emitter: TCastle2DParticleEmitterGPU;
  CurrentMenu: integer = 0;
  EffectFire,
  EffectTrippy,
  EffectJellyFish,
  EffectSpiral: TCastle2DParticleEffect;
  T: TCastleTransform;

type
  TScriptHelper = class
    class procedure FireEffect(AValue: TX3DField; const ATime: TX3DTime);
    class procedure TrippyEffect(AValue: TX3DField; const ATime: TX3DTime);
    class procedure JellyFishEffect(AValue: TX3DField; const ATime: TX3DTime);
    class procedure SpiralEffect(AValue: TX3DField; const ATime: TX3DTime);
  end;

procedure NewEmitter(const AEffect: TCastle2DParticleEffect);
begin
  Emitter.EmissionTime := 0;
  Emitter.ReleaseWhenDone := true;
  Emitter := TCastle2DParticleEmitterGPU.Create(T);
  Emitter.LoadEffect(AEffect, False);
  Emitter.StartEmitting := True;
  T.Add(Emitter);
end;

class procedure TScriptHelper.FireEffect(AValue: TX3DField; const ATime: TX3DTime);
begin
  if CurrentMenu <> 0 then
    NewEmitter(EffectFire);
  CurrentMenu := 0;
end;

class procedure TScriptHelper.TrippyEffect(AValue: TX3DField; const ATime: TX3DTime);
begin
  if CurrentMenu <> 1 then
    NewEmitter(EffectTrippy);
  CurrentMenu := 1;
end;

class procedure TScriptHelper.JellyFishEffect(AValue: TX3DField; const ATime: TX3DTime);
begin
  if CurrentMenu <> 2 then
    NewEmitter(EffectJellyFish);
  CurrentMenu := 2;
end;

class procedure TScriptHelper.SpiralEffect(AValue: TX3DField; const ATime: TX3DTime);
begin
  if CurrentMenu <> 3 then
    NewEmitter(EffectSpiral);
  CurrentMenu := 3;
end;

procedure ApplicationInitialize;
begin
  Window.Container.UIReferenceWidth := 1280;
  Window.Container.UIReferenceHeight := 720;
  Window.Container.UIScaling := usFitReferenceSize;

  Viewport := TCastleViewport.Create(Application);
  Viewport.Setup2D;
  Viewport.FullSize := true;
  Viewport.Camera.Orthographic.Width := 1280;
  Viewport.Camera.Orthographic.Height := 720;
  Viewport.Camera.Orthographic.Origin := Vector2(0.5, 0.5);
  Window.Controls.InsertFront(Viewport);

  MenuScene := TCastleScene.Create(Viewport);
  MenuScene.Setup2D;
  MenuScene.Load(ApplicationData('menu.x3dv'));
  MenuScene.RegisterCompiledScript('fire_effect', @TScriptHelper(nil).FireEffect);
  MenuScene.RegisterCompiledScript('trippy_effect', @TScriptHelper(nil).TrippyEffect);
  MenuScene.RegisterCompiledScript('jellyfish_effect', @TScriptHelper(nil).JellyFishEffect);
  MenuScene.RegisterCompiledScript('spiral_effect', @TScriptHelper(nil).SpiralEffect);
  MenuScene.Spatial := [ssDynamicCollisions];
  MenuScene.ProcessEvents := true;
  Viewport.Items.Add(MenuScene);

  EffectFire := TCastle2DParticleEffect.Create;
  EffectTrippy := TCastle2DParticleEffect.Create;
  EffectJellyFish := TCastle2DParticleEffect.Create;
  EffectSpiral := TCastle2DParticleEffect.Create;
  EffectFire.Load(ApplicationData('fire.pex'));
  EffectTrippy.Load(ApplicationData('trippy.pex'));
  EffectJellyFish.Load(ApplicationData('jellyfish.pex'));
  EffectSpiral.Load(ApplicationData('spiral.pex'));

  T := TCastleTransform.Create(Viewport);
  T.Translation := Vector3(-213, 0, 0);
  Emitter := TCastle2DParticleEmitterGPU.Create(T);
  Emitter.LoadEffect(EffectFire, False);
  Emitter.StartEmitting := True;
  T.Add(Emitter);

  Viewport.Items.Add(T);
end;

procedure WindowRender(Container: TUIContainer);
begin
  UIFont.Print(10, 40, Yellow, Format('Particle count: %d', [Emitter.ParticleCount]));
  UIFont.Print(10, 10, Yellow, Format('FPS: %s', [Container.Fps.ToString]));
end;

procedure WindowUpdate(Container: TUIContainer);
begin
end;

initialization
  Application.OnInitialize := @ApplicationInitialize;

  Window := TCastleWindowBase.Create(Application);
  Application.MainWindow := Window;
  Window.OnRender := @WindowRender;
  Window.OnUpdate := @WindowUpdate;
finalization
  FreeAndNil(EffectFire);
  FreeAndNil(EffectTrippy);
  FreeAndNil(EffectJellyFish);
  FreeAndNil(EffectSpiral);
end.
