{$mode objfpc}{$H+}

unit Game;

interface

uses
  CastleWindowTouch;

var
  Window: TCastleWindowTouch;

implementation

uses
  SysUtils, CastleWindow, CastleScene, CastleControls,
  CastleFilesUtils, CastleSceneCore, CastleKeysMouse, CastleColors,
  Castle2DSceneManager, CastleUIControls,
  Castle3D, CastleVectors, CastleTransform,
  X3DTime, X3DFields,
  Castle2DParticleEmitter;

var
  SceneManager: T2DSceneManager;
  MenuScene: T2DScene;
  Emitter: TCastle2DParticleEmitter;
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
  Emitter := TCastle2DParticleEmitter.Create(T);
  Emitter.LoadPEX(AEffect, false);
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

  SceneManager := T2DSceneManager.Create(Application);
  SceneManager.ProjectionAutoSize := false;
  SceneManager.ProjectionWidth := 1280;
  SceneManager.ProjectionHeight := 720;
  SceneManager.ProjectionOriginCenter := true;
  Window.Controls.InsertFront(SceneManager);

  MenuScene := T2DScene.Create(SceneManager);
  MenuScene.Load(ApplicationData('menu.x3dv'));
  MenuScene.RegisterCompiledScript('fire_effect', @TScriptHelper(nil).FireEffect);  
  MenuScene.RegisterCompiledScript('trippy_effect', @TScriptHelper(nil).TrippyEffect);
  MenuScene.RegisterCompiledScript('jellyfish_effect', @TScriptHelper(nil).JellyFishEffect);
  MenuScene.RegisterCompiledScript('spiral_effect', @TScriptHelper(nil).SpiralEffect);
  MenuScene.Spatial := [ssDynamicCollisions];
  MenuScene.ProcessEvents := true;
  SceneManager.Items.Add(MenuScene);
                                            
  EffectFire := TCastle2DParticleEffect.Create;  
  EffectTrippy := TCastle2DParticleEffect.Create;
  EffectJellyFish := TCastle2DParticleEffect.Create;
  EffectSpiral := TCastle2DParticleEffect.Create;
  EffectFire.Load(ApplicationData('fire.pex'));
  EffectTrippy.Load(ApplicationData('trippy.pex'));
  EffectJellyFish.Load(ApplicationData('jellyfish.pex'));
  EffectSpiral.Load(ApplicationData('spiral.pex'));

  T := TCastleTransform.Create(SceneManager);
  T.Translation := Vector3(-213, 0, 0);
  T.Scale := Vector3(1, -1, 1);
  Emitter := TCastle2DParticleEmitter.Create(T);
  Emitter.LoadPEX(EffectFire, false);
  T.Add(Emitter);

  SceneManager.Items.Add(T);
end;

procedure WindowRender(Container: TUIContainer);
begin
  UIFont.Print(10, 40, Yellow, Format('Particle count: %d', [Emitter.ParticleCount]));
  UIFont.Print(10, 10, Yellow, Format('FPS: %f', [Container.Fps.RealFps]));
end;

procedure WindowUpdate(Container: TUIContainer);
begin
end;

function MyGetApplicationName: string;
begin
  Result := 'test_particle_emitter';
end;

initialization
  OnGetApplicationName := @MyGetApplicationName;
  Application.OnInitialize := @ApplicationInitialize;

  Window := TCastleWindowTouch.Create(Application);
  Application.MainWindow := Window;
  Window.OnRender := @WindowRender;
  Window.OnUpdate := @WindowUpdate;

end.
