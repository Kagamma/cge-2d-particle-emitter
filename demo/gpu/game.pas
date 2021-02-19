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
  Castle2DParticleEmitter, Castle2DParticleEmitterGPU;

var
  SceneManager: T2DSceneManager;
  Emitter: TCastle2DParticleEmitterGPU;
  T: Single;

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

  Emitter := TCastle2DParticleEmitterGPU.Create(SceneManager);
  Emitter.LoadEffect(ApplicationData('spiral.pex'));
  Emitter.StartEmitting := True;
  Emitter.Scale := Vector3(1, -1, 1);

  SceneManager.Items.Add(Emitter);
end;

procedure WindowRender(Container: TUIContainer);
begin
  UIFont.Print(10, 10, Yellow, Format('FPS: %s', [Container.Fps.ToString]));
end;

procedure WindowUpdate(Container: TUIContainer);
begin
  // Move emitter around
  Emitter.Position := Vector2(Cos(T) * 250, Sin(T * 4) * 80);
  T := T + 0.01;
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
