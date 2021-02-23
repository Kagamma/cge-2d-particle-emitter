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

const
  RING_NUMBER = 10;
  RING_ITEM = 10;

var
  SceneManager: T2DSceneManager;
  Emitter: TCastle2DParticleEmitterGPU;
  T: Single;
  Instances: packed array[0..RING_NUMBER * RING_ITEM - 1] of TCastle2DParticleInstance;

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
  Emitter.LoadEffect(ApplicationData('fire.pex'));
  Emitter.StartEmitting := True;

  SceneManager.Items.Add(Emitter);
end;

procedure WindowRender(Container: TUIContainer);
begin
  UIFont.Print(10, 40, Yellow, Format('%d emitter instances, for a total of %d particles', [RING_NUMBER * RING_ITEM, RING_NUMBER * RING_ITEM* Emitter.ParticleCount]));
  UIFont.Print(10, 10, Yellow, Format('FPS: %s', [Container.Fps.ToString]));
end;

procedure WindowUpdate(Container: TUIContainer);
var
  I, J: Integer;
  F, G: Single;
begin
  // Instancing multiple particle emitters that share the same particles
  G := T;
  for J := 0 to RING_NUMBER - 1 do
  begin
    for I := 0 to RING_ITEM - 1 do
    begin
      F := G + I * PI * 2 / RING_ITEM;
      Instances[I + J * RING_ITEM].Translation := Vector2(Cos(F) * J * 30 + 40, Sin(F) * J * 30 + 40);
      Instances[I + J * RING_ITEM].Rotation := F - PI;
    end;
    G := G + 0.05;
  end;
  Emitter.SetInstanced(Instances);
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
