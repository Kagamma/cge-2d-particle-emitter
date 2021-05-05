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
  CastleViewport, CastleUIControls,
  Castle3D, CastleVectors, CastleTransform,
  X3DTime, X3DFields,
  Castle2DParticleEmitter, Castle2DParticleEmitterGPU;

const
  RING_NUMBER = 10;
  RING_ITEM = 10;

var
  Viewport: TCastleViewport;
  Emitter: TCastle2DParticleEmitterGPU;
  T: Single;
  Instances: packed array[0..RING_NUMBER * RING_ITEM - 1] of TCastle2DParticleInstance;

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

  Emitter := TCastle2DParticleEmitterGPU.Create(Viewport);
  Emitter.LoadEffect(ApplicationData('fire.pex'));
  Emitter.StartEmitting := True;

  Viewport.Items.Add(Emitter);
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
      Instances[I + J * RING_ITEM].Translation := Vector2(Cos(F) * J * 30, Sin(F) * J * 30);
      Instances[I + J * RING_ITEM].Rotation := F - PI;
    end;
    G := G + 0.05;
  end;
  Emitter.SetInstances(Instances);
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
