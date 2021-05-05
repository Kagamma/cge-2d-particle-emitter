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

var
  Viewport: TCastleViewport;
  Emitter: TCastle2DParticleEmitterGPU;
  T: Single;

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
  Emitter.LoadEffect(ApplicationData('spiral.pex'));
  Emitter.StartEmitting := True;

  Viewport.Items.Add(Emitter);
end;

procedure WindowRender(Container: TUIContainer);
begin
  UIFont.Print(10, 40, Yellow, Format('Particle count: %d', [Emitter.ParticleCount]));
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
