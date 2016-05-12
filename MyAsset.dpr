program MyAsset;

uses
  FMX.Forms,
  Main in 'Main.pas' {Form1},
  LUX in '_LIBRARY\LUXOPHIA\LUX\LUX.pas',
  LIB.Asset in '_LIBRARY\LIB.Asset.pas',
  LUX.FMX.Material in '_LIBRARY\LUXOPHIA\LUX.FMX\LUX.FMX.Material.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
