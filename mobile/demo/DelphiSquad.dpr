program DelphiSquad;

uses
  System.StartUpCopy,
  FMX.Forms,
  GoogleMaps in 'GoogleMaps.pas' {FormGoogleMaps},
  LocationSensor in 'LocationSensor.pas' {FormLocationSensor},
  Rest in 'Rest.pas' {FormRest};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Portrait];
  Application.CreateForm(TFormGoogleMaps, FormGoogleMaps);
  //Application.CreateForm(TFormRestTimer, FormRestTimer);
  //Application.CreateForm(TFormRest, FormRest);
  //Application.CreateForm(TFormRest, FormRest);
  //Application.CreateForm(TFormLocationSensor, FormLocationSensor);
  Application.Run;
end.
