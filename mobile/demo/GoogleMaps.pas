unit GoogleMaps;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Maps,
  FMX.Controls.Presentation, FMX.StdCtrls, System.Permissions, FMX.Objects;

type
  TFormGoogleMaps = class(TForm)
    MapView: TMapView;
    PnlTitlebar: TPanel;
    RclTitlebar: TRectangle;
    LblTitle: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormGoogleMaps: TFormGoogleMaps;

implementation

{$R *.fmx}
{$R *.NmXhdpiPh.fmx ANDROID}



procedure TFormGoogleMaps.FormCreate(Sender: TObject);
var
  coordinate : TMapCoordinate;

begin
  MapView.MapType := TMapType.Normal;

  //Center
  coordinate := TMapCoordinate.Create(-30.0277,-51.2287);
  MapView.Location := coordinate;

  //Pin
  MapView.AddMarker(TMapMarkerDescriptor.Create(coordinate, 'Tô Aqui'));


  //Zoom map
  MapView.Zoom := 5;

end;

end.
