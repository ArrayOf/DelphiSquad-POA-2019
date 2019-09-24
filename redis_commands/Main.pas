unit Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Threading,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Imaging.pngimage,
  Vcl.ExtCtrls,
  Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Image1: TImage;
    Panel3: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Panel4: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    ImageMapa: TImage;
    Memo1: TMemo;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    FLoadTask: IFuture<Integer>;
    procedure ShowMap(AContent: TStringStream);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Rules,
  Vcl.Imaging.jpeg;

procedure TForm1.Button1Click(Sender: TObject);
var
  oResponse: TStringStream;

begin
  Screen.Cursor := crHourGlass;
  oResponse     := nil;

  try
    oResponse := DataModule1.GetMap;
    Self.ShowMap(oResponse);
  finally
    oResponse.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  oResponse: TStringStream;
begin
  Screen.Cursor := crHourGlass;
  oResponse     := nil;

  try
    oResponse := DataModule1.WhereAreWe;
    Self.ShowMap(oResponse);
  finally
    oResponse.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  sFileName: string;
begin
  sFileName := 'E:\ArrayOf_DelphiSquad-POA-2019\dataset\TUP.csv';

  Self.FLoadTask := DataModule1.LoadListTUP(sFileName);

  Self.Button3.Enabled := False;
  Self.Timer1.Enabled  := True;

  Application.ProcessMessages;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
  FormatSettings.DecimalSeparator := '.';

  Self.FLoadTask := nil;

  Self.PageControl1.ActivePageIndex := 0;
end;

procedure TForm1.ShowMap(AContent: TStringStream);
var
  oJPG: TJPEGImage;
  oBMP: TBitmap;
begin
  oJPG := TJPEGImage.Create;
  oBMP := TBitmap.Create;

  oJPG.LoadFromStream(AContent);
  oBMP.Assign(oJPG);

  Self.ImageMapa.Picture.Bitmap.Assign(oBMP);

  oJPG.Free;
  oBMP.Free;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Self.Timer1.Enabled := False;

  if Assigned(Self.FLoadTask) then
  begin
    case Self.FLoadTask.Status of
      TTaskStatus.Created:
        ;
      TTaskStatus.WaitingToRun:
        ;
      TTaskStatus.Running:
        begin
          Self.Memo1.Lines.Add('Tarefa em andamento!');
        end;
      TTaskStatus.Completed:
        begin
          Self.Memo1.Lines.Add('Tarefa completa!');
          Self.Memo1.Lines.Add(Self.FLoadTask.Value.Tostring);
          Self.FLoadTask := nil;
        end;
      TTaskStatus.WaitingForChildren:
        ;
      TTaskStatus.Canceled:
        ;
      TTaskStatus.Exception:
        begin
          Self.Memo1.Lines.Add('Deu exception!');
          Self.FLoadTask := nil;
        end;
    end;

    if Assigned(Self.FLoadTask) then
    begin
      Self.Timer1.Enabled := True;
    end else
    begin
      Self.Button3.Enabled := True;
      Application.ProcessMessages;
    end;
  end;

end;

end.
