unit uFormMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, uFormChild, uInterfaceDispatcher;

type

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Edit1: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Childs:IInterfaceObservable<IFormEvents>;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  i: Integer;
  F: TFormChild;
begin
  for i := 0 to 9 do
  begin
    F:= TFormChild.Create(Self);
    Childs.AddObserver(F);
  end;

  Button3Click(nil);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Childs.Source.Hide;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Self.FormStyle:=fsNormal;
  Childs.Source.Show;
  Self.FormStyle:=fsStayOnTop;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  Childs.Source.SetLabel(Edit1.Text);
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  Childs.Source.DisposeOf;
  Childs.RemoveAllObserver;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Childs:=TDioInterfaceDispatcher<IFormEvents>.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Childs:=nil;
end;

end.
