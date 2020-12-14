unit uFormChild;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  {$M+}
  IFormEvents=interface
    ['{CF1DEFDF-1D48-4AD3-B470-3CF10FB8D95A}']
    procedure DisposeOf;
    procedure Hide;
    procedure Show;
    procedure SetLabel(const Value: String);
  end;


  TFormChild = class(TForm, IFormEvents)
    Label1: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetLabel(const Value: String);
  end;

var
  FormChild: TFormChild;

implementation

{$R *.dfm}

{ TFormChild }

procedure TFormChild.SetLabel(const Value: String);
begin
  Label1.Caption:=Value;
end;

end.
