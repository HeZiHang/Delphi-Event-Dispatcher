program pBatchControl;

uses
  Vcl.Forms,
  uFormChild in 'uFormChild.pas' {FormChild},
  uInterfaceDispatcher in '..\..\Source\uInterfaceDispatcher.pas',
  uFormMain in 'uFormMain.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
