program pPort;

uses
  Vcl.Forms,
  uFormMain in 'uFormMain.pas' {FormMain},
  uFormStatus in 'uFormStatus.pas' {FormStatus},
  uPort in 'uPort.pas',
  uInterfaceDispatcher in '..\..\Source\uInterfaceDispatcher.pas',
  uFormMemo in 'uFormMemo.pas' {FormMemo};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormStatus, FormStatus);
  Application.CreateForm(TFormMemo, FormMemo);
  Application.Run;
end.
