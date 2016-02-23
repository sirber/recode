program recode;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, laz_synapse, Unit1
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='Recode';
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

