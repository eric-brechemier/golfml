program golfumlwin32;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, ugolfmainform, thandicapgolfumlclass;

{$R *.res}

begin
  Application.Title:='GolfML Reader';
  Application.Initialize;
  application.createform(tmainform, mainform);
  Application.Run;
end.

