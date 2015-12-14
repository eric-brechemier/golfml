program golfuml;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, ugolfmainform, thandicapgolfumlclass
  { you can add units after this };

{$R *.res}

begin
  application.title:='Golf UML (Win 64)';
  Application.Initialize;
  application.createform(tmainform, mainform);
  Application.Run;
end.

