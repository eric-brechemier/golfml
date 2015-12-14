program golfuml_linux32;

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
  Application.Title:='Golf UML Linux';
  Application.Initialize;
  application.createform(tmainform, mainform);
  Application.Run;
end.

