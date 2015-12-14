program ugolfmainform;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, ugolfmainform, golfumlclass
  { you can add units after this };

{$R *.res}

begin
  application.title:='Golf UML';
  Application.Initialize;
  application.createform(tmainform, mainform);
  Application.Run;
end.

