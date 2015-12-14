program golfhelperwin64;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, umainform;

{$R *.res}
begin
  Application.Title:='Golf Helper';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(Tmainform, mainform);
  Application.Run;
end.

