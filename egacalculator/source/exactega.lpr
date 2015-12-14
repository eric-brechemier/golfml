program exactega;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, umainform, uscoreform;

{$R *.res}

begin
  Application. Title:='Exact EGA Calculator';
  Application.Initialize;
  Application. CreateForm( Tmainform, mainform);
  Application. CreateForm( Tscoreform, scoreform);
  Application.Run;
end.

