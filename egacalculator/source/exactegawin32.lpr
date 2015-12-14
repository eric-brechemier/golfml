program exactegawin32;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, umainform, usplashform, uscoreform
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='EGA Calculator';
  Application.Initialize;
  Application. CreateForm( Tmainform, mainform);
  Application. CreateForm( Tscoreform, scoreform);
  Application.CreateForm(Tsplashform, splashform);
  Application.Run;
end.

