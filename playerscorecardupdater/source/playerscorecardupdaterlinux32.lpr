program playerscorecardupdaterlinux32;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  umainform { you can add units after this };

{$R *.res}

begin
  application.title := 'Player Scorecard Updater';
//  RequireDerivedFormResource := True;
  Application.Initialize;
  application.createform(tmainform, mainform);
  Application.Run;
end.
