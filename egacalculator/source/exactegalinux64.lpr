program exactegalinux64;
(*
  == Exact EGA Calculator
  == Author: minesadorada
  == Lazarus 9.30.2
  == FPC 2.4.4
  ==
  == Version 1.0
  == [20120318]
  == Initial Build Win64,Win32,Linux32
  == [20120322]
  == Integrated HCPClass
*)
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,umainform,uscoreform,usplashform,
  thandicapclassunit;

{$R *.res}
begin
  Application.Title:='EGA Handicap Calculator';
  Application.Initialize;
  Application. CreateForm( Tmainform, mainform);
  Application. CreateForm( Tscoreform, scoreform);
  Application.CreateForm(Tsplashform, splashform);
  Application.Run;
end.

