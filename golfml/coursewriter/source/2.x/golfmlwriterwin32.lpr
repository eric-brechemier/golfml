program golfmlwriterwin32;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, umainform, ugolfmlwriter_globals,uwaitform
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='Golfml CourseWriter';
  Application.Initialize;
  Application.CreateForm(Tmainform,mainform);
  Application.CreateForm(Twaitform,waitform);
  Application.Run;
end.

