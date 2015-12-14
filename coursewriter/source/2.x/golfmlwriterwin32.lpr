program golfmlwriterwin32;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, umainform, ugolfmlwriter_globals,uwaitform, ucourseteepicker
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='Golfml CourseWriter';
  Application.Initialize;
  Application.CreateForm(Tmainform,mainform);
  Application.CreateForm(Twaitform,waitform);
  Application.CreateForm(Tcourseteepicker,courseteepicker);
  Application.Run;
end.

