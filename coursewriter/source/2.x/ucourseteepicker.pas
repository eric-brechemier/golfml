unit ucourseteepicker;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, fileutil, forms, controls, graphics, dialogs, StdCtrls,
  Buttons,ugolfmlwriter_globals;

type

  { Tcourseteepicker }

  Tcourseteepicker = class(tform)
    cmd_close: TBitBtn;
    cmb_coursepicker: tcombobox;
    cmb_teepicker: tcombobox;
    grp_main: TGroupBox;
    lbl_coursepicker: tlabel;
    lbl_teepicker: tlabel;
    procedure cmb_coursepickerchange(sender: tobject);
    procedure cmb_teepickerchange(sender: tobject);
    procedure formcreate(sender: tobject);
    procedure formshow(sender: tobject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  courseteepicker: Tcourseteepicker;

implementation

{$R *.lfm}
Uses umainform;

{ Tcourseteepicker }

procedure tcourseteepicker.formcreate(sender: tobject);
begin
  Icon:=Application.Icon;
end;

procedure tcourseteepicker.cmb_coursepickerchange(sender: tobject);
Var cCount:Cardinal;
begin
     cmb_teepicker.Clear;
     mainform.GolfmlClass.CourseIndex:=cmb_coursePicker.ItemIndex;
     For cCount:=0 to mainform.GolfmlClass.MaxTeeColourIndex do
     begin
         mainform.GolfmlClass.TeeColourIndex:=cCount;
         if mainform.GolfmlClass.TeeColour <> '' then
            cmb_teepicker.Items.Add(mainform.GolfmlClass.TeeColour);
     end;
     If cmb_teepicker.Items.Count > 0 then
        cmb_teepicker.ItemIndex:=0;

     // Set Globals
     sCoursePicker:=cmb_coursePicker.Items[cmb_coursePicker.ItemIndex];
     sTeePicker:=cmb_TeePicker.Items[cmb_TeePicker.ItemIndex];
end;

procedure tcourseteepicker.cmb_teepickerchange(sender: tobject);
begin
     sTeePicker:=cmb_TeePicker.Items[cmb_TeePicker.ItemIndex];
end;

procedure tcourseteepicker.formshow(sender: tobject);
Var cCount:Cardinal;
begin
     cmb_coursePicker.Clear;
     For cCount:=0 to mainform.GolfmlClass.MaxCourseIndex do
     begin
         mainform.GolfmlClass.CourseIndex:=cCount;
         cmb_coursePicker.Items.Add(mainform.GolfmlClass.CourseName);
     end;
     If cmb_coursePicker.Items.Count > 0 then
        cmb_coursePicker.ItemIndex:=0;

     cmb_teepicker.Clear;
     mainform.GolfmlClass.CourseIndex:=0;
     For cCount:=0 to mainform.GolfmlClass.MaxTeeColourIndex do
     begin
         mainform.GolfmlClass.TeeColourIndex:=cCount;
         if mainform.GolfmlClass.TeeColour <> '' then
            cmb_teepicker.Items.Add(mainform.GolfmlClass.TeeColour);
     end;
     If cmb_teepicker.Items.Count > 0 then
        cmb_teepicker.ItemIndex:=0;
     // Set Globals
     sCoursePicker:=cmb_coursePicker.Items[cmb_coursePicker.ItemIndex];
     sTeePicker:=cmb_TeePicker.Items[cmb_TeePicker.ItemIndex];

end;

end.

