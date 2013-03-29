unit umainform;

{ Golf Helper

  Copyright (C) 2013 Gordon Bamber (minesadorada@charcodelvalle.com)

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.

  Uses VersionSupport Mike Thompson - mike.cornflake@gmail.com
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  Menus, StdCtrls, ComCtrls, thcpclassUnit, tgolfmlclassUnit, IniFiles, lclintf,
  ExtCtrls, VersionSupport;

const
  C_AMENETYDELIMITERCHAR = ','; // Used in SplitAmenity
  C_ERRORAPOLOGY = 'An error has occurred that was not your fault.' + LineEnding;
  CR = LineEnding;

type

  { Tmainform }

  Tmainform = class(TForm)
    cmd_CONGUresetScores: TBitBtn;
    cmd_UpdateHandicap: TBitBtn;
    cmd_resetScores: TBitBtn;
    cmd_close: TBitBtn;
    cmd_CONGUUpdateHandicap: TBitBtn;
    edt_playerhandicap0: TEdit;
    edt_CONGUplayerhandicap0: TEdit;
    edt_CONGUplayerhandicap1: TEdit;
    edt_CONGUplayername: TEdit;
    edt_stablefordpoints: TEdit;
    edt_playername: TEdit;
    edt_playerhandicap: TEdit;
    edt_CONGUGross: TEdit;
    grp_CONGUHandicapScore: TGroupBox;
    grp_CONGUplayerinfo: TGroupBox;
    grp_CONGUStableford: TGroupBox;
    grp_StablefordSummary: TGroupBox;
    grp_HandicapScore: TGroupBox;
    grp_Stableford: TGroupBox;
    grp_playerinfo: TGroupBox;
    grp_CourseHoles: TGroupBox;
    grp_CourseAndTeeInfo: TGroupBox;
    grp_ClubAmenities: TGroupBox;
    grp_ClubInformation: TGroupBox;
    grp_CONGUScoreSummary: TGroupBox;
    Image1: TImage;
    Image2: TImage;
    lbl_CONGUNett: TLabel;
    lbl_GrossScore: TLabel;
    lbl_Category: TLabel;
    lbl_CONGUCategory: TLabel;
    lbl_CONGUNettScoreFirst9: TLabel;
    lbl_CONGUNettScoreLast9: TLabel;
    lbl_CONGUNettScoreTotal: TLabel;
    lbl_GeneralInformationHeading: TLabel;
    lbl_CONGIadjustedHandicap: TLabel;
    lbl_CONGUadjustedHandicap1: TLabel;
    lbl_decimal1: TLabel;
    lbl_CONGUplayerhandicap: TLabel;
    lbl_CONGUplayername: TLabel;
    lbl_CONGUGross: TLabel;
    lbl_CONGUGrossScoreFirst9: TLabel;
    lbl_CONGUGrossScoreLast9: TLabel;
    lbl_GeneralInformationParagraph: TLabel;
    lbl_StablefordScoreTotal: TLabel;
    lbl_StablefordScoreLast9: TLabel;
    lbl_StablefordScoreFirst9: TLabel;
    lbl_decimal: TLabel;
    lbl_playinghandicap1: TLabel;
    lbl_playinghandicap2: TLabel;
    lbl_adjustedHandicap: TLabel;
    lbl_adjustedHandicap1: TLabel;
    lbl_StablefordPoints: TLabel;
    lbl_playername: TLabel;
    lbl_playerhandicap: TLabel;
    lbl_playinghandicap: TLabel;
    lbl_CONGUGrossScoreTotal: TLabel;
    lbl_CourseNumberOfHoles: TLabel;
    lbl_CoursePar: TLabel;
    lbl_CourseLength: TLabel;
    lbl_CourseCourseIndex: TLabel;
    lbl_CourseSlopeIndex: TLabel;
    lbl_ClubCountry: TLabel;
    lbl_ClubMunicipality: TLabel;
    lbl_ClubPhone: TLabel;
    lbl_ClubPostcode: TLabel;
    lbl_ClubRegion: TLabel;
    lbl_ClubStreet: TLabel;
    lbl_ClubWebsite: TLabel;
    lbl_pageCourseClubName: TLabel;
    lbl_pageHandicapClubName: TLabel;
    lbl_pageGameClubName: TLabel;
    Memo_amenities: TMemo;
    mnu_helpAboutgolfml: TMenuItem;
    mnu_helpAbout: TMenuItem;
    mnu_helpAboutHCPClass: TMenuItem;
    mnu_optionsImperial: TMenuItem;
    mnu_optionsMetric: TMenuItem;
    mnu_Options: TMenuItem;
    mnu_fileRecentCourses: TMenuItem;
    mnu_help: TMenuItem;
    mnu_tee: TMenuItem;
    mnu_course: TMenuItem;
    mnu_fileImportGolfml: TMenuItem;
    mnu_fileExit: TMenuItem;
    mnu_File: TMenuItem;
    dlg_Import: TOpenDialog;
    page_container: TPageControl;
    page_information: TTabSheet;
    page_Course: TTabSheet;
    page_handicap: TTabSheet;
    page_congu: TTabSheet;
    ssbar: TStatusBar;
    topmenu: TMainMenu;
    ud_playerhandicap0: TUpDown;
    ud_playerhandicap1: TUpDown;
    ud_CONGUplayerhandicap0: TUpDown;
    ud_CONGUplayerhandicap1: TUpDown;
    ud_stablefordpoints: TUpDown;
    ud_CONGUGross: TUpDown;
    procedure cmd_CONGUUpdateHandicapClick(Sender: TObject);
    procedure cmd_resetScoresClick(Sender: TObject);
    procedure cmd_TestClick(Sender: TObject);
    procedure cmd_closeClick(Sender: TObject);
    procedure cmd_UpdateHandicapClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure Image2Click(Sender: TObject);
    procedure lbl_ClubWebsiteClick(Sender: TObject);
    procedure mnu_courseClick(Sender: TObject);
    procedure mnu_fileExitClick(Sender: TObject);
    procedure mnu_fileImportGolfmlClick(Sender: TObject);
    procedure mnu_helpAboutClick(Sender: TObject);
    procedure mnu_helpAboutgolfmlClick(Sender: TObject);
    procedure mnu_helpAboutHCPClassClick(Sender: TObject);
    procedure mnu_optionsImperialClick(Sender: TObject);
    procedure mnu_optionsMetricClick(Sender: TObject);
    procedure mnu_teeClick(Sender: TObject);
    procedure page_containerChange(Sender: TObject);
    procedure RecentCoursesMenuClick(Sender: TObject);
    procedure ud_CONGUGrossChanging(Sender: TObject; var AllowChange: boolean);
    procedure ud_CONGUGrossClick(Sender: TObject; Button: TUDBtnType);
    procedure ud_CONGUplayerhandicap0Click(Sender: TObject; Button: TUDBtnType);
    procedure ud_CONGUplayerhandicap1Click(Sender: TObject; Button: TUDBtnType);
    procedure ud_playerhandicap0Click(Sender: TObject; Button: TUDBtnType);
    procedure ud_playerhandicap1Click(Sender: TObject; Button: TUDBtnType);
    procedure ud_stablefordpointsClick(Sender: TObject; Button: TUDBtnType);
    procedure ShowHint(Sender: TObject);
  private
    { private declarations }
    CoursesStringList: TStringList;
    lbl_HoleNumberArray: array of TLabel;
    lbl_HoleNumberArray1: array of TLabel; // EGA Handicap Page
    edt_HandicapScoreArray: array of TEdit; // EGA Handicap Page
    lbl_StablefordPointsArray: array of TLabel; // EGA Handicap Page
    lbl_CONGUHoleNumberArray1: array of TLabel; // CONGU Handicap Page
    edt_CONGUHandicapScoreArray: array of TEdit; // CONGU Handicap Page
    lbl_CONGUStablefordPointsArray: array of TLabel; // CONGU Handicap Page
    lbl_HoleParArray: array of TLabel;
    lbl_HoleStrokeIndexArray: array of TLabel;
    lbl_HoleDistanceArray: array of TLabel;
    iCONGUNett: integer;
    procedure Process_edt_HandicapScoreArray(Sender: TObject;
      var Key: word; Shift: TShiftState);
    procedure Process_edt_CONGUHandicapScoreArray(Sender: TObject;
      var Key: word; Shift: TShiftState);
    procedure CourseMenuClick(Sender: TObject);
    procedure SetCourseFromMenu(const iCourse: cardinal);
    procedure LoadRecentCourseFromMenu(const iCourseIndexIntoStringList: integer);
    procedure TeeMenuClick(Sender: TObject);
    procedure SetTeeFromMenu(const iTeeColour: cardinal);
    procedure CloseApp;
    procedure InitNewCourse(const iCourse: cardinal);
    procedure ResetCourseMenu;
    procedure ResetTeeMenu;
    procedure ResetRecentCoursesMenu;
    procedure UpdatePagesFromCourse;
    procedure SplitAmenety(var fAmenityType, fAmenetyValue: string;
      const aString: string);
    procedure MakeHoleLabels;
    procedure MakeHandicapInputGrid;
    procedure EGAUpdatePlayingHandicap;
    procedure UpdateCategory;
    procedure SetHCPClassArrays;
    procedure EGAZeroScores;
    procedure CONGUZeroScores;
    procedure CONGUUpdateNett;
    procedure DisplayStablefordSummary;
    procedure DisplayCONGUSummary;
    procedure PopulateGeneralInfoPage;
  public
    { public declarations }
    GolfmlClass: TGolfmlClass;
    HCPClass: THCPClass;
    INI: TIniFile;
  end;

var
  mainform: Tmainform;

implementation

{$R *.lfm}

{ Tmainform }
procedure Tmainform.ShowHint(Sender: TObject);
var
  s: string;
begin
  s := GetLongHint(Application.Hint);
  if length(s) > 0 then
    ssbar.Panels[0].Text := s
  else
    ssbar.Panels[0].Text := Application.Title + ' (' + HCPClass.GetModeAsString + ' mode)';
end;

procedure Tmainform.PopulateGeneralInfoPage;
var
  s: string;
begin
  s := 'Getting Started:' + CR;
  s += 'In order to use this application, you first need to import a golf course (golfml) file. ';
  s += 'Click the File Menu, and choose either "' + mnu_fileImportGolfml.Caption +
    '" or "' + mnu_fileRecentCourses.Caption + '".';
  s += ' (This application has a number of Tenerife courses pre-installed, but you can make a golfml file for your own';
  s += ' course by launching the companion application "Golfml CourseWriter")' + CR;
  s += 'Once a course is loaded, 3 more Tabbed pages will appear:' + CR + CR;
  s += '1 ) The ' + page_Course.Caption +
    ' Page.  This displays all the information about your loaded club.';
  s += ' Click the Course and Tee menus to choose all the available courses and Tee positions within the club.'
    + CR + CR;
  s += '2 and 3 ) The ' + page_handicap.Caption + ' and ' +
    page_congu.Caption + ' Pages.  These are the "working pages" of';
  s += ' this application.  You will need to type in your name and handicap (EGA and/or CONGU) when first using them.';
  s += ' This information will be saved between sessions, but can be changed at any time.'
    + CR + CR;
  s += 'EGA/USGA uses your "playing handicap" and Category to calculate the Stableford points, whereas CONGU';
  s += ' uses the Standard Scratch Score and your CONGU handicap.  The adjustments use the appropriate calculations for each system.';
  lbl_GeneralInformationHeading.Caption := 'Welcome to ' + Application.Title;
  lbl_GeneralInformationParagraph.Caption := s;
end;

procedure Tmainform.Process_edt_CONGUHandicapScoreArray(Sender: TObject;
  var Key: word; Shift: TShiftState);
// Processes KeyUp and KeyDown event from all edt_CONGUHandicapScoreArray elements
// Note: edt_CONGUHandicapScoreArray[tempedit.Tag] is used to reference individual array number
// Note: No need to SetConguMode
var
  tempedit: TEdit;
  i: integer;
  singHcp: single;
begin
  tempedit := Sender as TEdit;
  if tempedit.Text = '' then
    Exit;
  SetHCPClassArrays;
  if TryStrToInt(tempedit.Text, i) = True then
  begin
    singHcp := ud_CONGUplayerhandicap0.Position +
      (ud_CONGUplayerhandicap1.Position div 10);
    HcpClass.ScoreArrayElement[tempedit.Tag] := i;
    HCPClass.CalculateConguAdjustedScore(singHcp, tempedit.Tag, tempedit.Tag);
    // Sets BestScore to Stableford points
    lbl_CONGUStablefordPointsArray[tempedit.Tag].Caption :=
      Format('Adjusted Score: %d', [HCPClass.AdjustedScore]);
  end
  else
    tempedit.Text := '0';
  DisplayCONGUSummary;
end;

procedure Tmainform.UpdateCategory;
var
  w: word;
  sing: single;
begin
  w := HCPClass.GetModeAsInteger; // Store Current Mode
  // EGA
  HCPClass.SetEGAMode;
  sing := ud_playerhandicap0.Position + (ud_playerhandicap1.Position / 10);
  HCPClass.GetCategory(sing);
  lbl_Category.Caption := Format('EGA Handicap Category: %d', [HCPClass.Category]);
  // CONGU
  HCPClass.SetCONGUMode;
  sing := ud_CONGUplayerhandicap0.Position + (ud_CONGUplayerhandicap1.Position / 10);
  HCPClass.GetCategory(sing);
  lbl_CONGUCategory.Caption := Format('CONGU Handicap Category: %d', [HCPClass.Category]);
  HCPClass.SetModeAsInteger(w); // Restore Current Mode
end;

procedure Tmainform.CONGUZeroScores;
// Reset all scores to zero and set StablefordPoints to 36
var
  w: word;
  sing: single;
begin
  SetHCPClassArrays;
  sing := ud_CONGUplayerhandicap0.Position + (ud_CONGUplayerhandicap1.Position / 10);
  for w := 0 to 17 do
  begin
    edt_CONGUHandicapScoreArray[w].Text := '0';
    HcpClass.ScoreArrayElement[w] := 0;
    HCPClass.CalculateConguAdjustedScore(sing, w, w);
    // Sets BestScore to Stableford points
    lbl_CONGUStablefordPointsArray[w].Caption :=
      Format('Adjusted Score: %d', [HCPClass.AdjustedScore]);
    lbl_CONGUStablefordPointsArray[w].Font.Color := clDefault;
  end;
  lbl_CONGUadjustedHandicap1.Caption:=Format('%.1f',[sing]);
  DisplayCONGUSummary;
  UpdateCategory;
end;

procedure Tmainform.EGAZeroScores;
// Reset all scores to zero and set StablefordPoints to 36
var
  w: word;
  sing: single;
begin
  for w := 0 to 17 do
  begin
    edt_HandicapScoreArray[w].Text := '0';
    lbl_StablefordPointsArray[w].Caption := '0 pts';
    lbl_StablefordPointsArray[w].Font.Color := clDefault;
    HCPClass.ScoreArrayElement[w] := 0;
  end;
  lbl_StablefordScoreFirst9.Caption := 'In:   0 pts';
  lbl_StablefordScoreLast9.Caption := 'Out:   0 pts';
  lbl_StablefordScoreTotal.Caption := 'Total:   0 pts';
  HCPClass.CalculateHcp36StablefordPoints;
  ud_stablefordpoints.Position := 36;
  EGAUpdatePlayingHandicap;
  UpdateCategory;
end;

procedure TMainform.DisplayCONGUSummary;
var
  w: word;
  singHCP: single;
begin
  SetHCPClassArrays;
  singHcp := ud_CONGUplayerhandicap0.Position + (ud_CONGUplayerhandicap1.Position div 10);
  if GolfmlClass.CourseNumberOfHoles > 8 then
  begin
    HCPClass.CalculateConguAdjustedScore(singHcp, 0, 8);
    lbl_CONGUGrossScoreFirst9.Caption := Format('In:   %d', [HCPClass.GrossScore]);
    lbl_CONGUNettScoreFirst9.Caption := Format('(In:   %d)', [HCPClass.AdjustedScore]);
  end;
  if GolfmlClass.CourseNumberOfHoles > 17 then
  begin
    HCPClass.CalculateConguAdjustedScore(singHcp, 9, 17);
    lbl_CONGUGrossScoreLast9.Caption := Format('Out:   %d', [HCPClass.GrossScore]);
    lbl_CONGUNettScoreLast9.Caption := Format('(Out:   %d)', [HCPClass.AdjustedScore]);
  end;
  HCPClass.CalculateConguAdjustedScore(singHcp, 0, GolfmlClass.CourseNumberOfHoles - 1);
  lbl_CONGUGrossScoreTotal.Caption := Format('Total:   %d', [HCPClass.GrossScore]);
  lbl_CONGUNettScoreTotal.Caption := Format('(Total:   %d)', [HCPClass.AdjustedScore]);
  w := TRUNC(GolfmlClass.CourseRating + 0.5);
  lbl_GrossScore.Caption := Format('Gross: %d', [HCPClass.AdjustedScore - w]);
  ud_CONGUGross.Position := HCPClass.AdjustedScore - w;
  CONGUUpdateNett;
end;

procedure Tmainform.DisplayStablefordSummary;
begin
  SetHCPClassArrays;
  if GolfmlClass.CourseNumberOfHoles > 8 then
    lbl_StablefordScoreFirst9.Caption :=
      Format('In:   %d pts', [HCPClass.CalculateStableFord(0, 8)]);
  if GolfmlClass.CourseNumberOfHoles > 17 then
    lbl_StablefordScoreLast9.Caption :=
      Format('Out:   %d pts', [HCPClass.CalculateStableFord(9, 17)]);
  lbl_StablefordScoreTotal.Caption :=
    Format('Total:   %d pts', [HCPClass.CalculateStableFord(
    0, GolfmlClass.CourseNumberOfHoles - 1)]);
end;

procedure Tmainform.cmd_UpdateHandicapClick(Sender: TObject);
begin
  // To update Stableford UpDown and playing handicap:

  SetHCPClassArrays;
  HCPClass.CalculateStableFord(0, GolfmlClass.CourseNumberOfHoles);
  if HCPClass.TotalStablefordPoints > 0 then
  begin
    ud_stablefordpoints.Position := HCPClass.TotalStablefordPoints;
    EGAUpdatePlayingHandicap;
    HCPClass.ShowHandicapAdjustmentReport;
    ud_playerhandicap0.Position := TRUNC(HCPClass.NewHandicap);
    ud_playerhandicap1.Position :=
      TRUNC((HCPClass.NewHandicap - TRUNC(HCPClass.NewHandicap)) * 10);
    EGAUpdatePlayingHandicap;
    HCPClass.EGAHandicap := HCPClass.NewHandicap;
    EGAZeroScores;
  end;
end;

procedure Tmainform.Process_edt_HandicapScoreArray(Sender: TObject;
  var Key: word; Shift: TShiftState);
// Processes KeyUp and KeyDown event from all edt_HandicapScoreArray elements
// Note: edt_HandicapScoreArray[tempedit.Tag] is used to reference individual array number
var
  tempedit: TEdit;
  i: integer;
begin
  tempedit := Sender as TEdit;
  if tempedit.Text = '' then
    Exit;
  if TryStrToInt(tempedit.Text, i) = True then
  begin
    SetHCPClassArrays; // Applies par,S.I. and Score to HCPClass arrays
    HCPClass.ScoreArrayElement[tempedit.Tag] := i;
    HCPClass.CalculateStableFord(tempedit.Tag, tempedit.Tag);
    // Sets BestScore to Stableford points
    lbl_StablefordPointsArray[tempedit.Tag].Caption :=
      Format('%d pts', [HCPClass.TotalStablefordPoints]);
    case HCPClass.TotalStablefordPoints of
      0: lbl_StablefordPointsArray[tempedit.Tag].Font.Color := clBlack;
      1: lbl_StablefordPointsArray[tempedit.Tag].Font.Color := clNavy;
      2: lbl_StablefordPointsArray[tempedit.Tag].Font.Color := clGreen;
      3: lbl_StablefordPointsArray[tempedit.Tag].Font.Color := clLime;
      else
        lbl_StablefordPointsArray[tempedit.Tag].Font.Color := clRed;
    end;
    DisplayStablefordSummary;
  end
  else
    tempedit.Text := '0';
{$IFDEF Debug}
  lbl_Test.Caption := Format('Points: %d', [HCPClass.BestScore]);
{$ENDIF}

end;

procedure Tmainform.SetHCPClassArrays;
// Called every time as user inputs any score into an edt_HandicapScoreArray1
var
  w: word;
  i: integer;
  s: string;
begin
  for w := 0 to GolfmlClass.CourseNumberOfHoles - 1 do
  begin
    GolfmlClass.HoleIndex := w;
    HCPClass.ParArrayElement[w] := GolfmlClass.Par;
    HCPClass.StrokeIndexArrayElement[w] := GolfmlClass.StrokeIndex;
  end;
end;


procedure Tmainform.cmd_TestClick(Sender: TObject);
begin
end;

procedure Tmainform.cmd_resetScoresClick(Sender: TObject);
begin
  CONGUZeroScores;
end;

procedure Tmainform.cmd_CONGUUpdateHandicapClick(Sender: TObject);
begin
  HCPClass.SetCONGUMode;
  CONGUUpdateNett;
  HCPClass.OldHandicap:=ud_CONGUplayerhandicap0.Position + (ud_CONGUplayerhandicap1.Position / 10);
  HCPClass.AdjustHandicap;
  lbl_CONGUadjustedHandicap1.Caption:=Format('%.1f',[HCPClass.NewHandicap]);
  HCPClass.ShowHandicapAdjustmentReport;
  ud_CONGUplayerhandicap0.Position := TRUNC(HCPClass.NewHandicap);
  ud_CONGUplayerhandicap1.Position :=
      TRUNC((HCPClass.NewHandicap - TRUNC(HCPClass.NewHandicap)) * 10);
  CONGUZeroScores;
end;

procedure Tmainform.MakeHandicapInputGrid;
// Called by Form Create
// Creates labels and edits for Course using arrays so that each control's properties
// can be referenced easily by usung its index
// Uses grp_HandicapScore,lbl_HoleNumberArray1,edt_HandicapScoreArray
var
  i, wCurrentLeft, wCurrentTop: word;
  StartTop, StartLeft: word;
begin
  // EGA GRID
  StartLeft := 6;
  StartTop := 15;
  // Hole Number
  SetLength(lbl_HoleNumberArray1, 18);
  wCurrentLeft := StartLeft;
  wCurrentTop := StartTop;
  for i := 0 to 8 do
  begin
    lbl_HoleNumberArray1[i] := TLabel.Create(nil);
    with lbl_HoleNumberArray1[i] do
    begin
      AutoSize := True;
      Caption := Format('Hole %d', [i + 1]);
      Top := wCurrentTop;
      Left := wCurrentLeft;
      TabStop := False;
      ParentColor := False;
      Parent := grp_HandicapScore;
    end;
    Inc(wCurrentTop, (grp_HandicapScore.Height div 10));
  end;
  wCurrentLeft := grp_HandicapScore.Width div 2;
  wCurrentTop := StartTop;
  for i := 9 to 17 do
  begin
    lbl_HoleNumberArray1[i] := TLabel.Create(nil);
    with lbl_HoleNumberArray1[i] do
    begin
      AutoSize := True;
      Caption := Format('Hole %d', [i + 1]);
      Top := wCurrentTop;
      Left := wCurrentLeft;
      TabStop := False;
      ParentColor := False;
      Parent := grp_HandicapScore;
    end;
    Inc(wCurrentTop, (grp_HandicapScore.Height div 10));
  end;

  // Edits
  Inc(StartLeft, lbl_HoleNumberArray1[0].Width + 50);
  SetLength(edt_HandicapScoreArray, 18);
  wCurrentLeft := StartLeft;
  wCurrentTop := StartTop;
  for i := 0 to 8 do
  begin
    edt_HandicapScoreArray[i] := TEdit.Create(nil);
    with edt_HandicapScoreArray[i] do
    begin
      Top := wCurrentTop;
      Left := wCurrentLeft;
      TabStop := True;
      Taborder := i;
      Width := 30;
      Text := '0';
      Tag := i;
      Hint := Format('Enter your score for hole %d', [i + 1]);
      ShowHint := True;
      OnKeyDown := @Process_edt_HandicapScoreArray;
      OnKeyUp := @Process_edt_HandicapScoreArray;
      Parent := grp_HandicapScore;
    end;
    Inc(wCurrentTop, (grp_HandicapScore.Height div 10));
  end;
  wCurrentLeft := (grp_HandicapScore.Width div 2) + StartLeft;
  wCurrentTop := StartTop;
  for i := 9 to 17 do
  begin
    edt_HandicapScoreArray[i] := TEdit.Create(nil);
    with edt_HandicapScoreArray[i] do
    begin
      Top := wCurrentTop;
      Left := wCurrentLeft;
      TabStop := True;
      Taborder := i;
      Width := 30;
      Text := '0';
      Tag := i;
      Hint := Format('Enter your score for hole %d', [i + 1]);
      ShowHint := True;
      OnKeyDown := @Process_edt_HandicapScoreArray;
      OnKeyUp := @Process_edt_HandicapScoreArray;
      Parent := grp_HandicapScore;
    end;
    Inc(wCurrentTop, (grp_HandicapScore.Height div 10));
  end;

  // Stableford Points labels
  Inc(StartLeft, edt_HandicapScoreArray[0].Width + 10);
  SetLength(lbl_StablefordPointsArray, 18);
  wCurrentLeft := StartLeft;
  wCurrentTop := StartTop;
  for i := 0 to 8 do
  begin
    lbl_StablefordPointsArray[i] := TLabel.Create(nil);
    with lbl_StablefordPointsArray[i] do
    begin
      Top := wCurrentTop;
      Left := wCurrentLeft;
      TabStop := True;
      Taborder := i;
      Caption := '0 pts';
      Tag := i;
      Parent := grp_HandicapScore;
    end;
    Inc(wCurrentTop, (grp_HandicapScore.Height div 10));
  end;
  wCurrentLeft := (grp_HandicapScore.Width div 2) + StartLeft;
  wCurrentTop := StartTop;
  for i := 9 to 17 do
  begin
    lbl_StablefordPointsArray[i] := TLabel.Create(nil);
    with lbl_StablefordPointsArray[i] do
    begin
      Top := wCurrentTop;
      Left := wCurrentLeft;
      TabStop := True;
      Taborder := i;
      Caption := '0 pts';
      Tag := i;
      Parent := grp_HandicapScore;
    end;
    Inc(wCurrentTop, (grp_HandicapScore.Height div 10));
  end;

  // CONGU GRID
  StartLeft := 6;
  StartTop := 15;
  // Hole Number
  SetLength(lbl_CONGUHoleNumberArray1, 18);
  wCurrentLeft := StartLeft;
  wCurrentTop := StartTop;
  for i := 0 to 8 do
  begin
    lbl_CONGUHoleNumberArray1[i] := TLabel.Create(nil);
    with lbl_CONGUHoleNumberArray1[i] do
    begin
      AutoSize := True;
      Caption := Format('Hole %d', [i + 1]);
      Top := wCurrentTop;
      Left := wCurrentLeft;
      TabStop := False;
      ParentColor := False;
      Parent := grp_CONGUHandicapScore;
    end;
    Inc(wCurrentTop, (grp_CONGUHandicapScore.Height div 10));
  end;
  wCurrentLeft := grp_CONGUHandicapScore.Width div 2;
  wCurrentTop := StartTop;
  for i := 9 to 17 do
  begin
    lbl_CONGUHoleNumberArray1[i] := TLabel.Create(nil);
    with lbl_CONGUHoleNumberArray1[i] do
    begin
      AutoSize := True;
      Caption := Format('Hole %d', [i + 1]);
      Top := wCurrentTop;
      Left := wCurrentLeft;
      TabStop := False;
      ParentColor := False;
      Parent := grp_CONGUHandicapScore;
    end;
    Inc(wCurrentTop, (grp_CONGUHandicapScore.Height div 10));
  end;

  // Edits
  Inc(StartLeft, lbl_CONGUHoleNumberArray1[0].Width + 50);
  SetLength(edt_CONGUHandicapScoreArray, 18);
  wCurrentLeft := StartLeft;
  wCurrentTop := StartTop;
  for i := 0 to 8 do
  begin
    edt_CONGUHandicapScoreArray[i] := TEdit.Create(nil);
    with edt_CONGUHandicapScoreArray[i] do
    begin
      Top := wCurrentTop;
      Left := wCurrentLeft;
      TabStop := True;
      Taborder := i;
      Width := 30;
      Text := '0';
      Tag := i;
      Hint := Format('Enter your score for hole %d', [i + 1]);
      ShowHint := True;
      OnKeyDown := @Process_edt_CONGUHandicapScoreArray;
      OnKeyUp := @Process_edt_CONGUHandicapScoreArray;
      Parent := grp_CONGUHandicapScore;
    end;
    Inc(wCurrentTop, (grp_CONGUHandicapScore.Height div 10));
  end;
  wCurrentLeft := (grp_CONGUHandicapScore.Width div 2) + StartLeft;
  wCurrentTop := StartTop;
  for i := 9 to 17 do
  begin
    edt_CONGUHandicapScoreArray[i] := TEdit.Create(nil);
    with edt_CONGUHandicapScoreArray[i] do
    begin
      Top := wCurrentTop;
      Left := wCurrentLeft;
      TabStop := True;
      Taborder := i;
      Width := 30;
      Text := '0';
      Tag := i;
      Hint := Format('Enter your score for hole %d', [i + 1]);
      ShowHint := True;
      OnKeyDown := @Process_edt_CONGUHandicapScoreArray;
      OnKeyUp := @Process_edt_CONGUHandicapScoreArray;
      Parent := grp_CONGUHandicapScore;
    end;
    Inc(wCurrentTop, (grp_CONGUHandicapScore.Height div 10));
  end;

  // Stableford Points labels
  Inc(StartLeft, edt_CONGUHandicapScoreArray[0].Width + 10);
  SetLength(lbl_CONGUStablefordPointsArray, 18);
  wCurrentLeft := StartLeft;
  wCurrentTop := StartTop;
  for i := 0 to 8 do
  begin
    lbl_CONGUStablefordPointsArray[i] := TLabel.Create(nil);
    with lbl_CONGUStablefordPointsArray[i] do
    begin
      Top := wCurrentTop;
      Left := wCurrentLeft;
      TabStop := True;
      Taborder := i;
      Caption := Format('Adjusted Score: %d', [0]);
      Tag := i;
      Parent := grp_CONGUHandicapScore;
    end;
    Inc(wCurrentTop, (grp_CONGUHandicapScore.Height div 10));
  end;
  wCurrentLeft := (grp_CONGUHandicapScore.Width div 2) + StartLeft;
  wCurrentTop := StartTop;
  for i := 9 to 17 do
  begin
    lbl_CONGUStablefordPointsArray[i] := TLabel.Create(nil);
    with lbl_CONGUStablefordPointsArray[i] do
    begin
      Top := wCurrentTop;
      Left := wCurrentLeft;
      TabStop := True;
      Taborder := i;
      Caption := Format('Adjusted Score: %d', [0]);
      Tag := i;
      Parent := grp_CONGUHandicapScore;
    end;
    Inc(wCurrentTop, (grp_CONGUHandicapScore.Height div 10));
  end;

end;

procedure Tmainform.MakeHoleLabels;
// Called by Form Create
// Creates labels for Hole info using arrays so that each label's properties
// can be referenced easily by usung its index
var
  i, wCurrentLeft, wCurrentTop: word;
  StartTop, StartLeft: word;
begin
  StartLeft := 6;
  StartTop := 8;
  // Hole Number
  SetLength(lbl_HoleNumberArray, 18);
  wCurrentLeft := StartLeft;
  wCurrentTop := StartTop;
  for i := 0 to 8 do
  begin
    lbl_HoleNumberArray[i] := TLabel.Create(nil);
    with lbl_HoleNumberArray[i] do
    begin
      AutoSize := True;
      Caption := Format('Hole %d', [i + 1]);
      Top := wCurrentTop;
      Left := wCurrentLeft;
      TabStop := False;
      ParentColor := False;
      Parent := grp_CourseHoles;
    end;
    Inc(wCurrentTop, (grp_CourseHoles.Height div 10));
  end;
  wCurrentLeft := grp_CourseHoles.Width div 2;
  wCurrentTop := StartTop;
  for i := 9 to 17 do
  begin
    lbl_HoleNumberArray[i] := TLabel.Create(nil);
    with lbl_HoleNumberArray[i] do
    begin
      AutoSize := True;
      Caption := Format('Hole %d', [i + 1]);
      Top := wCurrentTop;
      Left := wCurrentLeft;
      TabStop := False;
      ParentColor := False;
      Parent := grp_CourseHoles;
    end;
    Inc(wCurrentTop, (grp_CourseHoles.Height div 10));
  end;
  // Par
  Inc(StartLeft, lbl_HoleNumberArray[0].Width);
  SetLength(lbl_HoleParArray, 18);
  wCurrentLeft := StartLeft;
  wCurrentTop := StartTop;
  for i := 0 to 8 do
  begin
    lbl_HoleParArray[i] := TLabel.Create(nil);
    with lbl_HoleParArray[i] do
    begin
      Caption := Format('Par %d', [i + 1]);
      Top := wCurrentTop;
      Left := wCurrentLeft;
      TabStop := False;
      Parent := grp_CourseHoles;
    end;
    Inc(wCurrentTop, (grp_CourseHoles.Height div 10));
  end;
  wCurrentLeft := (grp_CourseHoles.Width div 2) + StartLeft;
  wCurrentTop := StartTop;
  for i := 9 to 17 do
  begin
    lbl_HoleParArray[i] := TLabel.Create(nil);
    with lbl_HoleParArray[i] do
    begin
      Caption := Format('Par %d', [i + 1]);
      Top := wCurrentTop;
      Left := wCurrentLeft;
      TabStop := False;
      Parent := grp_CourseHoles;
    end;
    Inc(wCurrentTop, (grp_CourseHoles.Height div 10));
  end;
  // SI
  Inc(StartLeft, lbl_HoleParArray[0].Width);
  SetLength(lbl_HoleStrokeIndexArray, 18);
  wCurrentLeft := StartLeft;
  wCurrentTop := StartTop;
  for i := 0 to 8 do
  begin
    lbl_HoleStrokeIndexArray[i] := TLabel.Create(nil);
    with lbl_HoleStrokeIndexArray[i] do
    begin
      Caption := Format('S.I. %d', [i + 1]);
      Top := wCurrentTop;
      Left := wCurrentLeft;
      TabStop := False;
      Parent := grp_CourseHoles;
    end;
    Inc(wCurrentTop, (grp_CourseHoles.Height div 10));
  end;
  wCurrentLeft := (grp_CourseHoles.Width div 2) + StartLeft;
  wCurrentTop := StartTop;
  for i := 9 to 17 do
  begin
    lbl_HoleStrokeIndexArray[i] := TLabel.Create(nil);
    with lbl_HoleStrokeIndexArray[i] do
    begin
      Caption := Format('S.I. %d', [i + 1]);
      Top := wCurrentTop;
      Left := wCurrentLeft;
      TabStop := False;
      Parent := grp_CourseHoles;
    end;
    Inc(wCurrentTop, (grp_CourseHoles.Height div 10));
  end;
  // Distance
  Inc(StartLeft, lbl_HoleStrokeIndexArray[0].Width);
  SetLength(lbl_HoleDistanceArray, 18);
  wCurrentLeft := StartLeft;
  wCurrentTop := StartTop;
  for i := 0 to 8 do
  begin
    lbl_HoleDistanceArray[i] := TLabel.Create(nil);
    with lbl_HoleDistanceArray[i] do
    begin
      Caption := Format('Dist %dm', [i + 1]);
      Top := wCurrentTop;
      Left := wCurrentLeft;
      TabStop := False;
      Parent := grp_CourseHoles;
    end;
    Inc(wCurrentTop, (grp_CourseHoles.Height div 10));
  end;
  wCurrentLeft := (grp_CourseHoles.Width div 2) + StartLeft;
  wCurrentTop := StartTop;
  for i := 9 to 17 do
  begin
    lbl_HoleDistanceArray[i] := TLabel.Create(nil);
    with lbl_HoleDistanceArray[i] do
    begin
      Caption := Format('Dist %dm', [i + 1]);
      Top := wCurrentTop;
      Left := wCurrentLeft;
      TabStop := False;
      Parent := grp_CourseHoles;
    end;
    Inc(wCurrentTop, (grp_CourseHoles.Height div 10));
  end;
end;

procedure Tmainform.LoadRecentCourseFromMenu(const iCourseIndexIntoStringList: integer);
// Called by RecentCoursesMenuClick
// Reads config file to find path of golfml file, then loads it
// procedure ResetRecentCoursesMenu populates CoursesStringList
// Checks for invalid entries and deletes them
var
  s: string;
begin
  // Read path to course from INI file
  s := INI.ReadString('Recent Courses',
    CoursesStringList[iCourseIndexIntoStringList], '');
  if FileExists(s) then
  begin
    GolfmlClass.CourseXMLPath := s;
    if GolfMlClass.GetCourseInfoFromFile then
      InitNewCourse(0);
  end
  else
  begin
    INI.DeleteKey('Recent Courses', CoursesStringList[iCourseIndexIntoStringList]);
    ResetRecentCoursesMenu;
  end;
end;

procedure Tmainform.SetTeeFromMenu(const iTeeColour: cardinal);
// Called by TeeMenuClick
begin
  GolfMlClass.TeeColourIndex := iTeeColour;
  mnu_tee[iTeeColour].Checked := True;
  UpdatePagesFromCourse;
end;

procedure Tmainform.SetCourseFromMenu(const iCourse: cardinal);
// Called by CourseMenuClick
begin
  GolfMlClass.CourseIndex := iCourse;
  ResetTeeMenu;
  SetTeeFromMenu(0);
end;

procedure Tmainform.RecentCoursesMenuClick(Sender: TObject);
// Common handler for all RecentCourses submenu entries
// Grab the unique Tag property and pass it as a parameter to another routine
var
  tempMenuItem: TMenuItem;
begin
  tempMenuItem := Sender as TMenuItem;
  LoadRecentCourseFromMenu(tempMenuItem.Tag);
end;

procedure Tmainform.ud_CONGUGrossChanging(Sender: TObject; var AllowChange: boolean);
begin
  // CONGUUpdateNett;
end;

procedure Tmainform.CONGUUpdateNett;
var
  wPlayingHandicap: integer;
  singPlayingHandicap: single;
begin
  singPlayingHandicap := ud_CONGUplayerhandicap0.Position +
    (ud_CONGUplayerhandicap1.Position / 10);
  wPlayingHandicap := TRUNC(singPlayingHandicap + 0.5);
  // lbl_Test.Caption:=Format('ud_CONGUGross.Position=%d',[ud_CONGUGross.Position]);
  iCONGUNett := ud_CONGUGross.Position - wPlayingHandicap;
  lbl_CONGUNett.Caption := Format('Net Score : %d', [iCONGUNett]);
  HCPClass.NetScore := iCONGUNett;
end;

procedure Tmainform.ud_CONGUGrossClick(Sender: TObject; Button: TUDBtnType);
begin
  CONGUUpdateNett;
end;

procedure Tmainform.ud_CONGUplayerhandicap0Click(Sender: TObject; Button: TUDBtnType);
begin
  CONGUZeroScores;
  if ud_CONGUplayerhandicap0.Position = 28 then
    ud_CONGUplayerhandicap1.Position := 0;
end;

procedure Tmainform.ud_CONGUplayerhandicap1Click(Sender: TObject; Button: TUDBtnType);
begin
  CONGUZeroScores;
  if ud_CONGUplayerhandicap0.Position = 28 then
    ud_CONGUplayerhandicap1.Position := 0;
end;

procedure Tmainform.EGAUpdatePlayingHandicap;
// Called whenever handicap or course changed
var
  s: string;
begin
  s := 'On this course on the %s tees, your EGA playing handicap is %f';
  HcpClass.EGAHandicap := ud_playerhandicap0.Position +
    (ud_playerhandicap1.Position / 10);
  lbl_playinghandicap.Caption :=
    Format(s, [GolfMLClass.TeeColour, Int(HcpClass.GetPlayingHandicap + 0.5)]);
  lbl_playinghandicap2.Caption := Format('%f', [Int(HcpClass.GetPlayingHandicap + 0.5)]);
  HCPClass.OldHandicap := HCPClass.EGAHandicap;
  HCPClass.StablefordPoints := ud_stablefordpoints.Position;
  HCPClass.AdjustHandicap;
  lbl_adjustedHandicap1.Caption := Format('%f', [HCPClass.NewHandicap]);
end;

procedure Tmainform.ud_playerhandicap0Click(Sender: TObject; Button: TUDBtnType);
begin
  EGAZeroScores;
  EGAUpdatePlayingHandicap;
end;

procedure Tmainform.ud_playerhandicap1Click(Sender: TObject; Button: TUDBtnType);
begin
  EGAZeroScores;
  EGAUpdatePlayingHandicap;
end;

procedure Tmainform.ud_stablefordpointsClick(Sender: TObject; Button: TUDBtnType);
begin
  EGAUpdatePlayingHandicap;
end;

procedure Tmainform.TeeMenuClick(Sender: TObject);
// Common handler for all Tee menu entries
// Grab the unique Tag property and pass it as a parameter to another routine
var
  tempMenuItem: TMenuItem;
begin
  tempMenuItem := Sender as TMenuItem;
  SetTeeFromMenu(tempMenuItem.Tag);
end;

procedure Tmainform.CourseMenuClick(Sender: TObject);
// Common handler for all Course menu entries
// Grab the unique Tag property and pass it as a parameter to another routine
var
  tempMenuItem: TMenuItem;
begin
  tempMenuItem := Sender as TMenuItem;
  SetCourseFromMenu(tempMenuItem.Tag);
end;

procedure Tmainform.ResetCourseMenu;
// Deletes, then makes up the Course menu afresh from the loaded golfml file
var
  tempmenu: TMenuItem;
  f: integer;
  s: string;
begin
  if not GolfmlClass.CourseLoaded then
    Exit;
  mnu_Course.Clear;
  for f := 0 to GolfmlClass.MaxCourseIndex do
  begin
    // Make a temporary submenu item up
    tempMenu := TMenuItem.Create(mnu_Course);
    GolfMlClass.CourseIndex := f;
    with tempMenu do
    begin
      Caption := GolfmlClass.CourseName;
      Tag := f; // This is used to identify which menu is clicked
      AutoCheck := True;
      RadioItem := True;
      OnClick := @CourseMenuClick; // common handler for all Courses
    end;
    mnu_Course.Enabled := True;
    // Add the submenu item to the Course menu
    mnu_Course.Add(tempMenu);
  end;
end;

procedure Tmainform.ResetTeeMenu;
// Deletes, then makes up the Tees menu afresh from the loaded golfml file
var
  tempmenu: TMenuItem;
  f: integer;
  s: string;
begin
  if not GolfmlClass.CourseLoaded then
    Exit;
  mnu_Tee.Clear;
  for f := 0 to GolfmlClass.MaxTeeColourIndex do
  begin
    // Make a temporary submenu item up
    tempMenu := TMenuItem.Create(mnu_Tee);
    GolfMlClass.TeeColourIndex := f;
    with tempMenu do
    begin
      Caption := GolfmlClass.TeeTitle;
      Tag := f; // This is used to identify which menu is clicked
      AutoCheck := True;
      RadioItem := True;
      OnClick := @TeeMenuClick; // common handler for all Tees
    end;
    mnu_Tee.Enabled := True;
    // Add the submenu item to the Tee menu
    mnu_Tee.Add(tempMenu);
  end;
end;

procedure Tmainform.ResetRecentCoursesMenu;
// Deletes, then makes up the Recent Courses submenu afresh from the Config file
var
  tempmenu: TMenuItem;
  f: integer;
  s: string;
begin
  mnu_fileRecentCourses.Enabled := True;
  CoursesStringList.Clear;
  INI.ReadSection('Recent Courses', CoursesStringList);
  if CoursesStringList.Count > 0 then
  begin
    mnu_fileRecentCourses.Clear;
    for f := 0 to CoursesStringList.Count - 1 do
    begin
      // Check file still exists. and only add if it does
      s := INI.ReadString('Recent Courses', CoursesStringList[f], '');
      if FileExists(s) then
      begin
        // Make a temporary submenu item up
        tempMenu := TMenuItem.Create(mnu_fileRecentCourses);
        with tempMenu do
        begin
          Caption := CoursesStringList[f];
          Tag := f; // This is used to identify which menu is clicked
          AutoCheck := False;
          RadioItem := False;
          OnClick := @RecentCoursesMenuClick; // common handler for all Tees
        end;
        // Add the submenu item to the Tee menu
        mnu_fileRecentCourses.Add(tempMenu);
      end
      else // File has been deleted/moved. Delete the conf file entry
        INI.DeleteKey('Recent Courses', CoursesStringList[f]);
    end;
  end
  else
    mnu_fileRecentCourses.Enabled := False;
end;

procedure Tmainform.UpdatePagesFromCourse;
// GolfmlClass already populated.
// Called from Course menu or Tee menu click events
// Important routine that updates the Classes and Controls on all pages
var
  s, dist: string;
  iAmenityCount, iHoleCount: word;
  BackColor, ForeColor: DWord;
begin
  GolfmlClass.Metric := mnu_optionsMetric.Checked;
  // COURSES PAGE
  // Title
  s := Format('%s - %s : %s tee (%s, %s)', [GolfMlClass.ClubName,
    GolfMlClass.CourseName, GolfMlClass.TeeTitle, GolfMlClass.TeeColour,
    GolfMlClass.TeeGender]);
  ssbar.Panels[1].Text := GolfMlClass.CourseName + ' ' + GolfMlClass.TeeColour + ' tees';
  lbl_pageCourseClubName.Caption := s;
  lbl_pageHandicapClubName.Caption := s;
  lbl_pageGameClubName.Caption := s;
  grp_HandicapScore.Caption := 'Input scores for ' + s;
  grp_CONGUHandicapScore.Caption := 'Input scores for ' + s;

  // Location
  lbl_ClubCountry.Caption := 'Country: ' + GolfMlClass.ClubCountry;
  lbl_ClubMunicipality.Caption := 'Munipality: ' + GolfMlClass.ClubMunicipality;
  lbl_ClubRegion.Caption := 'Region: ' + GolfMlClass.ClubRegion;
  lbl_ClubStreet.Caption := 'Street: ' + GolfMlClass.ClubStreet;
  lbl_ClubPostCode.Caption := 'Postcode: ' + GolfMlClass.ClubPostCode;
  lbl_ClubWebsite.Caption := 'Website ' + GolfMlClass.ClubWebsite;
  if Length(GolfmlClass.ClubWebsite) > 0 then
  begin
    lbl_ClubWebsite.Font.Color := clRed;
    lbl_ClubWebsite.Font.Style := [fsUnderline];
    lbl_ClubWebsite.Cursor := crHandPoint;
  end;
  lbl_ClubPhone.Caption := 'Tel: ' + GolfMlClass.ClubPhone;
  // Amenities
  Memo_amenities.Clear;
  for iAmenityCount := 0 to GolfMlClass.ClubMaxAmenitiesIndex do
  begin
    s := Format('%s', [GolfMlClass.ClubAmeneties[iAmenityCount]]);
    Memo_amenities.Lines.Add(s);
  end;
  // Course and Tee Info
  grp_CourseAndTeeInfo.Visible := False;
  grp_CourseAndTeeInfo.Caption :=
    Format('General Course and Tee information (%s, %s tee)',
    [GolfMlClass.CourseName, GolfMlClass.TeeTitle]);
  if GolfMlClass.Metric = True then
    dist := 'm'
  else
    dist := 'yd';
  s := Format('Length: Total=%d%s, In=%d%s, Out=%d%s',
    [GolfMlClass.TotalTeeDistance, dist, GolfMlClass.InTeeDistance,
    dist, GolfMlClass.OutTeeDistance, dist]);
  lbl_CourseLength.Caption := s;
  lbl_CourseNumberOfHoles.Caption :=
    Format('Holes: %d', [GolfMlClass.CourseNumberOfHoles]);
  lbl_CoursePar.Caption := Format('Par: %d', [GolfMlClass.CourseTotalPar]);
  lbl_CourseCourseIndex.Caption :=
    Format('Course Rating (EGA) /SSS (CONGU): %f', [GolfMlClass.CourseRating]);
  lbl_CourseSlopeIndex.Caption :=
    Format('Slope Rating (EGA): %d', [GolfMlClass.SlopeRating]);
  grp_CourseAndTeeInfo.Visible := True;

  // Holes Panels
  grp_CourseHoles.Visible := False;
  grp_HandicapScore.Visible := False;
  grp_CONGUHandicapScore.Visible := False;
  if GolfMlClass.TeeColour = 'gold' then
  begin
    BackColor := clOlive;
    ForeColor := clBlack;
  end;
  if GolfMlClass.TeeColour = 'black' then
  begin
    BackColor := clBlack;
    ForeColor := clWhite;
  end;
  if GolfMlClass.TeeColour = 'white' then
  begin
    BackColor := clSilver;
    ForeColor := clBlack;
  end;
  if GolfMlClass.TeeColour = 'yellow' then
  begin
    BackColor := clYellow;
    ForeColor := clBlack;
  end;
  if GolfMlClass.TeeColour = 'blue' then
  begin
    BackColor := clNavy;
    ForeColor := clWhite;
  end;
  if GolfMlClass.TeeColour = 'red' then
  begin
    BackColor := clMaroon;
    ForeColor := clWhite;
  end;

  grp_CourseHoles.Caption := Format('Course details (%s tees)', [GolfMlClass.TeeColour]);
  // Make all invisible to start with
  for iHoleCount := 0 to 17 do
  begin
    // COURSE
    lbl_HoleNumberArray[iHoleCount].Visible := False; //Course
    lbl_HoleDistanceArray[iHoleCount].Visible := False; // Course
    lbl_HoleParArray[iHoleCount].Visible := False;
    lbl_HoleStrokeIndexArray[iHoleCount].Visible := False;

    // Handicap
    lbl_HoleNumberArray1[iHoleCount].Visible := False;
    lbl_CONGUHoleNumberArray1[iHoleCount].Visible := False;
    lbl_StablefordPointsArray[iHoleCount].Visible := False;
    lbl_CONGUStablefordPointsArray[iHoleCount].Visible := False;
    edt_HandicapScoreArray[iHoleCount].Visible := False;
    edt_HandicapScoreArray[iHoleCount].Text := '0';
    edt_CONGUHandicapScoreArray[iHoleCount].Visible := False;
    edt_CONGUHandicapScoreArray[iHoleCount].Text := '0';
  end;
  if GolfMlClass.CourseNumberOfHoles < 10 then
  begin
    lbl_StablefordScoreLast9.Visible := False;
    lbl_CONGUGrossScoreLast9.Visible := False;
  end
  else
  begin
    lbl_StablefordScoreLast9.Visible := True;
    lbl_CONGUGrossScoreLast9.Visible := True;
  end;

  // Hole Info
  // Fill in info and make visible 9 or 18 holes
  for iHoleCount := 1 to GolfMlClass.CourseNumberOfHoles do
  begin
    GolfmlClass.HoleIndex := iHoleCount - 1;

    // Courses Page
    lbl_HoleNumberArray[iHoleCount - 1].Caption := Format('Hole: %d', [iHoleCount]);
    lbl_HoleNumberArray[iHoleCount - 1].Color := BackColor;
    lbl_HoleNumberArray[iHoleCount - 1].Font.Color := ForeColor;
    lbl_HoleNumberArray[iHoleCount - 1].Visible := True;

    lbl_HoleParArray[iHoleCount - 1].Caption := Format('Par %d', [GolfmlClass.Par]);
    lbl_HoleParArray[iHoleCount - 1].Visible := True;

    lbl_HoleStrokeIndexArray[iHoleCount - 1].Caption :=
      Format('S.I: %d', [GolfmlClass.StrokeIndex]);
    lbl_HoleStrokeIndexArray[iHoleCount - 1].Visible := True;
    if GolfmlClass.Metric then
      lbl_HoleDistanceArray[iHoleCount - 1].Caption :=
        Format('%d metres', [GolfmlClass.TeeDistanceMetres])
    else
      lbl_HoleDistanceArray[iHoleCount - 1].Caption :=
        Format('%d yards', [GolfmlClass.TeeDistanceYards]);

    lbl_HoleDistanceArray[iHoleCount - 1].Visible := True;

    // EGA Handicap Page
    lbl_HoleNumberArray1[iHoleCount - 1].Caption :=
      Format('Hole: %d Par %d (%d)', [iHoleCount, GolfmlClass.Par,
      GolfmlClass.StrokeIndex]);
    lbl_HoleNumberArray1[iHoleCount - 1].Color := BackColor;
    lbl_HoleNumberArray1[iHoleCount - 1].Font.Color := ForeColor;
    // Make Visible
    lbl_HoleNumberArray1[iHoleCount - 1].Visible := True;
    edt_HandicapScoreArray[iHoleCount - 1].Visible := True;
    lbl_StablefordPointsArray[iHoleCount - 1].Visible := True;

    // CONGU Handicap Page
    lbl_CONGUHoleNumberArray1[iHoleCount - 1].Caption :=
      Format('Hole: %d Par %d (%d)', [iHoleCount, GolfmlClass.Par,
      GolfmlClass.StrokeIndex]);
    lbl_CONGUHoleNumberArray1[iHoleCount - 1].Color := BackColor;
    lbl_CONGUHoleNumberArray1[iHoleCount - 1].Font.Color := ForeColor;
    // Make Visible
    lbl_CONGUHoleNumberArray1[iHoleCount - 1].Visible := True;
    edt_CONGUHandicapScoreArray[iHoleCount - 1].Visible := True;
    lbl_CONGUStablefordPointsArray[iHoleCount - 1].Visible := True;

  end;

  // Force the controls in the groupboxes to repaint
  grp_CourseHoles.Visible := True;
  grp_HandicapScore.Visible := True;
  grp_CONGUHandicapScore.Visible := True;

  // HANDICAP PAGES
  // Pass current course details from golfml class to handicap class
  HcpClass.CourseRating := GolfmlClass.CourseRating;
  HcpClass.SlopeRating := GolfmlClass.SlopeRating;
  EGAZeroScores; // calls EGAUpdatePlayingHandicap
  CONGUZeroScores;
end;

procedure Tmainform.InitNewCourse(const iCourse: cardinal);
// New Course has beeen loaded into GolfmlClass
// Called whenever a new course is imported
var
  i: cardinal;
begin
  // Pass course and tee index to golfml class
  GolfmlClass.CourseIndex := iCourse;
  GolfmlClass.TeeColourIndex := 0;

  // Set up HandicapClass properties for the new course
  HCPClass.Reset;
  {$IFDEF Debug}
  HCPClass.DebugMode := True;
  {$ENDIF}
  for i := 0 to GolfmlClass.CourseNumberOfHoles - 1 do
  begin
    GolfmlClass.HoleIndex := i;
    HcpClass.ParArrayElement[i] := GolfmlClass.Par;
  end;
  HcpClass.CourseRating := GolfmlClass.CourseRating;
  HcpClass.SlopeRating := GolfmlClass.SlopeRating;
  HcpClass.CoursePar := GolfmlClass.CourseTotalPar;

  // Set up Menus
  ResetCourseMenu; // Build up Courses menu and Tees menu
  SetCourseFromMenu(0); // Sets first Course
  SetTeeFromMenu(0); // Sets first Tee position
  mnu_Course[0].Checked := True;
  mnu_Tee[0].Checked := True;

  // Store this course in the config file and update the File menu
  INI.WriteString('Recent Courses', GolfmlClass.ClubName, GolfmlClass.CourseXMLPath);
  ResetRecentCoursesMenu;

  // Activate all tabs (App starts with only Pages[0] visible)
  for i := 0 to page_container.PageCount - 1 do
    page_container.Pages[i].TabVisible := True;
  page_container.ActivePage := page_Course;

  // Fill in this golfml data to all the pages
  UpdatePagesFromCourse;
end;

(******************************************************************************)
// Close app routines
procedure Tmainform.CloseApp;
// Tidy up any stuff here
begin
  // Save player info to config file
  INI.WriteString('Player', 'Name', edt_playername.Text);
  INI.WriteInteger('Player', 'EGAHandicap0', ud_playerhandicap0.Position);
  INI.WriteInteger('Player', 'EGAHandicap1', ud_playerhandicap1.Position);
  INI.WriteInteger('Player', 'CONGUHandicap0', ud_CONGUplayerhandicap0.Position);
  INI.WriteInteger('Player', 'CONGUHandicap1', ud_CONGUplayerhandicap1.Position);
  // Make certain of no memory leaks
  HCPClass.Free;
  GolfmlClass.Free;
  INI.Free;
  CoursesStringList.Free;
end;

procedure Tmainform.cmd_closeClick(Sender: TObject);
// Calls FormClose then CloseApp then Closes.
begin
  Close;
end;



procedure Tmainform.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseApp;
end;

procedure Tmainform.mnu_fileExitClick(Sender: TObject);
begin
  cmd_close.Click; // menu clicks close button
end;

(******************************************************************************)

procedure Tmainform.FormCreate(Sender: TObject);
var
  ConfigFilePath: string;
begin
  // Set up mainform
  Caption := Application.Title;
  Icon := Application.Icon;
  mnu_helpAbout.Caption := 'About ' + Application.Title + '...';
  ssbar.Panels[0].Text := Application.Title;
  Application.OnHint := @ShowHint;

  // Set up the Classes
  GolfmlClass := TGolfmlClass.Create;
  if not GolfmlClass.InitOk then
  begin
    MessageDlg('Sorry - cannot initialise Golfml Class.  Closing.',
      mtError, [mbOK], 0);
    Application.Terminate;
  end;
  HCPClass := THCPClass.Create;
  if not HCPClass.InitOk then
  begin
    MessageDlg('Sorry - cannot initialise HCPClass.  Closing.',
      mtError, [mbOK], 0);
    Application.Terminate;
  end;

  if Application.Params[1] = 'log' then
    HCPClass.Logging := True;
  if Application.Params[1] = 'debug' then
    HCPClass.Logging := True;


  // Set up the config file
  // Windows: In Application's own folder
  // Unix: In user's home directory
  {$ifdef windows}
  ConfigFilePath := ExtractFilePath(Application.EXEName) + 'golfhelper' +
    GetOS + '.ini';
  {$endif}
  {$ifdef Unix}
  ConfigFilePath := GetAppConfigFile(False) + '.conf';
  {$endif}
  INI := TIniFile.Create(ConfigFilePath);
  INI.WriteString('ProgramInfo', 'Application', Application.Title);
  INI.WriteString('ProgramInfo', 'GUI Version', GetFileVersion);
  INI.WriteString('ProgramInfo', 'HCPClass Version', HCPClass.Version);
  INI.WriteString('ProgramInfo', 'GolfmlClass Version', GolfmlClass.Version);
  INI.WriteString('ProgramInfo', 'Last Compiled', GetCompiledDate);
  INI.WriteString('ProgramInfo', 'Author', 'Gordon Bamber');
  INI.WriteString('ProgramInfo', 'Support', 'minesadorada@charcodelvalle.com');
  INI.WriteString('ProgramInfo', 'Licence', 'Creative Commons');

  if INI.ReadBool('Settings', 'Metric', True) then
    mnu_optionsMetric.Checked := True
  else
    mnu_optionsImperial.Checked := True;

  // Set up File menu 'Import Recent Course'
  CoursesStringList := TStringList.Create;
  ResetRecentCoursesMenu;

  // Start with only the Information Page active and visible
  page_course.Visible := False;
  page_Handicap.Visible := False;
  page_Congu.Visible := False;
  page_Information.Visible := True;
  page_container.ActivePage := page_Information;

  // Create and position the Hole info labels on Course and Handicap pages
  MakeHoleLabels;
  MakeHandicapInputGrid;

  // Set up virgin Handicap pages or populated with saved info
  edt_playername.Text := INI.ReadString('Player', 'Name', 'Type your name here');
  ud_playerhandicap0.Position := INI.ReadInteger('Player', 'EGAHandicap0', 28);
  ud_playerhandicap1.Position := INI.ReadInteger('Player', 'EGAHandicap1', 0);
  ud_CONGUplayerhandicap0.Position := INI.ReadInteger('Player', 'CONGUHandicap0', 28);
  ud_CONGUplayerhandicap1.Position := INI.ReadInteger('Player', 'CONGUHandicap1', 0);

  // Display instructions on opening page
  PopulateGeneralInfoPage;

end;

procedure Tmainform.FormDropFiles(Sender: TObject; const FileNames: array of string);
var
  s: string;
begin
  if ExtractFileExt(Filenames[0]) = '.xml' then
  begin
    s := GetTempDir(True) + 'golfml.xml';
    CopyFile(Filenames[0], s);
    GolfmlClass.CourseXMLPath := s;
    if not GolfmlClass.IsValidGolfmlFile then
    begin
      MessageDlg(GolfmlClass.CourseXMLPath + ' is not a valid golml file',
        mtError, [mbOK], 0);
      exit;
    end;
    if GolfMlClass.GetCourseInfoFromFile then
    begin
      GolfmlClass.CourseXMLPath := Filenames[0];
      DeleteFile(s);
      InitNewCourse(0);
    end
    else
      MessageDlg(GolfMlClass.ErrorString, mtError, [mbOK], 0);
  end
  else
    MessageDlg('Import cancelled', mtInformation, [mbOK], 0);

end;

procedure Tmainform.Image2Click(Sender: TObject);
begin
  OpenURL('http://code.google.com/p/golfml/');
end;

procedure Tmainform.lbl_ClubWebsiteClick(Sender: TObject);
// On the Courses page, make the web label hot if there is a URL available
begin
  if Length(GolfmlClass.ClubWebsite) > 0 then
    OpenURL(GolfmlClass.ClubWebsite);
end;

(******************************************************************************)
// Empty menu click routines
procedure Tmainform.mnu_courseClick(Sender: TObject);
begin
  if not GolfmlClass.CourseLoaded then
    MessageDlg('No Course Loaded. Please import a golf course from the File menu',
      mtError, [mbOK], 0);
end;

procedure Tmainform.mnu_teeClick(Sender: TObject);
begin
  if not GolfmlClass.CourseLoaded then
    MessageDlg('No Course Loaded. Please import a golf course from the File menu',
      mtError, [mbOK], 0);
end;

(******************************************************************************)

procedure Tmainform.page_containerChange(Sender: TObject);
// If no user info loaded then highlight the input field
begin
  if (page_container.ActivePage = page_handicap) and
    (edt_playername.Text = 'Type your name here') then
    edt_playername.SetFocus
  else
    edt_CONGUplayername.Text := edt_playername.Text;

  if (page_container.ActivePage = page_congu) and
    (edt_CONGUplayername.Text = 'Type your name here') then
    edt_CONGUplayername.SetFocus
  else
    edt_playername.Text := edt_CONGUplayername.Text;

  // Set appropriate Handicap Mode here?
  if (page_container.ActivePage = page_handicap) then
    HCPClass.SetEGAMode;
  if (page_container.ActivePage = page_congu) then
    HCPClass.SetCONGUMode;
  ssbar.Panels[0].Text := Application.Title + ' (' + HCPClass.GetModeAsString + ' mode)';
end;


procedure Tmainform.mnu_fileImportGolfmlClick(Sender: TObject);
// Import a golfml file from disk.
// Copy to temp dir and read from there
// this stops strange filenames from crashing GolfmlClass
// Write successful import path to the Config file
// Note: strange filenames won't write to the Config file
var
  s: string;
begin
  dlg_Import.InitialDir := ExtractFilePath(Application.EXEName);
  if dlg_Import.Execute then
  begin
    s := GetTempDir(True) + 'golfml.xml'; // Fetch temp file path
    CopyFile(dlg_Import.Filename, s);
    GolfmlClass.CourseXMLPath := s; // Load from the copy
    if not GolfmlClass.IsValidGolfmlFile then // Test for first element
    begin
      MessageDlg(GolfmlClass.CourseXMLPath + ' is not a valid golml file',
        mtError, [mbOK], 0);
      exit;
    end;
    if GolfMlClass.GetCourseInfoFromFile then
    begin
      // All data has been read from the temp copy
      // Write ORIGINAL path to config file
      GolfmlClass.CourseXMLPath := dlg_Import.Filename;
      DeleteFile(s); // Delete the temp copy
      InitNewCourse(0);
    end
    else
      MessageDlg(GolfMlClass.ErrorString, mtError, [mbOK], 0);
  end
  else
    MessageDlg('Import cancelled', mtInformation, [mbOK], 0);
end;

procedure Tmainform.mnu_helpAboutClick(Sender: TObject);
var
  s: string;
begin
  s := Application.Title + ' V' + GetFileVersion + CR + CR;
  s += 'Compilation date: ' + GetCompiledDate + CR;
  s += 'Made with ' + GetCompilerInfo + ' and ' + GetLCLVersion + CR;
  s += 'using ' + GetWidgetSet + CR;
  s += 'Target CPU: ' + GetTargetInfo + CR + CR;
  s += 'Golfml schema by Pierre Mareschal (http://code.google.com/p/golfml/)' + CR + CR;
  s += 'GolfmlClass V' + GolfmlClass.Version + ' by Gordon Bamber' + CR;
  s += 'HCPClass V' + HCPClass.Version + ' by Gordon Bamber' + CR;
  s += 'GUI V' + GetFileVersion + ' by Gordon Bamber' + CR;
  s += 'Config file: ' + INI.Filename + CR + CR;
  s += 'Published under GNU General Public License' + CR;
  s += 'Discretionary support: minesadorada@charcodelvalle.com';
  MessageDlg('About ' + Application.Title, s, mtInformation, [mbOK], 0);
end;

procedure Tmainform.mnu_helpAboutgolfmlClick(Sender: TObject);
Var s:String;
begin
  s:='GolfML is a text-based human-readable file format used to exchange golf data between applications and web sites.' + CR + CR;
  s+='Golf course information such as scorecard and yardage books, or players scores and statistics are the most common information carried by GolfML' + CR + CR;
  s+='Click the golml graphic to learn more online.';
  MessageDlg('About Golfml',s,mtInformation,[MBOK],0);
end;

procedure Tmainform.mnu_helpAboutHCPClassClick(Sender: TObject);
begin
  HCPClass.ShowHelp;
end;

procedure Tmainform.mnu_optionsImperialClick(Sender: TObject);
// Set all tee distances to Yards
begin
  GolfMlClass.Metric := False;
  UpdatePagesFromCourse;
  INI.WriteBool('Settings', 'Metric', False);
end;

procedure Tmainform.mnu_optionsMetricClick(Sender: TObject);
// Set all tee distances to Metres (default)
begin
  GolfMlClass.Metric := True;
  UpdatePagesFromCourse;
  INI.WriteBool('Settings', 'Metric', True);
end;

procedure Tmainform.SplitAmenety(var fAmenityType, fAmenetyValue: string;
  const aString: string);
// Changes fAmenityType, fAmenetyValue
// Uses C_AMENETYDELIMITERCHAR
// Note: This proc is unused
var
  i: integer;
begin
  if Length(aString) = 0 then
    exit;
  i := Pos(C_AMENETYDELIMITERCHAR, aString);
  if i > 1 then
  begin
    fAmenityType := LeftStr(aString, i - 1);
    fAmenetyValue := RightStr(aString, Length(aString) - i);
  end;
end;


end.
