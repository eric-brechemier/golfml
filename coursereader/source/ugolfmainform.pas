unit ugolfmainform;

(*
==
== A simple GUI for THCPGolfUMLClass (thandicapgolfumlclass)
==
*)
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fileutil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, ExtCtrls, Menus, thandicapgolfumlclass; // ugolfmlclass

const
  C_APPVERSION = '1.1.20120328';

const
  C_DEFAULTXMLFILE = 'coursedata.xml';

type

  { Tmainform }

  Tmainform = class(TForm)
    cmb_TeeTitleStringList: TComboBox;
    cmb_TeeGenderStringList: TComboBox;
    cmd_MoreClubInfo: TButton;
    cmd_close: TBitBtn;
    cmb_Courselist: TComboBox;
    cmb_TeeColourStringList: TComboBox;
    Image1: TImage;
    Label3: TLabel;
    lbl_TotalTeeDistance: TLabel;
    lbl_InTeeDistance: TLabel;
    lbl_OutTeeDistance: TLabel;
    lbl_TotalTeeDistanceinfo: TLabel;
    lbl_CourseNumberOfTees: TLabel;
    lbl_GolfCourseName: TLabel;
    lbl_GolfCourseName1: TLabel;
    lbl_GolfCourseName2: TLabel;
    lbl_GolfCourseName3: TLabel;
    lbl_GolfCourseName4: TLabel;
    lbl_GolfCourseName5: TLabel;
    lbl_BogeyRatinginfo: TLabel;
    lbl_BogeyRating: TLabel;
    lbl_InTeeDistanceinfo: TLabel;
    lbl_OutTeeDistanceinfo: TLabel;
    logo_golfml: TImage;
    Label1: TLabel;
    Label2: TLabel;
    lbl_CoursePar: TLabel;
    lbl_CourseRating: TLabel;
    lbl_CourseRating1: TLabel;
    lbl_CourseNumberOfHoles: TLabel;
    lbl_SlopeRating: TLabel;
    lbl_SlopeRating1: TLabel;
    MainMenu1: TMainMenu;
    mnu_helpaboutgolfml: TMenuItem;
    mnu_helpAboutApp: TMenuItem;
    mnu_fileExit: TMenuItem;
    mnu_fileOpen: TMenuItem;
    mnu_file: TMenuItem;
    menu_helpabout: TMenuItem;
    mnu_help: TMenuItem;
    OpenDialog1: TOpenDialog;
    opt_units: TRadioGroup;
    procedure cmb_CourselistChange(Sender: TObject);
    procedure cmb_TeeColourStringListChange(Sender: TObject);
    procedure cmb_TeeGenderStringListChange(Sender: TObject);
    procedure cmb_TeeTitleStringListChange(Sender: TObject);
    procedure cmd_MoreClubInfoClick(Sender: TObject);
    procedure formcreate(Sender: TObject);
    procedure menu_helpaboutClick(Sender: TObject);
    procedure mnu_fileExitClick(Sender: TObject);
    procedure mnu_fileOpenClick(Sender: TObject);
    procedure mnu_helpAboutAppClick(Sender: TObject);
    procedure mnu_helpaboutgolfmlClick(Sender: TObject);
    procedure opt_teecolourIndexClick(Sender: TObject);
    procedure opt_unitsClick(Sender: TObject);
  private
    { private declarations }
    // Arrays of visual controls
    HoleLabelArray: array[0..17] of TLabel;
    ParLabelArray: array[0..17] of TLabel;
    SILabelArray: array[0..17] of TLabel;
    DistanceLabelArray: array[0..17] of TLabel;
    procedure RefreshDisplay;
    procedure CreateHoleControls;
  public
    { public declarations }
    HCPGolfUMLClass: THCPGolfUMLClass;
    //    HCPGolfUMLClass:TGolfmlClass;
  end;

var
  mainform: Tmainform;

implementation

{$R *.lfm}

{ Tmainform }
procedure tmainform.CreateHoleControls;
// Quicker to create all the labels on-the-fly
// and then use loops to assign captions
var
  iCount, iWidth, iHeight, iTop, iVerticalOffset: integer;
begin
  iWidth := 52;
  iHeight := 15;
  iTop := 210;
  iVerticalOffset := 24;
  for iCount := 0 to 8 do
  begin
    HoleLabelArray[iCount] := TLabel.Create(nil);
    ParLabelArray[iCount] := TLabel.Create(nil);
    SILabelArray[iCount] := TLabel.Create(nil);
    DistanceLabelArray[iCount] := TLabel.Create(nil);
    with HoleLabelArray[iCount] do
    begin
      Top := iTop;
      Left := 18;
      Width := iWidth;
      Height := iHeight;
      Autosize := False;
      Tag := iCount;
      Alignment := taCenter;
      Caption := Format('Hole %d', [iCount + 1]);
      Parent := mainform;
    end;
    with ParLabelArray[iCount] do
    begin
      Top := iTop;
      Left := 72;
      Width := iWidth;
      Height := iHeight;
      Autosize := False;
      Tag := iCount;
      Caption := 'Par 0';
      Parent := mainform;
    end;
    with SILabelArray[iCount] do
    begin
      Top := iTop;
      Left := 120;
      Width := iWidth;
      Height := iHeight;
      Autosize := False;
      Tag := iCount;
      Caption := 'S.I. 0';
      Parent := mainform;
    end;
    with DistanceLabelArray[iCount] do
    begin
      Top := iTop;
      Left := 168;
      Width := iWidth;
      Height := iHeight;
      Autosize := False;
      Tag := iCount;
      Caption := '000m';
      Parent := mainform;
    end;
    iTop := iTop + iVerticalOffset;
  end;
  iTop := 210;
  for iCount := 9 to 17 do
  begin
    HoleLabelArray[iCount] := TLabel.Create(nil);
    ParLabelArray[iCount] := TLabel.Create(nil);
    SILabelArray[iCount] := TLabel.Create(nil);
    DistanceLabelArray[iCount] := TLabel.Create(nil);
    with HoleLabelArray[iCount] do
    begin
      Top := iTop;
      Left := 288;
      Width := iWidth;
      Height := iHeight;
      Autosize := False;
      Tag := iCount;
      Alignment := taCenter;
      Caption := Format('Hole %d', [iCount + 1]);
      Parent := mainform;
    end;
    with ParLabelArray[iCount] do
    begin
      Top := iTop;
      Left := 342;
      Width := iWidth;
      Height := iHeight;
      Autosize := False;
      Tag := iCount;
      Caption := 'Par 0';
      Parent := mainform;
    end;
    with SILabelArray[iCount] do
    begin
      Top := iTop;
      Left := 390;
      Width := iWidth;
      Height := iHeight;
      Autosize := False;
      Tag := iCount;
      Caption := 'SI: 0';
      Parent := mainform;
    end;
    with DistanceLabelArray[iCount] do
    begin
      Top := iTop;
      Left := 438;
      Width := iWidth;
      Height := iHeight;
      Autosize := False;
      Tag := iCount;
      Caption := '000m';
      Parent := mainform;
    end;
    iTop := iTop + iVerticalOffset;
  end;
end;

procedure tmainform.RefreshDisplay;
var
  iCount: integer;
  sColour: string;
begin
  lbl_GolfCourseName.Caption := HCPGolfUMLClass.ClubName;
  cmb_TeeTitleStringList.ItemIndex := cmb_TeeColourStringList.ItemIndex;
  cmb_TeeGenderStringList.ItemIndex := cmb_TeeColourStringList.ItemIndex;
  HCPGolfUMLClass.TeeColourIndex := cmb_TeeColourStringList.ItemIndex;
  sColour := cmb_TeeColourStringList.Items[cmb_TeeColourStringList.ItemIndex]; // Cosmetic!
  lbl_SlopeRating1.Caption := Format('%d', [HCPGolfUMLClass.SlopeRating]);
  lbl_CourseRating1.Caption := Format('%.1f', [HCPGolfUMLClass.CourseRating]);
  lbl_CourseNumberOfHoles.Caption := Format('%d', [HCPGolfUMLClass.CourseHoleCount]);
  lbl_CourseNumberOfTees.Caption := Format('%d', [HCPGolfUMLClass.CourseTeeColourCount]);
  lbl_CoursePar.Caption := Format('%d', [HCPGolfUMLClass.CoursePar]);
  lbl_BogeyRating.Caption := Format('%.1f', [HCPGolfUMLClass.BogeyRating]);
  if HCPGolfUMLClass.Metric then
  begin
    lbl_InTeeDistance.Caption := Format('%dm', [HCPGolfUMLClass.InTeeDistance]);
    lbl_OutTeeDistance.Caption := Format('%dm', [HCPGolfUMLClass.OutTeeDistance]);
    lbl_TotalTeeDistance.Caption :=
      Format('%dm', [HCPGolfUMLClass.TotalTeeDistance]);
  end
  else
  begin
    lbl_InTeeDistance.Caption := Format('%dyds', [HCPGolfUMLClass.InTeeDistance]);
    lbl_OutTeeDistance.Caption :=
      Format('%dyds', [HCPGolfUMLClass.OutTeeDistance]);
    lbl_TotalTeeDistance.Caption :=
      Format('%dyds', [HCPGolfUMLClass.TotalTeeDistance]);
  end;
  for iCount := 0 to 17 do
  begin
    HoleLabelArray[iCount].Visible := False;
    ParLabelArray[iCount].Visible := False;
    SILabelArray[iCount].Visible := False;
    DistanceLabelArray[iCount].Visible := False;

    HCPGolfUMLClass.Hole := iCount + 1;
    ParLabelArray[iCount].Caption := Format('Par %d', [HCPGolfUMLClass.Par]);
    SILabelArray[iCount].Caption := Format('SI: %d', [HCPGolfUMLClass.StrokeIndex]);
    if HCPGolfUMLClass.Metric = True then
      DistanceLabelArray[iCount].Caption := Format('%dm', [HCPGolfUMLClass.Distance])
    else
      DistanceLabelArray[iCount].Caption := Format('%dyds.', [HCPGolfUMLClass.Distance]);
    if iCount < HCPGolfUMLClass.CourseHoleCount then
    begin
      HoleLabelArray[iCount].Color := clDefault;
      HoleLabelArray[iCount].Font.Color := clDefault;
      if CompareText(sColour, 'White') = 0 then
      begin
        HoleLabelArray[iCount].Color := clWhite;
        HoleLabelArray[iCount].Font.Color := clBlack;
      end;
      if CompareText(sColour, 'Black') = 0 then
      begin
        HoleLabelArray[iCount].Color := clBlack;
        HoleLabelArray[iCount].Font.Color := clWhite;
      end;
      if CompareText(sColour, 'Blue') = 0 then
      begin
        HoleLabelArray[iCount].Color := clNavy;
        HoleLabelArray[iCount].Font.Color := clWhite;
      end;
      if CompareText(sColour, 'Gold') = 0 then
      begin
        HoleLabelArray[iCount].Color := clOlive;
        HoleLabelArray[iCount].Font.Color := clWhite;
      end;
      if CompareText(sColour, 'Red') = 0 then
      begin
        HoleLabelArray[iCount].Color := clRed;
      end;
      if CompareText(sColour, 'Yellow') = 0 then
      begin
        HoleLabelArray[iCount].Color := clYellow;
      end;
      HoleLabelArray[iCount].Visible := True;
      ParLabelArray[iCount].Visible := True;
      SILabelArray[iCount].Visible := True;
      DistanceLabelArray[iCount].Visible := True;
    end;
  end;
end;

procedure tmainform.formcreate(Sender: TObject);
var
  bcoursedata: boolean;
begin
  Icon := Application.Icon;
    {$IFDEF LINUX}
  Application.Title := Application.Title + ' (Linux';
    {$ENDIF}
    {$IFDEF WINDOWS}
  Application.Title := Application.Title + ' (Windows';
    {$ENDIF}
    {$ifdef CPU32}
  Application.Title := Application.Title + ' 32-bit)';
    {$ENDIF}
    {$ifdef CPU64}
  Application.Title := Application.Title + ' 64-bit)';
    {$ENDIF}
  Caption := Application.Title;
  CreateHoleControls;
  // Use overridden methods

  //  HCPGolfUMLClass:=TGolfmlClass.Create;
  HCPGolfUMLClass := THCPGolfUMLClass.Create;
  if not HCPGolfUMLClass.InitOK then
    HALT;

  // Load up default data if available
  HCPGolfUMLClass.CourseXMLPath := Application.Location + C_DEFAULTXMLFILE;
  bcoursedata := FileExists(HCPGolfUMLClass.CourseXMLPath);
  if bcoursedata then
    HCPGolfUMLClass.GetCourseInfoFromFile;

  cmb_courseList.Items.Clear;
  if bcoursedata then
    cmb_courseList.Items := HCPGolfUMLClass.CourseStringList;
  cmb_TeeColourStringList.Items.Clear;
  if bcoursedata then
  begin
    cmb_TeeColourStringList.Items := HCPGolfUMLClass.TeeColourStringList;
    cmb_TeeTitleStringList.Items := HCPGolfUMLClass.TeeTitleStringList;
    cmb_TeeGenderStringList.Items := HCPGolfUMLClass.TeeGenderStringList;
  end;
  cmb_courseList.ItemIndex := 0;
  cmb_TeeColourStringList.ItemIndex := 0;
  cmb_TeeTitleStringList.ItemIndex := 0;
  cmb_TeeGenderStringList.ItemIndex := 0;
  opt_units.ItemIndex := 0;
  if bcoursedata then
    RefreshDisplay
  else
    MessageDlg('Use File menu/Import Golfml file to load GolfML data',
      mtInformation, [mbOK], 0);
end;

procedure Tmainform.menu_helpaboutClick(Sender: TObject);
begin
  HCPGolfUMLClass.ShowHelp; // Use class method
end;

procedure Tmainform.mnu_fileExitClick(Sender: TObject);
begin
  Close;
end;

procedure Tmainform.mnu_fileOpenClick(Sender: TObject);
begin
  OpenDialog1.InitialDir := Application.Location;
  OpenDialog1.Title := 'Open a new GolfML data file';
  if OpenDialog1.Execute then
  begin
    HCPGolfUMLClass.Reset; // Re-initialise to blank
    HCPGolfUMLClass.CourseXMLPath := OpenDialog1.Filename; // Set property
    HCPGolfUMLClass.GetCourseInfoFromFile; // Call main method

    cmb_courseList.Items.Clear;
    cmb_courseList.Items := HCPGolfUMLClass.CourseStringList;
    cmb_TeeColourStringList.Items.Clear;
    cmb_TeeTitleStringList.Items.Clear;
    cmb_TeeGenderStringList.Items.Clear;
    cmb_TeeColourStringList.Items := HCPGolfUMLClass.TeeColourStringList;
    cmb_TeeTitleStringList.Items := HCPGolfUMLClass.TeeTitleStringList;
    cmb_TeeGenderStringList.Items := HCPGolfUMLClass.TeeGenderStringList;
    cmb_courseList.ItemIndex := 0;
    cmb_TeeColourStringList.ItemIndex := 0;
    opt_units.ItemIndex := 0;
    RefreshDisplay;
  end;
end;

procedure Tmainform.mnu_helpAboutAppClick(Sender: TObject);
var
  s: string;
begin
  s := Application.Title + LineEnding;
  s += 'Version ' + C_APPVERSION + LineEnding;
  s += 'by minesadorada' + LineEnding;
  s += 'GolfMl: http://code.google.com/p/golfml/';
  MessageDlg(s, mtInformation, [mbOK], 0);
end;

procedure Tmainform.mnu_helpaboutgolfmlClick(Sender: TObject);
var
  s: string;
begin
  s := 'GolfML is a text-based human-readable file format used to exchange golf data ';
  s += 'between applications and web sites.' + LineEnding + LineEnding;
  s += 'Golf course information such as scorecard ';
  s += 'and yardage books, or players scores and statistics are the most common information carried by GolfML.';
  messagedlg(s, mtInformation, [mbOK], 0);
end;

procedure Tmainform.opt_teecolourIndexClick(Sender: TObject);
begin
  RefreshDisplay;
end;

procedure Tmainform.opt_unitsClick(Sender: TObject);
begin
  if opt_units.ItemIndex = 0 then
    HCPGolfUMLClass.Metric := True
  else
    HCPGolfUMLClass.Metric := False;
  RefreshDisplay;
end;

procedure Tmainform.cmb_CourselistChange(Sender: TObject);
begin
  HCPGolfUMLClass.CourseIndex := cmb_courseList.ItemIndex;
  cmb_TeeColourStringList.Items.Clear;
  cmb_TeeTitleStringList.Items.Clear;
  cmb_TeeGenderStringList.Items.Clear;
  cmb_TeeColourStringList.Items := HCPGolfUMLClass.TeeColourStringList;
  cmb_TeeTitleStringList.Items := HCPGolfUMLClass.TeeTitleStringList;
  cmb_TeeGenderStringList.Items := HCPGolfUMLClass.TeeGenderStringList;
  cmb_TeeColourStringList.ItemIndex := 0;
  HCPGolfUMLClass.TeeColourIndex := cmb_TeeColourStringList.ItemIndex;
  RefreshDisplay;
end;

procedure Tmainform.cmb_TeeColourStringListChange(Sender: TObject);
begin
  RefreshDisplay;
end;

procedure Tmainform.cmb_TeeGenderStringListChange(Sender: TObject);
begin
  cmb_TeeColourStringList.ItemIndex := cmb_TeeGenderStringList.ItemIndex;
  RefreshDisplay;
end;

procedure Tmainform.cmb_TeeTitleStringListChange(Sender: TObject);
begin
  cmb_TeeColourStringList.ItemIndex := cmb_TeeTitleStringList.ItemIndex;
  RefreshDisplay;
end;

procedure Tmainform.cmd_MoreClubInfoClick(Sender: TObject);
var
  s: string;
begin
  // Gather all useful data in a big string
  s := '';
  if Length(HCPGolfUMLClass.ClubName) > 0 then
    s += HCPGolfUMLClass.ClubName + LineEnding;
  if Length(HCPGolfUMLClass.ClubStreet) > 0 then
    s += HCPGolfUMLClass.ClubStreet + LineEnding;
  if Length(HCPGolfUMLClass.ClubMunicipality) > 0 then
    s += HCPGolfUMLClass.ClubMunicipality + LineEnding;
  if Length(HCPGolfUMLClass.ClubRegion) > 0 then
    s += HCPGolfUMLClass.ClubRegion + LineEnding;
  if Length(HCPGolfUMLClass.ClubPostCode) > 0 then
    s += 'Zip: ' + HCPGolfUMLClass.ClubPostCode + LineEnding;
  if Length(HCPGolfUMLClass.ClubWebsite) > 0 then
    s += 'Website: ' + HCPGolfUMLClass.ClubWebsite + LineEnding;
  if Length(HCPGolfUMLClass.ClubCountry) > 0 then
    s += 'Country: ' + HCPGolfUMLClass.ClubCountry + LineEnding;
  if Length(HCPGolfUMLClass.ClubPhone) > 0 then
    s += 'Phone: ' + HCPGolfUMLClass.ClubPhone + LineEnding;
  if Length(HCPGolfUMLClass.ClubAmeneties) > 0 then
    s += 'Ameneties: ' + HCPGolfUMLClass.ClubAmeneties + LineEnding;
  if Length(HCPGolfUMLClass.ClubComments) > 0 then
    s += 'Comments: ' + HCPGolfUMLClass.ClubComments + LineEnding;
  if Length(s) = Length(HCPGolfUMLClass.ClubName) + 1 then
    s := 'Sorry, no further information';

  // Show the info
  MessageDlg(s, mtInformation, [mbOK], 0);
end;

end.
