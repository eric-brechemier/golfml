unit umainform;
(*
== Author: minesadorada@charcodelvalle.com
==
== Lazarus: 1.1
== FPC: 2.6.1
==
== golfml XML Writer/Editor GUI
==
== Version History
== 1.0
== Writer built and tested with TGolfmlClass
== 2.0
== Added editing ability
== 2.01
== Cleaned up Country List to ISO standard
== 2.1
== Amenity list into a configuration file
== 2.2-2.4
== Player processing
== Bugfix to golfmlclass golfml structure
== Various bugfixes
== Configuration file storage of player info
== 2.6
== Added regexp validation of phone number
== Added gps capability in golfmlclass
== 2.7
== Added saves/loads country codes in config file
== Win64 build in Lazarus v1.1.fpcv2.6.1
==
== TODO:
== Internationalisation si vous plait/por favor?
*)

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, strutils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Buttons, StdCtrls, ComCtrls, Menus, ExtCtrls, LazHelpHTML,
  ugolfmlwriter_globals, ugolfmlclass,uwaitform,INIFiles,lclintf,synregexpr;

CONST C_TEEBACKCOLOURARRAY:Array[C_GOLD..C_RED] of LongInt =
      (clOlive,clBlack,clWhite,clYellow,clNavy,clRed);
CONST C_TEEFORECOLOURARRAY:Array[C_GOLD..C_RED] of LongInt =
      (clWhite,clWhite,clBlack,clBlack,clWhite,clWhite);
type

  { Tmainform }

  Tmainform = class(TForm)
    bitbtn1: tbitbtn;
    edt_comment1: TEdit;
    edt_comment2: TEdit;
    edt_countryclubgpslong: TEdit;
    grp_amenities: TCheckGroup;
    cmd_newcourse: TBitBtn;
    cmd_makexml: TBitBtn;
    cmd_close: TBitBtn;
    cmb_CountryClubcountrycode: TComboBox;
    edt_countryclubphone: TEdit;
    edt_countryclubwebsite: TEdit;
    edt_countryclubregion: TEdit;
    edt_countryclubgpslat: TEdit;
    edt_countryclubpostcode: TEdit;
    edt_countryclubstreet: TEdit;
    edt_CountryClubName: TEdit;
    edt_countryclubmunicipality: TEdit;
    grp_instructions: TGroupBox;
    grp_countryclub: TGroupBox;
    HTMLBrowserHelpViewer1: THTMLBrowserHelpViewer;
    HTMLHelpDatabase1: THTMLHelpDatabase;
    img_golfml: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    label12: tlabel;
    label13: tlabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lbl_instructions: TLabel;
    lbl_numCourses: TLabel;
    MainMenu1: TMainMenu;
    mnu_Mode_PlayerScorecard: TMenuItem;
    mnu_mode_CourseStylesheet: TMenuItem;
    mnu_modeCourseOnly: TMenuItem;
    mnu_mode: TMenuItem;
    mnu_filenewclub: TMenuItem;
    mnu_fileImportGolfml: TMenuItem;
    mnu_helpAboutGolfml: TMenuItem;
    mnu_helpHintsOnOff: TMenuItem;
    mnu_helpaboutapp: TMenuItem;
    mnu_help: TMenuItem;
    mnu_fileexit: TMenuItem;
    mnu_file: TMenuItem;
    dlg_open: TOpenDialog;
    pagecontainer: TPageControl;
    dlg_SaveAs: TSaveDialog;
    tab_countryclub: TTabSheet;
    procedure cmb_CountryClubcountrycodeChange(Sender: TObject);
    procedure cmd_closeClick(Sender: TObject);
    procedure cmd_makexmlClick(Sender: TObject);
    procedure cmd_newcourseClick(Sender: TObject);
    procedure edt_countryclubmunicipalityEditingDone(Sender: TObject);
    procedure edt_CountryClubNameEditingDone(Sender: TObject);
    procedure edt_countryclubphoneEditingDone(Sender: TObject);
    procedure edt_countryclubpostcodeEditingDone(Sender: TObject);
    procedure edt_countryclubregionEditingDone(Sender: TObject);
    procedure edt_countryclubstreetEditingDone(Sender: TObject);
    procedure edt_countryclubwebsiteEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure label12click(sender: tobject);
    procedure mnu_fileImportGolfmlClick(Sender: TObject);
    procedure mnu_filenewclubClick(Sender: TObject);
    procedure mnu_helpaboutappClick(Sender: TObject);
    procedure mnu_helpaboutgolfmlclick(sender: tobject);
    procedure mnu_modecourseonlyclick(sender: tobject);
    procedure mnu_mode_coursestylesheetclick(sender: tobject);
    procedure mnu_mode_playerscorecardclick(sender: tobject);
    procedure pagecontainerCloseTabClicked(Sender: TObject);
    procedure pagecontainerPageChanged(Sender: TObject);
  private
    { private declarations }
    // DYNAMIC ARRAYS
    GolfmlClass:TGolfmlClass;
    fCourseArray:Array of String;
    pages:Array of TTabSheet;
    iNumCourses:Integer;

    fCountryClubName:String; // ClubName
    fCountryClubCountry:String; //  ClubCountry
    fCountryClubCountryCode:String; //  ClubCountryCode
    fCountryClubMunicipality:String; //  ClubMunicipality
    fCountryClubRegion:String; //  ClubRegion
    fCountryClubStreet:String; //  ClubStreet
    fCountryClubPostCode:String; //  ClubPostCode
    fCountryClubWebsite:String; //  ClubWebsite
    fCountryClubPhone:String; //  ClubPhone
    fCountryClubAmeneties:String; //  ClubAmeneties
    fCountryClubComments:String; //  ClubComments
    fCountryClubGPSLat:String;
    fCountryClubGPSLong:String;
    fCourseNumberOfHoles:Cardinal; //  CourseNumberOfHoles
    fCourseCount:Cardinal; //  CourseCount
    fTeeColourCount:Cardinal; //  TeeColourCount
    fMetric:Boolean; //  Metric
    fCoursePar:Cardinal; //  Par;
    fStrokeIndex:Cardinal; //  Strokeindex
    fTeeDistanceMetres,fTeeDistanceYards:Cardinal; //  Distance
    fSlopeRating:Cardinal; //  SlopeRating
    fCourseRating:Single; //  CourseRating
    fCourseXMLPath:String; // Unused
    fCourseLoaded:Boolean; // Unused
    fCourseName:String; // Course Name
    fTeeTitle,fTeeColour,fTeeGender:String; // Tee postion properties

    // Course control Arrays
    lbl_hole:Array of Array[0..17] of TLabel;
    lbl_ParInfo:Array of Array[0..1] of TLabel;
    edt_par:Array of Array[0..17] of TEdit;
    lbl_SIInfo:Array of Array[0..1] of TLabel;
    edt_SI:Array of Array[0..17] of TEdit;
    opt_units: Array of TRadioGroup;
    opt_9or18: Array of TRadioGroup;
    topbevel:Array of TBevel;
    teebevel:Array of TBevel;
    lbl_courseNameInfo: Array of TLabel;
    edt_courseName: Array of TEdit;
    // Tee Position control arrays (tee distance)
    // This could (and should) be one 3-dimensional array, but
    // the code is clearer when they are seperated
    lbl_TeeGoldInfo:Array of Array[0..1] of TLabel;
    edt_TeeGold:Array of Array[0..17] of TEdit;
    lbl_TeeBlackInfo:Array of Array[0..1] of TLabel;
    edt_TeeBlack:Array of Array[0..17] of TEdit;
    lbl_TeeWhiteInfo:Array of Array[0..1] of TLabel;
    edt_TeeWhite:Array of Array[0..17] of TEdit;
    lbl_TeeYellowInfo:Array of Array[0..1] of TLabel;
    edt_TeeYellow:Array of Array[0..17] of TEdit;
    lbl_TeeBlueInfo:Array of Array[0..1] of TLabel;
    edt_TeeBlue:Array of Array[0..17] of TEdit;
    lbl_TeeRedInfo:Array of Array[0..1] of TLabel;
    edt_TeeRed:Array of Array[0..17] of TEdit;

    chk_useteecolour:Array of Array[C_GOLD..C_RED] of TCheckBox;
    lbl_CourseRating:Array of Array[C_GOLD..C_RED] of TLabel;
    edt_CourseRating:Array of Array[C_GOLD..C_RED] of TEdit;
    lbl_SlopeRating:Array of Array[C_GOLD..C_RED] of TLabel;
    edt_SlopeRating:Array of Array[C_GOLD..C_RED] of TEdit;
    lbl_TeeTitle:Array of Array[C_GOLD..C_RED] of TLabel;
    edt_TeeTitle:Array of Array[C_GOLD..C_RED] of TEdit;
    lbl_TeeGender:Array of Array[C_GOLD..C_RED] of TLabel;
    spd_TeeGentlemen:Array of Array[C_GOLD..C_RED] of TSpeedButton;
    spd_TeeLadies:Array of Array[C_GOLD..C_RED] of TSpeedButton;

    pageLastIndex:Integer;
    INI:TINIFile;

    fPlayerName,fPlayerDateOfBirth,fPlayerGender:String;
    fPlayerHandicap:Single;
    fScoreCardCourseName,fScoreCardTeeColour:String;

    Procedure ShowInstructions; // Populated the Instructions panel
    procedure PopulateCoursePage; // Creates and initialises a set of Course control arrays
    procedure Process_edt_courseName(Sender: TObject);  // Array event handler
    procedure Process_opt_9or18(Sender: TObject); // Array event handler
    procedure Process_opt_units(Sender: TObject); // Array event handler
    procedure Process_chk_useteecolour(Sender: TObject); // Array event handler
    function AssignData:Boolean; // Exports to a golfml structute
    procedure DeleteAllCoursePages; // Destroys all Course pages and child controls
    function ImportData:Boolean; // Imports from a golfml structure
    procedure DisableTeePositionDisplay(Const iCourse,iTeePosition:Integer;Const Full18:Boolean);
    procedure EnableTeePositionDisplay(Const iCourse,iTeePosition:Integer;Const Full18:Boolean);
    procedure SplitAmenety(var fAmenityType,fAmenetyValue:String;Const aString:String);


  public
    { public declarations }
  end;

var
  mainform: Tmainform;

implementation

{$R *.lfm}

{ Tmainform }
(******************************************************************************)
procedure Tmainform.SplitAmenety(var fAmenityType,fAmenetyValue:String;Const aString:String);
// Changes fAmenityType, fAmenetyValue
// Uses C_AMENETYDELIMITERCHAR
Var
   i:Integer;
begin
     if Length(aString)=0 then exit;
     i:=Pos(C_AMENETYDELIMITERCHAR,aString);
     If i > 1 then
     Begin
          fAmenityType:=LeftStr(aString,i-1);
          fAmenetyValue:=RightStr(aString,Length(aString)-i);
     end;
end;
(******************************************************************************)
procedure Tmainform.DeleteAllCoursePages;
// Called from File menu: 'New Golf Course' and 'Import golfml file'
// Destroys all the course pagess
// Destroying the parent control also destroys all it's child controls automatically
Var
   iCount:Integer;
   s,sCode,sCountry:String;
begin
    pagecontainer.ActivePage:=tab_countryclub;  // Switch to the Club page
//    For iCount:=0 to High(pages) do
    For iCount:=0 to PageLastIndex do
      begin
           pages[iCount].Destroy; // Destroy the Course pages
      end;
    Setlength(pages,1); // Reset the control array
    PageLastIndex:=-1; // First increment is to Zero
    iNumCourses:=0; // 1-based local var
    GolfmlClass.Reset; // Re-initialise the class variables

    // Reset the Country Club page controls to default values
    edt_CountryClubName.Text:='My Golf Club';
    edt_countryclubstreet.Text:='';
    edt_countryclubmunicipality.Text:='';
    edt_countryclubregion.Text:='';
    edt_countryclubpostcode.Text:='';
    edt_countryclubphone.Text:='';
    edt_countryclubwebsite.Text:='http://www.';
    cmb_CountryClubcountrycode.ItemIndex:=0;
    s:=cmb_CountryClubcountrycode.Items[cmb_CountryClubcountrycode.ItemIndex];
    sCode:=LeftStr(s,2);
    sCountry:=RightStr(s,Length(s)-4);
    edt_countryclubgpslat.Text:='0';
    edt_countryclubgpslong.Text:='0';

    edt_comment1.Text:='';
    edt_comment2.Text:='';
    For iCount:=0 to grp_amenities.Items.Count-1 do
        grp_amenities.checked[iCount]:=false;
    tab_countryclub.Caption:='My Golf Club';
    edt_CountryClubName.Text:='My Golf Club';
    grp_countryclub.Caption:='My Golf Club';
    lbl_numCourses.Caption:=Format('Courses attached to %s: %d',
    [tab_countryclub.Caption,iNumCourses]);
    fCountryClubName:=tab_countryclub.Caption;
    cmd_newcourse.Visible:=True;

    fCountryClubName:='My Golf Club';
    fCountryClubCountry:='';
    fCountryClubCountryCode:='US';
    fCountryClubMunicipality:='';
    fCountryClubRegion:='';
    fCountryClubStreet:='';
    fCountryClubPostCode:='';
    fCountryClubWebsite:='http://www.';
    fCountryClubPhone:='';
    fCountryClubAmeneties:='';
    fCountryClubComments:='';
    fCourseNumberOfHoles:=0;
    fCourseCount:=0;
    fTeeColourCount:=0;
    fMetric:=True;
    fCoursePar:=0;
    fStrokeIndex:=0;
    fTeeDistanceMetres:=0;
    fTeeDistanceYards:=0;
    fSlopeRating:=0;
    fCourseRating:=0;
    fCourseName:='';
    fTeeTitle:='';
    fTeeColour:='';
    fTeeGender:='';
    MakeXMLMode:=C_MODECOURSEONLY;
    fScoreCardCourseName:='unknown';
    fScoreCardTeeColour:='unknown';
    fPlayerName:='unknown';
    fPlayerDateOfBirth:=FormatDateTime(ShortDateFormat,Now);
    fPlayerGender:='unknown';
    fPlayerHandicap:=36;
    fCountryClubGPSLat:='0';
    fCountryClubGPSLong:='0';

    mnu_modeCourseOnly.Checked:=True;
end;
(******************************************************************************)
function Tmainform.ImportData:Boolean;
Var
   iCourseCount,iTeeColourCount,iHoleCount,iAmenityCount,iCountryCount:Integer;
   s:String;
   iCount,iItemIndex:Integer;
   bMoreThanNineHolesFlag:Boolean;
begin
    Result:=False;
TRY //..EXCEPT
    PageLastIndex:=-1;
    SetLength(fCourseArray,GolfmlClass.MaxCourseIndex+1);
  TRY // ..FINALLY
     waitform.loadprogress.max:=Integer(GolfmlClass.MaxCourseIndex);
     waitform.loadprogress.Position:=0;
     s:='Importing ' + ExtractFilename(GolfmlClass.CourseXMLPath) + '...' + LineEnding;
     waitform.lbl_info.Caption:=s;
     waitform.show;
     Application.ProcessMessages;
// 1) Create Course Pages and populate Course Names
    For iCourseCount:=0 to GolfmlClass.MaxCourseIndex do
      begin
           cmd_newcourseClick(Nil); // Creates new Course Page
           GolfmlClass.CourseIndex:=iCourseCount;
           pages[iCourseCount].Caption:=GolfmlClass.CourseName;
           fCourseArray[iCourseCount]:=GolfmlClass.CourseName;
           waitform.loadprogress.Position:=iCourseCount;
           s:='Importing ' + ExtractFilename(GolfmlClass.CourseXMLPath) + '...' + LineEnding;
           s+=GolfmlClass.CourseName + ' Loaded OK';
           waitform.lbl_info.Caption:=s;
           Application.ProcessMessages;
      end;
// 2 ) Populate Country Club page
   With GolfmlClass do
    begin
         edt_CountryClubName.Text:=ClubName;
         tab_countryclub.Caption:=edt_CountryClubName.Text;
         grp_countryclub.Caption:=edt_CountryClubName.Text;
         edt_countryclubstreet.Text:=ClubStreet;
         edt_countryclubmunicipality.Text:=ClubMunicipality;
         edt_countryclubregion.Text:=ClubRegion;
         edt_countryclubpostcode.Text:=ClubPostCode;
         edt_countryclubgpslat.Text:=FloatToStr(ClubGPSLatitude);
         edt_countryclubgpslong.Text:=FloatToStr(ClubGPSLongitude);
         edt_countryclubphone.Text:=ClubPhone;
         edt_countryclubwebsite.Text:=ClubWebsite;
         // Only the first 2 comments are imported.  The 'Created By' and 'charcodelvalle' comments are ignored.
         If ClubMaxCommentsIndex > 0 then
            If (AnsiContainsStr(ClubComments[1],'Created by Golfml Coursewriter') = FALSE)
            AND (AnsiContainsStr(ClubComments[1],'charcodelvalle.com') = FALSE)
            then
               edt_comment1.Text:=ClubComments[1];
         If ClubMaxCommentsIndex > 1 then
            If (AnsiContainsStr(ClubComments[2],'Created by Golfml Coursewriter') = FALSE)
            AND (AnsiContainsStr(ClubComments[2],'charcodelvalle.com') = FALSE)
            then
               edt_comment2.Text:=ClubComments[2];
         For iAmenityCount:=0 to ClubMaxAmenitiesIndex do
           begin
                s:=ClubAmeneties[iAmenityCount];
                iItemIndex:=grp_amenities.Items.IndexOf(s);
                If iItemIndex > -1 then grp_amenities.Checked[iItemIndex]:=True;
           end;
         For iCountryCount:=0 to cmb_CountryClubcountrycode.Items.Count-1 do
           begin
                s:=UpperCase(LeftStr(cmb_CountryClubcountrycode.Items[iCountryCount],2));
                if (s = ClubCountryCode) then
                  begin
                       cmb_CountryClubcountrycode.ItemIndex:=iCountryCount;
                       Break;
                  end;
           end;
// 3 ) Populate each Course Page
          For iCourseCount:=0 to MaxCourseIndex do
            begin
                 GolfmlClass.CourseIndex:=iCourseCount;
                 CourseIndex:=iCourseCount;
                 edt_courseName[iCourseCount].Text:=CourseName;
                 // Set 9 or 18 holes option group
                 if CourseNumberOfHoles > 9 then
                 begin
                    bMoreThanNineHolesFlag:=TRUE;
                    opt_9or18[iCourseCount].ItemIndex:=0;
                 end
                 else
                 begin
                     bMoreThanNineHolesFlag:=FALSE;
                     opt_9or18[iCourseCount].ItemIndex:=1;
                 end;
                 // Populate Course Pars and StokeIndexes

                 For iHoleCount:=0 to CourseNumberOfHoles-1 do
                     Begin
                          HoleIndex:=iHoleCount;
                          edt_par[iCourseCount,iHoleCount].Text:=Format('%d',[Par]);
                          edt_SI[iCourseCount,iHoleCount].Text:=Format('%d',[StrokeIndex]);
                     end;

                 // Disable all tee positions for this course
                 For iCount:=C_GOLD to C_RED do
                     begin
                          DisableTeePositionDisplay(iCourseCount,iCount,TRUE); // Make all tee info invisible
                          chk_useteecolour[iCourseCount,iCount].Checked := False; // Uncheck all Tee Positions
                     end;
                 // Enable and populate only those tee positions where the tee color matches GOLD..RED
                 For iTeeColourCount:=0 to MaxTeeColourIndex do
                   begin
                       TeeColourIndex:=iTeeColourCount; // Switch the golfml class to this tee colour index
                       // Test if this TeeColourIndex matches our set of colours.  If so, populate the Tee Colour stuff
                       TeeColour:=LowerCase(TeeColour);
                            If TeeColour='gold' then
                            begin
                                 edt_CourseRating[iCourseCount,C_GOLD].Text:=Format('%0.1f',[CourseRating]);
                                 edt_SlopeRating[iCourseCount,C_GOLD].Text:=Format('%d',[SlopeRating]);
                                 edt_TeeTitle[iCourseCount,C_GOLD].Text:=TeeTitle;
                                 if LowerCase(TeeGender)='gentlemen' then
                                    spd_TeeGentlemen[iCourseCount,C_GOLD].Down:=True
                                 else
                                     spd_TeeGentlemen[iCourseCount,C_GOLD].Down:=False;
                                  For iHoleCount:=0 to CourseNumberOfHoles-1 do
                                      begin
                                           HoleIndex:=iHoleCount;
                                           edt_TeeGold[iCourseCount,iHoleCount].Text:=Format('%d',[TeeDistanceMetres]);
                                      end;
                                 EnableTeePositionDisplay(iCourseCount,C_GOLD,bMoreThanNineHolesFlag);
                                 chk_useteecolour[iCourseCount,C_GOLD].Checked := True;
                            end;
                            If TeeColour='black' then
                            begin
                                 edt_CourseRating[iCourseCount,C_BLACK].Text:=Format('%0.1f',[CourseRating]);
                                 edt_SlopeRating[iCourseCount,C_BLACK].Text:=Format('%d',[SlopeRating]);
                                 edt_TeeTitle[iCourseCount,C_BLACK].Text:=TeeTitle;
                                 if LowerCase(TeeGender)='gentlemen' then
                                    spd_TeeGentlemen[iCourseCount,C_BLACK].Down:=True
                                 else
                                     spd_TeeGentlemen[iCourseCount,C_BLACK].Down:=False;
                                 For iHoleCount:=0 to CourseNumberOfHoles-1 do
                                     begin
                                          HoleIndex:=iHoleCount;
                                          edt_TeeBlack[iCourseCount,iHoleCount].Text:=Format('%d',[TeeDistanceMetres]);
                                     end;
                                 EnableTeePositionDisplay(iCourseCount,C_BLACK,bMoreThanNineHolesFlag);
                                 chk_useteecolour[iCourseCount,C_BLACK].Checked := True;
                            end;
                            If TeeColour='white' then
                            begin
                                 edt_CourseRating[iCourseCount,C_WHITE].Text:=Format('%0.1f',[CourseRating]);
                                 edt_SlopeRating[iCourseCount,C_WHITE].Text:=Format('%d',[SlopeRating]);
                                 edt_TeeTitle[iCourseCount,C_WHITE].Text:=TeeTitle;
                                 if LowerCase(TeeGender)='gentlemen' then
                                    spd_TeeGentlemen[iCourseCount,C_WHITE].Down:=True
                                 else
                                     spd_TeeGentlemen[iCourseCount,C_WHITE].Down:=False;
                                 For iHoleCount:=0 to CourseNumberOfHoles-1 do
                                     begin
                                          HoleIndex:=iHoleCount;
                                          edt_TeeWhite[iCourseCount,iHoleCount].Text:=Format('%d',[TeeDistanceMetres]);
                                     end;
                                 EnableTeePositionDisplay(iCourseCount,C_WHITE,bMoreThanNineHolesFlag);
                                 chk_useteecolour[iCourseCount,C_WHITE].Checked := True;
                            end;
                            If TeeColour='yellow' then
                            begin
                                 edt_CourseRating[iCourseCount,C_YELLOW].Text:=Format('%0.1f',[CourseRating]);
                                 edt_SlopeRating[iCourseCount,C_YELLOW].Text:=Format('%d',[SlopeRating]);
                                 edt_TeeTitle[iCourseCount,C_YELLOW].Text:=TeeTitle;
                                 if LowerCase(TeeGender)='gentlemen' then
                                    spd_TeeGentlemen[iCourseCount,C_YELLOW].Down:=True
                                 else
                                     spd_TeeGentlemen[iCourseCount,C_YELLOW].Down:=False;
                                 For iHoleCount:=0 to CourseNumberOfHoles-1 do
                                     begin
                                          HoleIndex:=iHoleCount;
                                          edt_TeeYellow[iCourseCount,iHoleCount].Text:=Format('%d',[TeeDistanceMetres]);
                                     end;
                                 EnableTeePositionDisplay(iCourseCount,C_YELLOW,bMoreThanNineHolesFlag);
                                 chk_useteecolour[iCourseCount,C_YELLOW].Checked := True;
                            end;
                            If TeeColour='blue' then
                            begin
                                 edt_CourseRating[iCourseCount,C_BLUE].Text:=Format('%0.1f',[CourseRating]);
                                 edt_SlopeRating[iCourseCount,C_BLUE].Text:=Format('%d',[SlopeRating]);
                                 edt_TeeTitle[iCourseCount,C_BLUE].Text:=TeeTitle;
                                 if LowerCase(TeeGender)='gentlemen' then
                                    spd_TeeGentlemen[iCourseCount,C_BLUE].Down:=True
                                 else
                                     spd_TeeGentlemen[iCourseCount,C_BLUE].Down:=False;
                                 For iHoleCount:=0 to CourseNumberOfHoles-1 do
                                     begin
                                          HoleIndex:=iHoleCount;
                                          edt_TeeBlue[iCourseCount,iHoleCount].Text:=Format('%d',[TeeDistanceMetres]);
                                     end;
                                 EnableTeePositionDisplay(iCourseCount,C_BLUE,bMoreThanNineHolesFlag);
                                 chk_useteecolour[iCourseCount,C_BLUE].Checked := True;
                            end;
                            If (TeeColour='red') OR (TeeColour='orange') then
                            begin
                                 edt_CourseRating[iCourseCount,C_RED].Text:=Format('%0.1f',[CourseRating]);
                                 edt_SlopeRating[iCourseCount,C_RED].Text:=Format('%d',[SlopeRating]);
                                 edt_TeeTitle[iCourseCount,C_RED].Text:=TeeTitle;
                                 if LowerCase(TeeGender)='gentlemen' then
                                    spd_TeeGentlemen[iCourseCount,C_RED].Down:=True
                                 else
                                     spd_TeeGentlemen[iCourseCount,C_RED].Down:=False;
                                 For iHoleCount:=0 to CourseNumberOfHoles-1 do
                                     begin
                                          HoleIndex:=iHoleCount;
                                          edt_TeeRed[iCourseCount,iHoleCount].Text:=Format('%d',[TeeDistanceMetres]);
                                     end;
                                 EnableTeePositionDisplay(iCourseCount,C_RED,bMoreThanNineHolesFlag);
                                 chk_useteecolour[iCourseCount,C_RED].Checked := True;
                            end;
                   end; // ..of For iTeeColourCount:=0 to
            end;// ..of For iCourseCount:=0 to
    end; // ..of with golfmlclass do
    pagecontainer.ActivePage:=tab_countryclub;
    iNumCourses:=PageLastIndex+1; // Set local 1-based var
    lbl_numCourses.Caption:=Format('Courses attached to %s: %d',
    [tab_countryclub.Caption,iNumCourses]);
  FINALLY
          // Just for show...
          waitform.lbl_info.Caption:='Processing golfml data...';
          Application.ProcessMessages;
          waitform.loadprogress.max:=10;
          For iCourseCount:=1 to 10 do
            begin
                waitform.loadprogress.Position:=iCourseCount;
                Application.ProcessMessages;
                Sleep(50);
            end;
          // Hide progress form
          waitform.hide;
          GolfmlClass.CourseLoaded:=False;
          Result:=True;
  END;
EXCEPT
      raise Exception.Create(C_ERRORAPOLOGY + 'Unable to import this golfml file');
END;
end;
(******************************************************************************)
function Tmainform.AssignData:Boolean;
Var
   cCount,cCourseCount,cTeePositionCount,cHoleCount:Cardinal;
   cValidTeePositionCount:Cardinal;
   iTryInt:Integer;
   sTryFloat:Single;
   cNumHoles:Cardinal;
   bUseMetres:Boolean;
   iAmenityIndex:Integer;
   s,sComment1,sComment2:String;
   sCode,sCountry:String;
begin
  Result:=False;  // assume failure; look for success

  fCountryClubName:=edt_CountryClubName.Text;
  fCountryClubMunicipality:=edt_countryclubmunicipality.Text;
  fCountryClubRegion:=edt_countryclubregion.Text;
  fCountryClubStreet:=edt_countryclubstreet.Text;
  fCountryClubPostCode:=edt_countryclubpostcode.Text;
  fCountryClubPhone:=edt_countryclubphone.Text;
  fCountryClubWebsite:=edt_countryclubwebsite.Text;
  fCountryClubGPSLat:=edt_countryclubgpslat.Text;
  fCountryClubGPSLong:=edt_countryclubgpslong.Text;
  s:=cmb_CountryClubcountrycode.Items[cmb_CountryClubcountrycode.ItemIndex];
  sCode:=LeftStr(s,2);
  fCountryClubCountryCode:=UPPERCase(sCode);
  sCountry:=RightStr(s,Length(s)-4);
  //Strip leading spaces
  While (LeftStr(sCountry,1)=' ') do
        sCountry:=RightStr(s,Length(sCountry)-1);
  fCountryClubCountry:=sCountry;
  TRY
    With GolfmlClass do // Just reference property names from here
     begin
         // Set up Stylesheet Mode
         Case MakeXMLMode of
             C_MODECOURSESTYLESHEET:GolfmlClass.IncludeScoreCardStylesheetReference:=TRUE;
             C_MODECOURSEPLAYERSTYLESHEET:GolfmlClass.IncludePlayerScoreCardStylesheetReference:=TRUE;
         end;

         // 1 Country Club-wide data
          if Length(fCountryClubName) > 0 then
             ClubName:=fCountryClubName
          else
              ClubName:='Country Club'; // Property ClubName


          ClubCountry:=fCountryClubCountry; // Property ClubCountry
          ClubCountryCode:=fCountryClubCountryCode; // Property ClubCountryCode
          ClubMunicipality:=fCountryClubMunicipality; // Property ClubMunicipality
          ClubRegion:=fCountryClubRegion; // Property ClubRegion
          ClubStreet:=fCountryClubStreet; // Property ClubStreet
          ClubPostCode:=fCountryClubPostCode; // Property ClubPostCode
          ClubWebsite:=fCountryClubWebsite; // Property ClubWebsite
          ClubPhone:=fCountryClubPhone; // Property ClubPhone
          If TryStrToFloat(fCountryClubGPSLat,sTryFloat) then
             ClubGPSLatitude:=sTryFloat;
          If TryStrToFloat(fCountryClubGPSLong,sTryFloat) then
             ClubGPSLongitude:=sTryFloat;
          // Deal with CountryClub Comments
          sComment1:=edt_comment1.Text;
          sComment2:=edt_comment2.Text;
          // If comment only in field #2 then move it to field #1
          If (Length(sComment1)=0) and (Length(sComment2) > 0) then
             Begin
                  sComment1:=sComment2;
                  sComment2:='';
             end;
          if (Length(sComment1) > 0) then
             ClubComments[0]:=sComment1;
          if (Length(sComment2) > 0) then
             ClubComments[1]:=sComment2;
          // Deal with Amenities
          iAmenityIndex:=0;
          For cCount:=0 to grp_amenities.Items.Count-1 do
              begin
                   If grp_amenities.Checked[cCount] then
                      begin
                           // Only increment property arry if there is something to put in it
                           ClubAmeneties[iAmenityIndex]:=AmenetyCategoriesArray[cCount] + C_AMENETYDELIMITERCHAR + AmenetyValuesArray[cCount];
                           Inc(iAmenityIndex);
                      end;
              end;
     end;
     // 2 ) Assign to available courses
     If (PageLastIndex >=0) then
     begin
          With GolfmlClass do
          begin
          For cCourseCount:=0 to PageLastIndex do
              begin
                   // Course-wide data
                   CourseIndex:=cCourseCount;
                   // Set Name
                   If Length(edt_courseName[cCourseCount].Text) > 0 then
                      CourseName:=edt_courseName[cCourseCount].Text
                   else
                       CourseName:=Format('Course %d',[cCourseCount+1]);

                   // Set 9 or 18 holes
                   If opt_9or18[cCourseCount].ItemIndex=0 then
                       cNumHoles:=18
                   else
                       cNumHoles:=9;
                   // Were the distances input as Metres or Yards?
                   If opt_Units[cCourseCount].ItemIndex=0 then
                       bUseMetres:=True
                   else
                       bUseMetres:=False;
                   // Set Properties
                   CourseNumberOfHoles:=cNumHoles;
                   Metric:=bUseMetres;

                   // Course Par values
                   For cHoleCount:=0 to 8 do
                       begin
                            HoleIndex:=cHoleCount;
                            If TryStrToInt(edt_Par[cCourseCount,cHoleCount].Text,iTryInt) then
                               Par:=ABS(iTryInt)
                            else
                                begin
                                     ShowMessageFmt('Invalid Par value "%s" in Course %d, Hole %d',[edt_Par[cCourseCount,cHoleCount].Text,cCourseCount+1,cHoleCount+1]);
                                     Exit;
                                end;
                            If ((iTryInt < 1) OR (iTryInt > 6)) then
                               begin
                                    ShowMessageFmt('Invalid Par value "%d" in Course %d, Hole %d',[iTryInt,cCourseCount+1,cHoleCount+1]);
                                    Exit;
                               end;
                       end;
                   If (cNumHoles=18) then
                   For cHoleCount:=9 to 17 do
                       begin
                            HoleIndex:=cHoleCount;
                            If TryStrToInt(edt_Par[cCourseCount,cHoleCount].Text,iTryInt) then
                               Par:=ABS(iTryInt)
                            else
                                Begin
                                     ShowMessageFmt('Invalid Par value "%s" in Course %d, Hole %d',[edt_Par[cCourseCount,cHoleCount].Text,cCourseCount+1,cHoleCount+1]);
                                     Exit;
                                End;
                            If ((iTryInt < 1) OR (iTryInt > 6)) then
                               begin
                                    ShowMessageFmt('Invalid Par value "%d" in Course %d, Hole %d',[iTryInt,cCourseCount+1,cHoleCount+1]);
                                    Exit;
                               end;
                       end;

                   // Course Stroke Index values
                   For cHoleCount:=0 to 8 do
                       begin
                            HoleIndex:=cHoleCount;
                            If TryStrToInt(edt_SI[cCourseCount,cHoleCount].Text,iTryInt) then
                               StrokeIndex:=ABS(iTryInt)
                            else
                                Begin
                                     ShowMessageFmt('Invalid Stroke Index value "%s" in Course %d, Hole %d',[edt_SI[cCourseCount,cHoleCount].Text,cCourseCount+1,cHoleCount+1]);
                                     Exit;
                                end;
                            If ((iTryInt < 1) OR (iTryInt > 18)) then
                               begin
                                    ShowMessageFmt('Invalid Stroke Index value "%d" in Course %d, Hole %d',[iTryInt,cCourseCount+1,cHoleCount+1]);
                                    Exit;
                               end;
                       end;
                   If (cNumHoles=18) then
                   For cHoleCount:=9 to 17 do
                       begin
                            HoleIndex:=cHoleCount;
                            If TryStrToInt(edt_SI[cCourseCount,cHoleCount].Text,iTryInt) then
                               StrokeIndex:=iTryInt
                            else
                                Begin
                                     ShowMessageFmt('Invalid Stroke Index value "%s" in Course %d, Hole %d',[edt_SI[cCourseCount,cHoleCount].Text,cCourseCount+1,cHoleCount+1]);
                                     Exit;
                                end;
                            If ((iTryInt < 1) OR (iTryInt > 18)) then
                               begin
                                    ShowMessageFmt('Invalid Stroke Index value "%d" in Course %d, Hole %d',[iTryInt,cCourseCount+1,cHoleCount+1]);
                                    Exit;
                               end;
                       end;

                   // 3 ) Tee-specific data
                   cValidTeePositionCount:=0;  // Only store valid (checked) TeeIndexes

                   // Tee Distances
                   For cTeePositionCount:=C_GOLD to C_RED do
                       begin
                            if chk_useteecolour[cCourseCount,cTeePositionCount].Checked then
                               begin
                                    // Set Title, Colour and Gender of each valid tee
                                    TeeColourIndex:=cValidTeePositionCount;
                                    // TeeTitle
                                    If Length(edt_TeeTitle[cCourseCount,cValidTeePositionCount].Text) > 0 then
                                       TeeTitle:=edt_TeeTitle[cCourseCount,cValidTeePositionCount].Text;
                                    // TeeGender
                                    if spd_TeeGentlemen[cCourseCount,cValidTeePositionCount].Down=True then
                                       TeeGender:='gentlemen'
                                    else
                                        TeeGender:='ladies';
                                    // TeeColour
                                    Case cValidTeePositionCount of
                                         C_GOLD: TeeColour:='gold';
                                         C_BLACK: TeeColour:='black';
                                         C_WHITE: TeeColour:='white';
                                         C_YELLOW: TeeColour:='yellow';
                                         C_BLUE: TeeColour:='blue';
                                         C_RED: TeeColour:='red';
                                    end;
                                    // Course Rating for this tee
                                    If TryStrToFloat(edt_CourseRating[cCourseCount,cValidTeePositionCount].Text,sTryFloat) then
                                        CourseRating:=sTryFloat
                                    else
                                        Begin
                                             ShowMessageFmt('Invalid Course Rating value "%s" in Course %d, %s Tees',[edt_CourseRating[cCourseCount,cValidTeePositionCount].Text,cCourseCount+1,C_TEECOLOURSTRINGARRAY[cValidTeePositionCount]]);
                                             Exit;
                                        end;

                                    // Slope rating for this tee
                                    If TryStrToInt(edt_SlopeRating[cCourseCount,cValidTeePositionCount].Text,iTryInt) then
                                        SlopeRating:=ABS(iTryInt)
                                    else
                                        Begin
                                             ShowMessageFmt('Invalid Slope Rating value "%s" in Course %d, %s Tees',[edt_SlopeRating[cCourseCount,cValidTeePositionCount].Text,cCourseCount+1,C_TEECOLOURSTRINGARRAY[cValidTeePositionCount]]);
                                             Exit;
                                        End;
                                    // Set Distances for each hole
                                    For cHoleCount:=0 to 8 do
                                        begin
                                             HoleIndex:=cHoleCount;
                                             Case cValidTeePositionCount of
                                                  C_GOLD:
                                                         begin
                                                              if bUseMetres then
                                                                 begin
                                                                      If TryStrToInt(edt_TeeGold[cCourseCount,cHoleCount].Text,iTryInt) then
                                                                         TeeDistanceMetres:=StrToInt(edt_TeeGold[cCourseCount,cHoleCount].Text)
                                                                      else
                                                                      begin
                                                                           ShowMessageFmt('Invalid Tee Distance "%s" metres value in Course %d, Gold Tees, Hole %d',[edt_TeeGold[cCourseCount,cHoleCount].Text,cCourseCount+1,cHoleCount+1]);
                                                                           exit;
                                                                      end;
                                                                 end
                                                              else
                                                                  begin
                                                                       If TryStrToInt(edt_TeeGold[cCourseCount,cHoleCount].Text,iTryInt) then
                                                                          TeeDistanceYards:=StrToInt(edt_TeeGold[cCourseCount,cHoleCount].Text)
                                                                       else
                                                                       begin
                                                                            ShowMessageFmt('Invalid Tee Distance "%s" yards value in Course %d, Gold Tees, Hole %d',[edt_TeeGold[cCourseCount,cHoleCount].Text,cCourseCount+1,cHoleCount+1]);
                                                                       end;
                                                                  end;
                                                         end;
                                                  C_BLACK:
                                                         begin
                                                              if bUseMetres then
                                                                 begin
                                                                      If TryStrToInt(edt_TeeBlack[cCourseCount,cHoleCount].Text,iTryInt) then
                                                                         TeeDistanceMetres:=StrToInt(edt_TeeBlack[cCourseCount,cHoleCount].Text)
                                                                      else
                                                                      begin
                                                                           ShowMessageFmt('Invalid Tee Distance "%s" metres value in Course %d, Black Tees, Hole %d',[edt_TeeBlack[cCourseCount,cHoleCount].Text,cCourseCount+1,cHoleCount+1]);
                                                                           exit;
                                                                      end;
                                                                 end
                                                              else
                                                                  begin
                                                                       If TryStrToInt(edt_TeeBlack[cCourseCount,cHoleCount].Text,iTryInt) then
                                                                          TeeDistanceYards:=StrToInt(edt_TeeBlack[cCourseCount,cHoleCount].Text)
                                                                       else
                                                                       begin
                                                                            ShowMessageFmt('Invalid Tee Distance "%s" yards value in Course %d, Black Tees, Hole %d',[edt_TeeBlack[cCourseCount,cHoleCount].Text,cCourseCount+1,cHoleCount+1]);
                                                                            exit;
                                                                       end;
                                                                  end;
                                                         end;
                                                  C_WHITE:
                                                         begin
                                                              if bUseMetres then
                                                                 begin
                                                                      If TryStrToInt(edt_TeeWhite[cCourseCount,cHoleCount].Text,iTryInt) then
                                                                         TeeDistanceMetres:=StrToInt(edt_TeeWhite[cCourseCount,cHoleCount].Text)
                                                                      else
                                                                      begin
                                                                           ShowMessageFmt('Invalid Tee Distance "%s" metres value in Course %d, White Tees, Hole %d',[edt_TeeWhite[cCourseCount,cHoleCount].Text,cCourseCount+1,cHoleCount+1]);
                                                                           exit;
                                                                      end;
                                                                 end
                                                              else
                                                                  begin
                                                                       If TryStrToInt(edt_TeeWhite[cCourseCount,cHoleCount].Text,iTryInt) then
                                                                          TeeDistanceYards:=StrToInt(edt_TeeWhite[cCourseCount,cHoleCount].Text)
                                                                       else
                                                                       begin
                                                                            ShowMessageFmt('Invalid Tee Distance "%s" yards value in Course %d, White Tees, Hole %d',[edt_TeeWhite[cCourseCount,cHoleCount].Text,cCourseCount+1,cHoleCount+1]);
                                                                            exit;
                                                                       end;
                                                                  end;
                                                         end;
                                                  C_YELLOW:
                                                            begin
                                                                 if bUseMetres then
                                                                    begin
                                                                         If TryStrToInt(edt_TeeYellow[cCourseCount,cHoleCount].Text,iTryInt) then
                                                                            TeeDistanceMetres:=StrToInt(edt_TeeYellow[cCourseCount,cHoleCount].Text)
                                                                         else
                                                                         begin
                                                                              ShowMessageFmt('Invalid Tee Distance "%s" metres value in Course %d, Yellow Tees, Hole %d',[edt_TeeYellow[cCourseCount,cHoleCount].Text,cCourseCount+1,cHoleCount+1]);
                                                                              exit;
                                                                         end;
                                                                    end
                                                                 else
                                                                     begin
                                                                          If TryStrToInt(edt_TeeYellow[cCourseCount,cHoleCount].Text,iTryInt) then
                                                                             TeeDistanceYards:=StrToInt(edt_TeeYellow[cCourseCount,cHoleCount].Text)
                                                                          else
                                                                          begin
                                                                               ShowMessageFmt('Invalid Tee Distance "%s" yards value in Course %d, Yellow Tees, Hole %d',[edt_TeeYellow[cCourseCount,cHoleCount].Text,cCourseCount+1,cHoleCount+1]);
                                                                               exit;
                                                                          end;
                                                                     end;
                                                            end;
                                                  C_BLUE:
                                                         begin
                                                              if bUseMetres then
                                                                 begin
                                                                      If TryStrToInt(edt_TeeBlue[cCourseCount,cHoleCount].Text,iTryInt) then
                                                                         TeeDistanceMetres:=StrToInt(edt_TeeBlue[cCourseCount,cHoleCount].Text)
                                                                      else
                                                                      begin
                                                                           ShowMessageFmt('Invalid Tee Distance "%s" metres value in Course %d, Blue Tees, Hole %d',[edt_TeeBlue[cCourseCount,cHoleCount].Text,cCourseCount+1,cHoleCount+1]);
                                                                           exit;
                                                                      end;
                                                                 end
                                                              else
                                                                  begin
                                                                       If TryStrToInt(edt_TeeBlue[cCourseCount,cHoleCount].Text,iTryInt) then
                                                                          TeeDistanceYards:=StrToInt(edt_TeeBlue[cCourseCount,cHoleCount].Text)
                                                                       else
                                                                       begin
                                                                            ShowMessageFmt('Invalid Tee Distance "%s" yards value in Course %d, Blue Tees, Hole %d',[edt_TeeBlue[cCourseCount,cHoleCount].Text,cCourseCount+1,cHoleCount+1]);
                                                                            exit;
                                                                       end;
                                                                  end;
                                                         end;
                                                  C_RED:
                                                         begin
                                                              if bUseMetres then
                                                                 begin
                                                                      If TryStrToInt(edt_TeeRed[cCourseCount,cHoleCount].Text,iTryInt) then
                                                                         TeeDistanceMetres:=StrToInt(edt_TeeRed[cCourseCount,cHoleCount].Text)
                                                                      else
                                                                      begin
                                                                           ShowMessageFmt('Invalid Tee Distance "%s" metres value in Course %d, Red Tees, Hole %d',[edt_TeeRed[cCourseCount,cHoleCount].Text,cCourseCount+1,cHoleCount+1]);
                                                                           exit;
                                                                      end;
                                                                 end
                                                              else
                                                                  begin
                                                                       If TryStrToInt(edt_TeeRed[cCourseCount,cHoleCount].Text,iTryInt) then
                                                                          TeeDistanceYards:=StrToInt(edt_TeeRed[cCourseCount,cHoleCount].Text)
                                                                       else
                                                                       begin
                                                                            ShowMessageFmt('Invalid Tee Distance "%s" yards value in Course %d, Red Tees, Hole %d',[edt_TeeRed[cCourseCount,cHoleCount].Text,cCourseCount+1,cHoleCount+1]);
                                                                            exit;
                                                                       end;
                                                                  end;
                                                         end;
                                             end;
                                        end;
                                    If (cNumHoles=18) then
                                    For cHoleCount:=9 to 17 do
                                        begin
                                             HoleIndex:=cHoleCount;
                                             Case cValidTeePositionCount of
                                                  C_GOLD:
                                                         begin
                                                              if bUseMetres then
                                                                 begin
                                                                      If TryStrToInt(edt_TeeGold[cCourseCount,cHoleCount].Text,iTryInt) then
                                                                         TeeDistanceMetres:=StrToInt(edt_TeeGold[cCourseCount,cHoleCount].Text)
                                                                      else
                                                                      begin
                                                                           ShowMessageFmt('Invalid Tee Distance "%s" metres value in Course %d, Gold Tees, Hole %d',[edt_TeeGold[cCourseCount,cHoleCount].Text,cCourseCount+1,cHoleCount+1]);
                                                                           exit;
                                                                      end;
                                                                 end
                                                              else
                                                                  begin
                                                                       If TryStrToInt(edt_TeeGold[cCourseCount,cHoleCount].Text,iTryInt) then
                                                                          TeeDistanceYards:=StrToInt(edt_TeeGold[cCourseCount,cHoleCount].Text)
                                                                       else
                                                                       begin
                                                                            ShowMessageFmt('Invalid Tee Distance "%s" yards value in Course %d, Gold Tees, Hole %d',[edt_TeeGold[cCourseCount,cHoleCount].Text,cCourseCount+1,cHoleCount+1]);
                                                                            exit;
                                                                       end;
                                                                  end;
                                                         end;
                                                  C_BLACK:
                                                         begin
                                                              if bUseMetres then
                                                                 begin
                                                                      If TryStrToInt(edt_TeeBlack[cCourseCount,cHoleCount].Text,iTryInt) then
                                                                         TeeDistanceMetres:=StrToInt(edt_TeeBlack[cCourseCount,cHoleCount].Text)
                                                                      else
                                                                      begin
                                                                           ShowMessageFmt('Invalid Tee Distance "%s" metres value in Course %d, Black Tees, Hole %d',[edt_TeeBlack[cCourseCount,cHoleCount].Text,cCourseCount+1,cHoleCount+1]);
                                                                           exit;
                                                                      end;
                                                                 end
                                                              else
                                                                  begin
                                                                       If TryStrToInt(edt_TeeBlack[cCourseCount,cHoleCount].Text,iTryInt) then
                                                                          TeeDistanceYards:=StrToInt(edt_TeeBlack[cCourseCount,cHoleCount].Text)
                                                                       else
                                                                       begin
                                                                            ShowMessageFmt('Invalid Tee Distance "%s" yards value in Course %d, Black Tees, Hole %d',[edt_TeeBlack[cCourseCount,cHoleCount].Text,cCourseCount+1,cHoleCount+1]);
                                                                            exit;
                                                                       end;
                                                                  end;
                                                         end;
                                                  C_WHITE:
                                                         begin
                                                              if bUseMetres then
                                                                 begin
                                                                      If TryStrToInt(edt_TeeWhite[cCourseCount,cHoleCount].Text,iTryInt) then
                                                                         TeeDistanceMetres:=StrToInt(edt_TeeWhite[cCourseCount,cHoleCount].Text)
                                                                      else
                                                                      begin
                                                                           ShowMessageFmt('Invalid Tee Distance "%s" metres value in Course %d, White Tees, Hole %d',[edt_TeeWhite[cCourseCount,cHoleCount].Text,cCourseCount+1,cHoleCount+1]);
                                                                           exit;
                                                                      end;
                                                                 end
                                                              else
                                                                  begin
                                                                       If TryStrToInt(edt_TeeWhite[cCourseCount,cHoleCount].Text,iTryInt) then
                                                                          TeeDistanceYards:=StrToInt(edt_TeeWhite[cCourseCount,cHoleCount].Text)
                                                                       else
                                                                       begin
                                                                            ShowMessageFmt('Invalid Tee Distance "%s" yards value in Course %d, White Tees, Hole %d',[edt_TeeWhite[cCourseCount,cHoleCount].Text,cCourseCount+1,cHoleCount+1]);
                                                                            exit;
                                                                       end;
                                                                  end;
                                                         end;
                                                  C_YELLOW:
                                                            begin
                                                                 if bUseMetres then
                                                                    begin
                                                                         If TryStrToInt(edt_TeeYellow[cCourseCount,cHoleCount].Text,iTryInt) then
                                                                            TeeDistanceMetres:=StrToInt(edt_TeeYellow[cCourseCount,cHoleCount].Text)
                                                                         else
                                                                         begin
                                                                              ShowMessageFmt('Invalid Tee Distance "%s" metres value in Course %d, Yellow Tees, Hole %d',[edt_TeeYellow[cCourseCount,cHoleCount].Text,cCourseCount+1,cHoleCount+1]);
                                                                              exit;
                                                                         end;
                                                                    end
                                                                 else
                                                                     begin
                                                                          If TryStrToInt(edt_TeeYellow[cCourseCount,cHoleCount].Text,iTryInt) then
                                                                             TeeDistanceYards:=StrToInt(edt_TeeYellow[cCourseCount,cHoleCount].Text)
                                                                          else
                                                                          begin
                                                                               ShowMessageFmt('Invalid Tee Distance "%s" yards value in Course %d, Yellow Tees, Hole %d',[edt_TeeYellow[cCourseCount,cHoleCount].Text,cCourseCount+1,cHoleCount+1]);
                                                                               exit;
                                                                          end;
                                                                     end;
                                                            end;
                                                  C_BLUE:
                                                         begin
                                                              if bUseMetres then
                                                                 begin
                                                                      If TryStrToInt(edt_TeeBlue[cCourseCount,cHoleCount].Text,iTryInt) then
                                                                         TeeDistanceMetres:=StrToInt(edt_TeeBlue[cCourseCount,cHoleCount].Text)
                                                                      else
                                                                      begin
                                                                           ShowMessageFmt('Invalid Tee Distance "%s" metres value in Course %d, Blue Tees, Hole %d',[edt_TeeBlue[cCourseCount,cHoleCount].Text,cCourseCount+1,cHoleCount+1]);
                                                                           exit;
                                                                      end;
                                                                 end
                                                              else
                                                                  begin
                                                                       If TryStrToInt(edt_TeeBlue[cCourseCount,cHoleCount].Text,iTryInt) then
                                                                          TeeDistanceYards:=StrToInt(edt_TeeBlue[cCourseCount,cHoleCount].Text)
                                                                       else
                                                                       begin
                                                                            ShowMessageFmt('Invalid Tee Distance "%s" yards value in Course %d, Blue Tees, Hole %d',[edt_TeeBlue[cCourseCount,cHoleCount].Text,cCourseCount+1,cHoleCount+1]);
                                                                            exit;
                                                                       end;
                                                                  end;
                                                         end;
                                                  C_RED:
                                                         begin
                                                              if bUseMetres then
                                                                 begin
                                                                      If TryStrToInt(edt_TeeRed[cCourseCount,cHoleCount].Text,iTryInt) then
                                                                         TeeDistanceMetres:=StrToInt(edt_TeeRed[cCourseCount,cHoleCount].Text)
                                                                      else
                                                                      begin
                                                                           ShowMessageFmt('Invalid Tee Distance "%s" metres value in Course %d, Red Tees, Hole %d',[edt_TeeRed[cCourseCount,cHoleCount].Text,cCourseCount+1,cHoleCount+1]);
                                                                           exit;
                                                                      end;
                                                                 end
                                                              else
                                                                  begin
                                                                       If TryStrToInt(edt_TeeRed[cCourseCount,cHoleCount].Text,iTryInt) then
                                                                          TeeDistanceYards:=StrToInt(edt_TeeRed[cCourseCount,cHoleCount].Text)
                                                                       else
                                                                       begin
                                                                            ShowMessageFmt('Invalid Tee Distance "%s" yards value in Course %d, Red Tees, Hole %d',[edt_TeeRed[cCourseCount,cHoleCount].Text,cCourseCount+1,cHoleCount+1]);
                                                                            exit;
                                                                       end;
                                                                  end;
                                                         end;
                                             end;
                                        end;
                               end;
                            Inc(cValidTeePositionCount,1);
                       end;
              end;
          end;
     end;
     Result:=True;
  EXCEPT
    On E : EConvertError do
      ShowMessage(C_ERRORAPOLOGY + 'Invalid value entered');
  END;
end;
(******************************************************************************)
procedure Tmainform.FormCreate(Sender: TObject);
Var
   BrowserPath,BrowserParams:String;
   s,sCode,sCountry:String;
   iCount:Integer;
   AmenetiesList:TSTRINGS;
   fAmenityType,fAmenetyValue:String;
begin
     // Get/Set the config file path
    {$IFDEF LINUX}
            ConfigFilePath:=TrimFilename(DelSpace(GetAppConfigFile(true)));
            //TRUE=/etc/golfmlcoursewriter.cfg
            //FALSE=/<username>/.config/golfmlcoursewriter.cfg
    {$ENDIF}
    {$IFDEF WINDOWS}
            ForceDirectoriesUTF8(WINDOWSCONFIGPATH);
            ConfigFilePath:=WINDOWSCONFIGPATH + 'GolfmlCourseWriter.cfg';
    {$ENDIF}
    INI:=TINIFile.Create(ConfigFilePath);
    INI.CacheUpdates:=False;

    // Set the Application Title
    {$IFDEF LINUX}
            Application.Title:=Application.Title + ' (Linux';
    {$ENDIF}
    {$IFDEF WINDOWS}
            Application.Title:=Application.Title + ' (Windows';
    {$ENDIF}
    {$ifdef CPU32}
            Application.Title:=Application.Title + ' 32-bit)';
    {$ENDIF}
    {$ifdef CPU64}
            Application.Title:=Application.Title + ' 64-bit)';
    {$ENDIF}
    // Set form title and icon
    Caption:=Application.Title;
    Icon:=Application.Icon;
    // Local initialisations
    PageLastIndex:=-1; // First increment is to Zero
    iNumCourses:=0; // 1-based local var
    // Initialise the golfml read/write class
    GolfmlClass:=TGolfmlClass.Create;
    If NOT GolfmlClass.InitOK then
       begin
          ShowMessage('Fatal error: Unable to initialise GolfML Class.' + LineEnding + 'Click OK to quit.');
          Halt;
       end;
    // Mode
    MakeXMLMode:=C_MODECOURSEONLY;
    mnu_modeCourseOnly.Checked:=True;
    // Country Code Combo Box
    cmb_CountryClubcountrycode.ItemIndex:=0;
    s:=cmb_CountryClubcountrycode.Items[cmb_CountryClubcountrycode.ItemIndex];
    sCode:=LeftStr(s,2);
    sCountry:=RightStr(s,Length(s)-4);
    edt_countryclubgpslat.Text:='0';
    edt_countryclubgpslong.Text:='0';
    fCountryClubCountryCode:=sCode;
    fCountryClubCountry:=sCountry;
    // Populate instructions label
    ShowInstructions;
    // Set up HTML Help system
    HTMLBrowserHelpViewer1.FindDefaultBrowser(BrowserPath,BrowserParams);
    HTMLBrowserHelpViewer1.BrowserPath:=BrowserPath;
    HTMLBrowserHelpViewer1.BrowserParams:=BrowserParams;
    // Write current values into config file
    INI.WriteString('config','AppVersion',C_VERSION);
    INI.WriteString('config','GolfmlClassVersion',GolfmlClass.Version);
    INI.WriteString('config','LastRun',FormatDateTime(LongDateFormat,Now));
    INI.WriteString('config','BrowserPath',BrowserPath);
    INI.WriteString('config','HelpAndUpdates',HTMLHelpDatabase1.BaseURL + 'CourseWriterHelp');
    INI.WriteString('config','Contact','minesadorada@charcodelvalle.com');
    // Country Codes. 1st-time run: Write entries.  Subsequent: Read entries
    If INI.ReadString('Country Codes','CountryCode0','unknown')='unknown' then
       begin
            For iCount:=0 to cmb_CountryClubcountrycode.Items.Count-1 do
            begin
                 INI.WriteString('Country Codes',Format('CountryCode%d',[iCount]),cmb_CountryClubcountrycode.Items[iCount]);
            end;
       end
       else
       begin
            cmb_CountryClubcountrycode.Items.Clear;
            iCount:=0;
            While (INI.ReadString('Country Codes',Format('CountryCode%d',[iCount]),'unknown') <> 'unknown') do
                  begin
                       cmb_CountryClubcountrycode.Items.Add(INI.ReadString('Country Codes',Format('CountryCode%d',[iCount]),'unknown'));
                       Inc(iCount,1);
                  end;
       end;
    // Ameneties. 1st-time run: Write entries.  Subsequent: Read entries
    AmenetiesList:=TStringList.Create;
    TRY
    If INI.ReadString('ameneties','amenity0','unknown')='unknown' then
    // No config file entries, so Read from CONST Array
       begin
            For iCount:=0 to C_MAXAMENITIES-1 do
            begin
                INI.WriteString('ameneties',Format('amenity%d',[iCount]),
                AmenetyDefaultCategories[iCount] + C_AMENETYDELIMITERCHAR + AmenetyDefaultValues[iCount]);
                AmenetiesList.Add(AmenetyDefaultValues[iCount]);
                AmenetyCategoriesArray[iCount]:=AmenetyDefaultCategories[iCount];
                AmenetyValuesArray[iCount]:=AmenetyDefaultValues[iCount];
            end;
       end
    else // Read from INI file
        begin
             // We have to make a string list then assign it to the group control Items
                For iCount:=0 to C_MAXAMENITIES-1 do
                begin
                     fAmenityType:='';fAmenetyValue:='';
                     s:=INI.ReadString('ameneties',Format('amenity%d',[iCount]),'unknown');
                     If (s <> 'unknown') then
                     begin // INI entry is valid
                          SplitAmenety(fAmenityType,fAmenetyValue,s);
                          AmenetyCategoriesArray[iCount]:=fAmenityType;
                          AmenetyValuesArray[iCount]:=fAmenetyValue;
                     end
                     else
                     begin // INI entry is invalid
                          AmenetyCategoriesArray[iCount]:=AmenetyDefaultCategories[iCount];
                          AmenetyValuesArray[iCount]:=AmenetyDefaultValues[iCount];
                     end;
                     AmenetiesList.Add(fAmenetyValue);
                end;
        end;
        grp_amenities.Items:=AmenetiesList;
        FINALLY
               FreeAndNil(AmenetiesList);
        END;
end;

procedure Tmainform.FormShow(Sender: TObject);
begin
    edt_CountryClubName.SetFocus;
end;

procedure tmainform.label12click(sender: tobject);
begin
  OpenURL('http://code.google.com/p/golfml/wiki/CourseWriterHelp');
end;

procedure Tmainform.mnu_fileImportGolfmlClick(Sender: TObject);
var s:String;
begin
TRY
   // Choose config directory (Windows) or /usr/share/doc/ directory (Linux) as default
   // Preserve user choices via the config file.
   s:=INI.ReadString('config','golfmlFilesLocation','unknown');
   If s='unknown' then
      begin
           {$IFDEF WINDOWS}
                   ForceDirectoriesUTF8(WINDOWSCOURSESPATH);
                   dlg_open.InitialDir:=WINDOWSCOURSESPATH;
           {$ENDIF}
           {$IFDEF LINUX}
                   ForceDirectoriesUTF8(LINUXCOURSESPATH);
                   dlg_open.InitialDir:=LINUXCOURSESPATH;
           {$ENDIF}
      end
      else
          dlg_open.InitialDir:=s;

   If dlg_open.Execute then
        begin
             GolfmlClass.CourseXMLPath:=dlg_open.Filename;
             DeleteAllCoursePages;
             if GolfmlClass.GetCourseInfoFromFile then
                begin
                     If ImportData = TRUE then // Returns FALSE if there was a problem
                        begin
                             ShowMessageFmt('%s%sloaded OK.%s%d course(s) imported.',
                             [ExtractFileName(GolfmlClass.CourseXMLPath),LineEnding,LineEnding,GolfmlClass.MaxCourseIndex+1]);
                             dlg_SaveAs.InitialDir:=ExtractFilePath(dlg_open.Filename);
                             dlg_SaveAs.Filename:=ExtractFilename(dlg_open.Filename);
                             INI.WriteString('config','golfmlFilesLocation',ExtractFilePath(dlg_open.Filename));
                        end
                     else
                         MessageDlg(C_ERRORAPOLOGY + 'The application was unable to import for editing the file:' + LineEnding + GolfmlClass.CourseXMLPath + ' successfully',mtError,[MBOK],0);
                end;
        end;
EXCEPT
      raise Exception.Create(C_ERRORAPOLOGY);
END;
end;

procedure Tmainform.mnu_filenewclubClick(Sender: TObject);
begin
     DeleteAllCoursePages;
end;

(******************************************************************************)
procedure Tmainform.mnu_helpaboutappClick(Sender: TObject);
Var s:String;
begin
     s:=Application.Title + LineEnding;
     s+='GUI: v' + C_VERSION + LineEnding;
     s+='GolfmlClass: v' + GolfmlClass.Version + LineEnding;
     s+=Format('Config file: %s%s%s',[ConfigFilePath,LineEnding,LineEnding]);
     s+='All code by minesadorada@charcodelvalle.com' + LineEnding;
     s+='Distribution: Creative Commons' + LineEnding;
     s+='Updates: http://code.google.com/p/golfml/' + LineEnding;
     s+='Press F1 at any time for online help';
     MessageDlg(s,mtInformation,[MBOK],0);
end;

procedure tmainform.mnu_helpaboutgolfmlclick(sender: tobject);
Var s:String;
begin
     s:='GolfML is a text-based human-readable file format used to exchange ';
     s+='golf data between applications and web sites. ' + LineEnding;
     s+='Golf course information such as scorecard and yardage books, ';
     s+='or players scores and statistics are the most common information carried by GolfML.' + LineEnding;
     s+='Website: http://code.google.com/p/golfml/wiki/Welcome';
     MessageDlg(s,mtInformation,[MBOK],0);
end;

procedure tmainform.mnu_modecourseonlyclick(sender: tobject);
begin
  MakeXMLMode:=C_MODECOURSEONLY;
end;

procedure tmainform.mnu_mode_coursestylesheetclick(sender: tobject);
begin
    MakeXMLMode:=C_MODECOURSESTYLESHEET;
end;

procedure tmainform.mnu_mode_playerscorecardclick(sender: tobject);
begin
      MakeXMLMode:=C_MODECOURSEPLAYERSTYLESHEET;
end;

(******************************************************************************)
(******************************************************************************)
procedure Tmainform.pagecontainerCloseTabClicked(Sender: TObject);
// TODO: Handle course deletions
// Enable pagecontainer.Options [nboShowCloseButtons]
Var
   aTabSheet:TTabSheet;
   iIndex:Integer;
begin
     aTabSheet:=Sender as TTabSheet;
     iIndex:=aTabSheet.tag;
     if iIndex=0 then exit;
     pagecontainer.ActivePage:=tab_countryclub;  // Switch to the Club page
     pages[iIndex-1].Destroy; // Destroy the Course page

end;

(******************************************************************************)
procedure Tmainform.pagecontainerPageChanged(Sender: TObject);
Var wCount:Word;
begin
     If (pagecontainer.ActivePage = tab_countryclub) then
        CourseIndex:=C_COUNTRYCLUBINDEX
     else
     begin
     For wCount:=0 to PageLastIndex do
       begin
           If (pagecontainer.ActivePage = pages[wCount]) then
              begin
                   CourseIndex:=wCount; // Sets the global
                   TTeeColour:=Gold;
                   TeeIndex:=Ord(TTeeColour);
                   Break;
              end;
       end;
     end;
     // ShowMessageFmt('CourseIndex=%d, TeeIndex:=%d',[CourseIndex,Ord(TeeColour)]);
end;
(******************************************************************************)
procedure Tmainform.cmd_closeClick(Sender: TObject);
begin
  Close;
end;

procedure Tmainform.cmb_CountryClubcountrycodeChange(Sender: TObject);
  Var s,sCode,sCountry:String;
begin
     s:=cmb_CountryClubcountrycode.Items[cmb_CountryClubcountrycode.ItemIndex];
     sCode:=LeftStr(s,2);
     fCountryClubCountryCode:=UPPERCase(sCode);
     sCountry:=RightStr(s,Length(s)-4);
     fCountryClubCountry:=sCountry;
end;
(******************************************************************************)
procedure Tmainform.cmd_makexmlClick(Sender: TObject);
const
  { for short 8.3 file names }
  ShortForbiddenChars : set of Char = [';', '=', '+', '<', '>', '|',
                                       '"', '[', ']', '\', '/', ''''];
  { for long file names }
  LongForbiddenChars  : set of Char = ['<', '>', '|', '"', '\', '/', ':', '*', '?'];
// Inline Function
   function TestFilename(Filename: String; islong: Boolean) : Boolean;
   var
      I: integer;
   begin
        Result := Filename <> '';
        if islong then
           begin
                for I := 1 to Length(Filename) do
                Result := Result and not (Filename[I] in LongForbiddenChars);
           end
           else
           begin
           for I := 1 to Length(Filename) do
               Result := Result and not (Filename[I] in ShortForbiddenChars);
           end;
   end;

Var
  s,sError:String;
  fTryFloat:Single;
  dtTryDate:TDateTime;
begin
  // Only continue if there is at least one course
  If PageLastIndex < 0 then
     begin
          s:='You have not added any Golf Courses to ' + fCountryClubName + ' yet!' + LineEnding + LineEnding;
          s+='Click the [' + cmd_newcourse.Caption + '] button to add one or more golf courses, ';
          s+='fill in all the data for the course(s) then try again.  You could also Import a course to edit via the File menu.';
          MessageDlg(s,mtError,[MBOK],0);
          Exit;
     end;


  GolfmlClass.Reset; // Initialise all data
  // Choose application directory (Windows) or /usr/share/doc/ directory (Linux) as default
  // Preserve user choices via the config file.
  s:=INI.ReadString('config','golfmlFilesLocation','unknown');
  If s='unknown' then
     begin
          {$IFDEF WINDOWS}
                  // Default is folder with write permissions
                  ForceDirectoriesUTF8('C:\GolfmlCourseWriter\');
                  dlg_SaveAs.InitialDir:='C:\GolfmlCourseWriter\';
          {$ENDIF}
          {$IFDEF LINUX}
                  ForceDirectoriesUTF8('/usr/share/doc/golfml');
                  dlg_SaveAs.InitialDir:='/usr/share/doc/golfml/';
          {$ENDIF}
     end
     else
         dlg_SaveAs.InitialDir:=s;
  If AssignData then // AssignData function only returns true when all inputs were OK
     begin
          // Make up a defailt filename based on the Club Name (first time save)
          If Length(dlg_SaveAs.Filename) = 0 then
             dlg_SaveAs.Filename:=LowerCase(DelSpace(GolfmlClass.ClubName)) + '.xml'
          else
             dlg_SaveAs.Filename:=ExtractFileName(dlg_SaveAs.Filename);
          // Validate the filename for illegal characters
          if NOT TestFilename(dlg_SaveAs.Filename,TRUE) then
             begin
                  MessageDlg('Suggested golfml filename "' + dlg_SaveAs.Filename + '" would not be a valid.  Please choose a better one in the following dialog.',
                  mtWarning,[MBOK],0);
                  dlg_SaveAs.Filename:='invalidfilename.xml';
             end;
          // Deal with 'Make course scorecard' mode
          // No need to specify the xsl filename - use the golfmlclass default 'coursescorecard.xsl'
          // GolfmlClass.ScoreCardXSL is available to set
          If MakeXMLMode = C_MODECOURSESTYLESHEET then
             // Suggest a default filename
             If Pos('scorecard_',dlg_SaveAs.Filename)=0 then
                dlg_SaveAs.Filename:='scorecard_' + dlg_SaveAs.Filename;
          // Deal with 'Make personalised player scorecard' mode
          If MakeXMLMode = C_MODECOURSEPLAYERSTYLESHEET then
             begin
                  // Suggest a default filename
                  If Pos('playerscorecard_',dlg_SaveAs.Filename)=0 then
                     dlg_SaveAs.Filename:='playerscorecard_' + dlg_SaveAs.Filename;

                  // Use config file values if available
                  // Player Name
                  sError:=INI.ReadString('player','name','unknown');
                  Repeat
                        s:=InputBox('Player name input','Type in the Player name',sError);
                        sError:='Please type a valid Player name';
                  until Length(s) > 0;
                  fPlayerName:=s;

                  // Player Date of Birth
                  sError:=INI.ReadString('player','dateofbirth',FormatDateTime(ShortDateFormat,Now));
                  Repeat
                          s:=InputBox('Player date of birth input','Type in the Player date of birth',sError);
                          sError:='It must be a valid date ' + ShortDateFormat + ' e.g. ' + FormatDateTime(ShortDateFormat,Now);
                  until TryStrToDate(s,dtTryDate)=TRUE;
                  fPlayerDateOfBirth:=DateToStr(dtTryDate);

                  // Player Handicap
                  sError:=INI.ReadString('player','handicap','36.0');
                  Repeat
                          s:=InputBox('Player Handicap','Type in the Player Handicap',sError);
                          sError:='It must be a valid handicap number e.g. 36.0  Try again.';
                  until TryStrToFloat(s,fTryFloat)=TRUE;
                  fPlayerHandicap:=fTryFloat;

                  // Player Gender
                  fPlayerGender:=INI.ReadString('player','gender','unknown');
                  If fPlayerGender='unknown' then
                     If MessageDlg(Format('Is %s a male player?',[fPlayerName]),mtConfirmation,[MBYES,MBNO],0)=mrYes then
                        fPlayerGender:='male'
                     else
                         fPlayerGender:='female';

                  // Course name
                  sError:=INI.ReadString('scorecard','coursename','Main Course');
                  Repeat
                        s:=InputBox('Course name input','Type in the name of the course',sError);
                        sError:='Please type a valid Course name';
                  until Length(s) > 0;
                  fScoreCardCourseName:=s;

                  // Tee Colour
                  sError:=INI.ReadString('scorecard','teecolour','yellow');
                  Repeat
                        s:=LowerCase(InputBox('Tee colour input','Type in the tee colour',sError));
                        sError:='Please type a valid tee colour (white, yellow, red etc)';
                  until (s='gold')
                  OR (s='black')
                  OR (s='white')
                  OR (s='yellow')
                  OR (s='blue')
                  OR (s='red');
                  fScoreCardTeeColour:=s;

                  // Assign validated values to to GolfmlClass
                  // No need to specify the xsl filename - use the golfmlclass default 'playercorecard.xsl'
                  // GolfmlClass.PlayerScoreCardXSL is available to set
                  GolfmlClass.ScoreCardPlayerName:=fPlayerName;
                  GolfmlClass.ScoreCardPlayerGender:=fPlayerGender;
                  GolfmlClass.ScoreCardPlayerDateOfBirth:=fPlayerDateOfBirth;
                  GolfmlClass.ScoreCardPlayerHandicap:=fPlayerHandicap;
                  GolfmlClass.ScoreCardTeeColour:=fScoreCardTeeColour;
                  GolfmlClass.ScoreCardCourseName:=fScoreCardCourseName;
                  GolfmlClass.ScoreCardCSS:=C_GOLFMLCSSFILE;
                  // Preserve them in the config file for next time
                  INI.WriteString('player','name',fPlayerName);
                  INI.WriteString('player','gender',fPlayerGender);
                  INI.WriteString('player','dateofbirth',fPlayerDateOfBirth);
                  INI.WriteString('player','handicap',Format('%0.1f',[fPlayerHandicap]));
                  INI.WriteString('player','LastModified',FormatDateTime(LongDateFormat,Now));
                  INI.WriteString('scorecard','coursename',fScoreCardCourseName);
                  INI.WriteString('scorecard','teecolour',fScoreCardTeeColour);
                  INI.WriteString('scorecard','coursescorecardstylesheet',GolfmlClass.ScoreCardXSL);
                  INI.WriteString('scorecard','playerscorecardstylesheet',GolfmlClass.PlayerScoreCardXSL);
                  INI.WriteString('scorecard','golfmlcss',C_GOLFMLCSSFILE);
             end;

          If dlg_SaveAs.Execute then
             begin
                  GolfmlClass.CourseXMLPath:=dlg_SaveAs.Filename;
                  If GolfmlClass.MakeGolfmlFile then
                     begin
                          // Preserve users choice of save location for future sessions
                          INI.WriteString('config','golfmlFilesLocation',ExtractFilePath(dlg_SaveAs.FileName));
                          If MakeXMLMode = C_MODECOURSESTYLESHEET then
                          begin
                               s:=Format('Note: To display as a scorecard, you will need the files%s"%s" and "%s" in the same folder as "%s"',
                               [LineEnding,GolfmlClass.ScoreCardXSL,C_GOLFMLCSSFILE,ExtractFileName(GolfmlClass.CourseXMLPath)]);
                               MessageDlg(s,mtInformation,[MBOK],0);
                          end;
                          If MakeXMLMode = C_MODECOURSEPLAYERSTYLESHEET then
                          begin
                               s:=Format('Note: To display as a scorecard, you will need the files%s"%s" and "%s" in the same folder as "%s"',
                               [LineEnding,GolfmlClass.PlayerScoreCardXSL,C_GOLFMLCSSFILE,ExtractFileName(GolfmlClass.CourseXMLPath)]);
                               MessageDlg(s,mtInformation,[MBOK],0);
                          end;
                          s:=Format('golfml file successfully created at%s"%s"%s Would you like to start again with a new Club?',[LineEnding,GolfmlClass.CourseXMLPath,LineEnding]);
                          If MessageDlg(s,mtConfirmation,[MBYES,MBNO],0)=mrYes then DeleteAllCoursePages;
                     end
                  else
                      ShowMessageFmt('%sUnable to create new golfml file%s"%s"',[C_ERRORAPOLOGY,LineEnding,GolfmlClass.CourseXMLPath])
             end
     else
         ShowMessage('Save golfml file cancelled');
     end;
end;
(******************************************************************************)
procedure Tmainform.cmd_newcourseClick(Sender: TObject);
begin
     If Sender=cmd_newcourse then
        // Warn user only if New Course button Pressed...
        If MessageDlg('Are you sure?  A new course cannot be deleted!',
        mtConfirmation,[MBYES,MBCANCEL],0) <> mrYes then exit;

    // Make everything on-the-fly using dynamic arrays
    TRY
    // SetLengths and control creation are possible sources of failure hence TRY block
       TRY
          SetLength(pages,PageLastIndex+2); // Increase array size
          pages[PageLastIndex+1]:=TTabSheet.Create(Nil); // Create a new page
          With pages[PageLastIndex+1] do
          begin // Set new page properties
                Caption:=Format('Course %d',[PageLastIndex+2]);
                tag:=PageLastIndex+2; // page[0].tag=1
                parent:=pagecontainer;
          end;
       FINALLY // Try to fail gracefully by only incrementing PageIndex if successful
               // Page created OK, so populate it.
               Inc(PageLastIndex); //first time - to Zero
               if PageLastIndex >= C_MAXCOURSES-1 then cmd_newcourse.Visible:=False;
               // C_MAXCOURSES can be as high as you like.  Current=6
               PopulateCoursePage; // Huge routine to add all the input controls on-the-fly
               pagecontainer.ActivePage:=pages[PageLastIndex]; // Switch to new course page
               CourseIndex:=PageLastIndex; // Set Global
               // Set the focus on the Course Name edit box (not when auto-adding via import)
               If Sender=cmd_newcourse then edt_courseName[PageLastIndex].SetFocus;
               iNumCourses:=PageLastIndex+1; // Set local 1-based var
               lbl_numCourses.Caption:=Format('Courses attached to %s: %d',
               [tab_countryclub.Caption,iNumCourses]);
       END;
    EXCEPT
      // Failed on making new course and/or controls! Fatal error.
      On E:Exception do
         begin
              ShowMessage
              (C_ERRORAPOLOGY + '  Unable to create a new course. Quitting.');
              HALT;
         end;
    END;
end;

procedure Tmainform.edt_countryclubmunicipalityEditingDone(Sender: TObject);
begin
    If Length(edt_countryclubmunicipality.Text) > 0 then
       fCountryClubMunicipality:=edt_countryclubmunicipality.Text
    else fCountryClubMunicipality:='';
end;

(******************************************************************************)
procedure Tmainform.edt_CountryClubNameEditingDone(Sender: TObject);
begin
    If Length(edt_CountryClubName.Text) > 0 then
       begin
            tab_countryclub.Caption:=edt_CountryClubName.Text;
            grp_countryclub.Caption:=edt_CountryClubName.Text;
       end
    else
        begin
            tab_countryclub.Caption:='My Golf Club';
            edt_CountryClubName.Text:='My Golf Club';
            grp_countryclub.Caption:='My Golf Club';
        end;
    lbl_numCourses.Caption:=Format('Courses attached to %s: %d',
    [tab_countryclub.Caption,iNumCourses]);
    fCountryClubName:=tab_countryclub.Caption;

end;

procedure Tmainform.edt_countryclubphoneEditingDone(Sender: TObject);
CONST C_PATTERN='[\-+]?([1-9][0-9]{0,2})(-[1-9][0-9]{0,4})?(-[1-9][0-9]{0,13})(-[0-9]+)?';
var
  r:tregexpr;
  s:String;
  sHelp:String;
begin
    sHelp:='The telephone number %s is in the wrong format for golfml.' + LineEnding;
    sHelp:=sHelp + 'It should be like one of the following examples:' + LineEnding;
    sHelp:=sHelp + '1-800-1234567 (country-area-subscriber)' + LineEnding;
	sHelp:=sHelp + '1-800-1234567-89 (country-area-subscriber-extension)' + LineEnding;
	sHelp:=sHelp + '+34-912345678	 (country-subscriber)' + LineEnding;
	sHelp:=sHelp + '0034-912345678	 (country-subscriber)' + LineEnding;
	sHelp:=sHelp + '34-912345678	 (country-subscriber)' + LineEnding;
	sHelp:=sHelp + '34-912345678-9   (country-subscriber-extension)';
    If Length(edt_countryclubphone.Text) > 0 then
       begin
            s:=edt_countryclubphone.Text;
            TRY
               r:=TRegExpr.Create;
               r.Expression:=C_PATTERN;
               r.ModifierI:=true;
               r.Exec(s);
               if r.MatchPos[0]>=0 then
                  fCountryClubPhone:=DelSpace(s)
                   //showmessage('success, found '+r.Match[0])
               else
                   MessageDlg(Format(sHelp,[s]),mtError,[MBOK],0);
            finally
               r.Free;
            end;
       end
     else fCountryClubPhone:='';


end;

procedure Tmainform.edt_countryclubpostcodeEditingDone(Sender: TObject);
begin
     If Length(edt_countryclubpostcode.Text) > 0 then
        fCountryClubPostCode:=edt_countryclubpostcode.Text
     else fCountryClubPostCode:='';
end;

procedure Tmainform.edt_countryclubstreetEditingDone(Sender: TObject);
begin
     If Length(edt_countryclubstreet.Text) > 0 then
        fCountryClubStreet:=edt_countryclubstreet.Text
     else fCountryClubStreet:='';
end;

procedure Tmainform.edt_countryclubregionEditingDone(Sender: TObject);
begin
     If Length(edt_countryclubregion.Text) > 0 then
        fCountryClubRegion:=edt_countryclubregion.Text
     else fCountryClubRegion:='';
end;

procedure Tmainform.edt_countryclubwebsiteEditingDone(Sender: TObject);
begin
     If Length(edt_countryclubwebsite.Text) > 0 then
        fCountryClubWebsite:=edt_countryclubwebsite.Text
     else fCountryClubWebsite:='';
end;

 (******************************************************************************)
procedure Tmainform.PopulateCoursePage;
// Whenever a new Course page is created, this routine
// populates it with controls from the open arrays
// Reference a control's properties by ctrlArray[CourseIndex,HoleIndex]
CONST C_TOP=160; // Top of Tee Grid
CONST C_TEEGRIDLEFT=14; // Left of Tee Grid
CONST C_TEEPOSITIONSPACING=45; // Horizontal Distance between Tee Colour labels
CONST C_VERTICALOFFSET=25; // Vertical Distance between Tee Colour labels
Var
  wCount:Word;
  wHalfFormWidth,wLeft,wTop,wWidth,wHeight,wVOffSet,wCurrentLeft:Word;
begin
    wHalfFormWidth:=MainForm.Width DIV 2;
    wCurrentLeft:=C_TEEGRIDLEFT;

    TRY
       // Hole labels
        wLeft:=wCurrentLeft;
        wTop:=C_TOP+25;
        wWidth:=56;
        wHeight:=22;
        wVOffSet:=C_VERTICALOFFSET;
       SetLength(lbl_hole,PageLastIndex+1);
    For wCount:=0 to 8 do
        begin
         lbl_hole[PageLastIndex,wCount]:=TLabel.Create(nil);
         With lbl_hole[PageLastIndex,wCount] do
              begin
               Caption:=Format('Hole %d',[Succ(wCount)]);
               Top:=wTop;
               Left:=wLeft;
               Height:=wHeight;
               Width:=wWidth;
               TabStop:=False;
               Autosize:=False;
               Font.Style:=[fsBold];
               parent:=pages[PageLastIndex];
               Inc(wTop,wVOffSet);
              end;
        end;
    Inc(wLeft,wHalfFormWidth);
    wTop:=C_TOP+25;
    For wCount:=9 to 17 do
        begin
         lbl_hole[PageLastIndex,wCount]:=TLabel.Create(nil);
         With lbl_hole[PageLastIndex,wCount] do
              begin
               Caption:=Format('Hole %d',[Succ(wCount)]);
               Top:=wTop;
               Left:=wLeft;
               Height:=wHeight;
               Width:=wWidth;
               TabStop:=False;
               Autosize:=False;
               Font.Style:=[fsBold];
               parent:=pages[PageLastIndex];
               Inc(wTop,wVOffSet);
              end;
        end;
    // Par Heading
     Inc(wCurrentLeft,56);
     wLeft:=wCurrentLeft; //70;
     wTop:=C_TOP;
     wWidth:=20;
     wHeight:=22;
     // Label for Par
     SetLength(lbl_ParInfo,PageLastIndex+1);
     For wCount:=0 to 1 do
     begin
          lbl_ParInfo[PageLastIndex,wCount]:=TLabel.Create(nil);
          With lbl_ParInfo[PageLastIndex,wCount] do
          begin
               Top:=wTop;
               Left:=wLeft;
               Caption:='Par';
               TabStop:=False;
               Font.Style:=[fsBold];
               parent:=pages[PageLastIndex];
          end;
          Inc(wLeft,wHalfFormWidth);
     end;
    // Par Edit Controls
    wLeft:=wCurrentLeft;
    wTop:=C_TOP+21;
    SetLength(edt_par,PageLastIndex+1);
    For wCount:=0 to 8 do
        begin
         edt_par[PageLastIndex,wCount]:=TEdit.Create(nil);
         With edt_par[PageLastIndex,wCount] do
              begin
               Caption:=Format('%d',[0]);
               Top:=wTop;
               Left:=wLeft;
               Height:=wHeight;
               Width:=wWidth;
               Autosize:=False;
               parent:=pages[PageLastIndex];
               Inc(wTop,wVOffSet);
              end;
        end;
    Inc(wLeft,wHalfFormWidth);
    wTop:=C_TOP+21;
    For wCount:=9 to 17 do
        begin
         edt_par[PageLastIndex,wCount]:=TEdit.Create(nil);
         With edt_par[PageLastIndex,wCount] do
              begin
               Caption:=Format('%d',[0]);
               Top:=wTop;
               Left:=wLeft;
               Height:=wHeight;
               Width:=wWidth;
               Autosize:=False;
               parent:=pages[PageLastIndex];
               Inc(wTop,wVOffSet);
              end;
        end;

    // SI Heading
     Inc(wCurrentLeft,25);
     wLeft:=wCurrentLeft; //95;
     wTop:=C_TOP;
     wWidth:=20;
     wHeight:=22;
     // Label for S.I.
     SetLength(lbl_SIInfo,PageLastIndex+1);
     For wCount:=0 to 1 do
     begin
          lbl_SIInfo[PageLastIndex,wCount]:=TLabel.Create(nil);
          With lbl_SIInfo[PageLastIndex,wCount] do
          begin
               Top:=wTop;
               Left:=wLeft;
               Caption:='S.I.';
               TabStop:=False;
               Font.Style:=[fsBold];
               parent:=pages[PageLastIndex];
          end;
          Inc(wLeft,wHalfFormWidth);
     end;
    // SI Edit Controls
    wLeft:=wCurrentLeft;
    wTop:=C_TOP+21;
    SetLength(edt_SI,PageLastIndex+1);
    For wCount:=0 to 8 do
        begin
         edt_SI[PageLastIndex,wCount]:=TEdit.Create(nil);
         With edt_SI[PageLastIndex,wCount] do
              begin
               Caption:=Format('%d',[0]);
               Top:=wTop;
               Left:=wLeft;
               Height:=wHeight;
               Width:=wWidth;
               Autosize:=False;
               parent:=pages[PageLastIndex];
               Inc(wTop,wVOffSet);
              end;
        end;
    Inc(wLeft,wHalfFormWidth);
    wTop:=C_TOP+21;
    For wCount:=9 to 17 do
        begin
         edt_SI[PageLastIndex,wCount]:=TEdit.Create(nil);
         With edt_SI[PageLastIndex,wCount] do
              begin
               Caption:=Format('%d',[0]);
               Top:=wTop;
               Left:=wLeft;
               Height:=wHeight;
               Width:=wWidth;
               Autosize:=False;
               parent:=pages[PageLastIndex];
               Inc(wTop,wVOffSet);
              end;
        end;

    // TEEGOLD
     Inc(wCurrentLeft,30);
     wLeft:=wCurrentLeft;//125;
     wTop:=C_TOP;
     wWidth:=30;
     wHeight:=22;
     // Label for TeeGold.
     SetLength(lbl_TeeGoldInfo,PageLastIndex+1);
     For wCount:=0 to 1 do
     begin
          lbl_TeeGoldInfo[PageLastIndex,wCount]:=TLabel.Create(nil);
          With lbl_TeeGoldInfo[PageLastIndex,wCount] do
          begin
               Top:=wTop;
               Left:=wLeft;
               Caption:='Gold';
               TabStop:=False;
               Color:=clOlive;
               Font.Color:=clWhite;
               parent:=pages[PageLastIndex];
          end;
          Inc(wLeft,wHalfFormWidth);
     end;
    // TEEGOLD Edit Controls
    wLeft:=wCurrentLeft;
    wTop:=C_TOP+21;
    SetLength(edt_TeeGold,PageLastIndex+1);
    For wCount:=0 to 8 do
        begin
         edt_TeeGold[PageLastIndex,wCount]:=TEdit.Create(nil);
         With edt_TeeGold[PageLastIndex,wCount] do
              begin
               Caption:=Format('%d',[0]);
               Top:=wTop;
               Left:=wLeft;
               Height:=wHeight;
               Width:=wWidth;
               Autosize:=False;
               Color:=clOlive;
               Font.Color:=clWhite;
               parent:=pages[PageLastIndex];
               Inc(wTop,wVOffSet);
              end;
        end;
    Inc(wLeft,wHalfFormWidth);
    wTop:=C_TOP+21;
    For wCount:=9 to 17 do
        begin
         edt_TeeGold[PageLastIndex,wCount]:=TEdit.Create(nil);
         With edt_TeeGold[PageLastIndex,wCount] do
              begin
               Caption:=Format('%d',[0]);
               Top:=wTop;
               Left:=wLeft;
               Height:=wHeight;
               Width:=wWidth;
               Autosize:=False;
               Color:=clOlive;
               Font.Color:=clWhite;
               parent:=pages[PageLastIndex];
               Inc(wTop,wVOffSet);
              end;
        end;

    // TEEBLACK
     Inc(wCurrentLeft,C_TEEPOSITIONSPACING);
     wLeft:=wCurrentLeft; //+35
     wTop:=C_TOP;
     wWidth:=30;
     wHeight:=22;
     // Label for TeeBlack.
     SetLength(lbl_TeeBlackInfo,PageLastIndex+1);
     For wCount:=0 to 1 do
     begin
          lbl_TeeBlackInfo[PageLastIndex,wCount]:=TLabel.Create(nil);
          With lbl_TeeBlackInfo[PageLastIndex,wCount] do
          begin
               Top:=wTop;
               Left:=wLeft;
               Caption:='Black';
               TabStop:=False;
               Color:=clBlack;
               Font.Color:=clWhite;
               parent:=pages[PageLastIndex];
          end;
          Inc(wLeft,wHalfFormWidth);
     end;
    // TeeBlack Edit Controls
    wLeft:=wCurrentLeft;
    wTop:=C_TOP+21;
    SetLength(edt_TeeBlack,PageLastIndex+1);
    For wCount:=0 to 8 do
        begin
         edt_TeeBlack[PageLastIndex,wCount]:=TEdit.Create(nil);
         With edt_TeeBlack[PageLastIndex,wCount] do
              begin
               Caption:=Format('%d',[0]);
               Top:=wTop;
               Left:=wLeft;
               Height:=wHeight;
               Width:=wWidth;
               Autosize:=False;
               Color:=clBlack;
               Font.Color:=clWhite;
               parent:=pages[PageLastIndex];
               Inc(wTop,wVOffSet);
              end;
        end;
    Inc(wLeft,wHalfFormWidth);
    wTop:=C_TOP+21;
    For wCount:=9 to 17 do
        begin
         edt_TeeBlack[PageLastIndex,wCount]:=TEdit.Create(nil);
         With edt_TeeBlack[PageLastIndex,wCount] do
              begin
               Caption:=Format('%d',[0]);
               Top:=wTop;
               Left:=wLeft;
               Height:=wHeight;
               Width:=wWidth;
               Autosize:=False;
               Color:=clBlack;
               Font.Color:=clWhite;
               parent:=pages[PageLastIndex];
               Inc(wTop,wVOffSet);
              end;
        end;

// START OF TEE COLOUR GRID

    // TEEWHITE
     Inc(wCurrentLeft,C_TEEPOSITIONSPACING);
     wLeft:=wCurrentLeft; //+35
     wTop:=C_TOP;
     wWidth:=30;
     wHeight:=22;
     // Label for TeeWhite.
     SetLength(lbl_TeeWhiteInfo,PageLastIndex+1);
     For wCount:=0 to 1 do
     begin
          lbl_TeeWhiteInfo[PageLastIndex,wCount]:=TLabel.Create(nil);
          With lbl_TeeWhiteInfo[PageLastIndex,wCount] do
          begin
               Top:=wTop;
               Left:=wLeft;
               Caption:='White';
               Color:=clWhite;
               TabStop:=False;
               Font.Color:=clBlack;
               parent:=pages[PageLastIndex];
          end;
          Inc(wLeft,wHalfFormWidth);
     end;
    // TeeWhite Edit Controls
    wLeft:=wCurrentLeft;
    wTop:=C_TOP+21;
    SetLength(edt_TeeWhite,PageLastIndex+1);
    For wCount:=0 to 8 do
        begin
         edt_TeeWhite[PageLastIndex,wCount]:=TEdit.Create(nil);
         With edt_TeeWhite[PageLastIndex,wCount] do
              begin
               Caption:=Format('%d',[0]);
               Top:=wTop;
               Left:=wLeft;
               Height:=wHeight;
               Width:=wWidth;
               Autosize:=False;
               Color:=clWhite;
               Font.Color:=clBlack;
               parent:=pages[PageLastIndex];
               Inc(wTop,wVOffSet);
              end;
        end;
    Inc(wLeft,wHalfFormWidth);
    wTop:=C_TOP+21;
    For wCount:=9 to 17 do
        begin
         edt_TeeWhite[PageLastIndex,wCount]:=TEdit.Create(nil);
         With edt_TeeWhite[PageLastIndex,wCount] do
              begin
               Caption:=Format('%d',[0]);
               Top:=wTop;
               Left:=wLeft;
               Height:=wHeight;
               Width:=wWidth;
               Autosize:=False;
               Color:=clWhite;
               Font.Color:=clBlack;
               parent:=pages[PageLastIndex];
               Inc(wTop,wVOffSet);
              end;
        end;

    // TEEYELLOW
     Inc(wCurrentLeft,C_TEEPOSITIONSPACING);
     wLeft:=wCurrentLeft; //+35
     wTop:=C_TOP;
     wWidth:=30;
     wHeight:=22;
     // Label for TeeYellow.
     SetLength(lbl_TeeYellowInfo,PageLastIndex+1);
     For wCount:=0 to 1 do
     begin
          lbl_TeeYellowInfo[PageLastIndex,wCount]:=TLabel.Create(nil);
          With lbl_TeeYellowInfo[PageLastIndex,wCount] do
          begin
               Top:=wTop;
               Left:=wLeft;
               Caption:='Yellow';
               TabStop:=False;
               Color:=clYellow;
               Font.Color:=clBlack;
               parent:=pages[PageLastIndex];
          end;
          Inc(wLeft,wHalfFormWidth);
     end;
    // TeeYellow Edit Controls
    wLeft:=wCurrentLeft;
    wTop:=C_TOP+21;
    SetLength(edt_TeeYellow,PageLastIndex+1);
    For wCount:=0 to 8 do
        begin
         edt_TeeYellow[PageLastIndex,wCount]:=TEdit.Create(nil);
         With edt_TeeYellow[PageLastIndex,wCount] do
              begin
               Caption:=Format('%d',[0]);
               Top:=wTop;
               Left:=wLeft;
               Height:=wHeight;
               Width:=wWidth;
               Autosize:=False;
               Color:=clYellow;
               Font.Color:=clBlack;
               parent:=pages[PageLastIndex];
               Inc(wTop,wVOffSet);
              end;
        end;
    Inc(wLeft,wHalfFormWidth);
    wTop:=C_TOP+21;
    For wCount:=9 to 17 do
        begin
         edt_TeeYellow[PageLastIndex,wCount]:=TEdit.Create(nil);
         With edt_TeeYellow[PageLastIndex,wCount] do
              begin
               Caption:=Format('%d',[0]);
               Top:=wTop;
               Left:=wLeft;
               Height:=wHeight;
               Width:=wWidth;
               Autosize:=False;
               Color:=clYellow;
               Font.Color:=clBlack;
               parent:=pages[PageLastIndex];
               Inc(wTop,wVOffSet);
              end;
        end;

    // TEEBLUE
     Inc(wCurrentLeft,C_TEEPOSITIONSPACING);
     wLeft:=wCurrentLeft; //+35
     wTop:=C_TOP;
     wWidth:=30;
     wHeight:=22;
     // Label for TeeBlue.
     SetLength(lbl_TeeBlueInfo,PageLastIndex+1);
     For wCount:=0 to 1 do
     begin
          lbl_TeeBlueInfo[PageLastIndex,wCount]:=TLabel.Create(nil);
          With lbl_TeeBlueInfo[PageLastIndex,wCount] do
          begin
               Top:=wTop;
               Left:=wLeft;
               Caption:='Blue';
               TabStop:=False;
               Color:=clNavy;
               Font.Color:=clWhite;
               parent:=pages[PageLastIndex];
          end;
          Inc(wLeft,wHalfFormWidth);
     end;
    // TeeBlue Edit Controls
    wLeft:=wCurrentLeft;
    wTop:=C_TOP+21;
    SetLength(edt_TeeBlue,PageLastIndex+1);
    For wCount:=0 to 8 do
        begin
         edt_TeeBlue[PageLastIndex,wCount]:=TEdit.Create(nil);
         With edt_TeeBlue[PageLastIndex,wCount] do
              begin
               Caption:=Format('%d',[0]);
               Top:=wTop;
               Left:=wLeft;
               Height:=wHeight;
               Width:=wWidth;
               Autosize:=False;
               Color:=clNavy;
               Font.Color:=clWhite;
               parent:=pages[PageLastIndex];
               Inc(wTop,wVOffSet);
              end;
        end;
    Inc(wLeft,wHalfFormWidth);
    wTop:=C_TOP+21;
    For wCount:=9 to 17 do
        begin
         edt_TeeBlue[PageLastIndex,wCount]:=TEdit.Create(nil);
         With edt_TeeBlue[PageLastIndex,wCount] do
              begin
               Caption:=Format('%d',[0]);
               Top:=wTop;
               Left:=wLeft;
               Height:=wHeight;
               Width:=wWidth;
               Autosize:=False;
               Color:=clNavy;
               Font.Color:=clWhite;
               parent:=pages[PageLastIndex];
               Inc(wTop,wVOffSet);
              end;
        end;

    // TEERED
     Inc(wCurrentLeft,C_TEEPOSITIONSPACING);
     wLeft:=wCurrentLeft; //+35
     wTop:=C_TOP;
     wWidth:=30;
     wHeight:=22;
     // Label for TeeRed.
     SetLength(lbl_TeeRedInfo,PageLastIndex+1);
     For wCount:=0 to 1 do
     begin
          lbl_TeeRedInfo[PageLastIndex,wCount]:=TLabel.Create(nil);
          With lbl_TeeRedInfo[PageLastIndex,wCount] do
          begin
               Top:=wTop;
               Left:=wLeft;
               Caption:='Red';
               TabStop:=False;
               Color:=clRed;
               Font.Color:=clWhite;
               parent:=pages[PageLastIndex];
          end;
          Inc(wLeft,wHalfFormWidth);
     end;
    // TeeRed Edit Controls
    wLeft:=wCurrentLeft;
    wTop:=C_TOP+21;
    SetLength(edt_TeeRed,PageLastIndex+1);
    For wCount:=0 to 8 do
        begin
         edt_TeeRed[PageLastIndex,wCount]:=TEdit.Create(nil);
         With edt_TeeRed[PageLastIndex,wCount] do
              begin
               Caption:=Format('%d',[0]);
               Top:=wTop;
               Left:=wLeft;
               Height:=wHeight;
               Width:=wWidth;
               Autosize:=False;
               Color:=clRed;
               Font.Color:=clWhite;
               parent:=pages[PageLastIndex];
               Inc(wTop,wVOffSet);
              end;
        end;
    Inc(wLeft,wHalfFormWidth);
    wTop:=C_TOP+21;
    For wCount:=9 to 17 do
        begin
         edt_TeeRed[PageLastIndex,wCount]:=TEdit.Create(nil);
         With edt_TeeRed[PageLastIndex,wCount] do
              begin
               Caption:=Format('%d',[0]);
               Top:=wTop;
               Left:=wLeft;
               Height:=wHeight;
               Width:=wWidth;
               Autosize:=False;
               Color:=clRed;
               Font.Color:=clWhite;
               parent:=pages[PageLastIndex];
               Inc(wTop,wVOffSet);
              end;
        end;
// END OF TEE COLOUR GRID

// Distance METRIC/YARDS RADIOGROUP (IN BOTTOM PANEL)
   SetLength(opt_units,PageLastIndex+1);
   opt_units[PageLastIndex]:=TRadioGroup.Create(nil);
   With opt_units[PageLastIndex] do
        begin
             Top:=414;
             Left:=C_TEEGRIDLEFT-10;
             Width:=150;
             Height:=50;
             Columns:=2;
             TabStop:=False;
             Caption:='Distance Units';
             Items.Add('Metres');
             Items.Add('Yards');
             ItemIndex:=0;
             Tag:=PageLastIndex;
             OnClick:=@Process_opt_units;
             parent:=pages[PageLastIndex];
        end;

        // 9 or 18 holes RADIOGROUP (IN BOTTOM PANEL)
   SetLength(opt_9or18,PageLastIndex+1);
   opt_9or18[PageLastIndex]:=TRadioGroup.Create(nil);
   With opt_9or18[PageLastIndex] do
        begin
             Top:=414;
             Left:=mainform.Width-155;
             {$IFDEF WINDOWS}
             Width:=140;
             {$ENDIF}
             {$IFDEF LINUX}
             Width:=140;
             {$ENDIF}
             Height:=50;
             Columns:=2;
             Caption:='Number of holes';
             Items.Add('18');
             Items.Add('9');
             ItemIndex:=0;
             Tag:=PageLastIndex;
             TabStop:=False;
             OnClick:=@Process_opt_9or18;
             parent:=pages[PageLastIndex];
        end;

        // Course name Label  (IN BOTTOM PANEL)
   SetLength(lbl_courseNameInfo,PageLastIndex+1);
   lbl_courseNameInfo[PageLastIndex]:=TLabel.Create(nil);
   With lbl_courseNameInfo[PageLastIndex] do
        begin
             Top:=435;
             Left:=C_TEEGRIDLEFT + 160;
             Height:=wHeight;
             Width:= wWidth;
             Caption:='Course Title:';
             TabStop:=False;
             parent:=pages[PageLastIndex];
        end;
        // Course Name Edit Box  (IN BOTTOM PANEL)
        SetLength(edt_courseName,PageLastIndex+1);
        edt_courseName[PageLastIndex]:=TEdit.Create(nil);
        With edt_courseName[PageLastIndex] do
        begin
             Top:=430;
             Left:=C_TEEGRIDLEFT + 240;
             Height:=wHeight;
             Width:= 200;
             Tag:=PageLastIndex; // Pass the CourseIndex along..
             OnEditingDone:=@Process_edt_courseName;
             parent:=pages[PageLastIndex];
        end;

// BEVEL FRAMES
   // Lower frame
   SetLength(teebevel,PageLastIndex+1);
   teebevel[PageLastIndex]:=TBevel.Create(nil);
   With teebevel[PageLastIndex] do
   begin
        Top:=C_TOP-10;
        Left:=C_TEEGRIDLEFT-10;
        TabStop:=False;
        Height:=(wVOffSet * 9) + 35;
        Width:= mainform.width - 10;
        parent:=pages[PageLastIndex];
   end;

   // Top Frame
   SetLength(topbevel,PageLastIndex+1);
   topbevel[PageLastIndex]:=TBevel.Create(nil);
   With topbevel[PageLastIndex] do
   begin
        Top:=C_TEEGRIDLEFT-10;
        Left:=C_TEEGRIDLEFT-10;
        TabStop:=False;
        Height:=C_TOP-10;
        Width:= mainform.width - 10;
        parent:=pages[PageLastIndex];
   end;

   // Top - Use Tee Colour checkboxes
   wTop:=C_TEEGRIDLEFT;
   wVOffSet:=20;
   SetLength(chk_useteecolour,PageLastIndex+1);
   For wCount:=C_GOLD to C_RED do
       begin
           chk_useteecolour[PageLastIndex,wCount]:=TCheckBox.Create(nil);
           With chk_useteecolour[PageLastIndex,wCount] do
           begin
                Top:=wTop;
                Left:=C_TEEGRIDLEFT;
                Autosize:=false;
                TabStop:=False;
                Width:=90;
                Height:=wHeight;
                Tag:=wCount; // Tee-Colour
                Caption:=Format('%s tees',[C_TEECOLOURSTRINGARRAY[wCount]]);
                State:=cbChecked;
                BiDiMode:=bdRightToLeft;
                // AllowGrayed:=TRUE;
                Checked:=True;
                Color:=C_TEEBACKCOLOURARRAY[wCount];
                Font.Color:=C_TEEFORECOLOURARRAY[wCount];
                OnClick:=@Process_chk_useteecolour;
                parent:=pages[PageLastIndex];
           end;
           Inc(wTop,wVOffSet);
       end;

   // Top - Course Rating label and edit
   wTop:=C_TEEGRIDLEFT+5;
   wVOffSet:=20;
   SetLength(lbl_CourseRating,PageLastIndex+1);
   For wCount:=C_GOLD to C_RED do
       begin
           lbl_CourseRating[PageLastIndex,wCount]:=TLabel.Create(nil);
           With lbl_CourseRating[PageLastIndex,wCount] do
           begin
              Top:=wTop;
              Left:=C_TEEGRIDLEFT + 100;
              Autosize:=false;
              TabStop:=False;
              Width:=120;
              Height:=wHeight;
              Tag:=wCount; // Tee-Colour
              Caption:='Course Rating:';
              //Format('%s tees Course Rating:',[C_TEECOLOURSTRINGARRAY[wCount]]);
              //Color:=C_TEEBACKCOLOURARRAY[wCount];
              //Font.Color:=C_TEEFORECOLOURARRAY[wCount];
              parent:=pages[PageLastIndex];
           end;
           Inc(wTop,wVOffSet);
       end;

   wTop:=C_TEEGRIDLEFT;
   wVOffSet:=20;
   SetLength(edt_CourseRating,PageLastIndex+1);
   For wCount:=C_GOLD to C_RED do
       begin
           edt_CourseRating[PageLastIndex,wCount]:=TEdit.Create(nil);
           With edt_CourseRating[PageLastIndex,wCount] do
           begin
              Top:=wTop;
              Left:=C_TEEGRIDLEFT + 185;
              Autosize:=false;
              TabStop:=True;
              Width:=wWidth+4;
              Height:=wHeight;
              Tag:=wCount; // Tee-Colour
              Text:='00.0';
              Color:=C_TEEBACKCOLOURARRAY[wCount];
              Font.Color:=C_TEEFORECOLOURARRAY[wCount];
              parent:=pages[PageLastIndex];
           end;
           Inc(wTop,wVOffSet);
       end;

   // Top - Slope Rating label and edit
    wTop:=C_TEEGRIDLEFT+5;
    wVOffSet:=20;
    SetLength(lbl_SlopeRating,PageLastIndex+1);
    For wCount:=C_GOLD to C_RED do
        begin
            lbl_SlopeRating[PageLastIndex,wCount]:=TLabel.Create(nil);
            With lbl_SlopeRating[PageLastIndex,wCount] do
            begin
               Top:=wTop;
               Left:=C_TEEGRIDLEFT + 230;
               Autosize:=false;
               TabStop:=False;
               Width:=120;
               Height:=wHeight;
               Tag:=wCount; // Tee-Colour
               Caption:='Slope Rating:';
               parent:=pages[PageLastIndex];
            end;
            Inc(wTop,wVOffSet);
        end;

    wTop:=C_TEEGRIDLEFT;
    wVOffSet:=20;
    SetLength(edt_SlopeRating,PageLastIndex+1);
    For wCount:=C_GOLD to C_RED do
        begin
            edt_SlopeRating[PageLastIndex,wCount]:=TEdit.Create(nil);
            With edt_SlopeRating[PageLastIndex,wCount] do
            begin
               Top:=wTop;
               Left:=C_TEEGRIDLEFT + 305;
               Autosize:=false;
               TabStop:=True;
               Width:=wWidth+4;
               Height:=wHeight;
               Tag:=wCount; // Tee-Colour
               Text:='000';
               Color:=C_TEEBACKCOLOURARRAY[wCount];
               Font.Color:=C_TEEFORECOLOURARRAY[wCount];
               parent:=pages[PageLastIndex];
            end;
            Inc(wTop,wVOffSet);
        end;

    // Top - Tee Title label and edit
    wTop:=C_TEEGRIDLEFT+5;
    wVOffSet:=20;
    SetLength(lbl_TeeTitle,PageLastIndex+1);
    For wCount:=C_GOLD to C_RED do
        begin
            lbl_TeeTitle[PageLastIndex,wCount]:=TLabel.Create(nil);
            With lbl_TeeTitle[PageLastIndex,wCount] do
            begin
               Top:=wTop;
               Left:=C_TEEGRIDLEFT + 350;
               Autosize:=false;
               TabStop:=False;
               Width:=90;
               Height:=wHeight;
               Tag:=wCount; // Tee-Colour
               Caption:='Tee Title:';
               parent:=pages[PageLastIndex];
            end;
            Inc(wTop,wVOffSet);
        end;

    wTop:=C_TEEGRIDLEFT;
    wVOffSet:=20;
    SetLength(edt_TeeTitle,PageLastIndex+1);
    For wCount:=C_GOLD to C_RED do
        begin
            edt_TeeTitle[PageLastIndex,wCount]:=TEdit.Create(nil);
            With edt_TeeTitle[PageLastIndex,wCount] do
            begin
               Top:=wTop;
               Left:=C_TEEGRIDLEFT + 400;
               Autosize:=false;
               TabStop:=True;
               Width:=100;
               Height:=wHeight;
               Tag:=wCount; // Tee-Colour
               Text:=C_TEECOLOURSTRINGARRAY[wCount];
               //Color:=C_TEEBACKCOLOURARRAY[wCount];
               //Font.Color:=C_TEEFORECOLOURARRAY[wCount];
               parent:=pages[PageLastIndex];
            end;
            Inc(wTop,wVOffSet);
        end;


    // Top - Tee Gender label and SpeedButtons
    wTop:=C_TEEGRIDLEFT+5;
    wVOffSet:=20;
    SetLength(lbl_TeeGender,PageLastIndex+1);
    For wCount:=C_GOLD to C_RED do
        begin
            lbl_TeeGender[PageLastIndex,wCount]:=TLabel.Create(nil);
            With lbl_TeeGender[PageLastIndex,wCount] do
            begin
               Top:=wTop;
               Left:=C_TEEGRIDLEFT + 510;
               Autosize:=false;
               TabStop:=False;
               Width:=90;
               Height:=wHeight;
               Tag:=wCount; // Tee-Colour
               Caption:='Gender:';
               parent:=pages[PageLastIndex];
            end;
            Inc(wTop,wVOffSet);
        end;

    wTop:=C_TEEGRIDLEFT;
    wVOffSet:=20;
    SetLength(spd_TeeGentlemen,PageLastIndex+1);
    For wCount:=C_GOLD to C_RED do
        begin
            spd_TeeGentlemen[PageLastIndex,wCount]:=TSpeedButton.Create(nil);
            With spd_TeeGentlemen[PageLastIndex,wCount] do
            begin
               Top:=wTop;
               Left:=C_TEEGRIDLEFT + 560;
               Autosize:=false;
               TabStop:=False;
               Width:=76;
               Height:=wHeight;
               Tag:=wCount; // Tee-Colour
               Caption:='Gentlemen';
               GroupIndex:=wCount+1;
               If wCount = C_RED then Down:=False else Down:=True;
               parent:=pages[PageLastIndex];
            end;
            Inc(wTop,wVOffSet);
        end;
    wTop:=C_TEEGRIDLEFT;
    wVOffSet:=20;
    SetLength(spd_TeeLadies,PageLastIndex+1);
    For wCount:=C_GOLD to C_RED do
        begin
            spd_TeeLadies[PageLastIndex,wCount]:=TSpeedButton.Create(nil);
            With spd_TeeLadies[PageLastIndex,wCount] do
            begin
               Top:=wTop;
               Left:=C_TEEGRIDLEFT + 636;
               Autosize:=false;
               TabStop:=False;
               Width:=48;
               Height:=wHeight;
               Tag:=wCount; // Tee-Colour
               Caption:='Ladies';
               GroupIndex:=wCount+1;
               If wCount = C_RED then Down:=True else Down:=false;
               parent:=pages[PageLastIndex];
            end;
            Inc(wTop,wVOffSet);
        end;

    EXCEPT
      On E:Exception do ShowMessage('Unable to create controls');
    END;
end;
(******************************************************************************)
procedure Tmainform.DisableTeePositionDisplay(Const iCourse,iTeePosition:Integer;Const Full18:Boolean);
Var wCount:Word;
begin
    If GolfmlClass.CourseLoaded then
       begin
            lbl_ParInfo[iCourse,0].Visible:=False;
            lbl_SIInfo[iCourse,0].Visible:=False;
            lbl_ParInfo[iCourse,1].Visible:=False;
            lbl_SIInfo[iCourse,1].Visible:=False;
       end;
    For wCount:=0 to 17 do
        begin
             If GolfmlClass.CourseLoaded then
             begin
                  lbl_hole[iCourse,wCount].Visible:=False;
                  edt_par[iCourse,wCount].Visible:=False;
                  edt_SI[iCourse,wCount].Visible:=False;
             end;
            Case iTeePosition of
                 C_GOLD:
                   begin
                        edt_TeeGold[iCourse,wCount].Visible:=False;
                        lbl_TeeGoldInfo[iCourse,0].Visible:=False;
                        lbl_TeeGoldInfo[iCourse,1].Visible:=False;
                        lbl_CourseRating[iCourse,C_GOLD].Visible:=False;
                        edt_CourseRating[iCourse,C_GOLD].Visible:=False;
                        lbl_SlopeRating[iCourse,C_GOLD].Visible:=False;
                        edt_SlopeRating[iCourse,C_GOLD].Visible:=False;
                        lbl_TeeTitle[iCourse,C_GOLD].Visible:=False;
                        edt_TeeTitle[iCourse,C_GOLD].Visible:=False;
                        lbl_TeeGender[iCourse,C_GOLD].Visible:=False;
                        spd_TeeGentlemen[iCourse,C_GOLD].Visible:=False;
                        spd_TeeLadies[iCourse,C_GOLD].Visible:=False;
                   end;
                 C_BLACK:
                   begin
                        edt_TeeBlack[iCourse,wCount].Visible:=False;
                        lbl_TeeBlackInfo[iCourse,0].Visible:=False;
                        lbl_TeeBlackInfo[iCourse,1].Visible:=False;
                        lbl_CourseRating[iCourse,C_BLACK].Visible:=False;
                        edt_CourseRating[iCourse,C_BLACK].Visible:=False;
                        lbl_SlopeRating[iCourse,C_BLACK].Visible:=False;
                        edt_SlopeRating[iCourse,C_BLACK].Visible:=False;
                        lbl_TeeTitle[iCourse,C_BLACK].Visible:=False;
                        edt_TeeTitle[iCourse,C_BLACK].Visible:=False;
                        lbl_TeeGender[iCourse,C_BLACK].Visible:=False;
                        spd_TeeGentlemen[iCourse,C_BLACK].Visible:=False;
                        spd_TeeLadies[iCourse,C_BLACK].Visible:=False;
                   end;
                 C_WHITE:
                   begin
                        edt_TeeWhite[iCourse,wCount].Visible:=False;
                        lbl_TeeWhiteInfo[iCourse,0].Visible:=False;
                        lbl_TeeWhiteInfo[iCourse,1].Visible:=False;
                        lbl_CourseRating[iCourse,C_WHITE].Visible:=False;
                        edt_CourseRating[iCourse,C_WHITE].Visible:=False;
                        lbl_SlopeRating[iCourse,C_WHITE].Visible:=False;
                        edt_SlopeRating[iCourse,C_WHITE].Visible:=False;
                        lbl_TeeTitle[iCourse,C_WHITE].Visible:=False;
                        edt_TeeTitle[iCourse,C_WHITE].Visible:=False;
                        lbl_TeeGender[iCourse,C_WHITE].Visible:=False;
                        spd_TeeGentlemen[iCourse,C_WHITE].Visible:=False;
                        spd_TeeLadies[iCourse,C_WHITE].Visible:=False;
                   end;
                 C_YELLOW:
                   begin
                        edt_TeeYellow[iCourse,wCount].Visible:=False;
                        lbl_TeeYellowInfo[iCourse,0].Visible:=False;
                        lbl_TeeYellowInfo[iCourse,1].Visible:=False;
                        lbl_CourseRating[iCourse,C_YELLOW].Visible:=False;
                        edt_CourseRating[iCourse,C_YELLOW].Visible:=False;
                        lbl_SlopeRating[iCourse,C_YELLOW].Visible:=False;
                        edt_SlopeRating[iCourse,C_YELLOW].Visible:=False;
                        lbl_TeeTitle[iCourse,C_YELLOW].Visible:=False;
                        edt_TeeTitle[iCourse,C_YELLOW].Visible:=False;
                        lbl_TeeGender[iCourse,C_YELLOW].Visible:=False;
                        spd_TeeGentlemen[iCourse,C_YELLOW].Visible:=False;
                        spd_TeeLadies[iCourse,C_YELLOW].Visible:=False;
                   end;
                 C_BLUE:
                   begin
                      edt_TeeBlue[iCourse,wCount].Visible:=False;
                      lbl_TeeBlueInfo[iCourse,0].Visible:=False;
                      lbl_TeeBlueInfo[iCourse,1].Visible:=False;
                      lbl_CourseRating[iCourse,C_BLUE].Visible:=False;
                      edt_CourseRating[iCourse,C_BLUE].Visible:=False;
                      lbl_SlopeRating[iCourse,C_BLUE].Visible:=False;
                      edt_SlopeRating[iCourse,C_BLUE].Visible:=False;
                      lbl_TeeTitle[iCourse,C_BLUE].Visible:=False;
                      edt_TeeTitle[iCourse,C_BLUE].Visible:=False;
                      lbl_TeeGender[iCourse,C_BLUE].Visible:=False;
                      spd_TeeGentlemen[iCourse,C_BLUE].Visible:=False;
                      spd_TeeLadies[iCourse,C_BLUE].Visible:=False;
                   end;
                 C_RED:
                   begin
                      edt_TeeRed[iCourse,wCount].Visible:=False;
                      lbl_TeeRedInfo[iCourse,0].Visible:=False;
                      lbl_TeeRedInfo[iCourse,1].Visible:=False;
                      lbl_CourseRating[iCourse,C_RED].Visible:=False;
                      edt_CourseRating[iCourse,C_RED].Visible:=False;
                      lbl_SlopeRating[iCourse,C_RED].Visible:=False;
                      edt_SlopeRating[iCourse,C_RED].Visible:=False;
                      lbl_TeeTitle[iCourse,C_RED].Visible:=False;
                      edt_TeeTitle[iCourse,C_RED].Visible:=False;
                      lbl_TeeGender[iCourse,C_RED].Visible:=False;
                      spd_TeeGentlemen[iCourse,C_RED].Visible:=False;
                      spd_TeeLadies[iCourse,C_RED].Visible:=False;
                   end;
            end;
        end;
end;
procedure Tmainform.EnableTeePositionDisplay(Const iCourse,iTeePosition:Integer;Const Full18:Boolean);
Var wCount:Word;
begin
    lbl_ParInfo[iCourse,0].Visible:=True;
    lbl_SIInfo[iCourse,0].Visible:=True;
    If Full18 then
       begin
            lbl_ParInfo[iCourse,1].Visible:=True;
            lbl_SIInfo[iCourse,1].Visible:=True;
       end;

    For wCount:=0 to 17 do
        begin
            If Full18 then
               begin
                    lbl_hole[iCourse,wCount].Visible:=True;
                    edt_par[iCourse,wCount].Visible:=True;
                    edt_SI[iCourse,wCount].Visible:=True;
               end
               else
               if (wCount < 9) then
                  begin
                     lbl_hole[iCourse,wCount].Visible:=True;
                     edt_par[iCourse,wCount].Visible:=True;
                     edt_SI[iCourse,wCount].Visible:=True;
                  end;
            Case iTeePosition of
                C_GOLD:
                  begin
                       if (Full18=TRUE) then // 18 holes
                          begin
                               lbl_TeeGoldInfo[iCourse,0].Visible:=True;
                               lbl_TeeGoldInfo[iCourse,1].Visible:=True;
                               edt_TeeGold[iCourse,wCount].Visible:=True;
                          end
                          else
                              if (wCount < 9) then
                                 begin
                                      lbl_TeeGoldInfo[iCourse,0].Visible:=True;
                                      edt_TeeGold[iCourse,wCount].Visible:=True;
                                 end;
                       lbl_CourseRating[iCourse,C_GOLD].Visible:=True;
                       edt_CourseRating[iCourse,C_GOLD].Visible:=True;
                       lbl_SlopeRating[iCourse,C_GOLD].Visible:=True;
                       edt_SlopeRating[iCourse,C_GOLD].Visible:=True;
                       lbl_TeeTitle[iCourse,C_GOLD].Visible:=True;
                       edt_TeeTitle[iCourse,C_GOLD].Visible:=True;
                       lbl_TeeGender[iCourse,C_GOLD].Visible:=True;
                       spd_TeeGentlemen[iCourse,C_GOLD].Visible:=True;
                       spd_TeeLadies[iCourse,C_GOLD].Visible:=True;
                  end;
                C_BLACK:
                  begin
                       if (Full18=TRUE) then // 18 holes
                          begin
                               lbl_TeeBlackInfo[iCourse,0].Visible:=True;
                               lbl_TeeBlackInfo[iCourse,1].Visible:=True;
                               edt_TeeBlack[iCourse,wCount].Visible:=True;
                          end
                          else
                              if (wCount < 9) then
                                 begin
                                      lbl_TeeBlackInfo[iCourse,0].Visible:=True;
                                      edt_TeeBlack[iCourse,wCount].Visible:=True;
                                 end;
                       lbl_CourseRating[iCourse,C_BLACK].Visible:=True;
                       edt_CourseRating[iCourse,C_BLACK].Visible:=True;
                       lbl_SlopeRating[iCourse,C_BLACK].Visible:=True;
                       edt_SlopeRating[iCourse,C_BLACK].Visible:=True;
                       lbl_TeeTitle[iCourse,C_BLACK].Visible:=True;
                       edt_TeeTitle[iCourse,C_BLACK].Visible:=True;
                       lbl_TeeGender[iCourse,C_BLACK].Visible:=True;
                       spd_TeeGentlemen[iCourse,C_BLACK].Visible:=True;
                       spd_TeeLadies[iCourse,C_BLACK].Visible:=True;
                  end;
                C_WHITE:
                  begin
                       if (Full18=TRUE) then // 18 holes
                          begin
                               lbl_TeeWhiteInfo[iCourse,0].Visible:=True;
                               lbl_TeeWhiteInfo[iCourse,1].Visible:=True;
                               edt_TeeWhite[iCourse,wCount].Visible:=True;
                          end
                          else
                              if (wCount < 9) then
                                 begin
                                      lbl_TeeWhiteInfo[iCourse,0].Visible:=True;
                                      edt_TeeWhite[iCourse,wCount].Visible:=True;
                                 end;
                       lbl_CourseRating[iCourse,C_WHITE].Visible:=True;
                       edt_CourseRating[iCourse,C_WHITE].Visible:=True;
                       lbl_SlopeRating[iCourse,C_WHITE].Visible:=True;
                       edt_SlopeRating[iCourse,C_WHITE].Visible:=True;
                       lbl_TeeTitle[iCourse,C_WHITE].Visible:=True;
                       edt_TeeTitle[iCourse,C_WHITE].Visible:=True;
                       lbl_TeeGender[iCourse,C_WHITE].Visible:=True;
                       spd_TeeGentlemen[iCourse,C_WHITE].Visible:=True;
                       spd_TeeLadies[iCourse,C_WHITE].Visible:=True;
                  end;
                C_YELLOW:
                  begin
                       if (Full18=TRUE) then // 18 holes
                          begin
                               lbl_TeeYellowInfo[iCourse,0].Visible:=True;
                               lbl_TeeYellowInfo[iCourse,1].Visible:=True;
                               edt_TeeYellow[iCourse,wCount].Visible:=True;
                          end
                          else
                              if (wCount < 9) then
                                 begin
                                      lbl_TeeYellowInfo[iCourse,0].Visible:=True;
                                      edt_TeeYellow[iCourse,wCount].Visible:=True;
                                 end;
                       lbl_CourseRating[iCourse,C_YELLOW].Visible:=True;
                       edt_CourseRating[iCourse,C_YELLOW].Visible:=True;
                       lbl_SlopeRating[iCourse,C_YELLOW].Visible:=True;
                       edt_SlopeRating[iCourse,C_YELLOW].Visible:=True;
                       lbl_TeeTitle[iCourse,C_YELLOW].Visible:=True;
                       edt_TeeTitle[iCourse,C_YELLOW].Visible:=True;
                       lbl_TeeGender[iCourse,C_YELLOW].Visible:=True;
                       spd_TeeGentlemen[iCourse,C_YELLOW].Visible:=True;
                       spd_TeeLadies[iCourse,C_YELLOW].Visible:=True;
                  end;
                C_BLUE:
                  begin
                       if (Full18=TRUE) then // 18 holes
                          begin
                               lbl_TeeBlueInfo[iCourse,0].Visible:=True;
                               lbl_TeeBlueInfo[iCourse,1].Visible:=True;
                               edt_TeeBlue[iCourse,wCount].Visible:=True;
                          end
                          else
                              if (wCount < 9) then
                                 begin
                                      lbl_TeeBlueInfo[iCourse,0].Visible:=True;
                                      edt_TeeBlue[iCourse,wCount].Visible:=True;
                                 end;
                     lbl_CourseRating[iCourse,C_BLUE].Visible:=True;
                     edt_CourseRating[iCourse,C_BLUE].Visible:=True;
                     lbl_SlopeRating[iCourse,C_BLUE].Visible:=True;
                     edt_SlopeRating[iCourse,C_BLUE].Visible:=True;
                     lbl_TeeTitle[iCourse,C_BLUE].Visible:=True;
                     edt_TeeTitle[iCourse,C_BLUE].Visible:=True;
                     lbl_TeeGender[iCourse,C_BLUE].Visible:=True;
                     spd_TeeGentlemen[iCourse,C_BLUE].Visible:=True;
                     spd_TeeLadies[iCourse,C_BLUE].Visible:=True;
                  end;
                C_RED:
                  begin
                       if (Full18=TRUE) then // 18 holes
                          begin
                               lbl_TeeRedInfo[iCourse,0].Visible:=True;
                               lbl_TeeRedInfo[iCourse,1].Visible:=True;
                               edt_TeeRed[iCourse,wCount].Visible:=True;
                          end
                          else
                              if (wCount < 9) then
                                 begin
                                      lbl_TeeRedInfo[iCourse,0].Visible:=True;
                                      edt_TeeRed[iCourse,wCount].Visible:=True;
                                 end;
                     lbl_CourseRating[iCourse,C_RED].Visible:=True;
                     edt_CourseRating[iCourse,C_RED].Visible:=True;
                     lbl_SlopeRating[iCourse,C_RED].Visible:=True;
                     edt_SlopeRating[iCourse,C_RED].Visible:=True;
                     lbl_TeeTitle[iCourse,C_RED].Visible:=True;
                     edt_TeeTitle[iCourse,C_RED].Visible:=True;
                     lbl_TeeGender[iCourse,C_RED].Visible:=True;
                     spd_TeeGentlemen[iCourse,C_RED].Visible:=True;
                     spd_TeeLadies[iCourse,C_RED].Visible:=True;
                  end;
           end;
        end;
end;
procedure Tmainform.Process_chk_useteecolour(Sender: TObject);
// Turns tee color controls on or off
Var
  tempCheckBox:TCheckBox;
  tempColour:Cardinal; // Range C_GOLD to C_RED
  wTeeCount,wCount:Word;
  bAtLeastOneChecked,bFull18:Boolean;
Begin
     If GolfmlClass.CourseLoaded=True then Exit;
    // The Tag property is the TeeColour (C_GOLD to C_RED)
     tempCheckBox:=Sender as TCheckBox;

     // Make sure that at least one Tee Colour is checked for this course
    bAtLeastOneChecked:=False;
    For wTeeCount:=C_GOLD to C_RED do
    begin
        If (chk_useteecolour[CourseIndex,wTeeCount].Checked = True) then
        begin
           bAtLeastOneChecked:=True;
           Break;
        end;
    end;

    If NOT bAtLeastOneChecked then
       begin
            tempCheckBox.Checked:=True; // re-check the tee position
            Application.ProcessMessages; // Allow it to draw
            MessageDlg('You must have at least one Tee Position on this course!',mtWarning,[MBOK],0);
            Exit;
       end;
     if opt_9or18[CourseIndex].ItemIndex=0 then bFull18:=TRUE else bFull18:=FALSE;
     tempColour:=tempCheckBox.Tag;
     If tempCheckBox.Checked then // Use this Tee Colour
        EnableTeePositionDisplay(CourseIndex,tempColour,bFull18)
     else DisableTeePositionDisplay(CourseIndex,tempColour,bFull18);
end;
(******************************************************************************)
procedure Tmainform.Process_edt_courseName(Sender: TObject);
// Sets Course Name
Var tempEdit:TEdit;
  begin
       // The Tag property is the current Course Index
       tempEdit:=Sender as TEdit; // Cast an edit control to get to the properties
       If Length(tempEdit.Text)=0 then tempEdit.Text:=Format('Course %d',[tempEdit.Tag + 1]);
       pages[tempEdit.Tag].Caption:=tempEdit.Text; // Page Caption
       // Do we need to increase the dynamic array size?
       If (High(fCourseArray) < (tempEdit.Tag)) then
          SetLength(fCourseArray,tempEdit.Tag+1);
          // Assign to the global array;
       fCourseArray[tempEdit.Tag]:=tempEdit.Text;
  end;
(******************************************************************************)
procedure Tmainform.Process_opt_units(Sender: TObject);
// Converts Units metres -> Yards and Yards -> Metres
Var
  tempRadioGroup:TRadioGroup;
  tempCourseIndex:Word;
  wHoleCount:Word;
  cTempCardinal:Cardinal;
  iTryInt:Integer;
begin
     If GolfmlClass.CourseLoaded then Exit;
     tempRadioGroup:=Sender as TRadioGroup;
     tempCourseIndex:=tempRadioGroup.Tag;
     If tempRadioGroup.ItemIndex = 1 then
     begin // Convert Metres to Yards
           For wHoleCount:=0 to 17 do
               begin
                    If TryStrToInt(edt_TeeGold[tempCourseIndex,wHoleCount].Text,iTryInt) then
                       If iTryInt > 0 then
                          edt_TeeGold[tempCourseIndex,wHoleCount].Text:=IntToStr(TRUNC((iTryInt * 1.0936133) + 0.5));
                    If TryStrToInt(edt_TeeBlack[tempCourseIndex,wHoleCount].Text,iTryInt) then
                       If iTryInt > 0 then
                          edt_TeeBlack[tempCourseIndex,wHoleCount].Text:=IntToStr(TRUNC((iTryInt * 1.0936133) + 0.5));
                    If TryStrToInt(edt_TeeWhite[tempCourseIndex,wHoleCount].Text,iTryInt) then
                       If iTryInt > 0 then
                          edt_TeeWhite[tempCourseIndex,wHoleCount].Text:=IntToStr(TRUNC((iTryInt * 1.0936133) + 0.5));
                    If TryStrToInt(edt_TeeYellow[tempCourseIndex,wHoleCount].Text,iTryInt) then
                       If iTryInt > 0 then
                          edt_TeeYellow[tempCourseIndex,wHoleCount].Text:=IntToStr(TRUNC((iTryInt * 1.0936133) + 0.5));
                    If TryStrToInt(edt_TeeBlue[tempCourseIndex,wHoleCount].Text,iTryInt) then
                       If iTryInt > 0 then
                          edt_TeeBlue[tempCourseIndex,wHoleCount].Text:=IntToStr(TRUNC((iTryInt * 1.0936133) + 0.5));
                    If TryStrToInt(edt_TeeRed[tempCourseIndex,wHoleCount].Text,iTryInt) then
                       If iTryInt > 0 then
                          edt_TeeRed[tempCourseIndex,wHoleCount].Text:=IntToStr(TRUNC((iTryInt * 1.0936133) + 0.5));
               end;
     end
     else
     begin // Convert Yards to Metres
           For wHoleCount:=0 to 17 do
               begin
                    If TryStrToInt(edt_TeeGold[tempCourseIndex,wHoleCount].Text,iTryInt) then
                       If iTryInt > 0 then
                          edt_TeeGold[tempCourseIndex,wHoleCount].Text:=IntToStr(TRUNC((iTryInt * 0.9144) + 0.5));
                    If TryStrToInt(edt_TeeBlack[tempCourseIndex,wHoleCount].Text,iTryInt) then
                       If iTryInt > 0 then
                          edt_TeeBlack[tempCourseIndex,wHoleCount].Text:=IntToStr(TRUNC((iTryInt * 0.9144) + 0.5));
                    If TryStrToInt(edt_TeeWhite[tempCourseIndex,wHoleCount].Text,iTryInt) then
                       If iTryInt > 0 then
                          edt_TeeWhite[tempCourseIndex,wHoleCount].Text:=IntToStr(TRUNC((iTryInt * 0.9144) + 0.5));
                    If TryStrToInt(edt_TeeYellow[tempCourseIndex,wHoleCount].Text,iTryInt) then
                       If iTryInt > 0 then
                          edt_TeeYellow[tempCourseIndex,wHoleCount].Text:=IntToStr(TRUNC((iTryInt * 0.9144) + 0.5));
                    If TryStrToInt(edt_TeeBlue[tempCourseIndex,wHoleCount].Text,iTryInt) then
                       If iTryInt > 0 then
                          edt_TeeBlue[tempCourseIndex,wHoleCount].Text:=IntToStr(TRUNC((iTryInt * 0.9144) + 0.5));
                    If TryStrToInt(edt_TeeRed[tempCourseIndex,wHoleCount].Text,iTryInt) then
                       If iTryInt > 0 then
                          edt_TeeRed[tempCourseIndex,wHoleCount].Text:=IntToStr(TRUNC((iTryInt * 0.9144) + 0.5));
               end;
     end;

end;
(******************************************************************************)
procedure Tmainform.Process_opt_9or18(Sender: TObject);
// Toggles Visibility of Holes 9 to 18 for this course
Var
  tempRadioGroup:TRadioGroup;
  tempCourseIndex:Word;
  wCount:Word;
begin
     If GolfmlClass.CourseLoaded then Exit;
     tempRadioGroup:=Sender as TRadioGroup;
     tempCourseIndex:=tempRadioGroup.Tag;
     If tempRadioGroup.ItemIndex = 1 then
     begin
          lbl_ParInfo[tempCourseIndex,1].Visible:=False;
          lbl_SIInfo[tempCourseIndex,1].Visible:=False;
          lbl_TeeGoldInfo[tempCourseIndex,1].Visible:=False;
          lbl_TeeBlackInfo[tempCourseIndex,1].Visible:=False;
          lbl_TeeWhiteInfo[tempCourseIndex,1].Visible:=False;
          lbl_TeeYellowInfo[tempCourseIndex,1].Visible:=False;
          lbl_TeeBlueInfo[tempCourseIndex,1].Visible:=False;
          lbl_TeeRedInfo[tempCourseIndex,1].Visible:=False;
        For wCount:=9 to 17 do
        begin
             lbl_hole[tempCourseIndex,wCount].Visible:=False;
             edt_par[tempCourseIndex,wCount].Visible:=False;
             edt_SI[tempCourseIndex,wCount].Visible:=False;
             edt_TeeGold[tempCourseIndex,wCount].Visible:=False;
             edt_TeeBlack[tempCourseIndex,wCount].Visible:=False;
             edt_TeeWhite[tempCourseIndex,wCount].Visible:=False;
             edt_TeeYellow[tempCourseIndex,wCount].Visible:=False;
             edt_TeeBlue[tempCourseIndex,wCount].Visible:=False;
             edt_TeeRed[tempCourseIndex,wCount].Visible:=False;
        end;
     end
     else
     begin
         lbl_ParInfo[tempCourseIndex,1].Visible:=True;
         lbl_SIInfo[tempCourseIndex,1].Visible:=True;
         if chk_useteecolour[CourseIndex,C_GOLD].checked=True then
            lbl_TeeGoldInfo[tempCourseIndex,1].Visible:=True;
         if chk_useteecolour[CourseIndex,C_BLACK].checked=True then
            lbl_TeeBlackInfo[tempCourseIndex,1].Visible:=True;
         if chk_useteecolour[CourseIndex,C_WHITE].checked=True then
            lbl_TeeWhiteInfo[tempCourseIndex,1].Visible:=True;
         if chk_useteecolour[CourseIndex,C_YELLOW].checked=True then
            lbl_TeeYellowInfo[tempCourseIndex,1].Visible:=True;
         if chk_useteecolour[CourseIndex,C_BLUE].checked=True then
            lbl_TeeBlueInfo[tempCourseIndex,1].Visible:=True;
         if chk_useteecolour[CourseIndex,C_RED].checked=True then
            lbl_TeeRedInfo[tempCourseIndex,1].Visible:=True;
         For wCount:=9 to 17 do
         begin
             lbl_hole[tempCourseIndex,wCount].Visible:=True;
             edt_par[tempCourseIndex,wCount].Visible:=True;
             edt_SI[tempCourseIndex,wCount].Visible:=True;
             if chk_useteecolour[CourseIndex,C_GOLD].checked=True then
                edt_TeeGold[tempCourseIndex,wCount].Visible:=True;
             if chk_useteecolour[CourseIndex,C_BLACK].checked=True then
                edt_TeeBlack[tempCourseIndex,wCount].Visible:=True;
             if chk_useteecolour[CourseIndex,C_WHITE].checked=True then
                edt_TeeWhite[tempCourseIndex,wCount].Visible:=True;
             if chk_useteecolour[CourseIndex,C_YELLOW].checked=True then
                edt_TeeYellow[tempCourseIndex,wCount].Visible:=True;
             if chk_useteecolour[CourseIndex,C_BLUE].checked=True then
                edt_TeeBlue[tempCourseIndex,wCount].Visible:=True;
             if chk_useteecolour[CourseIndex,C_RED].checked=True then
                edt_TeeRed[tempCourseIndex,wCount].Visible:=True;
        end;
     end;
end;
(******************************************************************************)
Procedure Tmainform.ShowInstructions;
var s:String;
begin
     // grp_Instructions.Caption:='Getting started with ' + Application.Title;
     s:='Welcome to ' + Application.Title + '.';
     s+='  Its purpose is to create a standard golfml xml data file for a club and its courses.' + CR;
     s+='1 ) Fill in all the information you can on this page about the Club.' + CR;
     s+='2 ) Each club has one or more golf courses attached to it. ';
     s+='For each course, click the [' + cmd_newcourse.Caption + '] button, and ';
     s+='you will see a course input page. ';
     s+='Note: Once a new course is added, it cannot be deleted.' + CR;
     s+='3 ) Type in the name of the course, and then use the tick boxes at the top left ';
     s+='to select ONLY the tee positions that exist in your course.' + CR;
     s+='At the bottom, choose the distance units and also the number of holes for that course' + CR;
     s+='4 ) Fill in ALL the information for the course, for each hole and tee position.' + CR;
     s+='5 ) Use the [' + cmd_makexml.Caption + '] button to make the new Golfml file ';
     s+='when you are done.' + CR;
//     s+='Note1: You can turn the hints on or off using the Help menu "Show Hints"' + CR;
     s+='Tip: You can use the Tabs at the top at any time to flip between the courses and this page.' + CR;
     s+='Tip: Use the TAB key to advance to the next field quickly when entering data.';

     lbl_Instructions.Caption:=s;
end;
(******************************************************************************)

end.


