unit ugolfmlclass;

(*
== Author: minesadorada@charcodelvalle.com
==
== Lazarus: 1.1
== FPC: 2.6.1
==
== golfml XML Reader and Writer class
==
== Version History
== 1.0
==    Writer built and tested with GUI
== 2.0
==    Reader built and tested with GUI
== 2.1
==    Added <summary> to <tee-set>
== 2.2
==    Bugfix for <amenety> tag handling
== 2.3
==    Added styleshheet embedding via XML ProcessingInstruction
==    Added <player> tag handling
== 2.4
==    Added custom <application> section
== 2.5
==    Bugfix for bad scope
== 2.6
==    Added GPS Lat and Long
== 2.7
==    Added ErrorCode,ErrorString
==    Added FileExists check on loading XML file
==    Added default values for some missing XML elements
==
==
== 2.11
== Subclass THCPClass (thandicapclassunit.pas)
== Note InitOK function (Reset method) also calls inherited function
*)
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, XMLRead, XMLWrite, DOM,thandicapclassunit;
//  Forms, Controls, Graphics, Dialogs, Buttons, // DEBUGGING ONLY
//  StdCtrls, ComCtrls, ExtCtrls; // DEBUGGING ONLY

const
  C_GOLFMLCLASSVERSION = '2.11.20130318.0';

const
  C_METERS = True; // fMetric

const
  C_YARDS = False; // fMetric

const
  C_AMENETYDELIMITER = ','; // Used in SplitAmenity

const
  C_ERRORXMLREAD = 'An error has occurred in reading this golfml file.' + LineEnding;

const
  C_SCORECARDXSL = 'coursescorecard.xsl';

const
  C_PLAYERSCORECARDXSL = 'playerscorecard.xsl';

// ERROR CODE CONSTANTS FOR ErrorCode property (fErrorCode)
// and ErrorString property (fErrorString)
// Add more Error Codes and Error Strings as needed in Get/Set routines
CONST C_ERROR_NOERROR = 0;

CONST C_ERROR_BADPATH = 1;
CONST C_ERRORSTRING_BADPATH = 'Bad CourseXMLPath';

type
  TAmenetyType = (practice, store, food, corporate, golfers, bathroom, water, other);
// Unused V1.0

type
  {
  // Use this if you do not need the Handicap adjustment code
  TGolfmlClass = class(TObject) // A Subclass of the generic TObject
  }
  TGolfmlClass = class(THCPClass) // A Subclass of the THandicapClass
  private
    // These 3 indexing vars allow us to index into the dynamic arrays
    // to pull out properties as single values
    fCourseIndex: cardinal;
    fTeeColourIndex: cardinal;
    fHoleIndex: cardinal;
    // Read properties to show maximum index values after a golfml file has been imported
    fMaxCourseIndex: cardinal; // Set in priv_TestAndSetCourseArraySizes
    fMaxTeeColourIndex: cardinal; // Set in priv_SetCourseIndex

    // Country Club
    fCountryClubName: string; // Property ClubName
    fCountryClubCountry: string; // Property ClubCountry
    fCountryClubCountryCode: string; // Property ClubCountryCode
    fCountryClubMunicipality: string; // Property ClubMunicipality
    fCountryClubRegion: string; // Property ClubRegion
    fCountryClubStreet: string; // Property ClubStreet
    fCountryClubPostCode: string; // Property ClubPostCode
    fCountryClubWebsite: string; // Property ClubWebsite
    fCountryClubPhone: string; // Property ClubPhone
    // Amenities and Comments are stored in dynamic arrays
    fCountryClubAmenetiesIndex: cardinal;
    fCountryClubCommentsIndex: cardinal;
    fMaxCountryClubAmenetiesIndex: cardinal;
    fMaxCountryClubCommentsIndex: cardinal;
    fCountryClubAmenetiesArray: array of string; // Indexed Property ClubAmeneties
    fCountryClubCommentsArray: array of string; // Indexed Property ClubComments
    fCountryClubGPSLatitude: single;
    fCountryClubGPSLongitude: single;

    // For Each Country Club
    fCourseNameArray: array of string;
    fCourseNumberOfHolesArray: array of cardinal;
    fCourseTotalParArray: array of cardinal;


    // For Each [Course] In Country Club
    fCourseTeeColourArray: array of array of string; // Tee Colour Names
    fCourseTeeTitleArray: array of array of string; // Tee Title Names
    fCourseTeeGenderArray: array of array of string; // Tee Gender Names

    fCourseParArray: array of array[0..17] of cardinal;  // Course Hole Pars
    fCourseStrokeIndexArray: array of array[0..17] of cardinal; // Course Hole SI's

    // For Each [Tee Colour] In [Course] in Country Club
    fTeeColourCourseRatingArray: array of array of single; // Tee Colour Course Ratings
    fTeeColourSlopeRatingArray: array of array of cardinal; // Tee Colour Slope Ratings
    fTotalTeeDistance: cardinal; // Property TotalTeeDistance
    fTotalTeeDistancemetres, fTotalTeeDistanceyards: cardinal;
    fInTeeDistance: cardinal; // Property InTeeDistance
    fInTeeDistancemetres, fInTeeDistanceyards: cardinal;
    fOutTeeDistance: cardinal; // Property OutTeeDistance
    fOutTeeDistancemetres, fOutTeeDistanceyards: cardinal;

    // For Each [Tee] in [Tee Colour] In [Course] in Country Club
    fTeeDistanceArrayMeters: array of array of array[0..17] of cardinal; //Tee Distances
    fTeeDistanceArrayYards: array of array of array[0..17] of cardinal; // Tee Distances

    // Vars used in property read values
    fCourseNumberOfHoles: cardinal; // Property CourseNumberOfHoles
    fCourseCount: cardinal; // Property CourseCount
    fTeeColourCount: cardinal; // Property TeeColourCount
    fMetric: boolean; // Property Metric
    fCoursePar: cardinal; // Property Par;
    fStrokeIndex: cardinal; // Property Strokeindex
    fTeeDistanceMetres, fTeeDistanceYards: cardinal; // Property Distance
    fCourseTotalPar: cardinal; // Property CourseTotalPar
    fSlopeRating: cardinal;
    fCourseRating: single;
    fCourseXMLPath: string;
    fCourseLoaded: boolean;
    fCourseName: string;
    fTeeTitle, fTeeColour, fTeeGender: string;

    fIncludeScoreCardStylesheetReference: boolean;
    fIncludePlayerScoreCardStylesheetReference: boolean;
    fScoreCardPlayerName: string;
    fScoreCardPlayerGender: string;
    fScoreCardPlayerDateOfBirth: string;
    fScoreCardPlayerHandicap: single;
    fScoreCardXSL: string;
    fPlayerScoreCardXSL: string;
    fScoreCardTeeColour: string;
    fScoreCardCourseName: string;
    fScoreCardCSS: string;

    fErrorCode:Cardinal; // Initially=0
    fErrorString:String; // Initially Empty String

    fAmenityType, fAmenetyValue: string; // Only used in local procedure SplitAmenety
    procedure SplitAmenety(aString: string); // Uses C_AMENETYDELIMITER


    // These 2 procedures resize arrays on demand
    procedure priv_TestAndSetTeeColourArraySizes(aCourseIndex: cardinal;
      aTeeColourIndex: cardinal);
    procedure priv_TestAndSetCourseArraySizes(aCourseIndex: cardinal);

    function priv_PrettyString(aString: string): string; // Capitalises first letter
    // Various Sub-procedures for parsing Golfml document elements
    procedure priv_ProcessXMLTee(aNode: TDomNode; TeeColour: cardinal; sCourseName: string);
    procedure priv_ProcessXMLQualificationUSGA(aNode: TDomNode;
      TeeColour: cardinal; sCourseName: string);
    procedure priv_ProcessXMLQualification(aNode: TDomNode; TeeColour: cardinal;
      sCourseName: string);
    procedure priv_ProcessXMLTeeSet(aNode: TDomNode; TeeColour: cardinal;
      sCourseName: string);
    procedure priv_ProcessXMLGolfCourse(aNode: TDomNode);
    procedure priv_ProcessXMLCountryClub(aNode: TDomNode);
    procedure priv_ProcessXMLCountryClubAddress(aNode: TDomNode);
    procedure priv_ProcessXMLCountryClubContact(aNode: TDomNode);
    procedure priv_ProcessXMLCountryClubAmenety(aNode: TDomNode);
    procedure priv_ProcessXMLCountryClubNote(aNode: TDomNode);
    procedure priv_ProcessXMLCountryClubPosition(aNode: TDomNode);
    procedure priv_ProcessNoTeeSetsInCourse(cCourseIndex: cardinal);
    // Called from priv_SetCurrentCourse


    // GETS and SETS
    procedure priv_SetCourseXMLPath(aString: string);
    procedure priv_SetCourseIndex(aCardinal: cardinal);
    procedure priv_SetTeeColourIndex(aCardinal: cardinal);
    procedure priv_SetHoleIndex(aCardinal: cardinal);
    procedure priv_SetMetric(aBoolean: boolean);

    function priv_GetCourseName: string;
    procedure priv_SetCourseName(aString: string);

    procedure priv_SetCoursePar(aCardinal: cardinal);
    function priv_GetCoursePar: cardinal;

    procedure priv_SetCourseNumberOfHoles(aCardinal: cardinal);
    function priv_GetCourseNumberOfHoles: cardinal;

    procedure priv_SetStrokeIndex(aCardinal: cardinal);
    function priv_GetStrokeIndex: cardinal;

    procedure priv_SetCourseRating(aSingle: single);
    function priv_GetCourseRating: single;

    procedure priv_SetSlopeRating(aCardinal: cardinal);
    function priv_GetSlopeRating: cardinal;

    procedure priv_SetTeeDistanceMetres(aCardinal: cardinal);
    function priv_GetTeeDistanceMetres: cardinal;

    procedure priv_SetTeeDistanceYards(aCardinal: cardinal);
    function priv_GetTeeDistanceYards: cardinal;

    function priv_GetTeeTitle: string;
    procedure priv_SetTeeTitle(aString: string);

    function priv_GetTeeColour: string;
    procedure priv_SetTeeColour(aString: string);

    function priv_GetTeeGender: string;
    procedure priv_SetTeeGender(aString: string);

    procedure priv_SetClubName(aString: string);
    procedure priv_SetClubCountry(aString: string);
    procedure priv_SetClubCountryCode(aString: string);
    procedure priv_SetClubMunicipality(aString: string);
    procedure priv_SetClubRegion(aString: string);
    procedure priv_SetClubStreet(aString: string);
    procedure priv_SetClubPostCode(aString: string);
    procedure priv_SetClubWebsite(aString: string);
    procedure priv_SetClubPhone(aString: string);
    procedure priv_SetClubGPSLongitude(aSingle: single);
    procedure priv_SetClubGPSLatitude(aSingle: single);


    function priv_GetClubAmeneties(iIndex: integer): string;
    procedure priv_SetClubAmeneties(const iIndex: integer; aString: string);

    function priv_GetClubComments(iIndex: integer): string;
    procedure priv_SetClubComments(const iIndex: integer; aString: string);

    procedure priv_SetTotalTeeDistance;
    // Refreshes property TotalTeeDistance (fTotalTeeDistance)
    function priv_GetBogeyRating: single; // From courserating and sloperating

    function priv_GetMaxTeeColourIndex: cardinal;
    function priv_GetMaxCourseIndex: cardinal;

    procedure priv_SetIncludeScoreCardStylesheetReference(aBoolean: boolean);
    procedure priv_SetIncludePlayerScoreCardStylesheetReference(aBoolean: boolean);
    procedure priv_SetScoreCardPlayerName(aString: string);
    procedure priv_SetScoreCardPlayerGender(aString: string);
    procedure priv_SetScoreCardPlayerDateOfBirth(aString: string);
    procedure priv_SetScoreCardPlayerHandicap(aSingle: single);
    procedure priv_SetPlayerScoreCardXSL(aString: string);
    procedure priv_SetScoreCardXSL(aString: string);
    procedure priv_SetScoreCardTeeColour(aString: string);
    procedure priv_SetScoreCardCourseName(aString: string);

  protected
    // PROTECTED VARS AND METHODS FOR SUBCLASS USE
    // *********************************************************************
    pVersion: string; // Version String
    function prot_InitObj: boolean; override; // Initialises variables and arrays etc.
    // Uncomment if descending from TObject
  public
    // PUBLIC VARS METHODS AND PROPERTIES
    constructor Create; // Note no override for Constructor
    destructor Destroy; override;
    function InitOK: boolean; // If myObj.InitOK then proceed...
    procedure Reset; // Calls prot_InitObj

    // The main methods
    function MakeGolfmlFile: boolean;
    function GetCourseInfoFromFile: boolean; // Implements virtual method
    // Returns ErrorCode=C_ERROR_BADPATH and ErrorString=C_ERRORSTRING_BADPATH if path invalid;

    // Indexed properties cannot be published in a visual component
    property ClubAmeneties[iIndex: integer]: string
      read priv_GetClubAmeneties write priv_SetClubAmeneties;
    property ClubComments[iIndex: integer]: string
      read priv_GetClubComments write priv_SetClubComments;

  published
    // All these could be on a visual component
    // *********************************************************************
    // Read-Write Properties
    // *********************************************************************
    property CourseName: string read priv_GetCourseName write priv_SetCourseName;

    property CourseRating: single read priv_GetCourseRating write priv_SetCourseRating;
    property BogeyRating: single read priv_GetBogeyRating;
    property SlopeRating: cardinal read priv_GetSlopeRating write priv_SetSlopeRating;
    property CourseNumberOfHoles: cardinal read priv_GetCourseNumberOfHoles
      write priv_SetCourseNumberOfHoles;
    property CourseXMLPath: string read fCourseXMLPath write priv_SetCourseXMLPath;
    // Fileexists(CourseXMLPath) is not checked when getting or setting this property

    property CourseLoaded: boolean read fCourseLoaded write fCourseLoaded;

    property ClubName: string read fCountryClubName write priv_SetClubName;
    property ClubCountry: string read fCountryClubCountry write priv_SetClubCountry;
    property ClubCountryCode: string read fCountryClubCountryCode
      write priv_SetClubCountryCode;
    property ClubMunicipality: string read fCountryClubMunicipality
      write priv_SetClubMunicipality;
    property ClubRegion: string read fCountryClubRegion write priv_SetClubRegion;
    property ClubStreet: string read fCountryClubStreet write priv_SetClubStreet;
    property ClubPostCode: string read fCountryClubPostCode write priv_SetClubPostCode;
    property ClubWebsite: string read fCountryClubWebsite write priv_SetClubWebsite;
    property ClubPhone: string read fCountryClubPhone write priv_SetClubPhone;
    property ClubGPSLongitude: single read fCountryClubGPSLongitude
      write priv_SetClubGPSLongitude;
    property ClubGPSLatitude: single read fCountryClubGPSLatitude
      write priv_SetClubGPSLatitude;
    // Change courses by changing the CourseIndex property (0-based)
    property CourseIndex: cardinal read fCourseIndex write priv_SetCourseIndex;
    // Change Tee sets by changing the TeeColourIndex property (0-based)
    property TeeColourIndex: cardinal read fTeeColourIndex write priv_SetTeeColourIndex;
    // Change Hole by changing the HoleIndex property (0-based)
    property HoleIndex: cardinal read fHoleIndex write priv_SetHoleIndex; // (0-17)



    property Metric: boolean read fMetric write priv_SetMetric;
    // Affects Distance properties
    property TeeDistanceMetres: cardinal read priv_GetTeeDistanceMetres
      write priv_SetTeeDistanceMetres;
    property TeeDistanceYards: cardinal read priv_GetTeeDistanceYards
      write priv_SetTeeDistanceYards;
    property TeeTitle: string read priv_GetTeeTitle write priv_SetTeeTitle;
    property TeeColour: string read priv_GetTeeColour write priv_SetTeeColour;
    property TeeGender: string read priv_GetTeeGender write priv_SetTeeGender;

    property Par: cardinal read priv_GetCoursePar write priv_SetCoursePar;
    property StrokeIndex: cardinal read priv_GetStrokeIndex write priv_SetStrokeIndex;
    property IncludeScoreCardStylesheetReference: boolean
      read fIncludeScoreCardStylesheetReference
      write priv_SetIncludeScoreCardStylesheetReference;
    property IncludePlayerScoreCardStylesheetReference: boolean
      read fIncludePlayerScoreCardStylesheetReference
      write priv_SetIncludePlayerScoreCardStylesheetReference;
    property ScoreCardPlayerName: string read fScoreCardPlayerName
      write priv_SetScoreCardPlayerName;
    property ScoreCardPlayerGender: string read fScoreCardPlayerGender
      write priv_SetScoreCardPlayerGender;
    property ScoreCardPlayerDateOfBirth: string
      read fScoreCardPlayerDateOfBirth write priv_SetScoreCardPlayerDateOfBirth;
    property ScoreCardPlayerHandicap: single
      read fScoreCardPlayerHandicap write priv_SetScoreCardPlayerHandicap;
    property ScoreCardTeeColour: string read fScoreCardTeeColour
      write priv_SetScoreCardTeeColour;
    property ScoreCardCourseName: string read fScoreCardCourseName
      write priv_SetScoreCardCourseName;
    property ScoreCardXSL: string read fScoreCardXSL write priv_SetScoreCardXSL;
    property PlayerScoreCardXSL: string read fPlayerScoreCardXSL
      write priv_SetPlayerScoreCardXSL;
    property ScoreCardCSS: string read fScoreCardCSS write fScoreCardCSS;

    // *********************************************************************
    // Read-only Properties
    // *********************************************************************
    property Version: string read pVersion;
    // Set after a golfml file has been read from file
    property InTeeDistance: cardinal read fInTeeDistance;
    // fMetric determines metres or yards
    property OutTeeDistance: cardinal read fOutTeeDistance;
    // fMetric determines metres or yards
    property TotalTeeDistance: cardinal read fTotalTeeDistance;
    // fMetric determines metres or yards
    property CourseTotalPar: cardinal read fCourseTotalPar;
    // MaxIndex properties help when setting up a loop to read the indexed property values
    // e.g. For i:=0 to MaxCourseIndex do begin CourseIndex:=i; sCourseListItem:=CourseName; end;
    property MaxCourseIndex: cardinal read priv_GetMaxCourseIndex;
    property MaxTeeColourIndex: cardinal read priv_GetMaxTeeColourIndex;
    property ClubMaxAmenitiesIndex: cardinal read fMaxCountryClubAmenetiesIndex;
    property ClubMaxCommentsIndex: cardinal read fMaxCountryClubCommentsIndex;

    // Error Conditions
    property ErrorCode:Cardinal read fErrorCode; // Initially 0
    property ErrorString:String read fErrorString; // Initially Empty String;
  end;

implementation
// *********************************************************************
constructor TGolfmlClass.Create;
begin
  // First we call ancestor.Create code
  inherited Create;
  // Create class objects etc here...
end;
// *********************************************************************
destructor TGolfmlClass.Destroy;
begin
  // First do any tidy-up code before calling the inherited Destroy
  // Free class objects here...
  inherited Destroy;
end;

function TGolfmlClass.prot_InitObj: boolean;
  // PROTECTED
  // Do initialisation stuff here to reset the
  // vars/properties etc. to their default values/states
  // All dynamic arrays are sized to 1 element (element #0)
  // to ensure properties don't fail when accessed with no XML data
var
  cCourseCount, cTeeCount, cHoleCount, cCount: cardinal;
  s: string;
begin
  Inherited;
  SetLength(fCountryClubAmenetiesArray, 1);
  SetLength(fCountryClubCommentsArray, 1);

  SetLength(fCourseNameArray, 1);
  SetLength(fCourseNumberOfHolesArray, 1);
  SetLength(fCourseTotalParArray, 1);

  SetLength(fCourseTeeColourArray, 1);
  SetLength(fCourseTeeColourArray[0], 1);
  SetLength(fCourseTeeTitleArray, 1);
  SetLength(fCourseTeeTitleArray[0], 1);
  SetLength(fCourseTeeGenderArray, 1);
  SetLength(fCourseTeeGenderArray[0], 1);

  SetLength(fCourseParArray, 1);
  SetLength(fCourseStrokeIndexArray, 1);

  SetLength(fTeeColourCourseRatingArray, 1);
  SetLength(fTeeColourCourseRatingArray[0], 1);
  SetLength(fTeeColourSlopeRatingArray, 1);
  SetLength(fTeeColourSlopeRatingArray[0], 1);

  SetLength(fTeeDistanceArrayMeters, 1);
  SetLength(fTeeDistanceArrayMeters[0], 1);
  SetLength(fTeeDistanceArrayYards, 1);
  SetLength(fTeeDistanceArrayYards[0], 1);


  fCourseNameArray[0] := '';
  fCourseNumberOfHolesArray[0] := 0;
  fCourseTotalParArray[0] := 0;
  fCourseTeeColourArray[0, 0] := '';
  fCourseTeeTitleArray[0, 0] := '';
  fCourseTeeGenderArray[0, 0] := '';
  fTeeColourCourseRatingArray[0, 0] := 0; // Course 0, TeeColour 0
  fTeeColourSlopeRatingArray[0, 0] := 0; // Course 0, TeeColour 0
  fCountryClubAmenetiesArray[0] := '';
  fCountryClubCommentsArray[0] := '';


  for cHoleCount := 0 to 17 do
  begin
    fCourseParArray[0, cHoleCount] := 0;
    fCourseStrokeIndexArray[0, cHoleCount] := 0;
    fTeeDistanceArrayMeters[0, 0, cHoleCount] := 0;
    fTeeDistanceArrayYards[0, 0, cHoleCount] := 0;
  end;


  pVersion := C_GOLFMLCLASSVERSION;
  fCourseIndex := 0;
  fTeeColourIndex := 0;
  fMaxCourseIndex := 0;
  fMaxTeeColourIndex := 0;
  fHoleIndex := 0;
  fCourseLoaded := False;
  fInTeeDistance := 0;
  fOutTeeDistance := 0;
  fTotalTeeDistance := 0;

  fTotalTeeDistanceMetres := 0;
  fTotalTeeDistanceYards := 0;
  fInTeeDistanceMetres := 0;
  fInTeeDistanceYards := 0;
  fOutTeeDistanceMetres := 0;
  fOutTeeDistanceYards := 0;
  fCountryClubAmenetiesIndex := 0;
  fCountryClubCommentsIndex := 0;
  fMaxCountryClubAmenetiesIndex := 0;
  fMaxCountryClubCommentsIndex := 0;
  fCountryClubGPSLatitude := 0;
  fCountryClubGPSLongitude := 0;


  fCountryClubName := '';
  fCountryClubCountry := '';
  fCountryClubCountryCode := '';
  fCountryClubMunicipality := '';
  fCountryClubRegion := '';
  fCountryClubStreet := '';
  fCountryClubPostCode := '';
  fCountryClubWebsite := '';
  fCountryClubPhone := '';
  fCourseNumberOfHoles := 0;
  fCourseCount := 0;
  fTeeColourCount := 0;
  fMetric := True;
  fCoursePar := 0;
  fStrokeIndex := 0;
  fTeeDistanceMetres := 0;
  fTeeDistanceYards := 0;
  fCourseTotalPar := 0;
  fSlopeRating := 0;
  fCourseRating := 0;
  fCourseLoaded := False;
  fCourseName := '';
  ;
  fTeeTitle := '';
  fTeeColour := '';
  fTeeGender := '';

  fAmenityType := '';
  fAmenetyValue := '';

  fIncludeScoreCardStylesheetReference := False;
  fIncludePlayerScoreCardStylesheetReference := False;
  fScoreCardPlayerName := 'unknown';
  fScoreCardPlayerGender := 'unknown';
  fScoreCardPlayerDateOfBirth := FormatDateTime(ShortDateFormat, Now);
  fScoreCardPlayerHandicap := 36;
  fScoreCardXSL := C_SCORECARDXSL;
  fPlayerScoreCardXSL := C_PLAYERSCORECARDXSL;
  fScoreCardTeeColour := 'unknown';
  fScoreCardCourseName := 'unknown';
  fScoreCardCSS := 'golfml.css';
  fErrorCode:=C_ERROR_NOERROR; // Everything OK
  fErrorString:='';
  Result := True;

end;
// *********************************************************************
// PUBLIC METHODS
// *********************************************************************
procedure TGolfmlClass.Reset;
// Easy-to-remember method name
begin
  prot_InitObj;
end;
// *********************************************************************
function TGolfmlClass.InitOK: boolean;
  // If GolfMLClass.InitOk then.. carry on
  // else FatalError;
begin
  Result := prot_InitObj;
end;
// *********************************************************************
procedure TGolfmlClass.SplitAmenety(aString: string);
// Returns fAmenityType, fAmenetyValue
// Uses C_AMENETYDELIMITER
var
  i: integer;
begin
  if Length(aString) = 0 then
    exit;
  i := Pos(C_AMENETYDELIMITER, aString);
  try
    fAmenityType := LeftStr(aString, i - 1);
  except
    fAmenityType := LeftStr(aString, i);
  end;
  fAmenetyValue := RightStr(aString, Length(aString) - i);
end;
// *********************************************************************
function TGolfmlClass.MakeGolfmlFile: boolean;
  // Construct a new XML file from data in the private arrays
var
  Doc: TXMLDocument;
  RootNode, CountryClubNode, PlayerNode, ApplicationNode, VersionNode: TDOMNode;
  GolfCourseNode, TeeSetNode: TDOMNode;
  OuterNode, InnerNode, InnerInnerNode, TextNode: TDOMNode;
  cOuterLoop, cInnerLoop, cLoop: cardinal;
  cCourseLoop, cTeePositionLoop, cHoleLoop: cardinal;
  cInTeeDistanceMeters, cInTeeDistanceYards: cardinal;
  cOutTeeDistanceMeters, cOutTeeDistanceYards: cardinal;
  cCourseNumberOfHoles, cTotalCoursePar: cardinal;
  PI: TDOMProcessingInstruction;

begin
  Result := False; // Any early exit sets the result as FALSE
  fCourseLoaded := False; // Set up for correct property access
  try
    try
      // Create a document
      Doc := TXMLDocument.Create;

      // Add stylesheet?
      if fIncludeScoreCardStylesheetReference then
      begin
        PI := Doc.CreateProcessingInstruction(
          'xml-stylesheet', Format('type="text/xsl" href="%s"', [fScoreCardXSL]));
        Doc.Appendchild(PI);
      end;
      if fIncludePlayerScoreCardStylesheetReference then
      begin
        PI := Doc.CreateProcessingInstruction(
          'xml-stylesheet', Format('type="text/xsl" href="%s"', [fPlayerScoreCardXSL]));
        Doc.Appendchild(PI);
      end;

      // Create a root node with attributes
      RootNode := Doc.CreateElement('golfml');
      TDOMElement(RootNode).SetAttribute('xmlns', 'http://code.google.com/p/golfml');
      TDOMElement(RootNode).SetAttribute('xmlns:xsi',
        'http://www.w3.org/2001/XMLSchema-instance');
      TDOMElement(RootNode).SetAttribute('xmlns:g', 'http://code.google.com/p/golfml');
      TDOMElement(RootNode).SetAttribute('xmlns:golfmlclass',
        'http://www.charcodelvalle.com/golfml');
      TDOMElement(RootNode).SetAttribute('xsi:schemaLocation',
        'http://code.google.com/p/golfml/source/browse/golfml/schemas/golfml.xsd');
      TDOMElement(RootNode).SetAttribute('version', '0.9');
      // Add <golfml> to the document
      Doc.Appendchild(RootNode);
      RootNode := Doc.DocumentElement;


      // *********************************************************************
      // Country Club Section
      // *********************************************************************
      CountryClubNode := Doc.CreateElement('country-club');
      // *********************************************************************
      // <name>String</name>
      OuterNode := Doc.CreateElement('name');
      TextNode := Doc.CreateTextNode(fCountryClubName);
      OuterNode.AppendChild(TextNode);
      CountryClubNode.AppendChild(OuterNode);
      // *********************************************************************
      // <position><gps lat= lon= alt= /></position>
      OuterNode := Doc.CreateElement('position');
      InnerNode := Doc.CreateElement('gps');
      TDOMElement(InnerNode).SetAttribute('lat', FloatToStr(fCountryClubGPSLatitude));
      TDOMElement(InnerNode).SetAttribute('lon', FloatToStr(fCountryClubGPSLongitude));
      TDOMElement(InnerNode).SetAttribute('alt', '0');
      OuterNode.AppendChild(InnerNode);
      CountryClubNode.AppendChild(OuterNode);
      // *********************************************************************
      // <note><comment></comment></note>
      OuterNode := Doc.CreateElement('note');
      if Length(fCountryClubCommentsArray[0]) > 0 then
      begin
        for cLoop := 0 to High(fCountryClubCommentsArray) do
        begin
          InnerNode := Doc.CreateElement('comment');
          TDOMElement(InnerNode).SetAttribute('lang', 'en');
          TextNode := Doc.CreateTextNode(fCountryClubCommentsArray[cLoop]);
          InnerNode.AppendChild(TextNode);
          OuterNode.AppendChild(InnerNode);
        end;
      end;
      // Add 'Created by Golfml Coursewriter' comment
      InnerNode := Doc.CreateElement('comment');
      TDOMElement(InnerNode).SetAttribute('lang', 'en');
      TextNode := Doc.CreateTextNode('Created by Golfml Coursewriter');
      InnerNode.AppendChild(TextNode);
      OuterNode.AppendChild(InnerNode);

      InnerNode := Doc.CreateElement('comment');
      TDOMElement(InnerNode).SetAttribute('lang', 'en');
      TextNode := Doc.CreateTextNode('http://code.google.com/p/golfml/');
      InnerNode.AppendChild(TextNode);
      OuterNode.AppendChild(InnerNode);

      CountryClubNode.AppendChild(OuterNode);
      // *********************************************************************
      // <address><><></address>
      OuterNode := Doc.CreateElement('address');
      if Length(fCountryClubStreet) > 0 then
      begin
        InnerNode := Doc.CreateElement('street');
        TextNode := Doc.CreateTextNode(fCountryClubStreet);
        InnerNode.AppendChild(TextNode);
        OuterNode.AppendChild(InnerNode);
        CountryClubNode.AppendChild(OuterNode);
      end;
      if Length(fCountryClubMunicipality) > 0 then
      begin
        InnerNode := Doc.CreateElement('municipality');
        TextNode := Doc.CreateTextNode(fCountryClubMunicipality);
        InnerNode.AppendChild(TextNode);
        OuterNode.AppendChild(InnerNode);
        CountryClubNode.AppendChild(OuterNode);
      end;
      if Length(fCountryClubRegion) > 0 then
      begin
        InnerNode := Doc.CreateElement('region');
        TextNode := Doc.CreateTextNode(fCountryClubRegion);
        InnerNode.AppendChild(TextNode);
        OuterNode.AppendChild(InnerNode);
        CountryClubNode.AppendChild(OuterNode);
      end;
      if Length(fCountryClubPostCode) > 0 then
      begin
        InnerNode := Doc.CreateElement('postal-code');
        TextNode := Doc.CreateTextNode(fCountryClubPostCode);
        InnerNode.AppendChild(TextNode);
        OuterNode.AppendChild(InnerNode);
        CountryClubNode.AppendChild(OuterNode);
      end;
      if Length(fCountryClubCountry) > 0 then
      begin
        InnerNode := Doc.CreateElement('country');
        TDOMElement(InnerNode).SetAttribute('code', fCountryClubCountryCode);
        TextNode := Doc.CreateTextNode(fCountryClubCountry);
        InnerNode.AppendChild(TextNode);
        OuterNode.AppendChild(InnerNode);
        CountryClubNode.AppendChild(OuterNode);
      end;

      if (Length(fCountryClubWebsite) > 0) then
      begin
        if (fCountryClubWebsite <> 'http://www.') then
        begin
          InnerNode := Doc.CreateElement('website');
          TextNode := Doc.CreateTextNode(fCountryClubWebsite);
          InnerNode.AppendChild(TextNode);
          OuterNode.AppendChild(InnerNode);
          CountryClubNode.AppendChild(OuterNode);
        end;
      end;
      // *********************************************************************
      // <contact>
      if Length(fCountryClubPhone) > 0 then
      begin
        OuterNode := Doc.CreateElement('contact');
        TDOMElement(OuterNode).SetAttribute('type', 'club-house');
        InnerNode := Doc.CreateElement('phone');
        TextNode := Doc.CreateTextNode(fCountryClubPhone);
        InnerNode.AppendChild(TextNode);
        OuterNode.AppendChild(InnerNode);
        CountryClubNode.AppendChild(OuterNode);
      end;

      // *********************************************************************
      // <amenety type=>
      if Length(fCountryClubAmenetiesArray[0]) > 0 then
      begin
        fAmenityType := '';
        fAmenetyValue := '';
        for cLoop := 0 to High(fCountryClubAmenetiesArray) do
        begin
          SplitAmenety(fCountryClubAmenetiesArray[cLoop]);
          OuterNode := Doc.CreateElement('amenety');
          TDOMElement(OuterNode).SetAttribute('type', fAmenityType);
          TextNode := Doc.CreateTextNode(fAmenetyValue);
          OuterNode.AppendChild(TextNode);
          CountryClubNode.AppendChild(OuterNode);
        end;
      end;

      // *********************************************************************
      // Golf Courses Club Section
      // *********************************************************************
      // Golf Course Name
      for cCourseLoop := 0 to High(fCourseNameArray) do
      begin
        GolfCourseNode := Doc.CreateElement('golf-course');
        OuterNode := Doc.CreateElement('name');
        TextNode := Doc.CreateTextNode(fCourseNameArray[cCourseLoop]);
        OuterNode.AppendChild(TextNode);
        GolfCourseNode.AppendChild(OuterNode);
        for cTeePositionLoop := 0 to High(fCourseTeeColourArray[cCourseLoop]) do
        begin
          // <Tee-Set>s
          if Length(
            fCourseTeeColourArray[cCourseLoop, cTeePositionLoop]) = 0 then
            continue;

          TeeSetNode := Doc.CreateElement('tee-set');
          TDOMElement(TeeSetNode).SetAttribute(
            'name', fCourseTeeTitleArray[cCourseLoop, cTeePositionLoop]);
          TDOMElement(TeeSetNode).SetAttribute(
            'gender', fCourseTeeGenderArray[cCourseLoop, cTeePositionLoop]);
          TDOMElement(TeeSetNode).SetAttribute(
            'colour', fCourseTeeColourArray[cCourseLoop, cTeePositionLoop]);
          GolfCourseNode.AppendChild(TeeSetNode);

          // <qualification> Course Rating and Slope Rating
          OuterNode := Doc.CreateElement('qualification');
          InnerNode := Doc.CreateElement('qualification-usga');
          InnerInnerNode := Doc.CreateElement('rating');
          TextNode :=
            Doc.CreateTextNode(Format('%0.2f',
            [fTeeColourCourseRatingArray[cCourseLoop, cTeePositionLoop]]));
          InnerInnerNode.AppendChild(TextNode);
          InnerNode.AppendChild(InnerInnerNode);
          InnerInnerNode := Doc.CreateElement('slope');
          TextNode :=
            Doc.CreateTextNode(Format('%d',
            [fTeeColourSlopeRatingArray[cCourseLoop, cTeePositionLoop]]));
          InnerInnerNode.AppendChild(TextNode);
          InnerNode.AppendChild(InnerInnerNode);
          OuterNode.AppendChild(InnerNode);
          TeeSetNode.AppendChild(OuterNode);

          // <summary> back,front,holes,par
          // Calculate on-the-fly
          cInTeeDistanceMeters := 0;
          cInTeeDistanceYards := 0;
          cOutTeeDistanceMeters := 0;
          cOutTeeDistanceYards := 0;
          cCourseNumberOfHoles := 0;
          cTotalCoursePar := 0;
          for cHoleLoop := 0 to High(fCourseParArray[cCourseLoop]) do
          begin
            if fCourseParArray[cCourseLoop, cHoleLoop] = 0 then
              continue;
            if cHoleLoop < 9 then
            begin
              Inc(cInTeeDistanceMeters,
                fTeeDistanceArrayMeters[cCourseLoop, cTeePositionLoop, cHoleLoop]);
              Inc(cInTeeDistanceYards,
                fTeeDistanceArrayYards[cCourseLoop, cTeePositionLoop, cHoleLoop]);
            end
            else
            begin
              Inc(cOutTeeDistanceMeters,
                fTeeDistanceArrayMeters[cCourseLoop, cTeePositionLoop, cHoleLoop]);
              Inc(cOutTeeDistanceYards,
                fTeeDistanceArrayYards[cCourseLoop, cTeePositionLoop, cHoleLoop]);
            end;
            Inc(cCourseNumberOfHoles, 1);
            Inc(cTotalCoursePar,
              fCourseParArray[cCourseLoop, cHoleLoop]);
          end;
          // Write results to XML
          OuterNode := Doc.CreateElement('summary');

          InnerNode := Doc.CreateElement('front');
          TDOMElement(InnerNode).SetAttribute('units', 'meters');
          TextNode :=
            Doc.CreateTextNode(Format('%d', [cInTeeDistanceMeters]));
          InnerNode.AppendChild(TextNode);
          OuterNode.AppendChild(InnerNode);

          InnerNode := Doc.CreateElement('front');
          TDOMElement(InnerNode).SetAttribute('units', 'yards');
          TextNode :=
            Doc.CreateTextNode(Format('%d', [cInTeeDistanceYards]));
          InnerNode.AppendChild(TextNode);
          OuterNode.AppendChild(InnerNode);

          InnerNode := Doc.CreateElement('back');
          TDOMElement(InnerNode).SetAttribute('units', 'meters');
          TextNode :=
            Doc.CreateTextNode(Format('%d', [cOutTeeDistanceMeters]));
          InnerNode.AppendChild(TextNode);
          OuterNode.AppendChild(InnerNode);

          InnerNode := Doc.CreateElement('back');
          TDOMElement(InnerNode).SetAttribute('units', 'yards');
          TextNode :=
            Doc.CreateTextNode(Format('%d', [cOutTeeDistanceYards]));
          InnerNode.AppendChild(TextNode);
          OuterNode.AppendChild(InnerNode);

          InnerNode := Doc.CreateElement('holes');
          TextNode :=
            Doc.CreateTextNode(Format('%d', [cCourseNumberOfHoles]));
          InnerNode.AppendChild(TextNode);
          OuterNode.AppendChild(InnerNode);

          InnerNode := Doc.CreateElement('par');
          TextNode := Doc.CreateTextNode(Format('%d', [cTotalCoursePar]));
          InnerNode.AppendChild(TextNode);
          OuterNode.AppendChild(InnerNode);
          TeeSetNode.AppendChild(OuterNode);

          // <Tee>s
          for cHoleLoop := 0 to High(fCourseParArray[cCourseLoop]) do
          begin
            if fCourseParArray[cCourseLoop, cHoleLoop] = 0 then
              continue;

            OuterNode := Doc.CreateElement('tee');
            TDOMElement(OuterNode).SetAttribute(
              'number', IntToStr(cHoleLoop + 1));
            // Par
            InnerNode := Doc.CreateElement('par');
            TextNode :=
              Doc.CreateTextNode(Format('%d', [fCourseParArray[cCourseLoop, cHoleLoop]]));
            InnerNode.AppendChild(TextNode);
            OuterNode.AppendChild(InnerNode);
            // S.I.
            InnerNode := Doc.CreateElement('handicap-stroke');
            TextNode :=
              Doc.CreateTextNode(Format('%d', [fCourseStrokeIndexArray[cCourseLoop, cHoleLoop]]));
            InnerNode.AppendChild(TextNode);
            OuterNode.AppendChild(InnerNode);
            // Distance Metres
            InnerNode := Doc.CreateElement('length');
            TDOMElement(InnerNode).SetAttribute('units', 'meters');
            TextNode :=
              Doc.CreateTextNode(Format('%d', [fTeeDistanceArrayMeters[cCourseLoop,
              cTeePositionLoop, cHoleLoop]]));
            InnerNode.AppendChild(TextNode);
            OuterNode.AppendChild(InnerNode);

            // Distance Yards
            InnerNode := Doc.CreateElement('length');
            TDOMElement(InnerNode).SetAttribute('units', 'yards');
            TextNode :=
              Doc.CreateTextNode(Format('%d', [fTeeDistanceArrayYards[cCourseLoop,
              cTeePositionLoop, cHoleLoop]]));
            InnerNode.AppendChild(TextNode);
            OuterNode.AppendChild(InnerNode);

            TeeSetNode.AppendChild(OuterNode);
          end;
          GolfCourseNode.AppendChild(TeeSetNode);
        end;
        // Close Golf Course
        CountryClubNode.AppendChild(GolfCourseNode);
      end;
      // Write and close <country-club>
      RootNode.AppendChild(CountryClubNode);

      // *********************************************************************
      // Optional Player Section
      // *********************************************************************
      if fIncludePlayerScoreCardStylesheetReference then
      begin
        // Create Player section
        PlayerNode := Doc.CreateElement('player');
        InnerNode := Doc.CreateElement('name');
        TDOMElement(InnerNode).SetAttribute('gender', fScoreCardPlayerGender);
        TextNode := Doc.CreateTextNode(fScoreCardPlayerName);
        InnerNode.AppendChild(TextNode);
        PlayerNode.AppendChild(InnerNode);

        InnerNode := Doc.CreateElement('date-of-birth');
        TextNode := Doc.CreateTextNode(fScoreCardPlayerDateOfBirth);
        InnerNode.AppendChild(TextNode);
        PlayerNode.AppendChild(InnerNode);

        InnerNode := Doc.CreateElement('current-handicap');
        TDOMElement(InnerNode).SetAttribute('handicap-system', 'USGA');
        TextNode := Doc.CreateTextNode(Format('%0.1f', [fScoreCardPlayerHandicap]));
        InnerNode.AppendChild(TextNode);
        PlayerNode.AppendChild(InnerNode);
        RootNode.AppendChild(PlayerNode);
      end;
      // *********************************************************************
      // Application Section
      // *********************************************************************
      ApplicationNode := Doc.CreateElement('application');
      TDOMElement(ApplicationNode).SetAttribute('dotname', 'com.scorecard.app');
      TDOMElement(ApplicationNode).SetAttribute('name', 'scorecard application');
      TDOMElement(ApplicationNode).SetAttribute(
        'xmlns:s', 'http://app.scorecard.com/golfml');
      // Preference (1)
      InnerNode := Doc.CreateElement('s:preference');
      TDOMElement(InnerNode).SetAttribute('name', 'handicap-method');
      TextNode := Doc.CreateTextNode('usga');
      InnerNode.AppendChild(TextNode);
      ApplicationNode.AppendChild(InnerNode);
      // Preference (2)
      InnerNode := Doc.CreateElement('s:preference');
      TDOMElement(InnerNode).SetAttribute('name', 'units-system');
      TextNode := Doc.CreateTextNode('metric');
      InnerNode.AppendChild(TextNode);
      ApplicationNode.AppendChild(InnerNode);
      RootNode.AppendChild(ApplicationNode);

      if fIncludePlayerScoreCardStylesheetReference then
      begin
        // *********************************************************************
        // Custom Application Section
        // *********************************************************************
        // for the use of playerscorecard.xsl
        ApplicationNode := Doc.CreateElement('application');
        TDOMElement(ApplicationNode).SetAttribute(
          'dotname', 'com.charcodelvalle.golfmlclass');
        TDOMElement(ApplicationNode).SetAttribute('name', 'golfml custom scorecard');
        TDOMElement(ApplicationNode).SetAttribute(
          'xmlns:golfmlclass', 'http://code.google.com/p/golfml');
        TDOMElement(ApplicationNode).SetAttribute('version', C_GOLFMLCLASSVERSION);
        // Create custom golfmlclass: fields
        InnerNode := Doc.CreateElement('golfmlclass:scorecard-stylesheet');
        TextNode := Doc.CreateTextNode(fScoreCardXSL);
        InnerNode.AppendChild(TextNode);
        ApplicationNode.AppendChild(InnerNode);

        InnerNode := Doc.CreateElement('golfmlclass:playerscorecard-stylesheet');
        TextNode := Doc.CreateTextNode(fPlayerScoreCardXSL);
        InnerNode.AppendChild(TextNode);
        ApplicationNode.AppendChild(InnerNode);

        InnerNode := Doc.CreateElement('golfmlclass:scorecard-css');
        TextNode := Doc.CreateTextNode(fScoreCardCSS);
        InnerNode.AppendChild(TextNode);
        ApplicationNode.AppendChild(InnerNode);

        InnerNode := Doc.CreateElement('golfmlclass:handicap-system');
        TextNode := Doc.CreateTextNode('ega');
        InnerNode.AppendChild(TextNode);
        ApplicationNode.AppendChild(InnerNode);

        InnerNode := Doc.CreateElement('golfmlclass:units');
        if fMetric = C_METERS then
          TextNode := Doc.CreateTextNode('meters')
        else
          TextNode := Doc.CreateTextNode('yards');
        InnerNode.AppendChild(TextNode);
        ApplicationNode.AppendChild(InnerNode);

        InnerNode := Doc.CreateElement('golfmlclass:course-name');
        TextNode := Doc.CreateTextNode(fScoreCardCourseName);
        InnerNode.AppendChild(TextNode);
        ApplicationNode.AppendChild(InnerNode);

        InnerNode := Doc.CreateElement('golfmlclass:tee-colour');
        TextNode := Doc.CreateTextNode(fScoreCardTeeColour);
        InnerNode.AppendChild(TextNode);
        ApplicationNode.AppendChild(InnerNode);

        InnerNode := Doc.CreateElement('golfmlclass:player-name');
        TextNode := Doc.CreateTextNode(fScoreCardPlayerName);
        InnerNode.AppendChild(TextNode);
        ApplicationNode.AppendChild(InnerNode);

        InnerNode := Doc.CreateElement('golfmlclass:player-handicap');
        TextNode := Doc.CreateTextNode(Format('%0.1f', [fScoreCardPlayerHandicap]));
        InnerNode.AppendChild(TextNode);
        ApplicationNode.AppendChild(InnerNode);

        InnerNode := Doc.CreateElement('golfmlclass:last-updated');
        TextNode := Doc.CreateTextNode(DateTimeToStr(Now));
        InnerNode.AppendChild(TextNode);
        ApplicationNode.AppendChild(InnerNode);
        RootNode.AppendChild(ApplicationNode);
      end;


      // *********************************************************************
      // Version Section
      // *********************************************************************
      VersionNode := Doc.CreateElement('version');
      TDOMElement(VersionNode).SetAttribute('created', DateTimeToStr(Now));
      TDOMElement(VersionNode).SetAttribute('version', C_GOLFMLCLASSVERSION);
      TextNode := Doc.CreateTextNode('GolfMLClass');
      VersionNode.AppendChild(TextNode);
      RootNode.AppendChild(VersionNode);



      If FileExists(fCourseXMLPath) then
         begin
              WriteXMLFile(Doc, fCourseXMLPath);
              Result := True;
         end
         else
         begin
              fErrorCode:=C_ERROR_BADPATH;
              fErrorString:=C_ERRORSTRING_BADPATH;
              Result:=False;
         end;
    finally
      Doc.Free;
    end;

  except
    On E: Exception do
      raise;
  end;
end;
// *********************************************************************
function TGolfmlClass.GetCourseInfoFromFile: boolean;
  // Call Reset before changing the xml and calling this.
  // Be sure fCourseXMLPath is set and checked first!
  // Sets up the multi-dimensional f-arrays (all courses) and all other properties
  // by parsing the golfxml via a series of nested loops.
var
  Doc: TXMLDocument;
  CurrentNode: TDOMNode;
  cCount: cardinal;
begin
  try
    Result := False; // assume failure
    // Reset;
    If FileExists(fCourseXMLPath) then
      begin
            fCourseIndex := 0;
            fTeeColourIndex := 0;
            fHoleIndex := 0;
            ReadXMLFile(Doc, fCourseXMLPath);
      end
    else
      begin
        fErrorCode:=C_ERROR_BADPATH;
        fErrorString:=C_ERRORSTRING_BADPATH;
        Exit;
      end;
    CurrentNode := Doc.DocumentElement.FirstChild;
    // CurrentNode.NodeName=<country-club>
    priv_ProcessXMLCountryClub(CurrentNode);
    // Only the first <country-club> node is processed in this class.

    CurrentNode := CurrentNode.FirstChild; // Look at children of <country-club>

    while Assigned(CurrentNode) do
    begin
      // Loop though the <country-club> siblings
      // <golf-course>
      if CurrentNode.NodeName = 'golf-course' then
      begin
        priv_TestAndSetCourseArraySizes(fCourseIndex);
        // Increase if necessary
        priv_ProcessXMLGolfCourse(CurrentNode);
        Inc(fCourseIndex);
        fTeeColourIndex := 0; // New TeeColourSet
      end;
      // Insert processing of <player>
      // Insert processing of <application>
      // Insert processing of <version>
      // ..Next child of <country-club>
      CurrentNode := CurrentNode.NextSibling;
    end;
    Result := True; // Success!
  finally
    Doc.Free;
  end;
  fCourseIndex := 0;
  fCourseLoaded := True;
  // Check for missing tee-set node
  for cCount := 0 to High(fCourseTeeColourArray) do
    if High(fCourseTeeColourArray[cCount]) = -1 then
      priv_ProcessNoTeeSetsInCourse(cCount);
  priv_SetCourseIndex(fCourseIndex); // Sets fMax properties
end;

// *********************************************************************
// PRIVATE METHODS
// *********************************************************************
function TGolfmlClass.priv_GetBogeyRating: single;
  // From pcourserating and psloperating
  // CALCULATING BOGEY RATING from Course Rating and Slope Rating
  // Bogey rating minus Course Rating (C) multiplied by (5.381 men, 4.24 women)(K)  equals Slope Rating (S).
  // (B-C) * K = S
  // B-C = S/K
  // B=(S/K) * C
var
  bLadies: boolean;
  ftempCourseRating: single;
  ctempSlopeRating: cardinal;
begin
  Result := 0;
  // Trap for crap 1
  if fCourseIndex > priv_GetMaxCourseIndex then
    Exit;
  if fTeeColourIndex > priv_GetMaxTeeColourIndex then
    Exit;
  ftempCourseRating := fTeeColourCourseRatingArray[fCourseIndex, fTeeColourIndex];
  ctempSlopeRating := fTeeColourSlopeRatingArray[fCourseIndex, fTeeColourIndex];

  // Trap for crap 2
  if (ctempSlopeRating = 0) or (ftempCourseRating = 0) then
    Exit;

  bLadies := False;
  if CompareText(fCourseTeeGenderArray[fCourseIndex, fTeeColourIndex], 'ladies') = 0 then
    bLadies := True;
  if bLadies then
    Result := (ctempSlopeRating / 4.24) + ftempCourseRating
  else
    Result := (ctempSlopeRating / 5.381) + ftempCourseRating;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetTotalTeeDistance;
// Called when setting a new tee position from priv_SetTeeColourIndex
// Sets InTeeDistance,OutTeeDistance and TotalTeeDistance properties
// taking into account fMetric=C_METRES or fMetric=C_YARDS
// If yards set but not metres (or vice versa), then automatically converts.
// note: priv_SetMetric can also re-assign the properties directly
var
  cHoleCount: cardinal;
begin
  fInTeeDistance := 0;
  fOutTeeDistance := 0;
  fTotalTeeDistance := 0;

  fTotalTeeDistanceMetres := 0;
  fTotalTeeDistanceYards := 0;
  fInTeeDistanceMetres := 0;
  fInTeeDistanceYards := 0;
  fOutTeeDistanceMetres := 0;
  fOutTeeDistanceYards := 0;
  for cHoleCount := 0 to 8 do
  begin
    Inc(fInTeeDistanceMetres, fTeeDistanceArrayMeters[fCourseIndex,
      fTeeColourIndex, cHoleCount]);
    Inc(fInTeeDistanceYards, fTeeDistanceArrayYards[fCourseIndex,
      fTeeColourIndex, cHoleCount]);
  end;
  if fCourseNumberOfHoles > 8 then
  begin
    for cHoleCount := 9 to 17 do
    begin
      Inc(fOutTeeDistanceMetres, fTeeDistanceArrayMeters[fCourseIndex,
        fTeeColourIndex, cHoleCount]);
      Inc(fOutTeeDistanceYards, fTeeDistanceArrayYards[fCourseIndex,
        fTeeColourIndex, cHoleCount]);
    end;
  end;
  // Fetch Total
  fTotalTeeDistanceMetres := fInTeeDistanceMetres + fOutTeeDistanceMetres;
  fTotalTeeDistanceYards := fInTeeDistanceYards + fOutTeeDistanceYards;

  // If both yards and metres are set to nonzero, then do nothing, else convert on-the-fly
  // Fetch metres from yards?
  if (fMetric = C_METERS) and (fTotalTeeDistanceMetres = 0) and
    (fTotalTeeDistanceYards > 0) then
  begin
    fInTeeDistanceMetres := TRUNC((fInTeeDistanceYards * 0.9144) + 0.5);
    fOutTeeDistanceMetres := TRUNC((fOutTeeDistanceYards * 0.9144) + 0.5);
  end;

  // Fetch yards from metres?
  if (fMetric = C_YARDS) and (fTotalTeeDistanceYards = 0) and (fTotalTeeDistanceMetres > 0) then
  begin
    fInTeeDistanceYards := TRUNC((fInTeeDistanceMetres * 1.0936133) + 0.5);
    fOutTeeDistanceYards := TRUNC((fOutTeeDistanceMetres * 1.0936133) + 0.5);
  end;

  // Fetch Total again (possible rounding differences)
  fTotalTeeDistanceMetres := fInTeeDistanceMetres + fOutTeeDistanceMetres;
  fTotalTeeDistanceYards := fInTeeDistanceYards + fOutTeeDistanceYards;

  // Assign Total Distance property vars
  if fMetric = C_METERS then
  begin
    fInTeeDistance := fInTeeDistanceMetres;
    fOutTeeDistance := fOutTeeDistanceMetres;
    fTotalTeeDistance := fTotalTeeDistanceMetres;
  end
  else
  begin
    fInTeeDistance := fInTeeDistanceYards;
    fOutTeeDistance := fOutTeeDistanceYards;
    fTotalTeeDistance := fTotalTeeDistanceYards;
  end;

end;
// *********************************************************************
procedure TGolfmlClass.priv_ProcessNoTeeSetsInCourse(cCourseIndex: cardinal);
var
  cCount: cardinal;
begin
  // SET UP DUMMY INFO FOR A COURSE WITH A NAME BUT NO TEE SETS (COLOURS) HERE
  // ShowMessageFmt('No data for cCourseIndex=%d',[cCourseIndex]);
  // cCourseIndex is the index of the course (fCourseNameArray) with a name but no data

  // 1 ) Make a dummy TeeColourArray,TeeTitleArray,TeeGenderArray entry for the course
  SetLength(fCourseTeeColourArray[cCourseIndex], 1);
  fCourseTeeColourArray[cCourseIndex, 0] := 'No tee data';
  SetLength(fCourseTeeTitleArray[cCourseIndex], 1);
  fCourseTeeTitleArray[cCourseIndex, 0] := 'No tee data';
  SetLength(fCourseTeeGenderArray[cCourseIndex], 1);
  fCourseTeeGenderArray[cCourseIndex, 0] := 'No tee data';
  // 2 ) Make a Course Rating and Slope Rating for the dummy tee colour
  SetLength(fTeeColourCourseRatingArray[cCourseIndex], 1);
  fTeeColourCourseRatingArray[cCourseIndex, 0] := 0;
  SetLength(fTeeColourSlopeRatingArray[cCourseIndex], 1);
  fTeeColourSlopeRatingArray[cCourseIndex, 0] := 0;

  // 3 ) Make tee distances for the dummy tee colour
  SetLength(fTeeDistanceArrayMeters[cCourseIndex], 1);
  SetLength(fTeeDistanceArrayYards[cCourseIndex], 1);
  for cCount := 0 to 17 do
  begin
    fTeeDistanceArrayMeters[cCourseIndex, 0, cCount] := 0;
    fTeeDistanceArrayYards[cCourseIndex, 0, cCount] := 0;
  end;
end;
// *********************************************************************
function TGolfmlClass.priv_PrettyString(aString: string): string;
  // Capitalises first letter
  // Used to make tee attributes (usually all-lowercase) display nicely
var
  sFirst, sRest: string;
begin
  Result := aString;
  if Length(aString) > 1 then
  begin
    sFirst := UpperCase(LeftStr(AString, 1));
    sRest := RightStr(AString, Length(AString) - 1);
    Result := sFirst + sRest;
  end;
end;
// *********************************************************************
// Various XML Subroutines
procedure TGolfmlClass.priv_ProcessXMLTee(aNode: TDomNode; TeeColour: cardinal;
  sCourseName: string);
var
  CurrentNode: TDOMNode;
  iAttributeCount, iInteger, iTeeNumber: integer;
  aString: string;
begin
  // CurrentNode=<tee>
  // <tee> has attributes and sub-node values
  CurrentNode := aNode;
  if CurrentNode.HasAttributes and (CurrentNode.Attributes.Length > 0) then
    for iAttributeCount := 0 to CurrentNode.Attributes.Length - 1 do
    begin
      if CurrentNode.Attributes[iAttributeCount].NodeName = 'number' then
      begin
        aString := '0' + CurrentNode.Attributes[iAttributeCount].NodeValue;
        if TryStrToInt(aString, iInteger) = True then
          iTeeNumber := StrToInt(aString)
        else
        begin
          raise Exception.Create(C_ERRORXMLREAD + 'Error: Tee number attribute is invalid');
          Exit;
        end;
      end;
    end;
  // Now look at children of <tee>
  CurrentNode := aNode.FirstChild;
  while Assigned(CurrentNode) do
  begin
    if CurrentNode.NodeName = 'par' then
    begin
      aString := '0' + CurrentNode.TextContent;
      if TryStrToInt(aString, iInteger) = True then
        fCourseParArray[fCourseIndex, iTeeNumber - 1] := StrToInt(aString)
      else
        fCourseParArray[fCourseIndex, iTeeNumber - 1] := 0;
    end;
    if CurrentNode.NodeName = 'handicap-stroke' then
    begin
      aString := '0' + CurrentNode.TextContent;
      if TryStrToInt(aString, iInteger) = True then
        fCourseStrokeIndexArray[fCourseIndex, iTeeNumber - 1] := StrToInt(aString)
      else
      begin
        fCourseStrokeIndexArray[fCourseIndex, iTeeNumber - 1] := 0;
        raise Exception.Create(C_ERRORXMLREAD +
          'Error: A Stroke Index element is not a number');
      end;
    end;
    if CurrentNode.NodeName = 'length' then
      if CurrentNode.HasAttributes and (CurrentNode.Attributes.Length > 0) then
        for iAttributeCount := 0 to CurrentNode.Attributes.Length - 1 do
        begin
          if CurrentNode.Attributes[iAttributeCount].NodeName = 'units' then
          begin
            if CurrentNode.Attributes[iAttributeCount].NodeValue =
              'meters' then
            begin
              aString := '0' + CurrentNode.TextContent;
              if TryStrToInt(aString, iInteger) = True then
                fTeeDistanceArrayMeters[fCourseIndex,
                  TeeColour, iTeeNumber - 1] := StrToInt(aString)
              else
              begin
                fTeeDistanceArrayMeters[fCourseIndex,
                  TeeColour, iTeeNumber - 1] := 0;
                raise Exception.Create(C_ERRORXMLREAD +
                  'Error: A Distance element (meters) is invalid');
              end;
            end;
            if CurrentNode.Attributes[iAttributeCount].NodeValue =
              'yards' then
            begin
              aString := '0' + CurrentNode.TextContent;
              if TryStrToInt(aString, iInteger) = True then
                fTeeDistanceArrayYards[fCourseIndex,
                  TeeColour, iTeeNumber - 1] := StrToInt(aString)
              else
              begin
                fTeeDistanceArrayYards[fCourseIndex,
                  TeeColour, iTeeNumber - 1] := 0;
                raise Exception.Create(
                  C_ERRORXMLREAD + 'Error: A Distance element (yards) is invalid');
              end;
            end;
          end;
        end;
    CurrentNode := CurrentNode.NextSibling;
  end;
end;
// *********************************************************************
procedure TGolfmlClass.priv_ProcessXMLQualificationUSGA(aNode: TDomNode;
  TeeColour: cardinal; sCourseName: string);
var
  CurrentNode: TDOMNode;
  aString: string;
  iInteger: integer;
  fFloat: single;
begin
  // CurrentNode=<qualification-usga>
  // Now look at children of <qualification-usga>
  CurrentNode := aNode.FirstChild;
  while Assigned(CurrentNode) do
  begin
    if CurrentNode.NodeName = 'rating' then
    begin
      AString := '0' + CurrentNode.TextContent;
      if TryStrToFloat(AString, fFloat) = True then
        fTeeColourCourseRatingArray[fCourseIndex, TeeColour] :=
          StrToFloat(AString)
      else
      begin
        fTeeColourCourseRatingArray[fCourseIndex, TeeColour] := 0;
        raise Exception.Create(C_ERRORXMLREAD +
          'Error: A Course Rating element is invalid');
      end;

    end;
    if CurrentNode.NodeName = 'slope' then
    begin
      AString := '0' + CurrentNode.TextContent;
      if TryStrToInt(AString, iInteger) = True then
        fTeeColourSlopeRatingArray[fCourseIndex, TeeColour] := StrToInt(AString)
      else
      begin
        fTeeColourSlopeRatingArray[fCourseIndex, TeeColour] := 0;
        raise Exception.Create(C_ERRORXMLREAD +
          'Error: A Slope Rating element is invalid');
      end;
    end;
    CurrentNode := CurrentNode.NextSibling;
  end;

end;
// *********************************************************************
procedure TGolfmlClass.priv_ProcessXMLQualification(aNode: TDomNode;
  TeeColour: cardinal; sCourseName: string);
var
  CurrentNode: TDOMNode;
begin
  // CurrentNode=<qualification>
  // Now look at children of <qualification>
  CurrentNode := aNode.FirstChild;
  while Assigned(CurrentNode) do
  begin
    if CurrentNode.NodeName = 'qualification-usga' then
      priv_ProcessXMLQualificationUSGA(CurrentNode, TeeColour, sCourseName);
    CurrentNode := CurrentNode.NextSibling;
  end;
end;
// *********************************************************************
procedure TGolfmlClass.priv_ProcessXMLTeeSet(aNode: TDomNode;
  TeeColour: cardinal; sCourseName: string);
var
  CurrentNode: TDOMNode;
begin
  // CurrentNode=<tee-set>
  // Now look at children of <tee-set>
  CurrentNode := aNode.FirstChild;
  while Assigned(CurrentNode) do
  begin
    if CurrentNode.NodeName = 'qualification' then
      priv_ProcessXMLQualification(CurrentNode, TeeColour, sCourseName);
    if CurrentNode.NodeName = 'tee' then
      priv_ProcessXMLTee(CurrentNode, TeeColour, sCourseName);
    CurrentNode := CurrentNode.NextSibling;
  end;
end;
// *********************************************************************
procedure TGolfmlClass.priv_ProcessXMLGolfCourse(aNode: TDomNode);
// <golf-course> is a node with multiple sub-nodes of <tee-set>
var
  CurrentNode: TDOMNode;
  iAttributeCount: integer;
  sCourseName: string;
  cCount: cardinal;
begin
  // CurrentNode=<golf-course>
  // Now look at children of <golf-course>
  CurrentNode := aNode.FirstChild;
  while Assigned(CurrentNode) do
  begin
    if CurrentNode.NodeName = 'name' then
    begin
      if Length(CurrentNode.TextContent) > 0 then
              sCourseName := CurrentNode.TextContent
      else sCourseName := 'Unknown Course Name';
      fCourseNameArray[fCourseIndex] := sCourseName;  // Golf course name
    end;
    if CurrentNode.NodeName = 'tee-set' then
    begin
      if CurrentNode.HasAttributes and (CurrentNode.Attributes.Length > 0) then
      begin
        priv_TestAndSetTeeColourArraySizes(fCourseIndex, fTeeColourIndex);
        // Increases array sizes if needed

        for iAttributeCount := 0 to CurrentNode.Attributes.Length - 1 do
        begin
          if CurrentNode.Attributes[iAttributeCount].NodeName =
            'colour' then
            fCourseTeeColourArray[fCourseIndex, fTeeColourIndex] :=
              CurrentNode.Attributes[iAttributeCount].NodeValue;
          if CurrentNode.Attributes[iAttributeCount].NodeName =
            'gender' then
            fCourseTeeGenderArray[fCourseIndex, fTeeColourIndex] :=
              CurrentNode.Attributes[iAttributeCount].NodeValue;
          if CurrentNode.Attributes[iAttributeCount].NodeName = 'name' then
            fCourseTeeTitleArray[fCourseIndex, fTeeColourIndex] :=
              CurrentNode.Attributes[iAttributeCount].NodeValue;
        end; // of for.. loop
        priv_ProcessXMLTeeSet(CurrentNode, fTeeColourIndex, sCourseName);
        Inc(fTeeColourIndex, 1);
      end; // of hasattributes
    end; // of NodeName=tee-set
    CurrentNode := CurrentNode.NextSibling;
  end;
  // ShowMessageFmt('priv_ProcessXMLGolfCourse: High(fCourseTeeColourArray[fCourseIndex])=%d',[High(fCourseTeeColourArray[fCourseIndex])]);
end;
// *********************************************************************
procedure TGolfmlClass.priv_ProcessXMLCountryClubAddress(aNode: TDomNode);
var
  CurrentNode: TDOMNode;
  iAttributeCount: cardinal;
begin
  CurrentNode := ANode; // <address>
  CurrentNode := CurrentNode.FirstChild; // Look at children of <address>
  while Assigned(CurrentNode) do
  begin
    if CurrentNode.NodeName = 'street' then
      if Length(CurrentNode.TextContent) > 0 then
        fCountryClubStreet := CurrentNode.TextContent
      else fCountryClubStreet := 'Unknown';
    if CurrentNode.NodeName = 'postal-code' then
      if Length(CurrentNode.TextContent) > 0 then
        fCountryClubPostCode := CurrentNode.TextContent
      else fCountryClubPostCode := 'Unknown';
    if CurrentNode.NodeName = 'municipality' then
      if Length(CurrentNode.TextContent) > 0 then
        fCountryClubMunicipality := CurrentNode.TextContent
      else fCountryClubMunicipality := 'Unknown';
    if CurrentNode.NodeName = 'region' then
      if Length(CurrentNode.TextContent) > 0 then
        fCountryClubRegion := CurrentNode.TextContent
      else fCountryClubRegion := 'Unknown';
    if CurrentNode.NodeName = 'website' then
      if Length(CurrentNode.TextContent) > 0 then
        fCountryClubWebsite := CurrentNode.TextContent
      else fCountryClubWebsite := 'Unknown';
    if CurrentNode.NodeName = 'country' then
      if Length(CurrentNode.TextContent) > 0 then
        fCountryClubcountry := CurrentNode.TextContent
      else fCountryClubcountry := 'Unknown';
    if CurrentNode.HasAttributes and (CurrentNode.Attributes.Length > 0) then
      for iAttributeCount := 0 to CurrentNode.Attributes.Length - 1 do
      begin
        if CurrentNode.Attributes[iAttributeCount].NodeName = 'code' then
          fCountryClubCountryCode :=
            CurrentNode.Attributes[iAttributeCount].NodeValue;
      end;
    CurrentNode := CurrentNode.NextSibling;
  end;

end;
// *********************************************************************
procedure TGolfmlClass.priv_ProcessXMLCountryClubContact(aNode: TDomNode);
var
  CurrentNode: TDOMNode;
begin
  CurrentNode := ANode; // <contact>
  CurrentNode := CurrentNode.FirstChild; // Look at children of <contact>
  while Assigned(CurrentNode) do
  begin
    if CurrentNode.NodeName = 'phone' then
      if Length(CurrentNode.TextContent) > 0 then
        fCountryClubPhone := CurrentNode.TextContent;
    CurrentNode := CurrentNode.NextSibling;
  end;
end;
// *********************************************************************
procedure TGolfmlClass.priv_ProcessXMLCountryClubPosition(aNode: TDomNode);
var
  CurrentNode: TDOMNode;
  iAttributeCount: cardinal;
  aString: string;
  fTryFloat: single;
begin
  CurrentNode := ANode; // <position>
  CurrentNode := CurrentNode.FirstChild; // Look at children of <position>
  while Assigned(CurrentNode) do
  begin
    if CurrentNode.NodeName = 'gps' then
      if CurrentNode.HasAttributes and (CurrentNode.Attributes.Length > 0) then
        for iAttributeCount := 0 to CurrentNode.Attributes.Length - 1 do
        begin
          if CurrentNode.Attributes[iAttributeCount].NodeName = 'lat' then
          begin
            aString := CurrentNode.Attributes[iAttributeCount].NodeValue;
            if TryStrToFloat(aString, fTryFloat) then
              fCountryClubGPSLatitude := fTryFloat
            else
              fCountryClubGPSLatitude := 0;
          end;
          if CurrentNode.Attributes[iAttributeCount].NodeName = 'lon' then
          begin
            aString := CurrentNode.Attributes[iAttributeCount].NodeValue;
            if TryStrToFloat(aString, fTryFloat) then
              fCountryClubGPSLongitude := fTryFloat
            else
              fCountryClubGPSLongitude := 0;
          end;
        end;
    CurrentNode := CurrentNode.NextSibling;
  end;
end;

// *********************************************************************
procedure TGolfmlClass.priv_ProcessXMLCountryClubAmenety(aNode: TDomNode);
var
  CurrentNode: TDOMNode;
begin
  CurrentNode := ANode; // <amenety>
  if Length(CurrentNode.TextContent) > 0 then
  begin
    Inc(fMaxCountryClubAmenetiesIndex, 1);
    SetLength(fCountryClubAmenetiesArray, fMaxCountryClubAmenetiesIndex + 1);
    fCountryClubAmenetiesArray[fMaxCountryClubAmenetiesIndex] :=
      CurrentNode.TextContent;
  end;
end;
// *********************************************************************
procedure TGolfmlClass.priv_ProcessXMLCountryClubNote(aNode: TDomNode);
var
  CurrentNode: TDOMNode;
begin
  CurrentNode := ANode; // <note>
  CurrentNode := CurrentNode.FirstChild; // Look at children of <note>
  while Assigned(CurrentNode) do
  begin
    if CurrentNode.NodeName = 'comment' then
      if Length(CurrentNode.TextContent) > 0 then
      begin
        Inc(fMaxCountryClubCommentsIndex, 1);
        SetLength(fCountryClubCommentsArray, fMaxCountryClubCommentsIndex + 1);
        fCountryClubCommentsArray[fMaxCountryClubCommentsIndex] :=
          CurrentNode.TextContent;
      end;
    CurrentNode := CurrentNode.NextSibling;
  end;
end;
// *********************************************************************
procedure TGolfmlClass.priv_ProcessXMLCountryClub(aNode: TDomNode);
// Populates a private record structure which is then assigned to properties
var
  CurrentNode: TDOMNode;
begin
  CurrentNode := ANode; // <country-club>
  CurrentNode := CurrentNode.FirstChild; // Look at children of <country-club>
  while Assigned(CurrentNode) do
  begin
    if CurrentNode.NodeName = 'name' then
      fCountryClubName := CurrentNode.TextContent;
    if CurrentNode.NodeName = 'position' then
      priv_ProcessXMLCountryClubPosition(CurrentNode);
    if CurrentNode.NodeName = 'address' then
      priv_ProcessXMLCountryClubAddress(CurrentNode);
    if CurrentNode.NodeName = 'contact' then
      priv_ProcessXMLCountryClubContact(CurrentNode);
    if CurrentNode.NodeName = 'amenety' then
      priv_ProcessXMLCountryClubAmenety(CurrentNode);
    if CurrentNode.NodeName = 'note' then
      priv_ProcessXMLCountryClubNote(CurrentNode);
    CurrentNode := CurrentNode.NextSibling;
  end;

end;
// *********************************************************************
procedure TGolfmlClass.priv_TestAndSetCourseArraySizes(aCourseIndex: cardinal);
// Called from priv_SetCourseIndex and others
begin
  if aCourseIndex > High(fCourseNameArray) then
  begin
    SetLength(fCourseNameArray, aCourseIndex + 1);
    SetLength(fCourseNumberOfHolesArray, aCourseIndex + 1);
    SetLength(fCourseTeeColourArray, aCourseIndex + 1);
    SetLength(fCourseTeeTitleArray, aCourseIndex + 1);
    SetLength(fCourseTeeGenderArray, aCourseIndex + 1);
    SetLength(fTeeColourCourseRatingArray, aCourseIndex + 1);
    SetLength(fTeeColourSlopeRatingArray, aCourseIndex + 1);
    SetLength(fCourseStrokeIndexArray, aCourseIndex + 1);
    SetLength(fCourseParArray, aCourseIndex + 1);
    SetLength(fTeeDistanceArrayMeters, aCourseIndex + 1);
    SetLength(fTeeDistanceArrayYards, aCourseIndex + 1);
    SetLength(fCourseTotalParArray, aCourseIndex + 1);
    fMaxCourseIndex := aCourseIndex;
  end;
end;
// *********************************************************************
procedure TGolfmlClass.priv_TestAndSetTeeColourArraySizes(aCourseIndex: cardinal;
  aTeeColourIndex: cardinal);
// Called from priv_SetTeeColourIndex and others
begin
  priv_TestAndSetCourseArraySizes(aCourseIndex);
  if aTeeColourIndex > High(fCourseTeeColourArray[aCourseIndex]) then
  begin
    SetLength(fCourseTeeColourArray[aCourseIndex], aTeeColourIndex + 1);
    SetLength(fCourseTeeTitleArray[aCourseIndex], aTeeColourIndex + 1);
    SetLength(fCourseTeeGenderArray[aCourseIndex], aTeeColourIndex + 1);
    SetLength(fTeeColourCourseRatingArray[aCourseIndex], aTeeColourIndex + 1);
    SetLength(fTeeColourSlopeRatingArray[aCourseIndex], aTeeColourIndex + 1);
    SetLength(fTeeDistanceArrayMeters[aCourseIndex], aTeeColourIndex + 1);
    SetLength(fTeeDistanceArrayYards[aCourseIndex], aTeeColourIndex + 1);
    fMaxTeeColourIndex := aTeeColourIndex;
  end;
end;

// *********************************************************************
// *********************************************************************
// GET/SETS (ALL PRIVATE METHODS)
// *********************************************************************
procedure TGolfmlClass.priv_SetCourseIndex(aCardinal: cardinal);
// Sets 'CourseIndex' property
// Also sets 'CourseTotalPar' and 'CourseNumberofHoles' properties in golfml Load mode
var
  cHoleCount: cardinal;
begin
  // Assign
  fCourseIndex := aCardinal;
  priv_TestAndSetCourseArraySizes(fCourseIndex); // Increase array?
  fMaxTeeColourIndex := priv_GetMaxTeeColourIndex;
  fCourseNumberOfHoles := 0;
  fCourseTotalPar := 0;
  // Update properties
  fMaxCourseIndex := priv_GetMaxCourseIndex;
  fCourseCount := High(fCourseNameArray) + 1;
  if fCourseNumberOfHoles = 0 then
    fTeeColourCount := 0;
  if fCourseLoaded then // In golfml Load Mode
  begin
    fCourseNumberOfHoles := 0;
    fCourseTotalPar := 0;
    fCourseTotalParArray[fCourseIndex]:=0;
    // Set fCourseNumberOfHoles here by testing for StokeIndex > 0 for each hole
    // Set fCourseTotalParArray here by adding up the pars for this Course
    for cHoleCount := 0 to 17 do
    begin
      if fCourseStrokeIndexArray[fCourseIndex, cHoleCount] > 0 then
        Inc(fCourseNumberOfHoles, 1);
      Inc(fCourseTotalParArray[fCourseIndex],
        fCourseParArray[fCourseIndex, cHoleCount]);
    end;
    fCourseTotalPar := fCourseTotalParArray[fCourseIndex];
  end;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetTeeColourIndex(aCardinal: cardinal);
// Sets 'TeeColourIndex' property
var
  cHoleCount: cardinal;
begin
  // Assign
  fTeeColourIndex := aCardinal;
  priv_TestAndSetTeeColourArraySizes(fCourseIndex, fTeeColourIndex);
  fMaxTeeColourIndex := High(fCourseTeeColourArray[fCourseIndex]);
  fTeeColourCount := High(fCourseTeeColourArray[fCourseIndex]) + 1;
  if fCourseLoaded then
    priv_SetTotalTeeDistance; // Set TotalTeeDistance properties for this tee
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetHoleIndex(aCardinal: cardinal);
// Sets 'HoleIndex' property (0-based)
begin
  // Trap for crap
  if (aCardinal > 17) then
  begin
    raise Exception.Create(
      Format('HoleIndex %d out of bounds (Max:%d)', [aCardinal, fCourseNumberOfHoles]));
    Exit;
  end;
  // Assign
  if (fHoleIndex <> aCardinal) then
    fHoleIndex := aCardinal;  //fHoleIndex is zero-based
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetCourseXMLPath(aString: string);
// Sets 'CourseXMLPath' property
begin
  if fCourseXMLPath <> aString then
    fCourseXMLPath := aString;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetTeeDistanceMetres(aCardinal: cardinal);
begin
  fTeeDistanceMetres := aCardinal;
  priv_TestAndSetCourseArraySizes(fCourseIndex); // Increase array?
  priv_TestAndSetTeeColourArraySizes(fCourseIndex, fTeeColourIndex); // Increase Array?
  fTeeDistanceYards := TRUNC((fTeeDistanceMetres * 1.0936133) + 0.5);
  fTeeDistanceArrayMeters[fCourseIndex, fTeeColourIndex, fHoleIndex] :=
    fTeeDistanceMetres;
  fTeeDistanceArrayYards[fCourseIndex, fTeeColourIndex, fHoleIndex] := fTeeDistanceYards;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetTeeDistanceYards(aCardinal: cardinal);
begin
  fTeeDistanceYards := aCardinal;
  priv_TestAndSetCourseArraySizes(fCourseIndex); // Increase array?
  priv_TestAndSetTeeColourArraySizes(fCourseIndex, fTeeColourIndex); // Increase Array?
  fTeeDistanceMetres := TRUNC((fTeeDistanceYards * 0.9144) + 0.5);
  fTeeDistanceArrayMeters[fCourseIndex, fTeeColourIndex, fHoleIndex] :=
    fTeeDistanceMetres;
  fTeeDistanceArrayYards[fCourseIndex, fTeeColourIndex, fHoleIndex] := fTeeDistanceYards;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetTeeTitle(aString: string);
begin
  if (Length(aString) = 0) then
    exit;
  fTeeTitle := aString;
  priv_TestAndSetCourseArraySizes(fCourseIndex); // Increase array?
  priv_TestAndSetTeeColourArraySizes(fCourseIndex, fTeeColourIndex); // Increase Array?
  fCourseTeeTitleArray[CourseIndex, fTeeColourIndex] := fTeeTitle;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetTeeColour(aString: string);
begin
  if (Length(aString) = 0) then
    exit;
  fTeeColour := aString;
  priv_TestAndSetCourseArraySizes(fCourseIndex); // Increase array?
  priv_TestAndSetTeeColourArraySizes(fCourseIndex, fTeeColourIndex); // Increase Array?
  fCourseTeeColourArray[CourseIndex, fTeeColourIndex] := fTeeColour;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetTeeGender(aString: string);
begin
  if (Length(aString) = 0) then
    exit;
  fTeeGender := aString;
  priv_TestAndSetCourseArraySizes(fCourseIndex); // Increase array?
  priv_TestAndSetTeeColourArraySizes(fCourseIndex, fTeeColourIndex); // Increase Array?
  fCourseTeeGenderArray[CourseIndex, fTeeColourIndex] := fTeeGender;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetMetric(aBoolean: boolean);
begin
  fMetric := aBoolean;
  if fMetric = C_METERS then
  begin
    fTotalTeeDistance := fTotalTeeDistanceMetres;
    fInTeeDistance := fInTeeDistanceMetres;
    fOutTeeDistance := fOutTeeDistanceMetres;
  end
  else
  begin
    fTotalTeeDistance := fTotalTeeDistanceYards;
    fInTeeDistance := fInTeeDistanceYards;
    fOutTeeDistance := fOutTeeDistanceYards;
  end;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetCoursePar(aCardinal: cardinal);
begin
  if fCoursePar <> aCardinal then
    fCoursePar := aCardinal;
  priv_TestAndSetCourseArraySizes(fCourseIndex); // Increase array?
  priv_TestAndSetTeeColourArraySizes(fCourseIndex, fTeeColourIndex); // Increase Array?
  fCourseParArray[fCourseIndex, fHoleIndex] := fCoursePar;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetStrokeIndex(aCardinal: cardinal);
begin
  if fStrokeIndex <> aCardinal then
    fStrokeIndex := aCardinal;
  priv_TestAndSetCourseArraySizes(fCourseIndex); // Increase array?
  priv_TestAndSetTeeColourArraySizes(fCourseIndex, fTeeColourIndex); // Increase Array?
  fCourseStrokeIndexArray[fCourseIndex, fHoleIndex] := fStrokeIndex;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetCourseRating(aSingle: single);
begin
  if fCourseRating <> aSingle then
    fCourseRating := aSingle;
  priv_TestAndSetCourseArraySizes(fCourseIndex); // Increase array?
  priv_TestAndSetTeeColourArraySizes(fCourseIndex, fTeeColourIndex); // Increase Array?
  fTeeColourCourseRatingArray[fCourseIndex, fTeeColourIndex] := fCourseRating;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetSlopeRating(aCardinal: cardinal);
begin
  if fSlopeRating <> aCardinal then
    fSlopeRating := aCardinal;
  priv_TestAndSetCourseArraySizes(fCourseIndex); // Increase array?
  priv_TestAndSetTeeColourArraySizes(fCourseIndex, fTeeColourIndex); // Increase Array?
  fTeeColourSlopeRatingArray[fCourseIndex, fTeeColourIndex] := fSlopeRating;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetCourseNumberOfHoles(aCardinal: cardinal);
begin
  fCourseNumberOfHoles := aCardinal;
  priv_TestAndSetCourseArraySizes(fCourseIndex); // Increase array?
  fCourseNumberOfHolesArray[fCourseIndex] := fCourseNumberOfHoles;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetCourseName(aString: string);
begin
  if Length(aString) = 0 then
    Exit;
  fCourseName := aString;
  priv_TestAndSetCourseArraySizes(fCourseIndex); // Increase array?
  fCourseNameArray[fCourseIndex] := fCourseName;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetClubAmeneties(const iIndex: integer; aString: string);
begin
  if iIndex > High(fCountryClubAmenetiesArray) then
    SetLength(fCountryClubAmenetiesArray, iIndex + 1);
  fCountryClubAmenetiesArray[iIndex] := aString;
end;
// *********************************************************************
function TGolfmlClass.priv_GetClubAmeneties(iIndex: integer): string;
begin
  if iIndex > High(fCountryClubAmenetiesArray) then
    exit;
  Result := fCountryClubAmenetiesArray[iIndex];
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetClubComments(const iIndex: integer; aString: string);
begin
  if iIndex > High(fCountryClubCommentsArray) then
    SetLength(fCountryClubCommentsArray, iIndex + 1);
  fCountryClubCommentsArray[iIndex] := aString;
end;
// *********************************************************************
function TGolfmlClass.priv_GetClubComments(iIndex: integer): string;
begin
  if iIndex > High(fCountryClubCommentsArray) then
    exit;
  Result := fCountryClubCommentsArray[iIndex];
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetClubName(aString: string);
begin
  fCountryClubName := aString;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetClubCountry(aString: string);
begin
  fCountryClubCountry := aString;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetClubCountryCode(aString: string);
begin
  fCountryClubCountryCode := aString;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetClubMunicipality(aString: string);
begin
  fCountryClubMunicipality := aString;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetClubRegion(aString: string);
begin
  fCountryClubRegion := aString;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetClubStreet(aString: string);
begin
  fCountryClubStreet := aString;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetClubPostCode(aString: string);
begin
  fCountryClubPostCode := aString;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetClubWebsite(aString: string);
begin
  fCountryClubWebsite := aString;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetClubPhone(aString: string);
begin
  fCountryClubPhone := aString;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetClubGPSLongitude(aSingle: single);
begin
  fCountryClubGPSLongitude := aSingle;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetClubGPSLatitude(aSingle: single);
begin
  fCountryClubGPSLatitude := aSingle;
end;
// *********************************************************************
function TGolfmlClass.priv_GetCourseName: string;
begin
  // Trap for crap
  fCourseName := 'CourseIndex too high';
  Result := fCourseName;
  if fCourseIndex > High(fCourseNameArray) then
    Exit;
  // Assign
  fCourseName := fCourseNameArray[fCourseIndex];
  Result := fCourseName;
end;
// *********************************************************************
function TGolfmlClass.priv_GetTeeTitle: string;
begin
  // Trap for crap
  fTeeTitle := 'Invalid';
  Result := fTeeTitle;
  if fCourseIndex > priv_GetMaxCourseIndex then
    Exit;
  if fTeeColourIndex > priv_GetMaxTeeColourIndex then
    Exit;
  // Assign
  fTeeTitle := fCourseTeeTitleArray[fCourseIndex, fTeeColourIndex];
  Result := fTeeTitle;
end;
// *********************************************************************
function TGolfmlClass.priv_GetTeeColour: string;
begin
  // Trap for crap
  fTeeColour := 'Invalid';
  Result := fTeeColour;
  if fCourseIndex > priv_GetMaxCourseIndex then
    Exit;
  if fTeeColourIndex > priv_GetMaxTeeColourIndex then
    Exit;
  // Assign
  fTeeColour := fCourseTeeColourArray[fCourseIndex, fTeeColourIndex];
  Result := fTeeColour;
end;
// *********************************************************************
function TGolfmlClass.priv_GetTeeGender: string;
begin
  // Trap for crap
  fTeeGender := 'Invalid';
  Result := fTeeGender;
  if fCourseIndex > priv_GetMaxCourseIndex then
    Exit;
  if fTeeColourIndex > priv_GetMaxTeeColourIndex then
    Exit;
  // Assign
  fTeeGender := fCourseTeeGenderArray[fCourseIndex, fTeeColourIndex];
  Result := fTeeGender;
end;
// *********************************************************************
function TGolfmlClass.priv_GetCoursePar: cardinal;
begin
  // Trap for crap
  fCoursePar := 0;
  Result := fCoursePar;
  if fCourseIndex > priv_GetMaxCourseIndex then
    Exit;
  // Assign
  fCoursePar := fCourseParArray[fCourseIndex, fHoleIndex];
  Result := fCoursePar;
end;
// *********************************************************************
function TGolfmlClass.priv_GetStrokeIndex: cardinal;
begin
  // Trap for crap
  fStrokeIndex := 0;
  Result := fStrokeIndex;
  if fCourseIndex > priv_GetMaxCourseIndex then
    Exit;
  // Assign
  fStrokeIndex := fCourseStrokeIndexArray[fCourseIndex, fHoleIndex];
  Result := fStrokeIndex;
end;
// *********************************************************************
function TGolfmlClass.priv_GetCourseRating: single;
begin
  // Trap for crap
  fCourseRating := 0;
  Result := fCourseRating;
  if fCourseIndex > priv_GetMaxCourseIndex then
    Exit;
  if fTeeColourIndex > priv_GetMaxTeeColourIndex then
    Exit;
  // Assign
  fCourseRating := fTeeColourCourseRatingArray[fCourseIndex, fTeeColourIndex];
  Result := fCourseRating;
end;
// *********************************************************************
function TGolfmlClass.priv_GetSlopeRating: cardinal;
begin
  // Trap for crap
  fSlopeRating := 0;
  Result := fSlopeRating;
  if fCourseIndex > priv_GetMaxCourseIndex then
    Exit;
  if fTeeColourIndex > priv_GetMaxTeeColourIndex then
    Exit;
  // Assign
  fSlopeRating := fTeeColourSlopeRatingArray[fCourseIndex, fTeeColourIndex];
  Result := fSlopeRating;
end;
// *********************************************************************
function TGolfmlClass.priv_GetCourseNumberOfHoles: cardinal;
var
  cHoleCount: cardinal;
begin
  // Trap for crap
  fCourseNumberOfHoles := 0;
  Result := fCourseNumberOfHoles;
  if fCourseIndex > priv_GetMaxCourseIndex then
    Exit;
  // Assign
  if fCourseLoaded then // In golfml Load Mode
  begin
    // Set fCourseNumberOfHoles here by testing for StokeIndex > 0 for each hole
    for cHoleCount := 0 to 17 do
      if fCourseStrokeIndexArray[fCourseIndex, cHoleCount] > 0 then
        Inc(fCourseNumberOfHoles, 1);
    Result := fCourseNumberOfHoles;
  end;
  if fCourseIndex > priv_GetMaxCourseIndex then
    Exit;
  fCourseNumberOfHolesArray[fCourseIndex] := fCourseNumberOfHoles;
end;
// *********************************************************************
function TGolfmlClass.priv_GetTeeDistanceMetres: cardinal;
begin
  // Trap for crap
  fTeeDistanceMetres := 0;
  Result := fTeeDistanceMetres;
  if fCourseIndex > priv_GetMaxCourseIndex then
    Exit;
  if fTeeColourIndex > priv_GetMaxTeeColourIndex then
    Exit;
  // Assign
  fTeeDistanceYards := fTeeDistanceArrayYards[fCourseIndex, fTeeColourIndex, fHoleIndex];
  fTeeDistanceMetres := fTeeDistanceArrayMeters[fCourseIndex,
    fTeeColourIndex, fHoleIndex];
  // Fetch metres from yards?
  if (fTeeDistanceMetres = 0) and (fTeeDistanceYards > 0) then
    fTeeDistanceMetres := TRUNC((fTeeDistanceYards * 0.9144) + 0.5);
  Result := fTeeDistanceMetres;
end;
// *********************************************************************
function TGolfmlClass.priv_GetTeeDistanceYards: cardinal;
begin
  // Trap for crap
  fTeeDistanceYards := 0;
  Result := fTeeDistanceYards;
  if fCourseIndex > priv_GetMaxCourseIndex then
    Exit;
  if fTeeColourIndex > priv_GetMaxTeeColourIndex then
    Exit;
  // Assign
  fTeeDistanceYards := fTeeDistanceArrayYards[fCourseIndex, fTeeColourIndex, fHoleIndex];
  fTeeDistanceMetres := fTeeDistanceArrayMeters[fCourseIndex,
    fTeeColourIndex, fHoleIndex];
  // Fetch yards from metres?
  if (fTeeDistanceYards = 0) and (fTeeDistanceMetres > 0) then
    fTeeDistanceYards := TRUNC((fTeeDistanceMetres * 1.0936133) + 0.5);
  Result := fTeeDistanceYards;
end;
// *********************************************************************
function TGolfmlClass.priv_GetMaxTeeColourIndex: cardinal;
begin
  fMaxTeeColourIndex := 0;
  if (High(fCourseTeeColourArray[fCourseIndex]) > 0) then
    fMaxTeeColourIndex := High(fCourseTeeColourArray[fCourseIndex]);
  Result := fMaxTeeColourIndex;
end;
// *********************************************************************
function TGolfmlClass.priv_GetMaxCourseIndex: cardinal;
begin
  fMaxCourseIndex := 0;
  if (High(fCourseNameArray) > 0) then
    fMaxCourseIndex := High(fCourseNameArray);
  Result := fMaxCourseIndex;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetIncludeScoreCardStylesheetReference(aBoolean: boolean);
begin
  fIncludeScoreCardStylesheetReference := aBoolean;
  if fIncludeScoreCardStylesheetReference = True then
    fIncludePlayerScoreCardStylesheetReference := False;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetIncludePlayerScoreCardStylesheetReference(
  aBoolean: boolean);
begin
  fIncludePlayerScoreCardStylesheetReference := aBoolean;
  if fIncludePlayerScoreCardStylesheetReference = True then
    fIncludeScoreCardStylesheetReference := False;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetScoreCardPlayerName(aString: string);
begin
  fScoreCardPlayerName := aString;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetScoreCardPlayerGender(aString: string);
begin
  aString := LowerCase(aString);
  if ((aString = 'male') or (aString = 'female')) then
    fScoreCardPlayerGender := aString;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetScoreCardPlayerDateOfBirth(aString: string);
begin
  fScoreCardPlayerDateOfBirth := aString;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetScoreCardPlayerHandicap(aSingle: single);
begin
  fScoreCardPlayerHandicap := aSingle;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetPlayerScoreCardXSL(aString: string);
begin
  fPlayerScoreCardXSL := aString;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetScoreCardXSL(aString: string);
begin
  fScoreCardXSL := aString;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetScoreCardTeeColour(aString: string);
begin
  fScoreCardTeeColour := aString;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetScoreCardCourseName(aString: string);
begin
  fScoreCardCourseName := aString;
end;
// *********************************************************************
end.
