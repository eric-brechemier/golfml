unit ugolfmlclass;
(*
== Author: minesadorada@charcodelvalle.com
==
== Lazarus: 0.9.30-0
== FPC: 2.4.4
==
== golfml XML Reader and Writer class
==
== Version History
== 1.0
==    Writer built and tested with GUI
== 2.0
==    Reader built and tested with GUI
==
==
*)
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,XMLRead, XMLWrite, DOM;
//  Forms, Controls, Graphics, Dialogs, Buttons, // DEBUGGING ONLY
//  StdCtrls, ComCtrls, ExtCtrls; // DEBUGGING ONLY

const
  C_GOLFMLCLASSVERSION = '2.0.20120420';
CONST C_METERS=TRUE; // fMetric
CONST C_YARDS=FALSE; // fMetric
CONST C_AMENETYDELIMITER = ','; // Used in SplitAmenity
CONST C_ERRORXMLREAD = 'An error has occurred in reading this golfml file.' + LineEnding;
// <amenety type=('practice' | 'store' | 'food' | 'corporate' | 'golfers' | 'bathroom' | 'water' | 'other' )
Type
  TAmenetyType = (practice,store,food,corporate,golfers,bathroom,water,other); // Unused V1.0
type
  TGolfmlClass = class(TObject) // A Subclass of the generic TObject
  private
    // These 3 indexing vars allow us to index into the dynamic arrays
    // to pull out properties as single values
    fCourseIndex:Cardinal;
    fHoleIndex:Cardinal;
    fTeeColourIndex:Cardinal;
    // Read properties to show maximum index values after a golfml file has been imported
    fMaxCourseIndex:Cardinal; // Set in priv_TestAndSetCourseArraySizes
    fMaxTeeColourIndex:Cardinal; // Set in priv_SetCourseIndex

        // Country Club
    fCountryClubName:String; // Property ClubName
    fCountryClubCountry:String; // Property ClubCountry
    fCountryClubCountryCode:String; // Property ClubCountryCode
    fCountryClubMunicipality:String; // Property ClubMunicipality
    fCountryClubRegion:String; // Property ClubRegion
    fCountryClubStreet:String; // Property ClubStreet
    fCountryClubPostCode:String; // Property ClubPostCode
    fCountryClubWebsite:String; // Property ClubWebsite
    fCountryClubPhone:String; // Property ClubPhone
    // Amenities and Comments are stored in dynamic arrays
    fCountryClubAmenetiesIndex:Cardinal;
    fCountryClubCommentsIndex:Cardinal;
    fMaxCountryClubAmenetiesIndex:Cardinal;
    fMaxCountryClubCommentsIndex:Cardinal;
    fCountryClubAmenetiesArray:array of String; // Indexed Property ClubAmeneties
    fCountryClubCommentsArray: array of String; // Indexed Property ClubComments


    // For Each Country Club
    fCourseNameArray: array of String;
    fCourseNumberOfHolesArray:Array of Cardinal;
    fCourseTotalParArray:Array of Cardinal;


    // For Each [Course] In Country Club
    fCourseTeeColourArray: Array of Array of String; // Tee Colour Names
    fCourseTeeTitleArray: Array of Array of String; // Tee Title Names
    fCourseTeeGenderArray: Array of Array of String; // Tee Gender Names

    fCourseParArray: Array of Array[0..17] of Cardinal;  // Course Hole Pars
    fCourseStrokeIndexArray: Array of Array[0..17] of cardinal; // Course Hole SI's

    // For Each [Tee Colour] In [Course] in Country Club
    fTeeColourCourseRatingArray:Array of Array of Single; // Tee Colour Course Ratings
    fTeeColourSlopeRatingArray: Array of Array of Cardinal; // Tee Colour Slope Ratings
    fTotalTeeDistance:Cardinal; // Property TotalTeeDistance
    fTotalTeeDistancemetres,fTotalTeeDistanceyards:Cardinal;
    fInTeeDistance:Cardinal; // Property InTeeDistance
    fInTeeDistancemetres,fInTeeDistanceyards:Cardinal;
    fOutTeeDistance:Cardinal; // Property OutTeeDistance
    fOutTeeDistancemetres,fOutTeeDistanceyards:Cardinal;

    // For Each [Tee] in [Tee Colour] In [Course] in Country Club
    fTeeDistanceArrayMeters: Array of Array of Array[0..17] of Cardinal; //Tee Distances
    fTeeDistanceArrayYards: Array of Array of Array[0..17] of Cardinal; // Tee Distances

    // Vars used in property read values
    fCourseNumberOfHoles:Cardinal; // Property CourseNumberOfHoles
    fCourseCount:Cardinal; // Property CourseCount
    fTeeColourCount:Cardinal; // Property TeeColourCount
    fMetric:Boolean; // Property Metric
    fCoursePar:Cardinal; // Property Par;
    fStrokeIndex:Cardinal; // Property Strokeindex
    fTeeDistanceMetres,fTeeDistanceYards:Cardinal; // Property Distance
    fCourseTotalPar:Cardinal; // Property CourseTotalPar
    fSlopeRating:Cardinal;
    fCourseRating:Single;
    fCourseXMLPath:String;
    fCourseLoaded:Boolean;
    fCourseName:String;
    fTeeTitle,fTeeColour,fTeeGender:String;

    fAmenityType, fAmenetyValue:String; // Only used in local procedure SplitAmenety
    procedure SplitAmenety(aString:String); // Uses C_AMENETYDELIMITER


    // These 2 procedures resize arrays on demand
    procedure priv_TestAndSetTeeColourArraySizes(aCourseIndex:Cardinal;aTeeColourIndex:Cardinal);
    procedure priv_TestAndSetCourseArraySizes(aCourseIndex:Cardinal);

    Function priv_PrettyString(aString:String):String; // Capitalises first letter
    // Various Sub-procedures for parsing Golfml document elements
    Procedure priv_ProcessXMLTee(aNode:TDomNode;TeeColour:Cardinal;sCourseName:String);
    procedure priv_ProcessXMLQualificationUSGA(aNode:TDomNode;TeeColour:Cardinal;sCourseName:String);
    procedure priv_ProcessXMLQualification(aNode:TDomNode;TeeColour:Cardinal;sCourseName:String);
    procedure priv_ProcessXMLTeeSet(aNode:TDomNode;TeeColour:Cardinal;sCourseName:String);
    procedure priv_ProcessXMLGolfCourse(aNode:TDomNode);
    procedure priv_ProcessXMLCountryClub(aNode:TDomNode);
    procedure priv_ProcessXMLCountryClubAddress(aNode:TDomNode);
    procedure priv_ProcessXMLCountryClubContact(aNode:TDomNode);
    procedure priv_ProcessXMLCountryClubAmenety(aNode:TDomNode);
    procedure priv_ProcessXMLCountryClubNote(aNode:TDomNode);
    Procedure priv_ProcessNoTeeSetsInCourse(cCourseIndex:Cardinal); // Called from priv_SetCurrentCourse


    // GETS and SETS
    procedure priv_SetCourseXMLPath(aString: string);
    procedure priv_SetCourseIndex(aCardinal:Cardinal);
    procedure priv_SetTeeColourIndex(aCardinal:Cardinal);
    procedure priv_SetHoleIndex(aCardinal:Cardinal);
    procedure priv_SetMetric(aBoolean:Boolean);

    Function priv_GetCourseName:String;
    Procedure priv_SetCourseName(aString:String);

    procedure priv_SetCoursePar(aCardinal:Cardinal);
    function priv_GetCoursePar:Cardinal;

    Procedure priv_SetCourseNumberOfHoles(aCardinal:Cardinal);
    function priv_GetCourseNumberOfHoles:Cardinal;

    Procedure priv_SetStrokeIndex(aCardinal:Cardinal);
    function priv_GetStrokeIndex:Cardinal;

    procedure priv_SetCourseRating(aSingle:Single);
    function priv_GetCourseRating:Single;

    procedure priv_SetSlopeRating(aCardinal:Cardinal);
    function priv_GetSlopeRating:Cardinal;

    procedure priv_SetTeeDistanceMetres(aCardinal:Cardinal);
    function priv_GetTeeDistanceMetres:Cardinal;

    procedure priv_SetTeeDistanceYards(aCardinal:Cardinal);
    function priv_GetTeeDistanceYards:Cardinal;

    Function priv_GetTeeTitle:String;
    Procedure priv_SetTeeTitle(aString:String);

    Function priv_GetTeeColour:String;
    Procedure priv_SetTeeColour(aString:String);

    Function priv_GetTeeGender:String;
    Procedure priv_SetTeeGender(aString:String);

    procedure priv_SetClubName(aString:String);
    procedure priv_SetClubCountry(aString:String);
    procedure priv_SetClubCountryCode (aString:String);
    procedure priv_SetClubMunicipality(aString:String);
    procedure priv_SetClubRegion(aString:String);
    procedure priv_SetClubStreet(aString:String);
    procedure priv_SetClubPostCode(aString:String);
    procedure priv_SetClubWebsite(aString:String);
    procedure priv_SetClubPhone(aString:String);

    Function priv_GetClubAmeneties(iIndex:Integer):String;
    procedure priv_SetClubAmeneties(Const iIndex:Integer;aString:String);

    Function priv_GetClubComments(iIndex:Integer):String;
    procedure priv_SetClubComments(Const iIndex:Integer;aString:String);

    procedure priv_SetTotalTeeDistance; // Refreshes property TotalTeeDistance (fTotalTeeDistance)
    Function priv_GetBogeyRating:Single; // From courserating and sloperating

    function priv_GetMaxTeeColourIndex:Cardinal;
    function priv_GetMaxCourseIndex:Cardinal;
  protected
    // PROTECTED VARS AND METHODS FOR SUBCLASS USE
    // *********************************************************************
    pVersion:String; // Version String for this class
    function prot_InitObj: boolean; // Initialises variables and arrays etc.
  public
    // PUBLIC VARS METHODS AND PROPERTIES
    constructor Create; // Note no override for Constructor
    destructor Destroy; override;
    function InitOK: boolean; // If myObj.InitOK then proceed...
    Procedure Reset; // Calls prot_InitObj

    // The main method
    function MakeGolfmlFile:Boolean;
    Function GetCourseInfoFromFile:boolean;

    // Indexed properties cannot be published in a visual component
    property ClubAmeneties[iIndex:Integer]: string read priv_GetClubAmeneties write priv_SetClubAmeneties;
    property ClubComments[iIndex:Integer]: string read priv_GetClubComments write priv_SetClubComments;

  published
    // All these could be on a visual component
    // *********************************************************************
    // Read-Write Properties
    // *********************************************************************
    Property CourseName:String read priv_GetCourseName write priv_SetCourseName;

    Property CourseRating:Single read priv_GetCourseRating write priv_SetCourseRating;
    Property BogeyRating:Single read priv_GetBogeyRating;
    Property SlopeRating:Cardinal read priv_GetSlopeRating write priv_SetSlopeRating;
    Property CourseNumberOfHoles:Cardinal read priv_GetCourseNumberOfHoles write priv_SetCourseNumberOfHoles;
    property CourseXMLPath: string read fCourseXMLPath write priv_SetCourseXMLPath; // NO CHECKING
    property CourseLoaded:Boolean read fCourseLoaded write fCourseLoaded;

    property ClubName: string read fCountryClubName write priv_SetClubName;
    property ClubCountry: string read fCountryClubCountry write priv_SetClubCountry;
    property ClubCountryCode: string read fCountryClubCountryCode write priv_SetClubCountryCode;
    property ClubMunicipality: string read fCountryClubMunicipality write priv_SetClubMunicipality;
    property ClubRegion: string read fCountryClubRegion write priv_SetClubRegion;
    property ClubStreet: string read fCountryClubStreet write priv_SetClubStreet;
    property ClubPostCode: string read fCountryClubPostCode write priv_SetClubPostCode;
    property ClubWebsite: string read fCountryClubWebsite write priv_SetClubWebsite;
    property ClubPhone: string read fCountryClubPhone write priv_SetClubPhone;
    // Change courses by changing the CourseIndex property (0-based)
    property CourseIndex:Cardinal read fCourseIndex write priv_SetCourseIndex;
    // Change Tee sets by changing the TeeColourIndex property (0-based)
    Property TeeColourIndex:Cardinal read fTeeColourIndex Write priv_SetTeeColourIndex;
    // Change Hole by changing the HoleIndex property (0-based)
    property HoleIndex:Cardinal read fHoleIndex Write priv_SetHoleIndex; // (0-17)



    Property Metric:Boolean read fMetric write priv_SetMetric; // Affects Distance properties
    Property TeeDistanceMetres:Cardinal read priv_GetTeeDistanceMetres write priv_SetTeeDistanceMetres;
    Property TeeDistanceYards:Cardinal read priv_GetTeeDistanceYards write priv_SetTeeDistanceYards;
    property TeeTitle: string read priv_GetTeeTitle write priv_SetTeeTitle;
    property TeeColour: string read priv_GetTeeColour write priv_SetTeeColour;
    property TeeGender: string read priv_GetTeeGender write priv_SetTeeGender;

    Property Par:Cardinal read priv_GetCoursePar write priv_SetCoursePar;
    Property StrokeIndex:Cardinal read priv_GetStrokeIndex write priv_SetStrokeIndex;
    // *********************************************************************
    // Read-only Properties
    // *********************************************************************
    Property Version:String read pVersion;
    // Set after a golfml file has been read from file
    Property InTeeDistance:Cardinal read fInTeeDistance; // fMetric determines metres or yards
    Property OutTeeDistance:Cardinal read fOutTeeDistance; // fMetric determines metres or yards
    Property TotalTeeDistance:Cardinal read fTotalTeeDistance; // fMetric determines metres or yards
    Property CourseTotalPar:Cardinal read fCourseTotalPar;
    // MaxIndex properties help when setting up a loop to read the indexed property values
    // e.g. For i:=0 to MaxCourseIndex do begin CourseIndex:=i; sCourseListItem:=CourseName; end;
    property MaxCourseIndex:Cardinal read priv_GetMaxCourseIndex;
    Property MaxTeeColourIndex:Cardinal read priv_GetMaxTeeColourIndex;
    Property ClubMaxAmenitiesIndex:Cardinal read fMaxCountryClubAmenetiesIndex;
    Property ClubMaxCommentsIndex:Cardinal read fMaxCountryClubCommentsIndex;

end;

implementation
// *********************************************************************
constructor TGolfmlClass.Create;
begin
     // First we call the existing TObject.Create code
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
  cCourseCount,cTeeCount,cHoleCount,cCount: cardinal;
  s: string;
begin
     SetLength(fCountryClubAmenetiesArray,1);
     SetLength(fCountryClubCommentsArray,1);

     SetLength(fCourseNameArray,1);
     SetLength(fCourseNumberOfHolesArray,1);
     SetLength(fCourseTotalParArray,1);

     SetLength(fCourseTeeColourArray,1);
     SetLength(fCourseTeeColourArray[0],1);
     SetLength(fCourseTeeTitleArray,1);
     SetLength(fCourseTeeTitleArray[0],1);
     SetLength(fCourseTeeGenderArray,1);
     SetLength(fCourseTeeGenderArray[0],1);

     SetLength(fCourseParArray,1);
     SetLength(fCourseStrokeIndexArray,1);

     SetLength(fTeeColourCourseRatingArray,1);
     SetLength(fTeeColourCourseRatingArray[0],1);
     SetLength(fTeeColourSlopeRatingArray,1);
     SetLength(fTeeColourSlopeRatingArray[0],1);

     SetLength(fTeeDistanceArrayMeters,1);
     SetLength(fTeeDistanceArrayMeters[0],1);
     SetLength(fTeeDistanceArrayYards,1);
     SetLength(fTeeDistanceArrayYards[0],1);


     fCourseNameArray[0]:='';
     fCourseNumberOfHolesArray[0]:=0;
     fCourseTotalParArray[0]:=0;
     fCourseTeeColourArray[0,0]:='';
     fCourseTeeTitleArray[0,0]:='';
     fCourseTeeGenderArray[0,0]:='';
     fTeeColourCourseRatingArray[0,0]:=0; // Course 0, TeeColour 0
     fTeeColourSlopeRatingArray[0,0]:=0; // Course 0, TeeColour 0
     fCountryClubAmenetiesArray[0]:='';
     fCountryClubCommentsArray[0]:='';


     for cHoleCount := 0 to 17 do
       begin
            fCourseParArray[0,cHoleCount] := 0;
            fCourseStrokeIndexArray[0,cHoleCount] := 0;
            fTeeDistanceArrayMeters[0,0,cHoleCount]:=0;
            fTeeDistanceArrayYards[0,0,cHoleCount]:=0;
       end;


     pVersion := C_GOLFMLCLASSVERSION;
     fCourseIndex:=0;
     fTeeColourIndex:=0;
     fMaxCourseIndex:=0;
     fMaxTeeColourIndex:=0;
     fHoleIndex:=0;
     fCourseLoaded:=FALSE;
     fInTeeDistance:=0;
     fOutTeeDistance:=0;
     fTotalTeeDistance:=0;

     fTotalTeeDistanceMetres:=0;
     fTotalTeeDistanceYards:=0;
     fInTeeDistanceMetres:=0;
     fInTeeDistanceYards:=0;
     fOutTeeDistanceMetres:=0;
     fOutTeeDistanceYards:=0;
     fCountryClubAmenetiesIndex:=0;
     fCountryClubCommentsIndex:=0;
     fMaxCountryClubAmenetiesIndex:=0;
     fMaxCountryClubCommentsIndex:=0;

     fCountryClubName:='';
     fCountryClubCountry:='';
     fCountryClubCountryCode:='';
     fCountryClubMunicipality:='';
     fCountryClubRegion:='';
     fCountryClubStreet:='';
     fCountryClubPostCode:='';
     fCountryClubWebsite:='';
     fCountryClubPhone:='';
     fCourseNumberOfHoles:=0;
     fCourseCount:=0;
     fTeeColourCount:=0;
     fMetric:=TRUE;
     fCoursePar:=0;
     fStrokeIndex:=0;
     fTeeDistanceMetres:=0;
     fTeeDistanceYards:=0;
     fCourseTotalPar:=0;
     fSlopeRating:=0;
     fCourseRating:=0;
     fCourseLoaded:=FALSE;
     fCourseName:='';;
     fTeeTitle:='';
     fTeeColour:='';
     fTeeGender:='';

     fAmenityType:='';
     fAmenetyValue:='';

     Result := True;

end;
// *********************************************************************
// PUBLIC METHODS
// *********************************************************************
Procedure TGolfmlClass.Reset;
// Easy-to-remember method name
Begin
     prot_InitObj;
end;
// *********************************************************************
function TGolfmlClass.InitOK:Boolean;
// If GolfMLClass.InitOk then.. carry on
// else FatalError;
begin
     Result:=prot_InitObj;
end;
// *********************************************************************
procedure TGolfmlClass.SplitAmenety(aString:String);
// Returns fAmenityType, fAmenetyValue
// Uses C_AMENETYDELIMITER
Var
   i:Integer;
begin
     if Length(aString)=0 then exit;
     i:=Pos(C_AMENETYDELIMITER,aString);
     fAmenityType:=LeftStr(aString,i);
     fAmenetyValue:=RightStr(aString,Length(aString)-i);
end;
// *********************************************************************
function TGolfmlClass.MakeGolfmlFile:Boolean;
// Construct a new XML file from data in the private arrays
var
  Doc: TXMLDocument;
  RootNode, CountryClubNode,OuterNode,InnerNode,InnerInnerNode,TextNode: TDOMNode;
  GolfCourseNode,TeeSetNode:TDOMNode;
  cOuterLoop,cInnerLoop,cLoop:Cardinal;
  cCourseLoop,cTeePositionLoop,cHoleLoop:Cardinal;
begin
     Result:=False; // Any early exit sets the result as FALSE
     fCourseLoaded:=False; // Set up for correct property access
TRY
     TRY
       // Create a document
       Doc := TXMLDocument.Create;
       // Create a root node with attributes
       RootNode := Doc.CreateElement('golfml');
       TDOMElement(RootNode).SetAttribute('xmlns', 'xmlns=http://code.google.com/p/golfml');
       TDOMElement(RootNode).SetAttribute('xmlns:xsi', 'http://www.w3.org/2001/XMLSchema-instance');
       TDOMElement(RootNode).SetAttribute('xmlns:g', 'http://code.google.com/p/golfml');
       TDOMElement(RootNode).SetAttribute('xsi:schemaLocation', 'http://code.google.com/p/golfml/source/browse/golfml/schemas/golfml.xsd');
       TDOMElement(RootNode).SetAttribute('version', '0.9');
       // Add <golfml> to the document
       Doc.Appendchild(RootNode);
       RootNode:= Doc.DocumentElement;

       // *********************************************************************
       // Country Club Section
       // *********************************************************************
       CountryClubNode:=Doc.CreateElement('country-club');
       // *********************************************************************
       // <name>String</name>
       OuterNode:=Doc.CreateElement('name');
       TextNode:=Doc.CreateTextNode(fCountryClubName);
       OuterNode.AppendChild(TextNode);
       CountryClubNode.AppendChild(OuterNode);
       // *********************************************************************
       // <note><comment></comment></note>
       OuterNode:=Doc.CreateElement('note');
       If Length(fCountryClubCommentsArray[0]) > 0 then
           begin
               For cLoop:=0 to High(fCountryClubCommentsArray) do
                 begin
                      InnerNode:=Doc.CreateElement('comment');
                      TDOMElement(InnerNode).SetAttribute('lang',fCountryClubCountryCode);
                      TextNode:=Doc.CreateTextNode(fCountryClubCommentsArray[cLoop]);
                      InnerNode.AppendChild(TextNode);
                      OuterNode.AppendChild(InnerNode);
                 end;
           end;
           // Add 'Created by Golfml Coursewriter' comment
           InnerNode:=Doc.CreateElement('comment');
           TDOMElement(InnerNode).SetAttribute('lang','en');
           TextNode:=Doc.CreateTextNode('Created by Golfml Coursewriter');
           InnerNode.AppendChild(TextNode);
           OuterNode.AppendChild(InnerNode);

           InnerNode:=Doc.CreateElement('comment');
           TDOMElement(InnerNode).SetAttribute('lang','en');
           TextNode:=Doc.CreateTextNode('http://www.charcodelvalle.com/golfmlweb/');
           InnerNode.AppendChild(TextNode);
           OuterNode.AppendChild(InnerNode);

           CountryClubNode.AppendChild(OuterNode);
       // *********************************************************************
       // <address><><></address>
       OuterNode:=Doc.CreateElement('address');
       If Length(fCountryClubStreet) > 0 then
           begin
                InnerNode:=Doc.CreateElement('street');
                TextNode:=Doc.CreateTextNode(fCountryClubStreet);
                InnerNode.AppendChild(TextNode);
                OuterNode.AppendChild(InnerNode);
                CountryClubNode.AppendChild(OuterNode);
           end;
       If Length(fCountryClubMunicipality) > 0 then
           begin
                InnerNode:=Doc.CreateElement('municipality');
                TextNode:=Doc.CreateTextNode(fCountryClubMunicipality);
                InnerNode.AppendChild(TextNode);
                OuterNode.AppendChild(InnerNode);
                CountryClubNode.AppendChild(OuterNode);
           end;
       If Length(fCountryClubRegion) > 0 then
           begin
                InnerNode:=Doc.CreateElement('region');
                TextNode:=Doc.CreateTextNode(fCountryClubRegion);
                InnerNode.AppendChild(TextNode);
                OuterNode.AppendChild(InnerNode);
                CountryClubNode.AppendChild(OuterNode);
           end;
       If Length(fCountryClubPostCode) > 0 then
           begin
                InnerNode:=Doc.CreateElement('postal-code');
                TextNode:=Doc.CreateTextNode(fCountryClubPostCode);
                InnerNode.AppendChild(TextNode);
                OuterNode.AppendChild(InnerNode);
                CountryClubNode.AppendChild(OuterNode);
           end;
       If Length(fCountryClubCountry) > 0 then
           begin
                InnerNode:=Doc.CreateElement('country');
                TDOMElement(InnerNode).SetAttribute('code',fCountryClubCountryCode);
                TextNode:=Doc.CreateTextNode(fCountryClubCountry);
                InnerNode.AppendChild(TextNode);
                OuterNode.AppendChild(InnerNode);
                CountryClubNode.AppendChild(OuterNode);
           end;

       If Length(fCountryClubWebsite) > 0 then
           begin
                InnerNode:=Doc.CreateElement('website');
                TextNode:=Doc.CreateTextNode(fCountryClubWebsite);
                InnerNode.AppendChild(TextNode);
                OuterNode.AppendChild(InnerNode);
                CountryClubNode.AppendChild(OuterNode);
           end;
       // *********************************************************************
       // <contact>
       If Length(fCountryClubPhone) > 0 then
           begin
                OuterNode:=Doc.CreateElement('contact');
                TDOMElement(OuterNode).SetAttribute('type','club-house');
                InnerNode:=Doc.CreateElement('phone');
                TextNode:=Doc.CreateTextNode(fCountryClubPhone);
                InnerNode.AppendChild(TextNode);
                OuterNode.AppendChild(InnerNode);
                CountryClubNode.AppendChild(OuterNode);
           end;

       // *********************************************************************
       // <amenety type=>
       If Length(fCountryClubAmenetiesArray[0]) > 0 then
           begin
               fAmenityType:='';
               fAmenetyValue:='';
                For cLoop:=0 to High(fCountryClubAmenetiesArray) do
                    begin
                         SplitAmenety(fCountryClubAmenetiesArray[cLoop]);
                         OuterNode:=Doc.CreateElement('amenety');
                         TDOMElement(OuterNode).SetAttribute('type',fAmenityType);
                         TextNode:=Doc.CreateTextNode(fAmenetyValue);
                         OuterNode.AppendChild(TextNode);
                         CountryClubNode.AppendChild(OuterNode);
                    end;
           end;

       // *********************************************************************
       // Golf Courses Club Section
       // *********************************************************************
       // Golf Course Name
       For cCourseLoop:=0 to High(fCourseNameArray) do
           Begin
                GolfCourseNode:=Doc.CreateElement('golf-course');
                OuterNode:=Doc.CreateElement('name');
                TextNode:=Doc.CreateTextNode(fCourseNameArray[cCourseLoop]);
                OuterNode.AppendChild(TextNode);
                GolfCourseNode.AppendChild(OuterNode);
                For cTeePositionLoop:=0 to High(fCourseTeeColourArray[cCourseLoop]) do
                    begin
                         // <Tee-Set>s
                         If Length(fCourseTeeColourArray[cCourseLoop,cTeePositionLoop])=0 then continue;

                         TeeSetNode:=Doc.CreateElement('tee-set');
                         TDOMElement(TeeSetNode).SetAttribute('name',fCourseTeeTitleArray[cCourseLoop,cTeePositionLoop]);
                         TDOMElement(TeeSetNode).SetAttribute('gender',fCourseTeeGenderArray[cCourseLoop,cTeePositionLoop]);
                         TDOMElement(TeeSetNode).SetAttribute('colour',fCourseTeeColourArray[cCourseLoop,cTeePositionLoop]);
                         GolfCourseNode.AppendChild(TeeSetNode);

                         // <qualification> Course Rating and Slope Rating
                         OuterNode:=Doc.CreateElement('qualification');
                         InnerNode:=Doc.CreateElement('qualification-usga');
                         InnerInnerNode:=Doc.CreateElement('rating');
                         TextNode:=Doc.CreateTextNode(Format('%0.2f',[fTeeColourCourseRatingArray[cCourseLoop,cTeePositionLoop]]));
                         InnerInnerNode.AppendChild(TextNode);
                         InnerNode.AppendChild(InnerInnerNode);
                         InnerInnerNode:=Doc.CreateElement('slope');
                         TextNode:=Doc.CreateTextNode(Format('%d',[fTeeColourSlopeRatingArray[cCourseLoop,cTeePositionLoop]]));
                         InnerInnerNode.AppendChild(TextNode);
                         InnerNode.AppendChild(InnerInnerNode);
                         OuterNode.AppendChild(InnerNode);
                         TeeSetNode.AppendChild(OuterNode);

                         // <Tee>s
                         For cHoleLoop:=0 to High(fCourseParArray[cCourseLoop]) do
                             begin
                                  If fCourseParArray[cCourseLoop,cHoleLoop]=0 then continue;

                                  OuterNode:=Doc.CreateElement('tee');
                                  TDOMElement(OuterNode).SetAttribute('number',IntToStr(cHoleLoop+1));
                                  // Par
                                  InnerNode:=Doc.CreateElement('par');
                                  TextNode:=Doc.CreateTextNode(Format('%d',[fCourseParArray[cCourseLoop,cHoleLoop]]));
                                  InnerNode.AppendChild(TextNode);
                                  OuterNode.AppendChild(InnerNode);
                                  // S.I.
                                  InnerNode:=Doc.CreateElement('handicap-stroke');
                                  TextNode:=Doc.CreateTextNode(Format('%d',[fCourseStrokeIndexArray[cCourseLoop,cHoleLoop]]));
                                  InnerNode.AppendChild(TextNode);
                                  OuterNode.AppendChild(InnerNode);
                                  // Distance Metres
                                  InnerNode:=Doc.CreateElement('length');
                                  TDOMElement(InnerNode).SetAttribute('units','meters');
                                  TextNode:=Doc.CreateTextNode(Format('%d',[fTeeDistanceArrayMeters[cCourseLoop,cTeePositionLoop,cHoleLoop]]));
                                  InnerNode.AppendChild(TextNode);
                                  OuterNode.AppendChild(InnerNode);

                                  // Distance Yards
                                  InnerNode:=Doc.CreateElement('length');
                                  TDOMElement(InnerNode).SetAttribute('units','yards');
                                  TextNode:=Doc.CreateTextNode(Format('%d',[fTeeDistanceArrayYards[cCourseLoop,cTeePositionLoop,cHoleLoop]]));
                                  InnerNode.AppendChild(TextNode);
                                  OuterNode.AppendChild(InnerNode);

                                  TeeSetNode.AppendChild(OuterNode);
                             end;
                         GolfCourseNode.AppendChild(TeeSetNode);
                    end;
                // Close Golf Course
                CountryClubNode.AppendChild(GolfCourseNode);
           end;

       // *********************************************************************
       // Application Section
       // *********************************************************************
       OuterNode:=Doc.CreateElement('application');
         TDOMElement(OuterNode).SetAttribute('dotname','com.scorecard.app');
         TDOMElement(OuterNode).SetAttribute('name','scorecard application');
         TDOMElement(OuterNode).SetAttribute('xmlns:s','http://app.scorecard.com/golfml');
       InnerNode:=Doc.CreateElement('s:preference');
           TDOMElement(InnerNode).SetAttribute('name','handicap-method');
           TextNode:=Doc.CreateTextNode('usga');
           InnerNode.AppendChild(TextNode);
           OuterNode.AppendChild(InnerNode);

       InnerNode:=Doc.CreateElement('s:preference');
           TDOMElement(InnerNode).SetAttribute('name','units-system');
           TextNode:=Doc.CreateTextNode('metric');
           InnerNode.AppendChild(TextNode);
           OuterNode.AppendChild(InnerNode);
       CountryClubNode.AppendChild(OuterNode);


       // *********************************************************************
       // Version Section
       // *********************************************************************
       OuterNode:=Doc.CreateElement('version');
         TDOMElement(OuterNode).SetAttribute('created',DateTimeToStr(Now));
         TDOMElement(OuterNode).SetAttribute('version',C_GOLFMLCLASSVERSION);
       CountryClubNode.AppendChild(OuterNode);


       // Write and close <country-club>
       RootNode.AppendChild(CountryClubNode);


       WriteXMLFile(Doc,fCourseXMLPath);
       Result:=True;
       FINALLY
              Doc.Free;
       END;

EXCEPT
  On E: Exception do Raise;
END;
end;
// *********************************************************************
Function TGolfmlClass.GetCourseInfoFromFile:boolean;
// Call Reset before changing the xml and calling this.
// Be sure fCourseXMLPath is set and checked first!
// Sets up the multi-dimensional f-arrays (all courses) and all other properties
// by parsing the golfxml via a series of nested loops.
Var
   Doc: TXMLDocument;
   CurrentNode: TDOMNode;
   cCount:Cardinal;
begin
TRY
     Result:=FALSE; // assume failure
     // Reset;
     fCourseIndex:=0;
     fTeeColourIndex:=0;
     fHoleIndex:=0;
     ReadXMLFile(Doc, fCourseXMLPath);
       CurrentNode:=Doc.DocumentElement.FirstChild;
       // CurrentNode.NodeName=<country-club>
       priv_ProcessXMLCountryClub(CurrentNode);
       // Only the first <country-club> node is processed in this class.

       CurrentNode:=CurrentNode.FirstChild; // Look at children of <country-club>

       While Assigned(CurrentNode) do
            begin
                 // Loop though the <country-club> siblings
                 if CurrentNode.NodeName='golf-course' then
                   begin
                       priv_TestAndSetCourseArraySizes(fCourseIndex); // Increase if necessary
                       priv_ProcessXMLGolfCourse(CurrentNode);
                       Inc(fCourseIndex);
                       fTeeColourIndex:=0; // New TeeColourSet
                   end;
                 // ..Next child of <country-club>
                 CurrentNode:=CurrentNode.NextSibling;
            end;
            Result:=TRUE; // Success!
FINALLY
       Doc.Free;
END;
    fCourseIndex:=0;
    fCourseLoaded:=TRUE;
    // Check for missing tee-set node
    For cCount:=0 to High(fCourseTeeColourArray) do
        If High(fCourseTeeColourArray[cCount])=-1 then priv_ProcessNoTeeSetsInCourse(cCount);
    priv_SetCourseIndex(fCourseIndex); // Sets fMax properties
end;

// *********************************************************************
// PRIVATE METHODS
// *********************************************************************
Function TGolfmlClass.priv_GetBogeyRating:Single;
// From pcourserating and psloperating
// CALCULATING BOGEY RATING from Course Rating and Slope Rating
// Bogey rating minus Course Rating (C) multiplied by (5.381 men, 4.24 women)(K)  equals Slope Rating (S).
// (B-C) * K = S
// B-C = S/K
// B=(S/K) * C
Var
   bLadies:Boolean;
   ftempCourseRating:Single;
   ctempSlopeRating:Cardinal;
begin
     Result:=0;
     // Trap for crap 1
     If fCourseIndex > priv_GetMaxCourseIndex then Exit;
     If fTeeColourIndex > priv_GetMaxTeeColourIndex then Exit;
     ftempCourseRating:=fTeeColourCourseRatingArray[fCourseIndex,fTeeColourIndex];
     ctempSlopeRating:=fTeeColourSlopeRatingArray[fCourseIndex,fTeeColourIndex];

     // Trap for crap 2
     If (ctempSlopeRating=0) OR (ftempCourseRating=0) then Exit;

     bLadies:=False;
     If CompareText(fCourseTeeGenderArray[fCourseIndex,fTeeColourIndex],'ladies')=0 then bLadies:=True;
     If bLadies then
        Result := (ctempSlopeRating/4.24) + ftempCourseRating
     else
         Result := (ctempSlopeRating/5.381) + ftempCourseRating;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetTotalTeeDistance;
// Called when setting a new tee position from priv_SetTeeColourIndex
// Sets InTeeDistance,OutTeeDistance and TotalTeeDistance properties
// taking into account fMetric=C_METRES or fMetric=C_YARDS
// If yards set but not metres (or vice versa), then automatically converts.
// note: priv_SetMetric can also re-assign the properties directly
Var
   cHoleCount:Cardinal;
begin
fInTeeDistance:=0;
fOutTeeDistance:=0;
fTotalTeeDistance:=0;

fTotalTeeDistanceMetres:=0;
fTotalTeeDistanceYards:=0;
fInTeeDistanceMetres:=0;
fInTeeDistanceYards:=0;
fOutTeeDistanceMetres:=0;
fOutTeeDistanceYards:=0;
for cHoleCount := 0 to 8 do
    begin
         Inc(fInTeeDistanceMetres,fTeeDistanceArrayMeters[fCourseIndex,fTeeColourIndex,cHoleCount]);
         Inc(fInTeeDistanceYards,fTeeDistanceArrayYards[fCourseIndex,fTeeColourIndex,cHoleCount]);
    end;
If fCourseNumberOfHoles > 8 then
  begin
       for cHoleCount := 9 to 17 do
       begin
         Inc(fOutTeeDistanceMetres,fTeeDistanceArrayMeters[fCourseIndex,fTeeColourIndex,cHoleCount]);
         Inc(fOutTeeDistanceYards,fTeeDistanceArrayYards[fCourseIndex,fTeeColourIndex,cHoleCount]);
       end;
  end;
// Fetch Total
fTotalTeeDistanceMetres:=fInTeeDistanceMetres + fOutTeeDistanceMetres;
fTotalTeeDistanceYards:=fInTeeDistanceYards + fOutTeeDistanceYards;

// If both yards and metres are set to nonzero, then do nothing, else convert on-the-fly
// Fetch metres from yards?
If (fMetric=C_METERS)
AND (fTotalTeeDistanceMetres = 0)
AND (fTotalTeeDistanceYards > 0)
THEN
  begin
      fInTeeDistanceMetres:=TRUNC((fInTeeDistanceYards * 0.9144) + 0.5);
      fOutTeeDistanceMetres:=TRUNC((fOutTeeDistanceYards * 0.9144) + 0.5);
  end;

// Fetch yards from metres?
If (fMetric=C_YARDS)
AND (fTotalTeeDistanceYards = 0)
AND (fTotalTeeDistanceMetres > 0)
THEN
    begin
      fInTeeDistanceYards:=TRUNC((fInTeeDistanceMetres * 1.0936133) + 0.5);
      fOutTeeDistanceYards:=TRUNC((fOutTeeDistanceMetres * 1.0936133) + 0.5);
  end;

// Fetch Total again (possible rounding differences)
fTotalTeeDistanceMetres:=fInTeeDistanceMetres + fOutTeeDistanceMetres;
fTotalTeeDistanceYards:=fInTeeDistanceYards + fOutTeeDistanceYards;

// Assign Total Distance property vars
if fMetric=C_METERS then
   begin
        fInTeeDistance:=fInTeeDistanceMetres;
        fOutTeeDistance:=fOutTeeDistanceMetres;
        fTotalTeeDistance:=fTotalTeeDistanceMetres;
   end
else
    begin
         fInTeeDistance:=fInTeeDistanceYards;
         fOutTeeDistance:=fOutTeeDistanceYards;
         fTotalTeeDistance:=fTotalTeeDistanceYards;
    end;

end;
// *********************************************************************
Procedure TGolfmlClass.priv_ProcessNoTeeSetsInCourse(cCourseIndex:Cardinal);
Var cCount:Cardinal;
begin
     // SET UP DUMMY INFO FOR A COURSE WITH A NAME BUT NO TEE SETS (COLOURS) HERE
     // ShowMessageFmt('No data for cCourseIndex=%d',[cCourseIndex]);
     // cCourseIndex is the index of the course (fCourseNameArray) with a name but no data

     // 1 ) Make a dummy TeeColourArray,TeeTitleArray,TeeGenderArray entry for the course
     SetLength(fCourseTeeColourArray[cCourseIndex],1);
     fCourseTeeColourArray[cCourseIndex,0]:='No tee data';
     SetLength(fCourseTeeTitleArray[cCourseIndex],1);
     fCourseTeeTitleArray[cCourseIndex,0]:='No tee data';
     SetLength(fCourseTeeGenderArray[cCourseIndex],1);
     fCourseTeeGenderArray[cCourseIndex,0]:='No tee data';
     // 2 ) Make a Course Rating and Slope Rating for the dummy tee colour
     SetLength(fTeeColourCourseRatingArray[cCourseIndex],1);
     fTeeColourCourseRatingArray[cCourseIndex,0]:=0;
     SetLength(fTeeColourSlopeRatingArray[cCourseIndex],1);
     fTeeColourSlopeRatingArray[cCourseIndex,0]:=0;

     // 3 ) Make tee distances for the dummy tee colour
     SetLength(fTeeDistanceArrayMeters[cCourseIndex],1);
     SetLength(fTeeDistanceArrayYards[cCourseIndex],1);
     For cCount:=0 to 17 do
         begin
              fTeeDistanceArrayMeters[cCourseIndex,0,cCount]:=0;
              fTeeDistanceArrayYards[cCourseIndex,0,cCount]:=0;
         end;
end;
// *********************************************************************
Function TGolfmlClass.priv_PrettyString(aString:String):String;
// Capitalises first letter
// Used to make tee attributes (usually all-lowercase) display nicely
Var
   sFirst,sRest:String;
begin
     Result:=aString;
     If Length(aString) > 1 then
       begin
          sFirst:=UpperCase(LeftStr(AString,1));
          sRest:=RightStr(AString,Length(AString)-1);
          Result:=sFirst + sRest;
       end;
end;
// *********************************************************************
// Various XML Subroutines
Procedure TGolfmlClass.priv_ProcessXMLTee(aNode:TDomNode;TeeColour:Cardinal;sCourseName:String);
Var CurrentNode:TDOMNode;
    iAttributeCount,iInteger,iTeeNumber:Integer;
    aString:String;
begin
// CurrentNode=<tee>
// <tee> has attributes and sub-node values
  CurrentNode:=aNode;
  if CurrentNode.HasAttributes and (CurrentNode.Attributes.Length > 0) then
    For iAttributeCount:=0 to CurrentNode.Attributes.Length-1 do
      begin
           If CurrentNode.Attributes[iAttributeCount].NodeName='number' then
           begin
                aString:='0' + CurrentNode.Attributes[iAttributeCount].NodeValue;
                If TryStrToInt(aString,iInteger)=TRUE then
                   iTeeNumber:=StrToInt(aString)
                else
                   begin raise Exception.Create(C_ERRORXMLREAD + 'Error: Tee number attribute is invalid');Exit;End;
           end;
      end;
  // Now look at children of <tee>
   CurrentNode:=aNode.FirstChild;
   While Assigned(CurrentNode) do
   begin
        if CurrentNode.NodeName='par' then
          begin
               aString:='0' + CurrentNode.TextContent;
               If TryStrToInt(aString,iInteger)=TRUE then
                 fCourseParArray[fCourseIndex,iTeeNumber-1]:=StrToInt(aString)
               else fCourseParArray[fCourseIndex,iTeeNumber-1]:=0;
          end;
        if CurrentNode.NodeName='handicap-stroke' then
          begin
               aString:='0' + CurrentNode.TextContent;
               If TryStrToInt(aString,iInteger)=TRUE then
                  fCourseStrokeIndexArray[fCourseIndex,iTeeNumber-1]:=StrToInt(aString)
               else
               begin
                    fCourseStrokeIndexArray[fCourseIndex,iTeeNumber-1]:=0;
                    raise Exception.Create(C_ERRORXMLREAD + 'Error: A Stroke Index element is not a number');
               end;
          end;
        if CurrentNode.NodeName='length' then
            if CurrentNode.HasAttributes and (CurrentNode.Attributes.Length > 0) then
                For iAttributeCount:=0 to CurrentNode.Attributes.Length-1 do
                  begin
                       If CurrentNode.Attributes[iAttributeCount].NodeName='units' then
                         begin
                           if  CurrentNode.Attributes[iAttributeCount].NodeValue='meters' then
                            begin
                                 aString:='0' + CurrentNode.TextContent;
                                 If TryStrToInt(aString,iInteger)=TRUE then
                                    fTeeDistanceArrayMeters[fCourseIndex,TeeColour,iTeeNumber-1]:=StrToInt(aString)
                                 else
                                 begin
                                      fTeeDistanceArrayMeters[fCourseIndex,TeeColour,iTeeNumber-1]:=0;
                                      raise Exception.Create(C_ERRORXMLREAD + 'Error: A Distance element (meters) is invalid');
                                 end;
                            end;
                           if  CurrentNode.Attributes[iAttributeCount].NodeValue='yards' then
                                begin
                                     aString:='0' + CurrentNode.TextContent;
                                     If TryStrToInt(aString,iInteger)=TRUE then
                                        fTeeDistanceArrayYards[fCourseIndex,TeeColour,iTeeNumber-1]:=StrToInt(aString)
                                     else
                                     begin
                                          fTeeDistanceArrayYards[fCourseIndex,TeeColour,iTeeNumber-1]:=0;
                                          raise Exception.Create(C_ERRORXMLREAD + 'Error: A Distance element (yards) is invalid');
                                     end;
                                end;
                         end;
                  end;
        CurrentNode:=CurrentNode.NextSibling;
   end;
end;
// *********************************************************************
procedure TGolfmlClass.priv_ProcessXMLQualificationUSGA(aNode:TDomNode;TeeColour:Cardinal;sCourseName:String);
Var
   CurrentNode:TDOMNode;
   aString:String;
   iInteger:Integer;
   fFloat:Single;
begin
// CurrentNode=<qualification-usga>
// Now look at children of <qualification-usga>
 CurrentNode:=aNode.FirstChild;
 While Assigned(CurrentNode) do
 begin
      if CurrentNode.NodeName='rating' then
         begin
              AString:='0' + CurrentNode.TextContent;
              If TryStrToFloat(AString,fFloat)=TRUE then
                  fTeeColourCourseRatingArray[fCourseIndex,TeeColour]:=StrToFloat(AString)
              else
              begin
                   fTeeColourCourseRatingArray[fCourseIndex,TeeColour]:=0;
                   raise Exception.Create(C_ERRORXMLREAD + 'Error: A Course Rating element is invalid');
              end;

         end;
      if CurrentNode.NodeName='slope' then
         begin
           AString:='0' + CurrentNode.TextContent;
           If TryStrToInt(AString,iInteger)=TRUE then
               fTeeColourSlopeRatingArray[fCourseIndex,TeeColour]:=StrToInt(AString)
               else
           begin
                fTeeColourSlopeRatingArray[fCourseIndex,TeeColour]:=0;
                raise Exception.Create(C_ERRORXMLREAD + 'Error: A Slope Rating element is invalid');
           end;
         end;
      CurrentNode:=CurrentNode.NextSibling;
 end;

end;
// *********************************************************************
procedure TGolfmlClass.priv_ProcessXMLQualification(aNode:TDomNode;TeeColour:Cardinal;sCourseName:String);
Var CurrentNode:TDOMNode;
begin
// CurrentNode=<qualification>
// Now look at children of <qualification>
 CurrentNode:=aNode.FirstChild;
 While Assigned(CurrentNode) do
 begin
      if CurrentNode.NodeName='qualification-usga' then priv_ProcessXMLQualificationUSGA(CurrentNode,TeeColour,sCourseName);
      CurrentNode:=CurrentNode.NextSibling;
 end;
end;
// *********************************************************************
procedure TGolfmlClass.priv_ProcessXMLTeeSet(aNode:TDomNode;TeeColour:Cardinal;sCourseName:String);
Var CurrentNode:TDOMNode;
begin
// CurrentNode=<tee-set>
// Now look at children of <tee-set>
 CurrentNode:=aNode.FirstChild;
 While Assigned(CurrentNode) do
   begin
        if CurrentNode.NodeName='qualification' then priv_ProcessXMLQualification(CurrentNode,TeeColour,sCourseName);
        if CurrentNode.NodeName='tee' then priv_ProcessXMLTee(CurrentNode,TeeColour,sCourseName);
        CurrentNode:=CurrentNode.NextSibling;
   end;
end;
// *********************************************************************
procedure TGolfmlClass.priv_ProcessXMLGolfCourse(aNode:TDomNode);
// <golf-course> is a node with multiple sub-nodes of <tee-set>
Var
   CurrentNode:TDOMNode;
   iAttributeCount:Integer;
   sCourseName:String;
   cCount:Cardinal;
begin
  // CurrentNode=<golf-course>
  // Now look at children of <golf-course>
   CurrentNode:=aNode.FirstChild;
   While Assigned(CurrentNode) do
     begin
          if CurrentNode.NodeName='name' then
             begin
                  sCourseName:=CurrentNode.TextContent;
                  fCourseNameArray[fCourseIndex]:=sCourseName;  // Golf course name
             end;
          if CurrentNode.NodeName='tee-set' then
            begin
              if CurrentNode.HasAttributes and (CurrentNode.Attributes.Length > 0) then
                begin
                     priv_TestAndSetTeeColourArraySizes(fCourseIndex,fTeeColourIndex);
                     // Increases array sizes if needed

                     For iAttributeCount:=0 to CurrentNode.Attributes.Length-1 do
                     begin
                         If CurrentNode.Attributes[iAttributeCount].NodeName='colour' then
                            fCourseTeeColourArray[fCourseIndex,fTeeColourIndex]:=CurrentNode.Attributes[iAttributeCount].NodeValue;
                         If CurrentNode.Attributes[iAttributeCount].NodeName='gender' then
                            fCourseTeeGenderArray[fCourseIndex,fTeeColourIndex]:=CurrentNode.Attributes[iAttributeCount].NodeValue;
                         If CurrentNode.Attributes[iAttributeCount].NodeName='name' then
                            fCourseTeeTitleArray[fCourseIndex,fTeeColourIndex]:=CurrentNode.Attributes[iAttributeCount].NodeValue;
                     end; // of for.. loop
                     priv_ProcessXMLTeeSet(CurrentNode,fTeeColourIndex,sCourseName);
                     Inc(fTeeColourIndex,1);
                 end; // of hasattributes
            end; // of NodeName=tee-set
          CurrentNode:=CurrentNode.NextSibling;
     end;
     // ShowMessageFmt('priv_ProcessXMLGolfCourse: High(fCourseTeeColourArray[fCourseIndex])=%d',[High(fCourseTeeColourArray[fCourseIndex])]);
end;
// *********************************************************************
procedure TGolfmlClass.priv_ProcessXMLCountryClubAddress(aNode:TDomNode);
Var
   CurrentNode:TDOMNode;
   iAttributeCount:Cardinal;
begin
     CurrentNode:=ANode; // <address>
     CurrentNode:=CurrentNode.FirstChild; // Look at children of <address>
     While Assigned(CurrentNode) do
       begin
           if CurrentNode.NodeName='street' then
             If Length(CurrentNode.TextContent) > 0 then fCountryClubStreet:=CurrentNode.TextContent;
           if CurrentNode.NodeName='postal-code' then
             If Length(CurrentNode.TextContent) > 0 then fCountryClubPostCode:=CurrentNode.TextContent;
           if CurrentNode.NodeName='municipality' then
             If Length(CurrentNode.TextContent) > 0 then fCountryClubMunicipality:=CurrentNode.TextContent;
           if CurrentNode.NodeName='region' then
             If Length(CurrentNode.TextContent) > 0 then fCountryClubRegion:=CurrentNode.TextContent;
           if CurrentNode.NodeName='website' then
             If Length(CurrentNode.TextContent) > 0 then fCountryClubWebsite:=CurrentNode.TextContent;
           if CurrentNode.NodeName='country' then
             If Length(CurrentNode.TextContent) > 0 then fCountryClubcountry:=CurrentNode.TextContent;
           if CurrentNode.HasAttributes and (CurrentNode.Attributes.Length > 0) then
               For iAttributeCount:=0 to CurrentNode.Attributes.Length-1 do
                 begin
                      If CurrentNode.Attributes[iAttributeCount].NodeName='code' then
                        fCountryClubCountryCode:=CurrentNode.Attributes[iAttributeCount].NodeValue;
                 end;
           CurrentNode:=CurrentNode.NextSibling;
       end;

end;
// *********************************************************************
procedure TGolfmlClass.priv_ProcessXMLCountryClubContact(aNode:TDomNode);
Var
   CurrentNode:TDOMNode;
begin
     CurrentNode:=ANode; // <contact>
     CurrentNode:=CurrentNode.FirstChild; // Look at children of <contact>
     While Assigned(CurrentNode) do
       begin
           if CurrentNode.NodeName='phone' then
             If Length(CurrentNode.TextContent) > 0 then fCountryClubPhone:=CurrentNode.TextContent;
           CurrentNode:=CurrentNode.NextSibling;
       end;
end;
// *********************************************************************
procedure TGolfmlClass.priv_ProcessXMLCountryClubAmenety(aNode:TDomNode);
Var
   CurrentNode:TDOMNode;
begin
     CurrentNode:=ANode; // <amenety>
     If Length(CurrentNode.TextContent) > 0 then
       begin
         Inc(fMaxCountryClubAmenetiesIndex,1);
         SetLength(fCountryClubAmenetiesArray,fMaxCountryClubAmenetiesIndex + 1);
         fCountryClubAmenetiesArray[fMaxCountryClubAmenetiesIndex]:=CurrentNode.TextContent;
       end;
end;
// *********************************************************************
procedure TGolfmlClass.priv_ProcessXMLCountryClubNote(aNode:TDomNode);
Var
   CurrentNode:TDOMNode;
begin
     CurrentNode:=ANode; // <note>
     CurrentNode:=CurrentNode.FirstChild; // Look at children of <note>
     While Assigned(CurrentNode) do
       begin
           if CurrentNode.NodeName='comment' then
             If Length(CurrentNode.TextContent) > 0 then
               begin
                 Inc(fMaxCountryClubCommentsIndex,1);
                 SetLength(fCountryClubCommentsArray,fMaxCountryClubCommentsIndex + 1);
                 fCountryClubCommentsArray[fMaxCountryClubCommentsIndex]:=CurrentNode.TextContent;
               end;
           CurrentNode:=CurrentNode.NextSibling;
       end;
end;
// *********************************************************************
procedure TGolfmlClass.priv_ProcessXMLCountryClub(aNode:TDomNode);
// Populates a private record structure which is then assigned to properties
Var
   CurrentNode:TDOMNode;
begin
     CurrentNode:=ANode; // <country-club>
     CurrentNode:=CurrentNode.FirstChild; // Look at children of <country-club>
     While Assigned(CurrentNode) do
       begin
           if CurrentNode.NodeName='name' then fCountryClubName:=CurrentNode.TextContent;
           if CurrentNode.NodeName='address' then priv_ProcessXMLCountryClubAddress(CurrentNode);
           if CurrentNode.NodeName='contact' then priv_ProcessXMLCountryClubContact(CurrentNode);
           if CurrentNode.NodeName='amenety' then priv_ProcessXMLCountryClubAmenety(CurrentNode);
           if CurrentNode.NodeName='note' then priv_ProcessXMLCountryClubNote(CurrentNode);
           CurrentNode:=CurrentNode.NextSibling;
       end;

end;
// *********************************************************************
procedure TGolfmlClass.priv_TestAndSetCourseArraySizes(aCourseIndex:Cardinal);
// Called from priv_SetCourseIndex and others
begin
     If aCourseIndex > High(fCourseNameArray) then
       begin
            SetLength(fCourseNameArray,aCourseIndex+1);
            SetLength(fCourseNumberOfHolesArray,aCourseIndex+1);
            SetLength(fCourseTeeColourArray,aCourseIndex+1);
            SetLength(fCourseTeeTitleArray,aCourseIndex+1);
            SetLength(fCourseTeeGenderArray,aCourseIndex+1);
            SetLength(fTeeColourCourseRatingArray,aCourseIndex+1);
            SetLength(fTeeColourSlopeRatingArray,aCourseIndex+1);
            SetLength(fCourseStrokeIndexArray,aCourseIndex+1);
            SetLength(fCourseParArray,aCourseIndex+1);
            SetLength(fTeeDistanceArrayMeters,aCourseIndex+1);
            SetLength(fTeeDistanceArrayYards,aCourseIndex+1);
            SetLength(fCourseTotalParArray,aCourseIndex+1);
            fMaxCourseIndex:=aCourseIndex;
       end;
end;
// *********************************************************************
procedure TGolfmlClass.priv_TestAndSetTeeColourArraySizes(aCourseIndex:Cardinal;aTeeColourIndex:Cardinal);
// Called from priv_SetTeeColourIndex and others
Begin
     priv_TestAndSetCourseArraySizes(aCourseIndex);
     If aTeeColourIndex > High(fCourseTeeColourArray[aCourseIndex]) then
       begin
            SetLength(fCourseTeeColourArray[aCourseIndex],aTeeColourIndex+1);
            SetLength(fCourseTeeTitleArray[aCourseIndex],aTeeColourIndex+1);
            SetLength(fCourseTeeGenderArray[aCourseIndex],aTeeColourIndex+1);
            SetLength(fTeeColourCourseRatingArray[aCourseIndex],aTeeColourIndex+1);
            SetLength(fTeeColourSlopeRatingArray[aCourseIndex],aTeeColourIndex+1);
            SetLength(fTeeDistanceArrayMeters[aCourseIndex],aTeeColourIndex+1);
            SetLength(fTeeDistanceArrayYards[aCourseIndex],aTeeColourIndex+1);
            fMaxTeeColourIndex:=aTeeColourIndex;
       end;
end;

// *********************************************************************
// *********************************************************************
// GET/SETS (ALL PRIVATE METHODS)
// *********************************************************************
procedure TGolfmlClass.priv_SetCourseIndex(aCardinal:Cardinal);
// Sets 'CourseIndex' property
// Also sets 'CourseTotalPar' and 'CourseNumberofHoles' properties in golfml Load mode
Var cHoleCount:Cardinal;
Begin
     // Assign
     fCourseIndex:=aCardinal;
     priv_TestAndSetCourseArraySizes(fCourseIndex); // Increase array?
     fMaxTeeColourIndex:=priv_GetMaxTeeColourIndex;
     fCourseNumberOfHoles:=0;
     fCourseTotalPar:=0;
     // Update properties
     fMaxCourseIndex:=priv_GetMaxCourseIndex;
     fCourseCount:=High(fCourseNameArray)+1;
     if fCourseNumberOfHoles=0 then fTeeColourCount:=0;
     if fCourseLoaded then // In golfml Load Mode
       begin
            fCourseNumberOfHoles:=0;
            fCourseTotalPar:=0;
            // Set fCourseNumberOfHoles here by testing for StokeIndex > 0 for each hole
            // Set fCourseTotalParArray here by adding up the pars for this Course
            For cHoleCount:=0 to 17 do
                begin
                     if fCourseStrokeIndexArray[fCourseIndex,cHoleCount] > 0 then Inc(fCourseNumberOfHoles,1);
                     Inc(fCourseTotalParArray[fCourseIndex],fCourseParArray[fCourseIndex,cHoleCount]);
                end;
            fCourseTotalPar:=fCourseTotalParArray[fCourseIndex];
       end;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetTeeColourIndex(aCardinal:Cardinal);
// Sets 'TeeColourIndex' property
Var
    cHoleCount:Cardinal;
begin
     // Assign
     fTeeColourIndex:=aCardinal;
     priv_TestAndSetTeeColourArraySizes(fCourseIndex,fTeeColourIndex);
     fMaxTeeColourIndex:=High(fCourseTeeColourArray[fCourseIndex]);
     fTeeColourCount:=High(fCourseTeeColourArray[fCourseIndex])+1;
     if fCourseLoaded then priv_SetTotalTeeDistance; // Set TotalTeeDistance properties for this tee
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetHoleIndex(aCardinal:Cardinal);
// Sets 'HoleIndex' property (0-based)
begin
     // Trap for crap
     if (aCardinal > 17)
     then
         begin
              raise Exception.Create(Format('HoleIndex %d out of bounds (Max:%d)',[aCardinal,fCourseNumberOfHoles]));
              Exit;
         end;
     // Assign
     If (fHoleIndex <> aCardinal) then fHoleIndex:=aCardinal;  //fHoleIndex is zero-based
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetCourseXMLPath(aString: string);
// Sets 'CourseXMLPath' property
begin
     if fCourseXMLPath <> aString then fCourseXMLPath := aString;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetTeeDistanceMetres(aCardinal:Cardinal);
begin
     fTeeDistanceMetres:=aCardinal;
     priv_TestAndSetCourseArraySizes(fCourseIndex); // Increase array?
     priv_TestAndSetTeeColourArraySizes(fCourseIndex,fTeeColourIndex); // Increase Array?
     fTeeDistanceYards:=TRUNC((fTeeDistanceMetres * 1.0936133) + 0.5);
     fTeeDistanceArrayMeters[fCourseIndex,fTeeColourIndex,fHoleIndex]:=fTeeDistanceMetres;
     fTeeDistanceArrayYards[fCourseIndex,fTeeColourIndex,fHoleIndex]:=fTeeDistanceYards;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetTeeDistanceYards(aCardinal:Cardinal);
begin
     fTeeDistanceYards:=aCardinal;
     priv_TestAndSetCourseArraySizes(fCourseIndex); // Increase array?
     priv_TestAndSetTeeColourArraySizes(fCourseIndex,fTeeColourIndex); // Increase Array?
     fTeeDistanceMetres:=TRUNC((fTeeDistanceYards * 0.9144) + 0.5);
     fTeeDistanceArrayMeters[fCourseIndex,fTeeColourIndex,fHoleIndex]:=fTeeDistanceMetres;
     fTeeDistanceArrayYards[fCourseIndex,fTeeColourIndex,fHoleIndex]:=fTeeDistanceYards;
end;
// *********************************************************************
Procedure TGolfmlClass.priv_SetTeeTitle(aString:String);
begin
     if (Length(aString) = 0) then exit;
     fTeeTitle:=aString;
     priv_TestAndSetCourseArraySizes(fCourseIndex); // Increase array?
     priv_TestAndSetTeeColourArraySizes(fCourseIndex,fTeeColourIndex); // Increase Array?
     fCourseTeeTitleArray[CourseIndex,fTeeColourIndex]:=fTeeTitle
end;
// *********************************************************************
Procedure TGolfmlClass.priv_SetTeeColour(aString:String);
begin
     if (Length(aString) = 0) then exit;
     fTeeColour:=aString;
     priv_TestAndSetCourseArraySizes(fCourseIndex); // Increase array?
     priv_TestAndSetTeeColourArraySizes(fCourseIndex,fTeeColourIndex); // Increase Array?
     fCourseTeeColourArray[CourseIndex,fTeeColourIndex]:=fTeeColour
end;
// *********************************************************************
Procedure TGolfmlClass.priv_SetTeeGender(aString:String);
begin
     if (Length(aString) = 0) then exit;
     fTeeGender:=aString;
     priv_TestAndSetCourseArraySizes(fCourseIndex); // Increase array?
     priv_TestAndSetTeeColourArraySizes(fCourseIndex,fTeeColourIndex); // Increase Array?
     fCourseTeeGenderArray[CourseIndex,fTeeColourIndex]:=fTeeGender
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetMetric(aBoolean:Boolean);
begin
     fMetric:=aBoolean;
     If fMetric=C_METERS then
        begin
            fTotalTeeDistance:=fTotalTeeDistanceMetres;
            fInTeeDistance:=fInTeeDistanceMetres;
            fOutTeeDistance:=fOutTeeDistanceMetres;
        end
        else
        begin
           fTotalTeeDistance:=fTotalTeeDistanceYards;
           fInTeeDistance:=fInTeeDistanceYards;
           fOutTeeDistance:=fOutTeeDistanceYards;
        end;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetCoursePar(aCardinal:Cardinal);
begin
     if fCoursePar <> aCardinal then fCoursePar:=aCardinal;
     priv_TestAndSetCourseArraySizes(fCourseIndex); // Increase array?
     priv_TestAndSetTeeColourArraySizes(fCourseIndex,fTeeColourIndex); // Increase Array?
     fCourseParArray[fCourseIndex,fHoleIndex]:=fCoursePar;
end;
// *********************************************************************
Procedure TGolfmlClass.priv_SetStrokeIndex(aCardinal:Cardinal);
begin
     if fStrokeIndex <> aCardinal then fStrokeIndex:=aCardinal;
     priv_TestAndSetCourseArraySizes(fCourseIndex); // Increase array?
     priv_TestAndSetTeeColourArraySizes(fCourseIndex,fTeeColourIndex); // Increase Array?
     fCourseStrokeIndexArray[fCourseIndex,fHoleIndex]:=fStrokeIndex;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetCourseRating(aSingle:Single);
begin
     if fCourseRating <> aSingle then fCourseRating:=aSingle;
     priv_TestAndSetCourseArraySizes(fCourseIndex); // Increase array?
     priv_TestAndSetTeeColourArraySizes(fCourseIndex,fTeeColourIndex); // Increase Array?
     fTeeColourCourseRatingArray[fCourseIndex,fTeeColourIndex]:=fCourseRating;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetSlopeRating(aCardinal:Cardinal);
begin
     if fSlopeRating <> aCardinal then fSlopeRating:=aCardinal;
     priv_TestAndSetCourseArraySizes(fCourseIndex); // Increase array?
     priv_TestAndSetTeeColourArraySizes(fCourseIndex,fTeeColourIndex); // Increase Array?
     fTeeColourSlopeRatingArray[fCourseIndex,fTeeColourIndex]:=fSlopeRating;
end;
// *********************************************************************
Procedure TGolfmlClass.priv_SetCourseNumberOfHoles(aCardinal:Cardinal);
begin
     fCourseNumberOfHoles:=aCardinal;
     priv_TestAndSetCourseArraySizes(fCourseIndex); // Increase array?
     fCourseNumberOfHolesArray[fCourseIndex]:=fCourseNumberOfHoles;
end;
// *********************************************************************
Procedure TGolfmlClass.priv_SetCourseName(aString:String);
begin
     If Length(aString)=0 then Exit;
     fCourseName:=aString;
     priv_TestAndSetCourseArraySizes(fCourseIndex); // Increase array?
     fCourseNameArray[fCourseIndex]:=fCourseName;
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetClubAmeneties(Const iIndex:Integer;aString:String);
Begin
     If iIndex > High(fCountryClubAmenetiesArray) then
         SetLength(fCountryClubAmenetiesArray,iIndex+1);
     fCountryClubAmenetiesArray[iIndex]:=aString
end;
// *********************************************************************
Function TGolfmlClass.priv_GetClubAmeneties(iIndex:Integer):String;
Begin
     If iIndex > High(fCountryClubAmenetiesArray) then exit;
     Result:=fCountryClubAmenetiesArray[iIndex];
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetClubComments(Const iIndex:Integer;aString:String);
Begin
     If iIndex > High(fCountryClubCommentsArray) then
         SetLength(fCountryClubCommentsArray,iIndex+1);
     fCountryClubCommentsArray[iIndex]:=aString
end;
// *********************************************************************
Function TGolfmlClass.priv_GetClubComments(iIndex:Integer):String;
Begin
     If iIndex > High(fCountryClubCommentsArray) then exit;
     Result:=fCountryClubCommentsArray[iIndex];
end;
// *********************************************************************
procedure TGolfmlClass.priv_SetClubName(aString:String);
begin fCountryClubName:=aString; end;
// *********************************************************************
procedure TGolfmlClass.priv_SetClubCountry(aString:String);
begin fCountryClubCountry:=aString; end;
// *********************************************************************
procedure TGolfmlClass.priv_SetClubCountryCode (aString:String);
begin fCountryClubCountryCode:=aString; end;
// *********************************************************************
procedure TGolfmlClass.priv_SetClubMunicipality(aString:String);
begin fCountryClubMunicipality:=aString; end;
// *********************************************************************
procedure TGolfmlClass.priv_SetClubRegion(aString:String);
begin fCountryClubRegion:=aString; end;
// *********************************************************************
procedure TGolfmlClass.priv_SetClubStreet(aString:String);
begin fCountryClubStreet:=aString; end;
// *********************************************************************
procedure TGolfmlClass.priv_SetClubPostCode(aString:String);
begin fCountryClubPostCode:=aString; end;
// *********************************************************************
procedure TGolfmlClass.priv_SetClubWebsite(aString:String);
begin fCountryClubWebsite:=aString; end;
// *********************************************************************
procedure TGolfmlClass.priv_SetClubPhone(aString:String);
begin fCountryClubPhone:=aString; end;
// *********************************************************************
Function TGolfmlClass.priv_GetCourseName:String;
begin
     // Trap for crap
     fCourseName:='CourseIndex too high';
     Result:=fCourseName;
     if fCourseIndex > High(fCourseNameArray) then Exit;
     // Assign
     fCourseName:=fCourseNameArray[fCourseIndex];
     Result:=fCourseName;
end;
// *********************************************************************
Function TGolfmlClass.priv_GetTeeTitle:String;
begin
     // Trap for crap
     fTeeTitle:='Invalid';
     Result:=fTeeTitle;
     if fCourseIndex > priv_GetMaxCourseIndex then Exit;
     if fTeeColourIndex > priv_GetMaxTeeColourIndex then Exit;
     // Assign
     fTeeTitle:=fCourseTeeTitleArray[fCourseIndex,fTeeColourIndex];
     Result:=fTeeTitle;
end;
// *********************************************************************
Function TGolfmlClass.priv_GetTeeColour:String;
begin
     // Trap for crap
     fTeeColour:='Invalid';
     Result:=fTeeColour;
     if fCourseIndex > priv_GetMaxCourseIndex then Exit;
     if fTeeColourIndex > priv_GetMaxTeeColourIndex then Exit;
     // Assign
     fTeeColour:=fCourseTeeColourArray[fCourseIndex,fTeeColourIndex];
     Result:=fTeeColour;
end;
// *********************************************************************
Function TGolfmlClass.priv_GetTeeGender:String;
begin
     // Trap for crap
     fTeeGender:='Invalid';
     Result:=fTeeGender;
     if fCourseIndex > priv_GetMaxCourseIndex then Exit;
     if fTeeColourIndex > priv_GetMaxTeeColourIndex then Exit;
     // Assign
     fTeeGender:=fCourseTeeGenderArray[fCourseIndex,fTeeColourIndex];
     Result:=fTeeGender;
end;
// *********************************************************************
function TGolfmlClass.priv_GetCoursePar:Cardinal;
begin
     // Trap for crap
     fCoursePar:=0;
     Result:=fCoursePar;
     if fCourseIndex > priv_GetMaxCourseIndex then Exit;
     // Assign
     fCoursePar:=fCourseParArray[fCourseIndex,fHoleIndex];
     Result:=fCoursePar;
end;
// *********************************************************************
function TGolfmlClass.priv_GetStrokeIndex:Cardinal;
begin
     // Trap for crap
     fStrokeIndex:=0;
     Result:=fStrokeIndex;
     if fCourseIndex > priv_GetMaxCourseIndex then Exit;
     // Assign
     fStrokeIndex:=fCourseStrokeIndexArray[fCourseIndex,fHoleIndex];
     Result:=fStrokeIndex;
end;
// *********************************************************************
function TGolfmlClass.priv_GetCourseRating:Single;
Begin
     // Trap for crap
          fCourseRating:=0;
          Result:=fCourseRating;
          if fCourseIndex > priv_GetMaxCourseIndex then Exit;
          if fTeeColourIndex > priv_GetMaxTeeColourIndex then Exit;
          // Assign
          fCourseRating:=fTeeColourCourseRatingArray[fCourseIndex,fTeeColourIndex];
          Result:=fCourseRating;
end;
// *********************************************************************
function TGolfmlClass.priv_GetSlopeRating:Cardinal;
Begin
          // Trap for crap
          fSlopeRating:=0;
          Result:=fSlopeRating;
          if fCourseIndex > priv_GetMaxCourseIndex then Exit;
          if fTeeColourIndex > priv_GetMaxTeeColourIndex then Exit;
          // Assign
          fSlopeRating:=fTeeColourSlopeRatingArray[fCourseIndex,fTeeColourIndex];
          Result:=fSlopeRating;
end;
// *********************************************************************
function TGolfmlClass.priv_GetCourseNumberOfHoles:Cardinal;
Var cHoleCount:Cardinal;
Begin
     // Trap for crap
     fCourseNumberOfHoles:=0;
     Result:=fCourseNumberOfHoles;
     if fCourseIndex > priv_GetMaxCourseIndex then Exit;
     // Assign
     if fCourseLoaded then // In golfml Load Mode
       begin
     // Set fCourseNumberOfHoles here by testing for StokeIndex > 0 for each hole
        For cHoleCount:=0 to 17 do
            if fCourseStrokeIndexArray[fCourseIndex,cHoleCount] > 0 then Inc(fCourseNumberOfHoles,1);
       Result:=fCourseNumberOfHoles;
       end;
     if fCourseIndex > priv_GetMaxCourseIndex then Exit;
     fCourseNumberOfHolesArray[fCourseIndex]:=fCourseNumberOfHoles;
end;
// *********************************************************************
function TGolfmlClass.priv_GetTeeDistanceMetres:Cardinal;
begin
     // Trap for crap
     fTeeDistanceMetres:=0;
     Result:=fTeeDistanceMetres;
     if fCourseIndex > priv_GetMaxCourseIndex then Exit;
     if fTeeColourIndex > priv_GetMaxTeeColourIndex then Exit;
     // Assign
     fTeeDistanceYards:=fTeeDistanceArrayYards[fCourseIndex,fTeeColourIndex,fHoleIndex];
     fTeeDistanceMetres:=fTeeDistanceArrayMeters[fCourseIndex,fTeeColourIndex,fHoleIndex];
     // Fetch metres from yards?
     If (fTeeDistanceMetres = 0)
     AND (fTeeDistanceYards > 0)
     THEN fTeeDistanceMetres:=TRUNC((fTeeDistanceYards * 0.9144) + 0.5);
     Result:=fTeeDistanceMetres;
end;
// *********************************************************************
function TGolfmlClass.priv_GetTeeDistanceYards:Cardinal;
begin
     // Trap for crap
     fTeeDistanceYards:=0;
     Result:=fTeeDistanceYards;
     if fCourseIndex > priv_GetMaxCourseIndex then Exit;
     if fTeeColourIndex > priv_GetMaxTeeColourIndex then Exit;
     // Assign
     fTeeDistanceYards:=fTeeDistanceArrayYards[fCourseIndex,fTeeColourIndex,fHoleIndex];
     fTeeDistanceMetres:=fTeeDistanceArrayMeters[fCourseIndex,fTeeColourIndex,fHoleIndex];
     // Fetch yards from metres?
     If (fTeeDistanceYards = 0)
     AND (fTeeDistanceMetres > 0)
     THEN fTeeDistanceYards:=TRUNC((fTeeDistanceMetres * 1.0936133) + 0.5);
     Result:=fTeeDistanceYards;
end;
// *********************************************************************
function TGolfmlClass.priv_GetMaxTeeColourIndex:Cardinal;
Begin
     fMaxTeeColourIndex:=0;
     If (High(fCourseTeeColourArray[fCourseIndex]) > 0) then
       fMaxTeeColourIndex:=High(fCourseTeeColourArray[fCourseIndex]);
     Result:=fMaxTeeColourIndex;
end;
// *********************************************************************
function TGolfmlClass.priv_GetMaxCourseIndex:Cardinal;
Begin
     fMaxCourseIndex:=0;
     If (High(fCourseNameArray) > 0) Then
       fMaxCourseIndex:=High(fCourseNameArray);
     Result:=fMaxCourseIndex;
end;
// *********************************************************************

end.

