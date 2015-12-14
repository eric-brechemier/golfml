unit thandicapgolfumlclass;
(*
== Author: minesadorada
==
== Lazarus: 0.9.30-0
== FPC: 2.4.4
==
== Subclass of HCPClass with XML reading added
==
== Version History
== 1.0
   [20120322]
   General code for class.
== 1.1
== [20120330]
== All properties and methods added
== Tested and commented
==
*)
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, XMLRead, DOM, thandicapclassunit;
  //Forms, Controls, Graphics, Dialogs, Buttons, // DEBUGGING ONLY
  //StdCtrls, ComCtrls, ExtCtrls;

const
  C_HCPGolfUMLClassVERSION = '1.1.20120328';
CONST C_METERS=TRUE; // fMetric
CONST C_YARDS=FALSE; // fMetric

// <amenety type=('practice' | 'store' | 'food' | 'corporate' | 'golfers' | 'bathroom' | 'water' | 'other' )
type TCountryClubRecord = Record
  Name:ShortString;
  Country:ShortString;
  CountryCode:ShortString;
  Municipality:ShortString;
  Region:ShortString;
  Street:ShortString;
  PostCode:ShortString;
  Website:ShortString;
  Phone:ShortString;
  Amenities:String;
  Comments:String;
end;


type
  THCPGolfUMLClass = class(THCPClass) // Subclass of THCPClass
  private
    // PRIVATE VARS AND METHODS - Unavailable outside this class
    // *********************************************************************
    fCourseXMLPath: string;
    fCourseLoaded:Boolean; // True if XML file has been read
    // Each course is an open array
    // Each tee set is an open array
    // These private arrays are populated by GetCourseInfoFromFile, and the
    // GOLMl file may contain more than one course.
    // The Contents of the f-arrays are then copied
    // to the (protected) p-arrays as the Course or tee-set are set.

    // For Each Country Club
    fCourseArray: array of String;

    // For Each [Course] In Country Club
    fCourseTeeColourArray: Array of Array of String; // Tee Colour Names
    fCourseTeeTitleArray: Array of Array of String; // Tee Title Names
    fCourseTeeGenderArray: Array of Array of String; // Tee Gender Names

    fCourseParArray: Array of Array[0..17] of Cardinal;  // Hole Pars
    fCourseStrokeIndexArray: Array of Array[0..17] of cardinal; // Hole SI's

    // For Each Tee Colour In Course in Country Club
    fTeeColourCourseRatingArray:Array of Array of Single; // Tee Colour Course Ratings
    fTeeColourSlopeRatingArray: Array of Array of Cardinal; // Tee Colour Slope Ratings

    // For Each Tee in Tee Colour In Course in Country Club
    fTeeDistanceArrayMeters: Array of Array of Array[0..17] of Cardinal; //Tee Distances
    fTeeDistanceArrayYards: Array of Array of Array[0..17] of Cardinal; // Tee Distances


    fCurrentCourseIndex:Cardinal; // Index into the 'f' arrays
    fCurrentTeeColourIndex:Cardinal; // Index into the 'f' arrays
    fCurrentHoleIndex:Cardinal; // Index into the 'f' arrays


    fCountryClubRecord:TCountryClubRecord; // Used to hold temporary data
    fCountryClubName:String; // Property ClubName
    fCountryClubCountry:String; // Property ClubCountry
    fCountryClubCountryCode:String; // Property ClubCountryCode
    fCountryClubMunicipality:String; // Property ClubMunicipality
    fCountryClubRegion:String; // Property ClubRegion
    fCountryClubStreet:String; // Property ClubStreet
    fCountryClubPostCode:String; // Property ClubPostCode
    fCountryClubWebsite:String; // Property ClubWebsite
    fCountryClubPhone:String; // Property ClubPhone
    fCountryClubAmenities:String; // Property ClubAmeneties
    fCountryClubComments:String; // Property ClubComments


    fHoleNumber:Cardinal; // Property Hole (1-based)

    fCourseNumberOfHoles:Cardinal; // Property CourseHoleCount
    fCourseCount:Cardinal; // Property CourseCount
    fTeeColourCount:Cardinal; // Property TeeColourCount
    fMetric:Boolean; // Property Metric
    fPar:Cardinal; // Property Par;
    fStrokeIndex:Cardinal; // Property Strokeindex
    fTeeDistance:Cardinal; // Property Distance
    fTotalTeeDistance:Cardinal; // Property TotalTeeDistance
    fTotalTeeDistancemetres,fTotalTeeDistanceyards:Cardinal;
    fInTeeDistance:Cardinal; // Property InTeeDistance
    fInTeeDistancemetres,fInTeeDistanceyards:Cardinal;
    fOutTeeDistance:Cardinal; // Property OutTeeDistance
    fOutTeeDistancemetres,fOutTeeDistanceyards:Cardinal;

    Procedure priv_CalculateBogeyRating; // From courserating and sloperating
    procedure priv_SetTotalTeeDistance; // Refreshes property TotalTeeDistance (fTotalTeeDistance)

    procedure priv_TestAndSetCourseArraySizes(aCourseIndex:Cardinal);
    // Tests aCourseIndex against High(fCourseArray)
    procedure priv_TestAndSetTeeColourArraySizes(aCourseIndex:Cardinal;aTeeColourIndex:Cardinal);
    // Tests aTeeColourIndex against High(fCourseTeeColourArray[aCourseIndex])


    // GETS and SETS
    procedure priv_GetCoursesIntoStringList;
    procedure priv_GetTeecolorsIntoStringList;
    procedure priv_SetCourseXMLPath(aString: string);
    procedure priv_SetCurrentCourse(aCardinal:Cardinal);
    procedure priv_SetCurrentTeeColourIndex(aCardinal:Cardinal);
    procedure priv_SetCurrentHole(aCardinal:Cardinal);
    procedure priv_SetMetric(aBoolean:Boolean);

    Function priv_PrettyString(aString:String):String; // Capitalises first letter
    // Various Sub-procedures for parsing GolfUML document elements
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

  protected
    // PROTECTED VARS AND METHODS FOR SUBCLASS USE
    // *********************************************************************
    pCoursesStringList: TStrings;
    pTeeColourStringList:TStrings;
    pTeeTitleStringList:TStrings;
    pTeeGenderStringList:TStrings;

    // Not used by this class, but assigned.
    pCourseArray: array of String;

    // For the current Course.  Properties work on these
    pTeecolourArray:Array of String;
    pTeeTitleArray:Array of String;
    pTeeGenderArray:Array of String;
    pCourseRatingArray:Array of Single;
    pSlopeRatingArray:Array of Cardinal;
    pStrokeIndexArray:Array[0..17] of cardinal; // Can be overridden
    // For each TeeColourIndex of the CourseIndex
    pTeeDistanceArrayMeters:Array of Array [0..17] of Cardinal;
    pTeeDistanceArrayYards:Array of Array [0..17] of Cardinal;


    function prot_InitObj: boolean; virtual; // In case we subclass again

  public
    // PUBLIC VARS METHODS AND PROPERTIES
    constructor Create; // Note no override for Constructor
    destructor Destroy; override;
    function InitOK: boolean; // If myObj.InitOK then proceed...
    // *********************************************************************
    // Main Method
    Function GetCourseInfoFromFile:boolean; virtual; // Implementation of abstract method
    Procedure Reset; // Calls prot_InitObj

    // Non-publishable properties
    property CourseStringList: TStrings read pCoursesStringList;//priv_GetCoursesIntoStringList;
    property TeeColourStringList: TStrings read pTeeColourStringList;// priv_GetTeecolorsIntoStringList;
    property TeeTitleStringList: TStrings read pTeeTitleStringList;// priv_GetTeecolorsIntoStringList;
    property TeeGenderStringList: TStrings read pTeeGenderStringList;// priv_GetTeecolorsIntoStringList;

  published
    // Inherited useful properties from THCPClass (for Information)
    // Note all 'p' vars and proc_ methods from HCPClass can be used in this class
    // as well as public methods and CONST
    // (but not HCPClass 'priv_' methods or 'f' vars)
    // **************************************************************************
    // Property CoursePar:Cardinal read pCoursePar write priv_SetCoursePar;
    // Property CourseRating:Single read pCourseRating write priv_SetCourseRating;
    // Property SlopeRating:Cardinal read pSlopeRating write priv_SetSlopeRating;
    // Property BogeyRating:Single read pBogeyRating write priv_SetBogeyRating;

    // PUBLIC PROPERTIES AVAILABLE TO A VCL
    property CourseXMLPath: string read fCourseXMLPath write priv_SetCourseXMLPath; // NO CHECKING
    property CourseLoaded:Boolean read fCourseLoaded;

    property ClubName: string read fCountryClubName;
    property ClubCountry: string read fCountryClubCountry;
    property ClubCountryCode: string read fCountryClubCountryCode;
    property ClubMunicipality: string read fCountryClubMunicipality;
    property ClubRegion: string read fCountryClubRegion;
    property ClubStreet: string read fCountryClubStreet;
    property ClubPostCode: string read fCountryClubPostCode;
    property ClubWebsite: string read fCountryClubWebsite;
    property ClubPhone: string read fCountryClubPhone;
    property ClubAmeneties: string read fCountryClubAmenities;
    property ClubComments: string read fCountryClubComments;

    // Index into the StringList property
    // Change courses by changing the CourseIndex property (0-based)
    property CourseIndex:Cardinal read fCurrentCourseIndex write priv_SetCurrentCourse;
    // Index into the StringList property
    // Change Tee sets by changing the TeeColourIndex property (0-based)
    Property TeeColourIndex:Cardinal read fCurrentTeeColourIndex Write priv_SetCurrentTeeColourIndex;

    // These properties are 1-based
    Property CourseCount:Cardinal read fCourseCount;
    Property CourseTeeColourCount:Cardinal read fTeeColourCount;
    Property CourseHoleCount:Cardinal read fCourseNumberOfHoles;

    Property Metric:Boolean read fMetric write priv_SetMetric; // Affects Distance properties
    Property InTeeDistance:Cardinal read fInTeeDistance; // fMetric determines metres or yards
    Property OutTeeDistance:Cardinal read fOutTeeDistance; // fMetric determines metres or yards
    Property TotalTeeDistance:Cardinal read fTotalTeeDistance; // fMetric determines metres or yards


    property Hole:Cardinal read fHoleNumber Write priv_SetCurrentHole; // (1-18)
    // Setting Hole property sets the others
    Property Par:Cardinal read fPar;
    Property Distance:Cardinal read fTeeDistance; // fMetric determines metres or yards
    Property StrokeIndex:Cardinal read fStrokeIndex;
  end;



implementation
// *********************************************************************
constructor THCPGolfUMLClass.Create;
begin
  // First we call the existing HCPClass.Create code
  inherited Create;
  // ..and then add stuff
  pCoursesStringList:=TStringList.Create;
  pTeeColourStringList:=TStringList.Create;
  pTeeTitleStringList:=TStringList.Create;
  pTeeGenderStringList:=TStringList.Create;
end;
// *********************************************************************
destructor THCPGolfUMLClass.Destroy;
Var cCount:Cardinal;
begin
  // First do any tidy-up code before calling the inherited Destroy
  pTeeGenderStringList.Free;
  pTeeTitleStringList.Free;
  pTeeColourStringList.Free;
  pCoursesStringList.Free;
  inherited Destroy;
end;
// *********************************************************************
// PROTECTED METHODS
// *********************************************************************
function THCPGolfUMLClass.prot_InitObj: boolean;
  // PROTECTED
  // Do initialisation stuff here to reset the
  // vars/properties etc. to their default values/states
var
  cCourseCount,cTeeCount,cHoleCount,cCount: cardinal;
  s: string;
begin
  inherited prot_InitObj;
  // Initialises all the public, private and protected vars in ancestor THCPClass

  // Initialise new (or change THCPClass public/protected) vars for THCPGolfUMLClass here
  // The 'f' Course Data arrays are OpenArrays of Tee Position data
  // that are read from the coursedata.xml file
  // The 'p' Arrays are for a particular course
  // SetLength is the same as the Count property, and is 1-based.
  // DynArray.Count=1, and High(Dynarray)=0 , but Dynarray[1] is Nil
  // Dynarray[0] is first member

  SetLength(fCourseArray,1);
  SetLength(pCourseArray,1);

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


  fCourseArray[0]:='No Course data';
  fCourseTeeColourArray[0,0]:='No Tee data';
  fCourseTeeTitleArray[0,0]:='No Tee data';
  fCourseTeeGenderArray[0,0]:='No Tee data';

  SetLength(pCourseRatingArray,1);
  SetLength(pSlopeRatingArray,1);
  SetLength(pTeeDistanceArrayMeters,1);
  SetLength(pTeeDistanceArrayYards,1);


  for cHoleCount := 0 to 17 do
    begin
         fCourseParArray[0,cHoleCount] := 0;
         pParArray[cHoleCount] := 0;

         fCourseStrokeIndexArray[0,cHoleCount] := 0;
         pStrokeIndexArray[cHoleCount] := 0;

         fTeeDistanceArrayMeters[0,0,cHoleCount]:=0;
         pTeeDistanceArrayMeters[0,cHoleCount]:=0;
         fTeeDistanceArrayYards[0,0,cHoleCount]:=0;
         pTeeDistanceArrayYards[0,cHoleCount]:=0;
    end;
    fTeeColourCourseRatingArray[0,0]:=0; // Course 0, TeeColour 0
    pCourseRatingArray[0]:=0;

    fTeeColourSlopeRatingArray[0,0]:=0; // Course 0, TeeColour 0
    pSlopeRatingArray[0]:=0;


    pVersion := C_HCPGolfUMLClassVERSION; // HCPClass Version property overridden

  // The help text in ShowHelp added or replaced
  s := CR;
  s += 'HCPGolfUMLClass: a subclass of HCPClass by minesadorada' + CR;
  s += 'Freeware version ' + pVersion;
  pBannerText := s;

  // This subclass extra methods and properties here
  pPropertiesHelpText+=CR + '18: CourseXMLPath (not checked): String read/write';
  pPropertiesHelpText+=CR + '19: CourseStringList Tstrings readonly';
  pPropertiesHelpText+=CR + '20: TeeColourStringList TStrings readonly';
  pPropertiesHelpText+=CR + '21: CourseIndex,TeeColourIndex Cardinal read/write';
  pPropertiesHelpText+=CR + '22: Hole: Cardinal read/write';
  pPropertiesHelpText+=CR + '23: Par,StrokeIndex,Distance Cardinal readonly';
  pPropertiesHelpText+=CR + '24: Metric:Boolean read/write';

  pMethodsHelpText+=CR + '9: Procedure GetCourseInfoFromFile: reads a GolfML file';
  pMethodsHelpText+=CR + '(Call Reset method before reading a new GolfML file)';

  pInfoHelpText+=CR + '=== Using a GolfUML file ===';
  pInfoHelpText+=CR + '1: Set CourseXMLPath then call GetCourseInfoFromFile.';
  pInfoHelpText+=CR + 'Coursenames and tee positions are returned as String Lists';
  pInfoHelpText+=CR + '2: Access a course in the list by setting CourseIndex';
  pInfoHelpText+=CR + '3: Access a tee position in the course by setting TeeColourIndex';
  pInfoHelpText+=CR + 'Read the CourseRating and SlopeRating';
  pInfoHelpText+=CR + '4: Access Hole stats by setting the Hole property';
  pInfoHelpText+=CR + 'Read Par, StrokeIndex and Distance (Metric=True/False)';
  //  Templates for adding more:
  //  pMethodsHelpText+=CR + 'nn: This is a new method';
  //  pPropertiesHelpText+=CR + 'nn: This is a new Property';
  //  pInfoHelpText+=CR + 'This is more help';

  // Clear the stringlists
  pCoursesStringList.Clear;
  pCoursesStringList.Add('No courses');
  pTeeColourStringList.Clear;
  pTeeColourStringList.Add('No tees');
  pTeeTitleStringList.Clear;
  pTeeTitleStringList.Add('No tees');
  pTeeGenderStringList.Clear;
  pTeeGenderStringList.Add('No tees');

  fCourseXMLPath:='coursedata.xml';

  fCurrentCourseIndex:=0;
  fCurrentCourseIndex:=0;

  fCurrentTeeColourIndex:=0;
  fCurrentTeeColourIndex:=0;

  fCurrentHoleIndex:=0;

  fCourseLoaded:=FALSE;
  fHoleNumber:=1;
  fPar:=0;
  fTeeDistance:=0;
  fStrokeIndex:=0;
  fMetric:=TRUE;
  fCountryClubName:='Unknown Name';
  With fCountryClubRecord do
       begin
           Name:='';
           Country:='';
           CountryCode:='';
           Municipality:='';
           Region:='';
           Street:='';
           PostCode:='';
           Website:='';
           Phone:='';
           Amenities:='';
           Comments:='';
       end;
       Result := True;

end;
// *********************************************************************
Function THCPGolfUMLClass.GetCourseInfoFromFile:boolean;
// This is the main method.  Call Reset before changing the xml and calling this.
// Be sure fCourseXMLPath is set and checked first!
// Sets up the multi-dimensional f-arrays (all courses) and all other properties
// by parsing the golfxml via a series of nested loops.
Var
   Doc: TXMLDocument;
   CurrentNode: TDOMNode;
   cCount:Cardinal;
   cSavedCurrentCourse:Cardinal;
begin
TRY
     Result:=FALSE; // assume failure
     pCoursesStringList.Clear;
     pTeeColourStringList.Clear;
     // Reset;
     cSavedCurrentCourse:=fCurrentCourseIndex; // Preserve
     fCurrentCourseIndex:=0;
     fCurrentTeeColourIndex:=0;
     fCurrentHoleIndex:=0;
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
                       priv_TestAndSetCourseArraySizes(fCurrentCourseIndex); // Increase if necessary
                       priv_ProcessXMLGolfCourse(CurrentNode);
                       Inc(fCurrentCourseIndex);
                       fCurrentTeeColourIndex:=0; // New TeeColourSet
                   end;
                 // ..Next child of <country-club>
                 CurrentNode:=CurrentNode.NextSibling;
            end;
            Result:=TRUE; // Success!
FINALLY
       Doc.Free;
END;
    pCourseArray:=fCourseArray; // Set protected array;
    fCurrentCourseIndex:=cSavedCurrentCourse;
    fCourseLoaded:=TRUE;
    // Check for missing tee-set node
    For cCount:=0 to High(fCourseTeeColourArray) do
        If High(fCourseTeeColourArray[cCount])=-1 then priv_ProcessNoTeeSetsInCourse(cCount);
    priv_SetCurrentCourse(fCurrentCourseIndex);
end;
// *********************************************************************
// PRIVATE METHODS
// *********************************************************************
procedure THCPGolfUMLClass.priv_TestAndSetTeeColourArraySizes(aCourseIndex:Cardinal;aTeeColourIndex:Cardinal);
// Called from ProcessXMLGolfCourse
Begin
     If aTeeColourIndex > High(fCourseTeeColourArray[aCourseIndex]) then
       begin
            SetLength(fCourseTeeColourArray[aCourseIndex],aTeeColourIndex+1);
            SetLength(fCourseTeeTitleArray[aCourseIndex],aTeeColourIndex+1);
            SetLength(fCourseTeeGenderArray[aCourseIndex],aTeeColourIndex+1);
            SetLength(fTeeColourCourseRatingArray[aCourseIndex],aTeeColourIndex+1);
            SetLength(fTeeColourSlopeRatingArray[aCourseIndex],aTeeColourIndex+1);
            SetLength(fTeeDistanceArrayMeters[aCourseIndex],aTeeColourIndex+1);
            SetLength(fTeeDistanceArrayYards[aCourseIndex],aTeeColourIndex+1);
       end;
end;

// *********************************************************************
procedure THCPGolfUMLClass.priv_TestAndSetCourseArraySizes(aCourseIndex:Cardinal);
// Called from GetCourseInfoFromFile
begin
     If aCourseIndex > High(fCourseArray) then
       begin
            SetLength(fCourseArray,aCourseIndex+1);
            SetLength(pCourseArray,aCourseIndex+1);
            SetLength(fCourseTeeColourArray,aCourseIndex+1);
            SetLength(fCourseTeeTitleArray,aCourseIndex+1);
            SetLength(fCourseTeeGenderArray,aCourseIndex+1);
            SetLength(fTeeColourCourseRatingArray,aCourseIndex+1);
            SetLength(fTeeColourSlopeRatingArray,aCourseIndex+1);
            SetLength(fCourseStrokeIndexArray,aCourseIndex+1);
            SetLength(fCourseParArray,aCourseIndex+1);
            SetLength(fTeeDistanceArrayMeters,aCourseIndex+1);
            SetLength(fTeeDistanceArrayYards,aCourseIndex+1);
       end;
end;

// *********************************************************************
Procedure THCPGolfUMLClass.priv_CalculateBogeyRating;
// From pcourserating and psloperating
// CALCULATING BOGEY RATING from Course Rating and Slope Rating
// Bogey rating minus Course Rating (C) multiplied by (5.381 men, 4.24 women)(K)  equals Slope Rating (S).
// (B-C) * K = S
// B-C = S/K
// B=(S/K) * C
Var
   bLadies:Boolean;
begin
     // Trap for crap
     If (pSlopeRating=0) OR (pCourseRating=0) then
       begin
           pBogeyRating:=0;
           Exit;
       end;
     bLadies:=False;
     If CompareText(fCourseTeeGenderArray[fCurrentCourseIndex,fCurrentTeeColourIndex],'ladies')=0 then bLadies:=True;
     If bLadies then
        pBogeyRating := (pSlopeRating/4.24) + pCourseRating
     else
         pBogeyRating := (pSlopeRating/5.381) + pCourseRating;
end;
// *********************************************************************
Function THCPGolfUMLClass.priv_PrettyString(aString:String):String;
// Capitalises first letter
// Used to make tee attributes (usually all-lowercase) in the stringlists display nicely
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
Procedure THCPGolfUMLClass.priv_ProcessXMLTee(aNode:TDomNode;TeeColour:Cardinal;sCourseName:String);
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
                aString:=CurrentNode.Attributes[iAttributeCount].NodeValue;
                If TryStrToInt(aString,iInteger)=TRUE then
                   iTeeNumber:=StrToInt(aString);
           end;
      end;
  // Now look at children of <tee>
   CurrentNode:=aNode.FirstChild;
   While Assigned(CurrentNode) do
   begin
        if CurrentNode.NodeName='par' then
          begin
               aString:=CurrentNode.TextContent;
               If TryStrToInt(aString,iInteger)=TRUE then
                 fCourseParArray[fCurrentCourseIndex,iTeeNumber-1]:=StrToInt(aString);
          end;
        if CurrentNode.NodeName='handicap-stroke' then
          begin
               aString:=CurrentNode.TextContent;
               If TryStrToInt(aString,iInteger)=TRUE then
                  fCourseStrokeIndexArray[fCurrentCourseIndex,iTeeNumber-1]:=StrToInt(aString);
          end;
        if CurrentNode.NodeName='length' then
            if CurrentNode.HasAttributes and (CurrentNode.Attributes.Length > 0) then
                For iAttributeCount:=0 to CurrentNode.Attributes.Length-1 do
                  begin
                       If CurrentNode.Attributes[iAttributeCount].NodeName='units' then
                         begin
                           if  CurrentNode.Attributes[iAttributeCount].NodeValue='meters' then
                            begin
                                 aString:=CurrentNode.TextContent;
                                 If TryStrToInt(aString,iInteger)=TRUE then
                                    fTeeDistanceArrayMeters[fCurrentCourseIndex,TeeColour,iTeeNumber-1]:=StrToInt(aString);
                            end;
                           if  CurrentNode.Attributes[iAttributeCount].NodeValue='yards' then
                                begin
                                     aString:=CurrentNode.TextContent;
                                     If TryStrToInt(aString,iInteger)=TRUE then
                                        fTeeDistanceArrayYards[fCurrentCourseIndex,TeeColour,iTeeNumber-1]:=StrToInt(aString);
                                end;
                         end;
                  end;
        CurrentNode:=CurrentNode.NextSibling;

   end;

end;
// *********************************************************************
procedure THCPGolfUMLClass.priv_ProcessXMLQualificationUSGA(aNode:TDomNode;TeeColour:Cardinal;sCourseName:String);
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
              AString:=CurrentNode.TextContent;
              If TryStrToFloat(AString,fFloat)=TRUE then
                  fTeeColourCourseRatingArray[fCurrentCourseIndex,TeeColour]:=StrToFloat(AString);

         end;
      if CurrentNode.NodeName='slope' then
         begin
           AString:=CurrentNode.TextContent;
           If TryStrToInt(AString,iInteger)=TRUE then
               fTeeColourSlopeRatingArray[fCurrentCourseIndex,TeeColour]:=StrToInt(AString);
         end;
      CurrentNode:=CurrentNode.NextSibling;
 end;

end;
// *********************************************************************
procedure THCPGolfUMLClass.priv_ProcessXMLQualification(aNode:TDomNode;TeeColour:Cardinal;sCourseName:String);
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
procedure THCPGolfUMLClass.priv_ProcessXMLTeeSet(aNode:TDomNode;TeeColour:Cardinal;sCourseName:String);
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
procedure THCPGolfUMLClass.priv_ProcessXMLGolfCourse(aNode:TDomNode);
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
                  fCourseArray[fCurrentCourseIndex]:=sCourseName;  // Golf course name
             end;
          if CurrentNode.NodeName='tee-set' then
            begin
              if CurrentNode.HasAttributes and (CurrentNode.Attributes.Length > 0) then
                begin
                     priv_TestAndSetTeeColourArraySizes(fCurrentCourseIndex,fCurrentTeeColourIndex);
                     // Increases array sizes if needed

                     For iAttributeCount:=0 to CurrentNode.Attributes.Length-1 do
                     begin
                         If CurrentNode.Attributes[iAttributeCount].NodeName='colour' then
                            fCourseTeeColourArray[fCurrentCourseIndex,fCurrentTeeColourIndex]:=CurrentNode.Attributes[iAttributeCount].NodeValue;
                         If CurrentNode.Attributes[iAttributeCount].NodeName='gender' then
                            fCourseTeeGenderArray[fCurrentCourseIndex,fCurrentTeeColourIndex]:=CurrentNode.Attributes[iAttributeCount].NodeValue;
                         If CurrentNode.Attributes[iAttributeCount].NodeName='name' then
                            fCourseTeeTitleArray[fCurrentCourseIndex,fCurrentTeeColourIndex]:=CurrentNode.Attributes[iAttributeCount].NodeValue;
                     end; // of for.. loop
                     priv_ProcessXMLTeeSet(CurrentNode,fCurrentTeeColourIndex,sCourseName);
                     Inc(fCurrentTeeColourIndex,1);
                 end; // of hasattributes
            end; // of NodeName=tee-set
          CurrentNode:=CurrentNode.NextSibling;
     end;
     // ShowMessageFmt('priv_ProcessXMLGolfCourse: High(fCourseTeeColourArray[fCurrentCourseIndex])=%d',[High(fCourseTeeColourArray[fCurrentCourseIndex])]);
end;
// *********************************************************************
procedure THCPGolfUMLClass.priv_ProcessXMLCountryClubAddress(aNode:TDomNode);
Var
   CurrentNode:TDOMNode;
   iAttributeCount:Cardinal;
begin
     CurrentNode:=ANode; // <address>
     CurrentNode:=CurrentNode.FirstChild; // Look at children of <address>
     While Assigned(CurrentNode) do
       begin
           if CurrentNode.NodeName='street' then
             If Length(CurrentNode.TextContent) > 0 then fCountryClubRecord.Street:=CurrentNode.TextContent;
           if CurrentNode.NodeName='postal-code' then
             If Length(CurrentNode.TextContent) > 0 then fCountryClubRecord.PostCode:=CurrentNode.TextContent;
           if CurrentNode.NodeName='municipality' then
             If Length(CurrentNode.TextContent) > 0 then fCountryClubRecord.Municipality:=CurrentNode.TextContent;
           if CurrentNode.NodeName='region' then
             If Length(CurrentNode.TextContent) > 0 then fCountryClubRecord.Region:=CurrentNode.TextContent;
           if CurrentNode.NodeName='website' then
             If Length(CurrentNode.TextContent) > 0 then fCountryClubRecord.Website:=CurrentNode.TextContent;
           if CurrentNode.NodeName='country' then
             If Length(CurrentNode.TextContent) > 0 then fCountryClubRecord.country:=CurrentNode.TextContent;
           if CurrentNode.HasAttributes and (CurrentNode.Attributes.Length > 0) then
               For iAttributeCount:=0 to CurrentNode.Attributes.Length-1 do
                 begin
                      If CurrentNode.Attributes[iAttributeCount].NodeName='code' then
                        fCountryClubRecord.CountryCode:=CurrentNode.Attributes[iAttributeCount].NodeValue;
                 end;
           CurrentNode:=CurrentNode.NextSibling;
       end;

end;
// *********************************************************************
procedure THCPGolfUMLClass.priv_ProcessXMLCountryClubContact(aNode:TDomNode);
Var
   CurrentNode:TDOMNode;
begin
     CurrentNode:=ANode; // <contact>
     CurrentNode:=CurrentNode.FirstChild; // Look at children of <contact>
     While Assigned(CurrentNode) do
       begin
           if CurrentNode.NodeName='phone' then
             If Length(CurrentNode.TextContent) > 0 then fCountryClubRecord.Phone:=CurrentNode.TextContent;
           CurrentNode:=CurrentNode.NextSibling;
       end;
end;
// *********************************************************************
procedure THCPGolfUMLClass.priv_ProcessXMLCountryClubAmenety(aNode:TDomNode);
Var
   CurrentNode:TDOMNode;
begin
     CurrentNode:=ANode; // <amenety>
     If Length(CurrentNode.TextContent) > 0 then
        fCountryClubRecord.Amenities:=fCountryClubRecord.Amenities + CurrentNode.TextContent + CR;
end;
// *********************************************************************
procedure THCPGolfUMLClass.priv_ProcessXMLCountryClubNote(aNode:TDomNode);
Var
   CurrentNode:TDOMNode;
begin
     CurrentNode:=ANode; // <note>
     CurrentNode:=CurrentNode.FirstChild; // Look at children of <note>
     While Assigned(CurrentNode) do
       begin
           if CurrentNode.NodeName='comment' then
             If Length(CurrentNode.TextContent) > 0 then
                fCountryClubRecord.Comments:=fCountryClubRecord.Comments + CurrentNode.TextContent + CR;
           CurrentNode:=CurrentNode.NextSibling;
       end;
end;
// *********************************************************************
procedure THCPGolfUMLClass.priv_ProcessXMLCountryClub(aNode:TDomNode);
// Populates a private record structure which is then assigned to properties
Var
   CurrentNode:TDOMNode;
begin
     CurrentNode:=ANode; // <country-club>
     CurrentNode:=CurrentNode.FirstChild; // Look at children of <country-club>
     fCountryClubRecord.Amenities:='';
     fCountryClubRecord.Comments:='';
     While Assigned(CurrentNode) do
       begin
           if CurrentNode.NodeName='name' then fCountryClubRecord.Name:=CurrentNode.TextContent;
           if CurrentNode.NodeName='address' then priv_ProcessXMLCountryClubAddress(CurrentNode);
           if CurrentNode.NodeName='contact' then priv_ProcessXMLCountryClubContact(CurrentNode);
           if CurrentNode.NodeName='amenety' then priv_ProcessXMLCountryClubAmenety(CurrentNode);
           if CurrentNode.NodeName='note' then priv_ProcessXMLCountryClubNote(CurrentNode);
           CurrentNode:=CurrentNode.NextSibling;
       end;

     // Assign populated internal Record to properties here
     fCountryClubName:=fCountryClubRecord.Name;
     fCountryClubCountry:=fCountryClubRecord.Country;
     fCountryClubCountryCode:=fCountryClubRecord.CountryCode;
     fCountryClubMunicipality:=fCountryClubRecord.Municipality;
     fCountryClubRegion:=fCountryClubRecord.Region;
     fCountryClubStreet:=fCountryClubRecord.Street;
     fCountryClubPostCode:=fCountryClubRecord.PostCode;
     fCountryClubWebsite:=fCountryClubRecord.Website;
     fCountryClubPhone:=fCountryClubRecord.Phone;
     fCountryClubAmenities:=fCountryClubRecord.Amenities;
     fCountryClubComments:=fCountryClubRecord.Comments;
end;
// *********************************************************************


// *********************************************************************
// PUBLIC METHODS
// *********************************************************************
Procedure THCPGolfUMLClass.Reset;
Begin
     prot_InitObj;
end;
// *********************************************************************
function THCPGolfUMLClass.InitOK: boolean;
  // PUBLIC
  // Reset all vars.  Return FALSE and raise an exception if there's a problem.
  // This method overrides the ancestor
begin
  Result := False; // Assume failure
  ErrorString := 'THCPGolfUMLClass.InitOK failed.' + CR;
  ErrorString := ErrorString + 'This application is now unstable,' + CR;
  ErrorString := ErrorString + 'and you should terminate it now.';
  // Call InitOK after Create method and test for true/false.
  try
    Result := prot_InitObj; // Should return TRUE if all OK
  except
    raise Exception.Create(ErrorString);
  end;
end;

// *********************************************************************
// GET/SETS (ALL PRIVATE METHODS)
// *********************************************************************
Procedure THCPGolfUMLClass.priv_ProcessNoTeeSetsInCourse(cCourseIndex:Cardinal);
Var cCount:Cardinal;
begin
     // SET UP DUMMY INFO FOR A COURSE WITH A NAME BUT NO TEE SETS (COLOURS) HERE
     // ShowMessageFmt('No data for cCourseIndex=%d',[cCourseIndex]);
     // cCourseIndex is the index of the course (fCourseArray) with a name but no data

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
procedure THCPGolfUMLClass.priv_SetCurrentCourse(aCardinal:Cardinal);
// Sets 'CourseIndex' property
// Set up the p-arrays (this course) from the f-arrays (all courses)
// Resize them on-the-fly
Var
    cCourseCount,cHoleCount,cTeeColourCount:Cardinal;
Begin
     // Trap for crap
     If aCardinal >  High(fCourseArray) then Exit;

     // Assign
     fCurrentCourseIndex:=aCardinal;
     fCourseNumberOfHoles:=0;
     pCoursePar:=0; // HCPClass var

     // Copy over all the fArrays to the pArrays
     // Note fCourseNumberOfHoles is set here
     for cHoleCount := 0 to 17 do
       begin
            pStrokeIndexArray[cHoleCount]:=fCourseStrokeIndexArray[fCurrentCourseIndex,cHoleCount];
           if pStrokeIndexArray[cHoleCount] > 0 then Inc(fCourseNumberOfHoles,1);
           pParArray[cHoleCount]:=fCourseParArray[fCurrentCourseIndex,cHoleCount]; // HCPClass var
           Inc(pCoursePar,pParArray[cHoleCount]);
       end;

     // Reset the length of the p-arrays (this course)
     // because different courses have different numbrers of tee-sets
     SetLength(pTeeColourArray,High(fCourseTeeColourArray[fCurrentCourseIndex])+1);
     SetLength(pTeeTitleArray,High(fCourseTeeColourArray[fCurrentCourseIndex])+1);
     SetLength(pTeeGenderArray,High(fCourseTeeColourArray[fCurrentCourseIndex])+1);
     SetLength(pCourseRatingArray,High(fCourseTeeColourArray[fCurrentCourseIndex])+1);
     SetLength(pSlopeRatingArray,High(fCourseTeeColourArray[fCurrentCourseIndex])+1);
     SetLength(pTeeDistanceArrayMeters,High(fCourseTeeColourArray[fCurrentCourseIndex])+1);
     SetLength(pTeeDistanceArrayYards,High(fCourseTeeColourArray[fCurrentCourseIndex])+1);

     // Copy information from the f-arrays (all courses) to the p-arrays (this course)
     For cTeeColourCount:=0 to High(fCourseTeeColourArray[fCurrentCourseIndex]) do
         begin
              pTeeColourArray[cTeeColourCount]:=fCourseTeeColourArray[fCurrentCourseIndex,cTeeColourCount];
              pTeeTitleArray[cTeeColourCount]:=fCourseTeeTitleArray[fCurrentCourseIndex,cTeeColourCount];
              pTeeGenderArray[cTeeColourCount]:=fCourseTeeGenderArray[fCurrentCourseIndex,cTeeColourCount];
              pCourseRatingArray[cTeeColourCount]:=fTeeColourCourseRatingArray[fCurrentCourseIndex,cTeeColourCount];
              pSlopeRatingArray[cTeeColourCount]:=fTeeColourSlopeRatingArray[fCurrentCourseIndex,cTeeColourCount];
              for cHoleCount := 0 to 17 do//fCourseNumberOfHoles do
                  begin
                       pTeeDistanceArrayMeters[cTeeColourCount,cHoleCount]:=fTeeDistanceArrayMeters[fCurrentCourseIndex,cTeeColourCount,cHoleCount];
                       pTeeDistanceArrayYards[cTeeColourCount,cHoleCount]:=fTeeDistanceArrayYards[fCurrentCourseIndex,cTeeColourCount,cHoleCount];
                  end;
         end;

     // Update properties
     fCourseCount:=High(fCourseArray)+1;
     fTeeColourCount:=High(fCourseTeeColourArray[fCurrentCourseIndex])+1;
     if fCourseNumberOfHoles=0 then fTeeColourCount:=0;

     // Set to the first tee-set
     priv_SetCurrentTeeColourIndex(0);  // Set to the first Tee Set
     priv_GetCoursesIntoStringList; // Populate the pCoursesStringList
     priv_GetTeecolorsIntoStringList; // Populate the pTeeColourStringList

end;
// *********************************************************************
procedure THCPGolfUMLClass.priv_SetCurrentTeeColourIndex(aCardinal:Cardinal);
// Sets 'TeeColourIndex' property
// Copy to the  p-arrays (this course) from the f-arrays (all courses)
Var
    cHoleCount:Cardinal;
begin
     // Trap for crap
     If (aCardinal > High(pTeeColourArray)) then Exit;

     // Assign
     fCurrentTeeColourIndex:=aCardinal;
     pCourseRating:=pCourseRatingArray[fCurrentTeeColourIndex];
     pSlopeRating:=pSlopeRatingArray[fCurrentTeeColourIndex];
     for cHoleCount := 0 to 17 do //fCourseNumberOfHoles do
         begin
              pTeeDistanceArrayMeters[fCurrentTeeColourIndex,cHoleCount]:=fTeeDistanceArrayMeters[fCurrentCourseIndex,fCurrentTeeColourIndex,cHoleCount];
              pTeeDistanceArrayYards[fCurrentTeeColourIndex,cHoleCount]:=fTeeDistanceArrayYards[fCurrentCourseIndex,fCurrentTeeColourIndex,cHoleCount];
         end;


     // Set The BogeyRating from CourseRating and SlopeRating info
     priv_CalculateBogeyRating;
     priv_SetTotalTeeDistance;
end;
// *********************************************************************
Procedure THCPGolfUMLClass.priv_GetCoursesIntoStringList;
// Copies the strings from the p-arrays into StringLists
Var
   cCourseCount:Cardinal;
begin
     pCoursesStringList.Clear;
     For cCourseCount:=0 to High(fCourseArray) do
         pCoursesStringList.Add(fCourseArray[cCourseCount]);
     If pCoursesStringList.Count=0 then pCoursesStringList.Add('No course info');
end;
// *********************************************************************
Procedure THCPGolfUMLClass.priv_GetTeecolorsIntoStringList;
// Copies the strings from the p-arrays into StringLists
// for TeeColour, TeeTitle and TeeGender
Var
   cTeeColourCount:Cardinal;
begin
     pTeeColourStringList.Clear;
     For cTeeColourCount:=0 to High(fCourseTeeColourArray[fCurrentCourseIndex]) do
            pTeeColourStringList.Add(priv_PrettyString(fCourseTeeColourArray[fCurrentCourseIndex,cTeeColourCount]));
        If pTeeColourStringList.Count=0 then pTeeColourStringList.Add('No tee info');
     pTeeTitleStringList.Clear;
     For cTeeColourCount:=0 to High(fCourseTeeColourArray[fCurrentCourseIndex]) do
            pTeeTitleStringList.Add(priv_PrettyString(fCourseTeeTitleArray[fCurrentCourseIndex,cTeeColourCount]));
        If pTeeTitleStringList.Count=0 then pTeeTitleStringList.Add('No tee info');
     pTeeGenderStringList.Clear;
     For cTeeColourCount:=0 to High(fCourseTeeColourArray[fCurrentCourseIndex]) do
         pTeeGenderStringList.Add(priv_PrettyString(fCourseTeeGenderArray[fCurrentCourseIndex,cTeeColourCount]));
     If pTeeGenderStringList.Count=0 then pTeeGenderStringList.Add('No tee info');
end;

// *********************************************************************
procedure THCPGolfUMLClass.priv_SetCourseXMLPath(aString: string);
// Sets 'CourseXMLPath' property
begin
  if fCourseXMLPath <> aString then fCourseXMLPath := aString;
end;
// *********************************************************************
procedure THCPGolfUMLClass.priv_SetTotalTeeDistance;
// Called when setting a new tee position from priv_SetCurrentTeeColourIndex
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
         Inc(fInTeeDistanceMetres,pTeeDistanceArrayMeters[fCurrentTeeColourIndex,cHoleCount]);
         Inc(fInTeeDistanceYards,pTeeDistanceArrayYards[fCurrentTeeColourIndex,cHoleCount]);
    end;
If fCourseNumberOfHoles > 8 then
  begin
       for cHoleCount := 9 to 17 do
       begin
         Inc(fOutTeeDistanceMetres,pTeeDistanceArrayMeters[fCurrentTeeColourIndex,cHoleCount]);
         Inc(fOutTeeDistanceYards,pTeeDistanceArrayYards[fCurrentTeeColourIndex,cHoleCount]);
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
procedure THCPGolfUMLClass.priv_SetCurrentHole(aCardinal:Cardinal);
// Sets 'Hole' property
Var
    fCurrentDistanceMetres,fCurrentDistanceYards:Cardinal;
begin
     // Trap for crap
     if (aCardinal=fCurrentHoleIndex)
     OR (aCardinal > fCourseNumberOfHoles)
     OR (aCardinal < 1)
     then Exit;

     // Assign
     fHoleNumber:=aCardinal; // Property 'Hole'
     fCurrentHoleIndex:=aCardinal-1; //fCurrentHoleIndex is zero-based

     // Update the other per-hole properties
     // Distance
     fCurrentDistanceMetres:=pTeeDistanceArrayMeters[fCurrentTeeColourIndex,fCurrentHoleIndex];
     fCurrentDistanceYards:=pTeeDistanceArrayYards[fCurrentTeeColourIndex,fCurrentHoleIndex];
     if fMetric=C_METERS then
        fTeeDistance:=fCurrentDistanceMetres
     else
         fTeeDistance:=fCurrentDistanceYards;

// If both yards and metres are set to nonzero, then do nothing, else convert on-the-fly
     // Fetch metres from yards?
     If (fMetric=C_METERS)
     AND (fCurrentDistanceMetres = 0)
     AND (fCurrentDistanceYards > 0)
     THEN
        fTeeDistance:=TRUNC((fCurrentDistanceYards * 0.9144) + 0.5);
     // Fetch yards from metres?
     If (fMetric=C_YARDS)
     AND (fCurrentDistanceYards = 0)
     AND (fCurrentDistanceMetres > 0)
     THEN
        fTeeDistance:=TRUNC((fCurrentDistanceMetres * 1.0936133) + 0.5);

     // StrokeIndex and Par
     fStrokeIndex:=pStrokeIndexArray[fCurrentHoleIndex];
     fPar:=pParArray[fCurrentHoleIndex];
end;
// *********************************************************************
procedure THCPGolfUMLClass.priv_SetMetric(aBoolean:Boolean);
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

end.
