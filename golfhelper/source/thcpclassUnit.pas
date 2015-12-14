unit thcpclassUnit;

(*
== Author: minesadorada
==
== Lazarus: 0.9.30-0
== FPC: 2.4.4
== Lazy Puppy (Linux 32bit)
== Puppy FatDog (Linux 64 bit)
== Windows XP SP3 (Windows 32-bit)
== Windows 7 SP1 (Windows 64-bit)
==
== Version History
== 1.0
   [20120319]
   General code for class
   Debugging and Logging properties/methods
   OldHandicap, NewHandicap and HandicapChanged property
== 1.1
   [20120321]
   AdjustHandicap written and tested OK
   ShowHelp added
   ShowReport method and Report property added
== 1.2
   [20120321]
   Playing Handicap and Exact EGA Handicap methods and properties added
   ParArrayElement and ScoreArrayElement indexed properties added
   CalculateTotalStablefordPoints and Reset methods added
   ShowHelp method text updated
== 1.3
== [20120329]
== Added BogeyRating property
// CALCULATING BOGEY RATING from Course Rating and Slope Rating
// Bogey rating minus Course Rating (C) multiplied by (5.381 men, 4.24 women)(K)  equals Slope Rating (S).
// (B-C) * K = S
// B-C = S/K
// B=(S/K) * C
==
== [20130326]
== Added GrossScore property
== Complete update to incorporate CONGU as well as EGA
== Use SetEGAMode or SetCONGUMode.  Default on Create() is EGAMode
== Function GetModeAsString returns 'EGA' or 'CONGU'
==
==
== Purpose:
== Class to adjust EGA golf handicaps using published EGA rules
==
== How to use:
== 1 ) Declare the variable in the interface section
   HCPClass:THCPClass;
== 2 ) Create the instance in the implementation section (form create)
   HCPClass:=THCPClass.Create;
== 3 ) Set the properties and call the public methods
== EXAMPLE CODE TO USE ADJUSTHANDICAP METHOD
   If NOT HCPClass.InitOK then HALT; // Check the class is (re)initialised
   HCPClass.OldHandicap:=28.2;
   HCPClass.StablefordPoints:=30;
   If HCPClass.AdjustHandicap=True then
      begin sMyHandicapVar:=HCPClass.NewHandicap; HCPClass.ShowReport; end;
   Else
       ShowMessage(HCPClass.ErrorString);
       (Optionally Set HCPClass.DebugMode and/or HCPClass.LogMode)
==
== Call HCPClass.ShowHelp method to display all methods and properties and more help.

== CONGU RULES: http://www.mygolfexperience.com/golf/uk/golf-handicap.asp
*)
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, ComCtrls, ExtCtrls, eventlog;

// Constants are visible to subclasses
const
  CR = LineEnding; // CR is easier to type!
  C_LOGFILENAME = 'hcpclass.log'; // In case eventlog unit is used
  C_HCPCLASSVERSION = '1.5.20130322.0';

  act_NOCHANGE = 0;
  act_NEEDSRAISING = 1;
  act_NEEDSREDUCING = 2;
  C_ActionArray: array[act_NOCHANGE..act_NEEDSREDUCING] of
    string = ('No change required', 'Adjusting up...', 'Adjusting down...');

  // I could have used arrays here but the CONST are clearer to read and change
  // EGA CONSTANTS
  C_EGACAT1BUFFERUPPER = 36;
  C_EGACAT1BUFFERLOWER = 35;
  C_EGACAT2BUFFERUPPER = 36;
  C_EGACAT2BUFFERLOWER = 34;
  C_EGACAT3BUFFERUPPER = 36;
  C_EGACAT3BUFFERLOWER = 33;
  C_EGACAT4BUFFERUPPER = 36;
  C_EGACAT4BUFFERLOWER = 32;
  C_EGACAT5BUFFERUPPER = 36;
  C_EGACAT5BUFFERLOWER = 31;
  C_EGABufferStringArray: array[1..5] of
    string = ('35-36', '34-36', '33-36', '32-36', '31-36'); //For display
  C_EGACAT5DECREASE = 0.5;
  C_EGACAT5INCREASE = 0.2;
  C_EGACAT4DECREASE = 0.4;
  C_EGACAT4INCREASE = 0.1;
  C_EGACAT3DECREASE = 0.3;
  C_EGACAT3INCREASE = 0.1;
  C_EGACAT2DECREASE = 0.2;
  C_EGACAT2INCREASE = 0.1;
  C_EGACAT1DECREASE = 0.1;
  C_EGACAT1INCREASE = 0.1;
  C_EGACAT5MINIMUM = 26.5;
  C_EGACAT5MAXIMUM = 36;
  C_EGACAT4MINIMUM = 18.5;
  C_EGACAT4MAXIMUM = 26.4;
  C_EGACAT3MINIMUM = 11.5;
  C_EGACAT3MAXIMUM = 18.4;
  C_EGACAT2MINIMUM = 4.5;
  C_EGACAT2MAXIMUM = 11.4;
  C_EGACAT1MINIMUM = 0;
  C_EGACAT1MAXIMUM = 4.4;
  C_EGAIncreaseStringArray: array[1..5] of string = ('0.1', '0.1', '0.1', '0.1', '0.2');
  // for display
  C_EGADecreaseStringArray: array[1..5] of string = ('0.1', '0.2', '0.3', '0.4', '0.5');
  // for display

  // CONGU CONSTANTS
  C_CONGUCAT1BUFFER = 1;
  C_CONGUCAT2BUFFER = 2;
  C_CONGUCAT3BUFFER = 3;
  C_CONGUCAT4BUFFER = 4;
  C_CONGUCAT5BUFFER = 5;
  C_CONGUBufferStringArray: array[1..5] of
    string = ('+1', '+2', '+3', '+4', '+5'); //For display
  C_CONGUCAT5DECREASE = 0.5;
  C_CONGUCAT5INCREASE = 0.2;
  C_CONGUCAT4DECREASE = 0.4;
  C_CONGUCAT4INCREASE = 0.1;
  C_CONGUCAT3DECREASE = 0.3;
  C_CONGUCAT3INCREASE = 0.1;
  C_CONGUCAT2DECREASE = 0.2;
  C_CONGUCAT2INCREASE = 0.1;
  C_CONGUCAT1DECREASE = 0.1;
  C_CONGUCAT1INCREASE = 0.1;
  C_CONGUCAT5MINIMUM = 28.5; // Ladies only
  C_CONGUCAT5MAXIMUM = 36;   // Ladies only
  C_CONGUCAT4MINIMUM = 20.5;
  C_CONGUCAT4MAXIMUM = 28.4; // Ladies only > 28.0
  C_CONGUCAT3MINIMUM = 12.5;
  C_CONGUCAT3MAXIMUM = 20.4;
  C_CONGUCAT2MINIMUM = 5.5;
  C_CONGUCAT2MAXIMUM = 12.4;
  C_CONGUCAT1MINIMUM = 0.1;
  C_CONGUCAT1MAXIMUM = 5.4;
  C_CONGUIncreaseStringArray: array[1..5] of string = ('0.1', '0.1', '0.1', '0.1', '0');
  // for display
  C_CONGUDecreaseStringArray: array[1..5] of string = ('0.1', '0.2', '0.3', '0.4', '0.5');
  // for display

  C_GOLDTEES = 0;
  C_BLACKTEES = 1;
  C_BLUETEES = 2;
  C_WHITETEES = 3;
  C_YELLOWTEES = 4;
  C_REDTEES = 5;
  C_TEECOLOURARRAY: array[C_GOLDTEES..C_REDTEES] of
    string = ('Gold', 'Black', 'Blue', 'White', 'Yellow', 'Red');

  C_GOLFMLURL = 'http://code.google.com/p/golfml/wiki/Branding'; // TODO

type
  THandicapMode = (EGA, CONGU); // only > V1.4

type
  THCPClass = class(TObject)
  private
    // PRIVATE VARS AND METHODS - Invisible outside this class
    // Utility property vars
    fDebugMode: boolean; // Property 'DebugMode'
    fLogMode: boolean; // Property 'LogMode'
    fLogFileName: string; // Property 'LogFileName'

    // Property vars for Adjust Handicap
    foldhandicap: single; // Property 'OldHandicap'
    fnewhandicap: single; // Property 'NewHandicap'
    fstablefordpoints: cardinal; // Property 'StablefordPoints'
    fGrossScore: cardinal; // Property GrossScore
    fhandicapchanged: boolean; // Property 'HandicapChanged'
    fHandicapAdjustmentReportString: string; // Property 'Report'

    // Property vars for Playing Handicap
    fPlayingHandicap: single; // Property PlayingHandicap
    fEGAHandicap: single; // Property 'EGAhandicap'

    // Property Vars for Exact EGA Handicap
    fTotalStablefordPoints: cardinal; // Property 'TotalStablefordPoints'
    fScoreArray: array[0..17] of cardinal; // Property 'ScoreArray'
    fAdjustedScoreArray: array[0..17] of integer;
    fAdjustedScore:Cardinal;
    fNetScore:Integer;

    // General vars
    fCurrentCategory: cardinal; // Current Category
    fCurrentHandicap: single; // Current Handicap
    fAction: word; // 0,1 or 2 (C_ActionArray)
    fEventLog: TEventLog; // Event log object used if fDebugMode=True
    fHelpForm: TForm; // Form object for ShowHelp

    pVersion: string;

    fHandicapMode: THandicapMode;

    // GETS and SETS
    procedure priv_SetErrorString(aString: string); // Sets 'Errorstring' property
    procedure priv_SetOldHandicap(aSingle: single); // Sets 'OldHandicap' property
    procedure priv_SetNewHandicap(aSingle: single); // Sets 'NewHandicap' property
    procedure priv_SetDebugMode(aBoolean: boolean); // Sets 'DebugMode' property
    procedure priv_SetLogMode(aBoolean: boolean); // Sets 'LogMode' property
    procedure priv_SetStablefordPoints(aCardinal: cardinal);
    // Sets 'StablefordPoints' property
    procedure priv_SetLogFileName(aString: string); // Sets 'LogFileName' property
    procedure priv_SetCoursePar(aCardinal: cardinal); // Sets property 'CoursePar'
    procedure priv_SetCourseRating(aSingle: single); // Sets property 'CourseRating'
    procedure priv_SetSlopeRating(aCardinal: cardinal); // Sets property 'SlopeRating'
    procedure priv_SetBogeyRating(aSingle: single); // Sets property 'BogeyRating'
    procedure priv_SetEGAHandicap(aSingle: single); // Sets property 'EGAHandicap'
    procedure priv_SetTotalStablefordPoints(aCardinal: cardinal);
    // Sets property 'TotalStablefordPoints'

    // Get/Sets for indexed public properties
    procedure priv_SetParArrayElement(const Aindex: integer; AInteger: integer);
    function priv_GetParArrayElement(const Aindex: integer): integer;
    procedure priv_SetScoreArrayElement(const Aindex: integer; AInteger: integer);
    function priv_GetScoreArrayElement(const Aindex: integer): integer;
    procedure priv_SetStrokeIndexArrayElement(const Aindex: integer; AInteger: integer);
    function priv_GetStrokeIndexArrayElement(const Aindex: integer): integer;

    // GENERAL
    procedure priv_ShowDebugInfo(Fmt: string; Args: array of const);
    // Only if DebugMode=True
    function priv_EGAGetCategory: cardinal; // Returns category of fCurrentHandicap
    function priv_CONGUGetCategory: cardinal; // Returns category of fCurrentHandicap
    function priv_EGAIsInBuffer: boolean;
    // Returns true if handicap is in buffer zone.  Sets fAction.
    function priv_CONGUIsInBuffer: boolean;
    function priv_EGAReduceHandicap: single;
    // Uses fstablefordpoints. Changes fCurrentHandicap
    function priv_EGAAdjustHandicap: boolean; // Called by Public Method
    function priv_CONGUAdjustHandicap: boolean; // Called by Public Method
    procedure ShowMethods(Sender: TObject); // Used by ShowHelp method;
    procedure ShowProperties(Sender: TObject); // Used by ShowHelp method;
    procedure ShowInfo(Sender: TObject); // Used by ShowHelp method;
  protected
    // PROTECTED VARS AND METHODS
    // These can be overwritten in a descendant var or method with the same name
    pErrorString: string; // Property 'ErrorString'
    pCourseRating: single; // Property 'CourseRating'
    pBogeyRating: single; // Property 'BogeyRating'
    pSlopeRating: cardinal; // Property 'SlopeRating'
    pCoursePar: cardinal; // Property 'CoursePar'
    pParArray: array[0..17] of cardinal; // Property 'ParArray'
    pStrokeIndexArray: array[0..17] of cardinal; // Property 'StrokeIndexArray'
    pBannerText, pMethodsHelpText, pPropertiesHelpText, pInfoHelpText: string;
    // ShowHelp
    function prot_InitObj: boolean; virtual; // Init vars

    // Abstract function for all descendants to implement as they please
    // It doesn't exist in this class.
    function GetCourseInfoFromFile: boolean; virtual; abstract;

  public
    // PUBLIC VARS METHODS AND NON-VISUAL PROPERTIES
    constructor Create; // Note no override for Constructor
    destructor Destroy; override;
    function InitOK: boolean; // If myObj.InitOK then proceed...
    procedure Reset; // Resets all the vars and properties. (calls prot_InitObj)
    // DOES NOT CHANGE EGA or CONGU mode (only Create() sets the mode to EGA)

    // Manage fHandicapMode via methods
    procedure SetEGAMode; // Some procedures (AdjustHandicap) rely on the mode being set
    procedure SetCONGUMode;
    function GetModeAsString: string;
    function GetModeAsInteger: Integer; //0=EGA, 1=CONGU; Use to temporarily store mode
    Procedure SetModeAsInteger(iMode:Integer); //0=EGA, 1=CONGU; Use to restore mode
    function GetCategory(sHandicap:Single):Cardinal;

    function AdjustHandicap: boolean;
    // The main method.  Set OldHandicap and Stablefordpoints property first.
    // Sets NewHandicap

    function GetPlayingHandicap: single; // (EGA)
    // Uses fEGAhandicap,pCourseRating,pSlopeRating,pCoursePar

    // Calculating an EGA New Handicap from a 'best-of-3' Scorecard on a course and tee
    function CalculateHcp36StablefordPoints: cardinal;// Uses pParArray,fScoreArray
    // Sets fTotalStablefordPoints and fGrossScore
    function GetNewExactEGAHandicap: single;
    // Uses ONLY fTotalStablefordPoints,pSlopeRating
    // From a 'Best-of-3' scorecard on any course
    // Sets EGAHandicap property (calculated from scratch)


    function CalculateStableFord(cFromHoleNumber, cToHoleNumber: cardinal): cardinal;
    // Uses Assigned values in pParArray,fScoreArray,fStrokeIndexArray,fPlayingHandicap
    // To caculate a Stableford score for one or more holes
    // NOTE cFrom and cTo are ZERO-BASED!
    // NOTE Sets TotalStablefordPoints and GrossScore property

    function CalculateConguAdjustedScore(sHandicap:Single;cFromHoleNumber, cToHoleNumber: cardinal): cardinal;
    // Uses Assigned values in pParArray,fScoreArray,fStrokeIndexArray
    // To caculate a Congu Adjusted score for one or more holes
    // NOTE cFrom and cTo are ZERO-BASED!
    // NOTE Sets GrossScore property

    procedure ShowHandicapAdjustmentReport; // Displays fHandicapAdjustmentReportString in a MessageBox
    procedure ShowHelp; // Constructs a Help Form on-the-fly and displays it

    // Indexed property types cannot be Published, so put them in the Public section
    // pParArray and fScoreArray would be populated or read via a for-loop
    // TODO: Set a HoleIndex property before Reading/Writing Par, StrokeIndex and Score properties
    property ParArrayElement[Aindex: integer]: integer
      read priv_GetParArrayElement write priv_SetParArrayElement;
    property ScoreArrayElement[Aindex: integer]: integer
      read priv_GetScoreArrayElement write priv_SetScoreArrayElement;
    property StrokeIndexArrayElement[Aindex: integer]: integer
      read priv_GetStrokeIndexArrayElement write priv_SetStrokeIndexArrayElement;

  published
    // PUBLIC VISUAL PROPERTIES
    // These properties could be displayed in a visual component
    // Some read-only, some read + write
    // Default values are set in prot_InitObj
    property ErrorString: string read pErrorString write priv_SetErrorString;
    // Most methods in this class set ErrorString automatically,
    // but the user can write to it and use it in their error handling
    property DebugMode: boolean read fDebugMode write priv_SetDebugMode;
    // DebugMode=True triggers runtime messages for testing and also logging
    property LogFileName: string read fLogFileName write priv_SetLogFileName;
    property Logging: boolean read fLogMode write priv_SetLogMode;
    // DebugMode can be False whilst LogMode is True
    property Version: string read pVersion;

    // EGA
    property OldHandicap: single read foldhandicap write priv_SetOldHandicap;
    property NewHandicap: single read fnewhandicap write priv_SetNewHandicap;

    // CONGU
    property GrossScore: cardinal read fGrossScore write fGrossScore;
    property AdjustedScore:cardinal read fAdjustedScore;
    property NetScore:Integer read fNetScore write fNetScore;

    property HandicapChanged: boolean read fhandicapchanged;
    property Category: cardinal read fCurrentCategory;
    property HandicapAdjustmentReport: string read fHandicapAdjustmentReportString;

    property CoursePar: cardinal read pCoursePar write priv_SetCoursePar;
    property CourseRating: single read pCourseRating write priv_SetCourseRating;
    property SlopeRating: cardinal read pSlopeRating write priv_SetSlopeRating;
    property BogeyRating: single read pBogeyRating write priv_SetBogeyRating;
    property PlayingHandicap: single read fPlayingHandicap;

    property EGAHandicap: single read fEGAHandicap write priv_SetEGAHandicap;
    // Set by GetNewExactEGAHandicap from a 'Best-of-3' card

    property StablefordPoints: cardinal read fstablefordpoints
      write priv_SetStablefordPoints;
    // StablefordPoints used in AdjustHandicap routines

    property TotalStablefordPoints: cardinal
      read fTotalStablefordPoints write priv_SetTotalStablefordPoints;
    // TotalStablefordPoints set by CalculateHcp36StablefordPoints and CalculateStableFord


  end;



implementation
// *********************************************************************
constructor THCPClass.Create;
begin
  // First we call the existing the TObject.Create code
  inherited Create;
  // ..and then add stuff

  // Set up the Event Log parameters.
  // Log is inactive unless DebugMode=True OR LogMode=True;
  fEventLog := TEventLog.Create(nil);
  {$IFDEF LINUX}
  fEventLog.Filename := '/var/log/' + C_LOGFILENAME;
  {$ELSE}
  fEventLog.Filename := Application.Location + C_LOGFILENAME;
  {$ENDIF}
  fLogFileName := fEventLog.Filename; // Set the property var
  fEventLog.LogType := ltFile; // or ltSystem
  fDebugMode := False; // User has to set it to true
  fLogMode := False; // User has to set it to true
  fHandicapMode := EGA; // Use SetEGAMode or SetCONGUMode after Create
  // Note prot_InitObj and Reset retains fHandicapMode
end;
// *********************************************************************
destructor THCPClass.Destroy;
begin
  // First do any tidy-up code before calling the inherited Destroy
  fEventLog.Free;
  inherited Destroy;
end;
// *********************************************************************
// PROTECTED METHODS
// *********************************************************************
function THCPClass.prot_InitObj: boolean;
  // PROTECTED
  // Do initialisation stuff here to reset the
  // vars/properties etc. to their default values/states
var
  cHoleCount: cardinal;
  s: string;
begin
  fOldHandicap := 0; // Scratch
  fNewHandicap := 0; // Scratch
  fstablefordpoints := 36; // Results in unchanged handicap value
  fGrossScore := 0; // Used in CalculateHcp36StablefordPoints
  fAdjustedScore:=0; // CONGU
  fNetScore:=0; // CONGU
  fhandicapchanged := False; // Property value
  fAction := act_NOCHANGE; // No change to handicap needed
  fHandicapAdjustmentReportString := 'Nothing to report';

  pCoursePar := 72;
  pCourseRating := 72; // Property 'CourseRating'
  pSlopeRating := 113; // Property 'SlopeRating'
  fPlayingHandicap := 0; // Property PlayingHandicap
  fEGAHandicap := 0; // Property 'EGAhandicap'
  fTotalStablefordPoints := 36;
  for cHoleCount := 0 to 17 do
  begin
    pParArray[cHoleCount] := 4; // All par-4's
    fScoreArray[cHoleCount] := 4; // Score of Par
    pStrokeIndexArray[cHoleCount] := cHoleCount + 1; // Arbitary
  end;
  pVersion := C_HCPCLASSVERSION;
  // The help text in ShowHelp can be overridden by a descendant
  s := CR;
  s += 'HCPClass: an EGA and CONGU Handicap calculator by Gordon Bamber' + CR;
  s += 'Version ' + pVersion;
  pBannerText := s;

  s := 'Public Methods' + CR + CR;
  s += '1: Function InitOK (True=success, False=Exception)' + CR;
  s += '2: Procedure Reset: Resets all properties to default' + CR;
  s += '3: Procedure ShowHandicapAdjustmentReport' + CR;
  s += '   (used after calling AdjustHandicap method)' + CR;
  s += '4: Procedure ShowHelp: Shows this help text in a window' + CR;
  s += '5: Function AdjustHandicap: (Click [More Info] button)' + CR;
  s += '6: Function GetPlayingHandicap: (Click [More Info] button)' + CR;
  s += '7: Function CalculateHcp36StablefordPoints: (Click [More Info] button)' + CR;
  s += '8: Function GetNewExactEGAHandicap: (Click [More Info] button)';
  pMethodsHelpText := s;

  s := '1: DebugMode:boolean 2: Logging:boolean both read/write' + CR;
  s += '3: LogFileName:String read/write' + CR;
  s += '4: OldHandicap, 5: NewHandicap:Single both read/write' + CR;
  s += '6: StablefordPoints:Cardinal read/write' + CR;
  s += '7: HandicapChanged:Boolean readonly' + CR;
  s += '8: Category:Cardinal readonly' + CR;
  s += '9: HandicapAdjustmentReport:String readonly' + CR;
  s += '10:ErrorString :String read/write' + CR;
  s += '11:ParArrayElement[index]:Cardinal read/write [0..17]' + CR;
  s += '12:ScoreArrayElement[index]:Cardinal read/write [0..17]' + CR;
  s += '13:CoursePar:Cardinal' + CR;
  S += '14:SlopeRating:Cardinal read/write' + CR;
  s += '15:EGAHandicap:Single read/write' + CR;
  s += '16:TotalStablefordPoints:Cardinal read/write' + CR;
  s += '17:Version:String readonly';
  pPropertiesHelpText := s;

  s := 'There are 4 main functions - all following EGA rules.' + CR;
  s += '(Call Reset method to (re)initialise the properties)' + CR + CR;
  s += '1 ) AdjustHandicap (sets NewHandicap property)' + CR;
  s += 'Set OldHandicap and StablefordPoints property first' + CR;
  s += '2 ) GetPlayingHandicap (sets PlayingHandicap property)' + CR;
  s += 'Set EGAhandicap,CourseRating,SlopeRating,CoursePar first' + CR;
  s += '3 ) CalculateHcp36StablefordPoints (sets TotalStablefordPoints property)' + CR;
  s += 'Set ParArray,ScoreArray to retrieve TotalStablefordPoints (Hcp36 score)' + CR;
  s += '4 ) GetNewExactEGAHandicap (sets EGAHandicap property)' + CR;
  s += 'Set SlopeRating and TotalStablefordPoints to retrieve EGAHandicap';
  pInfoHelpText := s;

  Result := True;
end;
// *********************************************************************
function THCPClass.priv_CONGUAdjustHandicap: boolean; // Called by Public Method
// Uses fNetScore,fAdjustedScore,fCurrentCategory
Var fTemp:single;
  cCount:Cardinal;
begin
  ErrorString := 'THCPClass.priv_CONGUAdjustHandicap';
  Result := False;
  fCurrentHandicap := fOldHandicap; // Fetch from property
  fNewHandicap:=fOldHandicap;
  fCurrentCategory := priv_CONGUGetCategory;
  fhandicapchanged := False; // Default is not changed

  fHandicapAdjustmentReportString := 'Summary:' + CR + CR; // Assign
  fHandicapAdjustmentReportString += Format('Handicap %.1f (Cat. %d)%s',
    [fOldHandicap, fCurrentCategory, CR]);
  fHandicapAdjustmentReportString += Format('Net Score: %d%s', [fNetScore, CR]);

  if priv_CONGUIsInBuffer then
    fHandicapAdjustmentReportString += Format('Within buffer %s%s%s',
      [C_CONGUBufferStringArray[fCurrentCategory], CR, CR])
  else
    if fNetScore > 0 then fHandicapAdjustmentReportString += Format('Outside buffer %s%s%s',
      [C_CONGUBufferStringArray[fCurrentCategory], CR, CR]);
  fHandicapAdjustmentReportString += Format('%s%s', [C_ActionArray[fAction], CR]);
    case fAction of
      act_NOCHANGE: fCurrentHandicap := fOldHandicap;
      act_NEEDSRAISING:
        begin
          case fCurrentCategory of
            1: fCurrentHandicap := fCurrentHandicap + C_CONGUCAT1INCREASE;
            2: fCurrentHandicap := fCurrentHandicap + C_CONGUCAT2INCREASE;
            3: fCurrentHandicap := fCurrentHandicap + C_CONGUCAT3INCREASE;
            4: fCurrentHandicap := fCurrentHandicap + C_CONGUCAT4INCREASE;
            5: fCurrentHandicap := fCurrentHandicap + C_CONGUCAT5INCREASE;
          end;
          fhandicapchanged := True;
          fHandicapAdjustmentReportString +=
            Format('1: %.1f -> %.1f (+%s)%s',
            [foldhandicap, fCurrentHandicap,
            C_CONGUIncreaseStringArray[fCurrentCategory], CR]);
        end;
      act_NEEDSREDUCING:
        begin
             for cCount := 1 to ABS(fNetScore) do
                 begin
                    fTemp := fCurrentHandicap;
                    case fCurrentCategory of
                      1: fCurrentHandicap := fCurrentHandicap - C_CONGUCAT1DECREASE;
                      2: fCurrentHandicap := fCurrentHandicap - C_CONGUCAT2DECREASE;
                      3: fCurrentHandicap := fCurrentHandicap - C_CONGUCAT3DECREASE;
                      4: fCurrentHandicap := fCurrentHandicap - C_CONGUCAT4DECREASE;
                      5: fCurrentHandicap := fCurrentHandicap - C_CONGUCAT5DECREASE;
                    end;
                fHandicapAdjustmentReportString += Format('%d: Cat %d, %.1f -> %.1f (-%s)%s',
                  [(cCount), fCurrentCategory, fTemp,
                  fCurrentHandicap, C_CONGUDecreaseStringArray[fCurrentCategory], CR]);
                fCurrentCategory := priv_CONGUGetCategory; // New category?
                end;
             fhandicapchanged := True;
        end;
      end;
        fNewHandicap := fCurrentHandicap;

        if fhandicapchanged then
          if fNewHandicap > fOldHandicap then
            fHandicapAdjustmentReportString += Format('%sNew Handicap: %.1f (+%.1f) Cat.%d',
              [CR, fNewHandicap, (fNewHandicap - fOldHandicap), fCurrentCategory])
          else
            fHandicapAdjustmentReportString += Format('%sNew Handicap: %.1f (-%.1f) Cat.%d',
              [CR, fNewHandicap, (fOldHandicap - fNewHandicap), fCurrentCategory]);
end;
// *********************************************************************
function THCPClass.priv_EGAAdjustHandicap: boolean;
  // PROTECTED
  // Return value goes to public method
  // Sets fCurrentHandicap and fNewHandicap
begin
  ErrorString := 'THCPClass.priv_EGAAdjustHandicap';
  Result := False;
  fCurrentHandicap := fOldHandicap; // Fetch from property
  fCurrentCategory := priv_EGAGetCategory;
  fhandicapchanged := False; // Default is not changed

  fHandicapAdjustmentReportString := 'Summary:' + CR + CR; // Assign
  fHandicapAdjustmentReportString += Format('Handicap %.1f (Cat. %d)%s',
    [fOldHandicap, fCurrentCategory, CR]);
  fHandicapAdjustmentReportString += Format('Stableford points: %d%s', [fStablefordPoints, CR]);
  if priv_EGAIsInBuffer then
    fHandicapAdjustmentReportString += Format('Within buffer %s%s%s',
      [C_EGABufferStringArray[fCurrentCategory], CR, CR])
  else
    fHandicapAdjustmentReportString += Format('Outside buffer %s%s%s',
      [C_EGABufferStringArray[fCurrentCategory], CR, CR]);

  //HandicapAdjustmentReport:
  fHandicapAdjustmentReportString += Format('%s%s', [C_ActionArray[fAction], CR]);
  priv_ShowDebugInfo
  ('THCPClass.priv_EGAAdjustHandicap(before) : OldHandicap=%f, NewHandicap=%f, Stableford=%d Category=%d, fAction=%s',
    [fOldHandicap, fNewHandicap, fstablefordpoints, fCurrentCategory,
    C_ActionArray[fAction]]);

  case fAction of
    act_NOCHANGE: fCurrentHandicap := fOldHandicap;
    act_NEEDSRAISING:
    begin
      case fCurrentCategory of
        1: fCurrentHandicap := fCurrentHandicap + C_EGACAT1INCREASE;
        2: fCurrentHandicap := fCurrentHandicap + C_EGACAT2INCREASE;
        3: fCurrentHandicap := fCurrentHandicap + C_EGACAT3INCREASE;
        4: fCurrentHandicap := fCurrentHandicap + C_EGACAT4INCREASE;
        5: fCurrentHandicap := fCurrentHandicap + C_EGACAT5INCREASE;
      end;
      fhandicapchanged := True;
      fHandicapAdjustmentReportString +=
        Format('1: %.1f -> %.1f (+%s)%s',
        [foldhandicap, fCurrentHandicap,
        C_EGAIncreaseStringArray[fCurrentCategory], CR]);
    end;
    act_NEEDSREDUCING:
    begin
      fCurrentHandicap := priv_EGAReduceHandicap;
      fhandicapchanged := True;
    end;
  end;
  fNewHandicap := fCurrentHandicap;

  if fhandicapchanged then
    if fNewHandicap > fOldHandicap then
      fHandicapAdjustmentReportString += Format('%sNew Handicap: %.1f (+%.1f) Cat.%d',
        [CR, fNewHandicap, (fNewHandicap - fOldHandicap), fCurrentCategory])
    else
      fHandicapAdjustmentReportString += Format('%sNew Handicap: %.1f (-%.1f) Cat.%d',
        [CR, fNewHandicap, (fOldHandicap - fNewHandicap), fCurrentCategory]);

  priv_ShowDebugInfo
  ('THCPClass.priv_EGAAdjustHandicap(after) : OldHandicap=%f, NewHandicap=%f, Stableford=%d Category=%d, fAction=%s',
    [fOldHandicap, fNewHandicap, fstablefordpoints, fCurrentCategory,
    C_ActionArray[fAction]]);
  Result := True;
end;
// *********************************************************************
// PRIVATE METHODS
// *********************************************************************
procedure THCPClass.priv_ShowDebugInfo(Fmt: string; Args: array of const);
// DebugMode and/or Logging must be set for this to do anything at all
begin
  if fDebugMode then
    MessageDlg(Format(Fmt, Args), mtInformation, [mbOK], 0);
  if fLogMode then
    fEventLog.Debug(Fmt, Args);
end;
// *********************************************************************
function THCPClass.priv_CONGUIsInBuffer: boolean;
// Uses fNetScore and fCategory
// Sets fAction
// If Handicap is in appropriate buffer zone, return TRUE
// and fAction:=act_NOCHANGE

// If Handicap is ouside buffer zone, return FALSE
// and fAction:=act_NEEDSRAISING or fAction:=act_NEEDSREDUCING
begin
     Result := False; // Default unless changed by being in buffer zone
     fAction := act_NOCHANGE; // Default unless changed by being outside buffer zone
     If fNetScore < 0 then
       begin
         fAction := act_NEEDSREDUCING;
         Exit; // Bail out
       end;
     case fCurrentCategory of
       5:
         If fNetScore <= C_CONGUCAT5BUFFER then
            begin
                 Result := True; // In buffer zone
                 Exit; // Bail out
            end
            else
            begin
              fAction := act_NEEDSRAISING;
              Exit; // Bail out
            end;
       4:
         If fNetScore <= C_CONGUCAT4BUFFER then
          begin
               Result := True; // In buffer zone
               Exit; // Bail out
          end
          else
          begin
            fAction := act_NEEDSRAISING;
            Exit; // Bail out
          end;
       3:
         If fNetScore <= C_CONGUCAT3BUFFER then
          begin
               Result := True; // In buffer zone
               Exit; // Bail out
          end
          else
          begin
            fAction := act_NEEDSRAISING;
            Exit; // Bail out
          end;
       2:
         If fNetScore <= C_CONGUCAT2BUFFER then
          begin
               Result := True; // In buffer zone
               Exit; // Bail out
          end
          else
          begin
            fAction := act_NEEDSRAISING;
            Exit; // Bail out
          end;
       1:
         If fNetScore <= C_CONGUCAT1BUFFER then
          begin
               Result := True; // In buffer zone
               Exit; // Bail out
          end
          else
          begin
            fAction := act_NEEDSRAISING;
            Exit; // Bail out
          end;
     end;

end;

// *********************************************************************
function THCPClass.priv_EGAIsInBuffer: boolean;
  // Uses fStablefordpoints and fCategory
  // Sets fAction
  // If Handicap is in appropriate buffer zone, return TRUE
  // and fAction:=act_NOCHANGE

  // If Handicap is ouside buffer zone, return FALSE
  // and fAction:=act_NEEDSRAISING or fAction:=act_NEEDSREDUCING
begin
  Result := False; // Default unless changed by being in buffer zone
  fAction := act_NOCHANGE; // Default unless changed by being outside buffer zone
  case fCurrentCategory of
    5:
      if (fStablefordpoints <= C_EGACAT5BUFFERUPPER) and
        (fStablefordpoints >= C_EGACAT5BUFFERLOWER) then
      begin
        Result := True; // In buffer zone
        Exit; // Bail out
      end
      else
      if (fStablefordpoints < C_EGACAT5BUFFERLOWER) then
      begin
        fAction := act_NEEDSRAISING;
        Exit; // Bail out
      end
      else
      if (fStablefordpoints > C_EGACAT5BUFFERUPPER) then
      begin
        fAction := act_NEEDSREDUCING;
        Exit; // Bail out
      end;
    4:
      if (fStablefordpoints <= C_EGACAT4BUFFERUPPER) and
        (fStablefordpoints >= C_EGACAT4BUFFERLOWER) then
      begin
        Result := True; // In buffer zone
        Exit; // Bail out
      end
      else
      if (fStablefordpoints < C_EGACAT4BUFFERLOWER) then
      begin
        fAction := act_NEEDSRAISING;
        Exit; // Bail out
      end
      else
      if (fStablefordpoints > C_EGACAT4BUFFERUPPER) then
      begin
        fAction := act_NEEDSREDUCING;
        Exit; // Bail out
      end;
    3:
      if (fStablefordpoints <= C_EGACAT3BUFFERUPPER) and
        (fStablefordpoints >= C_EGACAT3BUFFERLOWER) then
      begin
        Result := True; // In buffer zone
        Exit; // Bail out
      end
      else
      if (fStablefordpoints < C_EGACAT3BUFFERLOWER) then
      begin
        fAction := act_NEEDSRAISING;
        Exit; // Bail out
      end
      else
      if (fStablefordpoints > C_EGACAT3BUFFERUPPER) then
      begin
        fAction := act_NEEDSREDUCING;
        Exit; // Bail out
      end;
    2:
      if (fStablefordpoints <= C_EGACAT2BUFFERUPPER) and
        (fStablefordpoints >= C_EGACAT2BUFFERLOWER) then
      begin
        Result := True; // In buffer zone
        Exit; // Bail out
      end
      else
      if (fStablefordpoints < C_EGACAT2BUFFERLOWER) then
      begin
        fAction := act_NEEDSRAISING;
        Exit; // Bail out
      end
      else
      if (fStablefordpoints > C_EGACAT2BUFFERUPPER) then
      begin
        fAction := act_NEEDSREDUCING;
        Exit; // Bail out
      end;
    1:
      if (fStablefordpoints <= C_EGACAT1BUFFERUPPER) and
        (fStablefordpoints >= C_EGACAT1BUFFERLOWER) then
      begin
        Result := True; // In buffer zone
        Exit; // Bail out
      end
      else
      if (fStablefordpoints < C_EGACAT1BUFFERLOWER) then
      begin
        fAction := act_NEEDSRAISING;
        Exit; // Bail out
      end
      else
      if (fStablefordpoints > C_EGACAT1BUFFERUPPER) then
      begin
        fAction := act_NEEDSREDUCING;
        Exit; // Bail out
      end;
  end;
end;
// *********************************************************************
function THCPClass.priv_EGAReduceHandicap: single;
  // Called by AdjustHandicap.  Uses fstablefordpoints and fCurrentHandicap
  // Returns reduced handicap
var
  cCount: cardinal;
  fTemp: single;
begin
  fCurrentCategory := priv_EGAGetCategory; // Confirm category?
  priv_ShowDebugInfo
  ('THCPClass.priv_EGAReduceHandicap(before) : OldHandicap=%f, NewHandicap=%f, Stableford=%d Category=%d, fAction=%s',
    [fOldHandicap, fNewHandicap, fstablefordpoints, fCurrentCategory,
    C_ActionArray[fAction]]);
  for cCount := fstablefordpoints downto 37 do
  begin
    fTemp := fCurrentHandicap;
    case fCurrentCategory of
      1: fCurrentHandicap := fCurrentHandicap - C_EGACAT1DECREASE;
      2: fCurrentHandicap := fCurrentHandicap - C_EGACAT2DECREASE;
      3: fCurrentHandicap := fCurrentHandicap - C_EGACAT3DECREASE;
      4: fCurrentHandicap := fCurrentHandicap - C_EGACAT4DECREASE;
      5: fCurrentHandicap := fCurrentHandicap - C_EGACAT5DECREASE;
    end;
    fHandicapAdjustmentReportString += Format('%d: Cat %d, %.1f -> %.1f (-%s)%s',
      [(fstablefordpoints - cCount + 1), fCurrentCategory, fTemp,
      fCurrentHandicap, C_EGADecreaseStringArray[fCurrentCategory], CR]);
    fCurrentCategory := priv_EGAGetCategory; // New category?
  end;
  priv_ShowDebugInfo
  ('THCPClass.priv_EGAReduceHandicap(after) : OldHandicap=%f, NewHandicap=%f, Stableford=%d Category=%d, fAction=%s',
    [fOldHandicap, fNewHandicap, fstablefordpoints, fCurrentCategory,
    C_ActionArray[fAction]]);

  Result := fCurrentHandicap;
end;
// *********************************************************************
function THCPClass.priv_EGAGetCategory: cardinal;
  // Returns category according to fCurrentHandicap
begin
  Result := 1;//Cat 1
  ErrorString := 'THCPClass.priv_EGAGetCategory';
  if fCurrentHandicap > C_EGACAT2MINIMUM then
    Inc(Result); // Cat2
  if fCurrentHandicap > C_EGACAT3MINIMUM then
    Inc(Result); // Cat3
  if fCurrentHandicap > C_EGACAT4MINIMUM then
    Inc(Result); // Cat4
  if fCurrentHandicap > C_EGACAT5MINIMUM then
    Inc(Result); // Cat5
  fCurrentCategory:=Result;
  priv_ShowDebugInfo
  ('priv_EGAGetCategory : Old Handicap=%f, Current Handicap=%f, Stableford=%d, Old Category=%d, New Category=%d',
    [fOldHandicap, fCurrentHandicap, fstablefordpoints, fCurrentCategory, Result]);
end;
// *********************************************************************
function THCPClass.priv_CONGUGetCategory: cardinal;
  // Returns category according to fCurrentHandicap
begin
  Result := 1;//Cat 1
  ErrorString := 'THCPClass.priv_CONGUGetCategory';
  if fCurrentHandicap > C_CONGUCAT2MINIMUM then
    Inc(Result); // Cat2
  if fCurrentHandicap > C_CONGUCAT3MINIMUM then
    Inc(Result); // Cat3
  if fCurrentHandicap > C_CONGUCAT4MINIMUM then
    Inc(Result); // Cat4
  if fCurrentHandicap > C_CONGUCAT5MINIMUM then
    Inc(Result); // Cat5
  fCurrentCategory:=Result;
  priv_ShowDebugInfo
  ('priv_CONGUGetCategory : Old Handicap=%f, Current Handicap=%f, Stableford=%d, Old Category=%d, New Category=%d',
    [fOldHandicap, fCurrentHandicap, fstablefordpoints, fCurrentCategory, Result]);
end;

// *********************************************************************
// PUBLIC METHODS
// *********************************************************************
// *********************************************************************
function THCPClass.CalculateConguAdjustedScore(sHandicap:Single;cFromHoleNumber, cToHoleNumber: cardinal): cardinal;
// Uses Assigned values in pParArray,fScoreArray,fStrokeIndexArray
// To caculate a Congu Adjusted score for one or more holes
// NOTE cFrom and cTo are ZERO-BASED!
// NOTE Sets GrossScore property and fAdjustedScoreArray
var
  iHole, iTemp, cGrossScore, cAdjustedTotalScore: cardinal;
  iRoundedHandicap:Integer;
begin
  cAdjustedTotalScore := 0;
  cGrossScore:=0;
  Result:=0;
  // Trap for Crap
  if (cFromHoleNumber < 0) or (cToHoleNumber > 17) then Exit;

  // Round Up for Integer Handicap
  iRoundedHandicap:=TRUNC(sHandicap + 0.5); // The CONGU Playing Handicap

  // For each hole: Real Score - StrokeIndex allowance = Adjusted Score
  // Stableford adjustment:  Maximum score is a DOUBLE BOGEY
  // i.e. Par 3 = 6, Par 4 = 7 Par 5 = 8
  // Add up Real Scores to give an ADJUSTED SCORE
  // Subtract your adjusted score from the Standard Scratch Score (Course Index)
  // Take this value (known as the Gross Score) and subtract your current handicap to get your Net Score.
  // Take your handicap category (1, 2, 3, 4, and 5) and determine the adjustment based on your buffer zone.


  // Calculate Adjusted Score
  for iHole := cFromHoleNumber to cToHoleNumber do
  begin
    fAdjustedScoreArray[iHole] := fScoreArray[iHole];
    // Take away any strokes due to Handicap and StrokeIndex
    if iRoundedHandicap > pStrokeIndexArray[iHole] then
      Dec(fAdjustedScoreArray[iHole], 1);
    if (iRoundedHandicap - 18) > pStrokeIndexArray[iHole] then
      Dec(fAdjustedScoreArray[iHole], 1);

    // Stableford Adjustment (max score is a double-bogey)
    // Also zero scores count as a double-bogey
    If (fAdjustedScoreArray[iHole] > (pParArray[iHole] + 3))
    OR (fAdjustedScoreArray[iHole] < 1)
    then
       fAdjustedScoreArray[iHole] := (pParArray[iHole] + 3);

    if (fScoreArray[iHole] >= (pParArray[iHole] + 3)) then
       fAdjustedScoreArray[iHole] := (pParArray[iHole] + 3);

    // Zero Score counts as double-bogey in GrossScore, too.
    if (fScoreArray[iHole] < 1) then
       Inc(cGrossScore,(pParArray[iHole] + 3))
    else
        Inc(cGrossScore,fScoreArray[iHole]);

    Inc(cAdjustedTotalScore,fAdjustedScoreArray[iHole]);
  end;
  fAdjustedScore:=cAdjustedTotalScore;
  Result:=fAdjustedScore;
  fGrossScore := cGrossScore;
  end;
function THCPClass.CalculateStableFord(cFromHoleNumber, cToHoleNumber:
  cardinal): cardinal;
  // Uses Assigned values in pParArray,fScoreArray,fStrokeIndexArray,fPlayingHandicap
  // To caculate a Stableford score for one or more holes
  // NOTE cFrom and cTo are ZERO-BASED!
  // NOTE Sets TotalStablefordPoints property
var
  iHole, iTemp, TotalPoints, cGrossScore: cardinal;
  MyParArray: array[0..17] of cardinal; // Pars based on Handicap
begin
  Result := 0;
  if (cFromHoleNumber < 0) or (cToHoleNumber > 17) then
    Exit;
  // Assign Par values based on cHandicap and fStrokeIndex Array
  for iHole := cFromHoleNumber to cToHoleNumber do
  begin
    MyParArray[iHole] := pParArray[iHole];
    if fPlayingHandicap > pStrokeIndexArray[iHole] then
      Inc(MyParArray[iHole], 1);
    if (fPlayingHandicap - 18) > pStrokeIndexArray[iHole] then
      Inc(MyParArray[iHole], 1);
  end;

  // Add up the points
  TotalPoints := 0;
  cGrossScore := 0;
  for iHole := cFromHoleNumber to cToHoleNumber do
  begin
    iTemp := fScoreArray[iHole];
    Inc(cGrossScore, iTemp);
    if iTemp = 0 then
      continue;
    if iTemp = MyParArray[iHole] + 1 then
      Inc(TotalPoints, 1);
    if iTemp = MyParArray[iHole] then
      Inc(TotalPoints, 2);
    if iTemp + 1 = MyParArray[iHole] then
      Inc(TotalPoints, 3);
    if iTemp + 2 = MyParArray[iHole] then
      Inc(TotalPoints, 4);
    if iTemp + 3 = MyParArray[iHole] then
      Inc(TotalPoints, 5);
    if iTemp + 4 = MyParArray[iHole] then
      Inc(TotalPoints, 6);
    if iTemp + 5 = MyParArray[iHole] then
      Inc(TotalPoints, 7);
    if iTemp + 6 = MyParArray[iHole] then
      Inc(TotalPoints, 8);
  end;
  Result := TotalPoints;
  fTotalStablefordPoints := TotalPoints;
  fGrossScore := cGrossScore;
end;
// *********************************************************************
function THCPClass.GetCategory(sHandicap:Single):Cardinal;
// Sets property Category (fCurrentCategory)
begin
    Result:=0;
    fCurrentHandicap:=sHandicap;
    If fHandicapMode=EGA then fCurrentCategory:=priv_EGAGetCategory
    else fCurrentCategory:=priv_CONGUGetCategory;
    Result:=fCurrentCategory;
end;
Procedure THCPClass.SetModeAsInteger(iMode:Integer); //0=EGA, 1=CONGU; Use to restore mode
begin
    SetEGAMode;
    if iMode > 0 then SetCONGUMode;
end;
function THCPClass.GetModeAsInteger: Integer; //0=EGA, 1=CONGU; Use to temporarily store mode
begin
    Result:=0;
    if fHandicapMode = CONGU then
      Result := 1;
end;
function THCPClass.GetModeAsString: string;
begin
  Result := 'EGA';
  if fHandicapMode = CONGU then
    Result := 'CONGU';
end;

procedure THCPClass.SetEGAMode;
begin
  fHandicapMode := EGA;
end;

procedure THCPClass.SetCONGUMode;
begin
  fHandicapMode := CONGU;
end;

procedure THCPClass.ShowInfo(Sender: TObject);
// Used by ShowHelp onClick event
var
  aButton: TBitBtn;
  s: string;
begin
  aButton := Sender as TBitBtn;
  s := pInfoHelpText;
  aButton.Parent.Controls[1].Caption := s;
end;
// *********************************************************************
procedure THCPClass.ShowMethods(Sender: TObject);
// Used by ShowHelp onClick event
var
  aButton: TBitBtn;
  s: string;
begin
  aButton := Sender as TBitBtn;
  s := pMethodsHelpText;
  aButton.Parent.Controls[1].Caption := s;
end;
// *********************************************************************
procedure THCPClass.ShowProperties(Sender: TObject);
// Used by ShowHelp onClick event
var
  aButton: TBitBtn;
  s: string;
begin
  aButton := Sender as TBitBtn;
  s := pPropertiesHelpText;
  aButton.Parent.Controls[1].Caption := s;
end;

procedure THCPClass.ShowHelp;
// Makes up a help form with controls on-the-fly
// This means the class code is standalone with no form unit code to include
var
  infoPanel, mainpanel: TLabel;
  CloseButton: TBitBtn;
  cmd_ShowMethods, cmd_ShowProperties, cmd_ShowInfo: TBitBtn;
  s: string;
begin
  // Default text
  s := pBannerText;
  // Create the form and controls in a TRY-FINALLY block
  try
    fHelpForm := TForm.Create(nil);
    infoPanel := TLabel.Create(nil);
    mainPanel := TLabel.Create(nil);
    CloseButton := TBitBtn.Create(nil);
    cmd_ShowMethods := TBitBtn.Create(nil);
    cmd_ShowProperties := TBitBtn.Create(nil);
    cmd_ShowInfo := TBitBtn.Create(nil);
    with fHelpForm do // Main Form
    begin
      Width := 400;
      Height := 480;
      Position := poScreenCenter;
      BorderStyle := bsSingle;
      Caption := 'Help for THCPClass';
      Formstyle := fsStayOnTop;
    end;
    with infopanel do // Header text
    begin
      Height := 70;
      Align := alTop;
      Alignment := taCenter;
      WordWrap := True;
      Caption := s;
      Parent := fHelpForm;
    end;
    with mainpanel do // Main text.  Control[1]
    begin
      Top := infopanel.Height;
      Height := 290;
      left := 10;
      Width := fHelpForm.Width - 40;
      Alignment := taLeftJustify;
      WordWrap := True;
      Caption := 'Click a button below for more information';
      Parent := fHelpForm;
    end;
    with cmd_ShowMethods do // Button
    begin
      Top := fHelpForm.Height - 40; // Change this to change all buttons .Top
      Left := 20;
      ; // Change this to change all buttons .Left
      kind := bkCustom;
      Caption := 'Methods';
      OnClick := @ShowMethods;
      Parent := fHelpForm;
    end;
    with cmd_ShowProperties do // Button
    begin
      Top := cmd_ShowMethods.Top;
      Left := cmd_ShowMethods.Left + cmd_ShowMethods.Width;
      kind := bkCustom;
      Caption := 'Properties';
      OnClick := @ShowProperties;
      Parent := fHelpForm;
    end;
    with cmd_ShowInfo do // Button
    begin
      Top := cmd_ShowMethods.Top;
      Left := cmd_ShowProperties.Left + cmd_ShowProperties.Width;
      kind := bkCustom;
      Caption := 'More Info';
      OnClick := @ShowInfo;
      Parent := fHelpForm;
    end;
    with CloseButton do // Button
    begin
      Top := cmd_ShowMethods.Top;
      Left := cmd_ShowInfo.Left + cmd_ShowInfo.Width + 50;
      kind := bkClose; // No event to write!
      Parent := fHelpForm;
    end;
    fHelpForm.Showmodal; // Wait for user to close the form.
  finally
    // Destroy the form and child controls here.
    mainpanel.Free;
    infopanel.Free;
    cmd_ShowMethods.Free;
    cmd_ShowProperties.Free;
    cmd_ShowInfo.Free;
    CloseButton.Free;
    fHelpForm.Free;
  end;
end;

// *********************************************************************
procedure THCPClass.ShowHandicapAdjustmentReport;
// Displays the text of the HandicapAdjustmentReport property in a dialog
begin
  MessageDlg(fHandicapAdjustmentReportString, mtInformation, [mbOK], 0);
end;

// *********************************************************************
function THCPClass.CalculateHcp36StablefordPoints: cardinal;
  // Uses pParArray,fScoreArray
  // Sets fTotalStablefordPoints and fGrossScore
var
  iCount, iTemp, TotalPoints, cGrossScore: cardinal;
  TeeArray: array[1..18] of cardinal;
begin
  Result := 0;
  for iCount := 1 to 18 do
    TeeArray[iCount] := pParArray[iCount - 1] + 2;
  TotalPoints := 0;
  cGrossScore := 0;
  for iCount := 1 to 18 do
  begin
    iTemp := fScoreArray[iCount - 1];
    Inc(cGrossScore, iTemp);
    if iTemp = 0 then
      continue;
    if iTemp = Teearray[iCount] + 1 then
      Inc(TotalPoints, 1);
    if iTemp = Teearray[iCount] then
      Inc(TotalPoints, 2);
    if iTemp + 1 = Teearray[iCount] then
      Inc(TotalPoints, 3);
    if iTemp + 2 = Teearray[iCount] then
      Inc(TotalPoints, 4);
    if iTemp + 3 = Teearray[iCount] then
      Inc(TotalPoints, 5);
    if iTemp + 4 = Teearray[iCount] then
      Inc(TotalPoints, 6);
    if iTemp + 5 = Teearray[iCount] then
      Inc(TotalPoints, 7);
    if iTemp + 6 = Teearray[iCount] then
      Inc(TotalPoints, 8);
  end;
  Result := TotalPoints;
  fTotalStablefordPoints := TotalPoints;
  fGrossScore := cGrossScore;
  priv_ShowDebugInfo
  ('THCPClass.CalculateHcp36StablefordPoints : fTotalStablefordPoints=%d',
    [fTotalStablefordPoints]);

end;
// *********************************************************************
function THCPClass.GetNewExactEGAHandicap: single;
  // Uses fTotalStablefordPoints,pSlopeRating
  // Called directly, gives unrounded result
  // Sets property EGAHandicap to rounded result
begin
  Result := 36 - ((fTotalStablefordPoints - 36) * (113 / pSlopeRating));
  fEGAHandicap := Result;
  fEGAHandicap := (Int((fEGAHandicap * 100) + 0.5)) / 100; // Round up to 2 Dec Places
  priv_ShowDebugInfo
  ('THCPClass.GetNewExactEGAHandicap : fTotalStablefordPoints=%d, pSlopeRating=%d, fEGAHandicap=%f, fEGAHandicap(exact)=%f',
    [fTotalStablefordPoints, pSlopeRating, fEGAHandicap, Result]);
end;
// *********************************************************************
function THCPClass.GetPlayingHandicap: single;
  // Uses fEGAHandicap,pCourseRating,pSlopeRating,pCoursePar
  // Called directly, gives unrounded result
  // Sets property PlayingHandicap to rounded result
begin
  Result := (fEGAhandicap * (pSlopeRating / 113)) + (pCourseRating - pCoursePar);
  fPlayingHandicap := Result;
  // INT(0.5 +Result); // or round UPWARDS to nearest Cardinal?
  fPlayingHandicap := (Int((fPlayingHandicap * 100) + 0.5)) / 100;
  // Round up to 2 Dec Places - better
  priv_ShowDebugInfo
  ('THCPClass.GetPlayingHandicap : fEGAhandicap=%f, pSlopeRating=%d, pCourseRating=%f, pCoursePar=%d, Playing Handicap=%f',
    [fEGAhandicap, pSlopeRating, pCourseRating, pCoursePar, fPlayingHandicap]);
end;

function THCPClass.AdjustHandicap: boolean;
  // The main Public Method.  Calls private method to do the deed.
begin
  Result := False;
  ErrorString := 'THCPClass.AdjustHandicap';
  If fHandicapMode = EGA then Result := priv_EGAAdjustHandicap; // Call the protected method
  If fHandicapMode = CONGU then Result := priv_CONGUAdjustHandicap; // Call the protected method
  priv_ShowDebugInfo
  ('THCPClass.AdjustHandicap : OldHandicap=%f, NewHandicap=%f, Stableford=%d Category=%d',
    [fOldHandicap, fNewHandicap, fstablefordpoints, fCurrentCategory]);
end;

procedure THCPClass.Reset;
// Resets all the properties to default values via protected method.
begin
  prot_InitObj;
end;

function THCPClass.InitOK: boolean;
  // PUBLIC
  // Call this after Create to check everything initialised OK
  // If Result=TRUE then proceed.
  // If Result=FALSE then halt the application.  It's a fatal error.
begin
  Result := False;
  ErrorString := 'THCPClass.InitOK failed.' + CR;
  ErrorString := ErrorString + 'This application is now unstable,' + CR;
  ErrorString := ErrorString + 'and you should terminate it now.';
  // Call InitOK after Create method and test for true/false.
  try
    Result := prot_InitObj;
    // Use the protected method.  We might want to add to it later.
  except
    raise Exception.Create(ErrorString);
  end;
end;

// *********************************************************************
// GET/SETS (ALL PRIVATE METHODS)
procedure THCPClass.priv_SetErrorString(aString: string);
begin
  if pErrorString <> aString then
    pErrorString := aString;
end;
// *********************************************************************
procedure THCPClass.priv_SetDebugMode(aBoolean: boolean);
// Sets 'DebugMode' property
begin
  fDebugMode := aBoolean;
  if fDebugMode then
    priv_SetLogMode(True);
  // Turning DebugMode on automatically turns logging mode on
  fEventLog.Info('THCPClass Debug mode set');
end;
// *********************************************************************
procedure THCPClass.priv_SetLogMode(aBoolean: boolean);
// Sets 'LogMode' property
begin
  fLogMode := aBoolean;
  fEventLog.Active := fLogMode;
  fEventLog.Info('THCPClass Log mode set');
end;
// *********************************************************************
procedure THCPClass.priv_SetOldHandicap(aSingle: single);
// Sets 'OldHandicap' property. No checking!
begin
  if fOldHandicap <> aSingle then
    fOldHandicap := aSingle;
end;
// *********************************************************************
procedure THCPClass.priv_SetNewHandicap(aSingle: single);
// Sets 'NewHandicap' property. No checking!
begin
  if fNewHandicap <> aSingle then
    fNewHandicap := aSingle;
end;
// *********************************************************************
procedure THCPClass.priv_SetStablefordPoints(aCardinal: cardinal);
// Sets 'StablefordPoints' property. No checking!
begin
  if fstablefordpoints <> aCardinal then
    fstablefordpoints := aCardinal;
end;
// *********************************************************************
procedure THCPClass.priv_SetLogFileName(aString: string);
// Sets 'LogFileName' property.
// Make sure eventlog isn't active if the filename is changed!
var
  IsActive: boolean;
begin
  if flogfilename <> aString then
  begin
    IsActive := fEventLog.Active;
    if IsActive then
      fEventLog.Active := False;
    flogfilename := aString;
    fEventLog.Filename := flogfilename;
    if IsActive then
      fEventLog.Active := True;
  end;
end;
// *********************************************************************
procedure THCPClass.priv_SetCourseRating(aSingle: single);
// Sets property 'CourseRating' No checking!
begin
  if pCourseRating <> aSingle then
    pCourseRating := aSingle;
end;
// *********************************************************************
procedure THCPClass.priv_SetBogeyRating(aSingle: single);
// Sets property 'BogeyRating' No Checking!
begin
  if pBogeyRating <> aSingle then
    pBogeyRating := aSingle;
end;
// *********************************************************************
procedure THCPClass.priv_SetSlopeRating(aCardinal: cardinal);
// Sets property 'SlopeRating' No checking!
begin
  if pSlopeRating <> aCardinal then
    pSlopeRating := aCardinal;
end;
// *********************************************************************
procedure THCPClass.priv_SetEGAHandicap(aSingle: single);
// Sets property 'EGAHandicap' No checking!
begin
  if fEGAHandicap <> aSingle then
    fEGAHandicap := aSingle;
end;
// *********************************************************************
procedure THCPClass.priv_SetCoursePar(aCardinal: cardinal);
// Sets property 'CoursePar' No checking!
begin
  if pCoursePar <> aCardinal then
    pCoursePar := aCardinal;
end;
// *********************************************************************
procedure THCPClass.priv_SetTotalStablefordPoints(aCardinal: cardinal);
// Sets property 'TotalStablefordPoints' No checking!
begin
  if fTotalStablefordPoints <> aCardinal then
    fTotalStablefordPoints := aCardinal;
end;
// *********************************************************************
function THCPClass.priv_GetParArrayElement(const Aindex: integer): integer;
  // Gets one element of pParArray[0..17].  Check the Index is in bounds
begin
  if (AIndex >= Low(pParArray)) and (AIndex <= High(pParArray)) then
    Result := pParArray[AIndex];
end;
// *********************************************************************
procedure THCPClass.priv_SetParArrayElement(const Aindex: integer; AInteger: integer);
// Sets one element of pParArray[0..17].  Check the Index is in bounds
begin
  if (AIndex >= Low(pParArray)) and (AIndex <= High(pParArray)) then
    pParArray[AIndex] := AInteger;
end;
// *********************************************************************
function THCPClass.priv_GetStrokeIndexArrayElement(const Aindex: integer): integer;
  // Gets one element of pStrokeIndexArray[0..17].  Check the Index is in bounds
begin
  if (AIndex >= Low(pStrokeIndexArray)) and (AIndex <= High(pStrokeIndexArray)) then
    Result := pStrokeIndexArray[AIndex];
end;
// *********************************************************************
procedure THCPClass.priv_SetStrokeIndexArrayElement(const Aindex: integer;
  AInteger: integer);
// Sets one element of pStrokeIndexArray[0..17].  Check the Index is in bounds
begin
  if (AIndex >= Low(pStrokeIndexArray)) and (AIndex <= High(pStrokeIndexArray)) then
    pStrokeIndexArray[AIndex] := AInteger;
end;
// *********************************************************************
function THCPClass.priv_GetScoreArrayElement(const Aindex: integer): integer;
  // Gets one element of fScoreArray[0..17].  Check the Index is in bounds
begin
  if (AIndex >= Low(fScoreArray)) and (AIndex <= High(fScoreArray)) then
    Result := fScoreArray[AIndex];
end;
// *********************************************************************
procedure THCPClass.priv_SetScoreArrayElement(const Aindex: integer; AInteger: integer);
// Sets one element of fScoreArray[0..17].  Check the Index is in bounds
begin
  if (AIndex >= Low(fScoreArray)) and (AIndex <= High(fScoreArray)) then
    fScoreArray[AIndex] := AInteger;
end;
// *********************************************************************

end.
