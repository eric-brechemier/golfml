unit thandicapclassunit;
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
   CalculateBestScore and Reset methods added
   ShowHelp method text updated
== 1.3
== [20120329]
== Added BogeyRating property
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
*)
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, ComCtrls, ExtCtrls,eventlog;

// Constants are visible to subclasses
CONST CR=LineEnding; // CR is easier to type!
CONST C_LOGFILENAME='hcpclass.log'; // In case eventlog unit is used
CONST C_HCPCLASSVERSION='1.3.20120329';

Const act_NOCHANGE=0;
Const act_NEEDSRAISING=1;
Const act_NEEDSREDUCING=2;
CONST C_ActionArray:Array[act_NOCHANGE..act_NEEDSREDUCING] Of  String
      =('No change required','Adjusting up...','Adjusting down...');

// I could have used arrays here but the CONST are clearer to read and change
CONST C_CAT1BUFFERUPPER=36;
CONST C_CAT1BUFFERLOWER=35;

CONST C_CAT2BUFFERUPPER=36;
CONST C_CAT2BUFFERLOWER=34;

CONST C_CAT3BUFFERUPPER=36;
CONST C_CAT3BUFFERLOWER=33;

CONST C_CAT4BUFFERUPPER=36;
CONST C_CAT4BUFFERLOWER=32;

CONST C_CAT5BUFFERUPPER=36;
CONST C_CAT5BUFFERLOWER=31;

CONST C_BufferStringArray:Array[1..5] of String
      =('35-36','34-36','33-36','32-36','31-36'); //For display

CONST C_CAT5DECREASE=0.5;
CONST C_CAT5INCREASE=0.2;

CONST C_CAT4DECREASE=0.4;
CONST C_CAT4INCREASE=0.1;

CONST C_CAT3DECREASE=0.3;
CONST C_CAT3INCREASE=0.1;

CONST C_CAT2DECREASE=0.2;
CONST C_CAT2INCREASE=0.1;

CONST C_CAT1DECREASE=0.1;
CONST C_CAT1INCREASE=0.1;

CONST C_CAT5MINIMUM=26.5;
CONST C_CAT5MAXIMUM=36;

CONST C_CAT4MINIMUM=18.5;
CONST C_CAT4MAXIMUM=26.4;

CONST C_CAT3MINIMUM=11.5;
CONST C_CAT3MAXIMUM=18.4;

CONST C_CAT2MINIMUM=4.5;
CONST C_CAT2MAXIMUM=11.4;

CONST C_CAT1MINIMUM=0;
CONST C_CAT1MAXIMUM=4.4;

CONST C_IncreaseStringArray:Array[1..5] of String
      =('0.1','0.1','0.1','0.1','0.2'); // for display
CONST C_DecreaseStringArray:Array[1..5] of String
      =('0.1','0.2','0.3','0.4','0.5'); // for display

CONST C_GOLDTEES=0;
CONST C_BLACKTEES=1;
CONST C_BLUETEES=2;
CONST C_WHITETEES=3;
CONST C_YELLOWTEES=4;
CONST C_REDTEES=5;
CONST C_TEECOLOURARRAY:Array[C_GOLDTEES..C_REDTEES] of String
      =('Gold','Black','Blue','White','Yellow','Red');

CONST C_GOLFMLURL='http://code.google.com/p/golfml/wiki/Branding'; // TODO

TYPE
 THCPClass = class(TObject)
 Private
 // PRIVATE VARS AND METHODS - Invisible outside this class
   // Utility property vars
   fDebugMode:Boolean; // Property 'DebugMode'
   fLogMode:Boolean; // Property 'LogMode'
   fLogFileName:String; // Property 'LogFileName'

   // Property vars for Adjust Handicap
   foldhandicap:single; // Property 'OldHandicap'
   fnewhandicap:single; // Property 'NewHandicap'
   fstablefordpoints:Cardinal; // Property 'StablefordPoints'
   fhandicapchanged:Boolean; // Property 'HandicapChanged'
   fReportText:String; // Property 'Report'

   // Property vars for Playing Handicap
   fPlayingHandicap:Single; // Property PlayingHandicap
   fEGAHandicap:Single; // Property 'EGAhandicap'

   // Property Vars for Exact EGA Handicap
   fBestScore:Cardinal; // Property 'BestScore'
   fScoreArray:Array[0..17] of Cardinal; // Property 'ScoreArray'

   // General vars
   fCurrentCategory:Cardinal; // Current Category
   fCurrentHandicap:Single; // Current Handicap
   fAction:Word; // 0,1 or 2 (C_ActionArray)
   fEventLog:TEventLog; // Event log object used if fDebugMode=True
   fHelpForm:TForm; // Form object for ShowHelp

   pVersion:String;

   // GETS and SETS
   Procedure priv_SetErrorString(aString:String); // Sets 'Errorstring' property
   Procedure priv_SetOldHandicap(aSingle:Single); // Sets 'OldHandicap' property
   Procedure priv_SetNewHandicap(aSingle:Single); // Sets 'NewHandicap' property
   Procedure priv_SetDebugMode(aBoolean:Boolean); // Sets 'DebugMode' property
   Procedure priv_SetLogMode(aBoolean:Boolean); // Sets 'LogMode' property
   Procedure priv_SetStablefordPoints(aCardinal:Cardinal); // Sets 'StablefordPoints' property
   Procedure priv_SetLogFileName(aString:String); // Sets 'LogFileName' property
   Procedure priv_SetCoursePar(aCardinal:Cardinal); // Sets property 'CoursePar'
   Procedure priv_SetCourseRating(aSingle:Single); // Sets property 'CourseRating'
   Procedure priv_SetSlopeRating(aCardinal:Cardinal); // Sets property 'SlopeRating'
   Procedure priv_SetBogeyRating(aSingle:Single); // Sets property 'BogeyRating'
   Procedure priv_SetEGAHandicap(aSingle:Single); // Sets property 'EGAHandicap'
   Procedure priv_SetBestScore(aCardinal:Cardinal); // Sets property 'BestScore'
   // Get/Sets for indexed public properties
   Procedure priv_SetParArrayElement(Const Aindex:Integer;AInteger:Integer);
   Function  priv_GetParArrayElement(Const Aindex:Integer):Integer;
   Procedure priv_SetScoreArrayElement(Const Aindex:Integer;AInteger:Integer);
   Function  priv_GetScoreArrayElement(Const Aindex:Integer):Integer;

   // GENERAL
   Procedure priv_ShowDebugInfo(Fmt:String;Args:Array of Const); // Only if DebugMode=True
   Function priv_GetCategory:Cardinal; // Returns category of fCurrentHandicap
   Function priv_IsInBuffer:Boolean; // Returns true if handicap is in buffer zone.  Sets fAction.
   Function priv_ReduceHandicap:Single; // Uses fstablefordpoints. Changes fCurrentHandicap
   Function priv_AdjustHandicap:Boolean; // Called by Public Method
   Procedure ShowMethods(Sender:TObject); // Used by ShowHelp method;
   Procedure ShowProperties(Sender:TObject); // Used by ShowHelp method;
   Procedure ShowInfo(Sender:TObject); // Used by ShowHelp method;
 Protected
 // PROTECTED VARS AND METHODS
 // These can be overwritten in a descendant var or method with the same name
   pErrorString:String; // Property 'ErrorString'
   pCourseRating:Single; // Property 'CourseRating'
   pBogeyRating:Single; // Property 'BogeyRating'
   pSlopeRating:Cardinal; // Property 'SlopeRating'
   pCoursePar:Cardinal; // Property 'CoursePar'
   pParArray:Array[0..17] of Cardinal; // Property 'ParArray'
   pBannerText,pMethodsHelpText,pPropertiesHelpText,pInfoHelpText:String; // ShowHelp
   Function prot_InitObj:boolean; virtual; // Init vars

   // Abstract function for all descendants to implement as they please
   // It doesn't exist in this class.
   Function GetCourseInfoFromFile:boolean; virtual; abstract;

 Public
 // PUBLIC VARS METHODS AND NON-VISUAL PROPERTIES
   constructor Create; // Note no override for Constructor
   destructor Destroy; override;
   Function InitOK:Boolean; // If myObj.InitOK then proceed...
   Procedure Reset; // Resets all the vars and properties.
   Function AdjustHandicap:Boolean; // The main method.  Set OldHandicap and Stablefordpoints property first.
   Function GetPlayingHandicap:Single; // Uses fEGAhandicap,pCourseRating,pSlopeRating,pCoursePar
   Function GetExactEGAHandicap:Single; // Uses fBestScore,pSlopeRating
   Function CalculateBestScore:Cardinal;// Uses pParArray,fScoreArray; Sets fBestScore
   Procedure ShowReport; // Displays fReportText in a MessageBox
   Procedure ShowHelp; // Constructs a Help Form on-the-fly and displays it

   // Indexed property types cannot be Published, so put them in the Public section
   // pParArray and fScoreArray would be populated or read via a for-loop
   Property ParArrayElement [Aindex:integer]:Integer read priv_GetParArrayElement write priv_SetParArrayElement;
   Property ScoreArrayElement [Aindex:integer]:Integer read priv_GetScoreArrayElement write priv_SetScoreArrayElement;

 Published
 // PUBLIC VISUAL PROPERTIES
 // These properties could be displayed in a visual component
 // Some read-only, some read + write
 // Default values are set in prot_InitObj
   Property ErrorString:String read pErrorString write priv_SetErrorString;
   // Most methods in this class set ErrorString automatically,
   // but the user can write to it and use it in their error handling
   Property DebugMode:boolean read fDebugMode write priv_SetDebugMode;
   // DebugMode=True triggers runtime messages for testing and also logging
   Property LogFileName:String read fLogFileName write priv_SetLogFileName;
   Property Logging:boolean read fLogMode write priv_SetLogMode;
   // DebugMode can be False whilst LogMode is True
   Property Version:String read pVersion;

   Property OldHandicap:Single read foldhandicap write priv_SetOldHandicap;
   Property NewHandicap:Single read fnewhandicap write priv_SetNewHandicap;
   Property StablefordPoints:Cardinal read fstablefordpoints write priv_SetStablefordPoints;
   Property HandicapChanged:Boolean read fhandicapchanged;
   Property Category:Cardinal read fCurrentCategory;
   Property Report:String read fReportText;

   Property CoursePar:Cardinal read pCoursePar write priv_SetCoursePar;
   Property CourseRating:Single read pCourseRating write priv_SetCourseRating;
   Property SlopeRating:Cardinal read pSlopeRating write priv_SetSlopeRating;
   Property BogeyRating:Single read pBogeyRating write priv_SetBogeyRating;
   Property PlayingHandicap:Single read fPlayingHandicap;
   Property EGAHandicap:Single read fEGAHandicap write priv_SetEGAHandicap;
   Property BestScore:Cardinal read fBestScore write priv_SetBestScore;

   // CALCULATING BOGEY RATING from Course Rating and Slope Rating
   // Bogey rating minus Course Rating (C) multiplied by (5.381 men, 4.24 women)(K)  equals Slope Rating (S).
   // (B-C) * K = S
   // B-C = S/K
   // B=(S/K) * C

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
  fEventLog:=TEventLog.Create(nil);
  {$IFDEF LINUX}
  fEventLog.Filename:='/var/log/' + C_LOGFILENAME;
  {$ELSE}
  fEventLog.Filename:=Application.Location + C_LOGFILENAME;
  {$ENDIF}
  fLogFileName:=fEventLog.Filename; // Set the property var
  fEventLog.LogType:=ltFile; // or ltSystem
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
 Function THCPClass.prot_InitObj:boolean;
 // PROTECTED
 // Do initialisation stuff here to reset the
 // vars/properties etc. to their default values/states
 Var
    cHoleCount:Cardinal;
    s:String;
 begin
    fOldHandicap:=0; // Scratch
    fNewHandicap:=0; // Scratch
    fDebugMode:=False; // User has to set it to true
    fLogMode:=False; // User has to set it to true
    fstablefordpoints:=36; // Results in unchanged handicap value
    fhandicapchanged:=False; // Property value
    fAction:=act_NOCHANGE; // No change to handicap needed
    fReportText:='Nothing to report';

    pCoursePar:=72;
    pCourseRating:=72; // Property 'CourseRating'
    pSlopeRating:=113; // Property 'SlopeRating'
    fPlayingHandicap:=0; // Property PlayingHandicap
    fEGAHandicap:=0; // Property 'EGAhandicap'
    fBestScore:=36;
    For cHoleCount:=0 to 17 do
        begin
             pParArray[cHoleCount]:=4; // All par-4's
             fScoreArray[cHoleCount]:=4; // Score of Par
        end;
    pVersion:=C_HCPCLASSVERSION;
    // The help text in ShowHelp can be overridden by a descendant
    s:=CR;
    s+='HCPClass: an EGA Handicap calculator by minesadorada' + CR;
    s+='Freeware version ' + pVersion;
    pBannerText:=s;

    s:='Public Methods' + CR + CR;
    s+='1: Function InitOK (True=success, False=Exception)'+CR;
    s+='2: Procedure Reset: Resets all properties to default'+CR;
    s+='3: Procedure ShowReport: MessageDlg of Report property'+CR;
    s+='(used after calling AdjustHandicap method)' + CR;
    s+='4: Procedure ShowHelp: Shows this help text in a window' + CR;
    s+='5: Function AdjustHandicap: (Click [More Info] button)' + CR;
    s+='6: Function GetPlayingHandicap: (Click [More Info] button)' + CR;
    s+='7: Function CalculateBestScore: (Click [More Info] button)' +CR;
    s+='8: Function GetExactEGAHandicap: (Click [More Info] button)';
    pMethodsHelpText:=s;

    s:='1: DebugMode:boolean 2: Logging:boolean both read/write' + CR;
     s+='3: LogFileName:String read/write' + CR;
     s+='4: OldHandicap, 5: NewHandicap:Single both read/write' + CR;
     s+='6: StablefordPoints:Cardinal read/write' + CR;
     s+='7: HandicapChanged:Boolean readonly' + CR;
     s+='8: Category:Cardinal readonly' + CR;
     s+='9: Report:String readonly' + CR;
     s+='10:ErrorString :String read/write' + CR;
     s+='11:ParArrayElement[index]:Cardinal read/write [0..17]' + CR;
     s+='12:ScoreArrayElement[index]:Cardinal read/write [0..17]' + CR;
     s+='13:CoursePar:Cardinal 14:SlopeRating:Cardinal read/write' + CR;
     s+='15:EGAHandicap:Single read/write' + CR;
     s+='16:BestScore:Cardinal read/write' + CR;
     s+='17:Version:String readonly';
    pPropertiesHelpText:=s;

    s:='There are 4 main functions - all following EGA rules.'+CR;
    s+='(Call Reset method to (re)initialise the properties)' + CR + CR;
    s+='1 ) AdjustHandicap (sets NewHandicap property)'+CR;
    s+='Set OldHandicap and StablefordPoints property first'+CR;
    s+='2 ) GetPlayingHandicap (sets PlayingHandicap property)'+CR;
    s+='Set EGAhandicap,CourseRating,SlopeRating,CoursePar first'+CR;
    s+='3 ) CalculateBestScore (sets BestScore property)' + CR;
    s+='Set ParArray,ScoreArray to retrieve BestScore (Hcp36 score)'+CR;
    s+='4 ) GetExactEGAHandicap (sets EGAHandicap property)'+CR;
    s+='Set SlopeRating and BestScore to retrieve EGAHandicap';
    pInfoHelpText:=s;

    Result:=True;
  end;
 // *********************************************************************
 Function THCPClass.priv_AdjustHandicap:Boolean;
 // PROTECTED
 // Return value goes to public method
 // Sets fCurrentHandicap and fNewHandicap
 begin
      ErrorString:='THCPClass.priv_AdjustHandicap';
      Result:=False;
      fCurrentHandicap:=fOldHandicap; // Fetch from property
      fCurrentCategory:=priv_GetCategory;
      fhandicapchanged:=False; // Default is not changed
      fReportText:='Summary:' + CR + CR; // Assign
      fReportText+=Format('Handicap %.1f (Cat. %d)%s',[fOldHandicap,fCurrentCategory,CR]);
      fReportText+=Format('Stableford points: %d%s',[fStablefordPoints,CR]);
      If priv_IsInBuffer then
         fReportText+=Format('Within buffer %s%s%s',[C_BufferStringArray[fCurrentCategory],CR,CR])
      else
          fReportText+=Format('Outside buffer %s%s%s',[C_BufferStringArray[fCurrentCategory],CR,CR]);

      //Report:
      fReportText+=Format('%s%s',[C_ActionArray[fAction],CR]);
      priv_ShowDebugInfo
      ('THCPClass.priv_AdjustHandicap(before) : OldHandicap=%f, NewHandicap=%f, Stableford=%d Category=%d, fAction=%s',
      [fOldHandicap,fNewHandicap,fstablefordpoints,fCurrentCategory,C_ActionArray[fAction]]);
      Case fAction of
           act_NOCHANGE:fCurrentHandicap:=fOldHandicap;
           act_NEEDSRAISING:
                  begin
                    Case fCurrentCategory of
                         1: fCurrentHandicap:=fCurrentHandicap + C_CAT1INCREASE;
                         2: fCurrentHandicap:=fCurrentHandicap + C_CAT2INCREASE;
                         3: fCurrentHandicap:=fCurrentHandicap + C_CAT3INCREASE;
                         4: fCurrentHandicap:=fCurrentHandicap + C_CAT4INCREASE;
                         5: fCurrentHandicap:=fCurrentHandicap + C_CAT5INCREASE;
                    end;
                    fhandicapchanged:=TRUE;
                    fReportText+=Format('1: %.1f -> %.1f (+%s)%s',
                    [foldhandicap,fCurrentHandicap,C_IncreaseStringArray[fCurrentCategory],CR]);
                  end;
            act_NEEDSREDUCING:
                   begin
                     fCurrentHandicap:=priv_ReduceHandicap;
                     fhandicapchanged:=TRUE;
                   end;
      end;
      fNewHandicap:=fCurrentHandicap;

      if fhandicapchanged then
         If fNewHandicap > fOldHandicap then
            fReportText+=Format('%sNew Handicap: %.1f (+%.1f) Cat.%d',
            [CR,fNewHandicap,(fNewHandicap - fOldHandicap),fCurrentCategory])
         else
             fReportText+=Format('%sNew Handicap: %.1f (-%.1f) Cat.%d',
             [CR,fNewHandicap,(fOldHandicap - fNewHandicap),fCurrentCategory]);

      priv_ShowDebugInfo
      ('THCPClass.priv_AdjustHandicap(after) : OldHandicap=%f, NewHandicap=%f, Stableford=%d Category=%d, fAction=%s',
      [fOldHandicap,fNewHandicap,fstablefordpoints,fCurrentCategory,C_ActionArray[fAction]]);
      Result:=TRUE;
 end;
 // *********************************************************************
 // PRIVATE METHODS
 // *********************************************************************
 Procedure THCPClass.priv_ShowDebugInfo(Fmt:String;Args:Array of Const);
 // DebugMode and/or Logging must be set for this to do anything at all
 begin
      If fDebugMode then
         MessageDlg(Format(Fmt,Args),mtInformation,[MBOK],0);
      If fLogMode then
         fEventLog.Debug(Fmt,Args);
 end;
 // *********************************************************************
 Function THCPClass.priv_IsInBuffer:Boolean;
 // Uses fStablefordpoints and fCategory
 // Sets fAction
 // If Handicap is in appropriate buffer zone, return TRUE
 // and fAction:=act_NOCHANGE

 // If Handicap is ouside buffer zone, return FALSE
 // and fAction:=act_NEEDSRAISING or fAction:=act_NEEDSREDUCING
 begin
     Result:=False; // Default unless changed by being in buffer zone
     fAction:=act_NOCHANGE; // Default unless changed by being outside buffer zone
     Case fCurrentCategory of
     5:
            If (fStablefordpoints <= C_CAT5BUFFERUPPER)
            AND (fStablefordpoints >= C_CAT5BUFFERLOWER)
            then
               begin
                    Result:=TRUE; // In buffer zone
                    Exit; // Bail out
               end
               else
               If (fStablefordpoints < C_CAT5BUFFERLOWER) then
               begin
                    fAction:=act_NEEDSRAISING;
                    Exit; // Bail out
               end
               else
               If (fStablefordpoints > C_CAT5BUFFERUPPER) then
               begin
                    fAction:=act_NEEDSREDUCING;
                    Exit; // Bail out
               end;
     4:
            If (fStablefordpoints <= C_CAT4BUFFERUPPER)
            AND (fStablefordpoints >= C_CAT4BUFFERLOWER)
            then
               begin
                    Result:=TRUE; // In buffer zone
                    Exit; // Bail out
               end
            else
               If (fStablefordpoints < C_CAT4BUFFERLOWER) then
               begin
                    fAction:=act_NEEDSRAISING;
                    Exit; // Bail out
               end
            else
               If (fStablefordpoints > C_CAT4BUFFERUPPER) then
               begin
                    fAction:=act_NEEDSREDUCING;
                    Exit; // Bail out
            end;
     3:
            If (fStablefordpoints <= C_CAT3BUFFERUPPER)
            AND (fStablefordpoints >= C_CAT3BUFFERLOWER)
            then
               begin
                    Result:=TRUE; // In buffer zone
                    Exit; // Bail out
               end
            else
               If (fStablefordpoints < C_CAT3BUFFERLOWER) then
               begin
                    fAction:=act_NEEDSRAISING;
                    Exit; // Bail out
               end
            else
               If (fStablefordpoints > C_CAT3BUFFERUPPER) then
               begin
                    fAction:=act_NEEDSREDUCING;
                    Exit; // Bail out
            end;
     2:
            If (fStablefordpoints <= C_CAT2BUFFERUPPER)
            AND (fStablefordpoints >= C_CAT2BUFFERLOWER)
            then
               begin
                    Result:=TRUE; // In buffer zone
                    Exit; // Bail out
               end
            else
                If (fStablefordpoints < C_CAT2BUFFERLOWER) then
               begin
                    fAction:=act_NEEDSRAISING;
                    Exit; // Bail out
               end
            else
                If (fStablefordpoints > C_CAT2BUFFERUPPER) then
               begin
                    fAction:=act_NEEDSREDUCING;
                    Exit; // Bail out
            end;
     1:
            If (fStablefordpoints <= C_CAT1BUFFERUPPER)
            AND (fStablefordpoints >= C_CAT1BUFFERLOWER)
            then
                begin
                     Result:=TRUE; // In buffer zone
                     Exit; // Bail out
               end
            else
                If (fStablefordpoints < C_CAT1BUFFERLOWER) then
               begin
                    fAction:=act_NEEDSRAISING;
                    Exit; // Bail out
               end
            else
                If (fStablefordpoints > C_CAT1BUFFERUPPER) then
               begin
                    fAction:=act_NEEDSREDUCING;
                    Exit; // Bail out
            end;
     end;
 end;
 // *********************************************************************
 Function THCPClass.priv_ReduceHandicap:Single;
 // Called by AdjustHandicap.  Uses fstablefordpoints and fCurrentHandicap
 // Returns reduced handicap
 Var
   cCount:Cardinal;
   fTemp:Single;
 begin
      fCurrentCategory:=priv_GetCategory; // Confirm category?
      priv_ShowDebugInfo
      ('THCPClass.priv_ReduceHandicap(before) : OldHandicap=%f, NewHandicap=%f, Stableford=%d Category=%d, fAction=%s',
      [fOldHandicap,fNewHandicap,fstablefordpoints,fCurrentCategory,C_ActionArray[fAction]]);
      For cCount:=fstablefordpoints downto 37 do
      begin
           fTemp:=fCurrentHandicap;
          Case fCurrentCategory of
               1:fCurrentHandicap:=fCurrentHandicap - C_CAT1DECREASE;
               2:fCurrentHandicap:=fCurrentHandicap - C_CAT2DECREASE;
               3:fCurrentHandicap:=fCurrentHandicap - C_CAT3DECREASE;
               4:fCurrentHandicap:=fCurrentHandicap - C_CAT4DECREASE;
               5:fCurrentHandicap:=fCurrentHandicap - C_CAT5DECREASE;
          end;
          fReportText+=Format('%d: Cat %d, %.1f -> %.1f (-%s)%s',
          [(fstablefordpoints-cCount+1),fCurrentCategory,fTemp,fCurrentHandicap,C_DecreaseStringArray[fCurrentCategory],CR]);
          fCurrentCategory:=priv_GetCategory; // New category?
      end;
      priv_ShowDebugInfo
      ('THCPClass.priv_ReduceHandicap(after) : OldHandicap=%f, NewHandicap=%f, Stableford=%d Category=%d, fAction=%s',
      [fOldHandicap,fNewHandicap,fstablefordpoints,fCurrentCategory,C_ActionArray[fAction]]);

      Result:=fCurrentHandicap;
 end;
 // *********************************************************************

 Function THCPClass.priv_GetCategory:Cardinal;
 // Returns category according to fCurrentHandicap
 begin
      Result:=1;//Cat 1
      ErrorString:='THCPClass.priv_GetCategory';
      If fCurrentHandicap > C_CAT2MINIMUM then Inc(Result); // Cat2
      If fCurrentHandicap > C_CAT3MINIMUM then Inc(Result); // Cat3
      If fCurrentHandicap > C_CAT4MINIMUM then Inc(Result); // Cat4
      If fCurrentHandicap > C_CAT5MINIMUM then Inc(Result); // Cat5
      priv_ShowDebugInfo
      ('priv_GetCategory : Old Handicap=%f, Current Handicap=%f, Stableford=%d, Old Category=%d, New Category=%d',
      [fOldHandicap,fCurrentHandicap,fstablefordpoints,fCurrentCategory,Result]);
      Exit; // Bail out.  No change to handicap;
end;

 // *********************************************************************
 // PUBLIC METHODS
 // *********************************************************************
 // *********************************************************************
 Procedure THCPClass.ShowInfo(Sender:TObject);
 // Used by ShowHelp onClick event
 Var
    aButton:TBitBtn;
    s:String;
 begin
    aButton:=Sender as TBitBtn;
    s:=pInfoHelpText;
    aButton.Parent.Controls[1].Caption:=s;
 end;
 // *********************************************************************
 Procedure THCPClass.ShowMethods(Sender:TObject);
 // Used by ShowHelp onClick event
 Var
    aButton:TBitBtn;
    s:String;
 begin
    aButton:=Sender as TBitBtn;
    s:=pMethodsHelpText;
    aButton.Parent.Controls[1].Caption:=s;
 end;
 // *********************************************************************
 Procedure THCPClass.ShowProperties(Sender:TObject);
 // Used by ShowHelp onClick event
 Var
    aButton:TBitBtn;
    s:String;
 begin
    aButton:=Sender as TBitBtn;
    s:=pPropertiesHelpText;
    aButton.Parent.Controls[1].Caption:=s;
 end;
Procedure THCPClass.ShowHelp;
// Makes up a help form with controls on-the-fly
// This means the class code is standalone with no form unit code to include
Var
  infoPanel,mainpanel:TLabel;
  CloseButton:TBitBtn;
  cmd_ShowMethods,cmd_ShowProperties,cmd_ShowInfo:TBitBtn;
  s:String;
begin
     // Default text
     s:=pBannerText;
     // Create the form and controls in a TRY-FINALLY block
     TRY
        fHelpForm:=TForm.Create(nil);
        infoPanel:=TLabel.Create(nil);
        mainPanel:=TLabel.Create(nil);
        CloseButton:=TBitBtn.Create(nil);
        cmd_ShowMethods:=TBitBtn.Create(nil);
        cmd_ShowProperties:=TBitBtn.Create(nil);
        cmd_ShowInfo:=TBitBtn.Create(nil);
        With fHelpForm do // Main Form
        begin
            Width:=340;
            Height:=480;
            Position:=poScreenCenter;
            BorderStyle:=bsSingle;
            Caption:='Help for THCPClass';
            Formstyle:=fsStayOnTop;
        end;
        With infopanel do // Header text
        begin
            Height:=70;
            Align:=alTop;
            Alignment:=taCenter;
            WordWrap:=True;
            Caption:=s;
            Parent:=fHelpForm;
        end;
        With mainpanel do // Main text.  Control[1]
        begin
            Top:=infopanel.Height;
            Height:=290;
            left:=10;
            width:=fHelpForm.Width-40;
            Alignment:=taLeftJustify;
            WordWrap:=True;
            Caption:='Click a button below for more information';
            Parent:=fHelpForm;
        end;
        With cmd_ShowMethods do // Button
        begin
            Top:=fHelpForm.Height-40; // Change this to change all buttons .Top
            Left:=20;; // Change this to change all buttons .Left
            kind:=bkCustom;
            Caption:='Methods';
            OnClick:=@ShowMethods;
            Parent:=fHelpForm;
        end;
        With cmd_ShowProperties do // Button
        begin
            Top:=cmd_ShowMethods.Top;
            Left:=cmd_ShowMethods.Left+cmd_ShowMethods.Width;
            kind:=bkCustom;
            Caption:='Properties';
            OnClick:=@ShowProperties;
            Parent:=fHelpForm;
        end;
        With cmd_ShowInfo do // Button
        begin
            Top:=cmd_ShowMethods.Top;
            Left:=cmd_ShowProperties.Left+cmd_ShowProperties.Width;
            kind:=bkCustom;
            Caption:='More Info';
            OnClick:=@ShowInfo;
            Parent:=fHelpForm;
        end;
        With CloseButton do // Button
        begin
            Top:=cmd_ShowMethods.Top;
            Left:=cmd_ShowInfo.Left+cmd_ShowInfo.Width ;
            kind:=bkClose; // No event to write!
            Parent:=fHelpForm;
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
 Procedure THCPClass.ShowReport;
 // Displays the text of the Report property in a dialog
 begin
      MessageDlg(fReportText,mtInformation,[MBOK],0);
 end;
 // *********************************************************************
Function THCPClass.CalculateBestScore:Cardinal;
// Uses pParArray,fScoreArray
// Sets fBestScore
Var
   iCount,iTemp,TotalPoints:Cardinal;
   TeeArray:Array[1..18] of Cardinal;
begin
     Result:=0;
     for iCount:=1 to 18 do
         TeeArray[iCount]:=pParArray[iCount-1]+2;
     TotalPoints:=0;
     for iCount:=1 to 18 do
         begin
            iTemp:=fScoreArray[iCount-1];
            if iTemp=0 then continue;
            if iTemp=Teearray[iCount]+1 then Inc(TotalPoints,1);
            if iTemp=Teearray[iCount] then Inc(TotalPoints,2);
            if iTemp+1=Teearray[iCount] then Inc(TotalPoints,3);
            if iTemp+2=Teearray[iCount] then Inc(TotalPoints,4);
            if iTemp+3=Teearray[iCount] then Inc(TotalPoints,5);
            if iTemp+4=Teearray[iCount] then Inc(TotalPoints,6);
         end;
     Result:=TotalPoints;
     fBestScore:=TotalPoints;
     priv_ShowDebugInfo
     ('THCPClass.CalculateBestScore : fBestScore=%d',
     [fBestScore]);

end;
 // *********************************************************************
 Function THCPClass.GetExactEGAHandicap:Single;
 // Uses fBestScore,pSlopeRating
 // Called directly, gives unrounded result
 // Sets property EGAHandicap to rounded result
 begin
      Result:=36 - ((fbestscore-36) * (113/pSlopeRating));
      fEGAHandicap:=Result;
      fEGAHandicap:=(Int((fEGAHandicap * 100) + 0.5)) / 100; // Round up to 2 Dec Places
      priv_ShowDebugInfo
      ('THCPClass.GetExactEGAHandicap : fbestscore=%d, pSlopeRating=%d, fEGAHandicap=%f, fEGAHandicap(exact)=%f',
      [fbestscore,pSlopeRating,fEGAHandicap,Result]);
 end;
 // *********************************************************************
 Function THCPClass.GetPlayingHandicap:Single;
 // Uses fEGAHandicap,pCourseRating,pSlopeRating,pCoursePar
 // Called directly, gives unrounded result
 // Sets property PlayingHandicap to rounded result
 begin
      Result:=(fEGAhandicap * (pSlopeRating/113)) + (pCourseRating - pCoursePar);
      fPlayingHandicap:=Result;  // INT(0.5 +Result); // or round UPWARDS to nearest Cardinal?
      fPlayingHandicap:=(Int((fPlayingHandicap * 100) + 0.5)) / 100; // Round up to 2 Dec Places - better
      priv_ShowDebugInfo
      ('THCPClass.GetPlayingHandicap : fEGAhandicap=%f, pSlopeRating=%d, pCourseRating=%f, pCoursePar=%d, Playing Handicap=%f',
      [fEGAhandicap,pSlopeRating,pCourseRating,pCoursePar,fPlayingHandicap]);
 end;

 Function THCPClass.AdjustHandicap:Boolean;
 // The main Public Method.  Calls private method to do the deed.
 begin
      Result:=False;
      ErrorString:='THCPClass.AdjustHandicap';
      Result:=priv_AdjustHandicap; // Call the protected method
      priv_ShowDebugInfo
      ('THCPClass.AdjustHandicap : OldHandicap=%f, NewHandicap=%f, Stableford=%d Category=%d',
      [fOldHandicap,fNewHandicap,fstablefordpoints,fCurrentCategory]);
end;
 Procedure THCPClass.Reset;
 // Resets all the properties to default values via protected method.
 begin
     prot_InitObj;
 end;

 Function THCPClass.InitOK:Boolean;
 // PUBLIC
 // Call this after Create to check everything initialised OK
 // If Result=TRUE then proceed.
 // If Result=FALSE then halt the application.  It's a fatal error.
  begin
       Result:=False;
       ErrorString:='THCPClass.InitOK failed.'  + CR;
       ErrorString:=ErrorString + 'This application is now unstable,' + CR;
       ErrorString:=ErrorString + 'and you should terminate it now.';
       // Call InitOK after Create method and test for true/false.
       TRY
          Result:=prot_InitObj;
          // Use the protected method.  We might want to add to it later.
       EXCEPT
          raise Exception.Create(ErrorString);
       END;
  end;

 // *********************************************************************
 // GET/SETS (ALL PRIVATE METHODS)
 Procedure THCPClass.priv_SetErrorString(aString:String);
 begin if pErrorString <> aString then pErrorString:=aString; end;
 // *********************************************************************
 Procedure THCPClass.priv_SetDebugMode(aBoolean:Boolean);
 // Sets 'DebugMode' property
 begin
      fDebugMode:=aBoolean;
      If fDebugMode then priv_SetLogMode(True);
      // Turning DebugMode on automatically turns logging mode on
      fEventLog.Info('THCPClass Debug mode set')
 end;
 // *********************************************************************
 Procedure THCPClass.priv_SetLogMode(aBoolean:Boolean);
 // Sets 'LogMode' property
 begin
      fLogMode:=aBoolean;
      fEventLog.Active:=fLogMode;
      fEventLog.Info('THCPClass Log mode set');
 end;
 // *********************************************************************
 Procedure THCPClass.priv_SetOldHandicap(aSingle:Single);
 // Sets 'OldHandicap' property. No checking!
 begin if fOldHandicap <> aSingle then fOldHandicap:=aSingle;end;
 // *********************************************************************
 Procedure THCPClass.priv_SetNewHandicap(aSingle:Single);
 // Sets 'NewHandicap' property. No checking!
 begin if fNewHandicap <> aSingle then fNewHandicap:=aSingle;end;
 // *********************************************************************
 Procedure THCPClass.priv_SetStablefordPoints(aCardinal:Cardinal);
 // Sets 'StablefordPoints' property. No checking!
 begin if fstablefordpoints <> aCardinal then fstablefordpoints:=aCardinal;end;
 // *********************************************************************
 Procedure THCPClass.priv_SetLogFileName(aString:String);
 // Sets 'LogFileName' property.
 // Make sure eventlog isn't active if the filename is changed!
 Var IsActive:Boolean;
 begin
      if flogfilename <> aString then
         begin
          IsActive:=fEventLog.Active;
          If IsActive then fEventLog.Active:=False;
          flogfilename:=aString;
          fEventLog.Filename:=flogfilename;
          If IsActive then fEventLog.Active:=True;
         end;
 end;
 // *********************************************************************
 Procedure THCPClass.priv_SetCourseRating(aSingle:Single);
 // Sets property 'CourseRating' No checking!
 begin if pCourseRating <> aSingle then pCourseRating:=aSingle;end;
 // *********************************************************************
 Procedure THCPClass.priv_SetBogeyRating(aSingle:Single);
 // Sets property 'BogeyRating' No Checking!
 begin if pBogeyRating <> aSingle then pBogeyRating:=aSingle;end;
 // *********************************************************************
 Procedure THCPClass.priv_SetSlopeRating(aCardinal:Cardinal);
 // Sets property 'SlopeRating' No checking!
 begin if pSlopeRating <> aCardinal then pSlopeRating:=aCardinal;end;
 // *********************************************************************
 Procedure THCPClass.priv_SetEGAHandicap(aSingle:Single);
 // Sets property 'EGAHandicap' No checking!
 begin if fEGAHandicap <> aSingle then fEGAHandicap:=aSingle;end;
 // *********************************************************************
 Procedure THCPClass.priv_SetCoursePar(aCardinal:Cardinal);
 // Sets property 'CoursePar' No checking!
 begin if pCoursePar <> aCardinal then pCoursePar:=aCardinal;end;
 // *********************************************************************
 Procedure THCPClass.priv_SetBestScore(aCardinal:Cardinal);
 // Sets property 'BestScore' No checking!
 begin if fBestScore <> aCardinal then fBestScore:=aCardinal;end;
 // *********************************************************************
 Function THCPClass.priv_GetParArrayElement(Const Aindex:Integer):Integer;
 // Gets one element of pParArray[0..17].  Check the Index is in bounds
 begin
     If (AIndex >= Low(pParArray)) AND (AIndex <= High(pParArray)) then
        Result:=pParArray[AIndex];
 end;
 // *********************************************************************
 Procedure THCPClass.priv_SetParArrayElement(Const Aindex:Integer;AInteger:Integer);
 // Sets one element of pParArray[0..17].  Check the Index is in bounds
 begin
     If (AIndex >= Low(pParArray)) AND (AIndex <= High(pParArray)) then
        pParArray[AIndex]:=AInteger;
 end;
 // *********************************************************************
 Function THCPClass.priv_GetScoreArrayElement(Const Aindex:Integer):Integer;
 // Gets one element of fScoreArray[0..17].  Check the Index is in bounds
 begin
     If (AIndex >= Low(fScoreArray)) AND (AIndex <= High(fScoreArray)) then
        Result:=fScoreArray[AIndex];
 end;
 // *********************************************************************
 Procedure THCPClass.priv_SetScoreArrayElement(Const Aindex:Integer;AInteger:Integer);
 // Sets one element of fScoreArray[0..17].  Check the Index is in bounds
 begin
     If (AIndex >= Low(fScoreArray)) AND (AIndex <= High(fScoreArray)) then
        fScoreArray[AIndex]:=AInteger;
 end;
 // *********************************************************************

 end.

