unit golfumlclass;

(*
== Author: minesadorada
==
== Lazarus: 0.9.30-0
== FPC: 2.4.4
==
== Version History
== 1.0
   [20120322]
   General code for class.
==
*)
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, eventlog, XMLRead, DOM, // All that's needed without message dialogs
  Forms, Controls, Graphics, Dialogs, Buttons, ComCtrls; // Needed with message dialogs

const
  CR = LineEnding; // CR is easier to type!

const
  C_LOGFILENAME = 'GolfUMLClass.log'; // In case eventlog unit is used

const
  C_VERSION = '1.0';

type
  TGolfUMLClass = class(TObject)
  private
// PRIVATE VARS AND METHODS - Unavailable outside this class
    fErrorString: string; // Property 'ErrorString'
    fDebugMode: boolean; // Property 'DebugMode'
    fLogMode: boolean; // Property 'LogMode'
    fLogFileName: string; // Set in Create
    fEventLog: TEventLog; // Event log object used if fLogMode=True
    // *********************************************************************
    // Add more from here
    fCourses: TStrings;
    fParArray: array[0..17] of cardinal;
    fStrokeIndexArray: array[0..17] of cardinal;
    fCourseXMLPath: string;

    // GETS and SETS
    procedure priv_SetErrorString(aString: string); // Sets 'Errorstring' property
    procedure priv_SetDebugMode(aBoolean: boolean); // Sets 'DebugMode' property
    procedure priv_SetLogMode(aBoolean: boolean); // Sets 'LogMode' property
    procedure priv_SetLogFileName(aString: string); // Sets 'LogFileName' property
    // GENERAL
    procedure priv_ShowDebugInfo(Fmt: string; Args: array of const);
    // Only does anything if DebugMode=True or LogMode=True
    // *********************************************************************
    // Add more from here
    function priv_GetCoursesIntoStringList: TStrings;
    function priv_GetHoleParElement(AIndex: integer): cardinal;
    function priv_GetHoleStrokeIndexElement(AIndex: integer): cardinal;
    procedure priv_SetHoleParElement(AIndex: integer; aCardinal: cardinal);
    procedure priv_SetHoleStrokeIndexElement(AIndex: integer; aCardinal: cardinal);
    procedure priv_SetCourseXMLPath(aString: string);
  protected
// PROTECTED METHODS
    // These can be overridden in a descendant (subclass)
    function prot_InitObj: boolean; virtual;
    // *********************************************************************
    // Add more from here

  public
// PUBLIC METHODS
    // NOTE NO PARAMETERS - PUBLIC METHODS USE PUBLIC PROPERTIES INSTEAD
    constructor Create; // Note no override for Constructor
    destructor Destroy; override;
    function InitOK: boolean; // If myObj.InitOK then proceed...
    // *********************************************************************
    // Add more from here
    property HoleParElement[aIndex: integer]: cardinal
      read priv_GetHoleParElement write priv_SetHoleParElement;
    property HoleStrokeIndex[aIndex: integer]: cardinal
      read priv_GetHoleStrokeIndexElement write priv_SetHoleStrokeIndexElement;
    property CourseList: TStrings read priv_GetCoursesIntoStringList;
  published
// PUBLIC PROPERTIES  - available via GolfUMLClass.Property:=SomeVar
    //                      or SomeVar:=GolfUMLClass.Property
    // Some read-only, some read + write
    property ErrorString: string read fErrorString write priv_SetErrorString;
    // Most methods in this class set ErrorString automatically,
    // but the user can write to it and use it in their error handling
    property DebugMode: boolean read fDebugMode write priv_SetDebugMode;
    // DebugMode=True triggers runtime messages for testing and also logging
    property LogFileName: string read fLogFileName write priv_SetLogFileName;
    property Logging: boolean read fLogMode write priv_SetLogMode;
    // DebugMode can be False whilst Logging is True but not vice versa
    // *********************************************************************
    // Add more from here
    property CourseXMLPath: string read fCourseXMLPath write priv_SetCourseXMLPath;
  end;



implementation
// *********************************************************************
constructor TGolfUMLClass.Create;
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
  fEventLog.Filename := C_LOGFILENAME;
  {$ENDIF}
  fLogFileName := fEventLog.Filename; // Set the default property
  fEventLog.LogType := ltFile; // or ltSystem
end;
// *********************************************************************
destructor TGolfUMLClass.Destroy;
begin
  // First do any tidy-up code before calling the inherited Destroy
  fEventLog.Free;
  inherited Destroy;
end;
// *********************************************************************
// PROTECTED METHODS
// *********************************************************************
function TGolfUMLClass.prot_InitObj: boolean;
  // PROTECTED
  // Do initialisation stuff here to reset the
  // vars/properties etc. to their default values/states
begin
  ErrorString := 'TGolfUMLClass.prot_InitObj';
  fDebugMode := False;
  fLogMode := False;
  Result := True;
end;
// *********************************************************************
// PRIVATE METHODS
// *********************************************************************

procedure TGolfUMLClass.priv_ShowDebugInfo(Fmt: string; Args: array of const);
begin
  if fDebugMode then
    MessageDlg(Format(Fmt, Args), mtInformation, [mbOK], 0);
  if fLogMode then
    fEventLog.Debug(Fmt, Args);
end;

// *********************************************************************
// PUBLIC METHODS
// *********************************************************************

function TGolfUMLClass.InitOK: boolean;
  // PUBLIC
begin
  Result := False;
  ErrorString := 'TGolfUMLClass.InitOK failed.' + CR;
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
// *********************************************************************
function TGolfUMLClass.priv_GetCoursesIntoStringList: TStrings;
begin

end;
// *********************************************************************
function TGolfUMLClass.priv_GetHoleParElement(AIndex: integer): cardinal;
begin

end;
// *********************************************************************
function TGolfUMLClass.priv_GetHoleStrokeIndexElement(AIndex: integer): cardinal;
begin

end;
// *********************************************************************
procedure TGolfUMLClass.priv_SetHoleParElement(AIndex: integer; aCardinal: cardinal);
begin

end;
// *********************************************************************
procedure TGolfUMLClass.priv_SetHoleStrokeIndexElement(AIndex: integer;
  aCardinal: cardinal);
begin

end;
// *********************************************************************
procedure TGolfUMLClass.priv_SetCourseXMLPath(aString: string);
// *********************************************************************
begin
  if fCourseXMLPath <> aString then
    fCourseXMLPath := aString;
end;

procedure TGolfUMLClass.priv_SetErrorString(aString: string);
// *********************************************************************
begin
  if fErrorString <> aString then
    fErrorString := aString;
end;
// *********************************************************************
procedure TGolfUMLClass.priv_SetLogFileName(aString: string);
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

procedure TGolfUMLClass.priv_SetDebugMode(aBoolean: boolean);
// Sets 'DebugMode' property
begin
  ErrorString := 'TGolfUMLClass.priv_SetDebugMode';
  fDebugMode := aBoolean;
  if fDebugMode then
    priv_SetLogMode(True);
  // Turning DebugMode on automatically turns logging mode on
  fEventLog.Info('TGolfUMLClass Debug mode set');
end;
// *********************************************************************
procedure TGolfUMLClass.priv_SetLogMode(aBoolean: boolean);
// Sets 'LogMode' property
begin
  ErrorString := 'TGolfUMLClass.priv_SetLogMode';
  fLogMode := aBoolean;
  fEventLog.Active := fLogMode;
  fEventLog.Info('TGolfUMLClass Log mode set');
end;

end.

