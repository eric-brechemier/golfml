unit classunit;
(*
== Author: minesadorada
==
== Lazarus: 0.9.30-0
== FPC: 2.4.4
==
== Version History
== 1.0
   [20120319]
   General code for class.
   Debug and Logging added
==
== Purpose:
== Skeleton code for a new Class
==
== How to use:
== 1 ) Declare the variable in the interface section
   MyClass:TMyClass;
== 2 ) Create the instance in the implementation section (form create)
   MyClass:=TMyClass.Create;
== 3 ) Set the properties and call the public methods
   If NOT MyClass.InitOK then HALT; // Check the class is (re)initialised
   (Optionally Set MyClass.DebugMode and/or MyClass.Logging)
*)
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,eventlog, // All that's needed without message dialogs
  Forms, Controls, Graphics, Dialogs, Buttons,ComCtrls; // Needed with message dialogs

CONST CR=LineEnding; // CR is easier to type!
CONST C_LOGFILENAME='MyClass.log'; // In case eventlog unit is used

TYPE
 TMyClass = class(TObject)
 Private
 // PRIVATE VARS AND METHODS - Unavailable outside this class
   fErrorString:String; // Property 'ErrorString'
   fDebugMode:Boolean; // Property 'DebugMode'
   fLogMode:Boolean; // Property 'LogMode'
   fLogFileName:String; // Set in Create
   fEventLog:TEventLog; // Event log object used if fLogMode=True
   // *********************************************************************
   // Add more from here

   // GETS and SETS
   Procedure priv_SetErrorString(aString:String); // Sets 'Errorstring' property
   Procedure priv_SetDebugMode(aBoolean:Boolean); // Sets 'DebugMode' property
   Procedure priv_SetLogMode(aBoolean:Boolean); // Sets 'LogMode' property
   Procedure priv_SetLogFileName(aString:String); // Sets 'LogFileName' property
   // GENERAL
   Procedure priv_ShowDebugInfo(Fmt:String;Args:Array of Const);
   // Only does anything if DebugMode=True or LogMode=True
   // *********************************************************************
   // Add more from here

 Protected
 // PROTECTED METHODS
 // These can be overridden in a descendant (subclass)
   Function prot_InitObj:boolean; virtual;
   // *********************************************************************
   // Add more from here

 Public
 // PUBLIC METHODS
 // NOTE NO PARAMETERS - PUBLIC METHODS USE PUBLIC PROPERTIES INSTEAD
   constructor Create; // Note no override for Constructor
   destructor Destroy; override;
   Function InitOK:Boolean; // If myObj.InitOK then proceed...
   // *********************************************************************
   // Add more from here

 Published
 // PUBLIC PROPERTIES  - available via myClass.Property:=SomeVar
 //                      or SomeVar:=myClass.Property
 // Some read-only, some read + write
   Property ErrorString:String read fErrorString write priv_SetErrorString;
   // Most methods in this class set ErrorString automatically,
   // but the user can write to it and use it in their error handling
   Property DebugMode:boolean read fDebugMode write priv_SetDebugMode;
   // DebugMode=True triggers runtime messages for testing and also logging
   Property LogFileName:String read fLogFileName write priv_SetLogFileName;;
   Property Logging:boolean read fLogMode write priv_SetLogMode;
   // DebugMode can be False whilst Logging is True but not vice versa
   // *********************************************************************
   // Add more from here

 end;



 implementation
 // *********************************************************************
 constructor TMyClass.Create;
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
  fEventLog.Filename:=C_LOGFILENAME;
  {$ENDIF}
  fLogFileName:=fEventLog.Filename; // Set the default property
  fEventLog.LogType:=ltFile; // or ltSystem
 end;
 // *********************************************************************
 destructor TMyClass.Destroy;
 begin
  // First do any tidy-up code before calling the inherited Destroy
  fEventLog.Free;
  inherited Destroy;
 end;
 // *********************************************************************
 // PROTECTED METHODS
 // *********************************************************************
 Function TMyClass.prot_InitObj:boolean;
 // PROTECTED
 // Do initialisation stuff here to reset the
 // vars/properties etc. to their default values/states
 begin
      ErrorString:='TMyClass.prot_InitObj';
      fDebugMode:=False;
      fLogMode:=False;
      Result:=True;
  end;
 // *********************************************************************
 // PRIVATE METHODS
 // *********************************************************************
 Procedure TMyClass.priv_ShowDebugInfo(Fmt:String;Args:Array of Const);
 begin
      If fDebugMode then
         MessageDlg(Format(Fmt,Args),mtInformation,[MBOK],0);
      If fLogMode then
         fEventLog.Debug(Fmt,Args);
 end;

 // *********************************************************************
 // PUBLIC METHODS
 // *********************************************************************

 Function TMyClass.InitOK:Boolean;
 // PUBLIC
  begin
       Result:=False;
       ErrorString:='TMyClass.InitOK failed.'  + CR;
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
 Procedure TMyClass.priv_SetErrorString(aString:String);
 begin if fErrorString <> aString then fErrorString:=aString; end;
 // *********************************************************************
 Procedure TMyClass.priv_SetLogFileName(aString:String);
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


 Procedure TMyClass.priv_SetDebugMode(aBoolean:Boolean);
 // Sets 'DebugMode' property
 begin
      ErrorString:='TMyClass.priv_SetDebugMode';
      fDebugMode:=aBoolean;
      If fDebugMode then priv_SetLogMode(True);
      // Turning DebugMode on automatically turns logging mode on
      fEventLog.Info('TMyClass Debug mode set')
 end;
 // *********************************************************************
 Procedure TMyClass.priv_SetLogMode(aBoolean:Boolean);
 // Sets 'LogMode' property
 begin
      ErrorString:='TMyClass.priv_SetLogMode';
      fLogMode:=aBoolean;
      fEventLog.Active:=fLogMode;
      fEventLog.Info('TMyClass Log mode set');
 end;

end.

