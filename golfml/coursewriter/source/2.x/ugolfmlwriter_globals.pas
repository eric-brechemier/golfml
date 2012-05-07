unit ugolfmlwriter_globals;
(*
== Author: minesadorada@charcodelvalle.com
==
== Lazarus: 0.9.30-0
== FPC: 2.4.4
==
==
*)
{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils;
CONST C_VERSION='2.5.20120507';

CONST C_GOLD=0;
CONST C_BLACK=1;
CONST C_WHITE=2;
CONST C_YELLOW=3;
CONST C_BLUE=4;
CONST C_RED=5;
CONST C_TEECOLOURSTRINGARRAY:Array[C_GOLD..C_RED] of String =
      ('Gold','Black','White','Yellow','Blue','Red');
// golfmlclass can have as many tee colours as you like.  This app uses just 6
CONST C_MAXCOURSES=6; // Not a restriction of golfmlclass - just available screen space
CONST C_COUNTRYCLUBINDEX=99; // So we know when it is selected
// Preceed fatal errors with a grovelling apology!
CONST C_ERRORAPOLOGY='An error has occurred that was not your fault.' + LineEnding;
CONST CR=LineEnding;
CONST C_MAXAMENITIES=16;
CONST C_AMENETYDELIMITERCHAR = ','; // Used in SplitAmenity
  // <amenety type=('practice' | 'store' | 'food' | 'corporate' | 'golfers' | 'bathroom' | 'water' | 'other' )
CONST AmenetyDefaultCategories:Array[0..C_MAXAMENITIES-1] of STRING =
      ('practice','practice','practice','practice','golfers','golfers','food','golfers',
      'bathroom','shop','food','food','other','corporate','corporate','other');
CONST AmenetyDefaultValues:Array[0..C_MAXAMENITIES-1] of STRING =
      ('Practice putting green','Practice driving range','Practice chipping area','Golf lessons',
      'Club hire','Buggy hire','Buggy bar','Club cleaning facilities','On-course bathroom facilities',
      'Clubhouse shop','Clubhouse bar','Clubhouse cafe/restaurant','Private parking',
      'Corporate/group facilities','PGA facilities','Private membership only');
// Writing Modes
CONST C_MODECOURSEONLY=0;
CONST C_MODECOURSESTYLESHEET=1;
CONST C_MODECOURSEPLAYERSTYLESHEET=2;
CONST C_GOLFMLCSSFILE='golfml.css';

Type
  TeeType = (Gold,Black,White,Yellow,Blue,Red);  // Unused
  // This is expandable, as the Tee Position arrays are Dynamic

Var
   // New Globals
  CourseIndex:Cardinal; // Current Course Index
  TeeIndex:Cardinal; // Current Tee Position index
  HoleIndex:Cardinal; // Current Hole Index
  TTeeColour:TeeType; // TeeColour:=Gold, Ord(Gold)=0; (unused)
  AmenetyCategoriesArray:Array[0..C_MAXAMENITIES-1] of STRING;
  AmenetyValuesArray:Array[0..C_MAXAMENITIES-1] of STRING;
  ConfigFilePath:String;
  MakeXMLMode:Word;
implementation

Uses uMainForm;



end.

