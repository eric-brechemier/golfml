unit uscoreform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls;
CONST CR=#10+#13;
type

  { Tscoreform }

  Tscoreform = class( TForm)
    cmd_calculate: TBitBtn;
    cmd_close: TBitBtn;
    lbl_intro: TLabel;
    procedure cmd_calculateclick(sender: tobject);
    procedure cmd_closeClick( Sender: TObject);
    procedure formcreate(sender: tobject);
    procedure formshow(sender: tobject);
  private
    { private declarations }
    LabelArray:Array[1..18] of TLabel;
    EditArray:Array[1..18] of TEdit;
    procedure MakeControls;
    Function CheckDigits:Integer;
    // Function CalculateBestScore:Integer;
    Function HCPClassCalculateBestScore:Integer;
  public
    { public declarations }
  end; 

var
  scoreform: Tscoreform;

implementation

{$R *.lfm}

{ Tscoreform }
Uses umainform;
Function tscoreform.HCPClassCalculateBestScore:Integer;
// Uses HCPClass properties and methods to calculate
// the stableford points using a scorecard and nominal handicap of 36.
// This imaginary score will then be used to calculate
// a new Exact EGA Handicap for a player according to EGA rules.
Var iCount:Integer;
begin
     For iCount:=0 to 17 do
         begin
            mainform.HCPClass.ScoreArrayElement[iCount]:=StrToInt(EditArray[iCount+1].Text);
         end;
     Result:=mainform.HCPClass.CalculateBestScore;
end;


procedure tscoreform.formcreate(sender: tobject);
Var s:String;
begin
  MakeControls;
  s:='Enter the scorecard scores, then click [Calculate], ';
  s+='and then [Done]';
  lbl_intro.Caption:=s;
  Caption:='Scorecard';
end;

procedure tscoreform.formshow(sender: tobject);
// ReInitialise controls and values
Var
   iCount:Integer;
begin
  For iCount:=1 to 18 do
      begin
           EditArray[iCount].Text:='0';
           mainform.HCPClass.Hole:=iCount;
           LabelArray[iCount].Caption:=Format('%d: Par %d',[iCount,mainform.HCPClass.Par]);
      end;

  EditArray[1].SetFocus;
end;

procedure Tscoreform. cmd_closeClick( Sender: TObject);
begin
     mainform.Show;
     Application.ProcessMessages; // Allow mainform to show promptly
     hide;
end;

procedure tscoreform.cmd_calculateclick(sender: tobject);
Var
   iError,iBestScore:Integer;
begin
     iError:=CheckDigits;
     If iError > 0 then
        begin
             ShowMessage(Format('Hoya %d score is not a valid number.  Please correct.',[iError]));
             Exit;
        end;
     iBestScore:=HCPClassCalculateBestScore; // Uses HCPClass to calculate the Exact EGA Handicap
     mainform.bestscore:=iBestScore;
     mainform.ud_beststableford.Position:=iBestScore; // NOTE MINIMUM OF 36
     mainform.CalculateAndDisplayExactHandicap; // Uses HCPClass
     mainform.CalculateAndDisplayPlayingHandicap; // Uses HCPClass
     If MessageDlg(Format('Calculated notional 36-handicap score is %d%s%s',[mainform.bestscore,CR,'Exit the scorecard?']),
     mtConfirmation,[MBYES,MBNO],0)=mrYes then cmd_close.Click;
end;


procedure Tscoreform.MakeControls;
// Make up the Labels and Edit controls on-the-fly as control arrays
Var iTop,iLabelLeft1,iLabelLeft2,iEditLeft1,iEditLeft2:Integer;
    iOffsetVertical,iHeight,iLabelWidth,iEditWidth:Integer;
    iTempTop,iCount:Integer;
begin
   iTop:=80;
   iLabelLeft1:=16;
   iLabelLeft2:=112;
   iEditLeft1:=64;
   iEditLeft2:=160;
   iOffsetVertical:=30;
   iHeight:=23;
   iLabelWidth:=38;
   iEditWidth:=32;
   iTempTop:=iTop;
   For iCount:=1 to 9 do
       begin
         mainform.HCPClass.Hole:=iCount;
         LabelArray[iCount]:=TLabel.Create(Nil);
         LabelArray[iCount].Top:=iTempTop;
         LabelArray[iCount].Left:=iLabelLeft1;
         LabelArray[iCount].Height:=iHeight;
         LabelArray[iCount].Width:=iLabelWidth;
         LabelArray[iCount].Caption:=Format('%d: Par %d',[iCount,mainform.HCPClass.Par]);
         LabelArray[iCount].Tag:=iCount;
         LabelArray[iCount].Parent:=scoreform;

         EditArray[iCount]:=TEdit.Create(Nil);
         EditArray[iCount].Top:=iTempTop;
         EditArray[iCount].Left:=iEditLeft1;
         EditArray[iCount].Height:=iHeight;
         EditArray[iCount].Width:=iEditWidth;
         EditArray[iCount].MaxLength:=2;
         EditArray[iCount].Text:='0';
         EditArray[iCount].TabOrder:=iCount;
         EditArray[iCount].Parent:=scoreform;
         Inc(iTempTop,iOffsetVertical);
       end;
   iTempTop:=iTop;
   For iCount:=10 to 18 do
       begin
            mainform.HCPClass.Hole:=iCount;
         LabelArray[iCount]:=TLabel.Create(Nil);
         LabelArray[iCount].Top:=iTempTop;
         LabelArray[iCount].Left:=iLabelLeft2;
         LabelArray[iCount].Height:=iHeight;
         LabelArray[iCount].Width:=iLabelWidth;
         LabelArray[iCount].Caption:=Format('%d: Par %d',[iCount,mainform.HCPClass.Par]);
         LabelArray[iCount].Tag:=iCount;
         LabelArray[iCount].Parent:=scoreform;

         EditArray[iCount]:=TEdit.Create(Nil);
         EditArray[iCount].Top:=iTempTop;
         EditArray[iCount].Left:=iEditLeft2;
         EditArray[iCount].Height:=iHeight;
         EditArray[iCount].Width:=iEditWidth;
         EditArray[iCount].MaxLength:=2;
         EditArray[iCount].Taborder:=iCount;
         EditArray[iCount].Text:='0';
         EditArray[iCount].Parent:=scoreform;
         Inc(iTempTop,iOffsetVertical);
       end;
       cmd_calculate.TabOrder:=19;
       cmd_close.TabOrder:=20;
end;
Function Tscoreform.CheckDigits:Integer;
// Returns bad EditArray index number if not in range 0..9 or Zero if OK
Var iCount,f:Integer;
    s:ShortString;
begin
     Result:=0;
     for iCount:=1 to 18 do
         begin
              s:=EditArray[iCount].Text;
              for f:=1 to length(s) do
                       If not (s[f] in ['0'..'9']) then
                          begin
                               Result:=iCount;
                               EXIT;

                          end;
         end;
end;

end.

