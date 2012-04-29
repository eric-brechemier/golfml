unit usplashform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  ExtCtrls, StdCtrls;
CONST CR=LineEnding;

type

  { Tsplashform }

  Tsplashform = class(TForm)
    cmd_close: TBitBtn;
    img_icon: TImage;
    lbl_text: TLabel;
    pnl_text: TPanel;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  splashform: Tsplashform;

implementation
Uses uMainForm;
{$R *.lfm}

{ Tsplashform }

procedure Tsplashform.FormCreate(Sender: TObject);
Var s:String;
begin
  If Application.Icon <> Nil then
   Begin
    Icon:=Application.Icon;
    img_Icon.Picture.Icon:=Application.Icon;
   End;
     s:=CR + CR + Application.Title + CR;
     s+=Format('by Gordon Bamber v.%s%s',[C_APPVERSION,CR]);
     s+='USGA/EGA ref. [www.ega-golf.ch'+CR;
     s+='/030000/documents/EGA_Hcp_Int.pdf]' + CR;
     s+='THCPGolfUMLClass v' + mainform.HCPClass.Version + CR;
     s+='Distribution: Creative Commons license' + CR;
     s+='Golfml: code.google.com/p/golfml/';
     lbl_text.Caption:=s;
end;

end.

