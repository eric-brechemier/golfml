unit usplashabout;

{$mode objfpc}{$H+}

interface

{
Due Credit
==========
uversion.pas by Mike Thompson - mike.cornflake@gmail.com
originally as VersionSupport.pas
See uversion.pas header for more details

==============================================
SplashAbout by minesadorada@charcodelvalle.com
==============================================

Purpose
=======
Constructs a Splash screen and About form with as little effort as possible :)
The windows and controls are created and destroyed on-the-fly to reduce application size

Files needed (in the same folder as your project)
================================================
usplashabout.pas,uversion.pas
Optional: gpl.txt, lgpl.txt,modifiedgpl.txt,mit.txt
Optional: <splashgraphic>.(bmp/jpg/gif)
Optional: <splashicon>.ico

Put usplashabout in the interface/uses of your main form
Declare a variable of type TSplashAbout in your form type definition

Example:
TForm1 = class(TForm)
...
private
  { private declarations }
  splash:TSplashAbout;
...
end;

EXAMPLE USE in form.Create()
(Most properties are Optional)
============================
splash:=TSplashAbout.Create(Self);
// (Uses Form Caption and Icon by default)

// Alternative: splash:=TSplashAbout.Create(Nil);
// (Uses Application.Title and project Icon by default)

splash.DelaySeconds:=3; // OPTIONAL. Default is 2 Seconds
splash.Title:='My Superb App'; // OPTIONAL. Default is Application Title of Form Caption
splash.IconFilePath:='ski.ico'; // OPTIONAL.  Default is Application.Icon or Form.Icon
splash.BackGroundImageFilePath:='splash.jpg'; // OPTIONAL.  Default is no Background Image. Optimal size=320 x 240
splash.LicenseFilePath:='gpl.txt'; // OPTIONAL.  Default is for Licence button to be absent on ShowAbout method
splash.LicenseType:='Public GPL License'; // OPTIONAL.  Default is no text
splash.CreditString:='Freeware by minesadorada'; // OPTIONAL.  Default is no text
splash.Author:='Mines A. Dorada'; // OPTIONAL.  Default is boilerplate text in LicenseFilePath
splash.SupportContact:='minesadorada@charcodelvalle.com'; // OPTIONAL.  Default is boilerplate text in LicenseFilePath
splash.ShowSplash;
=============================

EXAMPLE USE in Help/About
=========================
// Assuming splash class has already been created
splash.ShowAbout;

// If not, then:
splash:=TSplashAbout.Create(Nil);
// (See above for setting properties)
splash.ShowAbout;
}



uses
  Classes, SysUtils, Forms, Controls, Graphics, Buttons,
  ExtCtrls, StdCtrls, StrUtils, uversion;

type
  tFormType = (fSplash, fAbout);

type
  TSplashAbout = class(TObject)
  private
    splashform: TForm;
    fDelaySeconds: cardinal;
    fTitle: string;
    fIcon: TIcon;
    fIconFile, fBackGroundImageFileName: string;
    fVersionInfo: string;
    fFormType: tFormType;
    fLicenseFilePath: string;
    fLicenseType: string;
    fCreditString: string;
    fAuthor: string;
    fSupportContact: string;
    procedure CloseForm(Sender: TObject);
    procedure ShowForm;
    procedure ShowLicense(Sender: TObject);
  protected
  // Use anything from here..
  public
    constructor Create(Sender: TObject);
    procedure ShowSplash;
    procedure ShowAbout;
  published
    property DelaySeconds: cardinal read fDelaySeconds write fDelaySeconds;
    property Title: string read fTitle write fTitle;
    property Icon: TIcon read fIcon write fIcon;
    property IconFilePath: string read fIconFile write fIconFile;
    property BackGroundImageFilePath: string
      read fBackGroundImageFileName write fBackGroundImageFileName;
    property LicenseFilePath: string read fLicenseFilePath write fLicenseFilePath;
    property LicenseType: string read fLicenseType write fLicenseType;
    property CreditString: string read fCreditString write fCreditString;
    property SupportContact: string read fSupportContact write fSupportContact;
    property Author: string read fAuthor write fAuthor;
  end;

implementation

constructor TSplashAbout.Create(Sender: TObject);
// Initialise private vars
var
  Frm: TForm;
begin
  inherited Create;
  // Default values
  fDelaySeconds := 2;

  // Initialise from Application Object  [Create(Nil)]
  fTitle := 'My Application';
  fIcon := Application.Icon;
  // Or from main form [Create(Self)]
  if Sender is TForm then
  begin
    Frm := Sender as TForm;
    fTitle := Frm.Caption;
    fIcon := Frm.Icon;
  end;
  // Use uversion unit public methods to populate
  fVersionInfo := '';
  if (GetFileVersion <> 'No build information available') then
    fVersionInfo += 'Version ' + GetFileVersion + LineEnding + LineEnding;
  fVersionInfo += 'Made with: ' + GetLCLVersion;
  fVersionInfo += ' and ' + GetCompilerInfo + LineEnding;
  fVersionInfo += 'For: ' + GetTargetInfo + ' (' + GetWidgetSet + ')' + LineEnding;
  fVersionInfo += 'Last Compiled: ' + GetCompiledDate;

  // Optional property values
  fBackGroundImageFileName := '';
  fLicenseFilePath := '';
  fLicenseType := '';
  fCreditString := '';
  fAuthor := '';
  fSupportContact := '';

end;

procedure TSplashAbout.ShowLicense(Sender: TObject);
// Triggered by License button Click
var
  s: string;
  theList: TStringList;
  f: integer;
  LicenceForm: TForm;
  lblText: TLabel;
  closebuttton: TBitBtn;
begin
  // Trap for crap
  if not FileExists(fLicenseFilePath) then
    exit;

  // Use a string list to split the text file into lines
  theList := TStringList.Create;
  // Create a window, label and close button on-the-fly
  LicenceForm := TForm.Create(nil);
  lblText := TLabel.Create(LicenceForm);
  closebuttton := TBitBtn.Create(LicenceForm);
  // Load up the text
  s := LineEnding + LineEnding + fTitle + LineEnding;
  try
    theList.LoadFromFile(fLicenseFilePath);
    for f := 0 to TheList.Count - 1 do
      s += TheList[f] + LineEnding;
    s := AnsiReplaceText(s, '<year>',
{$I %DATE%}
      );
    // Replace boilerplate text if possible
    s := AnsiReplaceText(s, '<name of author>', fAuthor);
    s := AnsiReplaceText(s, '<contact>', '(' + fSupportContact + ')');
    s := AnsiReplaceText(s, '<copyright holders>', fAuthor);

    // Make up the form window and controls
    with LicenceForm do
    begin
      Width := 500;
      Height := 400;
      position := poScreenCenter;
      borderstyle := bsToolWindow;
      Caption := fTitle + ' Licensing';
      formstyle := fsSystemStayOnTop;
      // Label
      lblText.Align := alClient;
      lblText.Alignment := taCenter;
      lblText.Caption := s;
      lblText.Parent := LicenceForm;
      // Close Button
      closebuttton.Kind := bkClose;
      closebuttton.left := (Width div 2) - closebuttton.Width div 2;
      closebuttton.top := Height - closebuttton.Height - 10;
      closebuttton.parent := LicenceForm;
      // Show modally over the existing modal form
      ShowModal;
    end;
  finally
    FreeAndNil(theList);
    FreeAndNil(lblText);
    FreeAndNil(closebuttton);
    FreeAndNil(LicenceForm);
  end;
end;

procedure TSplashAbout.CloseForm(Sender: TObject);
// Triggered by a Timer.OnTimer event or CloseButton.Click
begin
  splashform.Close; // Hide and destroy
end;

procedure TSplashAbout.ShowSplash;
begin
  // Set the mode, then create and show the form
  fFormType := fSplash;
  ShowForm;
end;

procedure TSplashAbout.ShowAbout;
begin
  // Set the mode, then create and show the form
  fFormType := fAbout;
  ShowForm;
end;

procedure TSplashAbout.ShowForm;
// Main method
// Construct a form and show it modally
// Controls vary according to fFormType variable
var
  okbutton, LicenseButton: TBitBtn;
  delaytimer, scrolltimer: TTimer;
  lbl_Title, lbl_VersionInfo: TLabel;
  img_icon, img_background: TImage;
  bevel: TBevel;
  s: string;
begin
  // Temporarily create the form and controls
  splashform := TForm.CreateNew(nil);
  okbutton := TBitBtn.Create(splashform);
  LicenseButton := TBitBtn.Create(splashform);
  delaytimer := TTimer.Create(splashform);
  Scrolltimer := TTimer.Create(splashform);
  lbl_Title := TLabel.Create(SplashForm);
  lbl_VersionInfo := TLabel.Create(SplashForm);
  img_icon := TImage.Create(SplashForm);
  img_background := TImage.Create(SplashForm);
  bevel := TBevel.Create(SplashForm);
  // Now set positions and properties
  try
    with splashform do
    begin
      Width := 320;
      Height := 240;
      position := poScreenCenter;
      if fFormType = fAbout then
        borderstyle := bsToolWindow
      else
        borderstyle := bsnone;
      Caption := 'About ' + fTitle;
      formstyle := fsSystemStayOnTop;
      // Close Button
      if fFormType = fAbout then
      begin
        okbutton.Kind := bkClose;
        okbutton.left := (Width div 2) - okbutton.Width div 2;
        okbutton.top := Height - okbutton.Height - 10;
        okbutton.parent := splashform;
      end;
      // Delay Timer
      if fFormType = fSplash then
      begin
        if fDelaySeconds > 1000 then
          fDelaySeconds := fDelaySeconds div 1000;
        delaytimer.Interval := fDelaySeconds * 1000;
        delaytimer.OnTimer := @CloseForm;
        delaytimer.Enabled := True;
      end;
      // bevel
      bevel.Align := alClient;
      bevel.BorderSpacing.Around := 4;
      bevel.BorderSpacing.InnerBorder := 4;
      bevel.Parent := SplashForm;
      // Icon
      img_icon.Width := 32;
      img_icon.Height := 32;
      img_icon.Top := Top + 20;
      img_icon.Left := Width - 32 - 20;
      img_icon.Stretch := True;
      if FileExists(fIconFile) then
        fIcon.LoadFromFile(fIconFile);
      if fIcon <> nil then
        img_icon.Picture.Icon := fIcon;
      img_icon.Parent := SplashForm;
      // BackGround
      if FileExists(fBackGroundImageFileName) then
      begin
        img_background.Align := alClient;
        img_background.Stretch := True;
        img_background.Picture.LoadFromFile(fBackGroundImageFileName);
        img_background.Parent := SplashForm;
        img_background.SendToBack;
      end;
      // Title
      if fFormType = fSplash then
      begin
        lbl_Title.Top := Top + 32;
        lbl_Title.AutoSize := False;
        lbl_Title.Width := Width;
        lbl_Title.Font.Size := 12;
        lbl_Title.Font.Style := [];
        lbl_Title.Height := 32;
        lbl_Title.Alignment := taCenter;
        lbl_Title.Caption := fTitle;
        lbl_Title.Parent := SplashForm;
      end;
      // ScrollBox
      if (fFormType = fAbout) and FileExists(fLicenseFilePath) then
      begin
        LicenseButton.Top := okButton.Top;
        LicenseButton.Caption := 'License...';
        LicenseButton.left := Width - LicenseButton.Width - 10;
        LicenseButton.OnClick := @ShowLicense;
        LicenseButton.Parent := splashform;
      end;

      // Version Info
      lbl_VersionInfo.Left := 40;
      s := '';
      if fFormType = fAbout then
        s += fTitle;
      if fCreditString <> '' then
        s += LineEnding + fCreditString;
      s += LineEnding + fVersionInfo;
      if fLicenseType <> '' then
        s += LineEnding + LineEnding + 'Released under ' + fLicenseType;

      lbl_VersionInfo.Caption := s;
      if fFormType = fSplash then
        lbl_VersionInfo.Top := (Height div 2) - lbl_VersionInfo.Height - 20
      else
        lbl_VersionInfo.Top := 50;
      lbl_VersionInfo.Parent := SplashForm;
      Application.ProcessMessages;
      showModal;
      Application.ProcessMessages;
    end;
  finally
    // Controls normally destroyed with parent
    // but if Try block fails, ensure no memory leaks
    FreeAndNil(bevel);
    FreeAndNil(img_icon);
    FreeAndNil(img_background);
    FreeAndNil(lbl_Title);
    FreeAndNil(lbl_VersionInfo);
    FreeAndNil(okbutton);
    FreeAndNil(LicenseButton);
    FreeAndNil(delaytimer);
    FreeAndNil(Scrolltimer);
    FreeAndNil(splashform);
  end;

end;

end.
