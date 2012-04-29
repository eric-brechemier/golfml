unit uwaitform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls;

type

  { Twaitform }

  Twaitform = class(TForm)
    lbl_info: TLabel;
    loadprogress: TProgressBar;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  waitform: Twaitform;

implementation

{$R *.lfm}

end.

