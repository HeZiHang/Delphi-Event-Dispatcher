object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'FormMain'
  ClientHeight = 444
  ClientWidth = 616
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 616
    Height = 41
    Align = alTop
    TabOrder = 0
    ExplicitLeft = 224
    ExplicitTop = 216
    ExplicitWidth = 185
    object Button1: TButton
      Left = 32
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Start'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 136
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Stop'
      TabOrder = 1
      OnClick = Button2Click
    end
  end
end
