object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Batch Control Objects'
  ClientHeight = 369
  ClientWidth = 552
  Color = clBtnFace
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
  object Button1: TButton
    Left = 56
    Top = 56
    Width = 129
    Height = 25
    Caption = '1.Create 10 Forms'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 56
    Top = 112
    Width = 129
    Height = 25
    Caption = '2.Hide the Forms'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 56
    Top = 168
    Width = 129
    Height = 25
    Caption = '3.Show the Forms'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 56
    Top = 224
    Width = 129
    Height = 25
    Caption = '4.Set Label in the Forms'
    TabOrder = 3
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 56
    Top = 272
    Width = 129
    Height = 25
    Caption = '5.Close the Forms'
    TabOrder = 4
    OnClick = Button5Click
  end
  object Edit1: TEdit
    Left = 216
    Top = 226
    Width = 257
    Height = 21
    TabOrder = 5
    Text = 'Label  Text'
  end
end
