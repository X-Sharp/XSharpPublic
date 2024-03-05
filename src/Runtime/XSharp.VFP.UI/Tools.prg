// Tools.prg
// Created by    : fabri
// Creation Date : 5/16/2021 1:26:34 PM
// Created for   : 
// WorkStation   : FABXPS

FUNCTION VFPAlignmentConvert( n AS INT ) AS System.Drawing.ContentAlignment
	VAR convert := System.Drawing.ContentAlignment.MiddleLeft
	SWITCH n
	CASE 0
		convert := System.Drawing.ContentAlignment.MiddleLeft
	CASE 1
		convert := System.Drawing.ContentAlignment.MiddleRight
	CASE 2
		convert := System.Drawing.ContentAlignment.MiddleCenter
	CASE 4
		convert := System.Drawing.ContentAlignment.TopLeft
	CASE 5
		convert := System.Drawing.ContentAlignment.TopRight
	CASE 6
		convert := System.Drawing.ContentAlignment.TopCenter
	CASE 7
		convert := System.Drawing.ContentAlignment.BottomLeft
	CASE 8
		convert := System.Drawing.ContentAlignment.BottomRight
	CASE 9
		convert := System.Drawing.ContentAlignment.BottomCenter
	END SWITCH
	RETURN convert
	
FUNCTION VFPAlignmentConvert( convert AS System.Drawing.ContentAlignment ) AS INT
	VAR n := 0
	SWITCH convert
	CASE System.Drawing.ContentAlignment.MiddleLeft
		n := 0
	CASE System.Drawing.ContentAlignment.MiddleRight
		n := 1
	CASE System.Drawing.ContentAlignment.MiddleCenter
		n := 2
	CASE System.Drawing.ContentAlignment.TopLeft
		n := 4
	CASE System.Drawing.ContentAlignment.TopRight
		n := 5
	CASE System.Drawing.ContentAlignment.TopCenter
		n := 6
	CASE System.Drawing.ContentAlignment.BottomLeft
		n := 7
	CASE System.Drawing.ContentAlignment.BottomRight
		n := 8
	CASE System.Drawing.ContentAlignment.BottomCenter
		n := 9
	END SWITCH
	RETURN n
	
FUNCTION VFPImageStrechConvert( n AS INT ) AS System.Windows.Forms.PictureBoxSizeMode
	VAR convert := System.Windows.Forms.PictureBoxSizeMode.Normal
	SWITCH n
	CASE 1
		convert := System.Windows.Forms.PictureBoxSizeMode.AutoSize
	CASE 2
		convert := System.Windows.Forms.PictureBoxSizeMode.StretchImage
	END SWITCH
	RETURN convert
	
	
FUNCTION VFPTextAlignmentConvert( n AS INT ) AS System.Windows.Forms.HorizontalAlignment
	VAR convert := System.Windows.Forms.HorizontalAlignment.Left
	SWITCH n
	CASE 0
		convert := System.Windows.Forms.HorizontalAlignment.Left
	CASE 1
		convert := System.Windows.Forms.HorizontalAlignment.Right
	CASE 2
		convert := System.Windows.Forms.HorizontalAlignment.Center
	CASE 3
		convert := System.Windows.Forms.HorizontalAlignment.Left
	END SWITCH
	RETURN convert
	
	
FUNCTION VFPGuessType( uValue AS USUAL ) AS STRING
	LOCAL ret := String.Empty AS STRING
	//
	var newType := UsualType(uValue)
	//
	SWITCH newType
	CASE __UsualType.Float
	CASE __UsualType.Decimal
	CASE __UsualType.Currency
	CASE __UsualType.Int64
	CASE __UsualType.Long
		ret := "N"
	CASE __UsualType.Date
	CASE __UsualType.DateTime
		ret := "D"
	CASE __UsualType.Logic
		ret := "L"
	CASE __UsualType.String
		ret := "C"
	END SWITCH
	RETURN ret
	
	
	
	
	