//#using System.Drawing
#using System.Collections
#using System.Collections.Generic
#using System.Drawing


#include "VOWin32APILibrary.vh"

ENUM PropertyType
	MEMBER None
	MEMBER Numeric
	MEMBER Text
//	MEMBER Boolean
	MEMBER Enumerated
	MEMBER Type
	MEMBER CODE
	MEMBER @@Callback
END ENUM

ENUM VOStyle
	MEMBER None
	MEMBER Style
	MEMBER ExStyle
END ENUM

[flags];
ENUM PropertyStyles
	MEMBER None := 0
	MEMBER ReadOnly := 1
	MEMBER NoNULL := 2
	MEMBER NoAuto := 4
	MEMBER NoVisual := 8
	MEMBER NoCF := 16
	MEMBER Track := 32
	MEMBER NoCode := 64
END ENUM


CLASS VODesignProperty INHERIT DesignProperty
	EXPORT eVOStyle AS VOStyle

	EXPORT lMethod AS LOGIC
	EXPORT cType AS STRING
	EXPORT cMember AS STRING
	EXPORT lMultiple AS LOGIC
	EXPORT nMultiPos AS INT
	EXPORT cSymbolProp AS STRING

	CONSTRUCTOR(cName AS STRING,cCaption AS STRING,_cMember AS STRING,eType AS PropertyType)
		SUPER(cName,cCaption,eType)
		SELF:cMember := _cMember
	RETURN
	CONSTRUCTOR(cName AS STRING,cCaption AS STRING,_cMember AS STRING,eType AS PropertyType , eStyle AS PropertyStyles)
		SUPER(cName,cCaption,eType,eStyle)
		SELF:cMember := _cMember
	RETURN
	CONSTRUCTOR(cName AS STRING,cCaption AS STRING,_cMember AS STRING,_cEnum AS STRING)
		SUPER(cName,cCaption,_cEnum)
		SELF:cMember := _cMember

//		IF SELF:cMember != NULL
/*			LOCAL cAssignMap AS STRING
			cAssignMap := VOWindowEditorTemplate.GetAssignMap(SELF:cMember)
			IF cAssignMap != NULL .and. cAssignMap:Length != 0
				DO CASE
				CASE cAssignMap == "Color"
					SELF:cSpecialClass := "Color"
				CASE cAssignMap == "Brush"
					SELF:cSpecialClass := "Brush"
				CASE cAssignMap == "Font"
					SELF:cSpecialClass := "Font"
				CASE cAssignMap == "FillUsing"
					SELF:cSpecialClass := "FillUsing"
				END CASE
			END IF*/
//		END IF
		DO CASE
		CASE SELF:cEnumType != NULL .and. SELF:cEnumType:ToUpper() == "COLOR"
			SELF:cSpecialClass := "Color"
		CASE SELF:cEnumType != NULL .and. SELF:cEnumType:ToUpper() == "FONT"
			SELF:cSpecialClass := "Font"
		CASE SELF:cMember != NULL .and. SELF:cMember:ToUpper() == "FILLUSING"
			SELF:cSpecialClass := "FillUsing"
		END CASE
//		System.Diagnostics.Debug.WriteLine(cName + " , " + cCaption)
	RETURN
	CONSTRUCTOR(cName AS STRING,cCaption AS STRING,_cMember AS STRING,_cEnum AS STRING,eStyle AS PropertyStyles)
		SUPER(cName,cCaption,_cEnum,eStyle)
		SELF:cMember := _cMember

		DO CASE
		CASE SELF:cEnumType != NULL .and. SELF:cEnumType:ToUpper() == "COLOR"
			SELF:cSpecialClass := "Color"
		CASE SELF:cEnumType != NULL .and. SELF:cEnumType:ToUpper() == "FONT"
			SELF:cSpecialClass := "Font"
		CASE SELF:cMember != NULL .and. SELF:cMember:ToUpper() == "FILLUSING"
			SELF:cSpecialClass := "FillUsing"
		END CASE
//		System.Diagnostics.Debug.WriteLine(cName + " , " + cCaption)
	RETURN
	CONSTRUCTOR(cName AS STRING , _cEnumType AS STRING , _cEnumValues AS STRING, _eVOStyle AS VOStyle)
		SUPER(cName,cName,PropertyType.Enumerated,PropertyStyles.NoAuto)
		SELF:_Init(cName,cName,PropertyType.Enumerated,PropertyStyles.NoAuto)
		SELF:eVOStyle := _eVOStyle
		SELF:cEnumType := _cEnumType
		SELF:cEnumValues := _cEnumValues
		SELF:InitEnum()
	RETURN

	VIRTUAL ACCESS IsAuto AS LOGIC
		DO CASE
		CASE SELF:eVOStyle != VOStyle.None
			RETURN FALSE
		END CASE
	RETURN SUPER:IsAuto

	METHOD InitVOStyle(aList AS List<STRING>) AS VOID
		LOCAL n AS INT
		LOCAL cValue AS STRING
		LOCAL nMatches AS INT
/*		IF SELF:cEnumType == "BOOL"
//			SELF:Value := "FALSE"
			SELF:Value := SELF:aEnumTextValues[1]
		ENDIF*/
/*		IF aList == NULL
			RETURN
		ENDIF*/
/*		FOR n := 0 UPTO aList:Count - 1
			cValue := aList[n]:ToUpper()
			FOR m := 0 UPTO SELF:aEnumValues:Count - 1
//				IF SELF:aEnumValues[m]:ToUpper() == cValue:ToUpper()
				IF SELF:aEnumValues[m]:ToUpper():IndexOf(cValue) != -1
					SELF:Value := SELF:aEnumTextValues[m]
					RETURN
				ENDIF
			NEXT
		NEXT*/
		LOCAL aValues AS List<STRING>
		cValue := NULL
		
		IF aList != NULL
			FOR n := 0 UPTO SELF:aEnumValues:Count - 1
				aValues := Funcs.SplitString(SELF:aEnumValues[n]:ToUpper() , '|')
				IF aValues:Count > nMatches
					IF Funcs.ListContainsList(aList , aValues)
						cValue := SELF:aEnumTextValues[n]
						nMatches := aValues:Count
					ENDIF
				ENDIF
			NEXT
		END IF
		IF cValue != NULL
			SELF:Value := cValue
			RETURN
		ENDIF
		
		IF SELF:cEnumType == "BOOL"
			SELF:Value := "FALSE"
//			SELF:Value := SELF:aEnumTextValues[1]
		ENDIF
	RETURN

	VIRTUAL METHOD GetTextValue(oTest AS OBJECT) AS STRING
		LOCAL cRet AS STRING
		IF SELF:cSpecialClass != NULL
			DO CASE
			CASE oTest:GetType() == TypeOf(System.Drawing.Font)
				LOCAL oFont AS System.Drawing.Font
				oFont := (System.Drawing.Font)oTest
				cRet := oFont:Name + ", " + oFont:Size:ToString()
				IF oFont:Bold
					cRet += ", Bold"
				ENDIF
				IF oFont:Italic
					cRet += ", Italic"
				ENDIF
				IF oFont:Underline
					cRet += ", Underline"
				ENDIF
				IF oFont:Strikeout
					cRet += ", Strikeout"
				ENDIF
				RETURN cRet
			CASE oTest:GetType() == TypeOf(System.Drawing.Color)
				LOCAL oColor AS System.Drawing.Color
				oColor := (System.Drawing.Color)oTest
				cRet := "R:" + oColor:R:ToString()
				cRet += " G:" + oColor:G:ToString()
				cRet += " B:" + oColor:B:ToString()
				RETURN cRet
			CASE oTest:GetType() == TypeOf(FillUsingClass)
				RETURN oTest:ToString()
			CASE oTest:GetType() == TypeOf(MenuAccelerator)
				RETURN oTest:ToString()
			END CASE
		ENDIF
	RETURN SUPER:GetTextValue(oTest)
	VIRTUAL ASSIGN Value(_oValue AS OBJECT)
		LOCAL nAt AS INT
		LOCAL n AS INT
		
		IF SELF:cSpecialClass != NULL
			DO CASE
			CASE _oValue:GetType() == TypeOf(System.Drawing.Font)
				SELF:oValue := _oValue
				RETURN
			CASE _oValue:GetType() == TypeOf(System.Drawing.Color)
				SELF:oValue := _oValue
				RETURN
			CASE _oValue:GetType() == TypeOf(FillUsingClass)
				SELF:oValue := _oValue
				RETURN
			CASE _oValue:GetType() == TypeOf(MenuAccelerator)
				SELF:oValue := _oValue
				RETURN
			CASE _oValue:GetType() == TypeOf(STRING)
				LOCAL cValue AS STRING
				cValue := (STRING)_oValue
				DO CASE
				CASE SELF:cSpecialClass == "__MenuAccelerator"
					LOCAL cAccelerator AS STRING
					SELF:oValue := MenuAccelerator{"" , FALSE , FALSE , FALSE}
					TRY
						cValue := cValue:Trim()
						nAt := cValue:IndexOf("+")
						IF nAt != -1 .and. cValue != "+"
							cAccelerator := cValue:Substring(nAt + 1):Trim()
							cValue := cValue:Substring(0 , nAt):Trim():ToUpper()
						ELSE
							cAccelerator := cValue:Trim()
							cValue := ""
						END IF
						FOR n := 0 UPTO VOMenuEditor.AccelKeys:Count - 1
							IF VOMenuEditor.AccelKeys:GetName(n) == cAccelerator
								SELF:oValue := MenuAccelerator{cAccelerator , cValue:Contains("CTRL") , cValue:Contains("SHIFT") , cValue:Contains("ALT")}
								EXIT
							END IF
						NEXT
					END TRY
					RETURN
					
				CASE SELF:cSpecialClass == "Color" //.or. SELF:cSpecialClass == "Brush"
					IF cValue:Contains(" ")
						LOCAL aRGB AS STRING[]
						aRGB := cValue:Split(' ')
						SELF:oValue := System.Drawing.Color.FromArgb(Convert.ToInt32(aRGB[1]) , Convert.ToInt32(aRGB[2]) , Convert.ToInt32(aRGB[3]))
						RETURN
					END IF
				CASE SELF:cSpecialClass == "Font"
					IF cValue:Contains(":")
						LOCAL oFont AS System.Drawing.Font
						LOCAL cFont AS STRING
						LOCAL rSize AS REAL4
						LOCAL eStyle AS FontStyle
						LOCAL aFont AS STRING[]

						aFont := cValue:Split(':')
						cFont := aFont[2]
						rSize := (REAL4)10
						TRY
							rSize := Convert.ToSingle(aFont[1])
							IF rSize > (REAL4)500 // TODO big bad ugly hack
								rSize := rSize / (REAL4)100.0
							ENDIF
						END TRY
						FOR n := 3 UPTO aFont:Length
							DO CASE
							CASE aFont[n]:ToUpper() == "BOLD"
								eStyle += FontStyle.Bold
							CASE aFont[n]:ToUpper() == "ITALIC"
								eStyle += FontStyle.Italic
							CASE aFont[n]:ToUpper() == "STRIKETHRU"
								eStyle += FontStyle.Strikeout
							CASE aFont[n]:ToUpper() == "UNDERLINE"
								eStyle += FontStyle.Underline
							END CASE
						NEXT
						TRY
							oFont := Font{cFont , rSize , eStyle}
						END TRY
						SELF:oValue := oFont
						RETURN
					END IF
				CASE SELF:cSpecialClass == "FillUsing"
					LOCAL oUsing AS FillUsingClass
					oUsing := FillUsingClass{}
					DO CASE
					CASE cValue == ""
						SELF:oValue := ""
						RETURN
					CASE cValue:Length > 4 .and. cValue:ToUpper():StartsWith("SELF:")
						oUsing:eType := FillUsingType.UseMethod
						cValue := cValue:Substring(5 , cValue:Length - 5):Trim()
						nAt := cValue:IndexOf("(")
						IF nAt != -1
							cValue := cValue:Substring(0 , nAt):Trim()
						END IF
						oUsing:cValue := cValue
					CASE cValue:Contains("{},#")
						LOCAL aFields AS STRING[]
						oUsing:eType := FillUsingType.UseServer
						aFields := cValue:Split(',')
						cValue := aFields[1]:Trim()
						oUsing:cValue := cValue:Substring(0 , cValue:Length - 2)
						oUsing:cField1 := aFields[2]:Trim():Substring(1)
						oUsing:cField2 := aFields[3]:Trim():Substring(1)
					OTHERWISE
						oUsing:eType := FillUsingType.UseArray
						oUsing:cValue := cValue
					END CASE
					SELF:oValue := oUsing
					RETURN
				END CASE
			END CASE
		ENDIF
		SUPER:Value := _oValue
	RETURN
	VIRTUAL ACCESS CodeValue AS STRING
		IF SELF:cSpecialClass != NULL
			DO CASE
			CASE SELF:oValue:GetType() == TypeOf(System.Drawing.Font)
				LOCAL oFont AS System.Drawing.Font
				LOCAL cValue AS STRING
				oFont := (System.Drawing.Font)SELF:oValue
				cValue := " , "
//				cValue += oFont:Size:ToString():Replace(',' , '.')
				cValue += ((INT)Math.Round(oFont:Size)):ToString()
				cValue += " , " + e"\"" + oFont:Name + e"\""
				RETURN cValue
			CASE SELF:oValue:GetType() == TypeOf(System.Drawing.Color)
				LOCAL oColor AS System.Drawing.Color
				oColor := (System.Drawing.Color)SELF:oValue
				RETURN oColor:R:ToString() + " , " + oColor:G:ToString() + " , " + oColor:B:ToString()
			CASE SELF:oValue:GetType() == TypeOf(FillUsingClass)
				RETURN SELF:oValue:ToString()
			CASE SELF:oValue:GetType() == TypeOf(MenuAccelerator)
				RETURN SELF:oValue:ToString()
			END CASE
		ENDIF
	RETURN SUPER:CodeValue

	VIRTUAL ACCESS SaveValue AS STRING
		LOCAL cValue AS STRING

		IF SELF:cSpecialClass != NULL
			DO CASE
			CASE SELF:oValue:GetType() == TypeOf(System.Drawing.Font)
				LOCAL oFont AS System.Drawing.Font
				oFont := (System.Drawing.Font)SELF:oValue
				cValue := oFont:Size:ToString() + ":" + oFont:Name
				IF oFont:Bold
					cValue += ":Bold"
				ENDIF
				IF oFont:Italic
					cValue += ":Italic"
				ENDIF
				IF oFont:Underline
					cValue += ":Underline"
				ENDIF
				IF oFont:Strikeout
					cValue += ":Strikethru"
				ENDIF
				RETURN cValue
			CASE SELF:oValue:GetType() == TypeOf(System.Drawing.Color)
				LOCAL oColor AS System.Drawing.Color
				oColor := (System.Drawing.Color)SELF:oValue
				RETURN oColor:R:ToString() + " " + oColor:G:ToString() + " " + oColor:B:ToString()
			CASE SELF:oValue:GetType() == TypeOf(FillUsingClass)
				RETURN SELF:oValue:ToString()
			CASE SELF:oValue:GetType() == TypeOf(MenuAccelerator)
				LOCAL oAccel AS MenuAccelerator
				oAccel := (MenuAccelerator)SELF:Value
				IF oAccel:IsEmpty
					RETURN ""
				END IF
				cValue := ChrW((DWORD)(48 + iif(oAccel:Control , 4 , 0) + iif(oAccel:Shift , 2 , 0) + iif(oAccel:Alt , 1 , 0)))
				cValue += oAccel:Key
				RETURN cValue
			END CASE
		ENDIF
	RETURN SUPER:SaveValue
	
	METHOD ToString() AS STRING
		IF SELF:eVOStyle == VOStyle.None
			RETURN "Property " + SELF:Name
		END IF
	RETURN SELF:eVOStyle:ToString() + " " + SELF:Name
	
END CLASS



CLASS DesignProperty
	EXPORT Name AS STRING
	EXPORT Caption AS STRING
	EXPORT Type AS PropertyType
	EXPORT eStyle AS PropertyStyles
	EXPORT cPage AS STRING

	PROTECT oValue AS OBJECT

	EXPORT cEnumType AS STRING
	EXPORT aEnumTextValues AS List<STRING>
	EXPORT cEnumValues AS STRING
	EXPORT aEnumValues AS List<STRING>

	EXPORT lAllowNULL AS LOGIC
	EXPORT lTrack AS LOGIC
	EXPORT lNoVisual AS LOGIC
	EXPORT lReadOnly AS LOGIC
	EXPORT lNoAuto AS LOGIC

	EXPORT lNoCF AS LOGIC
	EXPORT lNoCode AS LOGIC

	EXPORT cSpecialClass AS STRING
	
	CONSTRUCTOR(cName AS STRING,eType AS PropertyType,eStyle AS PropertyStyles)
		SELF:_Init(cName,cName,eType,eStyle)
	RETURN
	CONSTRUCTOR(cName AS STRING,cCaption AS STRING,eType AS PropertyType)
		SELF:_Init(cName,cCaption,eType,PropertyStyles.None)
	RETURN
	CONSTRUCTOR(cName AS STRING,cCaption AS STRING,eType AS PropertyType,eStyle AS PropertyStyles)
		SELF:_Init(cName,cCaption,eType,eStyle)
	RETURN
	CONSTRUCTOR(cName AS STRING,cCaption AS STRING,_cEnum AS STRING,_eStyle AS PropertyStyles)
		SELF:_Init(cName,cCaption,PropertyType.Enumerated,eStyle)
		SELF:cEnumType := _cEnum
		IF SELF:cEnumType == "YESNO"
			SELF:lNoAuto := TRUE
		ENDIF
		SELF:InitEnum()
	RETURN
	CONSTRUCTOR(cName AS STRING,cCaption AS STRING,_cEnum AS STRING)
		SELF:_Init(cName,cCaption,PropertyType.Enumerated,PropertyStyles.None)
		SELF:cEnumType := _cEnum
		IF SELF:cEnumType == "YESNO"
			SELF:lNoAuto := TRUE
		ENDIF
		SELF:InitEnum()
	RETURN
	PROTECTED METHOD InitEnum() AS VOID
		LOCAL cValues AS STRING
		LOCAL cValue AS STRING
		LOCAL lAuto AS LOGIC
		LOCAL nAt AS INT

		cValues := VOWindowEditorTemplate.GetEnumerator(SELF:cEnumType)
		IF cValues == NULL
			SELF:aEnumTextValues:Add("<Property Type not Found>")
		ELSE
			DO WHILE cValues:Length != 0
				nAt := cValues:IndexOf(',')
				IF nAt == -1
					SELF:aEnumTextValues:Add(cValues)
					cValues := ""
				ELSE
					cValue := cValues:Substring(0 , nAt)
					IF cValue:ToUpper() == "AUTO"
						lAuto := TRUE
						SELF:aEnumTextValues:Add(cValue)
					ELSE
						SELF:aEnumTextValues:Add(cValue)
					ENDIF
					cValues := cValues:Substring(nAt + 1)
				ENDIF
			ENDDO
//			SELF:lNoAuto := !lAuto
			IF lAuto
				SELF:lNoAuto := FALSE
			ENDIF
		ENDIF
		
		cValues := SELF:cEnumValues
		IF cValues != NULL
			DO WHILE cValues:Length != 0
				nAt := cValues:IndexOf(':')
				IF nAt == -1
					SELF:aEnumValues:Add(cValues)
					cValues := ""
				ELSE
					SELF:aEnumValues:Add(cValues:Substring(0 , nAt))
					cValues := cValues:Substring(nAt + 1)
				ENDIF
			ENDDO
		ENDIF
		DO WHILE SELF:aEnumTextValues:Count > SELF:aEnumValues:Count
			SELF:aEnumValues:Add("")
		END DO
		
	RETURN
	METHOD _Init(cName AS STRING,cCaption AS STRING,eType AS PropertyType,_eStyle AS PropertyStyles) AS VOID
		SELF:cPage := "General"
		SELF:Name:=cName
		SELF:Caption:=cCaption
		SELF:Type:=eType
		SELF:eStyle := _eStyle
		IF SELF:Type == PropertyType.Enumerated
			SELF:aEnumTextValues := List<STRING>{}
			SELF:aEnumValues := List<STRING>{}
		ENDIF
		SELF:ReadStyles(_eStyle)
		SELF:PutEmptyValue()
//		SELF:cPage := "None"
/*		IF cName[0] == '_'
			SELF:cPage := "General"
		ENDIF*/
	RETURN
	
	METHOD ReadStyles(eStyle AS PropertyStyles) AS VOID
		IF _And(INT(eStyle),INT(PropertyStyles.ReadOnly))!=0
			SELF:lReadOnly := TRUE
		ENDIF
		IF _And(INT(eStyle),INT(PropertyStyles.NoNULL))!=0
			SELF:lAllowNULL := FALSE
		ENDIF
		IF _And(INT(eStyle),INT(PropertyStyles.NoAuto))!=0
			SELF:lNoAuto := TRUE
		ENDIF
		IF _And(INT(eStyle),INT(PropertyStyles.NoVisual))!=0
			SELF:lNoVisual := TRUE
		ENDIF
		IF _And(INT(eStyle),INT(PropertyStyles.Track))!=0
			SELF:lTrack := TRUE
		ENDIF
		IF _And(INT(eStyle),INT(PropertyStyles.NoCode))!=0
			SELF:lNoCode := TRUE
		ENDIF
		
	RETURN

	PROTECTED VIRTUAL METHOD PutEmptyValue() AS VOID
		DO CASE
		CASE SELF:Type == PropertyType.Text
			SELF:oValue := ""
		CASE SELF:Type == PropertyType.Type
			SELF:oValue := ""
		CASE SELF:Type == PropertyType.Numeric
			SELF:oValue := 0
		CASE SELF:Type == PropertyType.Enumerated
			SELF:oValue := 0
		CASE SELF:Type == PropertyType.Callback
			SELF:oValue := 0
		END CASE
	RETURN
	
	VIRTUAL ACCESS Value() AS OBJECT
	RETURN SELF:oValue
	VIRTUAL ACCESS ValueLogic() AS LOGIC
		IF SELF:Type == PropertyType.Enumerated
			RETURN (INT)SELF:oValue == 0
		ENDIF
	RETURN FALSE
	VIRTUAL ASSIGN Value(_oValue AS OBJECT)
		DO CASE
		CASE SELF:Type == PropertyType.Numeric
			IF _oValue:GetType() == TypeOf(STRING)
				TRY
					SELF:oValue := Convert.ToInt32((STRING)_oValue)
				END
			ELSE
				SELF:oValue := _oValue
			ENDIF
		CASE SELF:Type == PropertyType.Enumerated
			IF _oValue:GetType() == TypeOf(STRING)
				LOCAL cValue AS STRING
				LOCAL n AS INT
				cValue := ((STRING)_oValue):ToUpper()
				SELF:oValue := 0
				FOR n := 0 UPTO SELF:aEnumTextValues:Count - 1
					IF SELF:aEnumTextValues[n]:ToUpper() == cValue
						IF SELF:lNoAuto
							SELF:oValue := n
						ELSE
							SELF:oValue := n + 1
						ENDIF
						EXIT
					ENDIF
				NEXT
			ELSE
				SELF:oValue := _oValue
			ENDIF
		OTHERWISE
			SELF:oValue := _oValue
		END CASE
	RETURN
	
	VIRTUAL ACCESS TextValue() AS STRING
	RETURN SELF:GetTextValue(SELF:oValue)
	
	VIRTUAL METHOD GetTextValue(oTest AS OBJECT) AS STRING
		LOCAL cRet AS STRING
		LOCAL nValue AS INT
		DO CASE
		CASE SELF:Type == PropertyType.Text
			cRet := oTest:ToString()
		CASE SELF:Type == PropertyType.Type
			cRet := oTest:ToString()
		CASE SELF:Type == PropertyType.Numeric
			cRet := oTest:ToString()
		CASE SELF:Type == PropertyType.Callback
			IF (INT)oTest == 0
				cRet := "Not Defined"
			ELSE
				cRet := "Defined"
			ENDIF
			cRet := "Not supported yet"
		CASE SELF:Type == PropertyType.Enumerated
			IF oTest:GetType() == TypeOf(INT)
				nValue := (INT)oTest
				IF SELF:lNoAuto
					cRet := SELF:aEnumTextValues[nValue]
				ELSE
					IF nValue == 0
						cRet := ""
					ELSE
						cRet := SELF:aEnumTextValues[nValue - 1]
					ENDIF
				ENDIF
			ELSE
				cRet := oTest:ToString()
			END IF
		OTHERWISE
			cRet := ""
		END CASE
	RETURN cRet
	
	VIRTUAL ACCESS IsAuto AS LOGIC
		DO CASE
		CASE SELF:cSpecialClass != NULL .and. SELF:cSpecialClass == "FillUsing"
			IF SELF:oValue:GetType() == TypeOf(FillUsingClass)
				RETURN ((FillUsingClass)SELF:oValue):cValue:Trim() == ""
			ELSE
				RETURN TRUE
			END IF
		CASE SELF:Type == PropertyType.Text
			RETURN (STRING)SELF:oValue == ""
		CASE SELF:Type == PropertyType.Type
			RETURN (STRING)SELF:oValue == ""
		CASE SELF:Type == PropertyType.Numeric
			RETURN (INT)SELF:oValue == 0
		CASE SELF:Type == PropertyType.Enumerated
			IF SELF:lNoAuto
				RETURN FALSE
			ELSE
				IF SELF:oValue:GetType() == TypeOf(INT)
					RETURN (INT)SELF:oValue == 0
				ENDIF
			ENDIF
		END CASE
	RETURN FALSE

	VIRTUAL ACCESS SaveValue AS STRING
		IF SELF:Type == PropertyType.Text
			RETURN e"\"" + SELF:TextValue + e"\""
		ELSE
			RETURN SELF:TextValue
		ENDIF
//	RETURN NULL
	VIRTUAL ACCESS CodeValue AS STRING
	RETURN SELF:TextValue

END CLASS


STATIC CLASS VODefines
	STATIC PRIVATE aDefines AS Dictionary<STRING,DWORD>
	STATIC CONSTRUCTOR()
		VODefines.aDefines := Dictionary<STRING,DWORD>{}

		VODefines.aDefines:Add("WS_OVERLAPPED" , WS_OVERLAPPED)
		VODefines.aDefines:Add("WS_POPUP" , WS_POPUP)
		VODefines.aDefines:Add("WS_CHILD" , WS_CHILD)
		VODefines.aDefines:Add("WS_MINIMIZE" , WS_MINIMIZE)
		VODefines.aDefines:Add("WS_VISIBLE" , WS_VISIBLE)
		VODefines.aDefines:Add("WS_DISABLED" , WS_DISABLED)
		VODefines.aDefines:Add("WS_CLIPSIBLINGS" , WS_CLIPSIBLINGS)
		VODefines.aDefines:Add("WS_CLIPCHILDREN" , WS_CLIPCHILDREN)
		VODefines.aDefines:Add("WS_MAXIMIZE" , WS_MAXIMIZE)
		VODefines.aDefines:Add("WS_CAPTION" , WS_CAPTION)
		VODefines.aDefines:Add("WS_BORDER" , WS_BORDER)
		VODefines.aDefines:Add("WS_DLGFRAME" , WS_DLGFRAME)
		VODefines.aDefines:Add("WS_VSCROLL" , WS_VSCROLL)
		VODefines.aDefines:Add("WS_HSCROLL" , WS_HSCROLL)
		VODefines.aDefines:Add("WS_SYSMENU" , WS_SYSMENU)
		VODefines.aDefines:Add("WS_THICKFRAME" , WS_THICKFRAME)
		VODefines.aDefines:Add("WS_GROUP" , WS_GROUP)
		VODefines.aDefines:Add("WS_TABSTOP" , WS_TABSTOP)
		VODefines.aDefines:Add("WS_MINIMIZEBOX" , WS_MINIMIZEBOX)
		VODefines.aDefines:Add("WS_MAXIMIZEBOX" , WS_MAXIMIZEBOX)
		VODefines.aDefines:Add("WS_TILED" , WS_TILED)
		VODefines.aDefines:Add("WS_ICONIC" , WS_ICONIC)
		VODefines.aDefines:Add("WS_SIZEBOX" , WS_SIZEBOX)
		VODefines.aDefines:Add("WS_TILEDWINDOW" , WS_TILEDWINDOW)
		VODefines.aDefines:Add("WS_OVERLAPPEDWINDOW" , WS_OVERLAPPEDWINDOW)
		VODefines.aDefines:Add("WS_POPUPWINDOW" , WS_POPUPWINDOW)
		VODefines.aDefines:Add("WS_CHILDWINDOW" , WS_CHILDWINDOW)
		VODefines.aDefines:Add("WS_EX_DLGMODALFRAME" , WS_EX_DLGMODALFRAME)
		VODefines.aDefines:Add("WS_EX_NOPARENTNOTIFY" , WS_EX_NOPARENTNOTIFY)
		VODefines.aDefines:Add("WS_EX_TOPMOST" , WS_EX_TOPMOST)
		VODefines.aDefines:Add("WS_EX_ACCEPTFILES" , WS_EX_ACCEPTFILES)
		VODefines.aDefines:Add("WS_EX_TRANSPARENT" , WS_EX_TRANSPARENT)
		VODefines.aDefines:Add("WS_EX_MDICHILD" , WS_EX_MDICHILD)
		VODefines.aDefines:Add("WS_EX_TOOLWINDOW" , WS_EX_TOOLWINDOW)
		VODefines.aDefines:Add("WS_EX_WINDOWEDGE" , WS_EX_WINDOWEDGE)
		VODefines.aDefines:Add("WS_EX_CLIENTEDGE" , WS_EX_CLIENTEDGE)
		VODefines.aDefines:Add("WS_EX_CONTEXTHELP" , WS_EX_CONTEXTHELP)
		VODefines.aDefines:Add("WS_EX_RIGHT" , WS_EX_RIGHT)
		VODefines.aDefines:Add("WS_EX_LEFT" , WS_EX_LEFT)
		VODefines.aDefines:Add("WS_EX_RTLREADING" , WS_EX_RTLREADING)
		VODefines.aDefines:Add("WS_EX_LTRREADING" , WS_EX_LTRREADING)
		VODefines.aDefines:Add("WS_EX_LEFTSCROLLBAR" , WS_EX_LEFTSCROLLBAR)
		VODefines.aDefines:Add("WS_EX_RIGHTSCROLLBAR" , WS_EX_RIGHTSCROLLBAR)
		VODefines.aDefines:Add("WS_EX_CONTROLPARENT" , WS_EX_CONTROLPARENT)
		VODefines.aDefines:Add("WS_EX_STATICEDGE" , WS_EX_STATICEDGE)
		VODefines.aDefines:Add("WS_EX_APPWINDOW" , WS_EX_APPWINDOW)
		VODefines.aDefines:Add("WS_EX_OVERLAPPEDWINDOW" , WS_EX_OVERLAPPEDWINDOW)
		VODefines.aDefines:Add("WS_EX_PALETTEWINDOW" , WS_EX_PALETTEWINDOW)
		VODefines.aDefines:Add("WS_EX_LAYERED" , WS_EX_LAYERED)
		VODefines.aDefines:Add("WS_EX_NOINHERITLAYOUT" , WS_EX_NOINHERITLAYOUT)
		VODefines.aDefines:Add("WS_EX_LAYOUTRTL" , WS_EX_LAYOUTRTL)

		VODefines.aDefines:Add("ES_LEFT" , ES_LEFT)
		VODefines.aDefines:Add("ES_CENTER" , ES_CENTER)
		VODefines.aDefines:Add("ES_RIGHT" , ES_RIGHT)
		VODefines.aDefines:Add("ES_MULTILINE" , ES_MULTILINE)
		VODefines.aDefines:Add("ES_UPPERCASE" , ES_UPPERCASE)
		VODefines.aDefines:Add("ES_LOWERCASE" , ES_LOWERCASE)
		VODefines.aDefines:Add("ES_PASSWORD" , ES_PASSWORD)
		VODefines.aDefines:Add("ES_AUTOHSCROLL" , ES_AUTOHSCROLL)
		VODefines.aDefines:Add("ES_AUTOVSCROLL" , ES_AUTOVSCROLL)
		VODefines.aDefines:Add("ES_NOHIDESEL" , ES_NOHIDESEL)
		VODefines.aDefines:Add("ES_OEMCONVERT" , ES_OEMCONVERT)
		VODefines.aDefines:Add("ES_READONLY" , ES_READONLY)
		VODefines.aDefines:Add("ES_WANTRETURN" , ES_WANTRETURN)
		VODefines.aDefines:Add("ES_NUMBER" , ES_NUMBER)

		VODefines.aDefines:Add("BS_PUSHBUTTON" , BS_PUSHBUTTON)
		VODefines.aDefines:Add("BS_DEFPUSHBUTTON" , BS_DEFPUSHBUTTON)
		VODefines.aDefines:Add("BS_CHECKBOX" , BS_CHECKBOX)
		VODefines.aDefines:Add("BS_AUTOCHECKBOX" , BS_AUTOCHECKBOX)
		VODefines.aDefines:Add("BS_RADIOBUTTON" , BS_RADIOBUTTON)
		VODefines.aDefines:Add("BS_3STATE" , BS_3STATE)
		VODefines.aDefines:Add("BS_AUTO3STATE" , BS_AUTO3STATE)
		VODefines.aDefines:Add("BS_GROUPBOX" , BS_GROUPBOX)
		VODefines.aDefines:Add("BS_USERBUTTON" , BS_USERBUTTON)
		VODefines.aDefines:Add("BS_AUTORADIOBUTTON" , BS_AUTORADIOBUTTON)
		VODefines.aDefines:Add("BS_PUSHBOX" , BS_PUSHBOX)
		VODefines.aDefines:Add("BS_OWNERDRAW" , BS_OWNERDRAW)
		VODefines.aDefines:Add("BS_TYPEMASK" , BS_TYPEMASK)
		VODefines.aDefines:Add("BS_LEFTTEXT" , BS_LEFTTEXT)
		VODefines.aDefines:Add("BS_TEXT" , BS_TEXT)
		VODefines.aDefines:Add("BS_ICON" , BS_ICON)
		VODefines.aDefines:Add("BS_BITMAP" , BS_BITMAP)
		VODefines.aDefines:Add("BS_LEFT" , BS_LEFT)
		VODefines.aDefines:Add("BS_RIGHT" , BS_RIGHT)
		VODefines.aDefines:Add("BS_CENTER" , BS_CENTER)
		VODefines.aDefines:Add("BS_TOP" , BS_TOP)
		VODefines.aDefines:Add("BS_BOTTOM" , BS_BOTTOM)
		VODefines.aDefines:Add("BS_VCENTER" , BS_VCENTER)
		VODefines.aDefines:Add("BS_PUSHLIKE" , BS_PUSHLIKE)
		VODefines.aDefines:Add("BS_MULTILINE" , BS_MULTILINE)
		VODefines.aDefines:Add("BS_NOTIFY" , BS_NOTIFY)
		VODefines.aDefines:Add("BS_FLAT" , BS_FLAT)
		VODefines.aDefines:Add("BS_RIGHTBUTTON" , BS_RIGHTBUTTON)

		VODefines.aDefines:Add("SS_LEFT" , SS_LEFT)
		VODefines.aDefines:Add("SS_CENTER" , SS_CENTER)
		VODefines.aDefines:Add("SS_RIGHT" , SS_RIGHT)
		VODefines.aDefines:Add("SS_ICON" , SS_ICON)
		VODefines.aDefines:Add("SS_BLACKRECT" , SS_BLACKRECT)
		VODefines.aDefines:Add("SS_GRAYRECT" , SS_GRAYRECT)
		VODefines.aDefines:Add("SS_WHITERECT" , SS_WHITERECT)
		VODefines.aDefines:Add("SS_BLACKFRAME" , SS_BLACKFRAME)
		VODefines.aDefines:Add("SS_GRAYFRAME" , SS_GRAYFRAME)
		VODefines.aDefines:Add("SS_WHITEFRAME" , SS_WHITEFRAME)
		VODefines.aDefines:Add("SS_USERITEM" , SS_USERITEM)
		VODefines.aDefines:Add("SS_SIMPLE" , SS_SIMPLE)
		VODefines.aDefines:Add("SS_LEFTNOWORDWRAP" , SS_LEFTNOWORDWRAP)
		VODefines.aDefines:Add("SS_OWNERDRAW" , SS_OWNERDRAW)
		VODefines.aDefines:Add("SS_BITMAP" , SS_BITMAP)
		VODefines.aDefines:Add("SS_ENHMETAFILE" , SS_ENHMETAFILE)
		VODefines.aDefines:Add("SS_ETCHEDHORZ" , SS_ETCHEDHORZ)
		VODefines.aDefines:Add("SS_ETCHEDVERT" , SS_ETCHEDVERT)
		VODefines.aDefines:Add("SS_ETCHEDFRAME" , SS_ETCHEDFRAME)
		VODefines.aDefines:Add("SS_TYPEMASK" , SS_TYPEMASK)
		VODefines.aDefines:Add("SS_REALSIZECONTROL" , SS_REALSIZECONTROL)
		VODefines.aDefines:Add("SS_NOPREFIX" , SS_NOPREFIX)
		VODefines.aDefines:Add("SS_NOTIFY" , SS_NOTIFY)
		VODefines.aDefines:Add("SS_CENTERIMAGE" , SS_CENTERIMAGE)
		VODefines.aDefines:Add("SS_RIGHTJUST" , SS_RIGHTJUST)
		VODefines.aDefines:Add("SS_REALSIZEIMAGE" , SS_REALSIZEIMAGE)
		VODefines.aDefines:Add("SS_SUNKEN" , SS_SUNKEN)
		VODefines.aDefines:Add("SS_EDITCONTROL" , SS_EDITCONTROL)
		VODefines.aDefines:Add("SS_ENDELLIPSIS" , SS_ENDELLIPSIS)
		VODefines.aDefines:Add("SS_PATHELLIPSIS" , SS_PATHELLIPSIS)
		VODefines.aDefines:Add("SS_WORDELLIPSIS" , SS_WORDELLIPSIS)
		VODefines.aDefines:Add("SS_ELLIPSISMASK" , SS_ELLIPSISMASK)

		VODefines.aDefines:Add("DS_ABSALIGN" , DS_ABSALIGN)
		VODefines.aDefines:Add("DS_SYSMODAL" , DS_SYSMODAL)
		VODefines.aDefines:Add("DS_LOCALEDIT" , DS_LOCALEDIT)
		VODefines.aDefines:Add("DS_SETFONT" , DS_SETFONT)
		VODefines.aDefines:Add("DS_MODALFRAME" , DS_MODALFRAME)
		VODefines.aDefines:Add("DS_NOIDLEMSG" , DS_NOIDLEMSG)
		VODefines.aDefines:Add("DS_SETFOREGROUND" , DS_SETFOREGROUND)
		VODefines.aDefines:Add("DS_3DLOOK" , DS_3DLOOK)
		VODefines.aDefines:Add("DS_FIXEDSYS" , DS_FIXEDSYS)
		VODefines.aDefines:Add("DS_NOFAILCREATE" , DS_NOFAILCREATE)
		VODefines.aDefines:Add("DS_CONTROL" , DS_CONTROL)
		VODefines.aDefines:Add("DS_CENTER" , DS_CENTER)
		VODefines.aDefines:Add("DS_CENTERMOUSE" , DS_CENTERMOUSE)
		VODefines.aDefines:Add("DS_CONTEXTHELP" , DS_CONTEXTHELP)
		VODefines.aDefines:Add("DS_SHELLFONT" , DS_SHELLFONT)
		VODefines.aDefines:Add("DS_USEPIXELS" , DS_USEPIXELS)

		VODefines.aDefines:Add("LBS_NOTIFY" , LBS_NOTIFY)
		VODefines.aDefines:Add("LBS_SORT" , LBS_SORT)
		VODefines.aDefines:Add("LBS_NOREDRAW" , LBS_NOREDRAW)
		VODefines.aDefines:Add("LBS_MULTIPLESEL" , LBS_MULTIPLESEL)
		VODefines.aDefines:Add("LBS_OWNERDRAWFIXED" , LBS_OWNERDRAWFIXED)
		VODefines.aDefines:Add("LBS_OWNERDRAWVARIABLE" , LBS_OWNERDRAWVARIABLE)
		VODefines.aDefines:Add("LBS_HASSTRINGS" , LBS_HASSTRINGS)
		VODefines.aDefines:Add("LBS_USETABSTOPS" , LBS_USETABSTOPS)
		VODefines.aDefines:Add("LBS_NOINTEGRALHEIGHT" , LBS_NOINTEGRALHEIGHT)
		VODefines.aDefines:Add("LBS_MULTICOLUMN" , LBS_MULTICOLUMN)
		VODefines.aDefines:Add("LBS_WANTKEYBOARDINPUT" , LBS_WANTKEYBOARDINPUT)
		VODefines.aDefines:Add("LBS_EXTENDEDSEL" , LBS_EXTENDEDSEL)
		VODefines.aDefines:Add("LBS_DISABLENOSCROLL" , LBS_DISABLENOSCROLL)
		VODefines.aDefines:Add("LBS_NODATA" , LBS_NODATA)
		VODefines.aDefines:Add("LBS_NOSEL" , LBS_NOSEL)
		VODefines.aDefines:Add("LBS_COMBOBOX" , LBS_COMBOBOX)
		VODefines.aDefines:Add("LBS_STANDARD" , LBS_STANDARD)

		VODefines.aDefines:Add("CBS_SIMPLE" , CBS_SIMPLE)
		VODefines.aDefines:Add("CBS_DROPDOWN" , CBS_DROPDOWN)
		VODefines.aDefines:Add("CBS_DROPDOWNLIST" , CBS_DROPDOWNLIST)
		VODefines.aDefines:Add("CBS_OWNERDRAWFIXED" , CBS_OWNERDRAWFIXED)
		VODefines.aDefines:Add("CBS_OWNERDRAWVARIABLE" , CBS_OWNERDRAWVARIABLE)
		VODefines.aDefines:Add("CBS_AUTOHSCROLL" , CBS_AUTOHSCROLL)
		VODefines.aDefines:Add("CBS_OEMCONVERT" , CBS_OEMCONVERT)
		VODefines.aDefines:Add("CBS_SORT" , CBS_SORT)
		VODefines.aDefines:Add("CBS_HASSTRINGS" , CBS_HASSTRINGS)
		VODefines.aDefines:Add("CBS_NOINTEGRALHEIGHT" , CBS_NOINTEGRALHEIGHT)
		VODefines.aDefines:Add("CBS_DISABLENOSCROLL" , CBS_DISABLENOSCROLL)
		VODefines.aDefines:Add("CBS_UPPERCASE" , CBS_UPPERCASE)
		VODefines.aDefines:Add("CBS_LOWERCASE" , CBS_LOWERCASE)

		VODefines.aDefines:Add("SBS_HORZ" , SBS_HORZ)
		VODefines.aDefines:Add("SBS_VERT" , SBS_VERT)
		VODefines.aDefines:Add("SBS_TOPALIGN" , SBS_TOPALIGN)
		VODefines.aDefines:Add("SBS_LEFTALIGN" , SBS_LEFTALIGN)
		VODefines.aDefines:Add("SBS_BOTTOMALIGN" , SBS_BOTTOMALIGN)
		VODefines.aDefines:Add("SBS_RIGHTALIGN" , SBS_RIGHTALIGN)
		VODefines.aDefines:Add("SBS_SIZEBOXTOPLEFTALIGN" , SBS_SIZEBOXTOPLEFTALIGN)
		VODefines.aDefines:Add("SBS_SIZEBOXBOTTOMRIGHTALIGN" , SBS_SIZEBOXBOTTOMRIGHTALIGN)
		VODefines.aDefines:Add("SBS_SIZEBOX" , SBS_SIZEBOX)
		VODefines.aDefines:Add("SBS_SIZEGRIP" , SBS_SIZEGRIP)

		VODefines.aDefines:Add("TBS_AUTOTICKS" , TBS_AUTOTICKS)
		VODefines.aDefines:Add("TBS_VERT" , TBS_VERT)
		VODefines.aDefines:Add("TBS_HORZ" , TBS_HORZ)
		VODefines.aDefines:Add("TBS_TOP" , TBS_TOP)
		VODefines.aDefines:Add("TBS_BOTTOM" , TBS_BOTTOM)
		VODefines.aDefines:Add("TBS_LEFT" , TBS_LEFT)
		VODefines.aDefines:Add("TBS_RIGHT" , TBS_RIGHT)
		VODefines.aDefines:Add("TBS_BOTH" , TBS_BOTH)
		VODefines.aDefines:Add("TBS_NOTICKS" , TBS_NOTICKS)
		VODefines.aDefines:Add("TBS_ENABLESELRANGE" , TBS_ENABLESELRANGE)
		VODefines.aDefines:Add("TBS_FIXEDLENGTH" , TBS_FIXEDLENGTH)
		VODefines.aDefines:Add("TBS_NOTHUMB" , TBS_NOTHUMB)
		VODefines.aDefines:Add("TBS_TOOLTIPS" , TBS_TOOLTIPS)

		VODefines.aDefines:Add("UDS_WRAP" , UDS_WRAP)
		VODefines.aDefines:Add("UDS_SETBUDDYINT" , UDS_SETBUDDYINT)
		VODefines.aDefines:Add("UDS_ALIGNRIGHT" , UDS_ALIGNRIGHT)
		VODefines.aDefines:Add("UDS_ALIGNLEFT" , UDS_ALIGNLEFT)
		VODefines.aDefines:Add("UDS_AUTOBUDDY" , UDS_AUTOBUDDY)
		VODefines.aDefines:Add("UDS_ARROWKEYS" , UDS_ARROWKEYS)
		VODefines.aDefines:Add("UDS_HORZ" , UDS_HORZ)
		VODefines.aDefines:Add("UDS_NOTHOUSANDS" , UDS_NOTHOUSANDS)

		VODefines.aDefines:Add("PBS_SMOOTH" , PBS_SMOOTH)
		VODefines.aDefines:Add("PBS_VERTICAL" , PBS_VERTICAL)

		VODefines.aDefines:Add("LVS_ICON" , LVS_ICON)
		VODefines.aDefines:Add("LVS_REPORT" , LVS_REPORT)
		VODefines.aDefines:Add("LVS_SMALLICON" , LVS_SMALLICON)
		VODefines.aDefines:Add("LVS_LIST" , LVS_LIST)
		VODefines.aDefines:Add("LVS_TYPEMASK" , LVS_TYPEMASK)
		VODefines.aDefines:Add("LVS_SINGLESEL" , LVS_SINGLESEL)
		VODefines.aDefines:Add("LVS_SHOWSELALWAYS" , LVS_SHOWSELALWAYS)
		VODefines.aDefines:Add("LVS_SORTASCENDING" , LVS_SORTASCENDING)
		VODefines.aDefines:Add("LVS_SORTDESCENDING" , LVS_SORTDESCENDING)
		VODefines.aDefines:Add("LVS_SHAREIMAGELISTS" , LVS_SHAREIMAGELISTS)
		VODefines.aDefines:Add("LVS_NOLABELWRAP" , LVS_NOLABELWRAP)
		VODefines.aDefines:Add("LVS_AUTOARRANGE" , LVS_AUTOARRANGE)
		VODefines.aDefines:Add("LVS_EDITLABELS" , LVS_EDITLABELS)
		VODefines.aDefines:Add("LVS_OWNERDATA" , LVS_OWNERDATA)
		VODefines.aDefines:Add("LVS_NOSCROLL" , LVS_NOSCROLL)
		VODefines.aDefines:Add("LVS_TYPESTYLEMASK" , LVS_TYPESTYLEMASK)
		VODefines.aDefines:Add("LVS_ALIGNTOP" , LVS_ALIGNTOP)
		VODefines.aDefines:Add("LVS_ALIGNLEFT" , LVS_ALIGNLEFT)
		VODefines.aDefines:Add("LVS_ALIGNMASK" , LVS_ALIGNMASK)
		VODefines.aDefines:Add("LVS_OWNERDRAWFIXED" , LVS_OWNERDRAWFIXED)
		VODefines.aDefines:Add("LVS_NOCOLUMNHEADER" , LVS_NOCOLUMNHEADER)
		VODefines.aDefines:Add("LVS_NOSORTHEADER" , LVS_NOSORTHEADER)

		VODefines.aDefines:Add("TVS_HASBUTTONS" , TVS_HASBUTTONS)
		VODefines.aDefines:Add("TVS_HASLINES" , TVS_HASLINES)
		VODefines.aDefines:Add("TVS_LINESATROOT" , TVS_LINESATROOT)
		VODefines.aDefines:Add("TVS_EDITLABELS" , TVS_EDITLABELS)
		VODefines.aDefines:Add("TVS_DISABLEDRAGDROP" , TVS_DISABLEDRAGDROP)
		VODefines.aDefines:Add("TVS_SHOWSELALWAYS" , TVS_SHOWSELALWAYS)

		VODefines.aDefines:Add("TCS_SCROLLOPPOSITE" , TCS_SCROLLOPPOSITE)
		VODefines.aDefines:Add("TCS_BOTTOM" , TCS_BOTTOM)
		VODefines.aDefines:Add("TCS_RIGHT" , TCS_RIGHT)
		VODefines.aDefines:Add("TCS_MULTISELECT" , TCS_MULTISELECT)
		VODefines.aDefines:Add("TCS_FORCEICONLEFT" , TCS_FORCEICONLEFT)
		VODefines.aDefines:Add("TCS_FORCELABELLEFT" , TCS_FORCELABELLEFT)
		VODefines.aDefines:Add("TCS_HOTTRACK" , TCS_HOTTRACK)
		VODefines.aDefines:Add("TCS_VERTICAL" , TCS_VERTICAL)
		VODefines.aDefines:Add("TCS_TABS" , TCS_TABS)
		VODefines.aDefines:Add("TCS_BUTTONS" , TCS_BUTTONS)
		VODefines.aDefines:Add("TCS_SINGLELINE" , TCS_SINGLELINE)
		VODefines.aDefines:Add("TCS_MULTILINE" , TCS_MULTILINE)
		VODefines.aDefines:Add("TCS_RIGHTJUSTIFY" , TCS_RIGHTJUSTIFY)
		VODefines.aDefines:Add("TCS_FIXEDWIDTH" , TCS_FIXEDWIDTH)
		VODefines.aDefines:Add("TCS_RAGGEDRIGHT" , TCS_RAGGEDRIGHT)
		VODefines.aDefines:Add("TCS_FOCUSONBUTTONDOWN" , TCS_FOCUSONBUTTONDOWN)
		VODefines.aDefines:Add("TCS_OWNERDRAWFIXED" , TCS_OWNERDRAWFIXED)
		VODefines.aDefines:Add("TCS_TOOLTIPS" , TCS_TOOLTIPS)
		VODefines.aDefines:Add("TCS_FOCUSNEVER" , TCS_FOCUSNEVER)

		VODefines.aDefines:Add("ACS_CENTER" , ACS_CENTER)
		VODefines.aDefines:Add("ACS_TRANSPARENT" , ACS_TRANSPARENT)
		VODefines.aDefines:Add("ACS_AUTOPLAY" , ACS_AUTOPLAY)

		VODefines.aDefines:Add("MCS_DAYSTATE" , MCS_DAYSTATE)
		VODefines.aDefines:Add("MCS_MULTISELECT" , MCS_MULTISELECT)
		VODefines.aDefines:Add("MCS_WEEKNUMBERS" , MCS_WEEKNUMBERS)
		VODefines.aDefines:Add("MCS_NOTODAYCIRCLE" , MCS_NOTODAYCIRCLE)
		VODefines.aDefines:Add("MCS_NOTODAY" , MCS_NOTODAY)

		VODefines.aDefines:Add("DTS_UPDOWN" , DTS_UPDOWN)
		VODefines.aDefines:Add("DTS_SHOWNONE" , DTS_SHOWNONE)
		VODefines.aDefines:Add("DTS_SHORTDATEFORMAT" , DTS_SHORTDATEFORMAT)
		VODefines.aDefines:Add("DTS_LONGDATEFORMAT" , DTS_LONGDATEFORMAT)
		VODefines.aDefines:Add("DTS_TIMEFORMAT" , DTS_TIMEFORMAT)
		VODefines.aDefines:Add("DTS_APPCANPARSE" , DTS_APPCANPARSE)
		VODefines.aDefines:Add("DTS_RIGHTALIGN" , DTS_RIGHTALIGN)

		VODefines.aDefines:Add("ES_SAVESEL" , ES_SAVESEL)
		VODefines.aDefines:Add("ES_SUNKEN" , ES_SUNKEN)
		VODefines.aDefines:Add("ES_DISABLENOSCROLL" , ES_DISABLENOSCROLL)
		VODefines.aDefines:Add("ES_SELECTIONBAR" , ES_SELECTIONBAR)
		VODefines.aDefines:Add("ES_NOOLEDRAGDROP" , ES_NOOLEDRAGDROP)
		VODefines.aDefines:Add("ES_EX_NOCALLOLEINIT" , ES_EX_NOCALLOLEINIT)
		VODefines.aDefines:Add("ES_VERTICAL" , ES_VERTICAL)
		VODefines.aDefines:Add("ES_NOIME" , ES_NOIME)
		VODefines.aDefines:Add("ES_SELFIME" , ES_SELFIME)

	RETURN
	STATIC METHOD GetDefineValue(cDefine AS STRING) AS DWORD
		LOCAL dValue AS DWORD
		IF cDefine:Length == 0 .or. cDefine == "WS_NULL"
			RETURN 0
		ENDIF
		IF cDefine:IndexOf('|') == -1
//			TRY
				IF VODefines.aDefines:ContainsKey(cDefine)
					dValue := (DWORD)VODefines.aDefines[cDefine]
				ELSE
					dValue := 0
//					Funcs.WarningBox("Style value not found : " + cDefine , Resources.EditorName)
				ENDIF
//			CATCH
//				TRY
//					dValue := (DWORD)(INT)VODefines.aDefines[cDefine]
//				END
//			END
		ELSE
			LOCAL aSplit AS STRING[]
			LOCAL n AS INT
			aSplit := cDefine:Split('|')
			FOR n := 1 UPTO aSplit:Length
				dValue += VODefines.GetDefineValue(aSplit[n])
			NEXT
		ENDIF
	RETURN dValue
END CLASS
