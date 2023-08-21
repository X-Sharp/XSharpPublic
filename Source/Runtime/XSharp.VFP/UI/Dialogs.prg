//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


using Microsoft.Win32
using System.Drawing
using System.Reflection
using System.Windows.Forms

/// <include file="VFPDocs.xml" path="Runtimefunctions/getcolor/*" />
FUNCTION GetColor( nDefaultColorNumber ) AS INT CLIPPER
    LOCAL hRegKey AS RegistryKey
    LOCAL iColorref := -1 AS INT
    LOCAL cRegKeyName := "Software\Microsoft\VisualFoxPro\9.0\ChooseColor" AS STRING
    IF ! IsNumeric(nDefaultColorNumber)
        nDefaultColorNumber := 0
    ENDIF
    VAR iCustomColorsArr := INT[]{16} // contains the colorrefs of the sixteen user defined colors


    IF RuntimeState.RunningOnWindows

        IF ( hRegKey := Registry.CurrentUser:OpenSubKey( cRegKeyName , TRUE )) != NULL

            // fills iCustomColorsArr with values read from the registry.
            iCustomColorsArr := __ReadColors ( hRegKey )

            hRegKey:Close()

        ELSE

            hRegKey := Registry.CurrentUser:CreateSubKey(cRegKeyName )

            // the default values are sixteen WHITE colorrefs
            var white        := 16777215
            iCustomColorsArr :=	<INT> { white, white, white, white, white, white , white, white, ;
                white, white, white, white, white, white , white, white}

            // translates the iCustomColorsArr content to a byte[]{64} array. The content of the
            // byte array is written to the registry.
            __WriteColors ( hRegKey , iCustomColorsArr )

            hRegKey:Close()

        ENDIF

    ENDIF


    VAR oDlg := ColorDialog{}
    oDlg:Color := ColorTranslator.FromWin32(nDefaultColorNumber)
    IF RuntimeState.RunningOnWindows
        oDlg:CustomColors := iCustomColorsArr
    ENDIF

    IF oDlg:ShowDialog() == DialogResult.OK

        iColorref := ColorTranslator.ToWin32(oDlg:Color)


        IF RuntimeState.RunningOnWindows

            IF ( hRegKey := Registry.CurrentUser:OpenSubKey( cRegKeyName , TRUE )) != NULL

                // translates the content of the "oDlg:CustomColors" array to a byte[]{64} array. The content of the
                // byte array is written to the registry.
                __WriteColors ( hRegKey , oDlg:CustomColors )

                hRegKey:Close()

            ENDIF

        ENDIF

    ENDIF
    #region Local Functions

    LOCAL FUNCTION __WriteColors ( hSubKey AS RegistryKey , iCustomColorsArr AS INT[] ) AS VOID
        LOCAL i, j AS INT
        VAR bRegistryArr := BYTE[]{iCustomColorsArr:Length * 4}

        j := 1

        FOR i := 1 TO iCustomColorsArr:Length

            VAR oColor := ColorTranslator.FromWin32(iCustomColorsArr [i])

            bRegistryArr[j] := oColor:R
            bRegistryArr[j+1] := oColor:G
            bRegistryArr[j+2] := oColor:B

            //		?  bRegistryArr[j] , bRegistryArr[j+1] , bRegistryArr[j+2]

            j := j + 4

        NEXT

        hSubKey:SetValue ( "" , bRegistryArr , RegistryValueKind.Binary)

        RETURN
    END FUNCTION

    LOCAL FUNCTION __ReadColors ( hSubKey AS RegistryKey) AS INT[]
        LOCAL i, j AS INT
        LOCAL iCustomColorsArr AS INT[]
        VAR bRegistryArr := (BYTE[]) hSubKey:GetValue("")
        IF bRegistryArr != NULL
            j := 1
            iCustomColorsArr :=INT[]{bRegistryArr:Length / 4}
            FOR i := 1 TO bRegistryArr:Length STEP 4

                VAR oColor := Color.FromArgb ( 0 , bRegistryArr[i] , bRegistryArr[i+1] , bRegistryArr[i+2] )

                iCustomColorsArr[j] := ColorTranslator.ToWin32(oColor)
                //		? bRegistryArr[i] , bRegistryArr[i+1] , bRegistryArr[i+2] , iCustomColorsArr[j]
                j ++

            NEXT
        ELSE
            iCustomColorsArr :=INT[]{0}
        ENDIF
        RETURN iCustomColorsArr
    END FUNCTION
#endregion
RETURN iColorref



/// <include file="VFPDocs.xml" path="Runtimefunctions/getfont/*" />
FUNCTION GetFont(cFontName,nFontSize,cFontStyle,nFontCharSet) AS STRING CLIPPER
LOCAL oDlg AS FontDialog
LOCAL cSelectedFont := "" AS STRING
LOCAL eFontstyle AS FontStyle
LOCAL lReturnCharSet AS LOGIC
LOCAL oMi AS MethodInfo
LOCAL iParamCount AS INT


	iParamCount := PCount()

	IF (iParamCount == 1 .and. ! IsString ( cFontName )) .or. ;
		( IsString ( cFontName ) .and. String.IsNullOrEmpty(cFontName) )
		THROW ArgumentException {"parameter value must not be empty.", Nameof ( cFontName ) }

	ELSEIF iParamCount == 2 .and. ( IsNil ( cFontName) .or. IsNil ( nFontSize) )
		THROW ArgumentException {"two parameters must be passed." }

	ELSEIF iParamCount == 3 .and. ( IsNil ( nFontSize) .or. IsNil ( cFontStyle) )
		THROW ArgumentException {"three parameters must be passed." }

	ELSEIF iParamCount == 4 .and. ( IsNil ( nFontSize) .or. IsNil ( cFontStyle) .or. IsNil ( nFontCharSet ) )
		THROW ArgumentException {"four parameters must be passed." }

	ELSEIF iParamCount > 4
		THROW ArgumentException {"too many parameters." }

	ELSEIF ! IsNil( nFontSize  ) .and. ! IsNumeric ( nFontSize )
		THROW ArgumentException {"wrong parameter type, must be a numeric.", Nameof ( nFontSize ) }

	ELSEIF ! IsNil( cFontStyle  ) .and. ! IsString ( cFontStyle )
		THROW ArgumentException {"wrong parameter type, must be a string.", Nameof ( cFontStyle ) }

	ELSEIF ! IsNil( nFontCharSet  ) .and. ! IsNumeric ( nFontCharSet )
		THROW ArgumentException {"wrong parameter type, must be a numeric.", Nameof ( nFontCharSet ) }

	ENDIF

	// --------------

	oDlg := FontDialog{}

	oMi := TypeOf(FontDialog):GetMethod("SetOption", BindingFlags.NonPublic | BindingFlags.Instance)

	IF oMi != NULL

		// this enables the font description that is shown in the left/bottom area of the VFP Fontdialog
		oMi:Invoke(oDlg, { CF_PRINTERFONTS, TRUE })

		// btw. Its possible to include fonts like Courier, FixedSys, Terminal
		// But since the .net FontDialog supports true type fonts only, selecting e.g. FixedSys
		// would trigger the error "only true type fonts are supported"!
		//
		// oMI:Invoke(oDlg, { 0x00040000L , FALSE  }) // CF_TTONLY

    ENDIF

 	oDlg:ShowEffects := FALSE
	oDlg:ShowHelp := TRUE
	oDlg:MinSize := 4
	oDlg:MaxSize := 127


	lReturnCharSet := IIF ( IsNil ( nFontCharSet ) , FALSE , TRUE )

	IF oMi != NULL .and. ! lReturnCharSet
		// this disables the script combobox
		oMi:Invoke(oDlg, { CF_NOSCRIPTSEL, TRUE })
	ENDIF


	IF iParamCount > 0

		IF IsNil (nFontSize) .or. nFontSize < 1
			nFontSize := 10  // VFP default
		ENDIF

		IF IsNil ( cFontStyle )
			eFontstyle := FontStyle.Regular  // VFP default
		ELSE
			eFontstyle := StringToFontStyle( ((STRING) cFontStyle):ToUpper())
		ENDIF

		IF IsNil ( nFontCharSet )
			nFontCharSet := 1 // VFP default
		ENDIF

 		oDlg:Font := System.Drawing.Font{cFontName,nFontSize,eFontstyle , GraphicsUnit.Point , nFontCharSet }

	ENDIF


	IF oDlg:ShowDialog() == DialogResult.OK

		cSelectedFont := oDlg:Font:Name +"," + ;
			((INT) System.Math.Round(oDlg:Font:SizeInPoints,MidpointRounding.AwayFromZero)):ToString()+"," +;
			FontStyleToString (oDlg:Font:Style) + ;
			iif ( lReturnCharSet , "," + oDlg:Font:GdiCharSet:ToString() , "" )

	ENDIF

	//RETURN cSelectedFont  // https://github.com/X-Sharp/XSharpPublic/issues/1017

	// --------------------

	LOCAL FUNCTION StringToFontStyle ( c AS STRING ) AS FontStyle
	LOCAL eStyle := FontStyle.Regular AS FontStyle

	IF c:IndexOfAny( <char>{c'B',c'b'} ) > -1
		eStyle += FontStyle.Bold
	ENDIF

	IF c:IndexOfAny( <char>{c'I',c'i'} ) > -1
		eStyle += FontStyle.Italic
	ENDIF

	RETURN eStyle

	END FUNCTION

	// ------------

	LOCAL FUNCTION FontStyleToString ( eStyle AS FontStyle ) AS STRING
	LOCAL cStyle := "" AS STRING

	IF eStyle == FontStyle.Regular
		cStyle := "N"
	ELSE

		IF  eStyle:HasFlag(FontStyle.Bold )
			cStyle += "B"
		ENDIF

		IF eStyle:HasFlag(FontStyle.Italic )
			cStyle += "I"
		ENDIF

	ENDIF

	RETURN cStyle

	END FUNCTION

	// -----------

	RETURN cSelectedFont // https://github.com/X-Sharp/XSharpPublic/issues/1017

END FUNCTION

INTERNAL DEFINE CF_NOSCRIPTSEL := 0x00800000L
INTERNAL DEFINE CF_PRINTERFONTS := 0x00000002L



