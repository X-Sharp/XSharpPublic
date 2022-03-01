//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


using Microsoft.Win32
using System.Drawing
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


    IF IsRunningOnWindows()

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
    IF IsRunningOnWindows()
        oDlg:CustomColors := iCustomColorsArr
    ENDIF

    IF oDlg:ShowDialog() == DialogResult.OK

        iColorref := ColorTranslator.ToWin32(oDlg:Color)


        IF IsRunningOnWindows()

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


