//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.Reflection
USING System.IO
USING System.Text

BEGIN NAMESPACE XSharp.VFP

    PUBLIC INTERFACE IVfpUIProvider
        METHOD ShowMessageBox(cMessage AS STRING, nDialogBoxType AS LONG, cTitleBarText AS STRING, nTimeOut AS LONG) AS LONG
        METHOD SysMetric(nScreenElement AS LONG) AS LONG
        METHOD ShowAssertDialog(cExpression AS STRING, cMessage AS STRING) AS AssertResult
        METHOD GetColor(nDefaultColorNumber AS USUAL) AS INT
        METHOD GetFont(cFontName AS USUAL, nFontSize AS USUAL, cFontStyle AS USUAL, nFontCharSet AS USUAL) AS STRING
        METHOD GetPrinters(nValue AS INT) AS ARRAY
    END INTERFACE

    PUBLIC STATIC CLASS VfpUIService
        STATIC PRIVATE _provider AS IVfpUIProvider

        STATIC PUBLIC PROPERTY Provider AS IVfpUIProvider
            GET
                IF _provider == NULL
                    LoadProvider()
                ENDIF
                RETURN _provider
            END GET
        END PROPERTY

        STATIC PRIVATE METHOD LoadProvider() AS VOID
            // Exclude non interactive solutions (services/IIS)
            IF !Environment.UserInteractive
                _provider := HeadlessUIProvider{}
                RETURN
            ENDIF

            TRY
                LOCAL cDllPath AS STRING
                cDllPath := Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly():Location), "XSharp.VFP.UI.dll")

                IF File.Exists(cDllPath)
                    VAR oAsm := Assembly.LoadFrom(cDllPath)
                    VAR oType := oAsm:GetType("XSharp.VFP.UI.VfpUIProvider", FALSE, TRUE)

                    IF oType != NULL AND typeof(IVfpUIProvider):IsAssignableFrom(oType)
                        _provider := (IVfpUIProvider) Activator.CreateInstance(oType)
                    ENDIF
                ENDIF
            CATCH
                _provider := NULL
            END TRY

            IF _provider == NULL
                _provider := HeadlessUIProvider{}
            ENDIF
        END METHOD
    END CLASS

    INTERNAL CLASS HeadlessUIProvider IMPLEMENTS IVfpUIProvider

        PUBLIC METHOD ShowMessageBox(cMessage AS STRING, nDialogBoxType AS LONG, cTitleBarText AS STRING, nTimeOut AS LONG) AS LONG
            Console.WriteLine("MESSAGEBOX: " + cTitleBarText + " - " + cMessage)
            RETURN 1
        END METHOD

        PUBLIC METHOD SysMetric(nScreenElement AS LONG) AS LONG
            // There's no useful metrics on console mode
            RETURN 0
        END METHOD

        PUBLIC METHOD ShowAssertDialog(cExpression AS STRING, cMessage AS STRING) AS AssertResult
            Console.WriteLine("ASSERT FAILED: " + cMessage)
            RETURN AssertResult.Ignore
        END METHOD

        PUBLIC METHOD GetColor(nDefaultColorNumber AS USUAL) AS INT
            RETURN -1
        END METHOD

        PUBLIC METHOD GetFont(cFontName AS USUAL, nFontSize AS USUAL, cFontStyle AS USUAL, nFontCharSet AS USUAL) AS STRING
            RETURN ""
        END METHOD

        PUBLIC METHOD GetPrinters(nValue AS INT) AS ARRAY
            RETURN {}
        END METHOD
    END CLASS

END NAMESPACE
