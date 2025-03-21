// SystemFunctions.prg


USING System
USING System.Collections.Generic
USING System.Text


/// <include file="VFPDocs.xml" path="Runtimefunctions/sys/*" />
FUNCTION Sys(nSetting, uNewValue, uContextParam3) AS USUAL CLIPPER
    LOCAL retVal AS OBJECT
    retVal := FALSE
    //
    SWITCH nSetting

    CASE 0 // Network machine information.
        retVal := __GetEnv("USERDOMAIN") + " # " + __GetEnv("USERNAME")

    CASE 1 // Returns the current system date as a Julian day number character string.
        VAR currentDate := DateTime.Now
        VAR julianDay := currentDate:ToOADate() + 2415018.5
        retVal := ((INT)julianDay):ToString()

    CASE 3
        // Legal File Name
        VAR tempFileName = System.Guid.NewGuid():ToString()
        retVal := tempFileName:Replace("-", "")
        retVal := Left(retVal,8)

    CASE 5 // Default drive or volume.
        retVal := GetDefault()
        IF ( String.IsNullOrEmpty(retVal))
            retVal := CurDrive() + ":"
        ELSE
            // "SET DEFAULT TO" can contain a Path, get the Drive
            retVal := retVal:Substring(0,1) + ":"
        ENDIF

    CASE 16 // Executing program file name.
        // VAR asm := AppDomain.CurrentDomain.BaseDirectory
        // VAR asm := System.Reflection.Assembly.GetExecutingAssembly()
        // VAR path := asm:Location
        // retVal := path
        VAR level := 1
        // Add one, due to Issue #1704
        VAR calling = ProcName(level + 1)
        // Change Colon to Dot, to provide a VFP-Compatible result
        calling = calling:Replace(":", ".")
        // Remove the static FUNCTIONS class from X#
        IF calling:StartsWith("FUNCTIONS.")
            calling = calling:Substring(10)
        ENDIF
        // Where ?
        VAR file = ProcFile(level)
        retVal := "PROCEDURE " + calling + " " + file
        retval := retVal:ToUpper()

    CASE 987 // Map Remote Data to ANSI.
        RETURN FALSE

    CASE 2003 // Current directory.
        VAR strRetVal := Environment.CurrentDirectory
        IF ( strRetVal:IndexOf(":") >= 0 )
            strRetVal := strRetVal:Substring( strRetVal:IndexOf(":")+1 )
        ENDIF
        retVal := strRetVal

    CASE 2015 // Returns a unique 10-character procedure name that begins with an underscore followed by a combination of letters and numbers.
        VAR str := "_" + Sys(3)
        retVal := Left(str,10)

    CASE 2023 // Temporary Path.
        retVal := __GetEnv("TEMP")


    CASE 3050 // Set the foreground or background buffer memory size
        // Try to set a value
        IF !IsNil(uNewValue)
            IF IsNumeric(uContextParam3) .AND. (uContextParam3 > 0 )
                retVal := str( uContextParam3 ) // Bring you back your setting
            ELSE
                SWITCH uNewValue // This what VFP returns on MY computer ;)
                CASE 1
                    retVal := "1466695680"
                CASE 2
                    retVal := "366739456"
                END SWITCH
            ENDIF
        END IF

    OTHERWISE
        THROW NotImplementedException{}
    END SWITCH
    RETURN retVal

INTERNAL FUNCTION __GetEnv(cVariableName AS STRING) AS STRING
    LOCAL cRetVal AS STRING
    cRetVal := Environment.GetEnvironmentVariable(cVariableName)
    RETURN cRetVal
