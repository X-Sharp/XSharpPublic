//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.Collections
USING System.Collections.Generic
USING System.Text
USING System.Runtime.InteropServices

BEGIN NAMESPACE XSharp.RDD.SqlRDD

INTERNAL CLASS IniFile
    PROTECTED cFileName AS STRING
    PROPERTY FullName AS STRING GET cFileName
    CONSTRUCTOR(cFile AS STRING)
        cFileName := cFile
        RETURN

    METHOD Create() AS LOGIC STRICT
        TRY
            System.IO.File.WriteAllText(cFileName,"")
        CATCH
            RETURN FALSE
        END TRY
        RETURN TRUE

    METHOD DeleteEntry(sSection AS STRING,sEntry AS STRING) AS LOGIC
        SELF:WriteString( sSection, sEntry, NULL)
        RETURN TRUE

    METHOD DeleteSection(sSection AS STRING) AS LOGIC
        SELF:WriteString( sSection, NULL, NULL )
        RETURN TRUE

    METHOD GetInt(sSection AS STRING,sEntry AS STRING,nDefault AS LONGINT) AS LONGINT
        VAR sValue := GetString(sSection, sEntry, "")
        VAR nValue := nDefault
        IF ! String.IsNullOrEmpty(sValue)
            IF ! Int32.TryParse(sValue, OUT nValue)
                nValue := nDefault
            ENDIF
        ENDIF
        RETURN nValue

    METHOD GetLogic(sSection AS STRING,sEntry AS STRING,lDefault AS LOGIC) AS LOGIC
        VAR sValue := SELF:GetStringUpper(sSection, sEntry, "")
        VAR lResult := lDefault
        IF !String.IsNullOrEmpty(sValue)
            SWITCH sValue
            CASE "ON"
            CASE "TRUE"
            CASE "YES"
                lResult := TRUE
            OTHERWISE
                if (Int32.TryParse(sValue, out var intValue))
                    lResult := intValue != 0
                else
                    lResult := FALSE
                endif
            END SWITCH
        ENDIF
        RETURN lResult

    METHOD GetSection(sSection AS STRING) AS IList<STRING>
        VAR result := CHAR[]{4096}
        VAR done := FALSE
        LOCAL nSize := 0 AS LONG
        DO WHILE ! done
            nSize := GetPrivateProfileSection(sSection, @result, result:Length, cFileName)
            IF nSize >= result:Length
                // resize the buffer
                result := CHAR[]{result:Length*2}
            ELSE
                done := TRUE
            ENDIF
        ENDDO
        VAR sValues := STRING{result,0, nSize}
        VAR aElements := sValues:Split( c'\0')
        RETURN aElements

    METHOD GetString(sSection AS STRING,sEntry AS STRING,sDefault AS STRING) AS STRING
        VAR result := StringBuilder{255}
        VAR done := FALSE
        DO WHILE ! done
            VAR nSize := GetPrivateProfileString(sSection, sEntry, "", result, result:Capacity, cFileName)
            IF nSize >= result:Capacity -1
                // resize the buffer
                result := StringBuilder{result:Capacity*2}
            ELSE
                done := TRUE
            ENDIF
        ENDDO
        RETURN result:ToString()

    METHOD GetStringUpper(sSection AS STRING,sEntry AS STRING,sDefault AS STRING) AS STRING
        RETURN GetString(sSection, sEntry, sDefault):ToUpper()


    METHOD WriteInt(sSection AS STRING,sEntry AS STRING,nValue AS LONGINT) AS LOGIC
        RETURN WritePrivateProfileString(sSection, sEntry, nValue:ToString(), cFileName)

    METHOD WriteString(sSection AS STRING,sEntry AS STRING,sString AS STRING) AS LOGIC
        RETURN WritePrivateProfileString(sSection, sEntry, sString, cFileName)

#region external methods

    [DllImport("kernel32", CharSet := CharSet.Unicode)];
    STATIC METHOD WritePrivateProfileString(section AS STRING, Key AS STRING, strValue AS STRING, FileName AS STRING) AS LOGIC

    [DllImport("kernel32", CharSet := CharSet.Unicode)];
    STATIC METHOD GetPrivateProfileString(section AS STRING, Key AS STRING, strDefault AS STRING, retval AS StringBuilder, nSize AS LONG, FileName AS STRING) AS LONG

    [DllImport("kernel32", CharSet := CharSet.Unicode)];
    STATIC METHOD GetPrivateProfileSection(section AS STRING, retval AS CHAR PTR, nSize AS LONG, FileName AS STRING) AS LONG

#endregion
END CLASS
END NAMESPACE // XSharp.RDD.SqlRDD
