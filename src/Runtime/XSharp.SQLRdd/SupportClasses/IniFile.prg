//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System
using System.Collections
using System.Collections.Generic
using System.Text
using System.Runtime.InteropServices

begin namespace XSharp.RDD.SqlRDD
    
    class IniFile
    protected cFileName as string
    property FullName as string get cFileName
    property Exists   as logic get System.IO.File.Exists(cFileName)
    constructor(cFile as string)
        cFileName := cFile
        if File(cFile)
            cFileName := FPathName()
        endif

        return

    method Create() as logic strict
        try
            System.IO.File.WriteAllText(cFileName,"")
        catch
            return false
        end try
        return true

    method DeleteEntry(sSection as string,sEntry as string) as logic
        self:WriteString( sSection, sEntry, null)
        return true

    method DeleteSection(sSection as string) as logic
        self:WriteString( sSection, null, null )
        return true

    method GetInt(sSection as string,sEntry as string,nDefault as longint) as longint
        var sValue := GetString(sSection, sEntry, "")
        var nValue := nDefault
        if ! String.IsNullOrEmpty(sValue)
            if ! Int32.TryParse(sValue, out nValue)
                nValue := nDefault
            endif
        endif
        return nValue

    method GetLogic(sSection as string,sEntry as string,lDefault as logic) as logic
        var sValue := self:GetStringUpper(sSection, sEntry, "")
        var lResult := lDefault
        if !String.IsNullOrEmpty(sValue)
            switch sValue
            case "ON"
            case "TRUE"
            case "YES"
                lResult := true
            otherwise
                if (Int32.TryParse(sValue, out var intValue))
                    lResult := intValue != 0
                else
                    lResult := false
                endif
            end switch
        endif
        return lResult

    method GetSection(sSection as string) as IList<string>
        var result := char[]{4096}
        var done := false
        local nSize := 0 as long
        do while ! done
            nSize := GetPrivateProfileSection(sSection, @result, result:Length, cFileName)
            if nSize >= result:Length
                // resize the buffer
                result := char[]{result:Length*2}
            else
                done := true
            endif
        enddo
        var sValues := string{result,0, nSize}
        var aElements := sValues:Split( c'\0')
        return aElements

    method GetString(sSection as string,sEntry as string,sDefault as string) as string
        var result := StringBuilder{255}
        var done := false
        do while ! done
            var nSize := GetPrivateProfileString(sSection, sEntry, "", result, result:Capacity, cFileName)
            if nSize >= result:Capacity -1
                // resize the buffer
                result := StringBuilder{result:Capacity*2}
            else
                done := true
            endif
        enddo
        return result:ToString()

    method GetStringUpper(sSection as string,sEntry as string,sDefault as string) as string
        return GetString(sSection, sEntry, sDefault):ToUpper()


    method WriteInt(sSection as string,sEntry as string,nValue as longint) as logic
        return WritePrivateProfileString(sSection, sEntry, nValue:ToString(), cFileName)

    method WriteString(sSection as string,sEntry as string,sString as string) as logic
        return WritePrivateProfileString(sSection, sEntry, sString, cFileName)

#region external methods

        [DllImport("kernel32", CharSet := CharSet.Unicode)];
    static method WritePrivateProfileString(section as string, Key as string, strValue as string, FileName as string) as logic

        [DllImport("kernel32", CharSet := CharSet.Unicode)];
    static method GetPrivateProfileString(section as string, Key as string, strDefault as string, retval as StringBuilder, nSize as long, FileName as string) as long

        [DllImport("kernel32", CharSet := CharSet.Unicode)];
    static method GetPrivateProfileSection(section as string, retval as char ptr, nSize as long, FileName as string) as long

#endregion
end class
end namespace // XSharp.RDD.SqlRDD
