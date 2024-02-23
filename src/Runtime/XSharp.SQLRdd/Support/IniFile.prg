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

/// <summary>
/// Class to read/write an INI file
/// </summary>
class SqlDbIniFile
    protected cFileName as string
    /// <summary>
    /// Full path and name of the INI file
    /// </summary>
    property FullName as string get cFileName
    /// <summary>
    /// Does the file exist
    /// </summary>
    property Exists   as logic get System.IO.File.Exists(cFileName)
    /// <summary>
    /// Creaye a new INI file object
    /// </summary>
    /// <param name="cFile">File Name. May include path.</param>
    constructor(cFile as string)
        cFileName := cFile
        if File(cFile)
            cFileName := FPathName()
        endif

        return
    /// <summary>
    /// Create a new INI file
    /// </summary>
    /// <returns>true when succesfully created</returns>
    method Create() as logic strict
        try
            System.IO.File.WriteAllText(cFileName,"")
        catch
            return false
        end try
        return true
    /// <summary>
    /// Delete an entry from the INI file
    /// </summary>
    /// <param name="sSection">Section Name</param>
    /// <param name="sEntry">Entry Name</param>
    /// <returns>always true</returns>
    method DeleteEntry(sSection as string,sEntry as string) as logic
        self:WriteString( sSection, sEntry, null)
        return true

    /// <summary>
    /// Delete a section from the INI file
    /// </summary>
    /// <param name="sSection">Section Name</param>
    /// <returns>always true</returns>
    method DeleteSection(sSection as string) as logic
        self:WriteString( sSection, null, null )
        return true

    /// <summary>
    /// Read an integer from the INI file
    /// </summary>
    /// <param name="sSection">Section Name</param>
    /// <param name="sEntry">Entry Name</param>
    /// <param name="nDefault">Default value. This is returned when the entry is missing or not a valid number</param>
    /// <returns>The number from the ini file or the default</returns>
    method GetInt(sSection as string,sEntry as string,nDefault as longint) as longint
        var sValue := GetString(sSection, sEntry, "")
        var nValue := nDefault
        if ! String.IsNullOrEmpty(sValue)
            if ! Int32.TryParse(sValue, out nValue)
                nValue := nDefault
            endif
        endif
        return nValue

    /// <summary>
    /// Read a logic value from the INI file
    /// </summary>
    /// <param name="sSection">Section Name</param>
    /// <param name="sEntry">Entry Name</param>
    /// <param name="lDefault">Default value. This is returned when the entry is missing or not a valid logical</param>
    /// <returns>The value from ini file or the default</returns>
    /// <remarks>
    /// The following strings are considered as true: <br/>
    /// "ON", "TRUE", "YES" and a non numeric number. <br/>
    /// Any other value is considered as false
    /// </remarks>
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

    /// <summary>
    /// Read a sectionfrom the INI file
    /// </summary>
    /// <param name="sSection">Section Name</param>
    /// <returns>The complete section as a list of strings</returns>
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

    /// <summary>
    /// Read a string value from the INI file
    /// </summary>
    /// <param name="sSection">Section Name</param>
    /// <param name="sEntry">Entry Name</param>
    /// <param name="sDefault">Default value. This is returned when the entry is missing</param>
    /// <returns>The value from ini file or the default</returns>
    method GetString(sSection as string,sEntry as string,sDefault as string) as string
        var result := StringBuilder{255}
        var done := false
        do while ! done
            var nSize := GetPrivateProfileString(sSection, sEntry, sDefault, result, result:Capacity, cFileName)
            if nSize >= result:Capacity -1
                // resize the buffer
                result := StringBuilder{result:Capacity*2}
            else
                done := true
            endif
        enddo
        return result:ToString()

    /// <summary>
    /// Read a string value from the INI file and covert it to uppercase
    /// </summary>
    /// <param name="sSection">Section Name</param>
    /// <param name="sEntry">Entry Name</param>
    /// <param name="sDefault">Default value. This is returned when the entry is missing</param>
    /// <returns>The value from ini file or the default</returns>

    method GetStringUpper(sSection as string,sEntry as string,sDefault as string) as string
        return GetString(sSection, sEntry, sDefault):ToUpper()

    /// <summary>
    /// Write an integer to the INI file
    /// </summary>
    /// <param name="sSection">Section Name</param>
    /// <param name="sEntry">Entry Name</param>
    /// <param name="nValue">Value to write</param>

    /// <returns></returns>
    method WriteInt(sSection as string,sEntry as string,nValue as longint) as logic
        return WritePrivateProfileString(sSection, sEntry, nValue:ToString(), cFileName)

    /// <summary>
    /// Write an integer to the INI file
    /// </summary>
    /// <param name="sSection">Section Name</param>
    /// <param name="sEntry">Entry Name</param>
    /// <param name="sString">Value to write</param>
    method WriteString(sSection as string,sEntry as string,sString as string) as logic
        return WritePrivateProfileString(sSection, sEntry, sString, cFileName)

#region external methods

    /// <exclude />
    [DllImport("kernel32", CharSet := CharSet.Unicode)];
    private extern static method WritePrivateProfileString(section as string, Key as string, strValue as string, FileName as string) as logic

    /// <exclude />
    [DllImport("kernel32", CharSet := CharSet.Unicode)];
    private extern static method GetPrivateProfileString(section as string, Key as string, strDefault as string, retval as StringBuilder, nSize as long, FileName as string) as long

    /// <exclude />
    [DllImport("kernel32", CharSet := CharSet.Unicode)];
    private extern static method GetPrivateProfileSection(section as string, retval as char ptr, nSize as long, FileName as string) as long

#endregion
end class
end namespace // XSharp.RDD.SqlRDD
