//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using System.Collections.Generic
using System.IO
using System.Linq

internal static class XSharp.FileSearch
	static private foundEntries	:= List<object>{} as List<object>
	static private enumerator   := null as IEnumerator<object>
	static private currentItem	:= null as object
	static private isAtEnd		:= true as logic
	const timeFormat := "HH:MM:ss" as string
	
	public static method FFCount( filespec as string , attributes as dword ) as dword
		FFirst(filespec,attributes)
		return (dword)foundEntries:Count
		
	public static method FFirst( filespec as string , attributes as dword ) as logic
		// Split filespec in path and mask
		// when path is empty then path is current directory
		// make sure that we only search in the given path
		local cPath as string
		local cMask as string
		cPath := Path.GetDirectoryName(filespec)
		cMask := Path.GetFilename(filespec)
		if String.IsNullOrEmpty(cPath)
			cPath := System.Environment.CurrentDirectory
		endif
		foundEntries:Clear()
		if attributes == FA_VOLUME
				local allDrives := DriveInfo.GetDrives() as DriveInfo[]
				foreach drive as DriveInfo in allDrives
					foundEntries:Add(drive)
			next
		else
			local oDirInfo as DirectoryInfo
			oDirInfo := DirectoryInfo{cPath}
			if (attributes & FA_DIRECTORY) == FA_DIRECTORY
					local directories := oDirInfo:GetDirectories(cMask) as FileSystemInfo[]
					attributes -= (int) FA_DIRECTORY
					attributes += (int) FA_NORMAL
					var selectedDirs := from DirectoryInfo in directories ;
					where (DirectoryInfo:Attributes & (FileAttributes) attributes ) != 0 select DirectoryInfo
					foreach directory as DirectoryInfo in selectedDirs
						foundEntries:Add(directory)
				next 
			else
				attributes += (int) FA_NORMAL
				local files := oDirInfo:GetFiles(filespec) as FileInfo[]
				var selectedFiles := from FileInfo in files ;
				where ( FileInfo:Attributes & (FileAttributes) attributes) != 0 select FileInfo
				foreach file as FileInfo in files
					foundEntries:Add(file)
				next					
			endif
		endif
		enumerator := foundEntries:GetEnumerator()
		enumerator:Reset()
		isAtEnd := !enumerator:MoveNext()
		if  !isAtEnd
			currentItem := enumerator:Current
		endif
		return (foundEntries:Count > 0)
		
	public static method FNext() as logic
		if !isAtEnd
			isAtEnd := enumerator:MoveNext()
			if !isAtEnd
				currentItem := enumerator:Current
			endif
		endif
		return isAtEnd
		
	public static method FName() as string
		local name := "" as string
		if !isAtEnd
			if (currentItem is DriveInfo)
				name := ((DriveInfo)currentItem):Name
			elseif (currentItem is FileInfo)
				name := ((FileInfo)currentItem):Name
			elseif (currentItem is DirectoryInfo)
				name := ((DirectoryInfo) currentItem):Name
			endif
		endif
		return name
		
	public static method FSize() as dword
		local size := 0 as int
		if !isAtEnd
			if currentItem is DriveInfo
				size := (int)((DriveInfo) currentItem):TotalSize
			elseif (currentItem is FileInfo)
				size := (int)((FileInfo)currentItem):Length
			elseif currentItem is DirectoryInfo
				size := (int)((DirectoryInfo) currentItem):GetFileSystemInfos().LongLength
			endif						
		endif
		return (dword) size
		
	public static method FTime() as string
		local time := "00:00:00" as string
		if !isAtEnd
			if (currentItem is FileInfo)
				time := ((FileInfo)currentItem):LastWriteTime.ToString(timeFormat)
			elseif currentItem is DirectoryInfo
				time := ((DirectoryInfo) currentItem):LastWriteTime.ToString(timeFormat)
			endif						
		endif
		return  time
		
	public static method FDate() as DateTime
		local time := DateTime.MinValue as DateTime
		if !isAtEnd
			if (currentItem is FileInfo)
				time := ((FileInfo)currentItem):LastWriteTime
			elseif currentItem is DirectoryInfo
				time := ((DirectoryInfo) currentItem):LastWriteTime
			endif						
		endif
		return  time
		
		
	public static method FAttrib() as dword
		local attributes := 0x00000008 as int
		if !isAtEnd
			if (currentItem is FileInfo)
				attributes := (int)((FileInfo)currentItem):Attributes
			elseif currentItem is DirectoryInfo
				attributes := (int)((DirectoryInfo) currentItem):Attributes
			endif						
		endif
		return  (dword)attributes
		
		end class
	/// <summary>
	/// Return the number of files that match a given file specification and attribute.
	/// </summary>
	/// <param name="pszFile"></param>
	/// <param name="nAttr"></param>
	/// <returns>
/// </returns>
function FFCount(pszFile as string,nAttr as dword) as dword
	return XSharp.FileSearch.FFCount(pszFile,nAttr) 
	
	/// <summary>
	/// Find the first file that matches a given file specification or attribute.
	/// </summary>
	/// <param name="pszFile"></param>
	/// <param name="nAttr"></param>
	/// <returns>
	/// </returns>
function FFirst(pszFile as string,nAttr as dword) as logic
	return XSharp.FileSearch.FFirst(pszFile,nAttr)
	/// <summary>
	/// Determine the attributes of the file found after FFCount(), FFirst(), or FNext().
	/// </summary>
	/// <returns>
	/// </returns>
function FAttrib() as dword
	return XSharp.FileSearch.FAttrib() 
	
	
	/// <summary>
	/// Return the Date stamp of the file found by FFCount(), FFirst(), or FNext().
	/// </summary>
	/// <returns>
	/// </returns>
function FDate() as DateTime
	return XSharp.FileSearch.FDate()
	
	/// <summary>
	/// Return the name of the file found by FFCount(), FFirst(), or FNext().
	/// </summary>
	/// <returns>
	/// </returns>
function FName() as string
	return XSharp.FileSearch.FName()  
	
	
	
	
	/// <summary>
	/// Find the next file that matches the file previously found by FFirst().
	/// </summary>
	/// <returns>
	/// </returns>
function FNext() as logic
	return XSharp.FileSearch.FNext()     
	
	
	/// <summary>
	/// Return the size of the file found by FFCount(), FFirst(), or FNext().
	/// </summary>
	/// <returns>
	/// </returns>
function FSize() as dword
	return XSharp.FileSearch.FSize() 
	/// <summary>
	/// Return the time stamp of the file found by FFCount(), FFirst(), or FNext().
	/// </summary>
	/// <returns>
	/// </returns>
function FTime() as string
	return XSharp.FileSearch.FTime() 
	
	
	
	
	/// <summary>
	/// Determine if any file matches a given file specification.
	/// </summary>
	/// <param name="cFile">The name oif the file</param>
	/// <returns>
	/// True if the file exists, otherwise false
	/// </returns>
function File(cFile as string) as logic
	local lFound as logic
	local lHasWildCards as logic
	local aPaths as string[]
	local cTemp as string
	local lFirst as LOGIC
	IF String.IsNullOrEmpty(cFile)
		return FALSE
	endif
	lHasWildCards := cFile:IndexOfAny( <Char>{ '*', '?' } ) > 0
	XSharp.IO.File.LastFound := ""
	if ! lHasWildCards
		lFound := System.IO.File.Exists(cFile)
		if lFound
			XSharp.IO.File.LastFound := Path.GetFullPath( cFile )
			return TRUE
		endif
		aPaths := __GetSearchPaths()
		lFirst := TRUE
		foreach cPath as string in aPaths
			cTemp := System.IO.Path.Combine(cPath, cFile)
			if lFirst
				// store the first path that we looked in even when the file is not found
				// to be compatible with VO
				XSharp.IO.File.LastFound := cTemp
				lFirst := FALSE
			endif
			lFound := System.IO.File.Exists(cTemp)
			if lFound
				XSharp.IO.File.LastFound := cTemp
				return TRUE
			endif
		next
	else
		// wildcard, so use Directory.GetFiles()
		LOCAL files     AS STRING[]

		if Path.IsPathRooted(cFile)
			files := Directory.GetFiles( Path.GetDirectoryName( cFile ), Path.GetFileName( cFile ) )
			if files:Length > 0
				XSharp.IO.File.LastFound := files[1]
				return TRUE
			else
				// store the first path that we looked in even when the file is not found
				// to be compatible with VO
				XSharp.IO.File.LastFound := cFile
				return FALSE
			endif
		else
			// Look in current directory first and if that fails through the whole normal search list
			if __FileHelper(Environment.CurrentDirectory, cFile, false )
				return true
			endif
			aPaths := __GetSearchPaths()
			lFirst := TRUE
			foreach cPath as string in aPaths
				if __FileHelper(cPath, cFile, lFirst )
					return true
				endif
				lFirst := FALSE
			next
		endif
	endif
	return FALSE
	/// <summary>
	/// Return the name and path of the file that was used by FXOpen() or File().
	/// </summary>
	/// <returns>
	/// </returns>
function FPathName() as string

	return XSharp.IO.File.LastFound


internal function __FileHelper(cPath as string, cFileSpec as string, lSavePath as LOGIC) as logic
local cTemp as string
local cFile as string
local files as string[]
if ! System.IO.Directory.Exists(cPath)
	return false
endif
cTemp := System.IO.Path.Combine(cPath, cFileSpec)
cPath := System.IO.Path.GetDirectoryName(cTemp)
cFile := System.IO.Path.GetFileName(cTemp)
if ! System.IO.Directory.Exists(cPath)
	return false
endif
files := System.IO.Directory.GetFiles(cPath, cFile)
if files:Length > 0 
	XSharp.IO.File.LastFound := files[0]
elseif lSavePath
	XSharp.IO.File.LastFound := cTemp
endif
return files:Length > 0


internal function __GetSearchPaths() as string[]
	// Not found, now use the path settings from SetDefault and SetPath()
	// if SetPath() is empty then we look through the Environment variable Path
	local aDefault as string[]
	aDefault := __SetPathArray()
	if aDefault != null
		return aDefault
	endif

	var aPaths := List<string>{}
	var cPath  := SetDefault()
	if !String.IsNullOrEmpty(cPath)
		aPaths:Add(cPath)
	endif
	cPath := SetPath()
	if String.IsNullOrEmpty(cPath)
		cPath := GetEnv("PATH")
	endif
	if !String.IsNullOrEmpty(cPath)
		var aElements := cPath:Split(<char>{ ';' }, StringSplitOptions.RemoveEmptyEntries )
		aPaths:AddRange(aElements)
	endif
	aDefault := aPaths:ToArray()
	__SetPathArray(aDefault)
	return aDefault
