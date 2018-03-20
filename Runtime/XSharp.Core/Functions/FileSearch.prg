//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using System.Collections.Generic
using System.IO
using System.Linq

internal static class XSharp.FileSearch
	static foundEntries	:= List<Object>{} as List<Object>
	static enumerator   := null as IEnumerator<Object>
	static currentItem	:= null as Object
	static isAtEnd		:= true as logic
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
			local lAllowDir as LOGIC
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
		isAtEnd := enumerator:MoveNext()
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
/// Return the __VODate stamp of the file found by FFCount(), FFirst(), or FNext().
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
/// Return the name and path of the file that was used by FXOpen() or File().
/// </summary>
/// <returns>
/// </returns>
FUNCTION FPathName() AS STRING
	
	return XSharp.IO.FileHelper.LastFound

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
	FUNCTION FSize() AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the time stamp of the file found by FFCount(), FFirst(), or FNext().
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION FTime() AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty   


