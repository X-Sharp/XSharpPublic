//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System
USING System.Collections
USING System.IO
USING System.Linq
USING System.Runtime.InteropServices
USING System.Security
USING Microsoft.Win32.SafeHandles
USING System.Runtime
USING System.Runtime.ConstrainedExecution

BEGIN NAMESPACE XSharp.IO
	
	STATIC CLASS FileHelper
		STATIC foundEntries	:= ArrayList{} AS ArrayList
		STATIC enumerator   := NULL AS IEnumerator 
		STATIC currentItem	:= NULL AS OBJECT
		STATIC isAtEnd		:= TRUE AS LOGIC
		STATIC errorCode:= 0 AS DWORD
		
		PUBLIC STATIC METHOD FFCount( fileSpec AS STRING , attributes AS DWORD ) AS DWORD
			FFirst(fileSpec,attributes)
			RETURN (DWORD)foundEntries:Count
		
		PUBLIC STATIC METHOD FFirst( fileSpec AS STRING , attributes AS DWORD ) AS LOGIC
			foundEntries:Clear()
			LOCAL fileSpecification := fileSpec AS STRING
			IF attributes == 0x00000008
				LOCAL allDrives := DriveInfo.GetDrives() AS DriveInfo[]
				FOREACH  Drive AS DriveInfo IN allDrives
					foundEntries:Add(drive)
				NEXT
			ELSE
				IF ((FileAttributes)attributes & FileAttributes.Directory) == FileAttributes.Directory
					LOCAL directories := DirectoryInfo{fileSpecification}.GetDirectories() AS DirectoryInfo[]
					attributes -= (INT) FileAttributes.Directory
					attributes += (INT) FileAttributes.Normal
					VAR selectedDirs := FROM DirectoryInfo IN directories WHERE (DirectoryInfo:Attributes & (FileAttributes) attributes ) != 0 SELECT DirectoryInfo
					FOREACH directory AS DirectoryInfo IN selectedDirs
						foundEntries:Add(directory)
					NEXT 
				ELSE
					attributes += (INT) FileAttributes.Normal
					LOCAL files := DirectoryInfo{fileSpecification}.GetFiles() AS FileInfo[]
					VAR selectedFiles := FROM FileInfo IN files WHERE ( FileInfo:Attributes & (FileAttributes) attributes) != 0 SELECT FileInfo
					FOREACH file AS FileInfo IN files
						foundEntries:Add(file)
					NEXT					
				ENDIF
			ENDIF
			enumerator := foundEntries:GetEnumerator()
			isAtEnd := enumerator:MoveNext()
			IF  !isAtEnd
				currentItem := enumerator:Current
			ENDIF
			RETURN (foundEntries:Count > 0)
		
		PUBLIC STATIC METHOD FNext() AS LOGIC
			IF !isAtEnd
				isAtEnd := enumerator:MoveNext()
				IF !isAtEnd
					currentItem := enumerator:Current
				ENDIF
			ENDIF
			RETURN isAtEnd
		
		PUBLIC STATIC METHOD FName() AS STRING
			LOCAL name := "" AS STRING
			IF !isAtEnd
				IF currentItem IS DriveInfo
					name := ((DriveInfo) currentItem):Name
				ELSEIF (currentItem IS FileInfo)
					name := ((FileInfo)currentItem):Name
				ELSEIF (currentItem IS DirectoryInfo)
					name := ((DirectoryInfo) currentItem):Name
				ENDIF
			ENDIF
			RETURN name
		
		PUBLIC STATIC METHOD FSize() AS DWORD
			LOCAL size := 0 AS INT
			IF !isAtEnd
				IF currentItem IS DriveInfo
					size := (INT)((DriveInfo) currentItem):TotalSize
				ELSEIF (currentItem IS FileInfo)
					size := (INT)((FileInfo)currentItem):Length
				ELSEIF currentItem IS DirectoryInfo
					size := (INT)((DirectoryInfo) currentItem):GetFileSystemInfos().LongLength
				ENDIF						
			ENDIF
			RETURN (DWORD) size
		
		PUBLIC STATIC METHOD FTime() AS STRING
			LOCAL time := "00:00:00" AS STRING
			IF !isAtEnd
				IF (currentItem IS FileInfo)
					time := ((FileInfo)currentItem):LastWriteTime.ToString("HH:MM:ss")
				ELSEIF currentItem IS DirectoryInfo
					time := ((DirectoryInfo) currentItem):LastWriteTime.ToString("HH:MM:ss")
				ENDIF						
			ENDIF
			RETURN  time
		
		PUBLIC STATIC METHOD FDate() AS DateTime
			LOCAL time := DateTime.MinValue AS DateTime
			IF !isAtEnd
				IF (currentItem IS FileInfo)
					time := ((FileInfo)currentItem):LastWriteTime
				ELSEIF currentItem IS DirectoryInfo
					time := ((DirectoryInfo) currentItem):LastWriteTime
				ENDIF						
			ENDIF
			RETURN  time
		
		
		PUBLIC STATIC METHOD FAttrib() AS DWORD
			LOCAL attributes := 0x00000008 AS INT
			IF !isAtEnd
				IF (currentItem IS FileInfo)
					attributes := (INT)((FileInfo)currentItem):Attributes
				ELSEIF currentItem IS DirectoryInfo
					attributes := (INT)((DirectoryInfo) currentItem):Attributes
				ENDIF						
			ENDIF
			RETURN  (DWORD)attributes
		
		[DllImport("kernel32.dll", CharSet := CharSet.Auto, SetLastError := TRUE , EntryPoint := "CreateFileW" , ExactSpelling := TRUE)];
		PUBLIC STATIC UNSAFE EXTERN METHOD CreateFile(;
			[MarshalAs(UnmanagedType.LPTStr)] filename AS STRING,;
			[MarshalAs(UnmanagedType.U4)] accessMode AS FileAccess,;
			[MarshalAs(UnmanagedType.U4)] share AS FileShare ,;
			securityAttributes AS IntPtr,; // optional SECURITY_ATTRIBUTES struct or IntPtr.Zero
			[MarshalAs(UnmanagedType.U4)] creationDisposition AS FileMode,;
			[MarshalAs(UnmanagedType.U4)] flagsAndAttributes AS FileAttributes ,;
			templateFile AS PTR) AS PTR
		
		[DllImport("kernel32.dll",SetLastError := TRUE, ExactSpelling := TRUE, CharSet := CharSet.Auto)];
		[RETURN: MarshalAs(UnmanagedType.Bool)];
		PUBLIC STATIC EXTERN UNSAFE METHOD WriteFile(hFile AS PTR,  lpBuffer AS BYTE[],;
			nNumberOfBytesToWrite AS DWORD, lpNumberOfBytesWritten REF DWORD,;
			[In] lpOverlapped REF System.Threading.NativeOverlapped ) AS LOGIC
		
		[DllImport("kernel32.dll", SetLastError := TRUE, ExactSpelling := TRUE, CharSet := CharSet.Auto)];
		[ReliabilityContract(Consistency.WillNotCorruptState, Cer.Success)];
		[SuppressUnmanagedCodeSecurity];
		[RETURN: MarshalAs(UnmanagedType.Bool)];
		PUBLIC STATIC UNSAFE EXTERN METHOD CloseHandle(hObject AS PTR) AS LOGIC
		
		[DllImport("Kernel32.dll", SetLastError := TRUE, CharSet := CharSet.Auto, ExactSpelling := TRUE)];
		PUBLIC STATIC EXTERN UNSAFE METHOD SetFilePointer(;
			[In]  hFile AS SafeFileHandle,; 
			[In]  lDistanceToMove AS INT,;
			[Out] lpDistanceToMoveHigh AS INT,;
		[In]  dwMoveMethod AS EMoveMethod) AS DWORD
		
		
		PUBLIC STATIC UNSAFE METHOD FCreate( file AS STRING ) AS PTR
		RETURN FCreate(file, 0)
		
		PUBLIC STATIC UNSAFE METHOD FCreate( file AS STRING , attributes AS DWORD) AS PTR
			LOCAL mode := VOFileMode{0x100, (INT)attributes} AS VOFileMode
			RETURN FCreate(file, mode)
		
		PUBLIC STATIC UNSAFE METHOD FCreate(file AS STRING, mode AS VOFileMode) AS PTR
			LOCAL handle := CreateFile(file, (FileAccess)mode.AccessCode, (FileShare)mode.ShareCode, IntPtr.Zero, (FileMode)mode.CreateCode, (FileAttributes)mode.AttributesCode, IntPtr.Zero) AS PTR
			IF (((IntPtr)handle):ToInt32() == -1)
				FError((DWORD)Marshal.GetLastWin32Error())
			ELSE
				FError(0)
			ENDIF
			RETURN handle
		
		PUBLIC STATIC METHOD FError( code AS DWORD) AS DWORD
			LOCAL lastError := errorCode AS DWORD
			errorCode := code 
			RETURN lastError
		
		PUBLIC STATIC UNSAFE METHOD FWrite( handle AS PTR,buffer AS STRING) AS DWORD	
			RETURN FWrite(handle, buffer, (DWORD)buffer:Length)
		
		PUBLIC UNSAFE STATIC METHOD FWrite( handle AS PTR, buffer AS STRING, count AS DWORD) AS DWORD
			LOCAL successFull := FALSE AS LOGIC
			LOCAL bytes := System.Text.Encoding.ASCII.GetBytes(buffer) AS BYTE[]
			LOCAL bytesRead := 0 AS DWORD
			LOCAL overlapped := DEFAULT(System.Threading.NativeOverlapped) AS System.Threading.NativeOverlapped
			successFull := WriteFile(handle, bytes, count,REF bytesRead, REF overlapped)
			IF !successFull 
				FError((DWORD)Marshal.GetLastWin32Error())
			ENDIF
		RETURN (DWORD) bytesRead
		
		PUBLIC STATIC UNSAFE METHOD FClose(handle AS PTR) AS LOGIC
			LOCAL  isClosed := CloseHandle(handle) AS LOGIC
			IF (!isClosed)
				FError((DWORD)Marshal.GetLastWin32Error())
			ENDIF
		RETURN isClosed
		
		PUBLIC STATIC UNSAFE METHOD FSeek(handle AS PTR, lOffset AS LONG, dwOrigin AS DWORD) AS LONG
			LOCAL position := (INT) SetFilePointer(SafeFileHandle{(IntPtr)handle,TRUE}, lOffset, 0, (EMoveMethod)dwOrigin) AS INT
			IF (position == System.Int32.MaxValue)
				FError((DWORD)Marshal.GetLastWin32Error())
			ELSE
				FError(0)
			ENDIF
		RETURN (LONG)position
		
		PUBLIC STATIC UNSAFE METHOD FSeek( handle AS PTR, lOffset AS LONG) AS LONG
		RETURN FSeek(handle,lOffset,0)
		
		PUBLIC STATIC UNSAFE METHOD FTell(handle AS PTR) AS LONG
		RETURN FSeek(handle, 0, 1)
		
		PUBLIC STATIC UNSAFE METHOD FEof(handle AS PTR) AS LOGIC
			LOCAL offset := FTell(handle) AS INT
			LOCAL position := FSeek(handle, 0, 2) AS INT
			LOCAL isEof := ( offset == position ) AS LOGIC
			IF !isEof
				FSeek(handle, offset, 0)
			ENDIF
		RETURN isEof
		
		PUBLIC STATIC UNSAFE METHOD FRewind(handle AS PTR) AS INT
		RETURN (INT)FSeek(handle, 0, 0)
		
		PUBLIC STATIC UNSAFE METHOD FOpen(file AS STRING ) AS PTR
		RETURN FOpen(file, 0)
		
		PUBLIC STATIC UNSAFE METHOD FOpen(file AS STRING,mode AS INT) AS PTR
			LOCAL fileMode := VOFileMode{mode, 0} AS VOFileMode 
		RETURN FCreate(file, fileMode)
		
		PUBLIC STATIC METHOD FRename( oldFile AS STRING , newFile AS STRING) AS LOGIC
			LOCAL renamed := TRUE AS LOGIC
			TRY
				System.IO.File.Move(oldFile, newFile)
			CATCH 
				FError((DWORD)Marshal.GetLastWin32Error())
				renamed := FALSE
			END TRY
			RETURN renamed
		
		PUBLIC STATIC METHOD MoveFile(oldFile AS STRING,newFile AS STRING) AS LOGIC
			RETURN FRename(oldFile, newFile)
		
		PUBLIC STATIC METHOD FErase(fileName AS STRING) AS LOGIC
			LOCAL isDeleted := TRUE AS LOGIC
			TRY
				System.IO.File.Delete(fileName)
			CATCH 
				FError((DWORD)Marshal.GetLastWin32Error())
				isDeleted := FALSE
			END TRY
			RETURN isDeleted
		
		PUBLIC STATIC METHOD FCopy(fileName AS STRING,destination AS STRING) AS LOGIC
			LOCAL isDeleted := TRUE AS LOGIC
			TRY
				System.IO.File.Copy(fileName,destination,TRUE)
			CATCH 
				FError((DWORD)Marshal.GetLastWin32Error())
				isDeleted := FALSE
			END TRY
			RETURN isDeleted
		
	END CLASS
	
	PUBLIC STRUCTURE VOFileMode
		PUBLIC PROPERTY AccessCode		AS INT AUTO
		PUBLIC PROPERTY AttributesCode	AS INT AUTO
		PUBLIC PROPERTY CreateCode		AS INT AUTO
		PUBLIC PROPERTY ShareCode		AS INT AUTO
		
		CONSTRUCTOR(mode AS INT,attribs AS INT)
			/// see File.C ( __PrepareMode )
			IF (mode & 0x10000) == 0x10000
				mode -= 0x10000
			ENDIF
			
			IF attribs > 0 
				AttributesCode := attribs
			ELSE
				AttributesCode := FileAttributes.Normal
			ENDIF
			AccessCode := UNCHECKED((INT)0x80000000)
			
			IF ((mode & 2) == 2)
				AccessCode += 0x40000000
			ELSEIF ((mode & 1) == 1)
				AccessCode := 0x40000000
			ENDIF
			
			CreateCode := 3
			IF ((mode & 0x1000) == 0x1000)
				CreateCode := 2
				AccessCode := UNCHECKED((INT)0xc0000000)
			ENDIF
			ShareCode := 0
			LOCAL num := 0x70 AS INT
			SWITCH (mode & num)
				CASE 0x40
			ShareCode := 3
				CASE 0x20
			ShareCode := 1
				CASE 0x30
			ShareCode := 2
				CASE 0x10
			ShareCode := 0
				CASE 0
			ShareCode := 0
			END SWITCH
			RETURN
		
	END STRUCTURE
	
	ENUM EMoveMethod AS DWORD
		@@Begin := 0
		@@Current := 1
		@@End := 2
	END ENUM
	
	
	#region functions
		/// <summary>
		/// Return the number of files that match a given file specification and attribute.
		/// </summary>
		/// <param name="pszFile"></param>
		/// <param name="nAttr"></param>
		/// <returns>
		/// </returns>
		FUNCTION FFCount(pszFile AS STRING,nAttr AS DWORD) AS DWORD
			RETURN XSharp.IO.FileHelper.FFCount(pszFile,nAttr) 
		
		/// <summary>
		/// Find the first file that matches a given file specification or attribute.
		/// </summary>
		/// <param name="pszFile"></param>
		/// <param name="nAttr"></param>
		/// <returns>
		/// </returns>
		FUNCTION FFirst(pszFile AS STRING,nAttr AS DWORD) AS LOGIC
			RETURN XSharp.IO.FileHelper.FFirst(pszFile,nAttr)
		
		/// <summary>
		/// Determine the attributes of the file found after FFCount(), FFirst(), or FNext().
		/// </summary>
		/// <returns>
		/// </returns>
		FUNCTION FAttrib() AS DWORD
			RETURN XSharp.IO.FileHelper.FAttrib() 
		
		
		/// <summary>
		/// Return the __VODate stamp of the file found by FFCount(), FFirst(), or FNext().
		/// </summary>
		/// <returns>
		/// </returns>
		FUNCTION FDate() AS DateTime
			RETURN XSharp.IO.FileHelper.FDate()
		
		/// <summary>
		/// Get or set the error code for a file operation.
		/// </summary>
		/// <param name="nSet"></param>
		/// <returns>
		/// </returns>
		FUNCTION FError(nSet AS OBJECT) AS DWORD
			RETURN XSharp.IO.FileHelper.FError((DWORD) nSet)  
		
		/// <summary>
		/// Read a line from an open file.
		/// </summary>
		/// <param name="pFile"></param>
		/// <param name="nBuffLen"></param>
		/// <returns>
		/// </returns>
		FUNCTION FGetS(pFile AS OBJECT,nBuffLen AS OBJECT) AS STRING
			/// THROW NotImplementedException{}
			RETURN String.Empty   
		
		/// <summary>
		/// Change the size of a file opened with a low-level file function.
		/// </summary>
		/// <param name="pFile"></param>
		/// <param name="nOffset"></param>
		/// <returns>
		/// </returns>
		UNSAFE FUNCTION FChSize(pFile AS PTR,nOffset AS DWORD) AS DWORD
			/// THROW NotImplementedException{}
		RETURN 0   
		
		/// <summary>
		/// Close an open file and write the buffers to disk.
		/// </summary>
		/// <param name="pFile"></param>
		/// <returns>
		/// </returns>
		UNSAFE FUNCTION FClose(pFile AS PTR) AS LOGIC
		RETURN XSharp.IO.FileHelper.FClose(pFile)   
		
		/// <summary>
		/// Flush file buffers.
		/// </summary>
		/// <param name="pHandle"></param>
		/// <returns>
		/// </returns>
		UNSAFE FUNCTION FCommit(pHandle AS PTR) AS VOID
			/// THROW NotImplementedException{}
		RETURN 
		
		/// <summary>
		/// Determine if the file pointer is positioned at the end-of-file.
		/// </summary>
		/// <param name="pFILE"></param>
		/// <returns>
		/// </returns>
		UNSAFE FUNCTION FEof(pFILE AS PTR) AS LOGIC
		RETURN XSharp.IO.FileHelper.FEof(pFILE ) 
		
		/// <summary>
		/// Lock a portion of an open file.
		/// </summary>
		/// <param name="pHandle"></param>
		/// <param name="dwOffset"></param>
		/// <param name="dwLength"></param>
		/// <returns>
		/// </returns>
		UNSAFE FUNCTION FFLock(pHandle AS PTR,dwOffset AS DWORD,dwLength AS DWORD) AS LOGIC
			/// THROW NotImplementedException{}
		RETURN FALSE   
		
		/// <summary>
		/// Flush to disk a file opened with a low-level file function.
		/// </summary>
		/// <param name="phandle"></param>
		/// <returns>
		/// </returns>
		UNSAFE FUNCTION FFlush(phandle AS PTR) AS VOID
			/// THROW NotImplementedException{}
		RETURN 
		
		/// <summary>
		/// Unlock a portion of an opened file.
		/// </summary>
		/// <param name="phandle"></param>
		/// <param name="dwOffset"></param>
		/// <param name="dwLength"></param>
		/// <returns>
		/// </returns>
		UNSAFE FUNCTION FFUnLock(phandle AS PTR,dwOffset AS DWORD,dwLength AS DWORD) AS LOGIC
			/// THROW NotImplementedException{}
		RETURN FALSE   
		
		/// <summary>
		/// Read a line from an open file, specifying two strongly typed arguments.
		/// </summary>
		/// <param name="pFile"></param>
		/// <param name="nBuffLen"></param>
		/// <returns>
		/// </returns>
		UNSAFE FUNCTION FGetS2(pFile AS PTR,nBuffLen AS DWORD) AS STRING
			/// THROW NotImplementedException{}
		RETURN String.Empty   
		
		/// <summary>
		/// </summary>
		/// <param name="ptrBuff"></param>
		/// <param name="nLen"></param>
		/// <param name="nDec"></param>
		/// <returns>
		/// </returns>
		UNSAFE FUNCTION FieldVal(ptrBuff AS PTR,nLen AS INT,nDec AS INT) AS REAL8
			/// THROW NotImplementedException{}
		RETURN 0   
		
		/// <summary>
		/// </summary>
		/// <param name="ptrUsual"></param>
		/// <param name="dwLen"></param>
		/// <param name="dwDec"></param>
		/// <returns>
		/// </returns>
		UNSAFE FUNCTION __VOFloat2Str(ptrUsual AS PTR,dwLen AS DWORD,dwDec AS DWORD) AS STRING
			/// THROW NotImplementedException{}
		RETURN String.Empty
		
		/// <summary>
		/// Write a string, a carriage-return character, and a linefeed character to an open file, specifying three strongly-typed arguments.
		/// </summary>
		/// <param name="pFILE"></param>
		/// <param name="c"></param>
		/// <param name="nCount"></param>
		/// <returns>
		/// </returns>
		UNSAFE FUNCTION FPutS3(pFILE AS PTR,c AS STRING,nCount AS DWORD) AS DWORD
			/// THROW NotImplementedException{}
		RETURN 0   
		
		/// <summary>
		/// Read characters from a file into a buffer variable that is passed by reference.
		/// </summary>
		/// <param name="pHandle"></param>
		/// <param name="refC"></param>
		/// <param name="dwCount"></param>
		/// <returns>
		/// </returns>
		UNSAFE FUNCTION FRead(pHandle AS PTR,refC AS OBJECT,dwCount AS DWORD) AS DWORD
			/// THROW NotImplementedException{}
		RETURN 0   
		
		/// <summary>
		/// Read characters from a file into an allocated buffer.
		/// </summary>
		/// <param name="pHandle"></param>
		/// <param name="ptrBuffer"></param>
		/// <param name="dwCount"></param>
		/// <returns>
		/// </returns>
		UNSAFE FUNCTION FRead3(pHandle AS PTR,ptrBuffer AS PTR,dwCount AS DWORD) AS DWORD
			/// THROW NotImplementedException{}
		RETURN 0   
		
		/// <summary>
		/// Read characters from a file into an allocated buffer.
		/// </summary>
		/// <param name="pHandle"></param>
		/// <param name="ptrBuffer"></param>
		/// <param name="dwCount"></param>
		/// <param name="lAnsi"></param>
		/// <returns>
		/// </returns>
		UNSAFE FUNCTION FRead4(pHandle AS PTR,ptrBuffer AS PTR,dwCount AS DWORD,lAnsi AS LOGIC) AS DWORD
			/// THROW NotImplementedException{}
		RETURN 0   
		
		/// <summary>
		/// Read a line from an open file, specifying two strongly-typed arguments.
		/// </summary>
		/// <param name="pFile"></param>
		/// <param name="nBuffLen"></param>
		/// <returns>
		/// </returns>
		UNSAFE FUNCTION FReadLine2(pFile AS PTR,nBuffLen AS DWORD) AS STRING
			/// THROW NotImplementedException{}
		RETURN String.Empty   
		
		/// <summary>
		/// Read characters from a file.
		/// </summary>
		/// <param name="pHandle"></param>
		/// <param name="dwCount"></param>
		/// <returns>
		/// </returns>
		UNSAFE FUNCTION FReadStr(pHandle AS PTR,dwCount AS DWORD) AS STRING
			/// THROW NotImplementedException{}
		RETURN String.Empty   
		
		/// <summary>
		/// Read characters from a file into a buffer variable that is passed by reference.
		/// </summary>
		/// <param name="pHandle"></param>
		/// <param name="refC"></param>
		/// <param name="dwCount"></param>
		/// <returns>
		/// </returns>
		UNSAFE FUNCTION FReadText(pHandle AS PTR,refC AS OBJECT,dwCount AS DWORD) AS DWORD
			/// THROW NotImplementedException{}
		RETURN 0   
		
		/// <summary>
		/// Read characters from a file into an allocated buffer, with possible OEM to ANSI conversion, based on the current SetAnsi() setting.
		/// </summary>
		/// <param name="pHandle"></param>
		/// <param name="ptrBuffer"></param>
		/// <param name="dwCount"></param>
		/// <returns>
		/// </returns>
		UNSAFE FUNCTION FReadText3(pHandle AS PTR,ptrBuffer AS PTR,dwCount AS DWORD) AS DWORD
			/// THROW NotImplementedException{}
		RETURN 0   
		
		/// <summary>
		/// Set the file pointer at the top of an open file.
		/// </summary>
		/// <param name="pFile"></param>
		/// <returns>
		/// </returns>
		UNSAFE FUNCTION FRewind(pFile AS PTR) AS VOID
			/// THROW NotImplementedException{}
		RETURN   
		
		/// <summary>
		/// Set the file pointer to a new position, specifying three strongly-typed arguments.
		/// </summary>
		/// <param name="pHandle"></param>
		/// <param name="lOffset"></param>
		/// <param name="dwOrigin"></param>
		/// <returns>
		/// </returns>
		UNSAFE FUNCTION FSeek3(pHandle AS PTR,lOffset AS LONG,dwOrigin AS DWORD) AS LONG
			/// THROW NotImplementedException{}
		RETURN 0   
		
		/// <summary>
		/// Return the current position of the file pointer.
		/// </summary>
		/// <param name="pHandle"></param>
		/// <returns>
		/// </returns>
		UNSAFE FUNCTION FTell(pHandle AS PTR) AS LONG
			/// THROW NotImplementedException{}
		RETURN 0   
		
		/// <summary>
		/// Write the contents of a buffer to an open file.
		/// </summary>
		/// <param name="pHandle"></param>
		/// <param name="ptrBuffer"></param>
		/// <param name="dwCount"></param>
		/// <returns>
		/// </returns>
		UNSAFE FUNCTION FWrite3(pHandle AS PTR,ptrBuffer AS PTR,dwCount AS DWORD) AS DWORD
			/// THROW NotImplementedException{}
		RETURN 0   
		
		/// <summary>
		/// Write the contents of a buffer to an open file, with an ANSI to OEM conversion option.
		/// </summary>
		/// <param name="pHandle"></param>
		/// <param name="ptrBuffer"></param>
		/// <param name="dwCount"></param>
		/// <param name="lAnsi"></param>
		/// <returns>
		/// </returns>
		UNSAFE FUNCTION FWrite4(pHandle AS PTR,ptrBuffer AS PTR,dwCount AS DWORD,lAnsi AS LOGIC) AS DWORD
			/// THROW NotImplementedException{}
		RETURN 0   
		
		/// <summary>
		/// Write a string, a carriage-return character, and a linefeed character to an open file, specifying two strongly-typed arguments.
		/// </summary>
		/// <param name="pFILE"></param>
		/// <param name="c"></param>
		/// <param name="nCount"></param>
		/// <returns>
		/// </returns>
		UNSAFE FUNCTION FWriteLine3(pFILE AS PTR,c AS STRING,nCount AS DWORD) AS DWORD
			/// THROW NotImplementedException{}
		RETURN 0   
		
		/// <summary>
		/// Write the contents of a buffer to an open file, with SetAnsi() dependency.
		/// </summary>
		/// <param name="pHandle"></param>
		/// <param name="ptrBuffer"></param>
		/// <param name="dwCount"></param>
		/// <returns>
		/// </returns>
		UNSAFE FUNCTION FWriteText3(pHandle AS PTR,ptrBuffer AS PTR,dwCount AS DWORD) AS DWORD
			/// THROW NotImplementedException{}
		RETURN 0   
		
	#endregion
END NAMESPACE



/// <summary>
/// Remove spaces from a file name specified as a string.
/// </summary>
/// <param name="cName"></param>
/// <returns>
/// </returns>
FUNCTION AdjustFName(cName AS STRING) AS STRING
	LOCAL adjusted := NULL AS STRING
	IF ( !string.IsNullOrEmpty(cName) ) 
		adjusted := System.IO.Path.GetFileNameWithoutExtension(cName):TrimEnd()
		IF ( cName:IndexOf('.') > 0 ) 
			adjusted += System.IO.Path.GetExtension(cName)
		ENDIF
	ENDIF
	RETURN adjusted   



/// <summary>
/// Remove spaces from a file name specified as a string, changing the contents of the original file name as well as the returned file name.
/// </summary>
/// <param name="cName"></param>
/// <returns>
/// </returns>
FUNCTION AdjustFNameA(cName AS STRING) AS STRING
	THROW NotImplementedException{}
/// RETURN String.Empty   



/// <summary>
/// Copy a file to a new file or to a device.
/// </summary>
/// <param name="cSrc"></param>
/// <param name="cDest"></param>
/// <returns>
/// </returns>
FUNCTION FCopy(cSrc AS STRING,cDest AS STRING) AS LOGIC
	/// THROW NotImplementedException{}
	RETURN FALSE   

/// <summary>
/// Create a file or open and truncate an existing file, specifying two strongly typed arguments.
/// </summary>
/// <param name="cFile"></param>
/// <param name="dwAttr"></param>
/// <returns>
/// </returns>
UNSAFE FUNCTION FCreate2(cFile AS STRING,dwAttr AS DWORD) AS PTR
	/// THROW NotImplementedException{}
RETURN IntPtr.Zero


/// <summary>
/// Delete a file from disk.
/// </summary>
/// <param name="cFile"></param>
/// <returns>
/// </returns>
FUNCTION FErase(cFile AS STRING) AS LOGIC
	/// THROW NotImplementedException{}
	RETURN FALSE   

/// <summary>
/// Determine if any file matches a given file specification.
/// </summary>
/// <param name="cFile">The name oif the file</param>
/// <returns>
/// True if the file exists, otherwise false
/// </returns>
FUNCTION File(cFile AS STRING) AS LOGIC
RETURN	 System.IO.File.Exists(cFile)


/// <summary>
/// Open a file, specifying two strongly-typed arguments.
/// </summary>
/// <param name="cName"></param>
/// <param name="dwMode"></param>
/// <returns>
/// </returns>
UNSAFE FUNCTION FOpen2(cName AS STRING,dwMode AS DWORD) AS PTR
	/// THROW NotImplementedException{}
RETURN IntPtr.Zero





/// <summary>
/// Change the name of a file.
/// </summary>
/// <param name="cSrc"></param>
/// <param name="cDest"></param>
/// <returns>
/// </returns>
FUNCTION FRename(cSrc AS STRING,cDest AS STRING) AS LOGIC
	/// THROW NotImplementedException{}
	RETURN FALSE   

/// <summary>
/// Open a file.
/// </summary>
/// <param name="cFile"></param>
/// <param name="dwMode"></param>
/// <param name="cPath"></param>
/// <returns>
/// </returns>
UNSAFE FUNCTION FxOpen(cFile AS STRING,dwMode AS DWORD,cPath AS STRING) AS PTR
	/// THROW NotImplementedException{}
RETURN IntPtr.Zero



/// <summary>
/// </summary>
/// <param name="cAttr"></param>
/// <returns>
/// </returns>
FUNCTION String2FAttr(cAttr AS STRING) AS DWORD
	/// THROW NotImplementedException{}
	RETURN 0   


