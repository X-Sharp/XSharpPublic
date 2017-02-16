//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System
using System.Collections
using System.IO
using System.Linq
using System.Runtime.InteropServices
using Vulcan
using System.Security
using Microsoft.Win32.SafeHandles
using System.Runtime
using System.Runtime.ConstrainedExecution

begin namespace XSharp.IO

	static class FileHelper
        static foundEntries	:= ArrayList{} as ArrayList
        static enumerator   := null as IEnumerator 
        static currentItem	:= null as object
        static isAtEnd		:= true as logic
        static errorCode:= 0 as dword

		public static METHOD FFCount( fileSpec as STRING , attributes as dword ) as dword
			FFirst(fileSpec,attributes)
			return (dword)foundEntries:Count

		public static METHOD FFirst( fileSpec as STRING , attributes as dword ) as logic
			foundEntries:Clear()
			local fileSpecification := fileSpec as string
			if attributes == 0x00000008
			   local allDrives := DriveInfo.GetDrives() as DriveInfo[]
			   foreach  Drive as DriveInfo in allDrives
				foundEntries:Add(drive)
			   next
			else
				if ((FileAttributes)attributes & FileAttributes.Directory) == FileAttributes.Directory
				   local directories := DirectoryInfo{fileSpecification}.GetDirectories() as DirectoryInfo[]
				   attributes -= (int) FileAttributes.Directory
				   attributes += (int) FileAttributes.Normal
				   var selectedDirs := FROM DirectoryInfo IN directories WHERE (DirectoryInfo:Attributes & (FileAttributes) attributes ) != 0 SELECT DirectoryInfo
				   foreach directory as DirectoryInfo in selectedDirs
					foundEntries:Add(directory)
				   next 
				else
				   attributes += (int) FileAttributes.Normal
				   local files := DirectoryInfo{fileSpecification}.GetFiles() as FileInfo[]
				   var selectedFiles := FROM FileInfo IN files WHERE ( FileInfo:Attributes & (FileAttributes) attributes) != 0 SELECT FileInfo
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

		public static METHOD FNext() as logic
			if !isAtEnd
				isAtEnd := enumerator:MoveNext()
				if !isAtEnd
					currentItem := enumerator:Current
				endif
			endif
		return isAtEnd

		public static METHOD FName() as string
			local name := "" as string
			if !isAtEnd
			   if currentItem is DriveInfo
				name := ((DriveInfo) currentItem):Name
 			    elseif (currentItem is FileInfo)
						name := ((FileInfo)currentItem):Name
				elseif (currentItem is DirectoryInfo)
						name := ((DirectoryInfo) currentItem):Name
				endif
			endif
		return name

		public static METHOD FSize() as DWORD
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
		return (DWORD) size

		public static METHOD FTime() as string
			local time := "00:00:00" as string
			if !isAtEnd
			   if (currentItem is FileInfo)
			    time := ((FileInfo)currentItem):LastWriteTime.ToString("HH:MM:ss")
			   elseif currentItem is DirectoryInfo
			    time := ((DirectoryInfo) currentItem):LastWriteTime.ToString("HH:MM:ss")
			   endif						
			endif
		return  time

		public static METHOD FDate() as DateTime
			local time := DateTime.MinValue as DateTime
			if !isAtEnd
			   if (currentItem is FileInfo)
			    time := ((FileInfo)currentItem):LastWriteTime
			   elseif currentItem is DirectoryInfo
			    time := ((DirectoryInfo) currentItem):LastWriteTime
			   endif						
			endif
		return  time


		public static METHOD FAttrib() as DWORD
			local attributes := 0x00000008 as int
			if !isAtEnd
			   if (currentItem is FileInfo)
			    attributes := (int)((FileInfo)currentItem):Attributes
			   elseif currentItem is DirectoryInfo
			    attributes := (int)((DirectoryInfo) currentItem):Attributes
			   endif						
			endif
		return  (DWORD)attributes

		[DllImport("kernel32.dll", CharSet := CharSet.Auto, SetLastError := true , EntryPoint := "CreateFileW" , ExactSpelling := true)];
		public static unsafe extern Method CreateFile(;
		 [MarshalAs(UnmanagedType.LPTStr)] filename as string,;
		 [MarshalAs(UnmanagedType.U4)] accessMode as FileAccess,;
		 [MarshalAs(UnmanagedType.U4)] share as FileShare ,;
		 securityAttributes as IntPtr,; // optional SECURITY_ATTRIBUTES struct or IntPtr.Zero
		 [MarshalAs(UnmanagedType.U4)] creationDisposition as FileMode,;
		 [MarshalAs(UnmanagedType.U4)] flagsAndAttributes as FileAttributes ,;
		 templateFile as PTR) as PTR

		[DllImport("kernel32.dll",SetLastError := true, ExactSpelling := true, CharSet := CharSet.Auto)];
		[return: MarshalAs(UnmanagedType.Bool)];
		public static extern unsafe Method WriteFile(hFile as PTR,  lpBuffer as byte[],;
			nNumberOfBytesToWrite as dword, lpNumberOfBytesWritten ref dword,;
			[In] lpOverlapped ref System.Threading.NativeOverlapped ) as logic

		[DllImport("kernel32.dll", SetLastError := true, ExactSpelling := true, CharSet := CharSet.Auto)];
		[ReliabilityContract(Consistency.WillNotCorruptState, Cer.Success)];
		[SuppressUnmanagedCodeSecurity];
		[return: MarshalAs(UnmanagedType.Bool)];
		public static unsafe extern method CloseHandle(hObject as PTR) as logic

		[DllImport("Kernel32.dll", SetLastError := true, CharSet := CharSet.Auto, ExactSpelling := true)];
		public static extern unsafe method SetFilePointer(;
        [In]  hFile as SafeFileHandle,; 
        [In]  lDistanceToMove as int,;
        [Out] lpDistanceToMoveHigh as int,;
        [In]  dwMoveMethod as EMoveMethod) as dword


		public static unsafe method FCreate( file as string ) as PTR
		return FCreate(file, 0)

		public static unsafe method FCreate( file as string , attributes as dword) as PTR
		    local mode := VOFileMode{0x100, (int)attributes} as VOFileMode
        return FCreate(file, mode)

		public static unsafe method FCreate(file as string, mode as VOFileMode) as PTR
            local handle := CreateFile(file, (FileAccess)mode.AccessCode, (FileShare)mode.ShareCode, IntPtr.Zero, (FileMode)mode.CreateCode, (FileAttributes)mode.AttributesCode, IntPtr.Zero) as PTR
            if (((IntPtr)handle):ToInt32() == -1)
                FError((dword)Marshal.GetLastWin32Error())
            else
                FError(0)
			endif
        return handle

		public static method FError( code as dword) as dword
            local lastError := errorCode as dword
            errorCode := code 
        return lastError

		public static unsafe method FWrite( handle as PTR,buffer as string) as dword	
        return FWrite(handle, buffer, (dword)buffer:Length)

		public unsafe static method FWrite( handle as PTR, buffer as string, count as dword) as dword
            local successFull := false as logic
            local bytes := System.Text.Encoding.ASCII.GetBytes(buffer) as byte[]
            local bytesRead := 0 as dword
			local overlapped := default(System.Threading.NativeOverlapped) as System.Threading.NativeOverlapped
            successFull := WriteFile(handle, bytes, count,ref bytesRead, ref overlapped)
            if !successFull 
                FError((dword)Marshal.GetLastWin32Error())
            endif
        return (dword) bytesRead

        public static unsafe method FClose(handle as PTR) as logic
            local  isClosed := CloseHandle(handle) as logic
            if (!isClosed)
                FError((dword)Marshal.GetLastWin32Error())
            endif
        return isClosed

	    public static unsafe method FSeek(handle as PTR, lOffset as long, dwOrigin as dword) as long
            local position := (int) SetFilePointer(SafeFileHandle{(IntPtr)handle,true}, lOffset, 0, (EMoveMethod)dwOrigin) as int
            if (position == System.Int32.MaxValue)
                FError((dword)Marshal.GetLastWin32Error())
            else
                FError(0)
			endif
        return (long)position

		public static unsafe method FSeek( handle as PTR, lOffset as long) as long
	    return FSeek(handle,lOffset,0)

        public static unsafe method FTell(handle as PTR) as long
        return FSeek(handle, 0, 1)

        public static unsafe method FEof(handle as PTR) as logic
            local offset := FTell(handle) as int
            local position := FSeek(handle, 0, 2) as int
            local isEof := ( offset == position ) as logic
            if !isEof
                FSeek(handle, offset, 0)
			endif
        return isEof

        public static unsafe method FRewind(handle as PTR) as int
        return (int)FSeek(handle, 0, 0)

        public static unsafe method FOpen(file as string ) as PTR
        return FOpen(file, 0)

        public static unsafe method FOpen(file as string,mode as int) as PTR
            local fileMode := VOFileMode{mode, 0} as VOFileMode 
        return FCreate(file, fileMode)

        public static method FRename( oldFile as string , newFile as string) as logic
            local renamed := true as logic
            try
                System.IO.File.Move(oldFile, newFile)
            catch ex as Exception 
                FError((dword)Marshal.GetLastWin32Error())
                renamed := false
            end try
        return renamed

        public static method MoveFile(oldFile as string,newFile as string) as logic
        return FRename(oldFile, newFile)
        
        public static method FErase(fileName as string) as logic
            local isDeleted := true as logic
            try
                System.IO.File.Delete(fileName)
            catch ex as Exception
                FError((dword)Marshal.GetLastWin32Error())
                isDeleted := false
            end try
        return isDeleted

        public static method FCopy(fileName as string,destination as string) as logic
            local isDeleted := true as logic
            try
                System.IO.File.Copy(fileName,destination,true)
            catch ex as Exception
                FError((dword)Marshal.GetLastWin32Error())
                isDeleted := false
			end try
        return isDeleted

	end class

	public structure VOFileMode
		public PROPERTY AccessCode		as int auto
		public PROPERTY AttributesCode	as int auto
		public PROPERTY CreateCode		as int auto
		public PROPERTY ShareCode		as int auto
				
		constructor(mode as int,attribs as int)
			/// see File.C ( __PrepareMode )
			if (mode & 0x10000) == 0x10000
				mode -= 0x10000
			endif

			if attribs > 0 
				AttributesCode := attribs
			else
				AttributesCode := FileAttributes.Normal
			endif
			AccessCode := unchecked((int)0x80000000)

			if ((mode & 2) == 2)
				AccessCode += 0x40000000
			elseif ((mode & 1) == 1)
				AccessCode := 0x40000000
			endif

			CreateCode := 3
			if ((mode & 0x1000) == 0x1000)
				CreateCode := 2
				AccessCode := unchecked((int)0xc0000000)
			endif
			ShareCode := 0
			local num := 0x70 as int
			switch (mode & num)
				case 0x40
					ShareCode := 3
				case 0x20
					ShareCode := 1
				case 0x30
					ShareCode := 2
				case 0x10
					ShareCode := 0
				case 0
					ShareCode := 0
			end switch
		return

	end structure

	enum EMoveMethod as dword
		@@Begin := 0
		@@Current := 1
		@@End := 2
	end enum


	#region functions
	/// <summary>
	/// Return the number of files that match a given file specification and attribute.
	/// </summary>
	/// <param name="pszFile"></param>
	/// <param name="nAttr"></param>
	/// <returns>
	/// </returns>
	FUNCTION FFCount(pszFile AS STRING,nAttr AS DWORD) AS DWORD
		return XSharp.IO.FileHelper.FFCount(pszFile,nAttr) 

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
	unsafe FUNCTION FChSize(pFile AS PTR,nOffset AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Close an open file and write the buffers to disk.
	/// </summary>
	/// <param name="pFile"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION FClose(pFile AS PTR) AS LOGIC
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
	unsafe FUNCTION FFLock(pHandle AS PTR,dwOffset AS DWORD,dwLength AS DWORD) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Flush to disk a file opened with a low-level file function.
	/// </summary>
	/// <param name="phandle"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION FFlush(phandle AS PTR) AS VOID
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
	unsafe FUNCTION FFUnLock(phandle AS PTR,dwOffset AS DWORD,dwLength AS DWORD) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Read a line from an open file, specifying two strongly typed arguments.
	/// </summary>
	/// <param name="pFile"></param>
	/// <param name="nBuffLen"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION FGetS2(pFile AS PTR,nBuffLen AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty   

	/// <summary>
	/// </summary>
	/// <param name="ptrBuff"></param>
	/// <param name="nLen"></param>
	/// <param name="nDec"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION FieldVal(ptrBuff AS PTR,nLen AS INT,nDec AS INT) AS Real8
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="ptrUsual"></param>
	/// <param name="dwLen"></param>
	/// <param name="dwDec"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION __VOFloat2Str(ptrUsual AS PTR,dwLen AS DWORD,dwDec AS DWORD) AS String
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
	unsafe FUNCTION FPutS3(pFILE AS PTR,c AS STRING,nCount AS DWORD) AS DWORD
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
	unsafe FUNCTION FRead(pHandle AS PTR,refC AS OBJECT,dwCount AS DWORD) AS DWORD
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
	unsafe FUNCTION FRead3(pHandle AS PTR,ptrBuffer AS PTR,dwCount AS DWORD) AS DWORD
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
	unsafe FUNCTION FRead4(pHandle AS PTR,ptrBuffer AS PTR,dwCount AS DWORD,lAnsi AS LOGIC) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Read a line from an open file, specifying two strongly-typed arguments.
	/// </summary>
	/// <param name="pFile"></param>
	/// <param name="nBuffLen"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION FReadLine2(pFile AS PTR,nBuffLen AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty   

	/// <summary>
	/// Read characters from a file.
	/// </summary>
	/// <param name="pHandle"></param>
	/// <param name="dwCount"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION FReadStr(pHandle AS PTR,dwCount AS DWORD) AS STRING
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
	unsafe FUNCTION FReadText(pHandle AS PTR,refC AS OBJECT,dwCount AS DWORD) AS DWORD
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
	unsafe FUNCTION FReadText3(pHandle AS PTR,ptrBuffer AS PTR,dwCount AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Set the file pointer at the top of an open file.
	/// </summary>
	/// <param name="pFile"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION FRewind(pFile AS PTR) AS VOID
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
	unsafe FUNCTION FSeek3(pHandle AS PTR,lOffset AS LONG,dwOrigin AS DWORD) AS LONG
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the current position of the file pointer.
	/// </summary>
	/// <param name="pHandle"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION FTell(pHandle AS PTR) AS LONG
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
	unsafe FUNCTION FWrite3(pHandle AS PTR,ptrBuffer AS PTR,dwCount AS DWORD) AS DWORD
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
	unsafe FUNCTION FWrite4(pHandle AS PTR,ptrBuffer AS PTR,dwCount AS DWORD,lAnsi AS LOGIC) AS DWORD
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
	unsafe FUNCTION FWriteLine3(pFILE AS PTR,c AS STRING,nCount AS DWORD) AS DWORD
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
	unsafe FUNCTION FWriteText3(pHandle AS PTR,ptrBuffer AS PTR,dwCount AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	#endregion
end namespace



	/// <summary>
	/// Remove spaces from a file name specified as a string.
	/// </summary>
	/// <param name="cName"></param>
	/// <returns>
	/// </returns>
	FUNCTION AdjustFName(cName AS STRING) AS STRING
		local adjusted := null as string
		if ( !string.IsNullOrEmpty(cName) ) 
			adjusted := System.IO.Path.GetFileNameWithoutExtension(cName):TrimEnd()
			if ( cName:IndexOf('.') > 0 ) 
				adjusted += System.IO.Path.GetExtension(cName)
			endif
		endif
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
unsafe FUNCTION FCreate2(cFile AS STRING,dwAttr AS DWORD) AS PTR
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
return System.IO.File.Exists(cFile)


/// <summary>
/// Open a file, specifying two strongly-typed arguments.
/// </summary>
/// <param name="cName"></param>
/// <param name="dwMode"></param>
/// <returns>
/// </returns>
unsafe FUNCTION FOpen2(cName AS STRING,dwMode AS DWORD) AS PTR
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
unsafe FUNCTION FxOpen(cFile AS STRING,dwMode AS DWORD,cPath AS STRING) AS PTR
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


