//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
#using System
#using System.Collections
#using System.IO
#using System.Linq
#using System.Runtime.InteropServices

begin namespace XSharp.IO

	private static class FileHelper
        static foundEntries	:= ArrayList{} as ArrayList
        static enumerator   := null as IEnumerator 
        static currentItem	:= null as object
        static isAtEnd		:= true as logic
        static int errorCode:= 0 as int

		private static METHOD FFCount( fileSpec as PSZ , attributes as int ) as int
			FFirst(fileSpec,attributes)
		return foundEntries:Count

		private static METHOD FFirst( fielSpec as PSZ , attributes as int )
			foundEntries:Clear()
			local fileSpecification := Marshal.PtrToStringAnsi(fileSpec) as string
			if attributes == 0x00000008
			   local allDrives =: DriveInfo.GetDrives() as DriveInfo[]
			   foreach  Drive as DriveInfo in allDrives
				foundEntries:Add(drive)
			   next
			else
				if ((FileAttributes)attributes & FileAttibutes.Directory) == FileAttibutes.Directory
				   local directories := DirectoryInfo{fileSpecification}.GetDirectories()
				   attributes -= (int) FileAttibutes.Directory
				   attributes += (int) FileAttibutes.Normal
				   var selectedDirs := FROM DirectoryInfo IN directories WHERE (DirectoryInfo:Attributes & (FileAttributes) attributes ) != 0 SELECT DirectoryInfo
				   foreach directory as DirectoryInfo in selectedDirs
					foundEntries:Addd(directory)
				   next 
				else
				   attributes += (int) FileAttibutes.Normal
				   local files := DirectoryInfo{fileSpecification}.GetFiles()
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

		private static METHOD FNext() as logic
			if !isAtEnd
				isAtEnd := enumerator:MoveNext()
				if !isAtEnd
					currentItem := enumerator:Current
				endif
			endif
		return isAtEnd

		private static METHOD FName() as string
			local name := "" as string
			if !isAtEnd
			   if currentItem is DriveInfo
				name := ((DriveInfo) currentInfo):Name
			   elseif currentItem is FileInfo)
			    name := ((FileInfo)currentItem):Name
			   elseif currentItem is DirectoryInfo
			    name := ((DirectoryInfo) currentItem):Name
			   endif						
			endif
		return name

		private static METHOD FSize() as DWORD
			local size := 0 as LONG
			if !isAtEnd
			   if currentItem is DriveInfo
				size := ((DriveInfo) currentInfo):TotalSize
			   elseif currentItem is FileInfo)
			    size := ((FileInfo)currentItem):Length
			   elseif currentItem is DirectoryInfo
			    size := ((DirectoryInfo) currentItem):GetFileSystemInfos().LongLength
			   endif						
			endif
		return (DWORD) size

		private static METHOD FTime() as string
			local time := "00:00:00" as string
			if !isAtEnd
			   if currentItem is FileInfo)
			    time := ((FileInfo)currentItem):LastWriteTime.ToString("HH:MM:ss")
			   elseif currentItem is DirectoryInfo
			    time := ((DirectoryInfo) currentItem):LastWriteTime.ToString("HH:MM:ss")
			   endif						
			endif
		return  time

		private static METHOD FDate() as DATE
			local time := DateTime.MinValue as DateTime
			if !isAtEnd
			   if currentItem is FileInfo)
			    time := ((FileInfo)currentItem):LastWriteTime
			   elseif currentItem is DirectoryInfo
			    time := ((DirectoryInfo) currentItem):LastWriteTime
			   endif						
			endif
		return  Vulcan.__VODate{time}


		private static METHOD FAttrib() as DWORD
			local attributes := 0x00000008 as int
			if !isAtEnd
			   if currentItem is FileInfo)
			    attributes := (int)((FileInfo)currentItem):Attributes
			   elseif currentItem is DirectoryInfo
			    attributes := (int)((DirectoryInfo) currentItem):Attributes
			   endif						
			endif
		return  (DWORD)attributes

		[DllImport("kernel32.dll", CharSet := CharSet.Auto, SetLastError := true , EntryPoint := "CreateFileW" , ExactSpelling := true)]
		public static unsafe extern CreateFile(
		 [MarshalAs(UnmanagedType.LPTStr)] filename as string,
		 [MarshalAs(UnmanagedType.U4)] accessMode as FileAccess,
		 [MarshalAs(UnmanagedType.U4)] share as FileShare ,
		 IntPtr securityAttributes, // optional SECURITY_ATTRIBUTES struct or IntPtr.Zero
		 [MarshalAs(UnmanagedType.U4)] creationDisposition as FileMode,
		 [MarshalAs(UnmanagedType.U4)] flagsAndAttributes as FileAttributes ,
		 templateFile as PTR) as PTR

		[DllImport("kernel32.dll",SetLastError := true, ExactSpelling := true, CharSet := CharSet.Auto)]
		[return: MarshalAs(UnmanagedType.Bool)]
		static extern bool WriteFile(hFile as PTR,  lpBuffer as byte[],
			nNumberOfBytesToWrite as dword, out lpNumberOfBytesWritten as dword,
			[In] lpOverlapped ref System.Threading.NativeOverlapped )

		[DllImport("kernel32.dll", SetLastError := true, ExactSpelling := true, CharSet := CharSet.Auto)]
		[ReliabilityContract(Consistency.WillNotCorruptState, Cer.Success)]
		[SuppressUnmanagedCodeSecurity]
		[return: MarshalAs(UnmanagedType.Bool)]
		internal static unsafe extern CloseHandle(hObject as PTR) as logic

		[DllImport("Kernel32.dll", SetLastError := true, CharSet := CharSet.Auto, ExactSpelling := true)]
		static extern uint SetFilePointer(
        [In]  hFile as SafeFileHandle, 
        [In]  lDistanceToMove as int,
        [Out] out lpDistanceToMoveHigh as int,
        [In]  dwMoveMethod as EMoveMethod)


		public static method FCreate( file as string ) as PTR
		return FCreate(file, 0)

		public static method FCreate( file as string , attributes as dword) as PTR
		    VOFileMode mode := new VOFileMode(0x100, attributes)
        return FCreate(file, mode)

		public static method FCreate(file as string, mode as VOFileMode) as PTR
            local handle := CreateFile(file, mode.AccessCode, mode.ShareCode, IntPtr.Zero, mode.CreateCode, mode.AttributesCode, IntPtr.Zero) as PTR
            if (handle:ToInt32() == -1)
                FError((int)Marshal.GetLastWin32Error())
            else
                FError(0)
			endif
        return handle

		public static FError( code as dword) as dword
            local lastError := errorCode as dword
            errorCode := code 
        return lastError

		public static FWrite( handle as PTR,string buffer as string) as dword	
        return FWrite(handle, buffer, buffer:Length)

		public unsafe static FWrite( handle as PTR, buffer as string, count as dword) as dword
            local successFull := false as logic
            local bytes := System.Text.Encoding.ASCII.GetBytes(buffer) as byte[]
            local bytesRead := 0 
            successFull := WriteFile(handle, bytes, count, out bytesRead, 0)
            if !successFull 
                FError((int)Marshal.GetLastWin32Error())
            endif
            return (dword) bytesRead
        }

        public static FClose(handle as PTR) as logic
            local  isClosed := CloseHandle(handle) as logic
            if (!isClosed)
                FError((int)Marshal.GetLastWin32Error())
            endif
        return isClosed

	    public static unsafe FSeek(handle as PTR, lOffset as long, dwOrigin as dword) as long
            local position := SetFilePointer(handle, lOffset, IntPtr.Zero, dwOrigin) as int
            if (position == int.MaxValue)
                FError((int)Marshal.GetLastWin32Error())
            else
                FError(0)
			endif
        return (long)position

		public static unsafe FSeek( handle as PTR, lOffset as long) as long
	    return FSeek(handle,lOffset,0)

        public static unsafe FTell(handle as PTR) as long
        return FSeek(handle, 0, 1)

		private structure VOFileMode
		        public AccessCode		as int auto
		        public AttributesCode	as int auto
		        public CreateCode		as int auto
				public ShareCode		as int auto
				
				constructor(mode as int,atrribs as int)
					/// see File.C ( __PrepareMode )
					if (mode & 0x10000) == 0x10000
						mode -= 0x10000
					endif

					if attribs > 0 
					   AttributesCode := attribs
					else
					   AttributesCode := FileAttributes.Normal
					endif
					AccessCode = unchecked((int)0x80000000)

					if ((inMode & 2) == 2)
						AccessCode += 0x40000000
					else if ((mode & 1) == 1)
						AccessCode := 0x40000000
					endif

					CreateCode = 3
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


		end class

		private enum EMoveMethod as dword
			@@Begin := 0,
			@@Current := 1,
			@@End := 2
		end enum

	end class

	#region functions
	/// <summary>
	/// Return the number of files that match a given file specification and attribute.
	/// </summary>
	/// <param name="pszFile"></param>
	/// <param name="nAttr"></param>
	/// <returns>
	/// </returns>
	FUNCTION FFCount(pszFile AS PSZ,nAttr AS DWORD) AS DWORD
	return FileHelper.FFCount(pszFile,nAttr) 

	/// <summary>
	/// Find the first file that matches a given file specification or attribute.
	/// </summary>
	/// <param name="pszFile"></param>
	/// <param name="nAttr"></param>
	/// <returns>
	/// </returns>
	FUNCTION FFirst(pszFile AS PSZ,nAttr AS DWORD) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE 

	/// <summary>
	/// Determine the attributes of the file found after FFCount(), FFirst(), or FNext().
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION FAttrib() AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the number of fields in the current database file.
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION FCount() AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the date stamp of the file found by FFCount(), FFirst(), or FNext().
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION FDate() AS DATE
		/// THROW NotImplementedException{}
	RETURN (DATE)0   

	/// <summary>
	/// Get or set the error code for a file operation.
	/// </summary>
	/// <param name="nSet"></param>
	/// <returns>
	/// </returns>
	FUNCTION FError(nSet AS USUAL) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Read a line from an open file.
	/// </summary>
	/// <param name="pFile"></param>
	/// <param name="nBuffLen"></param>
	/// <returns>
	/// </returns>
	FUNCTION FGetS(pFile AS USUAL,nBuffLen AS USUAL) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Change the size of a file opened with a low-level file function.
	/// </summary>
	/// <param name="pFile"></param>
	/// <param name="nOffset"></param>
	/// <returns>
	/// </returns>
	FUNCTION FChSize(pFile AS PTR,nOffset AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Close an open file and write the buffers to disk.
	/// </summary>
	/// <param name="pFile"></param>
	/// <returns>
	/// </returns>
	FUNCTION FClose(pFile AS PTR) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

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
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Lock a portion of an open file.
	/// </summary>
	/// <param name="pHandle"></param>
	/// <param name="dwOffset"></param>
	/// <param name="dwLength"></param>
	/// <returns>
	/// </returns>
	FUNCTION FFLock(pHandle AS PTR,dwOffset AS DWORD,dwLength AS DWORD) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Flush to disk a file opened with a low-level file function.
	/// </summary>
	/// <param name="phandle"></param>
	/// <returns>
	/// </returns>
	FUNCTION FFlush(phandle AS PTR) AS VOID
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
	FUNCTION FFUnLock(phandle AS PTR,dwOffset AS DWORD,dwLength AS DWORD) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Read a line from an open file, specifying two strongly typed arguments.
	/// </summary>
	/// <param name="pFile"></param>
	/// <param name="nBuffLen"></param>
	/// <returns>
	/// </returns>
	FUNCTION FGetS2(pFile AS PTR,nBuffLen AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// </summary>
	/// <param name="ptrBuff"></param>
	/// <param name="nLen"></param>
	/// <param name="nDec"></param>
	/// <returns>
	/// </returns>
	FUNCTION FieldVal(ptrBuff AS PTR,nLen AS INT,nDec AS INT) AS FLOAT
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="ptrUsual"></param>
	/// <param name="dwLen"></param>
	/// <param name="dwDec"></param>
	/// <returns>
	/// </returns>
	FUNCTION Float2Psz(ptrUsual AS PTR,dwLen AS DWORD,dwDec AS DWORD) AS PSZ
		/// THROW NotImplementedException{}
	RETURN NULL   

	/// <summary>
	/// Write a string, a carriage-return character, and a linefeed character to an open file, specifying three strongly-typed arguments.
	/// </summary>
	/// <param name="pFILE"></param>
	/// <param name="c"></param>
	/// <param name="nCount"></param>
	/// <returns>
	/// </returns>
	FUNCTION FPutS3(pFILE AS PTR,c AS STRING,nCount AS DWORD) AS DWORD
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
	FUNCTION FRead(pHandle AS PTR,refC AS USUAL,dwCount AS DWORD) AS DWORD
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
	FUNCTION FRead3(pHandle AS PTR,ptrBuffer AS PTR,dwCount AS DWORD) AS DWORD
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
	FUNCTION FRead4(pHandle AS PTR,ptrBuffer AS PTR,dwCount AS DWORD,lAnsi AS LOGIC) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Read a line from an open file, specifying two strongly-typed arguments.
	/// </summary>
	/// <param name="pFile"></param>
	/// <param name="nBuffLen"></param>
	/// <returns>
	/// </returns>
	FUNCTION FReadLine2(pFile AS PTR,nBuffLen AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Read characters from a file.
	/// </summary>
	/// <param name="pHandle"></param>
	/// <param name="dwCount"></param>
	/// <returns>
	/// </returns>
	FUNCTION FReadStr(pHandle AS PTR,dwCount AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Read characters from a file into a buffer variable that is passed by reference.
	/// </summary>
	/// <param name="pHandle"></param>
	/// <param name="refC"></param>
	/// <param name="dwCount"></param>
	/// <returns>
	/// </returns>
	FUNCTION FReadText(pHandle AS PTR,refC AS USUAL,dwCount AS DWORD) AS DWORD
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
	FUNCTION FReadText3(pHandle AS PTR,ptrBuffer AS PTR,dwCount AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Set the file pointer at the top of an open file.
	/// </summary>
	/// <param name="pFile"></param>
	/// <returns>
	/// </returns>
	FUNCTION FRewind(pFile AS PTR) AS VOID
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
	FUNCTION FSeek3(pHandle AS PTR,lOffset AS LONG,dwOrigin AS DWORD) AS LONG
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the current position of the file pointer.
	/// </summary>
	/// <param name="pHandle"></param>
	/// <returns>
	/// </returns>
	FUNCTION FTell(pHandle AS PTR) AS LONG
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="ptrFunc"></param>
	/// <returns>
	/// </returns>
	FUNCTION FunctionPtr2Sym(ptrFunc AS PTR) AS SYMBOL
		/// THROW NotImplementedException{}
	RETURN NULL_SYMBOL   

	/// <summary>
	/// Write the contents of a buffer to an open file.
	/// </summary>
	/// <param name="pHandle"></param>
	/// <param name="ptrBuffer"></param>
	/// <param name="dwCount"></param>
	/// <returns>
	/// </returns>
	FUNCTION FWrite3(pHandle AS PTR,ptrBuffer AS PTR,dwCount AS DWORD) AS DWORD
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
	FUNCTION FWrite4(pHandle AS PTR,ptrBuffer AS PTR,dwCount AS DWORD,lAnsi AS LOGIC) AS DWORD
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
	FUNCTION FWriteLine3(pFILE AS PTR,c AS STRING,nCount AS DWORD) AS DWORD
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
	FUNCTION FWriteText3(pHandle AS PTR,ptrBuffer AS PTR,dwCount AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	#endregion
end namespace