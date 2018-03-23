USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text

BEGIN NAMESPACE ConsoleApplication1

	FUNCTION Start() AS VOID
		local hFile as IntPtr
		local sFile as string
		local sBlock as string
		local aBytes as byte[]
		sFile := "D:\Test.txt"
		? DateTime.Now
		for var j := 1 to 100
			hFile := FCreate2(sFile,0)
			sBlock := Replicate("X", 1000)
			aBytes := String2Bytes(sBlock)
			for var i := 1 to 1000
				FWrite3(hFile, aBytes, (DWORD) aBytes:Length)
			next
			FEof(hFile), FTell(hFIle)
			FCLose(hFile)
		next
		? DateTime.Now
		hFile := Fopen2(sFile, FO_READWRITE)
		? FSeek3(hFile, 0, FS_END)
		? FTell(hFile)
		FChSize(hFile, 10)
		? FSeek3(hFile, 0, FS_END)
		? FTell(hFile)
		? FClose(hFile)
		Console.Read()
END NAMESPACE
