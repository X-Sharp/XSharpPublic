USING System.Collections.Generic
USING System.Reflection
USING System.Resources
USING System.IO

FUNCTION Start( ) AS VOID
	LOCAL rm AS ResourceManager
	LOCAL aFiles AS STRING[]
	rm := ResourceManager{"VOSDK", Assembly.GetExecutingAssembly()}
	aFiles := (STRING[]) rm:GetObject("$filelist$")
	? aFiles:Length
	FOR LOCAL n := 1 AS INT UPTO aFiles:Length
		LOCAL aBytes AS BYTE[]
		aBytes := (BYTE[])rm:GetObject(aFiles[n])
		? aFiles[n]
		? aBytes:Length, "bytes"
		LOCAL cFile AS STRING
		cFile := "c:\test" + aFiles[n]
		Directory.CreateDirectory(FileInfo{cFile}:DirectoryName)
		File.WriteAllBytes(cFile , aBytes)
	NEXT
	
RETURN
