using System.IO
FUNCTION Start as VOID
    var hFile := Fopen("C:\Test\customers.dbf") 
    var oStream := FGetStream(hFile) 
    ? hFile, oStream
    var hSafePtr := oStream:SafeFileHandle
    var oStream2 := FileStream{hSafePtr, FileAccess.ReadWrite}
    oStream:Position := 100
    oStream2:Position := 200
    ? oStream:Position, oStream2:Position
    ? oStream:Length, oStream2:Length
    FClose(hFile)
    ? oStream:Position, oStream2:Position
    ? oStream:Length, oStream2:Length

    wait


