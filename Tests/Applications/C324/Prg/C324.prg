// error XS0266: Cannot implicitly convert type 'short' to 'byte'. An explicit conversion exists (are you missing a cast?)
// error XS0266: Cannot implicitly convert type 'int' to 'uint64'. An explicit conversion exists (are you missing a cast?)
// Small incompatibility with vulcan, which compiles the following with none /vo compatibility options enabled
FUNCTION Start() AS VOID
LOCAL nByte AS BYTE
LOCAL l := TRUE AS LOGIC
nByte := iif(l,10,0)
? nByte

LOCAL nUInt64 := 512 AS UInt64                
// The next line generates a XS9021 Signed/unsigned conversions from 'int' to 'uint64' may lead to loss of data or overflow errors
nUInt64 := (LONG) ((Double)nUInt64 / 256)    
? nUInt64

LOCAL dw as SHORT
dw := ~ TestMe()
? dw
RETURN


Function TestMe as SHORT
	RETURN 1
