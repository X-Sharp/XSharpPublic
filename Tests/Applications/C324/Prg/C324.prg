// error XS0266: Cannot implicitly convert type 'short' to 'byte'. An explicit conversion exists (are you missing a cast?)
// error XS0266: Cannot implicitly convert type 'int' to 'uint64'. An explicit conversion exists (are you missing a cast?)
// Small incompatibility with vulcan, which compiles the following with none /vo compatibility options enabled
FUNCTION Start() AS VOID
LOCAL nByte AS BYTE
LOCAL l := TRUE AS LOGIC
nByte := iif(l,10,0)
? nByte

LOCAL nUInt64 AS UInt64
nUInt64 := (LONG) ((Double)nUInt64 / 256)
? nUInt64
