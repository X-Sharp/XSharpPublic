FUNCTION Start() AS VOID
   ? UnsafeMethods.GetByte2of4(0x10203040) // 32 (hex 20)

CLASS UnsafeMethods
   UNSAFE STATIC METHOD GetByte2of4(d AS DWORD) AS BYTE
      LOCAL p AS BYTE PTR
      p := (BYTE PTR) @d // get direct pointer to the data
       RETURN p[3]
END CLASS
