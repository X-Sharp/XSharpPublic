// 882. Ambiguous call error with /vo4 in the VO dialect
// https://github.com/X-Sharp/XSharpPublic/issues/1211
// VO dialect, /vo4+
// If either vo4 is disabled, or Core dialect is used, then there are no errors at all
/*
error XS0121: The call is ambiguous between the following methods or properties: 'System.Math.Max(sbyte, sbyte)' and 'System.Math.Max(int, int)'
error XS0121: The call is ambiguous between the following methods or properties: 'System.Array.Copy(System.Array, int, System.Array, int, int)' and 'System.Array.Copy(System.Array, int64, System.Array, int64, int64)'
*/
FUNCTION Start( ) AS VOID
LOCAL dwValue := 1 AS DWORD
? Math.Max(dwValue, 2) // error
? Math.Max(2, dwValue) // error

LOCAL a := <BYTE>{} AS BYTE[]
LOCAL buffer := <BYTE>{} AS BYTE[]
LOCAL i64 := 0 AS INT64
LOCAL i32 := 0 AS INT
System.Array.Copy(a, i32, buffer, i32, 0) // OK
System.Array.Copy(a, i64, buffer, i64, 0) // error
System.Array.Copy(a, 0, buffer, i32, 0)  // OK
System.Array.Copy(a, 0, buffer, i64, 0)  // error

// the following I don't like them, but the Core dialect, or VO dialect with /vo4-, and also c# allow it with bo errors
System.Array.Copy(a, i32, buffer, i64, i32) // error
System.Array.Copy(a, i32, buffer, i32, i64) // error
System.Array.Copy(a, i64, buffer, i64, i32) // error

