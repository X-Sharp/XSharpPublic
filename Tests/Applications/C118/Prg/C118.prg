// 118. compiler crash after Assertion Failed: binding should be enclosed in a conditional access
FUNCTION Start() AS VOID
LOCAL n AS INT
LOCAL s AS STRING
n := 123
s := ( n ):ToString()
? s

