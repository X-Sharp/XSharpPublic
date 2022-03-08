// 842. Incorrect warnings with conversions when /vo4 or /vo11 are enabled
// https://github.com/X-Sharp/XSharpPublic/issues/987
// /vo4+ /vo11+ /warnings as errors
FUNCTION Start( ) AS VOID
LOCAL d := 1 AS DWORD
LOCAL w := 1 AS WORD
LOCAL b := 1 AS BYTE
LOCAL n := 1 AS INT

w := 1 + w 
w := 1 - w 
w := w - 1 

b := b + 1
b := 1 - b

d := 1 - d // warning XS9021: Signed/unsigned conversions from 'dword' to 'int' 
d := d + 1
d := 1 + d // warning XS9021: Signed/unsigned conversions from 'dword' to 'int' 
d := 1 + d - 2 // warning XS9021: Signed/unsigned conversions from 'dword' to 'int' 

d += 100 + d // warning XS9021: Signed/unsigned conversions from 'dword' to 'int' 

d := d - 1
d := d - (16 + 8 * d)  // warning XS9021: Signed/unsigned conversions from 'dword' to 'int' 

LOCAL u AS USUAL
u := n + INT( d )// warning XS9021: Signed/unsigned conversions from 'dword' to 'int' 
u := n + (INT) d // warning XS9021: Signed/unsigned conversions from 'dword' to 'int' 

u := d + DWORD( n )// warning XS9021: Signed/unsigned conversions from 'int' to 'dword'
u := d + (DWORD) n // warning XS9021: Signed/unsigned conversions from 'int' to 'dword'

u := 123 * d // warning XS9021: Signed/unsigned conversions from 'dword' to 'int' 
u := 123 + d // warning XS9021: Signed/unsigned conversions from 'dword' to 'int' 
u := 123 * n

u := (10*d)-9 // warning XS9021: Signed/unsigned conversions from 'dword' to 'int' 

? u
