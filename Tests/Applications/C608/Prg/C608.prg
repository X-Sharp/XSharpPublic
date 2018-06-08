// 609. error XS9059: Cannot convert Array Index from 'string' to 'int'.
/*
Following is a cut down sample from the jazz code. It gets preprocessed to:

? aaa["A"]=="A"

instead of the intended

? aaa[1]=="A"

that's due to the same word used in different casing "key" and "KEY" for different things in the preprocessor commands

Previous compiler versions did not even complain about this, but now at least in build 122
an error is reported that cannot use a string as ARRAY index. The original preprocessor
problem that generates the wrong code still remains of course.

Note that the result is the same no matter if /vo8 (compatible preprocessor behavior) is on or off
*/

#define KEY 1
#define Test(key) aaa[KEY]==key

FUNCTION Start() AS VOID
	LOCAL aaa := {"a","B","c"} AS ARRAY
	? Test("A")
RETURN
