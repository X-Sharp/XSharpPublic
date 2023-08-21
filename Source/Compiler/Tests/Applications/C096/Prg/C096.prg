// 96. error XS0571: 'ArrayList.this[int].get': cannot explicitly call operator or accessor

// that was part of code probably more than 10 years old, from the time that the vulcan (or cule?) compiler did not support accessing indexed properties
// not saying we should support this, just mentioning it because I had a lot of code using this!
FUNCTION Start() AS VOID
LOCAL a AS System.Collections.ArrayList
a := System.Collections.ArrayList{}
a:Add(17)
? a:get_Item(0)

