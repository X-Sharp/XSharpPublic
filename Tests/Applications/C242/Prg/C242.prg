// 242. error XS0266: Cannot implicitly convert type 'object' to 'System.Collections.ArrayList'. An explicit conversion exists (are you missing a cast?)\
// compile with /lb+
FUNCTION Start() AS VOID
LOCAL o AS System.Collections.ArrayList
LOCAL oo AS OBJECT
o := System.Collections.ArrayList{}
oo := o
o := oo
? o, oo 

