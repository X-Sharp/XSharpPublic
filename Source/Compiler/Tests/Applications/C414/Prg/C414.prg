// 414. compiler crash with dll that contains same named namespace with different casing
#using aa.bb.cc.dd
FUNCTION Start() AS VOID
LOCAL o1 AS test1
o1 := test1{}
? o1:ToString()

LOCAL o2 AS test2
o2 := test2{}
? o2:ToString()

