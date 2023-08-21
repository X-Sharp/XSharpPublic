// 844. Problem with iif() and NULL - /vo10 disabled
// error XS0029: Cannot implicitly convert type 'object' to 'string'
#pragma options("vo10", off)
#pragma warnings(219, off) // assigned but not used

FUNCTION Start() AS VOID
LOCAL c AS STRING
c := iif(FALSE, "", NULL)  // error XS0029
c := iif(FALSE, NULL, NULL)// error XS0029 (twice)
c := iif(FALSE, NULL, "")  // error XS0029

LOCAL o AS OBJECT
o := iif(TRUE,SomeClass{},NULL)
o := iif(TRUE,NULL,SomeClass{})

LOCAL s AS SomeClass
s := iif(TRUE,SomeClass{},NULL)
s := iif(TRUE,NULL,SomeClass{})

CLASS SomeClass
	PROPERTY MyProp1 AS SomeClass GET iif(SELF==NULL,SELF,NULL)
	PROPERTY MyProp2 AS SomeClass GET iif(SELF==NULL,NULL,SELF)
END CLASS
