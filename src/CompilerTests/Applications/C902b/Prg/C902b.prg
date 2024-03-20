// 902. Macro compiler problem passing more than 2 arguments by reference (test b)
// https://www.xsharp.eu/forum/topic?p=29165#p29165
// https://github.com/X-Sharp/XSharpPublic/issues/1445

FUNCTION Start() AS VOID STRICT
	PRIVATE oa
	PRIVATE ca
	PRIVATE cb
	PRIVATE cc
	
	LOCAL cCmd	AS STRING
	
	oa := a{}
	
	? "Direct call 2 parameters:"	
	ca	:= ""
	cb	:= NULL_DATE
	oa:CallByRef2(@ca,@cb,@cc)
	? ca
	? cb
	?
	
	? "Direct call 3 parameters:"	
	ca	:= ""
	cb	:= NULL_DATE
	cc	:= NULL_DATE
	oa:CallByRef3(@ca,@cb,@cc)
	? ca
	? cb
	? cc
	?
	
	? "Indirect call 2 parameters:"	
	ca	:= ""
	cb	:= NULL_DATE
	cCmd := "oa:CallByRef2(@ca,@cb)"
	Eval(&("{||"+cCmd+"}"))
	? ca
	? cb
	?
	
	? "Indirect call 3 parameters, third by value:"	
	ca	:= ""
	cb	:= NULL_DATE
	cc	:= NULL_DATE
	cCmd := "oa:CallByRef3a(@ca,@cb,cc)"
	Eval(&("{||"+cCmd+"}"))
	? ca
	? cb
	? cc
	?
	
	? "Indirect call 3 parameters, third by ref:"	
	ca	:= ""
	cb	:= NULL_DATE
	cc	:= NULL_DATE
	cCmd := "oa:CallByRef3(@ca,@cb,@cc)"
	Eval(&("{||"+cCmd+"}"))
	? ca
	? cb
	? cc
	?
	
	
RETURN

CLASS a

METHOD CallByRef2(a REF STRING,b REF DATE) AS LOGIC
LOCAL lOk	AS LOGIC

a := "OK"
b	:= Today()
lOk := TRUE

RETURN lOk

METHOD CallByRef3(a REF STRING,b REF DATE,c REF DATE) AS LOGIC
LOCAL lOk	AS LOGIC

a := "OK"
b	:= Today()
c := Today()+1
lOk := TRUE

RETURN lOk

METHOD CallByRef3a(a REF STRING,b REF DATE,c AS DATE) AS LOGIC
LOCAL lOk	AS LOGIC

a := "OK"
b	:= Today()
c := Today()+1
lOk := TRUE

RETURN lOk

END CLASS

