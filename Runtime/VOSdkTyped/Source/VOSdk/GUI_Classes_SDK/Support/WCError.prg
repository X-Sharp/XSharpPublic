

CLASS WCError INHERIT Error

CONSTRUCTOR(methodName, className, desc, var, varnum, lAllowIgnore) 
	LOCAL rsSubSystem AS ResourceString
	SUPER()

	rsSubSystem := ResourceString{__WCSLibraryName}
	SELF:SubSystem := rsSubSystem:Value

	IF IsNil(methodName) .AND. IsNil(className)
		FuncSym := String2Symbol(ProcName(1))
	ELSEIF IsNil(className)
		FuncSym := methodName
	ELSE
		FuncSym := String2Symbol(Symbol2String(className) + ":" + Symbol2String(methodName))
	ENDIF

	Default(@desc, __WCSInterfaceError)
	Default(@lAllowIgnore, TRUE)

	IF IsString(desc)
		SELF:Description := desc
	ELSE
		SELF:Description := ResourceString{desc}:Value
	ENDIF
	CanDefault := lAllowIgnore

	IF !IsNil(var) .OR. !IsNil(varnum)
		Arg:=AsString(var)
		ArgType:=UsualType(var)
	ENDIF

	IF !IsNil(varnum)
		ArgNum:=varnum
	ENDIF

	Args := <OBJECT>{}
	Tries := 1
	RETURN 

END CLASS

