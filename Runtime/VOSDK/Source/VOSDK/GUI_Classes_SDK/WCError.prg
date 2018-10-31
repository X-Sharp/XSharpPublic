PARTIAL CLASS WCError INHERIT error

CONSTRUCTOR(methodName, className, desc, VAR, varnum, lAllowIgnore) 
	LOCAL rsSubSystem AS ResourceString
    //RvdH 080609 Added call to super:Init to correctly fill the callstack
    SUPER()

	rsSubSystem := ResourceString{__WCSLibraryName}
	subsystem := rsSubSystem:Value

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
		description := desc
	ELSE
		description := ResourceString{desc}:Value
	ENDIF
	CanDefault := lAllowIgnore

	IF !IsNil(VAR) .OR. !IsNil(varnum)
		arg:=AsString(VAR)
		argtype:=UsualType(VAR)
	ENDIF

	IF !IsNil(varnum)
		Argnum:=varnum
	ENDIF

	args := {}
	tries := 1
	RETURN 

//RvdH 080814 Not needed. Also in parent class
//METHOD @@Throw() 
//	RETURN Eval(ErrorBlock(), SELF)
END CLASS

