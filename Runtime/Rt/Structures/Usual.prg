STRUCTURE Vulcan.__Usual
	PRIVATE Value as IntPtr
	STATIC PUBLIC _NIL AS __Usual
	CONSTRUCTOR()
		Value :=IntPtr.Zero
	STATIC CONSTRUCTOR
		_NIL := __Usual{}
	PROPERTY __Value as IntPtr GET Value
END STRUCTURE


