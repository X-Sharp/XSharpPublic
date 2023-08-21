/// <include file="Gui.xml" path="doc/EventContext/*" />
CLASS EventContext INHERIT VObject
	PROTECT ptrSelfPtr AS SelfPtr // PTR  dcaton 070301 - type it as it really is!


/// <include file="Gui.xml" path="doc/EventContext.Destroy/*" />
METHOD Destroy()  AS USUAL CLIPPER


   __WCSelfPtrFree(ptrSelfPtr)


	IF !InCollect()
		UnregisterAxit(SELF)
		ptrSelfPtr := NULL_PTR
	ENDIF


	SUPER:Destroy()


	RETURN SELF


/// <include file="Gui.xml" path="doc/EventContext.ctor/*" />
CONSTRUCTOR()
	SUPER()
	ptrSelfPtr :=__WCSelfPtrAlloc(SELF)


	RETURN


/// <include file="Gui.xml" path="doc/EventContext.Override/*" />
METHOD @@Override() 
	RETURN NIL


/// <include file="Gui.xml" path="doc/EventContext.SelfPtrStruc/*" />
ACCESS SelfPtrStruc
	RETURN ptrSelfPtr


END CLASS


