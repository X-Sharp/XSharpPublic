/// <include file="Gui.xml" path="doc/OLEDataWindow/*" />
CLASS OLEDataWindow INHERIT DataWindow
	// RvdH 041128
	// Empty class for compatibility with old code. All code is now in (Data)Window


/// <include file="Gui.xml" path="doc/OLEDataWindow.ctor/*" />
CONSTRUCTOR(oOwner, oSource, nResourceID) 
    
    
    SUPER(oOwner, oSource, nResourceID)




RETURN 
END CLASS


/// <include file="Gui.xml" path="doc/OLEShellWindow/*" />
CLASS OLEShellWindow INHERIT ShellWindow
	// RvdH 041128
   // Empty class for compatibility with old code. All code is now in (Shell)Window


/// <include file="Gui.xml" path="doc/OLEShellWindow.ctor/*" />
CONSTRUCTOR(oOwner) 
    
    
    SUPER(oOwner)




RETURN 
END CLASS


