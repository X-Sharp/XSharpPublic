CLASS OLEDataWindow INHERIT DataWindow
	// RvdH 041128
	// Empty class for compatibility with old code. All code is now in (Data)Window

CONSTRUCTOR(oOwner, oSource, nResourceID) 
    
    SUPER(oOwner, oSource, nResourceID)


RETURN 
END CLASS

CLASS OLEShellWindow INHERIT ShellWindow
	// RvdH 041128
   // Empty class for compatibility with old code. All code is now in (Shell)Window

CONSTRUCTOR(oOwner) 
    
    SUPER(oOwner)


RETURN 
END CLASS

