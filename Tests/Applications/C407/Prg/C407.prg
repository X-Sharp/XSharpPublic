// 407. error XS0101: The namespace '<global namespace>' already contains a definition for 'C407'
// probably exactly the same underlying reason with C167 & C267

// Note: removing this code:
CLASS C407
END CLASS

// and using the one below results to:
// error XS0426: The type name 'Exe' does not exist in the type 'C407'
BEGIN NAMESPACE C407
CLASS C407
END CLASS
END NAMESPACE

FUNCTION Start() AS VOID
LOCAL o1 := C407.C407{} AS C407.C407
LOCAL o2 := C407{} AS C407
? o1:ToString()
? o2:ToString()
