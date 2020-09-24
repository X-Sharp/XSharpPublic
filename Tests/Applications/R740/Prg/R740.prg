//   R740 Internal compiler error for BYTE(_CAST, LOGIC)
DEFINE FVIRTKEY := TRUE
FUNCTION start() AS VOID              
    LOCAL b AS BYTE                                 
    b := BYTE(_CAST, FVIRTKEY) 
    //b := _AND(FVIRTKEY, 255)
    ? b
RETURN  
