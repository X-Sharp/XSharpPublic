CLASS Container
   PROTECT _aChilds AS ARRAY

CONSTRUCTOR() 
   SELF:Clear()
   RETURN SELF

METHOD Register(oObject) 
   IF SELF:__Find(oObject) = 0
      AAdd(_aChilds, oObject)  //delete this line to test the destroying of the WebBrowser olecontrol
   ENDIF   
   RETURN oObject   


METHOD UnRegister(oObject) 
   LOCAL dwI AS DWORD
   
   IF (dwI := SELF:__Find(oObject)) > 0
      ADel(_aChilds, dwI)
      ASize(_aChilds, ALen(_aChilds)-1) 
   ENDIF   
   RETURN oObject
   

METHOD __Find(oObject) 
   LOCAL dwI, dwCount AS DWORD 
   
   dwCount := ALen(_aChilds)
   FOR dwI:= 1 UPTO dwCount
      IF _aChilds[dwI] = oObject
         RETURN dwI
      ENDIF   
   NEXT dwI
   
   RETURN 0

METHOD Clear() 
   _aChilds := {}
   RETURN NIL

ACCESS Elements 
   RETURN _aChilds       


END CLASS
