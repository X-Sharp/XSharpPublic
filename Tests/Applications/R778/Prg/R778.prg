// https://github.com/X-Sharp/XSharpPublic/issues/660
// XS0112 Error for Extension Method 
USING System.Text
STATIC CLASS XXX
     PUBLIC METHOD AppendUsual(SELF sb AS StringBuilder, value AS OBJECT) AS VOID PASCAL 
         IF (sb != NULL .and. value != NIL) 
             sb:Append((STRING)value) 
      ENDIF 
 END CLASS 
