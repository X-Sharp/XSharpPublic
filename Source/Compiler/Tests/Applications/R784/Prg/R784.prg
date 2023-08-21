DEFINE LVN_FIRST    := (0U-100U)       
DEFINE LVN_ITEMCHANGED  := (LVN_FIRST -1)     // derive the unsigned from LVN_FIRST

DEFINE LVM_FIRST	:= 0x1000    
DEFINE LVM_GETBKIMAGE  := LVM_GETBKIMAGEA     // make sure this is marked as constant since it is resolved at compile time
DEFINE LVM_GETBKIMAGEA := (LVM_FIRST + 69)    // derive the signed from LVM_FIRST

USING System.Reflection

FUNCTION Start( ) AS VOID
    VAR type := Typeof(Functions)
    FOREACH fld AS FieldInfo IN  type:GetFields()                  
        ? fld:Name, fld:GetValue(NULL)
       IF fld:Name:StartsWith("LVM")  
           xAssert(fld:fieldType == typeof(LONG))
           xAssert(fld:IsLiteral)
       ELSEIF fld:Name:StartsWith("LVN")        
           xAssert(fld:fieldType == typeof(DWORD))
           xAssert(fld:IsLiteral)
       ENDIF
    NEXT
RETURN


PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF
