// error XS0118: 'XSTools' is a namespace but is used like a type
USING XSTools

FUNCTION Start() AS VOID
LOCAL o AS XSTools // error XS0118: 'XSTools' is a namespace but is used like a type
XSTools.GetTrue() // error XS0234: The type or namespace name 'GetTrue' does not exist in the namespace 'XSTools'

BEGIN NAMESPACE XSTools
    CLASS XSTools
    STATIC METHOD GetTrue() AS LOGIC
    RETURN TRUE
    END CLASS
END NAMESPACE
