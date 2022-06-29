// 252. error XS0136: A local or parameter named 'oErr' cannot be declared in this scope because that name is used in an enclosing local scope to define a local or parameter
#pragma warnings(219, off)
FUNCTION Start() AS VOID
LOCAL oErr AS USUAL
BEGIN SEQUENCE
LOCAL cb AS CODEBLOCK
cb := {|oErr| oErr }
RECOVER USING oErr
? oErr
END SEQUENCE

