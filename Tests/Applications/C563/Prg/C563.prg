// 563. Unhandled Exception: System.MissingFieldException: Field not found: 'C563_helper.Functions.sTestVulcan'.

FUNCTION Start() AS VOID
// System.MissingFieldException at runtime:
? sTestVulcan
sTestVulcan := 123
anotherglobal++

// this one works ok:
? vnFunc()
RETURN
