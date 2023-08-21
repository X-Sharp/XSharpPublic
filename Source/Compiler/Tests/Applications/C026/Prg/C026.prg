// 26. Vulcan incompatinility - error XS1524: Expected catch or finally
#pragma warnings(9101, off) // try without catch
FUNCTION Start() AS VOID

TRY
   NOP
END TRY
RETURN

