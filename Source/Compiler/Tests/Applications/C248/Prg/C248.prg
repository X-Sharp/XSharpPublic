// 248.
// error XS0029: Cannot implicitly convert type 'void' to 'Vulcan.__Usual'
// error XS1662: Cannot convert lambda expression to intended delegate type because some of the return types in the block are not implicitly convertible to the delegate return type
#pragma warnings(618, off) // obsolete ErrorBlock
FUNCTION __IterateForSum1( ) AS VOID

CLASS Testclass12
METHOD __DbServerEval( uBlock AS USUAL) AS VOID
METHOD Test() AS VOID
LOCAL n AS INT
n := 0
SELF:__DBServerEval( { || n += 1, __IterateForSum1( ) })
END CLASS

FUNCTION Test2 AS VOID
		ErrorBlock({|e| ErrorDialog{(Exception)e}:Show()})
RETURN
