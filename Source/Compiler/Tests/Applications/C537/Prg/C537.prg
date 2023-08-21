// 537. error XS9055: Codeblocks cannot be declared with the Lambda Expression Syntax.
#pragma warnings(219, off) // assigned but not used
#pragma warnings(165, off) // unassigned

FUNCTION Start( ) AS VOID
LOCAL l := FALSE AS LOGIC
IF l
	LOCAL cAlias AS STRING
	alias->(DBUnlock()) // ok
	(cAlias)->(DBUnlock()) // ok
	LOCAL u AS USUAL
	u := alias->(DBUnlock())
	u := (cAlias)->DBUnlock()
	u := (cAlias)->(DBUnlock())
	? (cAlias)->DBUnlock()

	LOCAL o AS OBJECT
	o := (cAlias)->DBUnlock()
END IF
RETURN

FUNCTION TestO() AS OBJECT
RETURN alias->(DBUnlock())
FUNCTION TestU() AS USUAL
RETURN alias->(DBUnlock())

