// 539. error XS0218: In order for '__Usual.operator &(__Usual, __Usual)' to be applicable as a short circuit operator, its declaring type '__Usual' must define operator true and operator false
FUNCTION Start( ) AS VOID
	LOCAL u AS USUAL
	LOCAL lTest AS LOGIC
	u := {|| lTest .and. alias->EOF()}
	u := {|| lTest .and. alias->RecNo() == 1}
	u := {|| lTest .or. alias->EOF()}
	
	u := { {|| lTest .and. alias->(RecNo()) <= alias->(LastRec())} }
RETURN
