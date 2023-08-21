// 797. Failed to emit module with VAR and ARRAY of INT
FUNCTION Start( ) AS VOID
 VAR aArray := ARRAY of INT {5} { 1, 2, 3, 4, 5 }
  VAR aLen := ALen(Aarray)
  ? aLen
  IF aLen == 0
      NOP
  ENDIF
RETURN
