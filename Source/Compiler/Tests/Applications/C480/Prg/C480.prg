-
STATIC GLOBAL __glRestoreWorkarea := FALSE AS LOGIC //SE-060527

// 480. Compiler crash with invalid syntax
// the "-" line needs to be the first one

STATIC GLOBAL sgLockMode := ccOptimistic AS DWORD

FUNCTION __DBSAPPEND( lRelease AS LOGIC, nTries := 1 AS DWORD ) AS LOGIC STRICT
   LOCAL lOk := FALSE AS LOGIC
   //SE-060527
   DO WHILE nTries > 0
      NetErr( FALSE )
      IF VODBAppend(lRelease)
         lOk := TRUE
         EXIT
      ENDIF
      // Append always retries. Does not check for NetErr()!
      nTries --
   ENDDO     
   IF (! lOk)
      NetErr(TRUE)
   ENDIF
   RETURN lOk


