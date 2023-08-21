// https://github.com/X-Sharp/XSharpPublic/issues/1187
// https://github.com/X-Sharp/XSharpPublic/issues/1190
STATIC CLASS Test
    PUBLIC STATIC METHOD GetVal() AS LOGIC
        LOCAL nAbc := 1 AS INT

        IF (nAbc > 0)  // error here and not in the elseif line
            NOP()
        ELSEIF (SELF:nAbc == 0)
            NOP()
        ENDIF
        
        REPEAT
	        ? "test"
        UNTIL (unkonwn == 123)

        RETURN TRUE
END CLASS


