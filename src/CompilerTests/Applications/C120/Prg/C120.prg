// 120. error XS0154: The property or indexer 'Foo.Alignment' cannot be used in this context because it lacks the get accessor
CLASS Alignment
	STATIC EXPORT Left AS INT
END CLASS

CLASS Foo
	PROPERTY Alignment AS INT
		SET
			IF Alignment.Left == 1
		       NOP
			ENDIF
		END SET
	END PROPERTY
	METHOD Bar() AS VOID
	? Alignment.Left
END CLASS

FUNCTION Start() AS VOID

RETURN
