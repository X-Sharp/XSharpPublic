//710. Problems casting numeric values to LOGIC

DEFINE myWS_TABSTOP := 65536
	
FUNCTION Start() AS VOID
	// Following return TRUE as expected
	xAssert( LOGIC(_CAST, GetValue(1))   )
	xAssert( LOGIC(_CAST, GetValue(128)) )
	xAssert( LOGIC(_CAST, 1)             )
	xAssert( LOGIC(_CAST, 2)             )
	xAssert( LOGIC(_CAST, 64)            )
	xAssert( LOGIC(_CAST, 128)           )
	xAssert( LOGIC(_CAST, 255)           )
	
	// All following return FALSE while VO returns TRUE
	xAssert( LOGIC(_CAST, GetValue(256))                          )
	xAssert( LOGIC(_CAST, GetValue(1024))                         )
	xAssert( LOGIC(_CAST, GetValue(65536))                        )
	xAssert( LOGIC(_CAST, 256)                                    )
	xAssert( LOGIC(_CAST, 256U)                                   )
	xAssert( LOGIC(_CAST, WORD(256))                              )
	xAssert( LOGIC(_CAST, 1024)                                   )
	xAssert( LOGIC(_CAST, 1024U)                                  )
	xAssert( LOGIC(_CAST, WORD(1024))                             )
	xAssert( LOGIC(_CAST, 65536)                                  )
	xAssert( LOGIC(_CAST, 65536U)                                 )
	xAssert( LOGIC(_CAST, _And(myWS_TABSTOP , GetValue(65536)))   )
	xAssert( LOGIC(_CAST, _And(GetValue(65536) , myWS_TABSTOP))   )
	xAssert( LOGIC(_CAST, _And(myWS_TABSTOP, 65536))              )
RETURN

FUNCTION GetValue(n AS INT) AS INT
RETURN n



PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

