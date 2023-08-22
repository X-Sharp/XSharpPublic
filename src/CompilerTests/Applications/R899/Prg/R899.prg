// See https://github.com/X-Sharp/XSharpPublic/issues/1289
#xtranslate ORA_DRV(<Method> ( [<arg_list,...>] ) ) => ;
    (__oraResult := ORA_<Method>( [<arg_list>] ),;
     iif( 1=2 ,;
         "garbage",;
         __oraResult))

#xtranslate ORA_DRV_LOCALS => local __oraResult := nil


Function Test
    ORA_DRV_LOCALS
    LOCAL nHandle := "alias"
    return Upper( ORA_DRV(Alias(nHandle)))


FUNCTION Start( ) AS VOID
	xAssert(Test() == "ALIAS")
RETURN


FUNCTION Ora_Alias(nHandle)
    RETURN nHandle


PROC xAssert(l AS LOGIC)  AS VOID
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF
RETURN
