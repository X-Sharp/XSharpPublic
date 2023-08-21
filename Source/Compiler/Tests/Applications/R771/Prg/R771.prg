// R771: Assertion Failed: Abort=Quit, Retry=Debug, Ignore=Continue
// Unexpected value 'UnconvertedConditionalOperator' of type 'LanguageService.CodeAnalysis.XSharp.BoundKind'

DEFINE QC_CostChargeQuantity := 1 
DEFINE QC_QuoteChargeQuantity := 2
    
FUNCTION Start() AS VOID STRICT
    
    LOCAL aQC AS ARRAY
    LOCAL dwOffset AS DWORD
    aQC := {1, 2}
    dwOffset := aQC[IIF(Upper(AllTrim("Test"))==Upper(AllTrim("TestB")),QC_QuoteChargeQuantity,QC_QuoteChargeQuantity)] //Compile crash
    xAssert(dwOffset == 2)
RETURN      



PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF
