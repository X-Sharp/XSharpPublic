FUNCTION Start() AS VOID STRICT
LOCAL bBlock AS _CODEBLOCK
LOCAL cVar AS USUAL
LOCAL cClass AS USUAL

cVar := "x"
PRIVATE &cVar
&cVar := "a"
? &cVar

bBlock := &( "{||MyTestFunc(@x)}" )
bBlock:Eval()
? &cVar // this is ok now

cClass := "c"
PRIVATE &cClass
bBlock := &( "{||c := MyTestClass{}}" )
bBlock:Eval()

bBlock := &( "{||c:TestMethod(@x)}" )
bBlock:Eval()

? &cVar // this still doesn't work

WAIT

RETURN

FUNCTION MyTestFunc(y)
y := "b"
RETURN TRUE

CLASS MyTestClass
DECLARE METHOD TestMethod

METHOD TestMethod(y REF STRING) AS VOID
y := "c"
RETURN

END CLASS
