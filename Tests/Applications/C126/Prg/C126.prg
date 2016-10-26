// 126. error: no viable alternative at input 'LOCAL'
FUNCTION Start() AS VOID
BEGIN USING LOCAL f1 := System.Windows.Forms.Form{} AS System.Windows.Forms.Form
	? f1:ToString()
END

