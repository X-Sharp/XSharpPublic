// 127. error: extraneous input 'f2' expecting...
FUNCTION Start() AS VOID
BEGIN USING VAR f2 := System.Windows.Forms.Form{}
	? f2:ToString()
END

