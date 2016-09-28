// Application : XPorter
// Interfaces.prg , Created : 28-9-2016   15:05
// User : robert

INTERFACE IProgress
	METHOD WriteLine(cText AS STRING) AS VOID     
	METHOD Stop() AS VOID
	METHOD Start() AS VOID
END INTERFACE	
