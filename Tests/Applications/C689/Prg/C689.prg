// 689. error XS1061: 'System.DateTime' does not contain a definition for 'yyyy
FUNCTION Start() AS VOID
	LOCAL s AS STRING

	LOCAL n AS INT
	n := 123
	s := i"{n:0000}" //ok
	? s
	
	LOCAL dData AS DateTime
	dData := DateTime.Now
	s := i"{dData:yyyy-MM-dd}" // error XS0103
	? s
RETURN
