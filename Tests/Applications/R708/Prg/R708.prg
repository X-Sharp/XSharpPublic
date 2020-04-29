// test case for https://github.com/X-Sharp/XSharpPublic/issues/373
Function Start As Void
	TRY
	Var loData = Empty{}
	loData.AddProperty("FirstName", "Matt")
	loData.AddProperty("State", "Alabama")

	? "Empty object created, poperties added:"
	? loData.FirstName
	? loData.State 
	? loData.BadPropertyName
	
	Wait
	CATCH e as Exception
		MEssageBox(e:ToString())
	END TRY
	RETURN
