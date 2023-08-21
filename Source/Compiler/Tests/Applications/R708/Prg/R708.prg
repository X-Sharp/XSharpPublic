// test case for https://github.com/X-Sharp/XSharpPublic/issues/373
#pragma warnings(9098, off) // late binding
Function Start As Void
	TRY
    LOCAL loData = Empty{} AS Empty
	AddProperty(loData, "FirstName", "Matt")
	AddProperty(loData, "State", "Alabama")

	? "Empty object created, poperties added:"
	? loData.FirstName
	? loData.State
	? loData.BadPropertyName

	Wait
	CATCH e as Exception
		MEssageBox(e:ToString())
	END TRY
	RETURN
