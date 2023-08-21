//
// This example shows various new expression formats
//
using System.Collections.Generic

Function Start() as void
	VAR oNone   := Person{"No", "Parent"}

	FOREACH VAR oValue in GetList()
		if oValue IS STRING						// Value IS Type
			? (String) oValue
		ELSEIF oValue IS INT
			? (Int) oValue
		ELSEIF oValue IS DateTime
			? (DateTime) oValue
		ELSEIF oValue IS Person
			LOCAL oPerson as Person
			oPerson := (Person) oValue
			? oPerson:FirstName, oPerson:LastName
			oPerson := oPerson:Parent DEFAULT oNone		// Value DEFAULT Value2 . When Value IS NULL then Value2 will be used
			? "Parent: ", oPerson:FirstName, oPerson:LastName
		ENDIF
	NEXT
	LOCAL oEmptyPerson as Person
	LOCAL sName as STRING
	oEmptyPerson := GetAPerson()
	sName	:= oEmptyPerson?:FirstName			// Conditional Access: This will not crash, even when Person is a NULL_OBJECT
	? sName DEFAULT "None"
	Console.ReadLine()
	RETURN

FUNCTION GetList() AS List<OBJECT>
	VAR aList := List<OBJECT>{}
	aList:Add(DateTime.Now)
	aList:Add("abcdefg")
	aList:Add(123456)
	VAR oPerson := Person{"John", "Doe"}
	aList:Add(oPerson)
	VAR oChild := Person{"Jane", "Doe"}
	oChild:Parent := oPerson
	aList:Add(oChild)
	RETURN aList

CLASS Person 
	EXPORT FirstName	AS STRING
	EXPORT LastName		as STRING
	EXPORT Parent		as Person

	CONSTRUCTOR(First as STRING, Last as STRING)
		FirstName	:= First
		LastName	:= Last

END CLASS


FUNCTION GetAPerson() as Person
	RETURN NULL_OBJECT

