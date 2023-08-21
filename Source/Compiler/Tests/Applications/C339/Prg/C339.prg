// 339. error XS0103: The name 'symTabName' does not exist in the current context
CLASS Foo
ACCESS TabCaption (symTabName) 
	RETURN symTabName
ASSIGN TabCaption (cCaption, symTabName) 
	? symTabName , cCaption
END CLASS

FUNCTION Start() AS VOID
LOCAL o AS Foo
o := Foo{}
? o:TabCaption[#abc]
o:TabCaption[#index] := "value"

// vulcan prints "value , INDEX"

