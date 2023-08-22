// 858. Runtime error problems with codeblocks
// https://github.com/X-Sharp/XSharpPublic/issues/1056
FUNCTION Start() AS VOID STRICT
	DoTests{}
RETURN      

CLASS DoTests
EXPORT untyped AS OBJECT
EXPORT typed AS MemberClass

CONSTRUCTOR()
LOCAL aArray := NULL_ARRAY AS ARRAY
LOCAL lTest := TRUE AS LOGIC

SELF:typed := MemberClass{}
SELF:untyped := SELF:typed

? Eval( {|x|x == SELF:untyped:cSomeProperty} , "test" )								// OK
? Eval( {|x|x == SELF:untyped:cSomeProperty .AND. lTest} , "test" )					// No exported variable
? Eval( {|x|x == SELF:untyped:cSomeProperty .AND. aArray == NULL_ARRAY} , "test" )	// Value cannot be null.
? Eval( {|x|lTest .AND. x == SELF:untyped:cSomeProperty} , "test" )					// No exported variable
? Eval( {|x|aArray == NULL_ARRAY .AND. x == SELF:untyped:cSomeProperty} , "test" )	// Value cannot be null.
? Eval( {|x|aArray != {} .AND. x == SELF:untyped:cSomeProperty} , "test" )			// Value cannot be null.

? Eval( {|x|x == SELF:typed:cSomeProperty} , "test" )								// OK
? Eval( {|x|x == SELF:typed:cSomeProperty .AND. lTest} , "test" )					// Object reference not set to an instance of an object.
? Eval( {|x|x == SELF:typed:cSomeProperty .AND. aArray != NULL_ARRAY} , "test" )	// Unable to cast object of type 'MemberClass' to type 'System.String'.
? Eval( {|x|aArray == NULL_ARRAY .AND. x == SELF:typed:cSomeProperty} , "test" )	// Unable to cast object of type 'MemberClass' to type 'System.String'.
? Eval( {|x|lTest .and. x == SELF:typed:cSomeProperty} , "test" )					// Object reference not set to an instance of an object.

RETURN 

END CLASS
   
CLASS MemberClass
EXPORT cSomeProperty := "test" AS STRING 
END CLASS
