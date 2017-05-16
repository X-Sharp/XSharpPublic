// 518. Compiler crash with Chr( func() ) in Core
FUNCTION Start() AS VOID
? Chr( SomeFunction() )
? _Chr( SomeFunction() )
RETURN
