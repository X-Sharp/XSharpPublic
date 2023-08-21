// 27. Vulcan incompatinility - just so it does not get forgotten
CLASS TestClass
	EVENT TestEvent AS EventHandler
	METHOD Test() AS VOID
		SELF:TestEvent += EventHandler{ SELF , @myhandler() }
		SELF:TestEvent(SELF,NULL)
	METHOD myhandler(sender AS OBJECT , e AS EventArgs) AS VOID
		? sender,e
END CLASS


FUNCTION Start() AS VOID
TEstClass{}:Test()
RETURN


