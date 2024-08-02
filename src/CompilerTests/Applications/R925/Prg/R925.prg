
FUNCTION Start( ) AS VOID
	Foo{}
RETURN


CLASS Foo
    property System as logic auto

constructor()

	global::System.Diagnostics.Debug.WriteLine( "Hi" )
    global::System.Diagnostics.Debug.WriteLine( System.ToString() )
	return

END CLASS
