// error XS0103: The name 'oMyDelegate' does not exist in the current context
DELEGATE MyDelegate(c AS STRING) AS INT

FUNCTION Test(c AS STRING) AS INT
RETURN 123

FUNCTION Start() AS VOID
LOCAL oMyDelegate AS MyDelegate
oMyDelegate := Test
? oMyDelegate("asd") // error
? oMyDelegate:Invoke("asd") // OK

// this one is ok
CLASS EventClass
	EVENT TestEvent AS EventHandler
	METHOD RaiseEvent() AS VOID
		IF SELF:TestEvent != NULL
			SELF:TestEvent(NULL,NULL)
		END IF
	RETURN
END CLASS

