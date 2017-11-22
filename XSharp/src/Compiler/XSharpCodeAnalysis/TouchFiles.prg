FUNCTION start (args AS STRING[]) AS VOID
	IF (args:Length < 2)
		Console.WriteLine("Syntax: TouchFiles <Sourcefile> <DestFile1> [... <DestFileN>]")
		Console.WriteLine("This will copy the timestamp of <SourceFile> and apply it to all the destFiles")
		RETURN
	ENDIF                                               
	IF !System.IO.File.Exists(args[1])
		Console.WriteLine( "File " + args[1]+" not found")
		RETURN
	ENDIF 
	Console.WriteLine("Copy timestamp from "+args[1]+" to" )
	VAR dt := System.IO.File.GetLastWriteTime(args[1])
	FOR VAR i := 2 TO args:Length
		IF SYstem.IO.File.Exists(args[i])
			TRY
				System.IO.File.SetLastWriteTime(args[i], dt) 
				Console.WriteLine( " "+args[i])
			CATCH e AS Exception
				Console.WriteLine( "Cannot set date for file: " +args[i]+ e:Message)
			END TRY
		ELSE                            
			Console.WriteLine( "File " + args[i]+" not found")
		ENDIF
	NEXT
	RETURN
