FUNCTION start (args AS STRING[]) AS VOID
	IF (args:Length < 2)
		Console.WriteLine("Syntax: TouchFiles <Sourcefile> <DestFile1> [... <DestFileN>] [/q]")
		Console.WriteLine("This will copy the timestamp of <SourceFile> and apply it to all the destFiles")
		Console.WriteLine("/q activates QUIET mode")
		RETURN
	ENDIF            
	IF !System.IO.File.Exists(args[1])
		Console.WriteLine( "File " + args[1]+" not found")
		RETURN
	ENDIF 
	VAR dt := System.IO.File.GetLastWriteTime(args[1])
	local first as logic
	first := true
	local quiet := FALSE as logic
	FOR VAR i := 2 TO args:Length
		if args[i]:ToLower():StartsWith("/q")
			quiet := TRUE
		ENDIF
		
	NEXT
	FOR VAR i := 2 TO args:Length
		if args[i]:ToLower():StartsWith("/q")
			loop
		endif
		IF System.IO.File.Exists(args[i])
			TRY
				var dtfile :=System.IO.File.GetLastWriteTime(args[i])
				if dtfile != dt
					if first 
						IF ! quiet
							Console.WriteLine("Copy timestamp from "+args[1]+" to" )
						endif
						first := false
					endif
					System.IO.File.SetLastWriteTime(args[i], dt) 
					IF ! quiet
						Console.WriteLine( " "+args[i])
					ENDIF
				endif
			CATCH e AS Exception
				Console.WriteLine( "Cannot set date for file: " +args[i]+ e:Message)
			END TRY
		ELSE                            
			Console.WriteLine( "File " + args[i]+" not found")
		ENDIF
	NEXT
	RETURN
