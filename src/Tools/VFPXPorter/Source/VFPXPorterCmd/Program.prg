USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING System.IO
USING VFPXPorterLib

BEGIN NAMESPACE FabVFPXPorterCmd

	FUNCTION Start( args AS STRING[] ) AS INT STRICT
		Console.WriteLine("VFPXPorter")
		//
		IF args:Length == 0
			Usage()
			RETURN 1
		ENDIF
		//
		VAR inputList      := List<STRING>{}
		LOCAL pjxFile      := "" AS STRING
		LOCAL outputFolder := "" AS STRING
		LOCAL logFile      := NULL AS STRING
		LOCAL doBackup     := FALSE AS LOGIC
		LOCAL hasError     := FALSE AS LOGIC
		VAR settings       := XPorterSettings{}
		//
		FOREACH VAR arg IN args
			IF arg:StartsWith("-f:", StringComparison.InvariantCultureIgnoreCase)
				inputList:Add(StripQuotes(arg:Substring(3)))
			ELSEIF arg:StartsWith("-p:", StringComparison.InvariantCultureIgnoreCase)
				pjxFile := StripQuotes(arg:Substring(3))
			ELSEIF arg:StartsWith("-o:", StringComparison.InvariantCultureIgnoreCase)
				outputFolder := StripQuotes(arg:Substring(3))
			ELSEIF arg:StartsWith("-l:", StringComparison.InvariantCultureIgnoreCase)
				logFile := StripQuotes(arg:Substring(3))
			ELSEIF arg:StartsWith("-outputType:", StringComparison.InvariantCultureIgnoreCase)
				VAR typeName := StripQuotes(arg:Substring(12))
				IF !Enum.TryParse<ProjectType>(typeName, TRUE, OUT VAR pt)
					Console.ForegroundColor := ConsoleColor.Yellow
					Console.WriteLine("Warning: unknown -outputType value '" + typeName + "' — using default WindowsExe.")
					Console.ResetColor()
				ELSE
					settings:OutputType := pt
				ENDIF
			ELSEIF arg:StartsWith("-modifier:", StringComparison.InvariantCultureIgnoreCase)
				settings:Modifier := StripQuotes(arg:Substring(10)):ToUpperInvariant()
			ELSEIF String.Compare(arg, "-b", TRUE) == 0
				doBackup := TRUE
			ELSEIF String.Compare(arg, "-keepOriginal", TRUE) == 0
				settings:KeepOriginal := TRUE
			ELSEIF String.Compare(arg, "-noKeepOriginal", TRUE) == 0
				settings:KeepOriginal := FALSE
			ELSEIF String.Compare(arg, "-storeInFolders", TRUE) == 0
				settings:StoreInFolders := TRUE
			ELSE
				Console.ForegroundColor := ConsoleColor.Yellow
				Console.WriteLine("Warning: unknown argument '" + arg + "'")
				Console.ResetColor()
			ENDIF
		NEXT
		//
		// Validate output folder — create it if missing
		IF String.IsNullOrEmpty(outputFolder)
			Console.ForegroundColor := ConsoleColor.Red
			Console.WriteLine("Error: -o:<output folder> is required.")
			Console.ResetColor()
			RETURN 1
		ENDIF
		IF !Directory.Exists(outputFolder)
			TRY
				Directory.CreateDirectory(outputFolder)
			CATCH e AS Exception
				Console.ForegroundColor := ConsoleColor.Red
				Console.WriteLine("Error: cannot create output folder: " + e:Message)
				Console.ResetColor()
				RETURN 1
			END TRY
		ENDIF
		//
		// Configure logger
		IF !String.IsNullOrEmpty(logFile)
			XPorterLogger.SetLoggerToFile(logFile)
		ENDIF
		//
		// Project export (-p)
		IF !String.IsNullOrEmpty(pjxFile)
			IF !File.Exists(pjxFile)
				pjxFile := Path.Combine(Directory.GetCurrentDirectory(), pjxFile)
			ENDIF
			IF !File.Exists(pjxFile)
				Console.ForegroundColor := ConsoleColor.Red
				Console.WriteLine("Error: project file not found: " + pjxFile)
				Console.ResetColor()
				hasError := TRUE
			ELSE
				IF !PJXExport(pjxFile, outputFolder, doBackup, settings)
					hasError := TRUE
				ENDIF
			ENDIF
		ENDIF
		//
		// Single-file export (-f)
		FOREACH VAR inputFile IN inputList
			VAR resolvedFile := inputFile
			IF !File.Exists(resolvedFile)
				resolvedFile := Path.Combine(Directory.GetCurrentDirectory(), inputFile)
			ENDIF
			IF !File.Exists(resolvedFile)
				Console.ForegroundColor := ConsoleColor.Red
				Console.WriteLine("Error: input file not found: " + inputFile)
				Console.ResetColor()
				hasError := TRUE
				LOOP
			ENDIF
			VAR ext := Path.GetExtension(resolvedFile)
			IF String.Compare(ext, ".scx", TRUE) == 0 .OR. String.Compare(ext, ".vcx", TRUE) == 0
				IF !SCXExport(resolvedFile, outputFolder, doBackup, settings)
					hasError := TRUE
				ENDIF
			ELSE
				Console.ForegroundColor := ConsoleColor.Yellow
				Console.WriteLine("Warning: unsupported file type '" + ext + "' — skipped: " + resolvedFile)
				Console.ResetColor()
			ENDIF
		NEXT
		//
		IF hasError
			Console.ForegroundColor := ConsoleColor.Red
			Console.WriteLine("Completed with errors.")
			Console.ResetColor()
			RETURN 1
		ENDIF
		Console.WriteLine("Done.")
		RETURN 0

	// Strip leading and trailing double-quote characters from a path value.
	// Handles the case where the shell passes quotes through to the argument string.
	FUNCTION StripQuotes( value AS STRING ) AS STRING
		RETURN value:Trim('"')

END NAMESPACE
