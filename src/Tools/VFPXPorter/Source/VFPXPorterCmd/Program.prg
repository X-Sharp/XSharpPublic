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
		VAR inputList   := List<STRING>{}
		LOCAL pjxFile      := "" AS STRING
		LOCAL outputFolder := "" AS STRING
		LOCAL logFile      := NULL AS STRING
		LOCAL doBackup     := FALSE AS LOGIC
		LOCAL hasError     := FALSE AS LOGIC
		//
		FOREACH VAR arg IN args
			IF arg:StartsWith("-f:", StringComparison.InvariantCultureIgnoreCase)
				inputList:Add(arg:Substring(3))
			ELSEIF arg:StartsWith("-p:", StringComparison.InvariantCultureIgnoreCase)
				pjxFile := arg:Substring(3)
			ELSEIF arg:StartsWith("-o:", StringComparison.InvariantCultureIgnoreCase)
				outputFolder := arg:Substring(3)
			ELSEIF arg:StartsWith("-b", StringComparison.InvariantCultureIgnoreCase)
				doBackup := TRUE
			ELSEIF arg:StartsWith("-l:", StringComparison.InvariantCultureIgnoreCase)
				logFile := arg:Substring(3)
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
				IF !PJXExport(pjxFile, outputFolder, doBackup)
					hasError := TRUE
				ENDIF
			ENDIF
		ENDIF
		//
		// Single-file export (-f)
		FOREACH VAR cFile IN inputList
			VAR inputFile := cFile
			IF !File.Exists(inputFile)
				inputFile := Path.Combine(Directory.GetCurrentDirectory(), cFile)
			ENDIF
			IF !File.Exists(inputFile)
				Console.ForegroundColor := ConsoleColor.Red
				Console.WriteLine("Error: input file not found: " + inputFile)
				Console.ResetColor()
				hasError := TRUE
				LOOP
			ENDIF
			VAR ext := Path.GetExtension(inputFile)
			IF String.Compare(ext, ".scx", TRUE) == 0 .OR. String.Compare(ext, ".vcx", TRUE) == 0
				IF !SCXExport(inputFile, outputFolder, doBackup)
					hasError := TRUE
				ENDIF
			ELSE
				Console.ForegroundColor := ConsoleColor.Yellow
				Console.WriteLine("Warning: unsupported file type '" + ext + "' — skipped: " + inputFile)
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

END NAMESPACE
