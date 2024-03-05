USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING System.IO
USING VFPXPorterLib

BEGIN NAMESPACE FabVFPXPorterCmd

	FUNCTION Start( args AS STRING[] ) AS VOID STRICT
		Console.WriteLine("VFPXPorter :" )
		//
		IF ( args:Length == 0 )
			Usage()
			RETURN
		ENDIF
		//
		VAR inputList := List<STRING>{}
		LOCAL outputFolder := "" AS STRING
		LOCAL logFile := NULL AS STRING
		LOCAL doBackup := FALSE AS LOGIC
		//
		FOREACH VAR arg IN args
			Console.WriteLine(arg)
			//
			IF arg:StartsWith("-f:",StringComparison.InvariantCultureIgnoreCase )
				inputList:Add( arg:Substring(3))
			ELSEIF arg:StartsWith("-o:",StringComparison.InvariantCultureIgnoreCase )
				outputFolder := arg:Substring(3)
			ELSEIF arg:StartsWith("-b",StringComparison.InvariantCultureIgnoreCase )
				doBackup := TRUE
			ELSEIF arg:StartsWith("-l:",StringComparison.InvariantCultureIgnoreCase )
				logFile := arg:Substring(3)
			ENDIF
		NEXT
		//
		IF !Directory.Exists( outputFolder )
				Console.ForegroundColor := ConsoleColor.Red
			Console.WriteLine( "Error : Output Path doesn't exist." )
		ELSE
			FOREACH VAR cFile IN inputList
				VAR inputFile := cFile
				VAR found := File.Exists( inputFile)
				IF !found
					// Try something else
					inputFile := Path.Combine( Directory.GetCurrentDirectory(), cFile )
					found := File.Exists( inputFile)
				ENDIF
				IF !found					
					Console.ForegroundColor := ConsoleColor.Red
					Console.WriteLine( "Error : Input File doesn't exist. " + inputFile )
				ELSE
					VAR ext := Path.GetExtension( inputFile )
					IF String.Compare( ext, ".scx", TRUE )==0
						//
						SCXExport( inputFile, outputFolder, doBackup, logFile )

					ENDIF
				ENDIF
			NEXT
		ENDIF
		//
		Console.WriteLine("Press any key to continue...")
		Console.ReadKey()
		
		END NAMESPACE
