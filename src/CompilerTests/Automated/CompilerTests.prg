// !!!!!!!!!!!!!!!!   compile with /vo2+   !!!!!!!!!!!!!!!!!!!
#pragma options ("vo2", on)
USING System
USING System.IO
USING System.Collections
USING System.Collections.Generic
USING System.Diagnostics

GLOBAL gcRuntimeFolder AS STRING
GLOBAL gcCompilerFilename AS STRING
GLOBAL gaLog := List<STRING>{} AS List<STRING>
GLOBAL gaCompilerMessages := List<STRING>{} AS List<STRING>

USING Xide

FUNCTION Start() AS VOID
	LOCAL oProject AS ProjectClass
	LOCAL oXide AS XideHelper
	LOCAL aGroupsToBuild AS List<STRING>


	LOCAL cProjectFile AS STRING
	LOCAL lTestTheFixedOnes AS LOGIC
	LOCAL cTestTheFixedOnes AS STRING
	LOCAL cConfigName AS STRING
	LOCAL cLogFilename AS STRING


	// options here
	gcCompilerFilename := Environment.GetEnvironmentVariable("XSCOMPILER")?:Trim()
	cProjectFile       := Environment.GetEnvironmentVariable("XSTESTPROJECT")?:Trim()
	gcRuntimeFolder    := Environment.GetEnvironmentVariable("XSRUNTIMEFOLDER")?:Trim()
	cTestTheFixedOnes  := Environment.GetEnvironmentVariable("XSFIXEDTESTS")?:Trim()
	cConfigName        := Environment.GetEnvironmentVariable("XSCONFIG")?:Trim()
	cLogFilename       := Environment.GetEnvironmentVariable("XSLOGFILE")?:Trim()
	IF String.IsNullOrEmpty(gcCompilerFilename ) .OR. ;
		String.IsNullOrEmpty(cProjectFile      ) .OR. ;
		String.IsNullOrEmpty(gcRuntimeFolder   ) .OR. ;
		String.IsNullOrEmpty(cTestTheFixedOnes ) .OR. ;
		String.IsNullOrEmpty(cConfigName       ) .OR. ;
		String.IsNullOrEmpty(cLogFilename      )
		Console.WriteLine("This program needs the following Environment variables:")
		Console.WriteLine("XSCOMPILER     : Full path to the compiler including the exe name")
		Console.WriteLine("XSTESTPROJECT  : Full path to the test project")
		Console.WriteLine("XSRUNTIMEFOLDER: Full path to the folder that has the runtime DLLs.")
		Console.WriteLine("XSFIXEDTESTS   : TRUE or FALSE. TRUE runs the tests that should all compiler without errors.")
		Console.WriteLine("XSCONFIG       : DEBUG or RELEASE")
		Console.WriteLine("XSLOGFILE      : Full path to the output files name that the tool produces")
		RETURN
	ENDIF
	lTestTheFixedOnes := cTestTheFixedOnes:ToUpper() == "TRUE"



	IF lTestTheFixedOnes
		// FIXED, Runtime/Fixed
		aGroupsToBuild := List<STRING>{}{"715D7617-320E-41FF-B82F-E314D950CD23","601137E1-03A5-45B4-994E-BFD3410FDE72"}
	ELSE
		aGroupsToBuild := List<STRING>{}{"A95A4D23-3574-40FD-B873-22D28ACCA212"} // MUST REPORT ERROR
	ENDIF
//	aGroupsToBuild := List<STRING>{}{"B9CFE839-D401-428D-94E9-E9D21E9F772D"} // NEW

	oXide := XideHelper{}
	oProject := ProjectClass{cProjectFile}
	oXide:LoadProject(oProject)

	Message( "Total apps: " +oProject:GetAppCount():ToString())
	Message( "" )

	DoTests(oXide, aGroupsToBuild, cConfigName, lTestTheFixedOnes)

	System.IO.File.WriteAllLines(cLogFilename, gaLog)

RETURN

FUNCTION DoRuntimeTests(oXide AS XideHelper, cConfigName AS STRING) AS INT
	LOCAL oProject AS ProjectClass
	LOCAL oOptions AS CompileOptions
	LOCAL oProcess AS Process
	LOCAL oApp AS AppClass
	LOCAL nFail AS INT

	oProject := oXide:Project

	oApp := oProject:GetAppFromGuid("B098889A-A74F-46CE-8771-3AAEEECA0F99")
	gaCompilerMessages := List<STRING>{}
	oApp:aCompilerMessages := gaCompilerMessages
	Message("Compiling test: " + oApp:cName)
	oOptions := oXide:GetCompileOptions(oApp, cConfigName)

	oProcess := CreateProcess()
	oOptions:cSwitches := oOptions:cSwitches:Replace(":GUI",":NOGUI")
	oProcess:StartInfo:Arguments := oOptions:cSwitches + " /fullpaths /noconfig /shared"
	oApp:cCompilerOptions := oOptions:cSwitches

	oProcess:Start()
	oProcess:BeginErrorReadLine()
	oProcess:BeginOutputReadLine()

	IF oProcess:WaitForExit(20000)
		Message( "Exit code compiling runtime tests: " + oProcess:ExitCode:ToString() )
	ELSE
		Message( "Timed out compiling runtime tests!" )
	ENDIF
	Message( "" )
	oProcess:Dispose()

	oProcess := CreateProcess()

	LOCAL oConfig AS ProjectConfigClass
	oConfig := oProject:oConfigs:Get( iif(cConfigName:ToUpper() == "DEBUG" , 1 , 2) )

	oProcess:StartInfo:FileName := oApp:GetOutputFileName(oConfig:cSubFolder,FALSE)
	oProcess:StartInfo:Arguments := ""

	oProcess:Start()
	oProcess:BeginErrorReadLine()
	oProcess:BeginOutputReadLine()
	local result as LOGIC
	TRY
	    result := oProcess:WaitForExit(30000)
	CATCH e as Exception
	    Message( "Exception running runtime tests " )
	    Message( e:ToString() )
	    result := FALSE

	END TRY
	IF result
		nFail := oProcess:ExitCode
		Message( "Exit code running runtime tests: " + oProcess:ExitCode:ToString() )
	ELSE
		Message( "Timed out running runtime tests!" )
	ENDIF
	Message( "" )

	FOREACH cMessage AS STRING IN gaCompilerMessages
		Message(cMessage)
	NEXT
RETURN nFail


PROCEDURE DoTests(oXide AS XideHelper, aGroupsToBuild AS List<STRING>, cConfigName AS STRING, lTestTheFixedOnes AS LOGIC)
	LOCAL nCount, nFail, nSuccess, nCrash AS INT
	LOCAL oProject AS ProjectClass
	LOCAL aFailed AS List<AppClass>
	LOCAL oProcess AS Process

	Message("Running tests, expecting them to " + iif(lTestTheFixedOnes , "succeed" , "fail"))
	Message("")

	aFailed := List<AppClass>{}
	oProject := oXide:Project

	LOCAL aApps AS SortedList<STRING,AppClass>
	aApps := SortedList<STRING,AppClass>{}
	FOR LOCAL nApp := 1 AS INT UPTO oProject:GetAppCount()
		LOCAL oApp AS AppClass
		LOCAL cKey AS STRING
		oApp := oProject:GetApp(nApp)
		// make sure helper apps get compiled first
		cKey := iif(oApp:cName:ToUpper():Contains("HELPER") , "0" , "1")
		cKey += "_" + oApp:cName
		aApps:Add(cKey ,oApp)
	NEXT

	FOREACH oPair AS KeyValuePair<STRING,AppClass> IN aApps
		LOCAL oApp AS AppClass
		oApp := oPair:Value
		gaCompilerMessages := List<STRING>{}
		oApp:aCompilerMessages := gaCompilerMessages
		IF IsAppInGroups(oApp, aGroupsToBuild)
			Message("Compiling test: " + oApp:cName)
			IF oApp:eLanguage == ApplicationLanguage.VulcanNet
				IF oApp:cName:ToUpper():Contains("HELPER")
					LOOP
				END IF
				oApp:aCompilerMessages:Add("Test is using vulcan language!!!!!")
				Message("*****************  Test is using vulcan language!!!!! ********************")
//				aFailed:Add(oApp)
//				Console.Beep()
//				Console.ReadLine()
				LOOP
			END IF

			LOCAL oOptions AS CompileOptions
			oOptions := oXide:GetCompileOptions(oApp, cConfigName)

			oProcess := CreateProcess()
			oProcess:StartInfo:Arguments := oOptions:cSwitches + " /fullpaths /noconfig /shared"
			oApp:cCompilerOptions := oOptions:cSwitches

			oProcess:Start()
			oProcess:BeginErrorReadLine()
			oProcess:BeginOutputReadLine()

			IF oProcess:WaitForExit(20000)
				Message( "Exit code: " + oProcess:ExitCode:ToString() )
			ELSE
				Message("Timed out!")
				oApp:aCompilerMessages:Add("Timed out!")
				aFailed:Add(oApp)
				oProcess:Dispose()
				oProcess := CreateProcess()
				nCount ++
				nCrash ++
				LOOP
			ENDIF
			Message( "" )

			IF oApp:cName:ToUpper():EndsWith("HELPER")
				NOP
			ELSE
				nCount ++
				SWITCH oProcess:ExitCode
				CASE 0
					nSuccess ++
					IF .not. lTestTheFixedOnes
						aFailed:Add(oApp)
					END IF
				CASE 1
					nFail ++
					IF lTestTheFixedOnes
						aFailed:Add(oApp)
					END IF
				OTHERWISE
					nCrash ++
					aFailed:Add(oApp)
				END SWITCH
			END IF
			oProcess:Dispose()
		END IF
	NEXT

	Message("")
	Message("End of compiling tests, now performing runtime tests")
	Message("")
	LOCAL nRuntimeFail AS INT
	IF lTestTheFixedOnes
	    nRuntimeFail := DoRuntimeTests(oXide, cConfigName)
	ENDIF
	Message("")
	Message("End of runtime tests")
	Message("")

	Message( "===============================" )
	Message( "Total tests: " + nCount:ToString() )
	Message( "Success: " + nSuccess:ToString() )
	Message( "Fail: " + nFail:ToString() )
	Message( "Compiler Problem: " + nCrash:ToString() )
	IF lTestTheFixedOnes
		Message( "===============================" )
		Message( "Runtime tests failed: " + nRuntimeFail:ToString() )
	ENDIF
	Message( "===============================" )
	Message( "Result: " + iif(aFailed:Count + nRuntimeFail == 0 , "SUCESS!" , "Fail :(") )
	Message( "===============================" )
	IF aFailed:Count != 0
		Message( "Failed tests:" )
		FOREACH oFailed AS AppClass IN aFailed

			Message( "-------------" )
			Message( oFailed:cName )
			Message( "" )
			Message( oFailed:cCompilerOptions )
			Message( "" )
			FOREACH cMessage AS STRING IN oFailed:aCompilerMessages
				Message( cMessage )
			NEXT
			Message( "" )
			Message( "-------------" )
		NEXT
		Message( "===============================" )
	END IF
RETURN

FUNCTION CreateProcess() AS Process
	LOCAL oProcess AS Process
	oProcess := Process{}
	oProcess:StartInfo:FileName := gcCompilerFilename
//	oProcess:StartInfo:WorkingDirectory := Fun.GetDirectory(oCompile:cFullOut)
//	oProcess:StartInfo:WindowStyle := ProcessWindowStyle.Hidden
//	oProcess:StartInfo:CreateNoWindow := TRUE
	oProcess:StartInfo:UseShellExecute := FALSE
	oProcess:StartInfo:RedirectStandardError := TRUE
	oProcess:StartInfo:RedirectStandardOutput := TRUE
	oProcess:StartInfo:StandardErrorEncoding := System.Text.Encoding.UTF8
	oProcess:StartInfo:StandardOutputEncoding := System.Text.Encoding.UTF8
	oProcess:ErrorDataReceived += CompilerMessageReceived
	oProcess:OutputDataReceived += CompilerMessageReceived
RETURN oProcess


PROCEDURE CompilerMessageReceived(o AS OBJECT, e AS DataReceivedEventArgs) AS VOID
	BEGIN LOCK gaCompilerMessages
		gaCompilerMessages:Add(e:Data)
	END LOCK
RETURN



FUNCTION IsAppInGroups(oApp AS AppClass, aGroupsToBuild AS List<STRING>) AS LOGIC
	FOREACH cGroupGuid AS STRING IN aGroupsToBuild
		LOCAL oAppGroup AS ApplicationGroupClass
		oAppGroup := oApp:GetProject():FindAppGroup(cGroupGuid)
		IF oAppGroup != NULL
			IF oAppGroup:ContainsApp(oApp)
				RETURN TRUE
			END IF
		END IF
	NEXT
RETURN FALSE

PROCEDURE Message(cMessage AS STRING)
	? cMessage
	gaLog:Add(cMessage)
RETURN



BEGIN NAMESPACE Xide

CLASS XideHelper
PROTECT _oProject AS ProjectClass
PROTECT cSelectedConfig := "" AS STRING

CONSTRUCTOR()
//	SELF:Project := ProjectClass{""}
RETURN

ACCESS Project AS ProjectClass
RETURN SELF:_oProject

METHOD LoadProject(oProject AS ProjectClass) AS LOGIC
RETURN SELF:LoadProject(oProject , FALSE)
METHOD LoadProject(oProject AS ProjectClass , lInitOnly AS LOGIC) AS LOGIC
LOCAL oStream AS StreamReader
LOCAL oFileStream AS FileStream
LOCAL cFile AS STRING
LOCAL lRet:=TRUE AS LOGIC
SELF:_oProject := oProject
cFile:=oProject:cProjectFile
IF !File.Exists(cFile)
//	Fun.ShowTextBox("Error loading project","Project file '" + cFile + "' was not found",3)
	RETURN FALSE
END IF
TRY

/*	oFileStream := FileStream{cFile , FileMode.Open}
//    oStream:=StreamReader{cFile,System.Text.Encoding.GetEncoding(0)}
    oStream:=StreamReader{oFileStream,System.Text.Encoding.GetEncoding(0)}*/
    IF lInitOnly
		TRY
			oFileStream := FileStream{cFile , FileMode.Open ,FileAccess.Read , FileShare.None}
		CATCH
			oProject:lLocked := TRUE
			oFileStream := FileStream{cFile , FileMode.Open ,FileAccess.Read , FileShare.ReadWrite}
		END TRY
    ELSE
		oFileStream := FileStream{cFile , FileMode.Open ,FileAccess.ReadWrite , FileShare.ReadWrite}
    ENDIF
   	oStream:=StreamReader{oFileStream,System.Text.Encoding.GetEncoding(0)}

   	oProject:cFolder := Fun.GetDirectory(oProject:cProjectFile)

    SELF:LoadProject(oProject , oStream , lInitOnly)

	IF !lInitOnly
		oProject:oConfigs:AddDebugRelease()
		oProject:UpdateAppConfigs()

		IF SELF:cSelectedConfig == ""
			SELF:cSelectedConfig := oProject:oConfigs:GetDebug():cGuid
		ENDIF
	ENDIF

	oProject:cFolder := Fun.GetDirectory(oProject:cProjectFile)
	IF lInitOnly
		oStream:Close()
	ENDIF
CATCH
//	Fun.ShowTextBox("Error loading project","Project file '" + cFile + "' is corrupted.",3)
	lRet:=FALSE
END TRY

TRY
	IF lInitOnly
		oFileStream:Close()
	ENDIF
END TRY

RETURN lRet

METHOD LoadProject(oProject AS ProjectClass,oStream AS StreamReader,lInitOnly AS LOGIC) AS LOGIC
	LOCAL aAppGroups AS SortedList<STRING,ApplicationGroupClass>
	LOCAL oActiveAppGroup AS ApplicationGroupClass
	LOCAL oConfig AS ProjectConfigClass
	LOCAL cLine,cNewLine AS STRING
	LOCAL sLine AS FileLine
	LOCAL oApp AS AppClass
	LOCAL asSplit AS STRING[]
	LOCAL nAt AS DWORD
	LOCAL n AS INT

	cNewLine := NULL

	oProject:cFileStatusGuid := ""
	oProject:cCustomControlsGuid := ""
	oProject:cDefaultPropertiesGuid := ""

	aAppGroups := SortedList<STRING,ApplicationGroupClass>{}

	DO WHILE oStream:Peek()!=-1

		IF cNewLine == NULL
		    cLine:=oStream:ReadLine()
		    sLine:=Fun.AnalyzeFileLine(cLine)
		ELSE
			cLine := cNewLine
			sLine := Fun.AnalyzeFileLine(cLine)
			cNewLine := NULL
		ENDIF

	    DO CASE
	    CASE Left(sLine:cParam:Trim() , SLen(Glo.gcImportBreak) ) == Glo.gcImportBreak
	    	EXIT

	    CASE sLine:cParam=="PROJECT"
	    	oProject:cName:=sLine:cValue
	    	IF Instr("VULCANNETSAMPLES" , oProject:cName:ToUpper())
	    		oProject:SetSamples()
	    	ENDIF
	    CASE sLine:cParam=="APPLICATION"

	    	IF lInitOnly
	    		EXIT
	    	ENDIF
	    	oApp:=AppClass{oProject,sLine:cValue}
//	    	oApp:lCompiled := TRUE // gia na mhn xreiazetai compile prwta
	    	oProject:AddApp(oApp)
	    	IF oActiveAppGroup != NULL
	    		oActiveAppGroup:AddApp(oApp)
	    	END IF
	    	cNewLine := SELF:LoadApplication(oApp , oStream)
	    	IF !lInitOnly
				oApp:oConfigs:AddDebugRelease()
	    	ENDIF

	    CASE sLine:cParam=="PRGENCODING"
/*	    	TRY
		    	oProject:oPrgEncoding := Glo.gaEncodings[System.Array.IndexOf(Glo.gaEncodingTexts , sLine:cValue) + 1]
	    	CATCH
	    		oProject:oPrgEncoding := Glo.gaEncodings[1]
	    	END TRY*/
            NOP

	    CASE sLine:cParam=="SUPPORTCF"
	    	oProject:lSupportCF := sLine:lValue
	    CASE sLine:cParam=="NOAPPFILECREATE"
	    	oProject:lNoAppFileCreate := sLine:lValue

	    CASE sLine:cParam=="AUTOEXPORTFOLDER"
	    	oProject:cAutoExportFolder := sLine:cValue
	    CASE sLine:cParam=="AUTOEXPORTTYPE"
	    	oProject:nAutoExportType := sLine:nValue
	    CASE sLine:cParam=="AUTOEXPORTINCLUDE"
	    	oProject:nAutoExportInclude := sLine:nValue
	    CASE sLine:cParam=="AUTOEXPORTINTERVAL"
	    	oProject:nAutoExportInterval := sLine:nValue
	    CASE sLine:cParam=="AUTOEXPORTON"
	    	oProject:nAutoExportOn := sLine:nValue


	    CASE sLine:cParam=="COPYASSEMBLIESTOFOLDER"
	    	oProject:cCopyAssembliesToFolder := sLine:cValue
	    CASE sLine:cParam=="DEBUGEXECUTABLE"
	    	oProject:cDebugExecutable := sLine:cValue


/*	    CASE sLine:cParam=="FOLDER"
	    	oProject:cFolder:=sLine:cValue*/
	    CASE sLine:cParam=="OUTPUTFOLDER" // nono gia to project
	    	oProject:cOutputFolder:=sLine:cValue
/*	    CASE sLine:cParam=="EXPORTFOLDER" // nono gia to project
	    	oProject:cExportDir:=sLine:cValue*/
	    CASE sLine:cParam=="DESCRIPTION"
	    	oProject:cDescription:=sLine:cValue
	    CASE sLine:cParam=="GUID"
	    	oProject:cGuid:=sLine:cValue:Trim()


	    CASE sLine:cParam=="PRIMARYAPP" .or. sLine:cParam=="STARTUPAPP"
	    	oProject:cPrimaryApp:=sLine:cValue:Trim()


	    CASE sLine:cParam == "CONFIG" .or. sLine:cParam == "ACTIVECONFIG" // activeconfig moved to projectfilestatus
	    	TRY
	    		cLine := sLine:cValue
		    	nAt:=At(",",cLine)
	    		oConfig := ProjectConfigClass{Left(cLine,nAt-1):Trim()}
	    		cLine := SubStr(cLine,nAt + 1)
		    	nAt:=At(",",cLine)
	    		oConfig:cGuid := Left(cLine,nAt-1):Trim()
	    		oConfig:cSubFolder := SubStr(cLine,nAt + 1):Trim()
	    		oProject:oConfigs:Add(oConfig)
	    	END

	    CASE sLine:cParam == "LAYOUT"
/*	    	TRY
	    		cLine := sLine:cValue
	    		nAt := At("," , cLine)
	    		IF nAt != 0
	    			oProject:aLayouts:Add(DesignerLayout{Left(cLine , nAt - 1):Trim() , SubStr(cLine , nAt + 1):Trim()})
	    		END IF
	    	END TRY*/
            NOP
	    CASE sLine:cParam == "FILESTATUSGUID"
	    	oProject:cFileStatusGuid := sLine:cValue
	    CASE sLine:cParam == "CUSTOMCONTROLSGUID"
	    	oProject:cCustomControlsGuid := sLine:cValue
	    CASE sLine:cParam == "DEFAULTPROPERTIESGUID"
	    	oProject:cDefaultPropertiesGuid := sLine:cValue

	    CASE sLine:cParam == "APPLICATIONGROUP"
	    	LOCAL oAppGroup AS ApplicationGroupClass
	    	asSplit := sLine:cValue:Split(<Char>{','})
	    	FOR n := 1 UPTO asSplit:Length
	    		asSplit[n] := asSplit[n]:Trim()
	    	NEXT
	    	IF asSplit:Length == 3
		    	oAppGroup := ApplicationGroupClass{oProject , asSplit[1]}
		    	oAppGroup:cGuid := asSplit[2]
		    	aAppGroups:Add(oAppGroup:cGuid , oAppGroup)
		    	IF aAppGroups:ContainsKey(asSplit[3])
			    	aAppGroups[asSplit[3]]:AddAppGroup(oAppGroup)
		    	ELSE
			    	oProject:AddAppGroup(oAppGroup)
		    	ENDIF
	    	ELSEIF asSplit:Length == 1
	    		IF asSplit[1]:Length != 0 .and. aAppGroups:ContainsKey(asSplit[1])
	    			oActiveAppGroup := aAppGroups[asSplit[1]]
	    		ELSE
	    			oActiveAppGroup := NULL
	    		END IF
	    	ELSEIF asSplit:Length == 0
	    		oActiveAppGroup := NULL
	    	END IF


	    CASE sLine:cParam=="FRAMEWORK"
/*    		IF sLine:nValue == 2
    			lInitWithCF := TRUE
    		ENDIF*/

            NOP

	    CASE sLine:cParam == "OPENFILE" .or. sLine:cParam == "ACTIVEFILE"
	    	// moved to projectfilestatus
            NOP

	    OTHERWISE

			oProject:oExport:ReadOptions(sLine)

	    END CASE
	ENDDO
RETURN TRUE


METHOD LoadApplication(cAefFile AS STRING , lInitOnly AS LOGIC) AS AppClass
	LOCAL oStream AS StreamReader
	LOCAL cLine AS STRING
	LOCAL sLine AS FileLine
	LOCAL oApp AS AppClass

	IF !File.Exists(cAefFile)
		RETURN NULL
	ENDIF
    TRY
	    oStream:=StreamReader{cAefFile,System.Text.Encoding.GetEncoding(0)}
		DO WHILE oStream:Peek()!=-1
		    cLine:=oStream:ReadLine()
		    sLine:=Fun.AnalyzeFileLine(cLine)
		    IF sLine:cParam=="APPLICATION"
				oApp := AppClass{SELF:Project , sLine:cValue}
				cLine := SELF:LoadApplication(oApp , oStream , lInitOnly)
		    ENDIF
		    IF Left(cLine , SLen(Glo.gcImportBreak)) == Glo.gcImportBreak
		    	EXIT
		    ENDIF
		ENDDO
		oStream:Close()
	CATCH
		oApp := NULL
	END TRY

RETURN oApp

METHOD LoadApplication(oApp AS AppClass,oStream AS StreamReader) AS STRING
	STATIC LOCAL aRuntime := <STRING>{;
	"XSharp.Core","XSharp.VO","XSharp.RT","XSharp.RDD","XSharp.VFP","XSharp.XPP","XSharp.MacroCompiler","XSharp.Data",;
	"VulcanRTFuncs","VulcanRT",;
	"VulcanVOConsoleClasses","VulcanVOGUIClasses","VulcanVOInternetClasses","VulcanVORDDClasses","VulcanVOSQLClasses","VulcanVOSystemClasses","VulcanVOWin32APILibrary",;
	"VOConsoleClasses","VOGUIClasses","VOInternetClasses","VORDDClasses","VOSQLClasses","VOSystemClasses","VOWin32APILibrary";
	} AS STRING[]
	LOCAL cResult AS STRING
	cResult := SELF:LoadApplication(oApp , oStream , FALSE)

	FOR LOCAL nRef := 0 AS INT UPTO oApp:aReferences:Count - 1
		LOCAL oRef AS ReferenceObject
		oRef := (ReferenceObject)oApp:aReferences[nRef]
		FOREACH cTest AS STRING IN aRuntime
			IF oRef:cName:ToUpper():Contains(cTest:ToUpper())
				LOCAL cFileName AS STRING
				cFileName := gcRuntimeFolder + "\" + cTest + ".dll"
				oRef := ReferenceObject{cFileName , ReferenceType.Browse}
				oRef:cFileName := cFileName
				oApp:aReferences[nRef] := oRef
				EXIT
			END IF
		NEXT
	NEXT

RETURN cResult
METHOD LoadApplication(oApp AS AppClass,oStream AS StreamReader, lInitOnly AS LOGIC) AS STRING
	LOCAL sLine AS FileLine
	LOCAL oFile AS FileClass
	LOCAL cLine AS STRING
	LOCAL oAppConfig AS ProjectConfigClass
	LOCAL oFileGroup AS FileGroupClass
	LOCAL cConfig,cGuid AS STRING
	LOCAL oRef , oTestRef AS ReferenceObject
	LOCAL cRef AS STRING
	LOCAL oResFile AS ResourceFileClass
	LOCAL oRes AS ResourceClass
	LOCAL eResType AS ResourceType
	LOCAL oLicFile AS LicenseFileClass
	LOCAL cResType AS STRING
	LOCAL nAt AS DWORD
	LOCAL oEvent AS BuildEvent
	LOCAL n AS INT

	LOCAL lHasRefRT AS LOGIC
	LOCAL lHasRefVO AS LOGIC

	oApp:cGalleryName := oApp:cName
	oApp:cAssembly := oApp:cName
	oApp:lFileGroupFolders := TRUE
	oApp:lFileGroupFolders := FALSE

	DO WHILE oStream:Peek()!=-1
	    cLine:=oStream:ReadLine()
	    sLine:=Fun.AnalyzeFileLine(cLine)

	    IF sLine:cParam=="APPLICATION" .or. ; // sLine:cParam=="APPLICATION" .or. ;
	    	sLine:cParam=="CONFIG" .or. sLine:cParam=="ACTIVECONFIG" .or. ;
	    	sLine:cParam=="FRAMEWORK" .or. sLine:cParam=="APPLICATIONGROUP" .or. ;
	    	Left(sLine:cParam, SLen(Glo.gcImportBreak) )== Glo.gcImportBreak .or. ;
	    	sLine:cParam=="OPENFILE" .or. sLine:cParam=="ACTIVEFILE" .or. ;
	    	Left(sLine:cParam , SLen(Glo.gcImportBreak)) == Glo.gcImportBreak

		    	SELF:LoadApplication_ResolveFileGroups(oApp)
				IF lHasRefVO .and. .not. lHasRefRT
					oApp:AddGacReference("XSharp.RT")
				END IF
		    	RETURN cLine

	    ENDIF

    	IF lInitOnly
		    IF sLine:cParam=="GUID" .or. sLine:cParam=="FILE" .or. sLine:cParam=="FILEGROUP" .or. sLine:cParam=="FILEFOLDER" .or. sLine:cParam=="APPCONFIG"
	    		RETURN ""
	    	ENDIF
    	ENDIF

		IF oAppConfig == NULL
		    Fun.LoadOption(cLine,oApp:oOptions)
		ELSE
		    Fun.LoadConfigOption(cLine,oAppConfig:oOptions)
		ENDIF

		oApp:oExport:ReadOptions(sLine)

		LOCAL cUpper AS STRING
		cUpper := sLine:cValue:ToUpper()
		DO CASE
	    CASE sLine:cParam=="TARGET"
			oApp:nTarget:=sLine:nValue
			IF (oApp:nTarget<0) .or. (oApp:nTarget>2)
				oApp:nTarget:=0
			END IF
		CASE sLine:cParam == "PLATFORM"
			DO CASE
			CASE cUpper == "X86"
				oApp:ePlatform := Platform.x86
			CASE cUpper == "X64"
				oApp:ePlatform := Platform.x64
			OTHERWISE
				oApp:ePlatform := Platform.AnyCPU
			END CASE
	    CASE sLine:cParam == "LANGUAGE"
	    	DO CASE
	    	CASE cUpper == "CSHARP" .or. cUpper == "C#"
	    		oApp:eLanguage := ApplicationLanguage.CSharp
	    	CASE cUpper == "XSHARP" .or. cUpper == "X#"
	    		oApp:eLanguage := ApplicationLanguage.XSharp
	    	CASE cUpper == "VULCAN" .or. cUpper == "VULCAN.NET" .or. cUpper == "VULCANNET"
	    		oApp:eLanguage := ApplicationLanguage.VulcanNet
	    	CASE cUpper == "VISUALBASIC"
	    		oApp:eLanguage := ApplicationLanguage.VBNet
	    	CASE cUpper == "HARBOUR" .or. cUpper == "XHARBOUR"
	    		oApp:eLanguage := ApplicationLanguage.Harbour
	    	OTHERWISE
	    		oApp:eLanguage := ApplicationLanguage.VulcanNet
	    	END CASE
	    CASE sLine:cParam == "RUNTIME"
	    	IF cUpper == "CLR2"
	    		oApp:eClr := ClrType.v2
	    	ELSE
	    		oApp:eClr := ClrType.v4
	    	ENDIF
	    	oApp:eClr := ClrType.v4
	    CASE sLine:cParam == "DIALECT"
	    	DO CASE
	    	CASE cUpper == "CORE"
	    		oApp:eDialect := ApplicationDialect.Core
	    	CASE cUpper == "VULCAN"
	    		oApp:eDialect := ApplicationDialect.Vulcan
	    	CASE cUpper == "VO"
	    		oApp:eDialect := ApplicationDialect.VO
	    	CASE cUpper == "HARBOUR"
	    		oApp:eDialect := ApplicationDialect.Harbour
	    	CASE cUpper == "XBASEPP"
	    		oApp:eDialect := ApplicationDialect.XBasePP
	    	CASE cUpper == "FOXPRO" .or. cUpper == "VFP"
	    		oApp:eDialect := ApplicationDialect.FoxPro
	    	OTHERWISE
	    		IF oApp:eLanguage == ApplicationLanguage.XSharp
		    		oApp:eDialect := ApplicationDialect.Core
	    		ELSE
		    		oApp:eDialect := ApplicationDialect.None
	    		ENDIF
	    	END CASE
	    CASE sLine:cParam=="NAMESPACE"
	    	oApp:cNameSpace:=sLine:cValue
	    CASE sLine:cParam=="ASSEMBLY"
	    	oApp:cAssembly:=sLine:cValue
	    CASE sLine:cParam=="EXTENSION"
	    	oApp:cExtension:=sLine:cValue:Trim()
	    CASE sLine:cParam=="APPLICATIONICON"
	    	oApp:cIconFileName:=sLine:cValue
	    CASE sLine:cParam=="OUTPUTFOLDER"
	    	oApp:cOutputFolder:=sLine:cValue

	    CASE sLine:cParam=="INCLUDEPATH"
	    	oApp:cIncludePaths := sLine:cValue:Trim()
	    CASE sLine:cParam=="STDDEFSFILE"
	    	oApp:cStdDefsFile := sLine:cValue:Trim()
	    CASE sLine:cParam=="APPTORUN"
	    	oApp:cAppToRun := sLine:cValue:Trim()

		// todo Na fygoun auta
	    CASE sLine:cParam=="POSTBUILDENABLED"
	    	IF oApp:aPostBuild:Count == 0;oApp:aPostBuild:Add(BuildEvent{});ENDIF
	    	((BuildEvent)oApp:aPostBuild[0]):lEnabled := ! sLine:cValue:Trim() == "0"
	    CASE sLine:cParam=="POSTBUILDWAIT"
	    	IF oApp:aPostBuild:Count == 0;oApp:aPostBuild:Add(BuildEvent{});ENDIF
	    	((BuildEvent)oApp:aPostBuild[0]):lWait := ! sLine:cValue:Trim() == "0"
	    CASE sLine:cParam=="POSTBUILDEVENT"
	    	IF oApp:aPostBuild:Count == 0;oApp:aPostBuild:Add(BuildEvent{});ENDIF
	    	((BuildEvent)oApp:aPostBuild[0]):cEvent := sLine:cValue:Trim()
	    CASE sLine:cParam=="POSTBUILDARGS"
	    	IF oApp:aPostBuild:Count == 0;oApp:aPostBuild:Add(BuildEvent{});ENDIF
	    	((BuildEvent)oApp:aPostBuild[0]):cArgs := sLine:cValue:Trim()

	    CASE sLine:cParam=="PREBUILDENABLED"
	    	IF oApp:aPreBuild:Count == 0;oApp:aPreBuild:Add(BuildEvent{});ENDIF
	    	((BuildEvent)oApp:aPreBuild[0]):lEnabled := ! sLine:cValue:Trim() == "0"
	    CASE sLine:cParam=="PREBUILDWAIT"
	    	IF oApp:aPreBuild:Count == 0;oApp:aPreBuild:Add(BuildEvent{});ENDIF
	    	((BuildEvent)oApp:aPreBuild[0]):lWait := ! sLine:cValue:Trim() == "0"
	    	((BuildEvent)oApp:aPreBuild[0]):lWait := TRUE
	    CASE sLine:cParam=="PREBUILDEVENT"
	    	IF oApp:aPreBuild:Count == 0;oApp:aPreBuild:Add(BuildEvent{});ENDIF
	    	((BuildEvent)oApp:aPreBuild[0]):cEvent := sLine:cValue:Trim()
	    CASE sLine:cParam=="PREBUILDARGS"
	    	IF oApp:aPreBuild:Count == 0;oApp:aPreBuild:Add(BuildEvent{});ENDIF
	    	((BuildEvent)oApp:aPreBuild[0]):cArgs := sLine:cValue:Trim()


	    CASE sLine:cParam  == "PREBUILDEVENTEXE"
	    	oEvent := BuildEvent{}
	    	oEvent:cEvent := sLine:cValue
	    	IF !oEvent:cEvent:Trim() == ""
		    	oApp:aPreBuild:Add(oEvent)
	    	ENDIF
	    CASE sLine:cParam == "POSTBUILDEVENTEXE"
	    	oEvent := BuildEvent{}
	    	oEvent:cEvent := sLine:cValue
	    	IF !oEvent:cEvent:Trim() == ""
		    	oApp:aPostBuild:Add(oEvent)
	    	ENDIF
	    CASE sLine:cParam=="BUILDEVENTARGS" .and. oEvent != NULL
	    	oEvent:cArgs := sLine:cValue
	    CASE sLine:cParam=="BUILDEVENTENABLED" .and. oEvent != NULL
	    	oEvent:lEnabled := ! sLine:cValue:Trim() == "0"
	    CASE sLine:cParam=="BUILDEVENTWAIT" .and. oEvent != NULL
	    	oEvent:lWait := ! sLine:cValue:Trim() == "0"
	    CASE sLine:cParam=="BUILDEVENTHIDDEN" .and. oEvent != NULL
	    	oEvent:lHidden := ! sLine:cValue:Trim() == "0"



	    CASE sLine:cParam=="SIGNASSEMBLY"
	    	oApp:lSignAssembly := ! sLine:cValue:Trim() == "0"
	    CASE sLine:cParam=="KEYFILE"
	    	oApp:cKeyFile := sLine:cValue:Trim()

	    CASE sLine:cParam=="GALLERYNAME"
	    	IF sLine:cValue:Trim():Length != 0
		    	oApp:cGalleryName := sLine:cValue
	    	ENDIF
	    CASE sLine:cParam=="GALLERYPAGE"
	    	oApp:cGalleryPage := sLine:cValue
	    CASE sLine:cParam=="GALLERYDEFAULTNAME"
	    	oApp:cGalleryDefaultName := sLine:cValue

	    CASE sLine:cParam=="FILE"
	    	oFile:=FileClass{oApp,sLine:cValue}
	    	oApp:AddFile(oFile)
	    	IF .not. lInitOnly
		    	TRY
		    		LOCAL cFileName AS STRING
		    		cFileName := oFile:FullFileName
		    		IF Fun.SafeFileExists(cFileName + ".rc")
		    			oFile:oRcPrg := FileClass{oApp , cFileName + ".rc"}
		    			oFile:oRcPrg:lRcVh := TRUE
		    		END IF
		    		IF Fun.SafeFileExists(cFileName + ".vh")
		    			oFile:oVhPrg := FileClass{oApp , cFileName + ".vh"}
		    			oFile:oVhPrg:lRcVh := TRUE
		    		END IF
		    	END TRY
	    	END IF
	    CASE sLine:cParam=="FILEGUID"
	    	oFile:cGuid:=sLine:cValue:Trim()
	    CASE sLine:cParam=="DESIGNERFILEGUID"
	    	LOCAL oDesigner AS FileClass
	    	oDesigner := FileClass{oApp , oFile:DesignerPrgFileName}
	    	oDesigner:oBasePrg := oFile
	    	oDesigner:lDesPrg := TRUE
	    	oFile:oDesPrg := oDesigner
	    	oApp:AddFile(oDesigner)
	    	oDesigner:cGuid := sLine:cValue:Trim()
	    CASE sLine:cParam=="FILEFILEGROUP"
	    	FOR n:=1 UPTO oApp:GetFileGroupCount()
	    		IF oApp:GetFileGroup(n):cGuid == sLine:cValue:Trim()
	    			oApp:GetFileGroup(n):AddFile(oFile)
	    		END IF
	    	NEXT
	    CASE sLine:cParam=="FILEGROUPGROUP"
	    	oFileGroup:cTemp := sLine:cValue:Trim()
/*	    	FOR n:=1 UPTO oApp:GetFileGroupCount()
	    		IF oApp:GetFileGroup(n):cGuid == sLine:cValue:Trim()
	    			oApp:GetFileGroup(n):aFileGroups:Add(oFileGroup)
	    			oFileGroup:oGroup := oApp:GetFileGroup(n)
	    		END IF
	    	NEXT*/

	    CASE sLine:cParam=="FILEGROUPFOLDERS"
//	    	oApp:lFileGroupFolders := ! sLine:cValue:Trim() == "0"
	    	oApp:lFileGroupFolders := FALSE

	    CASE sLine:cParam=="BUILD" .or. sLine:cParam=="INCLUDEINPROJECTBUILD"
	    	oApp:lIncludeInProjectBuild := ! sLine:cValue:Trim() == "0"
	    CASE sLine:cParam=="INCLUDEINPROJECTSEARCH"
	    	oApp:lIncludeInProjectSearch := ! sLine:cValue:Trim() == "0"
	    CASE sLine:cParam=="INCLUDEINPROJECTEXPORT"
	    	oApp:lIncludeInProjectExport := ! sLine:cValue:Trim() == "0"
	    CASE sLine:cParam=="COMPILE"
	    	IF sLine:cValue:Trim() == "0"
	    		oFile:eFileDiskType := FileDiskType.Text
	    	END IF
	    CASE sLine:cParam=="FILETYPE"
		    DO CASE
		    CASE sLine:cValue:Trim():ToUpper() == "TEXT"
		    	oFile:eFileDiskType := FileDiskType.Text
		    CASE sLine:cValue:Trim():ToUpper() == "BINARY"
		    	oFile:eFileDiskType := FileDiskType.Binary
		    OTHERWISE
		    	oFile:eFileDiskType := FileDiskType.Code
		    END CASE
	    CASE sLine:cParam=="COPYTOBIN"
	    	oFile:lCopyToBin := ! sLine:cValue:Trim() == "0"

	    CASE sLine:cParam=="FILEGROUP"
	    	oFileGroup:=FileGroupClass{oApp,sLine:cValue}
	    	oApp:AddFileGroup(oFileGroup)
	    CASE sLine:cParam=="FILEFOLDER"
	    	oFileGroup:=FileGroupClass{oApp,sLine:cValue}
	    	oFileGroup:lFolder := TRUE
	    	oApp:AddFileGroup(oFileGroup)
	    CASE sLine:cParam=="FILEGROUPGUID"
	    	oFileGroup:cGuid:=sLine:cValue:Trim()

	    CASE sLine:cParam=="LICENSEFILE"
	    	oLicFile := LicenseFileClass{oApp}
	    	oLicFile:cName := Fun.GetFileName(sLine:cValue)
	    	oLicFile:cFileName := sLine:cValue
	    	oApp:AddLicenseFile(oLicFile)
	    CASE sLine:cParam=="LICENSEFILEGUID"
	    	oLicFile:cGuid:=sLine:cValue:Trim()

	    CASE sLine:cParam=="RESOURCEFILE" .or. sLine:cParam=="STANDARDRESOURCEFILE" .or. sLine:cParam=="NATIVERESOURCEFILE"
	    	oResFile := ResourceFileClass{oApp}
	    	IF sLine:cParam=="STANDARDRESOURCEFILE"
		    	oResFile:cName := Fun.GetFileName(sLine:cValue)
		    	oResFile:lStandard := sLine:cParam=="STANDARDRESOURCEFILE"
		    	oResFile:cFileName := sLine:cValue
	    	ELSE
		    	cLine := sLine:cValue
		    	nAt := At("," , cLine)
		    	IF nAt != 0
		    		cResType := Left(cLine , nAt - 1):Trim():ToUpper()
		    		DO CASE
		    		CASE cResType == "DESIGNERS"
		    			oResFile:lStandard := TRUE
		    		CASE cResType == "EXTERNAL"
		    			oResFile:lExternal := TRUE
		    		END CASE
		    		cLine := SubStr(cLine , nAt + 1):Trim()
		    	ENDIF
		    	oResFile:cName := Fun.GetFileName(cLine)
		    	oResFile:cFileName := cLine
		    	oResFile:lNative := sLine:cParam=="NATIVERESOURCEFILE"
	    	ENDIF
	    	oApp:AddResourceFile(oResFile)
	    CASE sLine:cParam=="RESOURCEFILEGUID"
	    	oResFile:cGuid:=sLine:cValue:Trim()
	    CASE sLine:cParam=="LINKED"
	    	oResFile:lLinked := sLine:lValue
	    CASE sLine:cParam=="MODIFIED"
	    	oResFile:lMustCreate := sLine:lValue

	    CASE sLine:cParam=="RESOURCE"
	    	cLine := sLine:cValue
	    	nAt := At("," , cLine)
	    	IF nAt == 0
	    		eResType := ResourceType.Bitmap
	    	ELSE
	    		cResType := Left(cLine , nAt - 1):Trim():ToUpper()
	    		DO CASE
	    		CASE cResType == "BITMAP"
	    			eResType := ResourceType.Bitmap
	    		CASE cResType == "ICON"
	    			eResType := ResourceType.Icon
	    		CASE cResType == "STRING"
	    			eResType := ResourceType.String
	    		CASE cResType == "BYTEARRAY"
	    			eResType := ResourceType.ByteArray
	    		END CASE
	    		cLine := SubStr(cLine , nAt + 1):Trim()
	    	ENDIF
	    	oRes := ResourceClass{oResFile , cLine , eResType}
	    	oResFile:AddResource(oRes)
	    CASE sLine:cParam=="RESOURCEGUID"
	    	oRes:cGuid:=sLine:cValue:Trim()

	    CASE sLine:cParam=="REFERENCEGAC"
	    	LOCAL eClr AS ClrType
	    	eClr := ClrType.v4
	    	cLine := sLine:cValue
	    	nAt := At("," , cLine)
	    	IF (cLine:StartsWith("CLR2") .or. cLine:StartsWith("CLR4")) .and. nAt != 0
	    		IF cLine:StartsWith("CLR2")
	    			eClr := ClrType.v2
	    		ELSE
	    			eClr := ClrType.v4
	    		END IF
	    		eClr := ClrType.v4
	    		cLine := SubStr(cLine , nAt +1)
	    		nAt := At("," , cLine)
	    	END IF
	    	IF nAt == 0
	    		cRef := cLine
	    		cLine := ""
	    	ELSE
	    		cRef := Left(cLine,nAt-1)
	    		cLine := SubStr(cLine , nAt +1)
	    	ENDIF
	    	oRef := ReferenceObject{cRef,ReferenceType.GAC}
	    	ReadReference(oRef , cLine)
	    	oTestRef := GACClass.GetClosestReference(oRef:cName , oRef:cVersion , eClr)
	    	IF oTestRef != NULL
	    		oTestRef:lCopy := oRef:lCopy
	    		oTestRef:eFrameworks := oRef:eFrameworks
	    		oRef := oTestRef
	    		IF oRef:cName:ToUpper() == "XSHARP.VO"
	    			lHasRefVO := TRUE
	    		ELSEIF oRef:cName:ToUpper() == "XSHARP.RT"
	    			lHasRefRT := TRUE
	    		END IF
	    	END IF
	    	oApp:AddReference(oRef)
	    CASE sLine:cParam=="REFERENCEPROJECT"
	    	cLine := sLine:cValue
	    	nAt := At("," , cLine)
	    	IF nAt == 0
	    		cRef := cLine
	    		cLine := ""
	    	ELSE
	    		cRef := Left(cLine,nAt-1)
	    		cLine := SubStr(cLine , nAt +1)
	    	ENDIF
	    	oRef:=ReferenceObject{"*** Unresolved reference ***",ReferenceType.Project}
	    	oRef:cGuid:=cRef
	    	ReadReference(oRef , cLine)
	    	oApp:AddReference(oRef)
	    CASE sLine:cParam=="REFERENCEBROWSE"
	    	cLine := sLine:cValue
	    	nAt := At("," , cLine)
	    	IF nAt == 0
	    		cRef := cLine
	    		cLine := ""
	    	ELSE
	    		cRef := Left(cLine,nAt-1)
	    		cLine := SubStr(cLine , nAt +1)
	    	ENDIF
	    	oRef:=ReferenceObject{cRef,ReferenceType.Browse}
	    	oRef:cFileName := cRef
	    	ReadReference(oRef , cLine)
	    	oApp:AddReference(oRef)


	    CASE sLine:cParam=="FRAMEWORKS"
	    	n := sLine:nValue
	    	DO CASE
	    	CASE n == 1
	    		oApp:eFrameworks := Frameworks.Full
	    	CASE n == 2
	    		oApp:eFrameworks := Frameworks.CF
	    	CASE n == 3
	    		oApp:eFrameworks := Frameworks.Both
	    	END CASE
	    CASE sLine:cParam == "APPCONFIG"
	    	TRY
	    		cLine := sLine:cValue
		    	nAt:=At(",",cLine)
		    	cConfig := Left(cLine,nAt-1):Trim()
		    	cGuid := SubStr(cLine,nAt + 1):Trim()
	    		oAppConfig := ProjectConfigClass{cConfig}
	    		oAppConfig:cGuid := cGuid
	    		oApp:oConfigs:Add(oAppConfig)
	    	END TRY


	    CASE sLine:cParam == "FOLDER"
	    	oApp:cFolder := sLine:cValue
	    CASE sLine:cParam == "PRGSUBFOLDER"
	    	oApp:cPrgFolder := sLine:cValue
	    CASE sLine:cParam == "RESOURCESSUBFOLDER"
	    	oApp:cResourcesFolder := sLine:cValue
/*	    CASE sLine:cParam == "EXPORTFOLDER"
	    	oApp:cExportDir := sLine:cValue*/
	    CASE sLine:cParam == "DESCRIPTION"
	    	oApp:cDescription := Fun.TagLineToCRLF(sLine:cValue)
	    CASE sLine:cParam == "GUID"
	    	oApp:cGuid := sLine:cValue:Trim()

		END CASE

	ENDDO

	IF oApp:aPreBuild:Count == 1 .and. ((BuildEvent)oApp:aPreBuild[0]):cEvent:Trim() == ""
		oApp:aPreBuild:Clear()
	ENDIF
	IF oApp:aPostBuild:Count == 1 .and. ((BuildEvent)oApp:aPostBuild[0]):cEvent:Trim() == ""
		oApp:aPostBuild:Clear()
	ENDIF

	SELF:LoadApplication_ResolveFileGroups(oApp)

	IF lHasRefVO .and. .not. lHasRefRT
		oApp:AddGacReference("XSharp.RT")
	END IF

RETURN ""

METHOD LoadApplication_ResolveFileGroups(oApp AS AppClass) AS VOID
	LOCAL oFileGroup,oTest AS FileGroupClass
	LOCAL n,m AS INT
	FOR n := 1 UPTO oApp:GetFileGroupCount()
		oFileGroup := oApp:GetFileGroup(n)
		FOR m := 1 UPTO oApp:GetFileGroupCount()
			oTest := oApp:GetFileGroup(m)
			IF oTest:cTemp:Trim() == oFileGroup:cGuid:Trim()
				oFileGroup:AddFileGroup(oTest)
//				oFileGroup:aFileGroups:Add(oTest)
//				oTest:oGroup := oFileGroup
//				EXIT
			ENDIF
		NEXT
	NEXT
RETURN

STATIC METHOD ReadReference(oRef AS ReferenceObject,cLine AS STRING) AS VOID
	LOCAL nAt AS DWORD
	LOCAL n AS INT
	TRY
		nAt := At("," , cLine)
		IF nAt!=0
			n := INT(Val(Left(cLine,nAt-1)))
			cLine := SubStr(cLine , nAt + 1):Trim()
			DO CASE
			CASE n == 1
				oRef:eFrameworks := Frameworks.Full
			CASE n == 2
				oRef:eFrameworks := Frameworks.CF
			CASE n == 3
				oRef:eFrameworks := Frameworks.Both
			END CASE
			nAt := At("," , cLine)
			IF nAt == 0
				n := INT(Val(cLine))
				oRef:lCopy := n == 1
			ELSE
				n := INT(Val(Left(cLine,nAt-1)))
				cLine := SubStr(cLine , nAt + 1):Trim()
				oRef:lCopy := n == 1
				IF oRef:eType == ReferenceType.GAC
					oRef:cVersion := cLine:Trim()
				ELSE
					oRef:cLoadingName := cLine:Trim()
				ENDIF
			ENDIF
		ENDIF
	END TRY
RETURN

VIRTUAL METHOD GetCompileOptions(oApp AS AppClass, cConfigName AS STRING) AS CompileOptions
	LOCAL oPrimary AS AppClass
	LOCAL oFile AS FileClass
	LOCAL oOptions AS Options
	LOCAL nTarget AS INT
	LOCAL ePlatform AS Platform
	LOCAL oLibrary AS AppClass
	LOCAL cOutSwitch AS STRING
	LOCAL cFileName AS STRING
	LOCAL oConfig,oAppConfig AS ProjectConfigClass
	LOCAL oConfigOptions AS ConfigOptions
//	LOCAL lSyntaxOnly AS LOGIC
//	LOCAL lDebugInit AS LOGIC
	LOCAL cDll AS STRING
	LOCAL lCF AS LOGIC
	LOCAL n AS INT
	LOCAL lDefineTrace AS LOGIC
	LOCAL lDefineDebug AS LOGIC
	LOCAL cTemp AS STRING

	LOCAL oRef AS ReferenceObject
	LOCAL oRes AS ResourceFileClass
	LOCAL cRef AS STRING

	LOCAL oCompile AS CompileOptions
	oCompile:=CompileOptions{}

	lCF := FALSE
	oConfig:=SELF:Project:oConfigs:Get( iif(cConfigName:ToUpper() == "DEBUG" , 1 , 2) )
	IF oConfig == NULL
		RETURN NULL
	ENDIF
	oCompile:cConfigGuid := oConfig:cGuid
	oCompile:eLanguage := oApp:eLanguage
	oAppConfig := oApp:oConfigs:Get(oConfig:cGuid)
	oOptions:=oApp:oOptions
	oConfigOptions:=oAppConfig:oOptions
	nTarget := oApp:nTarget
	ePlatform := oApp:ePlatform
	IF lCF
		oCompile:cCmdLine := oConfigOptions:cCommandLineCF
	ELSE
		oCompile:cCmdLine := oConfigOptions:cCommandLine
	ENDIF
	oCompile:lDebug := oConfigOptions:lDebug
	lDefineTrace := oConfigOptions:lDefineTrace
	lDefineDebug := oConfigOptions:lDefineDebug
//	lDebugInit := oConfigOptions:lDebugInit

	FOR n:=1 UPTO oApp:GetFileCount()
		oFile := oApp:GetFile(n)
		IF oFile:eFileDiskType == FileDiskType.Code .and. (oFile:oBasePrg == NULL .or. oFile:oBasePrg:eFileDiskType == FileDiskType.Code)
			cFileName := oFile:cFileName
			cFileName := Fun.GetAppFileName(oFile:GetApp(),cFileName)
			oCompile:cSwitches += e"\"" + cFileName + e"\"" + " "
			IF oCompile:lForResponse
				oCompile:cSwitches += ChrW(13) + ChrW(10)
			ENDIF
		END IF
	NEXT

	FOR n := 1 UPTO oApp:GetResourceFileCount()
		oRes := oApp:GetResourceFile(n)
		cRef := oRes:cFileName
		IF oRes:lNative
			LOOP
		ENDIF
		cRef := Fun.GetAppFileName(oApp , cRef)
		oCompile:cSwitches += "/resource:" + e"\"" + cRef + e"\"" + " "
		IF oCompile:lForResponse
			oCompile:cSwitches += ChrW(13) + ChrW(10)
		ENDIF
	NEXT

	FOR n := 1 UPTO oApp:GetReferenceCount()
		oRef := oApp:GetReference(n)
/*		IF (lCF .and. _And( INT(oRef:eFrameworks) , INT(Frameworks.CF)) == 0) .or. ;
			(!lCF .and. _And( INT(oRef:eFrameworks) , INT(Frameworks.Full)) == 0)
			LOOP
		ENDIF*/
		DO CASE
		CASE oRef:eType==ReferenceType.GAC
			cDll := oRef:cName:Trim()

/*			IF glReplaceVulcanRuntimeWithXSharp
				IF cDll:ToUpperInvariant():Contains("VULCAN")
					cDll := cDll:ToUpperInvariant()
					DO CASE
					CASE cDll:StartsWith("VULCANRTFUNCS")
						oRef := GACClass.GetClosestReference("XSharp.RT")
						cDll := oRef:cFileName
						oCompile:aReferences:Add(cDll)
						oCompile:cSwitches+="/r:" + e"\"" + cDll + e"\"" + " "

						oRef := GACClass.GetClosestReference("XSharp.VO")
						cDll := oRef:cName:Trim()
					CASE cDll:StartsWith("VULCANRT")
						oRef := GACClass.GetClosestReference("XSharp.Core")
						cDll := oRef:cName:Trim()
					CASE cDll:StartsWith("VULCANVO")
						oRef := GACClass.GetClosestReference(cDll:Substring(6))
						cDll := oRef:cName:Trim()
					END CASE
				END IF
			END IF*/

			IF !Right(cDll , 4):ToLower() == ".dll"
				cDll += ".dll"
			ENDIF
/*			If !InStr(".DLL" , cDll:ToUpper())
				cDll += ".dll"
			EndIf*/
			IF oRef:cVersion == ""
				oRef := GACClass.GetClosestReference(oRef)
			ENDIF
			IF !oRef:cFileName == "" .and. File.Exists(oRef:cFileName)
				cDll := oRef:cFileName
			ENDIF
			oCompile:cSwitches+="/r:" + e"\"" + cDll + e"\"" + " "
		CASE oRef:eType==ReferenceType.Project
			oLibrary:=SELF:GetAppFromGuid(oRef:cGuid)
			IF oLibrary!=NULL
				oCompile:cSwitches+="/r:" + e"\"" + oLibrary:GetOutputFileName(oConfig:cSubFolder,lCF) + e"\"" + " "
			END IF
		CASE oRef:eType==ReferenceType.Browse
			cRef := oRef:cFileName
			DO CASE
			CASE Left(cRef:ToUpper() , 10) == "%CFFOLDER%"
//				cRef := Fun.ConnectPathAndFileName(Glo.gcCFDir , SubStr(cRef , 11))
				            NOP

			CASE Left(cRef:ToUpper() ,  8) == "%CFPATH%"
//				cRef := Fun.ConnectPathAndFileName(Glo.gcCFDir , SubStr(cRef , 9))
            NOP

			CASE Left(cRef:ToUpper() , 15) == "%PROJECTFOLDER%"
				cRef := Fun.ConnectPathAndFileName(oApp:GetProject():FullFolder , SubStr(cRef , 16))
			CASE Left(cRef:ToUpper() , 13) == "%PROJECTPATH%"
				cRef := Fun.ConnectPathAndFileName(oApp:GetProject():FullFolder , SubStr(cRef , 14))
			CASE Left(cRef:ToUpper() , 9) == "%APPPATH%"
				cRef := Fun.ConnectPathAndFileName(oApp:FullFolder , SubStr(cRef , 10))
			CASE Left(cRef:ToUpper() , 11) == "%APPFOLDER%"
				cRef := Fun.ConnectPathAndFileName(oApp:FullFolder , SubStr(cRef , 12))
			END CASE
			oCompile:cSwitches+="/r:" + e"\"" + cRef + e"\"" + " "
		END CASE
		IF oCompile:lForResponse
			oCompile:cSwitches += ChrW(13) + ChrW(10)
		ENDIF
	NEXT
//	oCompile:cOut:=oApp:cName
	oCompile:cOut:=oApp:cAssembly
	IF oCompile:cOut:Trim() == ""
		oCompile:cOut:=oApp:cName
	ENDIF
	oCompile:oCompileApp:=oApp
	oCompile:lAlreadyCompiled := oApp:IsCompiled(oConfig:cGuid)
//	oCompile:nProject:=oTag:nProject
//	oCompile:nApp:=oTag:nApp
//	oCompile:cAppGuid:=oTag:oPrjItem:GetApp():cGuid
	oCompile:cAppGuid:=oApp:cGuid

/*	IF SELF:Project:IsSamples
		oCompile:cSwitches+="/out:" + e"\"" + oApp:GetFileNameSamples() + e"\"" + " "
		cOutSwitch:="/out:" + e"\"" + oApp:GetFileNameSamples() + e"\"" + " "
		oCompile:cFullOut:=oApp:GetFileNameSamples()
	ELSE*/

		DO CASE
		CASE oCompile:eLanguage == ApplicationLanguage.CSharp
			oCompile:cSwitches := "/out:" + e"\"" + oApp:GetOutputFileName(oConfig:cSubFolder,lCF) + e"\"" + " " + oCompile:cSwitches
		CASE oCompile:eLanguage == ApplicationLanguage.VulcanNet .or. oCompile:eLanguage == ApplicationLanguage.XSharp
			oCompile:cSwitches+="/out:" + e"\"" + oApp:GetOutputFileName(oConfig:cSubFolder,lCF) + e"\"" + " "
			IF oCompile:lForResponse
				oCompile:cSwitches += ChrW(13) + ChrW(10)
			ENDIF
		END CASE

		cOutSwitch := "/out:" + e"\"" + oApp:GetOutputFileName(oConfig:cSubFolder,lCF) + e"\"" + " "

		oCompile:cFullOut := oApp:GetOutputFileName(oConfig:cSubFolder,lCF)
/*	ENDIF*/

	IF !oApp:cNameSpace:Trim()=="" .and. (oApp:eLanguage == ApplicationLanguage.VulcanNet .or. oApp:eLanguage == ApplicationLanguage.XSharp)
		oCompile:cSwitches += "/ns:"+oApp:cNameSpace+" "
		IF oCompile:lForResponse
			oCompile:cSwitches += ChrW(13) + ChrW(10)
		ENDIF
	ENDIF

	IF oApp:lSignAssembly .and. !oApp:cKeyFile:Trim() == ""
		cTemp := oApp:cKeyFile:Trim()
		cTemp := Fun.GetAppFileNameNew(oApp , cTemp)
		DO CASE
		CASE oCompile:eLanguage == ApplicationLanguage.XSharp
			oCompile:cSwitches += "/keyfile:" + Chr(34) + cTemp + Chr(34) + " "
		CASE oCompile:eLanguage == ApplicationLanguage.CSharp
			oCompile:cSwitches += "/keyfile:" + Chr(34) + cTemp + Chr(34) + " "
		CASE oCompile:eLanguage == ApplicationLanguage.VulcanNet
			oCompile:cSwitches += "/snk:" + Chr(34) + cTemp + Chr(34) + " "
		ENDCASE
		IF oCompile:lForResponse
			oCompile:cSwitches += ChrW(13) + ChrW(10)
		ENDIF
	ENDIF

	IF !oApp:cIncludePaths:Trim() == ""
		cTemp := oApp:cIncludePaths:Trim()
		cTemp := Fun.GetAppFileNameNewER(oApp , cTemp)
		cTemp := Fun.PathNoSlash(cTemp)
		oCompile:cSwitches += "/i:" + Chr(34) + cTemp + Chr(34) + " "
		IF oCompile:lForResponse
			oCompile:cSwitches += ChrW(13) + ChrW(10)
		ENDIF
	ENDIF

/*	IF oCompile:eLanguage != ApplicationLanguage.CSharp
		cTemp := Application.StartupPath + "\Config"
		IF Fun.SafeFileExists(cTemp + "\VOWin32APILibrary.vh")
			oCompile:cSwitches += "/i:" + Chr(34) + cTemp + Chr(34) + " "
			IF oCompile:lForResponse
				oCompile:cSwitches += ChrW(13) + ChrW(10)
			ENDIF
		END IF
	ENDIF*/

	// moved to Compile() etc
//	oCompile:cSwitches += "/noconfig "

	LOCAL lDecidedAppToRun := FALSE AS LOGIC
/*	IF .not. String.IsNullOrWhiteSpace( oApp:cAppToRun )
		IF Fun.SafeFileExists( oApp:cAppToRun )
			oCompile:cRun := oApp:cAppToRun:Trim()
			oCompile:nRunTarget := 0
			lDecidedAppToRun := TRUE
		ELSE
			LOCAL oAppToRun AS AppClass
			LOCAL cGuid AS STRING
			LOCAL cName AS STRING
			LOCAL nAt AS INT
			nAt := (INT)At("," , oApp:cAppToRun)
			IF nAt != 0
				cGuid := Left(oApp:cAppToRun , nAt - 1):Trim()
				cName := SubStr(oApp:cAppToRun , nAt + 1):Trim()
			ELSE
				cGuid := oApp:cAppToRun:Trim()
				cName := "<#<none>#>"
			END IF
			oAppToRun := SELF:GetAppFromGuid(cGuid)
			IF oAppToRun == NULL
				oAppToRun := SELF:GetAppFromName(cName)
			END IF
			IF oAppToRun != NULL
				oCompile:cRun := oAppToRun:GetOutputFileName(oConfig:cSubFolder,lCF)
				oCompile:nRunTarget := oAppToRun:nTarget
				lDecidedAppToRun := TRUE
			END IF
		END IF
	END IF*/

	IF .not. lDecidedAppToRun

		IF oApp:nTarget<=1
			oCompile:cRun:=oCompile:cFullOut
			oCompile:nRunTarget:=oApp:nTarget
		ELSE
			oPrimary:=oApp:GetProject():GetPrimaryApp()
			IF oPrimary==NULL .or. oPrimary:nTarget>1
				oCompile:cRun:=""
				oCompile:nRunTarget:=-1
			ELSE
				oCompile:cRun:=oPrimary:GetOutputFileName(oConfig:cSubFolder,lCF)
				oCompile:nRunTarget:=oPrimary:nTarget
			END IF
		END IF

	END IF


oCompile:nTarget:=nTarget

DO CASE
CASE oCompile:eLanguage == ApplicationLanguage.VulcanNet
	Message("*****************  Test is using vulcan language!!!!! ********************")
CASE oCompile:eLanguage == ApplicationLanguage.CSharp

	DO CASE
	CASE nTarget==0
		oCompile:cSwitches:=" /t:exe " + oCompile:cSwitches
		IF oApp != NULL .and. oApp:cExtension:Trim() != ""
			oCompile:cOut += "." + oApp:cExtension
		ELSE
			oCompile:cOut+=".exe"
		ENDIF
	CASE nTarget==1
		oCompile:cSwitches:=" /t:winexe " + oCompile:cSwitches
		IF oApp != NULL .and. oApp:cExtension:Trim() != ""
			oCompile:cOut += "." + oApp:cExtension
		ELSE
			oCompile:cOut+=".exe"
		ENDIF
	CASE nTarget==2
		oCompile:cSwitches:=" /t:library " + oCompile:cSwitches
		oCompile:cOut+=".dll"
	END CASE

	DO CASE
	CASE ePlatform == Platform.x86 .or. (oApp != NULL .and. oConfigOptions:lForceX86)
		oCompile:cSwitches += " /platform:x86"
	CASE ePlatform == Platform.x64
		oCompile:cSwitches += " /platform:x64"
	END CASE

	IF oOptions:lUnsafe
		oCompile:cSwitches+=" /unsafe+"
	END IF
	IF oCompile:lDebug
		oCompile:cSwitches+=" /debug+ "
	ELSE
		oCompile:cSwitches+=" /debug- "
	END IF
	IF oConfigOptions:lWarningsErrors
		oCompile:cSwitches+=" /warnaserror+"
	ELSE
		oCompile:cSwitches+=" /warnaserror-"
	END IF
	IF lDefineDebug
		oCompile:cSwitches += " /define:DEBUG"
	END IF
	IF lDefineTrace
		oCompile:cSwitches += " /define:TRACE"
	END IF

	oCompile:cSwitches+=" " + Fun.CRLFToLine(oOptions:cSwitches)
//	oCompile:cOriginalSwitches+=" " + Fun.CRLFToLine(oOptions:cSwitches)

CASE oCompile:eLanguage == ApplicationLanguage.XSharp

	DO CASE
	CASE nTarget==0 .or. (oApp != NULL .and. oConfigOptions:lForceConsole)
		oCompile:cSwitches+="/t:exe "
		IF oApp != NULL .and. oApp:cExtension:Trim() != ""
			oCompile:cOut += "." + oApp:cExtension
		ELSE
			oCompile:cOut+=".exe"
		ENDIF
	CASE nTarget==1
		oCompile:cSwitches+="/t:winexe "
		IF oApp != NULL .and. oApp:cExtension:Trim() != ""
			oCompile:cOut += "." + oApp:cExtension
		ELSE
			oCompile:cOut+=".exe"
		ENDIF
	CASE nTarget==2
		oCompile:cSwitches+="/t:library "
		oCompile:cOut+=".dll"
	END CASE

	IF oApp != NULL
		DO CASE
		CASE oApp:eDialect == ApplicationDialect.Vulcan
			oCompile:cSwitches += "/dialect:vulcan "
		CASE oApp:eDialect == ApplicationDialect.VO
			oCompile:cSwitches += "/dialect:VO "
		CASE oApp:eDialect == ApplicationDialect.Harbour
			oCompile:cSwitches += "/dialect:harbour "
		CASE oApp:eDialect == ApplicationDialect.XBasePP
			oCompile:cSwitches += "/dialect:Xpp "
		CASE oApp:eDialect == ApplicationDialect.FoxPro
			oCompile:cSwitches += "/dialect:foxpro "
		END CASE
	END IF

	DO CASE
	CASE ePlatform == Platform.x86 .or. (oApp != NULL .and. oConfigOptions:lForceX86)
		oCompile:cSwitches += " /platform:x86"
	CASE ePlatform == Platform.x64
		oCompile:cSwitches += " /platform:x64"
	END CASE

	IF oCompile:lDebug
		oCompile:cSwitches+=" /debug+ "
	END IF
	IF oConfigOptions:lWarningsErrors
		oCompile:cSwitches+=" /warnaserror+"
	ELSE
		oCompile:cSwitches+=" /warnaserror-"
	END IF
	IF lDefineDebug
		oCompile:cSwitches += " /define:DEBUG"
	END IF
	IF lDefineTrace
		oCompile:cSwitches += " /define:TRACE"
	END IF
	IF oConfigOptions:lOptimize
		oCompile:cSwitches+=" /optimize+"
	END IF

	IF oOptions:lZeroArrays
		oCompile:cSwitches+=" /az+"
	END IF
	IF oOptions:lCaseSensitive
		oCompile:cSwitches+=" /cs"
	END IF
	IF oOptions:lImplicitNamespace
		oCompile:cSwitches+=" /ins"
	END IF

	IF oOptions:lVO1
		oCompile:cSwitches+=" /vo1+"
	END IF
	IF oOptions:lVO2
		oCompile:cSwitches+=" /vo2+"
	END IF
	IF oOptions:lVO3
		oCompile:cSwitches+=" /vo3+"
	END IF
	IF oOptions:lVO4
		oCompile:cSwitches+=" /vo4+"
	END IF
	IF oOptions:lVO5
		oCompile:cSwitches+=" /vo5+"
	END IF
	IF oOptions:lVO6
		oCompile:cSwitches+=" /vo6+"
	END IF
	IF oOptions:lVO7
		oCompile:cSwitches+=" /vo7+"
	END IF
	IF oOptions:lVO8
		oCompile:cSwitches+=" /vo8+"
	END IF
	IF oOptions:lVO9
		oCompile:cSwitches+=" /vo9+"
	END IF
	IF oOptions:lVO10
		oCompile:cSwitches+=" /vo10+"
	END IF
	IF oOptions:lVO11
		oCompile:cSwitches+=" /vo11+"
	END IF
	IF oOptions:lVO12
		oCompile:cSwitches+=" /vo12+"
	END IF
	IF oOptions:lVO13
		oCompile:cSwitches+=" /vo13+"
	END IF
	IF oOptions:lVO14
		oCompile:cSwitches+=" /vo14+"
	END IF
/*	IF oOptions:lVO15
		oCompile:cSwitches+=" /vo15+"
	END IF*/
	IF oOptions:lVO16
		oCompile:cSwitches+=" /vo16+"
	END IF

	IF oOptions:lFOX1
		oCompile:cSwitches+=" /fox1+"
	END IF
	IF oOptions:lFOX2
		oCompile:cSwitches+=" /fox2+"
	END IF
	IF oOptions:lXPP1
		oCompile:cSwitches+=" /xpp1+"
	END IF

	IF oOptions:lLateBound
		oCompile:cSwitches+=" /lb+"
	END IF
	IF oOptions:lUnsafe
		oCompile:cSwitches+=" /unsafe+"
	END IF
	IF oOptions:lUndeclared
		oCompile:cSwitches+=" /undeclared+"
	END IF
	IF oOptions:lUseNativeVersion
		oCompile:cSwitches+=" /usenativeversion+"
	END IF
	IF oOptions:lMemVar
		oCompile:cSwitches+=" /memvar+"
	END IF
	IF oOptions:lIgnoreStdDefs
		oCompile:cSwitches+=" /nostddefs+"
	END IF
	IF oOptions:lOvf
		oCompile:cSwitches+=" /ovf+"
	ELSE
		oCompile:cSwitches+=" /ovf-"
	END IF
	IF oOptions:lFOvf
		oCompile:cSwitches+=" /fovf+"
	ELSE
		oCompile:cSwitches+=" /fovf-"
	END IF

	IF oApp != NULL .and. ! String.IsNullOrWhiteSpace( oApp:cStdDefsFile:Trim() )
		cTemp := oApp:cStdDefsFile:Trim()
		cTemp := Fun.GetAppFileNameNewER(oApp , cTemp)
		oCompile:cSwitches += " /stddefs:" + Chr(34) + cTemp + Chr(34) + " "
		IF oCompile:lForResponse
			oCompile:cSwitches += ChrW(13) + ChrW(10)
		ENDIF
	ENDIF

	IF oOptions:lResponseOnly
		oCompile:cSwitches:=" "+cOutSwitch+" "
	END IF

	oCompile:cSwitches+=" " + Fun.CRLFToLine(oOptions:cSwitches)
//	oCompile:cOriginalSwitches+=" " + Fun.CRLFToLine(oOptions:cSwitches)

/*	IF Glo.glDisable165 .and. !Instr("/nowarn:165" , oCompile:cSwitches)
		oCompile:cSwitches += " /nowarn:165"
	ENDIF*/

	IF lCF
		oCompile:cSwitches+=" " + Fun.CRLFToLine(oConfigOptions:cSwitchesCF)
//		oCompile:cOriginalSwitches := Fun.CRLFToLine(oConfigOptions:cSwitchesCF) + " " + oOptions:cSwitches
	ELSE
		oCompile:cSwitches+=" " + Fun.CRLFToLine(oConfigOptions:cSwitches)
//		oCompile:cOriginalSwitches := Fun.CRLFToLine(oConfigOptions:cSwitches) + " " + oOptions:cSwitches
	ENDIF

	// needs to be added to the commandline, never in the response file
/*	IF Glo.glCompileSharedMode .and. .not. oCompile:cSwitches:ToUpper():Contains("/SHARED")
		oCompile:cSwitches += " /shared"
	ENDIF*/

	oCompile:cSwitches += " /utf8output"


/*	IF oApp != NULL .and. oApp:eDialect == ApplicationDialect.Vulcan
		IF .not. oCompile:cSwitches:ToUpper():Contains("/VO15")
			oCompile:cSwitches += " /vo15+ "
		END IF
	END IF*/

END CASE

RETURN oCompile

	VIRTUAL METHOD GetAppFromGuid(cGuid AS STRING) AS AppClass
		IF SELF:Project == NULL
			RETURN NULL
		ENDIF
	RETURN SELF:GetAppFromGuid(SELF:Project , cGuid)
	VIRTUAL METHOD GetAppFromGuid(oProject AS ProjectClass , cGuid AS STRING) AS AppClass
		LOCAL oApp AS AppClass
		LOCAL m AS INT
		IF oProject == NULL
			RETURN NULL
		END IF
		cGuid := cGuid:ToUpper()
		FOR m:=1 UPTO oProject:GetAppCount()
			oApp := oProject:GetApp(m)
			IF oApp:cGuid:ToUpper() == cGuid
				RETURN oApp
			END IF
		NEXT
	RETURN NULL

END CLASS

CLASS CompileOptions
	EXPORT cOut AS STRING
	EXPORT cFullOut AS STRING
	EXPORT cCmdLine AS STRING
	EXPORT cSwitches AS STRING
	EXPORT cAppGuid AS STRING
	EXPORT nTarget AS INT
	EXPORT cRun AS STRING
	EXPORT nRunTarget AS INT
	EXPORT lForResponse AS LOGIC
	EXPORT lAlreadyCompiled AS LOGIC
	EXPORT lResponseOnly AS LOGIC

	EXPORT oCompileApp AS AppClass

	EXPORT eLanguage AS ApplicationLanguage

	EXPORT lDebug AS LOGIC
	EXPORT lToRun AS LOGIC

//	EXPORT cOriginalSwitches AS STRING
	EXPORT cConfigGuid AS STRING

	EXPORT lCompileInsteadOfRun AS LOGIC
	CONSTRUCTOR()
		SELF:cCmdLine:=""
		SELF:cOut:=""
		SELF:cSwitches:=""
		SELF:cAppGuid:=""
		SELF:cRun:=""
//		SELF:cOriginalSwitches := ""
		SELF:cConfigGuid := ""
	RETURN
	METHOD SetCompiled(cConfigGuid AS STRING) AS VOID
		IF SELF:oCompileApp != NULL
			SELF:oCompileApp:SetCompiled(cConfigGuid)
		END IF
	RETURN
END CLASS


INTERFACE IApplicationContainer
	METHOD GetAppCount() AS INT
	METHOD GetApp(nApp AS INT) AS AppClass
	METHOD AddApp(oApp AS AppClass) AS VOID
	METHOD RemoveApp(oApp AS AppClass) AS VOID

	METHOD GetAppGroupsCount() AS INT
	METHOD GetAppGroup(nGroup AS INT) AS ApplicationGroupClass
	METHOD AddAppGroup(oGroup AS ApplicationGroupClass) AS VOID
	METHOD RemoveAppGroup(oGroup AS ApplicationGroupClass) AS VOID
END INTERFACE

ENUM FileDiskType
	MEMBER Code
	MEMBER Text
	MEMBER Binary
END ENUM


CLASS FileLine
	EXPORT cParam AS STRING
	EXPORT cValue AS STRING
	EXPORT nValue AS INT
	EXPORT lValue AS LOGIC
END CLASS

CLASS ProjectFileStatus
	EXPORT cFileGuid AS STRING
	EXPORT aCollapsed AS List<INT>
	CONSTRUCTOR(_cFileGuid AS STRING)
		SELF:cFileGuid := _cFileGuid
		SELF:aCollapsed := List<INT>{}
	RETURN
END CLASS

ENUM AutoExportOn
	MEMBER ProjectOpen := 1
	MEMBER ProjectClose := 2
	MEMBER Interval := 4
	MEMBER Forced := 8
END ENUM


ENUM ProjectType AS INT
	MEMBER EmptyProject
	MEMBER Project
	MEMBER Config
	MEMBER Application

	MEMBER Reference
	MEMBER ReferenceGroup

	MEMBER FileGroup
	MEMBER File
	MEMBER Designer

	MEMBER ApplicationGroup

	MEMBER ResourceGroup
	MEMBER ResourceFile
	MEMBER Resource

	MEMBER LicenseGroup
	MEMBER LicenseFile


//	MEMBER HelpFile
END ENUM

ENUM ResourceType
	MEMBER Bitmap
	MEMBER Icon
	MEMBER @@String
	MEMBER ByteArray
END ENUM


CLASS BuildEvent
	EXPORT lEnabled AS LOGIC
	EXPORT lWait AS LOGIC
	EXPORT cEvent AS STRING
	EXPORT cArgs AS STRING
	EXPORT lCapture AS LOGIC
	EXPORT lHidden AS LOGIC
	CONSTRUCTOR()
		SUPER()
		SELF:lEnabled := TRUE
		SELF:lWait := TRUE
		SELF:cEvent := ""
		SELF:cArgs := ""
		SELF:lCapture := FALSE
		SELF:lHidden := FALSE
	RETURN
END CLASS



CLASS ExportOptions
	EXPORT eType AS ProjectType
	EXPORT lResources AS LOGIC
	EXPORT lImages AS LOGIC
	EXPORT lPrg AS LOGIC
	EXPORT lDll AS LOGIC
	EXPORT lConfig AS LOGIC
CONSTRUCTOR(_eType AS ProjectType)
	SELF:eType := _eType
	SELF:lPrg := TRUE
	METHOD SaveOptions(oStream AS StreamWriter) AS VOID
		oStream:WriteLine("ExportResources = " + iif(SELF:lResources , "1" , "0"))
		IF eType == ProjectType.Project
			oStream:WriteLine("ExportConfig = " + iif(SELF:lConfig , "1" , "0"))
		ENDIF
		oStream:WriteLine("ExportImages = " + iif(SELF:lImages , "1" , "0"))
	RETURN
	METHOD ReadOptions(sLine AS FileLine) AS VOID
		LOCAL cParam AS STRING
		cParam := sLine:cParam:ToUpper()
		DO CASE
		CASE cParam == "EXPORTPRG"
			SELF:lPrg := sLine:lValue
		CASE cParam == "EXPORTRESOURCES"
			SELF:lResources := sLine:lValue
		CASE cParam == "EXPORTIMAGES"
			SELF:lImages := sLine:lValue
		CASE cParam == "EXPORTCONFIG" .and. eType == ProjectType.Project
			SELF:lConfig := sLine:lValue
		CASE cParam == "EXPORTDLL"
			SELF:lDll := sLine:lValue
		END CASE
	RETURN
END CLASS


CLASS GeneralProjectClass
EXPORT cName AS STRING
EXPORT cFolder AS STRING
EXPORT cDescription AS STRING
EXPORT cGuid AS STRING
EXPORT eType AS ProjectType
EXPORT cTemp AS STRING
EXPORT oParent AS GeneralProjectClass // warning: added lately only supported by AppGroups for now
CONSTRUCTOR()
SELF:cGuid := Fun.NewGuid()
SELF:cTemp := ""
RETURN
VIRTUAL ACCESS FullFolder() AS STRING
RETURN SELF:cFolder
VIRTUAL ACCESS IsConfig() AS LOGIC
RETURN cGuid == "CONFIG"
VIRTUAL ACCESS FullFileName() AS STRING
RETURN ""
VIRTUAL METHOD GetProject() AS ProjectClass
RETURN NULL
VIRTUAL METHOD GetApp() AS AppClass
RETURN NULL
VIRTUAL METHOD GetFile() AS FileClass
RETURN NULL
VIRTUAL METHOD AddItem(oItem AS GeneralProjectClass) AS VOID
	DO CASE
	CASE oItem:eType == ProjectType.File
		SELF:AddFile((FileClass)oItem)
	CASE oItem:eType == ProjectType.FileGroup
		SELF:AddFileGroup((FileGroupClass)oItem)
	END CASE
RETURN
VIRTUAL METHOD AddFile(oFile AS FileClass) AS VOID
RETURN
VIRTUAL METHOD AddFileGroup(oGroup AS FileGroupClass) AS VOID
RETURN
VIRTUAL METHOD GetFileGroup() AS FileGroupClass
RETURN NULL
VIRTUAL METHOD GetReference() AS ReferenceObject
RETURN NULL
VIRTUAL METHOD GetResourceFile() AS ResourceFileClass
RETURN NULL
VIRTUAL METHOD GetLicenseFile() AS LicenseFileClass
RETURN NULL
VIRTUAL METHOD RemoveItem(oItem AS GeneralProjectClass) AS VOID
	DO CASE
	CASE oItem:eType == ProjectType.File
		SELF:RemoveFile((FileClass)oItem)
	CASE oItem:eType == ProjectType.FileGroup
		SELF:RemoveFileGroup((FileGroupClass)oItem)
	END CASE
RETURN
VIRTUAL METHOD RemoveFile(oFile AS FileClass) AS VOID
RETURN
VIRTUAL METHOD RemoveFileGroup(oGroup AS FileGroupClass) AS VOID
RETURN
VIRTUAL METHOD GetFileGroup(nGroup AS INT) AS FileGroupClass
RETURN NULL
VIRTUAL METHOD GetFileGroupCount() AS INT
RETURN 0

END CLASS



CLASS ProjectClass INHERIT GeneralProjectClass IMPLEMENTS IApplicationContainer //,IAppGroupContainer
	PROTECT aApps AS List<AppClass>

	EXPORT cProjectFile AS STRING
	EXPORT cPrimaryApp AS STRING
	EXPORT cOutputFolder AS STRING
	EXPORT cOutputFolderCF AS STRING
	EXPORT oConfigs AS ProjectConfigurations
	EXPORT lSupportCF AS LOGIC
	EXPORT lLocked AS LOGIC
	EXPORT lNotFound AS LOGIC
	EXPORT lNoAppFileCreate AS LOGIC

	EXPORT cExportDir AS STRING
	EXPORT cExportAppsDir AS STRING
	EXPORT cExportFilesDir AS STRING
	EXPORT cImportAppsDir AS STRING
	EXPORT cImportFilesDir AS STRING
	EXPORT cAddFilesDir AS STRING

	EXPORT oExport AS ExportOptions
	EXPORT cFileStatusGuid , cDefaultPropertiesGuid , cCustomControlsGuid AS STRING
	EXPORT aLayouts AS ArrayList
	PROTECT aAppGroups AS List<ApplicationGroupClass>
	EXPORT oPrgEncoding AS System.Text.Encoding

	EXPORT cAutoExportFolder AS STRING
	EXPORT nAutoExportType AS INT
	EXPORT nAutoExportOn AS INT
	EXPORT nAutoExportInterval AS INT
	EXPORT nAutoExportInclude AS INT
	EXPORT lDirtySinceAutoExport AS LOGIC

	EXPORT cCopyAssembliesToFolder AS STRING
	EXPORT cDebugExecutable AS STRING

	CONSTRUCTOR(_cFile AS STRING)
	SUPER()
	SELF:cName:=""
	SELF:eType:=ProjectType.Project
	SELF:cFolder:=""
	SELF:cDescription:=""

	SELF:cExportDir := ""
	SELF:cExportAppsDir := ""
	SELF:cExportFilesDir := ""
	SELF:cImportAppsDir := ""
	SELF:cImportFilesDir := ""
	SELF:cAddFilesDir := ""

	SELF:cProjectFile:=_cFile
	SELF:cPrimaryApp:=""
	SELF:aApps:=List<AppClass>{}
	SELF:oConfigs := ProjectConfigurations{}
	SELF:cOutputFolder := "%ProjectPath%\Bin\"
	SELF:cOutputFolderCF := "%ProjectPath%\BinCF\"
	SELF:oExport := ExportOptions{ProjectType.Project}

	SELF:cFileStatusGuid := Fun.NewGuid()
	SELF:cDefaultPropertiesGuid := Fun.NewGuid()
	SELF:cCustomControlsGuid := Fun.NewGuid()

	SELF:aAppGroups := List<ApplicationGroupClass>{}

	SELF:aLayouts := ArrayList{}
	SELF:oPrgEncoding := System.Text.Encoding.Default

	SELF:cAutoExportFolder := "%ProjectPath%\Export"

	SELF:cCopyAssembliesToFolder := ""
	SELF:cDebugExecutable := ""
	RETURN

	STATIC METHOD AdjustProjectPath(oProject AS ProjectClass , cDir REF STRING) AS LOGIC
		LOCAL cProjDir AS STRING
		LOCAL cRet AS STRING

		cProjDir := oProject:cFolder
		cProjDir := Fun.PathAddSlash(cProjDir)
		IF Left(Upper(cDir),SLen(cProjDir))==Upper(cProjDir)
			cRet := SubStr(cDir,SLen(cProjDir))
			IF !Left(cRet,1)=="\"
				cRet:="\"+cRet
			END IF
			cRet:="%ProjectPath%" + cRet
			cDir := cRet
		END IF

	RETURN At("%ProjectPath%" , cDir) == 1

	VIRTUAL METHOD AddApp(oApp AS AppClass) AS VOID
		IF !SELF:aApps:Contains(oApp)
			SELF:aApps:Add(oApp)
			// oxi oApp := self akome, giati AddApp() xrhsimopoiei to ProjectPropertiesDlg
		ENDIF
		oApp:oParent := SELF // etsi ki'alliws, mporei prin na htan se AppGroup
	RETURN

//	 kaleitai mono apo to ProjectPropertiesDlg, ilithiodws ginetai add kathe app
//	 sto neo project class, xwris na kseroume akoma an tha ginei OK to dialog.
//	 Opote to oApp:oParent := SELF den prepei na ekteleitai
	VIRTUAL METHOD AddApp_Hack(oApp AS AppClass) AS VOID
		IF !SELF:aApps:Contains(oApp)
			SELF:aApps:Add(oApp)
			// oxi oApp := self akome, giati AddApp() xrhsimopoiei to ProjectPropertiesDlg
		ENDIF
	RETURN

	VIRTUAL METHOD RemoveApp(oApp AS AppClass) AS VOID
		IF SELF:aApps:Contains(oApp)
			SELF:aApps:Remove(oApp)
		ENDIF
	RETURN

	VIRTUAL METHOD GetPrimaryApp() AS AppClass
	LOCAL n AS INT
	FOR n:=0 UPTO SELF:aApps:Count - 1
		IF SELF:aApps[n]:cGuid:ToUpperInvariant()==SELF:cPrimaryApp:ToUpperInvariant()
			RETURN SELF:aApps[n]
		END IF
	NEXT
	RETURN NULL
	VIRTUAL METHOD GetProject() AS ProjectClass
	RETURN SELF
	VIRTUAL METHOD GetApp(nApp AS INT) AS AppClass
	RETURN SELF:aApps[nApp - 1]
	VIRTUAL METHOD GetAppCount() AS INT
	RETURN SELF:aApps:Count
	VIRTUAL METHOD GetAppGroupsCount() AS INT
	RETURN SELF:aAppGroups:Count
	VIRTUAL METHOD GetAppGroup(nGroup AS INT) AS ApplicationGroupClass
	RETURN SELF:aAppGroups[nGroup - 1]
	VIRTUAL METHOD RemoveAppGroup(oGroup AS ApplicationGroupClass) AS VOID
		IF SELF:aAppGroups:Contains(oGroup)
			SELF:aAppGroups:Remove(oGroup)
		END IF
	RETURN
	VIRTUAL METHOD AddAppGroup(oGroup AS ApplicationGroupClass) AS VOID
		IF .not. SELF:aAppGroups:Contains(oGroup)
			SELF:aAppGroups:Add(oGroup)
			oGroup:oParent := SELF
		END IF
	RETURN

	// apo ProjectPropertiesDlg, gia na mhn ekteleitai to oGroup:oParent := SELF akoma, giati
	// mporei to dialog na ginei cancel
	VIRTUAL METHOD AddAppGroup_Hack(oGroup AS ApplicationGroupClass) AS VOID
		IF .not. SELF:aAppGroups:Contains(oGroup)
			SELF:aAppGroups:Add(oGroup)
		END IF
	RETURN

VIRTUAL METHOD UpdateSubItems() AS VOID
	LOCAL n AS INT
	FOR n:=1 UPTO SELF:GetAppCount()
		LOCAL oApp AS AppClass
		oApp := SELF:GetApp(n)
		oApp:oProject := SELF
		IF oApp:oParent == NULL .or. oApp:oParent:GetType() != TypeOf(ApplicationGroupClass)
			oApp:oParent := SELF
		END IF
	NEXT
	FOR n:=1 UPTO SELF:GetAppGroupsCount()
		LOCAL oAppGroup AS ApplicationGroupClass
		oAppGroup := SELF:GetAppGroup(n)
		oAppGroup:oProject := SELF
		IF oAppGroup:oParent == NULL .or. oAppGroup:oParent:GetType() != TypeOf(ApplicationGroupClass)
			oAppGroup:oParent := SELF
		END IF
	NEXT
RETURN
VIRTUAL METHOD UpdateAppConfigs() AS VOID
	LOCAL n AS INT
	FOR n:=1 UPTO SELF:GetAppCount()
		SELF:GetApp(n):UpdateConfigsFromProject()
	NEXT
RETURN
VIRTUAL METHOD SetSamples() AS VOID
	SELF:cGuid := "99999999-9999-9999-9999-999999999999"
RETURN
VIRTUAL ACCESS IsSamples AS LOGIC
RETURN SELF:cGuid == "99999999-9999-9999-9999-999999999999"

VIRTUAL ACCESS FullTemplatesFolder AS STRING
RETURN Fun.ConnectPaths(SELF:cFolder , "Templates")
VIRTUAL ACCESS FullStatusFileName AS STRING
RETURN Fun.ConnectPathAndFileName(SELF:cFolder , Fun.GetFileNameNoExt(SELF:cProjectFile) + ".vicfg")

VIRTUAL ACCESS FullBinFolder AS STRING
RETURN Fun.GetProjectFileNameNew(SELF , SELF:cOutputFolder)
VIRTUAL ACCESS FullBinCFFolder AS STRING
RETURN Fun.GetProjectFileNameNew(SELF , SELF:cOutputFolderCF)

VIRTUAL METHOD GetAbsolutePath(cPath AS STRING) AS STRING
RETURN Fun.GetProjectFileName(SELF,cPath)

VIRTUAL METHOD GetAppFromGuid(cGuid AS STRING) AS AppClass
LOCAL oApp AS AppClass
LOCAL m AS INT

cGuid := cGuid:ToUpperInvariant()
FOR m:=0 UPTO SELF:aApps:Count - 1
	oApp:=SELF:aApps[m]
	IF oApp:cGuid:ToUpperInvariant() == cGuid
		RETURN oApp
	END IF
NEXT
RETURN NULL

VIRTUAL METHOD GetCustomControlsFileName() AS STRING
	LOCAL cFileName AS STRING
	cFileName := Fun.ConnectPaths(SELF:cFolder , "Config")
	cFileName := Fun.ConnectPathAndFileName(cFileName , "CustomControls.vicfg")
RETURN cFileName
VIRTUAL METHOD GetDefaultPropertiesFileName() AS STRING
	LOCAL cFileName AS STRING
	cFileName := Fun.ConnectPaths(SELF:cFolder , "Config")
	cFileName := Fun.ConnectPathAndFileName(cFileName , "DefaultProperties.vicfg")
RETURN cFileName

VIRTUAL METHOD UpdateSavedTime() AS VOID
	LOCAL n AS INT
	FOR n := 1 UPTO SELF:GetAppCount()
		SELF:GetApp(n):UpdateSavedTime()
	NEXT
RETURN

VIRTUAL ACCESS FullExportFolder AS STRING
RETURN Fun.GetProjectFileNameNew(SELF , SELF:cAutoExportFolder)

METHOD FindAppGroup(cGroupGuid AS STRING) AS ApplicationGroupClass
	FOR LOCAL nAppGroup := 1 AS INT UPTO SELF:GetAppGroupsCount()
		LOCAL oAppGroup AS ApplicationGroupClass
		oAppGroup := SELF:GetAppGroup(nAppGroup)
		IF oAppGroup:cGuid == cGroupGuid
			RETURN oAppGroup
		END IF
		oAppGroup := SELF:FindAppGroup(cGroupGuid, oAppGroup)
		IF oAppGroup != NULL
			RETURN oAppGroup
		END IF
	NEXT
RETURN NULL
METHOD FindAppGroup(cGroupGuid AS STRING, oAppGroupParent AS ApplicationGroupClass) AS ApplicationGroupClass
	IF oAppGroupParent:cGuid == cGroupGuid
		RETURN oAppGroupParent
	END IF
	FOR LOCAL nChildGroup := 1 AS INT UPTO oAppGroupParent:GetAppGroupsCount()
		IF oAppGroupParent:GetAppGroup(nChildGroup):cGuid == cGroupGuid
			RETURN oAppGroupParent:GetAppGroup(nChildGroup)
		END IF
	NEXT
RETURN NULL

END CLASS


CLASS AppClass INHERIT GeneralProjectClass
	PROTECT aFiles AS ArrayList
	PROTECT aFileGroups AS ArrayList
	EXPORT aReferences AS ArrayList
	PROTECT aResourceFiles AS ArrayList
	PROTECT aLicenseFiles AS ArrayList

	EXPORT oOptions AS Options
	EXPORT oProject AS ProjectClass
	EXPORT nTemplate AS INT
	EXPORT oConfigs AS ProjectConfigurations
	EXPORT nTarget AS Int32
	EXPORT ePlatform AS Platform
	EXPORT eFrameworks AS Frameworks
	EXPORT cNameSpace AS STRING
	EXPORT cAssembly AS STRING
	EXPORT cExtension AS STRING
	EXPORT cPrgFolder AS STRING
	EXPORT cResourcesFolder AS STRING
	EXPORT cOutputFolder AS STRING
	EXPORT cGalleryName AS STRING
	EXPORT cGalleryPage AS STRING
	EXPORT cGalleryFile AS STRING
	EXPORT cGalleryDefaultName AS STRING
//	EXPORT cExportDir AS STRING
	EXPORT lIncludeInProjectBuild AS LOGIC
	EXPORT lIncludeInProjectSearch AS LOGIC
	EXPORT lIncludeInProjectExport AS LOGIC
	EXPORT oExport AS ExportOptions
	EXPORT cIconFileName AS STRING
	EXPORT eLanguage AS ApplicationLanguage
	EXPORT eClr AS ClrType

	EXPORT eDialect AS ApplicationDialect

	EXPORT aPostBuild AS ArrayList
	EXPORT aPreBuild AS ArrayList

	EXPORT lSignAssembly AS LOGIC
	EXPORT cKeyFile AS STRING

	EXPORT lFileGroupFolders AS LOGIC
	EXPORT cIncludePaths AS STRING
	EXPORT cStdDefsFile AS STRING

	EXPORT dLastTouched AS DateTime
	EXPORT dRealSavedTime AS DateTime

	EXPORT lDirtySinceAutoExport AS LOGIC

	EXPORT cAppToRun AS STRING

	EXPORT aCompilerMessages := List<STRING>{} AS List<STRING>
	EXPORT cCompilerOptions AS STRING

CONSTRUCTOR(_oProject AS ProjectClass,_cName AS STRING)
	SUPER()
	SELF:cName:=_cName
	SELF:cGalleryName := ""
	SELF:cGalleryFile := ""
	SELF:cGalleryPage := ""
//	SELF:cExportDir := ""
	SELF:cPrgFolder := "\"
	SELF:cResourcesFolder := "\"
	SELF:cIconFileName := ""
	SELF:lIncludeInProjectBuild := TRUE
	SELF:lIncludeInProjectSearch := TRUE
	SELF:lIncludeInProjectExport := TRUE
	SELF:eType:=ProjectType.Application
	SELF:oProject:=_oProject
	SELF:aFiles:=ArrayList{}
	SELF:aFileGroups:=ArrayList{}
	SELF:oOptions:=Options{}
	SELF:cNameSpace:=""
	SELF:cAssembly:=""
	SELF:cExtension:=""
	SELF:oConfigs := ProjectConfigurations{}
	SELF:eFrameworks := Frameworks.Full
	SELF:nTarget:=0
	SELF:ePlatform := Platform.AnyCPU
	SELF:eLanguage := ApplicationLanguage.XSharp
	SELF:eClr := ClrType.v4
	SELF:eDialect := ApplicationDialect.None
	SELF:cKeyFile := ""

	SELF:aPostBuild := ArrayList{}
	SELF:aPreBuild := ArrayList{}

	SELF:aReferences := ArrayList{}
	SELF:aResourceFiles := ArrayList{}
	SELF:aLicenseFiles := ArrayList{}
	SELF:oExport := ExportOptions{ProjectType.Application}

	SELF:cIncludePaths := ""
	SELF:cStdDefsFile := ""

//	SELF:dLastTouched := DateTime.MaxValue
	SELF:dLastTouched := DateTime.Now

	SELF:dRealSavedTime := DateTime.MinValue

	SELF:cAppToRun := ""
RETURN

VIRTUAL METHOD SetDirty() AS LOGIC
RETURN SELF:SetDirty(DateTime.Now)
VIRTUAL METHOD SetDirty(dDateTime AS DateTime) AS LOGIC
	IF SELF:dLastTouched == dDateTime
		RETURN FALSE
	END IF
	SELF:dLastTouched := dDateTime
RETURN TRUE

VIRTUAL METHOD UpdateSavedTime() AS VOID
	LOCAL oFile AS FileClass
	LOCAL n AS INT
	FOR n := 1 UPTO SELF:GetFileCount()
		oFile := SELF:GetFile(n)
		IF oFile:dRealSavedTime > SELF:dRealSavedTime
			SELF:drealSavedTime := oFile:dRealSavedTime
		END IF
	NEXT
RETURN

VIRTUAL ACCESS AppFile AS STRING
	LOCAL cAppFile AS STRING
	IF SELF:GetProject() == NULL
		cAppFile := ""
	ELSE
		cAppFile := SELF:GetProject():GetAbsolutePath(SELF:cFolder)
		cAppFile := Fun.ConnectPathAndFileName(cAppFile , SELF:cName + ".viapp")
	ENDIF
RETURN cAppFile
VIRTUAL METHOD AppFileForName(cTestName AS STRING , cTestFolder AS STRING) AS STRING
	LOCAL cAppFile AS STRING
	IF SELF:GetProject() == NULL
		cAppFile := ""
	ELSE
		cAppFile := SELF:GetProject():GetAbsolutePath(cTestFolder)
		cAppFile := Fun.ConnectPathAndFileName(cAppFile , cTestName + ".xiapp")
	ENDIF
RETURN cAppFile
VIRTUAL ACCESS FullFolder() AS STRING
	LOCAL cRet AS STRING
	cRet := SELF:cFolder
	cRet := Fun.GetProjectFileName(SELF:GetProject() , cRet)
RETURN cRet
VIRTUAL ACCESS FullResourcesFolder() AS STRING
	LOCAL cRet AS STRING
	cRet := Fun.ConnectPaths(SELF:FullFolder , SELF:cResourcesFolder)
RETURN cRet
VIRTUAL ACCESS FullStandardResourcesFileName() AS STRING
	LOCAL cRet AS STRING
	cRet := Fun.ConnectPaths(SELF:FullFolder , SELF:cResourcesFolder)
	cRet := Fun.ConnectPathAndFileName(cRet , "Designers.resources")
RETURN cRet
VIRTUAL ACCESS FullPrgFolder() AS STRING
	LOCAL cRet AS STRING
	cRet := Fun.ConnectPaths(SELF:FullFolder , SELF:cPrgFolder)
RETURN cRet

VIRTUAL METHOD SetCompiled(cConfigGuid AS STRING) AS VOID
	SELF:oConfigs:SetCompiled(cConfigGuid)
RETURN
VIRTUAL METHOD IsCompiled(cConfigGuid AS STRING) AS LOGIC
RETURN SELF:oConfigs:IsCompiled(cConfigGuid)
VIRTUAL METHOD ResetCompiled() AS VOID
	IF SELF:oConfigs != NULL
		SELF:oConfigs:ResetCompiled()
	END IF
RETURN

VIRTUAL METHOD GetOutputFileName(cSubFolder AS STRING,lCF AS LOGIC) AS STRING
LOCAL cName AS STRING
IF String.IsNullOrEmpty(SELF:cOutputFolder)
	IF lCF
		cName:=Fun.GetProjectFileName(SELF:GetProject(),SELF:oProject:cOutputFolderCF)
	ELSE
		cName:=Fun.GetProjectFileName(SELF:GetProject(),SELF:oProject:cOutputFolder)
	ENDIF
	cName:=Fun.ConnectPaths(cName,cSubFolder)
ELSE
	cName := SELF:cOutputFolder
	Fun.ReplaceText(REF cName , "%ProjectOutputPath%" , iif(lCF , SELF:oProject:cOutputFolderCF , SELF:oProject:cOutputFolder))
	Fun.ReplaceText(REF cName , "%ProjectOutputCFPath%" , SELF:oProject:cOutputFolderCF)
	Fun.ReplaceText(REF cName , "%ProjectOutputFolder%" , iif(lCF , SELF:oProject:cOutputFolderCF , SELF:oProject:cOutputFolder))
	Fun.ReplaceText(REF cName , "%ProjectOutputCFFolder%" , SELF:oProject:cOutputFolderCF)
	Fun.ReplaceText(REF cName , "%AppPath%" , SELF:cFolder)
	Fun.ReplaceText(REF cName , "%AppFolder%" , SELF:cFolder)
	// output paths above may contain %OutputPath% tokens themselves, so need to be examined first
	Fun.ReplaceText(REF cName , "%ProjectPath%" , SELF:oProject:cFolder)
	Fun.ReplaceText(REF cName , "%ProjectFolder%" , SELF:oProject:cFolder)
	Fun.ReplaceText(REF cName , "%ConfigPath%" , cSubFolder)
	cSubFolder := cSubFolder:Replace("\" , "")
	cSubFolder := cSubFolder:Replace("/" , "")
	Fun.ReplaceText(REF cName , "%ConfigFolder%" , cSubFolder)
END IF
//cName:=Fun.ConnectPathAndFileName(cName,SELF:cName)
IF SELF:cAssembly:Trim() == ""
	cName:=Fun.ConnectPathAndFileName(cName,SELF:cName)
ELSE
	cName:=Fun.ConnectPathAndFileName(cName,SELF:cAssembly)
ENDIF
IF SELF:nTarget<=1
	IF SELF:cExtension:Trim() != ""
		cName += "." + SELF:cExtension:Trim()
	ELSE
		cName+=".exe"
	ENDIF
ELSE
	cName+=".dll"
END IF
RETURN cName
VIRTUAL METHOD IsPrimary() AS LOGIC
IF SELF:oProject==NULL
	RETURN FALSE
END IF
RETURN Upper(SELF:oProject:cPrimaryApp)==Upper(SELF:cGuid)
VIRTUAL METHOD SetPrimary() AS VOID
SELF:oProject:cPrimaryApp:=SELF:cGuid
RETURN
VIRTUAL METHOD GetProject() AS ProjectClass
RETURN SELF:oProject
VIRTUAL METHOD GetApp() AS AppClass
RETURN SELF
VIRTUAL METHOD GetFileFromGuid(cGuid AS STRING) AS FileClass
	LOCAL oFile AS FileClass
	LOCAL n AS INT
	FOR n := 0 UPTO SELF:aFiles:Count - 1
		oFile := (FileClass)SELF:aFiles[n]
		IF oFile:cGuid == cGuid
			RETURN oFile
		ENDIF
		IF oFile:oRcPrg != NULL .and. oFile:oRcPrg:cGuid == cGuid
			RETURN oFile:oRcPrg
		END IF
		IF oFile:oVhPrg != NULL .and. oFile:oVhPrg:cGuid == cGuid
			RETURN oFile:oVhPrg
		END IF
	NEXT
RETURN NULL
VIRTUAL METHOD GetResFileFromGuid(cGuid AS STRING) AS ResourceFileClass
	LOCAL oFile AS ResourceFileClass
	LOCAL n AS INT
	FOR n := 1 UPTO SELF:GetResourceFileCount()
		oFile := SELF:GetResourceFile(n)
		IF oFile:cGuid == cGuid
			RETURN oFile
		ENDIF
	NEXT
RETURN NULL
VIRTUAL METHOD GetResourceFromGuid(cGuid AS STRING) AS ResourceClass
	LOCAL oFile AS ResourceFileClass
	LOCAL oRes AS ResourceClass
	LOCAL n AS INT
	FOR n := 1 UPTO SELF:GetResourceFileCount()
		oFile := SELF:GetResourceFile(n)
		oRes := oFile:GetResourceFromGuid(cGuid)
		IF oRes != NULL
			RETURN oRes
		ENDIF
	NEXT
RETURN NULL

VIRTUAL METHOD GetFile(nFile AS INT) AS FileClass
RETURN (FileClass)SELF:aFiles[nFile - 1]
VIRTUAL METHOD GetFileCount() AS INT
RETURN SELF:aFiles:Count
// todo katalathos, bgazei lathh to peverify
/*Method RemoveFileGroup(nGroup As Int) As Int
Return Self:aFileGroups:RemoveAt(nGroup)
Method RemoveFileGroup(oGroup As FileGroupClass) As Int
Return Self:aFileGroups:Remove(oGroup)*/
VIRTUAL METHOD GetFileGroup(nGroup AS INT) AS FileGroupClass
RETURN (FileGroupClass)SELF:aFileGroups[nGroup-1]
VIRTUAL METHOD GetFileGroupCount() AS INT
RETURN SELF:aFileGroups:Count
VIRTUAL METHOD RemoveFileGroup(nGroup AS INT) AS VOID
SELF:aFileGroups:RemoveAt(nGroup - 1)
RETURN
VIRTUAL METHOD RemoveFileGroup(oGroup AS FileGroupClass) AS VOID
IF SELF:aFileGroups:Contains(oGroup)
	SELF:aFileGroups:Remove(oGroup)
END IF
RETURN
VIRTUAL METHOD AddFileGroup(oGroup AS FileGroupClass) AS VOID
IF !SELF:aFileGroups:Contains(oGroup)
	SELF:aFileGroups:Add(oGroup)
END IF
RETURN
VIRTUAL METHOD RemoveFile(oFile AS FileClass) AS VOID
	IF SELF:aFiles:Contains(oFile)
		SELF:aFiles:Remove(oFile)
		oFile:oApp := NULL
	END IF
RETURN
VIRTUAL METHOD AddFile(oFile AS FileClass) AS VOID
	IF !SELF:aFiles:Contains(oFile)
		SELF:aFiles:Add(oFile)
	END IF
	oFile:oApp := SELF
RETURN


VIRTUAL METHOD GetLicenseFile(nRes AS INT) AS LicenseFileClass
RETURN (LicenseFileClass)SELF:aLicenseFiles[nRes - 1]
VIRTUAL METHOD GetLicenseFileCount() AS INT
RETURN SELF:aLicenseFiles:Count
VIRTUAL METHOD AddLicenseFile(oFile AS LicenseFileClass) AS VOID
	SELF:aLicenseFiles:Add(oFile)
	oFile:oApp := SELF
RETURN
VIRTUAL METHOD RemoveLicenseFile(oFile AS LicenseFileClass) AS VOID
	SELF:aLicenseFiles:Remove(oFile)
	oFile:oApp := NULL
RETURN
VIRTUAL METHOD LicenseFileExists(cFileName AS STRING) AS LOGIC
	LOCAL oFile AS LicenseFileClass
	LOCAL n AS INT
	cFileName := cFileName:ToUpper()
	FOR n := 1 UPTO SELF:GetLicenseFileCount()
		oFile := SELF:GetLicenseFile(n)
		IF oFile:FullFileName:ToUpper() == Fun.GetAppFileName(SELF:GetApp() , cFileName):ToUpper()
			RETURN TRUE
		ENDIF
	NEXT
RETURN FALSE


VIRTUAL METHOD GetResourceFile(cResFile AS STRING) AS ResourceFileClass
	LOCAL oResFile AS ResourceFileClass
	LOCAL n AS INT
	cResFile := cResFile:ToUpper()
	FOR n := 1 UPTO SELF:GetResourceFileCount()
		oResFile := SELF:GetResourceFile(n)
		IF oResFile:cName:ToUpper() == cResFile
			RETURN oResFile
		END IF
	NEXT
RETURN NULL
VIRTUAL METHOD GetResourceFile(nRes AS INT) AS ResourceFileClass
RETURN (ResourceFileClass)SELF:aResourceFiles[nRes - 1]
VIRTUAL METHOD GetResourceFileCount() AS INT
RETURN SELF:aResourceFiles:Count
VIRTUAL METHOD AddResourceFile(oFile AS ResourceFileClass) AS VOID
	SELF:aResourceFiles:Add(oFile)
	oFile:oApp := SELF
RETURN
VIRTUAL METHOD RemoveResourceFile(oFile AS ResourceFileClass) AS VOID
	SELF:aResourceFiles:Remove(oFile)
	oFile:oApp := NULL
RETURN
VIRTUAL METHOD ResourceFileExists(cFileName AS STRING) AS LOGIC
	LOCAL oFile AS ResourceFileClass
	LOCAL n AS INT
	cFileName := cFileName:ToUpper()
	FOR n := 1 UPTO SELF:GetResourceFileCount()
		oFile := SELF:GetResourceFile(n)
//		IF oFile:FullFileName:ToUpper() == cFileName
		IF oFile:FullFileName:ToUpper() == Fun.GetAppFileName(SELF:GetApp() , cFileName):ToUpper()
			RETURN TRUE
		ENDIF
	NEXT
RETURN FALSE
VIRTUAL METHOD GetStandardResourceFile() AS ResourceFileClass
	LOCAL oRes AS ResourceFileClass
	LOCAL n AS INT
	FOR n := 1 UPTO SELF:GetResourceFileCount()
		oRes := SELF:GetResourceFile(n)
		IF oRes:lStandard
			RETURN oRes
		ENDIF
	NEXT
RETURN NULL

VIRTUAL METHOD GetReferenceCount() AS INT
RETURN SELF:aReferences:Count
VIRTUAL METHOD GetReference(nRef AS INT) AS ReferenceObject
RETURN (ReferenceObject)SELF:aReferences[nRef - 1]
VIRTUAL METHOD AddReference(oRef AS ReferenceObject) AS LOGIC
	SELF:aReferences:Add(oRef)
RETURN TRUE
VIRTUAL METHOD AddGacReference(cRef AS STRING) AS LOGIC
	LOCAL oRef AS ReferenceObject
	oRef := GACClass.GetClosestReference(cRef , "" , ClrType.v4)
	IF oRef == NULL
		RETURN FALSE
	END IF
	oRef:lCopy := FALSE
	oRef:eFrameworks := Frameworks.Full
	SELF:AddReference(oRef)
RETURN TRUE
VIRTUAL METHOD RemoveReference(nRef AS INT) AS LOGIC
	nRef --
	IF nRef >= 0 .and. nRef < SELF:aReferences:Count
		SELF:aReferences:RemoveAt(nRef)
		RETURN TRUE
	END IF
RETURN FALSE
VIRTUAL METHOD RemoveReference(cName AS STRING , eType AS ReferenceType) AS LOGIC
	LOCAL oRef AS ReferenceObject
	LOCAL n AS INT
	FOR n := 0 UPTO SELF:aReferences:Count - 1
		oRef := (ReferenceObject)SELF:aReferences[n]
		IF oRef:eType == eType .and. oRef:cName:ToUpper() == cName:ToUpper()
			SELF:aReferences:RemoveAt(n)
			RETURN TRUE
		END IF
	NEXT
RETURN FALSE
VIRTUAL METHOD UpdateReference(oOld AS ReferenceObject , oNew AS ReferenceObject) AS LOGIC
	LOCAL oRef AS ReferenceObject
	LOCAL n AS INT
	FOR n := 0 UPTO SELF:aReferences:Count - 1
		oRef := (ReferenceObject)SELF:aReferences[n]
		IF oRef == oOld
			SELF:aReferences[n] := oNew
			RETURN TRUE
		END IF
	NEXT
RETURN FALSE
VIRTUAL METHOD ClearReferences() AS VOID
	SELF:aReferences:Clear()
RETURN

VIRTUAL METHOD UpdateConfigsFromProject() AS VOID
SELF:UpdateConfigsFromProject(FALSE)
RETURN
VIRTUAL METHOD UpdateConfigsFromProject(lMessages AS LOGIC) AS VOID
	LOCAL oConfig,oProjConfig,oAppConfig AS ProjectConfigClass
	LOCAL n AS INT

	FOR n:=1 UPTO SELF:oProject:oConfigs:Count
		oProjConfig := SELF:oProject:oConfigs:@@Get(n)
		oConfig := SELF:oConfigs:@@Get( oProjConfig:cGuid)
		IF oConfig == NULL
			oConfig := SELF:oConfigs:GetFromName( oProjConfig:cName)
			IF oConfig == NULL
				oConfig := ProjectConfigClass{oProjConfig:cName,oProjConfig:cGuid}
				SELF:oConfigs:Add(oConfig)
			ELSE
				oConfig:cGuid := oProjConfig:cGuid
			ENDIF
		ELSE
			oConfig:cName := oProjConfig:cName
		ENDIF
	NEXT

	n:=1
	DO WHILE n <= SELF:oConfigs:Count
		oAppConfig := SELF:oConfigs:@@Get(n)
		IF SELF:oProject:oConfigs:@@Get( oAppConfig:cGuid) == NULL
			SELF:oConfigs:Remove(n)
		ELSE
			n++
		ENDIF
	ENDDO

RETURN

VIRTUAL METHOD UpdateSubItems() AS VOID
	LOCAL n AS INT
	FOR n:=1 UPTO SELF:GetFileCount()
		SELF:GetFile(n):oApp := SELF
	NEXT
	FOR n:=1 UPTO SELF:GetFileGroupCount()
		SELF:GetFileGroup(n):oApp := SELF
	NEXT
	FOR n:=1 UPTO SELF:GetResourceFileCount()
		SELF:GetResourceFile(n):oApp := SELF
	NEXT
RETURN

VIRTUAL METHOD RecreateGuids() AS VOID
	LOCAL n AS INT
	SELF:cGuid := Fun.NewGuid()
	FOR n:=1 UPTO SELF:GetFileCount()
		SELF:GetFile(n):cGuid := Fun.NewGuid()
	NEXT
	FOR n:=1 UPTO SELF:GetFileGroupCount()
		SELF:GetFileGroup(n):cGuid := Fun.NewGuid()
	NEXT
RETURN

VIRTUAL METHOD GetAbsolutePath(cPath AS STRING) AS STRING
RETURN Fun.GetAppFileName(SELF,cPath)

VIRTUAL METHOD ClrChanged() AS VOID
	LOCAL oRes AS ResourceFileClass
	LOCAL n AS INT
	FOR n := 1 UPTO SELF:GetResourceFileCount()
		oRes := SELF:GetResourceFile(n)
		IF .not. oRes:lNative .and. .not. oRes:lExternal
			oRes:lMustCreate := TRUE
		END IF
	NEXT
RETURN

VIRTUAL METHOD GetBaseFileOfRcVh(oRcVhFile AS FileClass) AS FileClass
	LOCAL oFile AS FileClass
	LOCAL n AS INT
	FOR n := 1 UPTO SELF:GetFileCount()
		oFile := SELF:GetFile(n)
		IF oFile:oRcPrg == oRcVhFile .or. oFile:oVhPrg == oRcVhFile
			RETURN oFile
		END IF
	NEXT
RETURN NULL

VIRTUAL METHOD ToString() AS STRING
//RETURN "AppClass: " + SELF:cName
RETURN SELF:cName

END CLASS




CLASS FileClass INHERIT GeneralProjectClass
EXPORT oApp AS AppClass
EXPORT oGroup AS FileGroupClass
EXPORT cFileName AS STRING
EXPORT lConfig AS LOGIC
EXPORT eFileDiskType AS FileDiskType
EXPORT lCopyToBin AS LOGIC

EXPORT dSavedTime , dSavedDesTime AS DateTime
EXPORT dRealSavedTime AS DateTime

EXPORT lDesPrg AS LOGIC
EXPORT oDesPrg AS FileClass
EXPORT oBasePrg AS FileClass

EXPORT oRcPrg , oVhPrg AS FileClass
EXPORT lRcVh AS LOGIC

EXPORT cGalleryName := "" AS STRING
EXPORT cGalleryPage := "" AS STRING
EXPORT cGalleryDescription := "" AS STRING
EXPORT cGalleryBitmap := "" AS STRING
EXPORT nGalleryIndex AS INT

EXPORT eLanguage AS ApplicationLanguage

CONSTRUCTOR(_oApp AS AppClass,_cFileName AS STRING)
SUPER()
SELF:cFileName:=_cFileName
SELF:eType:=ProjectType.File
SELF:oApp:=_oApp
SELF:cName:=Fun.GetFileName(_cFileName)
IF SELF:oApp == NULL
	SELF:eLanguage := ApplicationLanguage.None
ELSE
	SELF:eLanguage := SELF:oApp:eLanguage
END IF

SELF:dSavedTime := DateTime.MinValue
SELF:dSavedDesTime := DateTime.MinValue
SELF:dRealSavedTime := DateTime.MinValue

RETURN
CONSTRUCTOR(_oApp AS AppClass,_oGroup AS FileGroupClass,_cFileName AS STRING)
SUPER()
SELF:cFileName:=_cFileName
SELF:eType:=ProjectType.File
SELF:oApp:=_oApp
SELF:oGroup:=_oGroup
SELF:cName:=Fun.GetFileName(_cFileName)
IF SELF:oApp == NULL
	SELF:eLanguage := ApplicationLanguage.None
ELSE
	SELF:eLanguage := SELF:oApp:eLanguage
END IF

SELF:dSavedTime := DateTime.MinValue
SELF:dSavedDesTime := DateTime.MinValue
SELF:dRealSavedTime := DateTime.MinValue

RETURN
VIRTUAL ACCESS IsConfig AS LOGIC
RETURN SELF:lConfig
VIRTUAL METHOD GetProject() AS ProjectClass
	IF SELF:oApp == NULL //config file
		RETURN NULL
	ENDIF
RETURN SELF:oApp:oProject
VIRTUAL METHOD GetApp() AS AppClass
RETURN SELF:oApp
VIRTUAL METHOD GetFileGroup() AS FileGroupClass
RETURN SELF:oGroup
VIRTUAL METHOD GetFile() AS FileClass
RETURN SELF
VIRTUAL ACCESS FullFileName() AS STRING
RETURN Fun.GetAppFileName(SELF:GetApp() , SELF:cFileName)

VIRTUAL ACCESS DesignerPrgFileName AS STRING
RETURN Fun.PathAddSlash(Fun.GetDirectory(SELF:cFileName)) + Fun.GetFileNameNoExt(SELF:cFileName) + ".Designer.prg"

END CLASS



CLASS ApplicationGroupClass INHERIT GeneralProjectClass IMPLEMENTS IApplicationContainer //,IAppGroupContainer
	PROTECT aAppGroups AS List<ApplicationGroupClass>
	PROTECT aApps AS List<AppClass>
	EXPORT oProject AS ProjectClass
	CONSTRUCTOR(_oProject AS ProjectClass , _cName AS STRING)
		SUPER()
		SELF:cName := _cName
		SELF:eType := ProjectType.ApplicationGroup
		SELF:oProject := _oProject
		SELF:aApps := List<AppClass>{}
		SELF:aAppGroups := List<ApplicationGroupClass>{}
	RETURN
	VIRTUAL METHOD GetProject() AS ProjectClass
	RETURN SELF:oProject

	VIRTUAL METHOD GetAppGroupsCount() AS INT
	RETURN SELF:aAppGroups:Count
	VIRTUAL METHOD GetAppGroup(nGroup AS INT) AS ApplicationGroupClass
	RETURN SELF:aAppGroups[nGroup - 1]
	VIRTUAL METHOD RemoveAppGroup(oGroup AS ApplicationGroupClass) AS VOID
		IF SELF:aAppGroups:Contains(oGroup)
			SELF:aAppGroups:Remove(oGroup)
		END IF
	RETURN
	VIRTUAL METHOD AddAppGroup(oGroup AS ApplicationGroupClass) AS VOID
		IF .not. SELF:aAppGroups:Contains(oGroup)
			SELF:aAppGroups:Add(oGroup)
			oGroup:oParent := SELF
		END IF
	RETURN
	VIRTUAL METHOD ContainsAppGroup(oSearchGroup AS ApplicationGroupClass) AS LOGIC
		IF SELF:aAppGroups:Contains(oSearchGroup)
			RETURN TRUE
		END IF
		FOREACH oGroup AS ApplicationGroupClass IN SELF:aAppGroups
			IF oGroup:ContainsAppGroup(oSearchGroup)
				RETURN TRUE
			END IF
		NEXT
	RETURN FALSE

	VIRTUAL METHOD AddApp(oApp AS AppClass) AS VOID
		IF .not. SELF:aApps:Contains(oApp)
			SELF:aApps:Add(oApp)
			oApp:oParent := SELF
		ENDIF
	RETURN
	VIRTUAL METHOD RemoveApp(oApp AS AppClass) AS VOID
		IF SELF:aApps:Contains(oApp)
			SELF:aApps:Remove(oApp)
		ENDIF
	RETURN
	VIRTUAL METHOD GetAppCount() AS INT
	RETURN SELF:aApps:Count
	VIRTUAL METHOD GetApp(nApp AS INT) AS AppClass
	RETURN SELF:aApps[nApp - 1]

	VIRTUAL METHOD ContainsApp(oSearchApp AS AppClass) AS LOGIC
	IF SELF:aApps:Contains(oSearchApp)
		RETURN TRUE
	ENDIF
	FOREACH oGroup AS ApplicationGroupClass IN SELF:aAppGroups
		IF oGroup:ContainsApp(oSearchApp)
			RETURN TRUE
		END IF
	NEXT
	RETURN FALSE

END CLASS


CLASS FileGroupClass INHERIT GeneralProjectClass
PROTECT aFiles AS ArrayList
PROTECT aFileGroups AS ArrayList

EXPORT oApp AS AppClass
EXPORT cFileName AS STRING
EXPORT oGroup AS FileGroupClass
EXPORT lFolder AS LOGIC
CONSTRUCTOR(_oApp AS AppClass,_cName AS STRING)
SUPER()
SELF:cName:=_cName
SELF:eType:=ProjectType.FileGroup
SELF:oApp:=_oApp
SELF:aFiles:=ArrayList{}
SELF:aFileGroups:=ArrayList{}
RETURN
VIRTUAL METHOD GetProject() AS ProjectClass
RETURN SELF:oApp:oProject
VIRTUAL METHOD GetApp() AS AppClass
RETURN SELF:oApp
VIRTUAL METHOD GetFileGroup() AS FileGroupClass
RETURN SELF
VIRTUAL METHOD GetFile(nFile AS INT) AS FileClass
RETURN (FileClass)SELF:aFiles[nFile-1]
VIRTUAL METHOD GetFileCount() AS INT
RETURN SELF:aFiles:Count
VIRTUAL METHOD AddFile(oFile AS FileClass) AS VOID
	SELF:GetApp():AddFile(oFile)
	SELF:aFiles:Add(oFile)
	oFile:oGroup := SELF
RETURN
VIRTUAL METHOD RemoveFile(oFile AS FileClass) AS VOID
	IF SELF:aFiles:Contains(oFile)
		SELF:aFiles:Remove(oFile)
		oFile:oGroup:=NULL
	END IF
RETURN

VIRTUAL METHOD GetFileGroup(nGroup AS INT) AS FileGroupClass
RETURN (FileGroupClass)SELF:aFileGroups[nGroup-1]
VIRTUAL METHOD GetFileGroupCount() AS INT
RETURN SELF:aFileGroups:Count
VIRTUAL METHOD AddFileGroup(oGroup AS FileGroupClass) AS VOID
IF !SELF:aFileGroups:Contains(oGroup)
	SELF:aFileGroups:Add(oGroup)
END IF
oGroup:oGroup := SELF
RETURN
VIRTUAL METHOD RemoveFileGroup(nGroup AS INT) AS VOID
SELF:aFileGroups:RemoveAt(nGroup - 1)
oGroup:oGroup := NULL
RETURN
VIRTUAL METHOD RemoveFileGroup(oGroup AS FileGroupClass) AS VOID
IF SELF:aFileGroups:Contains(oGroup)
	SELF:aFileGroups:Remove(oGroup)
ENDIF
oGroup:oGroup := NULL
RETURN

VIRTUAL ACCESS BaseFolder() AS STRING
	LOCAL cFolder AS STRING
	IF SELF:oGroup != NULL
		cFolder := SELF:oGroup:FullFolder
	ELSE
		cFolder := SELF:oApp:FullPrgFolder
	ENDIF
RETURN cFolder
VIRTUAL ACCESS FullFolder() AS STRING
	IF .not. SELF:lFolder
		RETURN SELF:BaseFolder
	END IF
RETURN Fun.ConnectPaths(SELF:BaseFolder , SELF:cName)

END CLASS



CLASS ResourceClass INHERIT GeneralProjectClass
	EXPORT oResFile AS ResourceFileClass
	EXPORT cFileName AS STRING
	EXPORT eResType AS ResourceType
CONSTRUCTOR(_oResFile AS ResourceFileClass,_cFileName AS STRING)
	SUPER()
	SELF:eType := ProjectType.Resource
	SELF:cFileName := _cFileName
	SELF:cName := SELF:cFileName
	IF SELF:IsSWF
		SELF:cName := SubStr(SELF:cFileName , 22)
	ELSE
		SELF:cName := Fun.GetFileName(SELF:cFileName)
	ENDIF
	SELF:oResFile := _oResFile
	SELF:eResType := ResourceType.Bitmap
RETURN
CONSTRUCTOR(_oResFile AS ResourceFileClass,_cFileName AS STRING , _eResType AS ResourceType)
	SUPER()
	SELF:eType := ProjectType.Resource
	SELF:cFileName := _cFileName
	SELF:cName := SELF:cFileName
	IF SELF:IsSWF
		SELF:cName := SubStr(SELF:cFileName , 22)
	ELSE
		SELF:cName := Fun.GetFileName(SELF:cFileName)
	ENDIF
	SELF:oResFile := _oResFile
	SELF:eResType := _eResType
RETURN
VIRTUAL METHOD GetProject() AS ProjectClass
RETURN SELF:oResFile:GetProject()
VIRTUAL METHOD GetApp() AS AppClass
RETURN SELF:oResFile:GetApp()
VIRTUAL METHOD GetResourceFile() AS ResourceFileClass
RETURN SELF:oResFile
VIRTUAL METHOD GetResource() AS ResourceClass
RETURN SELF
VIRTUAL ACCESS IsSWF() AS LOGIC
RETURN Instr("SYSTEM.WINDOWS.FORMS" , SELF:cFileName:ToUpper())
VIRTUAL ACCESS FullFileName() AS STRING
	IF SELF:IsSWF
		RETURN ""
	ENDIF
RETURN Fun.GetAppFileName(SELF:GetApp() , SELF:cFileName)

END CLASS




CLASS ResourceFileClass INHERIT GeneralProjectClass
	PROTECT aResources AS ArrayList

	EXPORT oApp AS AppClass
	EXPORT cFileName AS STRING
	EXPORT lMustCreate AS LOGIC
	EXPORT lStandard AS LOGIC
	EXPORT lLinked AS LOGIC
	EXPORT lExternal AS LOGIC
	EXPORT lNative AS LOGIC
CONSTRUCTOR(_oApp AS AppClass)
	SUPER()
	SELF:cFileName := ""
	SELF:cName := ""
	SELF:eType := ProjectType.ResourceFile
	SELF:oApp := _oApp
	SELF:aResources := ArrayList{}
	SELF:lLinked := TRUE
RETURN
CONSTRUCTOR(_oApp AS AppClass,_cFileName AS STRING)
	SUPER()
	SELF:cFileName := _cFileName
	SELF:cName := Fun.GetFileName(SELF:cFileName)
	SELF:eType := ProjectType.ResourceFile
	SELF:oApp := _oApp
	SELF:aResources := ArrayList{}
//	SELF:cFileName := "%AppPath%\" + SELF:cName + ".resources"
	SELF:lLinked := TRUE
RETURN
VIRTUAL ACCESS FullFileName() AS STRING
RETURN Fun.GetAppFileName(SELF:GetApp() , SELF:cFileName)
VIRTUAL METHOD GetProject() AS ProjectClass
RETURN SELF:oApp:oProject
VIRTUAL METHOD GetApp() AS AppClass
RETURN SELF:oApp
VIRTUAL METHOD GetResourceFile() AS ResourceFileClass
RETURN SELF
VIRTUAL METHOD GetResource(nRes AS INT) AS ResourceClass
RETURN (ResourceClass)SELF:aResources[nRes-1]
VIRTUAL METHOD GetResourceCount() AS INT
RETURN SELF:aResources:Count
VIRTUAL METHOD GetResourceFromGuid(cGuid AS STRING) AS ResourceClass
	LOCAL oRes AS ResourceClass
	LOCAL n AS INT
	FOR n := 1 UPTO SELF:GetResourceCount()
		oRes := SELF:GetResource(n)
		IF oRes:cGuid == cGuid
			RETURN oRes
		ENDIF
	NEXT
RETURN NULL

VIRTUAL METHOD RemoveAllResources() AS VOID
	LOCAL oRes AS ResourceClass
	DO WHILE SELF:GetResourceCount() != 0
		oRes := SELF:GetResource(1)
		SELF:aResources:Remove(oRes)
		oRes:oResFile := NULL
	ENDDO
RETURN
VIRTUAL METHOD RemoveResource(oRes AS ResourceClass) AS VOID
	LOCAL n AS INT
	FOR n:=1 UPTO SELF:GetResourceCount()
		IF SELF:GetResource(n) == oRes
			SELF:aResources:Remove(oRes)
			oRes:oResFile := NULL
			EXIT
		END IF
	NEXT
RETURN
VIRTUAL METHOD ResourceExists(oTest AS ResourceClass) AS LOGIC
/*	LOCAL oRes AS ResourceClass
	LOCAL n AS INT
	FOR n := 1 UPTO SELF:GetResourceCount()
		oRes := SELF:GetResource(n)
//		IF oRes:cName == oTest:cName
//		IF oRes:cFileName == oTest:cFileName
		IF Fun.PathsEqual(oRes:cFileName , oTest:cFileName)
			RETURN TRUE
		ENDIF
	NEXT
RETURN FALSE*/
RETURN SELF:ResourceExists(oTest:cFileName)
VIRTUAL METHOD ResourceExists(cFileName AS STRING) AS LOGIC
	LOCAL oRes AS ResourceClass
	LOCAL n AS INT
	FOR n := 1 UPTO SELF:GetResourceCount()
		oRes := SELF:GetResource(n)
		IF Fun.PathsEqual(oRes:cFileName , cFileName)
			RETURN TRUE
		ENDIF
	NEXT
RETURN FALSE
VIRTUAL METHOD ResourceKeyExists(oTest AS ResourceClass) AS LOGIC
	LOCAL oRes AS ResourceClass
	LOCAL cKey , cTest AS STRING
	LOCAL n AS INT

	IF oTest:IsSWF
		cTest := oTest:cName
	ELSE
		cTest := Fun.GetFileName(oTest:cFileName)
	ENDIF
	cTest := cTest:ToUpper()

	FOR n := 1 UPTO SELF:GetResourceCount()
		oRes := SELF:GetResource(n)
		IF oRes:IsSWF
			cKey := oRes:cName
		ELSE
			cKey := Fun.GetFileName(oRes:cFileName)
		ENDIF
		IF cKey:ToUpper() == cTest
			RETURN TRUE
		ENDIF
	NEXT
RETURN FALSE

VIRTUAL METHOD AddResource(oRes AS ResourceClass) AS VOID
	IF SELF:ResourceExists(oRes)
		RETURN
	ENDIF
	SELF:aResources:Add(oRes)
	oRes:oResFile := SELF
RETURN

END CLASS

CLASS LicenseFileClass INHERIT GeneralProjectClass
	EXPORT oApp AS AppClass
	EXPORT cFileName AS STRING
	EXPORT lLinked AS LOGIC
CONSTRUCTOR(_oApp AS AppClass)
	SUPER()
	SELF:cFileName := ""
	SELF:cName := ""
	SELF:eType := ProjectType.LicenseFile
	SELF:oApp := _oApp
RETURN
CONSTRUCTOR(_oApp AS AppClass,_cFileName AS STRING)
	SUPER()
	SELF:cFileName := _cFileName
	SELF:cName := Fun.GetFileName(SELF:cFileName)
	SELF:eType := ProjectType.LicenseFile
	SELF:oApp := _oApp
	SELF:lLinked := TRUE
RETURN
VIRTUAL ACCESS FullFileName() AS STRING
RETURN Fun.GetAppFileName(SELF:GetApp() , SELF:cFileName)
VIRTUAL METHOD GetProject() AS ProjectClass
RETURN SELF:oApp:oProject
VIRTUAL METHOD GetApp() AS AppClass
RETURN SELF:oApp
VIRTUAL METHOD GetLicenseFile() AS LicenseFileClass
RETURN SELF

END CLASS




CLASS AppSubClass INHERIT GeneralProjectClass
PROTECT oApp AS AppClass
PROTECT oFile AS FileClass
PROTECT oItem AS OBJECT
CONSTRUCTOR(_oApp AS AppClass,_cName AS STRING,_oItem AS OBJECT,_eType AS ProjectType)
SUPER()
SELF:cName:=_cName
SELF:eType:=_eType
SELF:oApp:=_oApp
SELF:oItem := _oItem
RETURN
CONSTRUCTOR(_oFile AS FileClass,_cName AS STRING,_oItem AS OBJECT,_eType AS ProjectType)
SUPER()
SELF:cName:=_cName
SELF:eType:=_eType
SELF:oFile := _oFile
SELF:oApp := SELF:oFile:GetApp()
SELF:oItem := _oItem
RETURN
VIRTUAL METHOD GetProject() AS ProjectClass
RETURN SELF:oApp:oProject
VIRTUAL METHOD GetApp() AS AppClass
RETURN SELF:oApp
VIRTUAL METHOD GetReference() AS ReferenceObject
	IF SELF:eType != ProjectType.Reference
		RETURN NULL
	ENDIF
RETURN (ReferenceObject)SELF:oItem
VIRTUAL METHOD GetItem() AS OBJECT
RETURN SELF:oItem
VIRTUAL METHOD GetFile() AS FileClass
RETURN SELF:oFile

END CLASS






ENUM ReferenceType
	MEMBER GAC
	MEMBER Project
	MEMBER Ide
	MEMBER Browse
END ENUM

ENUM ClrType
	MEMBER v2
	MEMBER v4
	MEMBER Unknown
END ENUM

CLASS ReferenceObject
EXPORT eType AS ReferenceType
EXPORT cName AS STRING
EXPORT cLoadingName AS STRING
EXPORT cFileName AS STRING
EXPORT cGuid AS STRING
EXPORT eFrameworks AS Frameworks
EXPORT lCopy AS LOGIC
EXPORT cVersion AS STRING
EXPORT cCulture AS STRING
EXPORT cKeyToken AS STRING
EXPORT cArchitecture AS STRING
EXPORT eClr AS ClrType
CONSTRUCTOR(_cName AS STRING,_eType AS ReferenceType)
SELF:cName:=_cName
SELF:eType:=_eType
SELF:eClr := ClrType.v4
SELF:_Init()
RETURN
CONSTRUCTOR(_cName AS STRING,_eType AS ReferenceType , _eClr AS ClrType)
SELF:cName:=_cName
SELF:eType:=_eType
SELF:eClr := _eClr
SELF:_Init()
RETURN
INTERNAL METHOD _Init() AS VOID
SELF:cLoadingName:=""
SELF:eFrameworks := Frameworks.Full
SELF:cVersion := ""
SELF:cCulture := ""
SELF:cKeyToken := ""
SELF:cArchitecture := ""
RETURN
VIRTUAL METHOD ToString() AS STRING
	LOCAL cRet AS STRING
	cRet := SELF:cName
	IF SELF:eType == ReferenceType.GAC
//		RETURN SELF:cName + "," + SELF:cVersion //+ "," + SELF:cArchitecture
//		RETURN SELF:cName + "   (" + SELF:cVersion + ")" //+ "," + SELF:cArchitecture
		IF SELF:eClr != ClrType.Unknown
			cRet += "   " + iif(SELF:eClr == ClrType.v2 , "(v2.0)" , "(v4.0)")
		END IF
		cRet += "   (" + SELF:cVersion + ")" //+ "," + SELF:cArchitecture
	ENDIF
RETURN cRet

VIRTUAL METHOD Clone() AS ReferenceObject
	LOCAL oRef AS ReferenceObject
	oRef := ReferenceObject{SELF:cName , SELF:eType}
	oRef:cName := cName
	oRef:cLoadingName := SELF:cLoadingName
	oRef:cFileName := SELF:cFileName
	oRef:cGuid := SELF:cGuid
	oRef:eFrameworks := SELF:eFrameworks
	oRef:lCopy := SELF:lCopy
	oRef:cVersion := SELF:cVersion
	oRef:cCulture := SELF:cCulture
	oRef:cKeyToken := SELF:cKeyToken
	oRef:cArchitecture := SELF:cArchitecture
	oRef:eClr := SELF:eClr
RETURN oRef

END CLASS

CLASS ConfigOptions
	EXPORT cSwitches AS STRING
	EXPORT cSwitchesCF AS STRING
	EXPORT cCommandLine AS STRING
	EXPORT cCommandLineCF AS STRING
	EXPORT lWarningsErrors AS LOGIC
	EXPORT lDebug AS LOGIC
	EXPORT lDebugInit AS LOGIC
	EXPORT lDefineTrace AS LOGIC
	EXPORT lDefineDebug AS LOGIC
	EXPORT lSyntaxOnly AS LOGIC
	EXPORT lForceConsole AS LOGIC
	EXPORT lForceX86 AS LOGIC
	EXPORT lOptimize AS LOGIC
	CONSTRUCTOR()
		SELF:Reset()
	RETURN
	METHOD Reset() AS VOID
		SELF:cSwitches := ""
//		SELF:cSwitchesCF := "/nostdlib"
		SELF:cSwitchesCF := "" // todo exei problhma o compiler
		SELF:cCommandLine := ""
		SELF:cCommandLineCF := ""
		SELF:lDebug := FALSE
		SELF:lDebugInit := FALSE
		SELF:lDefineDebug := FALSE
		SELF:lDefineTrace := FALSE
		SELF:lWarningsErrors := FALSE
		SELF:lSyntaxOnly := FALSE
		SELF:lForceConsole := FALSE
		SELF:lForceX86 := FALSE
		SELF:lOptimize := FALSE
	RETURN
END CLASS


CLASS ProjectConfigurations
	PROTECT aConfigs AS ArrayList
	CONSTRUCTOR()
		SELF:aConfigs := ArrayList{}
	RETURN
	METHOD SetCompiled(cConfigGuid AS STRING) AS VOID
		LOCAL oConfig AS ProjectConfigClass
		IF cConfigGuid == NULL .or. cConfigGuid == ""
			RETURN
		END IF
		oConfig := SELF:Get(cConfigGuid)
		IF oConfig != NULL
			oConfig:lCompiled := TRUE
		END IF
	RETURN
	METHOD IsCompiled(cConfigGuid AS STRING) AS LOGIC
		LOCAL oConfig AS ProjectConfigClass
		IF cConfigGuid == NULL .or. cConfigGuid == ""
			RETURN FALSE
		END IF
		oConfig := SELF:Get(cConfigGuid)
		IF oConfig != NULL
			RETURN oConfig:lCompiled
		END IF
	RETURN FALSE
	METHOD ResetCompiled() AS VOID
		LOCAL n AS INT
		FOR n := 1 UPTO SELF:Count
			SELF:Get(n):lCompiled := FALSE
		NEXT
	RETURN
	METHOD Add(oConfig AS ProjectConfigClass) AS ProjectConfigClass
		LOCAL nConfig AS INT
		FOR nConfig := 0 UPTO SELF:aConfigs:Count - 1
			IF ((ProjectConfigClass)SELF:aConfigs[nConfig]):cGuid == oConfig:cGuid
				RETURN NULL
			ENDIF
		NEXT
		SELF:aConfigs:Add(oConfig)
	RETURN oConfig
	METHOD AddDebugRelease() AS VOID
		LOCAL oConfig AS ProjectConfigClass
		IF SELF:Count == 0 .or. !SELF:Get(1):IsDebug
			oConfig := ProjectConfigClass{"Debug"}
			oConfig:cGuid := "11111111-1111-1111-1111-111111111111"
			oConfig:cSubFolder := "\Debug\"
			SELF:aConfigs:Insert(0 , oConfig)
			oConfig := ProjectConfigClass{"Release"}
			oConfig:cGuid := "22222222-2222-2222-2222-222222222222"
			oConfig:cSubFolder := "\Release\"
			SELF:aConfigs:Insert(1 , oConfig)
		ENDIF
	RETURN
	METHOD Remove(oConfig AS ProjectConfigClass) AS VOID
		LOCAL nConfig AS INT
		FOR nConfig := 0 UPTO SELF:aConfigs:Count - 1
			IF ((ProjectConfigClass)SELF:aConfigs[nConfig]):cGuid == oConfig:cGuid
				SELF:aConfigs:RemoveAt(nConfig)
				RETURN
			ENDIF
		NEXT
	RETURN
	METHOD Remove(cGuid AS STRING) AS VOID
		LOCAL nConfig AS INT
		FOR nConfig := 0 UPTO SELF:aConfigs:Count - 1
			IF ((ProjectConfigClass)SELF:aConfigs[nConfig]):cGuid == cGuid
				SELF:aConfigs:RemoveAt(nConfig)
				RETURN
			ENDIF
		NEXT
	RETURN
	METHOD Remove(nConfig AS INT) AS VOID
		IF nConfig != 0 .and. nConfig <= SELF:aConfigs:Count
			SELF:aConfigs:RemoveAt(nConfig - 1)
		ENDIF
	RETURN

	METHOD GetDebug() AS ProjectConfigClass
	RETURN (ProjectConfigClass)SELF:aConfigs[0]
	METHOD GetRelease() AS ProjectConfigClass
	RETURN (ProjectConfigClass)SELF:aConfigs[1]
	METHOD Get(nConfig AS INT) AS ProjectConfigClass
		IF SELF:aConfigs:Count == 0 .or. SELF:aConfigs:Count < nConfig
			RETURN NULL
		ENDIF
	RETURN (ProjectConfigClass)SELF:aConfigs[nConfig - 1]
	METHOD Get(cGuid AS STRING) AS ProjectConfigClass
		LOCAL n AS INT
		FOR n := 1 UPTO SELF:Count
			IF SELF:Get(n):cGuid == cGuid
				RETURN SELF:Get(n)
			ENDIF
		NEXT
	RETURN NULL
	METHOD GetFromName(cName AS STRING) AS ProjectConfigClass
		LOCAL n AS INT
		cName := cName:ToUpper()
		FOR n:=1 UPTO SELF:Count
			IF SELF:Get(n):cName:ToUpper() == cName
				RETURN SELF:Get(n)
			ENDIF
		NEXT
	RETURN NULL
	ACCESS Count AS INT
	RETURN SELF:aConfigs:Count
END CLASS





CLASS ProjectConfigClass
	EXPORT cName AS STRING  // Project Only
	EXPORT cGuid AS STRING
	EXPORT cSubFolder AS STRING // Project Only
	EXPORT oOptions AS ConfigOptions  // App Only
	EXPORT lCompiled AS LOGIC
	CONSTRUCTOR(_cName AS STRING)
		SELF:cName := _cName
		SELF:oOptions := ConfigOptions{}
		SELF:cGuid := Fun.NewGuid()
	RETURN
	CONSTRUCTOR(_cName AS STRING,_cGuid AS STRING)
		SELF:cName := _cName
		SELF:oOptions := ConfigOptions{}
		SELF:cGuid := _cGuid
	RETURN
	ACCESS IsDebug AS LOGIC
	RETURN SELF:cGuid == "11111111-1111-1111-1111-111111111111"
	ACCESS IsRelease AS LOGIC
	RETURN SELF:cGuid == "22222222-2222-2222-2222-222222222222"
END CLASS



CLASS Options // reto update CloneOptions()
	EXPORT cSwitches AS STRING
	EXPORT lCaseSensitive AS LOGIC
	EXPORT lZeroArrays AS LOGIC
	EXPORT lResponseOnly AS LOGIC
	EXPORT lIgnoreVulcanRsp AS LOGIC
	EXPORT lIgnoreStdDefs AS LOGIC
	EXPORT lLateBound AS LOGIC
	EXPORT lUnsafe AS LOGIC
	EXPORT lImplicitNamespace AS LOGIC
	EXPORT lUndeclared AS LOGIC
	EXPORT lMemVar AS LOGIC
	EXPORT lUseNativeVersion AS LOGIC

	EXPORT lOvf,lFOvf AS LOGIC

	EXPORT lVO1 AS LOGIC
	EXPORT lVO2 AS LOGIC
	EXPORT lVO3 AS LOGIC
	EXPORT lVO4 AS LOGIC
	EXPORT lVO5 AS LOGIC
	EXPORT lVO6 AS LOGIC
	EXPORT lVO7 AS LOGIC
	EXPORT lVO8 AS LOGIC
	EXPORT lVO9 AS LOGIC
	EXPORT lVO10 AS LOGIC
	EXPORT lVO11 AS LOGIC
	EXPORT lVO12 AS LOGIC
	EXPORT lVO13 AS LOGIC
	EXPORT lVO14 AS LOGIC
	EXPORT lVO15 AS LOGIC
	EXPORT lVO16 AS LOGIC

	EXPORT lFOX1 AS LOGIC
	EXPORT lFOX2 AS LOGIC

	EXPORT lXPP1 AS LOGIC
//	EXPORT lIntegerDivisions AS LOGIC

	EXPORT oEmptyConfig AS ProjectConfigClass
	CONSTRUCTOR()
		SELF:oEmptyConfig := ProjectConfigClass{"" , ""}
		SELF:Reset()
	RETURN
	METHOD Reset() AS VOID
		SELF:lCaseSensitive := FALSE
		SELF:lZeroArrays := FALSE
		SELF:lLateBound := FALSE
		SELF:lResponseOnly := FALSE
		SELF:cSwitches := ""
		SELF:lResponseOnly := FALSE
		SELF:lIgnoreVulcanRsp := FALSE
		SELF:lIgnoreStdDefs := FALSE
		SELF:lLateBound := FALSE
		SELF:lUnsafe := FALSE
		SELF:lUndeclared := FALSE
		SELF:lMemVar := FALSE
		SELF:lImplicitNamespace := FALSE
		SELF:lUseNativeVersion := FALSE

		SELF:lOvf := FALSE
		SELF:lFOvf := FALSE

		SELF:lVO1 := FALSE
		SELF:lVO2 := FALSE
		SELF:lVO3 := FALSE
		SELF:lVO4 := FALSE
		SELF:lVO5 := FALSE
		SELF:lVO6 := FALSE
		SELF:lVO7 := FALSE
		SELF:lVO8 := FALSE
		SELF:lVO9 := FALSE
		SELF:lVO10 := FALSE
		SELF:lVO11 := FALSE
		SELF:lVO12 := FALSE
		SELF:lVO13 := FALSE
		SELF:lVO14 := FALSE
		SELF:lVO15 := FALSE
		SELF:lVO16 := FALSE

		SELF:lFOX1 := FALSE
		SELF:lFOX2 := FALSE
		SELF:lXPP1 := FALSE

//		SELF:lIntegerDivisions := FALSE

		SELF:oEmptyConfig:oOptions:lDebug := TRUE
		SELF:oEmptyConfig:oOptions:lDebugInit := FALSE
		SELF:oEmptyConfig:oOptions:lWarningsErrors := FALSE
		SELF:oEmptyConfig:oOptions:lForceConsole := FALSE
		SELF:oEmptyConfig:oOptions:lForceX86 := FALSE
		SELF:oEmptyConfig:oOptions:lOptimize := FALSE
	RETURN
END CLASS

ENUM FileType
	MEMBER VulcanNet
	MEMBER CSharp
	MEMBER XSharp
	MEMBER VBNet
	MEMBER Harbour
	MEMBER TextFile
	MEMBER VO
	MEMBER None
END ENUM

ENUM ApplicationLanguage
	MEMBER VulcanNet
	MEMBER CSharp
	MEMBER XSharp
	MEMBER VBNet
	MEMBER Harbour
	MEMBER None
END ENUM

ENUM ApplicationDialect
	MEMBER None
	MEMBER Core
	MEMBER VO
	MEMBER Vulcan
	MEMBER Harbour
	MEMBER XBasePP
	MEMBER FoxPro
END ENUM

ENUM Platform
	MEMBER AnyCPU
	MEMBER x86
	MEMBER x64
END ENUM

[flags];
ENUM Frameworks
	MEMBER Full := 1
	MEMBER CF := 2
	MEMBER Both := 3
END ENUM

CLASS Glo
	STATIC EXPORT gcImportBreak := "%#IDE#%" AS STRING
	STATIC EXPORT gcDotNetInstallDrive := "C" AS STRING
END CLASS

CLASS Fun
	STATIC METHOD CRLFToLine(cString AS STRING) AS STRING
		cString:=StrTran(cString,ChrW(13)+ChrW(10)," ")
		cString:=StrTran(cString,ChrW(10)+ChrW(13)," ")
		cString:=StrTran(cString,ChrW(13)," ")
		cString:=StrTran(cString,ChrW(10)," ")
	RETURN cString

	STATIC METHOD CRLFToTagLine(cString AS STRING) AS STRING
		cString:=StrTran(cString,ChrW(13)+ChrW(10),"`")
		cString:=StrTran(cString,ChrW(10)+ChrW(13),"`")
		cString:=StrTran(cString,ChrW(13),"`")
		cString:=StrTran(cString,ChrW(10),"`")
	RETURN cString

	STATIC METHOD TagLineToCRLF(cString AS STRING) AS STRING
		cString:=StrTran(cString,"`",ChrW(13)+ChrW(10))
	RETURN cString

	STATIC METHOD LoadOption(cLine AS STRING,oOptions AS Options) AS LOGIC
		LOCAL sLine AS FileLine
		LOCAL lNoOption AS LOGIC
		TRY
			sLine:=Fun.AnalyzeFileLine(cLine)
//			sLine:=sLine
			sLine:cParam := sLine:cParam:TrimStart(NULL)
			IF !(sLine:cParam=="")
				DO CASE
				CASE sLine:cParam=="SWITCHES"
					oOptions:cSwitches:=Fun.TagLineToCRLF(sLine:cValue)
/*				Case sLine:cParam=="TARGET"
					oOptions:nTarget:=sLine:nValue
					If (oOptions:nTarget<0) .or. (oOptions:nTarget>3)
						oOptions:nTarget:=0
					End If*/
				CASE sLine:cParam=="ZEROARRAYS"
					oOptions:lZeroArrays:=sLine:lValue
				CASE sLine:cParam=="IMPLICITNAMESPACE"
					oOptions:lImplicitNamespace:=sLine:lValue
				CASE sLine:cParam=="CASESENSITIVE"
					oOptions:lCaseSensitive:=sLine:lValue
				CASE sLine:cParam=="RESPONSEONLY"
					oOptions:lResponseOnly:=sLine:lValue
				CASE sLine:cParam=="IGNOREVULCANRSP"
					oOptions:lIgnoreVulcanRsp:=sLine:lValue
				CASE sLine:cParam=="IGNORESTDDEFS"
					oOptions:lIgnoreStdDefs:=sLine:lValue
				CASE sLine:cParam=="LATEBOUND"
					oOptions:lLateBound:=sLine:lValue
				CASE sLine:cParam=="UNSAFE"
					oOptions:lUnsafe:=sLine:lValue
				CASE sLine:cParam=="UNDECLARED"
					oOptions:lUndeclared:=sLine:lValue
				CASE sLine:cParam=="USENATIVEVERSION"
					oOptions:lUseNativeVersion:=sLine:lValue
				CASE sLine:cParam=="MEMVAR"
					oOptions:lMemVar:=sLine:lValue
				CASE sLine:cParam=="OVF"
					oOptions:lOvf:=sLine:lValue
				CASE sLine:cParam=="FOVF"
					oOptions:lFOvf:=sLine:lValue
//				CASE sLine:cParam=="INTEGERDIVISIONS"
//					oOptions:lIntegerDivisions := sLine:lValue

//				CASE sLine:cParam=="VO"
//					oOptions:lVO:=sLine:lValue

				CASE sLine:cParam=="VO1"
					oOptions:lVO1:=sLine:lValue
				CASE sLine:cParam=="VO2"
					oOptions:lVO2:=sLine:lValue
				CASE sLine:cParam=="VO3"
					oOptions:lVO3:=sLine:lValue
				CASE sLine:cParam=="VO4"
					oOptions:lVO4:=sLine:lValue
				CASE sLine:cParam=="VO5"
					oOptions:lVO5:=sLine:lValue
				CASE sLine:cParam=="VO6"
					oOptions:lVO6:=sLine:lValue
				CASE sLine:cParam=="VO7"
					oOptions:lVO7:=sLine:lValue
				CASE sLine:cParam=="VO8"
					oOptions:lVO8:=sLine:lValue
				CASE sLine:cParam=="VO9"
					oOptions:lVO9:=sLine:lValue
				CASE sLine:cParam=="VO10"
					oOptions:lVO10:=sLine:lValue
				CASE sLine:cParam=="VO11"
					oOptions:lVO11:=sLine:lValue
				CASE sLine:cParam=="VO12"
					oOptions:lVO12:=sLine:lValue
				CASE sLine:cParam=="VO13"
					oOptions:lVO13:=sLine:lValue
				CASE sLine:cParam=="VO14"
					oOptions:lVO14:=sLine:lValue
/*				CASE sLine:cParam=="VO15"
					oOptions:lVO15:=sLine:lValue*/
				CASE sLine:cParam=="VO16"
					oOptions:lVO16:=sLine:lValue

				CASE sLine:cParam=="FOX1"
					oOptions:lFOX1:=sLine:lValue
				CASE sLine:cParam=="FOX2"
					oOptions:lFOX2:=sLine:lValue
				CASE sLine:cParam=="XPP1"
					oOptions:lXPP1:=sLine:lValue

				OTHERWISE
					lNoOption:=TRUE
				END CASE

/*				// Default /vo14 to true, when other /vo options are enabled
				IF sLine:cParam:StartsWith("VO") .and. "123567890":IndexOf(sLine:cParam[sLine:cParam:Length - 1]) != 0 .and. sLine:lValue
					oOptions:lVO14 := TRUE
				END IF*/

			END IF
		END TRY
	RETURN !lNoOption

	STATIC METHOD LoadConfigOption(cLine AS STRING,oOptions AS ConfigOptions) AS LOGIC
		LOCAL sLine AS FileLine
		LOCAL lNoOption AS LOGIC
		TRY
			sLine:=Fun.AnalyzeFileLine(cLine)
//			sLine:=sLine
			sLine:cParam := sLine:cParam:TrimStart(NULL)
			IF !(sLine:cParam=="")
				DO CASE
				CASE sLine:cParam=="SWITCHES"
					oOptions:cSwitches:=Fun.TagLineToCRLF(sLine:cValue)
				CASE sLine:cParam=="SWITCHESCF"
					oOptions:cSwitchesCF:=Fun.TagLineToCRLF(sLine:cValue)
				CASE sLine:cParam=="COMMANDLINE"
					oOptions:cCommandLine:=sLine:cValue
				CASE sLine:cParam=="COMMANDLINECF"
					oOptions:cCommandLineCF:=sLine:cValue
/*				Case sLine:cParam=="TARGET"
					oOptions:nTarget:=sLine:nValue
					If (oOptions:nTarget<0) .or. (oOptions:nTarget>3)
						oOptions:nTarget:=0
					End If*/
				CASE sLine:cParam=="DEBUG"
					oOptions:lDebug:=sLine:lValue
					// giati mexri ver 0.98 den yphrxe ksexwristo definedebug (htan mazi me to debug)
					// an einai diaforetiko, tha ksanadiabastei parakatw
					oOptions:lDefineDebug:=sLine:lValue
				CASE sLine:cParam=="DEBUGINIT"
					oOptions:lDebugInit:=sLine:lValue
				CASE sLine:cParam=="DEFINEDEBUG"
					oOptions:lDefineDebug:=sLine:lValue
				CASE sLine:cParam=="DEFINETRACE"
					oOptions:lDefineTrace:=sLine:lValue
				CASE sLine:cParam=="WARNINGSERRORS"
					oOptions:lWarningsErrors:=sLine:lValue
				CASE sLine:cParam=="FORCECONSOLE"
					oOptions:lForceConsole:=sLine:lValue
				CASE sLine:cParam=="FORCEX86"
					oOptions:lForceX86:=sLine:lValue
				CASE sLine:cParam=="OPTIMIZE"
					oOptions:lOptimize:=sLine:lValue
				CASE sLine:cParam=="SYNTAXONLY"
					oOptions:lSyntaxOnly:=sLine:lValue
				OTHERWISE
					lNoOption:=TRUE
				END CASE
			END IF
		END TRY
	RETURN !lNoOption

	STATIC METHOD NewGuid() AS STRING
	RETURN System.Guid.NewGuid():ToString():ToUpper()

	STATIC METHOD GetProjectFileName(oProject AS ProjectClass,cFileName AS STRING) AS STRING
	LOCAL cRet AS STRING
	IF oProject == NULL
		RETURN cFileName
	END IF
	IF Left(Upper(cFileName),14)=="%PROJECTPATH%\"
//		cRet:=oProject:cFolder+SubStr(cFileName,15)
		cRet := Fun.ConnectPathAndFileName(oProject:cFolder , SubStr(cFileName,15))
	ELSE
		cRet := cFileName
	END IF
	RETURN cRet
	STATIC METHOD GetAppFileName(oApp AS AppClass,cFileName AS STRING) AS STRING
	LOCAL cRet AS STRING
	IF oApp == NULL
		RETURN cFileName
	END IF
	IF Left(Upper(cFileName),10)=="%APPPATH%\"
//		cRet:=oApp:cFolder+SubStr(cFileName,11)
		cRet := Fun.ConnectPathAndFileName(oApp:cFolder , SubStr(cFileName,11))
		cRet := Fun.GetProjectFileName(oApp:GetProject() , cRet)
	ELSEIF Left(Upper(cFileName),14)=="%PROJECTPATH%\"
		IF oApp:GetProject() == NULL
			cRet := cFileName
		ELSE
//			cRet:=oApp:GetProject():cFolder+SubStr(cFileName,15)
			cRet := Fun.ConnectPathAndFileName(oApp:GetProject():cFolder , SubStr(cFileName,15))
		ENDIF
	ELSE
		cRet := cFileName
	END IF
	RETURN cRet

	STATIC METHOD GetProjectFileNameNew(oProject AS ProjectClass,cFileName AS STRING) AS STRING
//	 kai xwris \ sto telos
	LOCAL cRet AS STRING
	IF oProject == NULL
		RETURN cFileName
	END IF
	IF Left(Upper(cFileName),13)=="%PROJECTPATH%"
		cRet := Fun.PathNoSlash(oProject:cFolder) + SubStr(cFileName,14)
	ELSE
		cRet := cFileName
	END IF
	RETURN cRet
	STATIC METHOD GetAppFileNameNew(oApp AS AppClass,cFileName AS STRING) AS STRING
//	 kai xwris \ sto telos
	LOCAL cRet AS STRING
	IF oApp == NULL
		RETURN cFileName
	END IF
	IF Left(Upper(cFileName),9)=="%APPPATH%"
		cRet := Fun.PathNoSlash(oApp:cFolder) + SubStr(cFileName,10)
		cRet := Fun.GetProjectFileNameNew(oApp:GetProject() , cRet)
	ELSEIF Left(Upper(cFileName),13)=="%PROJECTPATH%"
		IF oApp:GetProject() == NULL
			cRet := cFileName
		ELSE
			cRet := Fun.PathNoSlash(oApp:GetProject():cFolder) + SubStr(cFileName,14)
		ENDIF
	ELSE
		cRet := cFileName
	END IF
	RETURN cRet

	STATIC METHOD GetAppFileNameNewER(oApp AS AppClass,cFileName AS STRING) AS STRING
		LOCAL cRet AS STRING
		IF oApp == NULL
			RETURN cFileName
		END IF

		LOCAL aValues AS NameValueCollection
		aValues := NameValueCollection{}
		aValues:Add("%APPPATH%" , iif(oApp == NULL , "%ApPath%" , Fun.PathNoSlash(oApp:cFolder) ) )
		aValues:Add("%PROJECTPATH%" , iif(oApp:GetProject() == NULL , "%ProjectPath%" , Fun.PathNoSlash(oApp:GetProject():cFolder) ) )
		cRet := Fun.TranslateLine(cFileName , aValues)
	RETURN cRet

	STATIC METHOD TranslateLine(cLine AS STRING , aValues AS NameValueCollection) AS STRING
		LOCAL n , nAt AS INT
		LOCAL cUpper AS STRING
		LOCAL cRet AS STRING
		LOCAL nLength AS INT
		LOCAL cValue AS STRING
		LOCAL oNameValue AS NameValue
		LOCAL lDidAction AS LOGIC

/*		IF !cLine:Contains("%")
			RETURN cLine
		END IF*/

		cUpper := cLine:ToUpper()
		cRet := cLine
		REPEAT
			lDidAction := FALSE
			FOR n := 0 UPTO aValues:Count - 1
				oNameValue := aValues:Get(n)
//				nAt := cUpper:IndexOf("%" + oNameValue:Name:ToUpper() + "%")
				nAt := cUpper:IndexOf(oNameValue:Name:ToUpper())
				IF nAt != - 1
//					nLength := oNameValue:Name:Length + 2
					nLength := oNameValue:Name:Length
				END IF
				IF nAt != -1
					cRet := cLine:Substring(0 , nAt)
					cValue := (STRING)oNameValue:Value
					cRet += cValue
					IF nAt + nLength < cLine:Length
						cRet += cLine:Substring(nAt + nLength)
					END IF
					cLine := cRet
					cUpper := cLine:ToUpper()
					lDidAction := TRUE
				END IF
			NEXT
		UNTIL ! lDidAction // in case there are multiple occurences of the same tag

	RETURN cRet

	STATIC METHOD ConnectPaths(cPath1 AS STRING,cPath2 AS STRING) AS STRING
		IF !Right(cPath1,1) == "\"
			cPath1 += "\"
		ENDIF
		IF cPath2 == "" .or. cPath2 == "\"
			RETURN cPath1
		ENDIF
		IF Left(cPath2,1) == "\"
			cPath2 := SubStr(cPath2 , 2)
		ENDIF
		IF !Right(cPath2,1) == "\"
			cPath2 += "\"
		ENDIF
	RETURN cPath1 + cPath2
	STATIC METHOD ConnectPathsNoSlash(cPath1 AS STRING,cPath2 AS STRING) AS STRING
		IF !Right(cPath1,1) == "\"
			cPath1 += "\"
		ENDIF
		IF cPath2 == "" .or. cPath2 == "\"
			RETURN cPath1
		ENDIF
		IF Left(cPath2,1) == "\"
			cPath2 := SubStr(cPath2 , 2)
		ENDIF
	RETURN Fun.PathNoSlash(cPath1 + cPath2)
	STATIC METHOD ConnectPathAndFileName(cPath AS STRING,cFileName AS STRING) AS STRING
		IF !Right(cPath,1) == "\"
			cPath += "\"
		ENDIF
		IF Left(cFileName,1) == "\"
			cFileName := SubStr(cFileName , 2)
		ENDIF
	RETURN cPath + cFileName
	STATIC METHOD GetDirectory(cFile AS STRING) AS STRING
	LOCAL sDrive:="", sDir:="", sFile:="", sExt:=""  AS STRING
	IF cFile:Trim():Length == 0
		RETURN ""
	END IF
	IF Left(cFile,1) == "%" // %cffolder% etc
		LOCAL nAt AS INT
		nAt := (INT)RAt("\" , cFile)
		IF nAt != 0 .and. nAt < SLen(cFile)
			cFile := Left(cFile , nAt)
		ENDIF
		RETURN cFile
	ENDIF
	SplitPath( cFile, REF sDrive, REF sDir, REF sFile, REF sExt)
	RETURN sDrive+sDir
	STATIC METHOD GetDrive(cFile AS STRING) AS STRING
	LOCAL sDrive:="", sDir:="", sFile:="", sExt:=""  AS STRING
	IF cFile:Trim():Length == 0
		RETURN ""
	END IF
	SplitPath( cFile, REF sDrive, REF sDir, REF sFile, REF sExt)
	RETURN sDrive
	STATIC METHOD GetFileName(cFile AS STRING) AS STRING
	LOCAL sDrive:="", sDir:="", sFile:="", sExt:=""  AS STRING
	IF cFile:Trim():Length == 0
		RETURN ""
	END IF
	SplitPath( cFile, REF sDrive, REF sDir, REF sFile, REF sExt)
	RETURN sFile+sExt
	STATIC METHOD GetFileNameNoExt(cFile AS STRING) AS STRING
	LOCAL sDrive:="", sDir:="", sFile:="", sExt:=""  AS STRING
	IF cFile:Trim():Length == 0
		RETURN "Unnamed"
	END IF
	SplitPath( cFile, REF sDrive, REF sDir, REF sFile, REF sExt)
	RETURN sFile
	STATIC METHOD GetExt(cFile AS STRING) AS STRING
	LOCAL sDrive:="", sDir:="", sFile:="", sExt:=""  AS STRING
	IF cFile:Trim():Length == 0
		RETURN "Unnamed"
	END IF
	SplitPath( cFile, REF sDrive, REF sDir, REF sFile, REF sExt)
	IF Left(sExt , 1) == "."
		sExt :=SubStr(sExt , 2)
	ENDIF
	RETURN sExt
	STATIC METHOD GetFullFileNameNoExt(cFile AS STRING) AS STRING
	LOCAL sDrive:="", sDir:="", sFile:="", sExt:=""  AS STRING
	IF cFile:Trim():Length == 0
		RETURN "Unnamed"
	END IF
	SplitPath( cFile, REF sDrive, REF sDir, REF sFile, REF sExt)
	RETURN sDrive+sDir+sFile

	STATIC METHOD AnalyzeFileLine(cLine AS STRING) AS FileLine
	LOCAL strFileLine AS FileLine
	LOCAL nAt AS INT
	strFileLine:=FileLine{}
	strFileLine:cParam:=""
	IF SLen(cLine)>2 .and. !(Left(cLine,1)=="\") .and. !(Left(cLine,1)==";")
		nAt:=(INT)At(" = ",cLine)
		IF nAt!=0
			strFileLine:cParam := Left(cLine,nAt-1):ToUpper():Trim()
			strFileLine:cValue:=SubStr(cLine,nAt+3):Trim()
			TRY
			strFileLine:nValue:=INT(Val(strFileLine:cValue))
			strFileLine:lValue:=strFileLine:nValue==1
			END TRY
		ELSE
			nAt:=(INT)At("=",cLine)
			IF nAt!=0
				strFileLine:cParam:= Left(cLine,nAt-1):ToUpper():Trim()
				strFileLine:cValue:=SubStr(cLine,nAt+1):Trim()
				TRY
				strFileLine:nValue:=INT(Val(strFileLine:cValue))
				strFileLine:lValue:=strFileLine:nValue==1
				END TRY
			END IF
		END IF
	END IF
	RETURN strFileLine
	STATIC METHOD PathsEqual(cPath1 AS STRING , cPath2 AS STRING) AS LOGIC
	cPath1 := Fun.PathAddSlash(cPath1)
	cPath1 := StrTran(cPath1 , "\\" , "\")
	cPath2 := Fun.PathAddSlash(cPath2)
	cPath2 := StrTran(cPath2 , "\\" , "\")
	RETURN cPath1:ToUpper() == cPath2:ToUpper()
	STATIC METHOD PathAddSlash(cPath AS STRING) AS STRING
		cPath := cPath:Trim()
		IF !Right(cPath , 1) == "\"
			cPath += "\"
		ENDIF
	RETURN cPath
	STATIC METHOD PathNoSlash(cPath AS STRING) AS STRING
		cPath := cPath:Trim()
		DO WHILE Right(cPath , 1) == "\"
			cPath := Left(cPath , cPath:Length - 1)
		ENDDO
	RETURN cPath
	STATIC METHOD PathNoSlashBoth(cPath AS STRING) AS STRING
		cPath := cPath:Trim()
		DO WHILE Right(cPath , 1) == "\"
			cPath := Left(cPath , cPath:Length - 1)
		ENDDO
		DO WHILE Left(cPath , 1) == "\"
			cPath := SubStr(cPath , 2)
		ENDDO
	RETURN cPath

	STATIC METHOD ReplaceText(cText REF STRING , cSearch AS STRING , cReplace AS STRING) AS STRING
		LOCAL nAt AS INT
		// TODO Dirtiest Hack Ever (the FOR loop)
		// But we don't want to use a DO WHILE <textexists>, as it may result to an
		// infinite loop, if the replace text for some reason contains the search string
		FOR LOCAL n := 1 AS INT UPTO 100
			nAt := cText:ToUpperInvariant():IndexOf(cSearch:ToUpperInvariant())
			IF nAt != -1
				cText := cText:Substring(0 , nAt) + cReplace + cText:Substring(nAt + cSearch:Length)
			ELSE
				EXIT
			END IF
		NEXT
	RETURN cText

	STATIC METHOD SafeFileExists(cSource AS STRING) AS LOGIC
		LOCAL lRet AS LOGIC
		TRY
			lRet := System.IO.File.Exists(cSource)
		END TRY
	RETURN lRet
END CLASS

STRUCTURE NameValue
	EXPORT Name AS STRING
	EXPORT Value AS OBJECT
	CONSTRUCTOR(_cName AS STRING , _oValue AS OBJECT)
		SELF:Name := _cName
		SELF:Value := _oValue
	RETURN
END STRUCTURE

CLASS NameValueCollection IMPLEMENTS ICollection
	PROTECT aCollection AS ArrayList
	PROTECT oEnumerator AS NameValueEnumerator
	CONSTRUCTOR()
		SELF:aCollection := ArrayList{}
		SELF:oEnumerator := NameValueEnumerator{SELF}
	RETURN
	METHOD Clear() AS VOID
		SELF:aCollection:Clear()
	RETURN
	METHOD Add(cName AS STRING , oValue AS OBJECT) AS VOID
		SELF:aCollection:Add(NameValue{cName , oValue})
	RETURN
	METHOD Get(nIndex AS INT) AS NameValue
	RETURN ((NameValue)SELF:aCollection[nIndex])
	METHOD Set(nIndex AS INT , oValue AS OBJECT) AS VOID
		LOCAL oPair AS NameValue
		oPair := SELF:Get(nIndex)
		oPair:Value := oValue
		SELF:aCollection[nIndex] := oPair
	RETURN
	METHOD GetName(nIndex AS INT) AS STRING
	RETURN ((NameValue)SELF:aCollection[nIndex]):Name
	METHOD GetValue(nIndex AS INT) AS OBJECT
	RETURN ((NameValue)SELF:aCollection[nIndex]):Value
	METHOD GetValue(cName AS STRING) AS OBJECT
		LOCAL nIndex AS INT
		nIndex := SELF:GetNameIndex(cName)
		IF nIndex != -1
			RETURN SELF:GetValue(nIndex)
		END IF
	RETURN NULL
	METHOD SetName(nIndex AS INT , cName AS STRING) AS VOID
		LOCAL oValue AS OBJECT
		oValue := SELF:Get(nIndex):Value
		SELF:aCollection[nIndex] := NameValue{cName , oValue}
	RETURN
	METHOD SetValue(nIndex AS INT , oValue AS OBJECT) AS VOID
		LOCAL cName AS STRING
		cName := SELF:Get(nIndex):Name
		SELF:aCollection[nIndex] := NameValue{cName , oValue}
	RETURN
	METHOD GetNameIndex(cName AS STRING) AS INT
		LOCAL n AS INT
		cName := cName:ToUpper()
		FOR n := 0 UPTO SELF:aCollection:Count - 1
			IF ((NameValue)SELF:aCollection[n]):Name:ToUpper() == cName
				RETURN n
			END IF
		NEXT
	RETURN -1
	METHOD GetValueIndex(oValue AS OBJECT) AS INT
		LOCAL n AS INT
		FOR n := 0 UPTO SELF:aCollection:Count - 1
			IF ((NameValue)SELF:aCollection[n]):Value == oValue
				RETURN n
			END IF
		NEXT
	RETURN -1

	METHOD ContainsName(cName AS STRING) AS LOGIC
	RETURN SELF:GetNameIndex(cName) != -1

	VIRTUAL ACCESS Count() AS INT
	RETURN SELF:aCollection:Count
	VIRTUAL ACCESS IsSynchronized() AS LOGIC
	RETURN FALSE
	VIRTUAL ACCESS SyncRoot() AS OBJECT
	RETURN NULL
	VIRTUAL METHOD CopyTo(aArray AS System.Array , nIndex AS INT) AS VOID
	RETURN
	VIRTUAL METHOD GetEnumerator() AS IEnumerator
		SELF:oEnumerator:Reset()
	RETURN SELF:oEnumerator
END CLASS

CLASS NameValueEnumerator IMPLEMENTS IEnumerator
	PROTECT oCollection AS NameValueCollection
	PROTECT nIndex AS INT
	CONSTRUCTOR(_oCollection AS NameValueCollection)
		SELF:oCollection := _oCollection
		SELF:Reset()
	RETURN
	VIRTUAL ACCESS Current() AS OBJECT
	RETURN SELF:oCollection:Get(SELF:nIndex)
	VIRTUAL METHOD MoveNext() AS LOGIC
		SELF:nIndex ++
	RETURN SELF:nIndex < SELF:oCollection:Count
	VIRTUAL METHOD Reset() AS VOID
		SELF:nIndex := -1
	RETURN

END CLASS

CLASS GACClass
	STATIC PRIVATE aGAC AS SortedList
	STATIC CONSTRUCTOR()
		Load()
	RETURN

	STATIC METHOD Load() AS VOID

		GACClass.aGAC := SortedList{}
/*		IF System.IO.Directory.Exists(Glo.gcDotNetInstallDrive + ":\WINDOWS\ASSEMBLY\GAC_32")
			GACClass.LoadFromDir(Glo.gcDotNetInstallDrive + ":\WINDOWS\ASSEMBLY\GAC_32" , ClrType.v2)
		ELSEIF System.IO.Directory.Exists(Glo.gcDotNetInstallDrive + ":\WINNT\ASSEMBLY\GAC_32")
			GACClass.LoadFromDir(Glo.gcDotNetInstallDrive + ":\WINNT\ASSEMBLY\GAC_32" , ClrType.v2)
		END IF
		IF System.IO.Directory.Exists(Glo.gcDotNetInstallDrive + ":\WINDOWS\ASSEMBLY\GAC_MSIL")
			GACClass.LoadFromDir(Glo.gcDotNetInstallDrive + ":\WINDOWS\ASSEMBLY\GAC_MSIL" , ClrType.v2)
		ELSEIF System.IO.Directory.Exists(Glo.gcDotNetInstallDrive + ":\WINNT\ASSEMBLY\GAC_MSIL")
			GACClass.LoadFromDir(Glo.gcDotNetInstallDrive + ":\WINNT\ASSEMBLY\GAC_MSIL" , ClrType.v2)
		END IF*/

		IF TRUE
			IF System.IO.Directory.Exists(Glo.gcDotNetInstallDrive + ":\WINDOWS\Microsoft.NET\ASSEMBLY\GAC_32")
				GACClass.LoadFromDir(Glo.gcDotNetInstallDrive + ":\WINDOWS\Microsoft.NET\ASSEMBLY\GAC_32" , ClrType.v4)
			ELSEIF System.IO.Directory.Exists(Glo.gcDotNetInstallDrive + ":\WINNT\Microsoft.NET\ASSEMBLY\GAC_32")
				GACClass.LoadFromDir(Glo.gcDotNetInstallDrive + ":\WINNT\Microsoft.NET\ASSEMBLY\GAC_32" , ClrType.v4)
			END IF
			IF System.IO.Directory.Exists(Glo.gcDotNetInstallDrive + ":\WINDOWS\Microsoft.NET\ASSEMBLY\GAC_MSIL")
				GACClass.LoadFromDir(Glo.gcDotNetInstallDrive + ":\WINDOWS\Microsoft.NET\ASSEMBLY\GAC_MSIL" , ClrType.v4)
			ELSEIF System.IO.Directory.Exists(Glo.gcDotNetInstallDrive + ":\WINNT\Microsoft.NET\ASSEMBLY\GAC_MSIL")
				GACClass.LoadFromDir(Glo.gcDotNetInstallDrive + ":\WINNT\Microsoft.NET\ASSEMBLY\GAC_MSIL" , ClrType.v4)
			END IF
		END IF

	RETURN

	STATIC METHOD GetAllReferences() AS ReferenceObject[]
		LOCAL aRef AS ReferenceObject[]
		LOCAL n AS INT
		aRef := ReferenceObject[]{GACClass.aGAC:Count}
		FOR n := 1 UPTO aRef:Length
			aRef[n] := ((ReferenceObject)GACClass.aGAC:GetByIndex(n - 1)):Clone()
		NEXT
	RETURN aRef

	STATIC METHOD GetClosestReference(oRef AS ReferenceObject) AS ReferenceObject
		LOCAL oRet AS ReferenceObject
		oRet := GACClass.GetClosestReference(oRef:cName , oRef:cVersion , oRef:eClr)
		IF oRet == NULL
			oRet := GACClass.GetClosestReference(oRef:cName , oRef:cVersion , ClrType.Unknown)
		END IF
		IF oRet != NULL
			oRef := oRet:Clone()
		ENDIF
	RETURN oRef
	STATIC METHOD GetClosestReference(cName AS STRING) AS ReferenceObject
	RETURN GACClass.GetClosestReference(cName , "")
	STATIC METHOD GetClosestReference(cName AS STRING , eClr AS ClrType , lAllClr AS LOGIC) AS ReferenceObject
		LOCAL oRef AS ReferenceObject
		oRef := GACClass.GetClosestReference(cName , "" , eClr)
		IF oRef == NULL .and. lAllClr
			oRef := GACClass.GetClosestReference(cName , "" , ClrType.Unknown)
		END IF
	RETURN oRef
	STATIC METHOD GetClosestReference(cName AS STRING , cVersion AS STRING) AS ReferenceObject
		LOCAL oRef AS ReferenceObject
		oRef := GACClass.GetClosestReference(cName , cVersion , ClrType.v4)
		IF oRef == NULL
			oRef := GACClass.GetClosestReference(cName , cVersion , ClrType.Unknown)
		END IF
	RETURN oRef
	STATIC METHOD GetClosestReference(cName AS STRING , cVersion AS STRING , eClr AS ClrType) AS ReferenceObject
		LOCAL oRef AS ReferenceObject
		LOCAL oRet AS ReferenceObject
		LOCAL n AS INT
		cName := cName:ToUpper()
		cVersion := cVersion:ToUpper()
		FOR n := 0 UPTO GACClass.aGAC:Count - 1
			oRef := (ReferenceObject)GACClass.aGAC:GetByIndex(n)
			IF eClr != ClrType.Unknown .and. oRef:eClr != eClr
				LOOP
			END IF
			IF oRef:cName:ToUpper() == cName
				oRet := oRef:Clone()
				IF oRet:cVersion:ToUpper() == cVersion
					RETURN oRet
				ENDIF
			ENDIF
		NEXT
	RETURN oRet

	STATIC METHOD LoadFromDir(cFolder AS STRING , eClr AS ClrType) AS VOID
		LOCAL aFiles AS STRING[]
		LOCAL aDirs AS STRING[]
		LOCAL aVerDirs AS STRING[]
		LOCAL cBaseFolder AS STRING
		LOCAL cVerFolder AS STRING
		LOCAL cName AS STRING
		LOCAL oRef AS ReferenceObject
		LOCAL cKey AS STRING
		LOCAL n,m,nAt AS INT

		aDirs := Directory.GetDirectories(cFolder)
		FOR n := 1 UPTO aDirs:Length
			cBaseFolder := aDirs[n]
			cName := Fun.GetFileName(cBaseFolder)
			aVerDirs := Directory.GetDirectories(cBaseFolder)
			FOR m := 1 UPTO aVerDirs:Length
				cVerFolder := aVerDirs[m]
				aFiles := Directory.GetFiles(cVerFolder , "*.dll")
				cVerFolder := Fun.GetFileName(cVerFolder)
				IF eClr == ClrType.v4 .and. cVerFolder:StartsWith("v4.0_")
					cVerFolder := cVerFolder:Substring(5)
				END IF
				nAt := cVerFolder:IndexOf('_')
				IF nAt > 0 .and. aFiles:Length > 0
					oRef := ReferenceObject{cName , ReferenceType.GAC}
					oRef:eClr := eClr
					IF aFiles:Length != 0
						oRef:cFileName := aFiles[1]
					ENDIF
					oRef:cVersion := cVerFolder:Substring(0, nAt)
					cVerFolder := cVerFolder:Substring(nAt + 1)
					nAt := cVerFolder:IndexOf('_')
					IF nAt == -1
						oRef:cCulture := ""
						oRef:cKeyToken := cVerFolder
					ELSEIF nAt == 0
						oRef:cCulture := ""
						oRef:cKeyToken := cVerFolder:Substring(1)
					ELSE
						oRef:cCulture := cVerFolder:Substring(0 , nAt -1)
						oRef:cKeyToken := cVerFolder:Substring(nAt + 1)
					ENDIF
					DO CASE
					CASE cBaseFolder:IndexOf("GAC_32") != 0
						oRef:cArchitecture := "32"
					CASE cBaseFolder:IndexOf("GAC_64") != 0
						oRef:cArchitecture := "64"
					CASE cBaseFolder:IndexOf("GAC_MSIL") != 0
						oRef:cArchitecture := "MSIL"
					END CASE
					oRef:eFrameworks := Frameworks.Full
					oRef:lCopy := FALSE
					cKey := oRef:cName + oRef:cVersion + oRef:cCulture + oRef:cKeyToken + oRef:cArchitecture
					IF !GACClass.aGAC:ContainsKey(cKey)
						IF .not. String.IsNullOrEmpty(oRef:cVersion) .and. oRef:cName:StartsWith("Vulcan") .and. oRef:cVersion:StartsWith("1.1.1")
//						IF .not. String.IsNullOrEmpty(oRef:cVersion) .and. oRef:cName:StartsWith("Vulcan") .and. oRef:cVersion:StartsWith("1.1.20")
							LOOP
						END IF
						GACClass.aGAC:Add(cKey , oRef)
					ENDIF
				ENDIF
			NEXT

		NEXT

	RETURN

END CLASS

END NAMESPACE

FUNCTION Chr(c AS INT) AS STRING
RETURN System.String{(Char)c,1}

FUNCTION ChrW(c AS DWORD) AS STRING
RETURN System.String{(Char)c,1}
//RETURN Convert.ToString((Char)c)
FUNCTION ChrW( c AS INT) AS STRING
RETURN System.String{(Char)c,1}
//RETURN Convert.ToString((Char)c)

FUNCTION Space(dwSize AS DWORD) AS STRING
RETURN System.String{' ' , (INT)dwSize}
FUNCTION Space(dwSize AS INT ) AS STRING
RETURN System.String{' ' , dwSize}

FUNCTION Left(c AS STRING , dwLen AS INT) AS STRING
RETURN Left(c , (DWORD)dwLen)
FUNCTION Left(c AS STRING , dwLen AS DWORD) AS STRING
RETURN iif(dwLen >= c:Length , c , c:Substring(0 , (INT)dwLen ))

FUNCTION Right(c AS STRING , dwLen AS INT) AS STRING
RETURN Right(c , (DWORD)dwLen)
FUNCTION Right(c AS STRING , dwLen AS DWORD) AS STRING
RETURN IIF( dwLen >= c:Length , c , c:Substring(c:Length - (INT)dwLen , (INT)dwLen))

FUNCTION SLen(c AS STRING) AS DWORD
RETURN (DWORD)c:Length

FUNCTION SubStr(c AS STRING , nStart AS DWORD , dwLen AS DWORD) AS STRING
RETURN SubStr(c , (INT)nStart , (INT)dwLen)
FUNCTION SubStr(c AS STRING , nStart AS INT , dwLen AS INT) AS STRING
	LOCAL cReturn AS STRING

	cReturn := ""

	IF nStart == 0
		nStart := 1
	ENDIF

	IF nStart < 0
		nStart := c:Length+nStart+1
	ENDIF

	IF dwLen < 0
		dwLen := c:Length
	ENDIF

	IF nStart <= c:Length .and. nStart > 0
		dwLen := Math.Min( c:Length - nStart + 1, dwLen )
		cReturn := c:Substring( nStart - 1, dwLen )
	ENDIF

RETURN cReturn

FUNCTION SubStr(c AS STRING , nStart AS DWORD) AS STRING
RETURN SubStr(c , (INT)nStart)
FUNCTION SubStr(c AS STRING , nStart AS INT) AS STRING
	LOCAL cReturn AS STRING

	cReturn := ""

	IF nStart == 0
		nStart := 1
	ENDIF

	IF nStart < 0
		nStart := c:Length+nStart+1
	ENDIF

	LOCAL nLength AS INT
	IF nStart <= c:Length .and. nStart > 0
		nLength := Math.Min( c:Length - nStart + 1, c:Length )
		cReturn := c:Substring( nStart - 1, nLength )
	ENDIF

RETURN cReturn

FUNCTION At(cSearch AS STRING , c AS STRING) AS DWORD
	IF String.IsNullOrEmpty(cSearch)
		RETURN 0
	ENDIF
RETURN (DWORD)c:IndexOf(cSearch,StringComparison.Ordinal) + 1
FUNCTION At3(cSearch AS STRING , c AS STRING , dwOff AS INT) AS DWORD
RETURN At3(cSearch,c,(DWORD)dwOff)
FUNCTION At3(cSearch AS STRING , c AS STRING , dwOff AS DWORD) AS DWORD
	IF String.IsNullOrEmpty( cSearch )
		RETURN 0
	ENDIF

	IF dwOff < (DWORD)c:Length
		RETURN (DWORD)c:IndexOf(cSearch,(INT)dwOff,StringComparison.Ordinal) + 1
	ENDIF

RETURN 0
FUNCTION RAt(cSearch AS STRING , c AS STRING) AS DWORD
	IF String.IsNullOrEmpty( cSearch )
		RETURN 0
	ENDIF
RETURN (DWORD)c:LastIndexOf(cSearch,StringComparison.Ordinal) + 1

FUNCTION Occurs(cSearch AS STRING , c AS STRING) AS DWORD
	IF String.IsNullOrEmpty(cSearch)
		RETURN 0
	END IF
RETURN Occurs(cSearch[0] , c)
FUNCTION Occurs(cSearch AS Char , c AS STRING) AS DWORD
	LOCAL nCount AS DWORD
	LOCAL n AS INT

	IF String.IsNullOrEmpty(c)
		RETURN 0
	ENDIF

	FOR n := 0 UPTO c:Length - 1
		IF c[n] == cSearch
			nCount ++
		END IF
	NEXT
RETURN nCount

FUNCTION AScanExact(a AS STRING[] , cSearch AS STRING) AS INT
RETURN System.Array.IndexOf(a, cSearch) + 1

FUNCTION Upper(s AS STRING) AS STRING
RETURN s:ToUpper()
FUNCTION Lower(s AS STRING) AS STRING
RETURN s:ToLower()

FUNCTION InStr(cSearch AS STRING , c AS STRING) AS LOGIC
	IF String.IsNullOrEmpty(cSearch)
		RETURN FALSE
	ENDIF
RETURN c:IndexOf(cSearch , StringComparison.Ordinal) != -1

FUNCTION AsHexString(n AS INT) AS STRING
RETURN String.Format("{0:X8}", n)

FUNCTION CharPos(c AS STRING , n AS INT) AS STRING
RETURN CharPos(c,(DWORD)n)
FUNCTION CharPos(c AS STRING , n AS DWORD) AS STRING
	LOCAL char AS STRING
	IF n > 0 .and. n <= (DWORD)c:Length
		char := c:Substring((INT)n - 1 , 1)
	ENDIF
RETURN char

FUNCTION Val(c AS STRING) AS INT
	LOCAL nResult AS INT
	LOCAL n AS INT
	n := c:IndexOf('.')
	IF n != -1
		c := c:Substring(0,n)
		IF c:Length == 0
			c := "0"
		END IF
	END IF
	n := c:IndexOf(',')
	IF n != -1
		c := c:Substring(0,n)
		IF c:Length == 0
			c := "0"
		END IF
	END IF
	Int32.TryParse(c , OUT nResult)
RETURN nResult

FUNCTION ValR8(cReal AS STRING) AS REAL8
	LOCAL cInt,cDec AS STRING
	LOCAL rRet AS REAL8
	LOCAL nDiv AS INT
	LOCAL n AS DWORD
	n := At("." , cReal)
	IF n == 0
		rRet := REAL8(Val(cReal))
	ELSE
		cInt := Left(cReal , n - 1)
		cDec := SubStr(cReal , n + 1)
		rRet := REAL8(Val(cInt))
		IF cDec:Length != 0
			nDiv := 10
			FOR n := 2 UPTO SLen(cDec)
				nDiv *= 10
			NEXT
			rRet += REAL8(Val(cDec)) / REAL8(nDiv)
		ENDIF
	ENDIF
RETURN rRet

FUNCTION Empty(c AS STRING) AS LOGIC
	LOCAL nChar AS INT
	LOCAL n AS INT
	IF String.IsNullOrEmpty(c)
		RETURN TRUE
	ENDIF
	FOR n := 0 UPTO c:Length - 1
		nChar := (INT)c[n]
		IF nChar != 9 .and. nChar != 10 .and. nChar != 13 .and. nChar != 32
			RETURN FALSE
		ENDIF
	NEXT
RETURN TRUE

FUNCTION StrTran(c AS STRING , cSearch AS STRING , cReplace AS STRING) AS STRING
RETURN c:Replace(cSearch , cReplace)

FUNCTION AbsInt(n AS INT) AS INT
RETURN Math.Abs(n)

FUNCTION LTrim(s AS STRING) AS STRING
RETURN s:TrimStart(NULL)

FUNCTION IsUpper(c AS STRING) AS LOGIC
	IF String.IsNullOrEmpty(c)
		RETURN FALSE
	ENDIF
RETURN Char.IsUpper(c[0])
FUNCTION IsLower(c AS STRING) AS LOGIC
	IF String.IsNullOrEmpty(c)
		RETURN FALSE
	ENDIF
RETURN Char.IsLower(c[0])

FUNCTION Proper(c AS STRING) AS STRING
	LOCAL cRet AS STRING
	DO CASE
	CASE String.IsNullOrEmpty(c)
		cRet := ""
	CASE c:Length == 1
		cRet := c:ToUpper()
	OTHERWISE
		cRet := c:Substring(0,1):ToUpper() + c:Substring(1):ToLower()
	END CASE
RETURN cRet

FUNCTION SplitPath(c AS STRING,sDrive REF STRING, sDir REF STRING, sFile  REF STRING, sExt REF STRING) AS VOID
	TRY
		c:=System.IO.Path.GetFullPath(c)
		sDir:=System.IO.Path.GetDirectoryName(c)
		sFile:=System.IO.Path.GetFileNameWithoutExtension(c)
		sExt:=System.IO.Path.GetExtension(c)
		sDrive:=Left(sDir,2)
		sDir:=SubStr(sDir,3)
		IF !(Right(sDir,1)=="\")
			sDir+="\"
		END IF
	CATCH // todo auto mphke giati kapoies fores o compiler kanei report errors me corrupted filename
		sDir:=""
		sFile:=""
		sExt:=""
		sDrive:=""
		sDir:=""
	END TRY
RETURN


