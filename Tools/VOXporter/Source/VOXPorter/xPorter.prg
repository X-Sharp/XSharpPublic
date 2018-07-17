USING System.IO
USING System.Collections
USING System.Collections.Generic
USING System.Windows.Forms
USING System.Reflection
USING System.Text
USING System.Drawing

USING Xide

GLOBAL DefaultOutputFolder := "" AS STRING
GLOBAL DefaultSourceFolder := "" AS STRING
GLOBAL SDKDefines_FileName := "" AS STRING
GLOBAL RuntimeFolder := "" AS STRING
GLOBAL NoWarningScreen := FALSE AS LOGIC

GLOBAL Replace_VOOleContainer := NULL AS STRING

DEFINE ToolName := "VO-xPorter" AS STRING

[STAThread];
FUNCTION Start(asParams AS STRING[]) AS VOID
	LOCAL oOptions AS xPorterOptions
	oOptions := xPorterOptions{}

	oOptions:RemoveClassClause := TRUE
//	oOptions:ChangePascalToStrict := TRUE
	oOptions:ChangeInitAxitToCtorDtor := TRUE
	oOptions:RemoveExportLocalClause := TRUE
//	oOptions:RemoveInitAxitReturnClause := TRUE
//	oOptions:RemoveAssignReturnClause := TRUE
	oOptions:RemoveDeclareMethod := TRUE
	oOptions:AddMissingConstrutors := FALSE
	oOptions:AllClassesPartial := FALSE
	oOptions:IgnoreDuplicateDefines := TRUE
	oOptions:DontGenerateEmptyFiles := TRUE
	oOptions:AdjustCallbackFunctions := TRUE
	
	xPorter.Options := oOptions
	
	ReadIni()
	
	ReadCommandLine(asParams)

	ShowWarningScreen()

	xPorter.uiForm := xPorterUI{oOptions}
	
	Application.EnableVisualStyles()
	Application.Run(xPorter.uiForm)
	
RETURN

FUNCTION ReadCommandLine(asParams AS STRING[]) AS VOID
	FOREACH cParam AS STRING IN asParams
		LOCAL cUpper := cParam:ToUpper() AS STRING
		LOCAL cFileName AS STRING
		TRY
			DO CASE
			CASE cUpper:StartsWith("/S:")
				cFileName := cParam:Substring(3)
				IF SafeFileExists(cFileName) .or. SafeFolderExists(cFileName)
					DefaultSourceFolder := cFileName
				END IF
			CASE cUpper:StartsWith("/D:")
				cFileName := cParam:Substring(3)
				IF Path.IsPathRooted(cFileName)
					DefaultOutputFolder := cFileName
				END IF
			CASE cUpper:StartsWith("/R:")
				cFileName := cParam:Substring(3)
				IF SafeFolderExists(cFileName)
					RuntimeFolder := cFileName
				END IF
			CASE cUpper:StartsWith("/NOWARNING")
				NoWarningScreen := TRUE
			END CASE
		END TRY
	NEXT
	
RETURN 

FUNCTION ReadIni() AS VOID
	LOCAL cFileName AS STRING
	cFileName := Application.StartupPath + "\voxporter.ini"
	IF File.Exists(cFileName)
		LOCAL aLines AS STRING[]
		aLines := File.ReadAllLines(cFileName)
		FOREACH cLine AS STRING IN aLines
			LOCAL nIndex AS INT
			nIndex := cLine:IndexOf('=')
			IF nIndex != -1
				LOCAL cKey,cValue AS STRING
				cKey := cLine:Substring(0 , nIndex):Trim():ToUpper()
				cValue := cLine:Substring(nIndex + 1):Trim()
				DO CASE
				CASE cKey == "OUTPUTFOLDER"
					DefaultOutputFolder := cValue
				CASE cKey == "SOURCEFOLDER"
					DefaultSourceFolder := cValue
				CASE cKey == "VOFOLDER"
					VOFolder.Set(cValue)
				CASE cKey == "SDKDEFINESDLL"
					SDKDefines_FileName := cValue
				CASE cKey == "NOWARNINGSCREEN"
					NoWarningScreen := cValue == "1"
				CASE cKey == "_VOOLECONTAINER"
					Replace_VOOleContainer := cValue
				CASE cKey == "RUNTIMELOCATION"
					RuntimeFolder := cValue
				END CASE
			END IF
		NEXT
	END IF
	
	IF String.IsNullOrWhiteSpace(SDKDefines_FileName) .or. .not. File.Exists(SDKDefines_FileName)
		IF File.Exists(Application.StartupPath + "\SDK_Defines.dll")
			SDKDefines_FileName := Application.StartupPath + "\SDK_Defines.dll"
		END IF
	END IF
	
	TRY
		IF .not. VOFolder.IsValid()
			LOCAL vo,ver AS Microsoft.Win32.RegistryKey
			vo := Microsoft.Win32.Registry.LocalMachine:OpenSubKey("Software\GrafX\Visual Objects")
			FOREACH subkey AS STRING IN vo:GetSubKeyNames()
				ver := vo:OpenSubKey(subkey)
				IF ver:GetValue("InstallPath") != NULL
					VOFolder.Set(ver:GetValue("InstallPath"):ToString())
				END IF
			NEXT
		END IF
	END TRY
	
RETURN 

CLASS WarningDialog INHERIT Form
	PROTECT oTimer AS Timer
	PROTECT lAllowClose AS LOGIC
	CONSTRUCTOR(cRtfText AS STRING)
		SUPER()
		SELF:Size := Size{800,700}
		SELF:StartPosition := FormStartPosition.CenterScreen
		SELF:FormBorderStyle := FormBorderStyle.Fixed3D
		SELF:MaximizeBox := FALSE
		SELF:MinimizeBox := FALSE
		SELF:ShowIcon := FALSE
		SELF:Text := "VO-xPorter important information!"

		LOCAL oRtf AS RichTextBox
		oRtf := RichTextBox{}
		oRtf:Dock := DockStyle.Fill
		SELF:Controls:Add(oRtf)
		oRtf:Rtf := cRtfText
		
		SELF:oTimer := Timer{}
		SELF:oTimer:Interval := 5000
		SELF:oTimer:Tick += SELF:Timer_tick
		SELF:oTimer:Start()
	RETURN
	PROTECTED METHOD Timer_tick(sender AS OBJECT , e AS EventArgs) AS VOID
		SELF:lAllowClose := TRUE
		SELF:oTimer:Dispose()
	RETURN
	PROTECTED METHOD OnClosing(e AS System.ComponentModel.CancelEventArgs) AS VOID
		SUPER:OnClosing(e)
		IF .not. SELF:lAllowClose .and. Control.ModifierKeys != Keys.Control
			e:Cancel := TRUE
			SELF:oTimer:Stop()
			MessageBox.Show("Please read this important information!" , ToolName , MEssageBoxButtons.OK , MessageBoxIcon.Exclamation)
			SELF:oTimer:Interval := 1500
			SELF:oTimer:Start()
		ENDIF
	RETURN
END CLASS

FUNCTION ShowWarningScreen() AS VOID
	LOCAL oForm AS WarningDialog
	IF NoWarningScreen
		RETURN
	END IF
	TRY
		oForm := WarningDialog{File.ReadAllText("ReadMe.rtf")}
		oForm:ShowDialog()
	END TRY
RETURN 

CLASS VOFolder
	STATIC PROTECT _scFolder := NULL AS STRING
	STATIC METHOD Set(cFolder AS STRING) AS VOID
		_scFolder := cFolder
	RETURN
	STATIC METHOD IsValid() AS LOGIC
		LOCAL lValid := FALSE AS LOGIC
		TRY
			lValid := .not. (String.IsNullOrEmpty(_scFolder) .or. .not. Directory.Exists(_scFolder))
		END TRY
	RETURN lValid
	STATIC METHOD Get() AS STRING
		STATIC LOCAL lChecked := FALSE AS LOGIC
		IF .not. lChecked
			lChecked := TRUE
			IF .not. VOFolder.IsValid()
				MessageBox.Show(e"Could not find VO installation folder, macros in code like %cavosamplesrootdir% will not be translated directly.\r\n\r\nYou can manually set the VO folder in xPorter.ini." , ToolName)
			END IF
		END IF
	RETURN _scFolder
END CLASS

STRUCTURE xPorterOptions
	EXPORT RemoveClassClause AS LOGIC
//	EXPORT ChangePascalToStrict AS LOGIC
	EXPORT ChangeInitAxitToCtorDtor AS LOGIC
	EXPORT RemoveDeclareMethod AS LOGIC
	EXPORT RemoveExportLocalClause AS LOGIC
//	EXPORT RemoveInitAxitReturnClause AS LOGIC
//	EXPORT RemoveAssignReturnClause AS LOGIC
	EXPORT AllClassesPartial AS LOGIC
	EXPORT AddMissingConstrutors AS LOGIC
	EXPORT IgnoreDuplicateDefines AS LOGIC
	EXPORT DontGenerateEmptyFiles AS LOGIC
	EXPORT AdjustCallbackFunctions AS LOGIC
	EXPORT ExportOnlyDefines AS LOGIC
END STRUCTURE

INTERFACE IProgressBar
	METHOD SetProgressBarRange(nValue AS INT) AS VOID
	METHOD SetProgressBarValue(nValue AS INT) AS VOID
	METHOD AdvanceProgressbar() AS VOID
END INTERFACE

CLASS xPorter
	STATIC EXPORT Options AS xPorterOptions
	STATIC EXPORT uiForm AS xPorterUI

	STATIC EXPORT ExportXideBinaries := TRUE AS LOGIC
	

	STATIC PROPERTY OverWriteProjectFiles AS LOGIC AUTO

	STATIC PROTECT _aFoundDefines := SortedList<STRING,STRING>{} AS SortedList<STRING,STRING>
	STATIC PROTECT _aSDKDefines AS Dictionary<STRING,STRING>
	STATIC PROPERTY SDKDefines AS Dictionary<STRING,STRING> GET _aSDKDefines
	
	STATIC CONSTRUCTOR()
		
//		MessageBox.Show("loading file : " + Environment.CurrentDirectory + "\SDK_Defines.dll")
		
		TRY
			LOCAL oAssembly AS Assembly
			// This did not load with reflection in Otto's pc
//			oAssembly := Assembly.LoadFile(Environment.CurrentDirectory + "\SDK_Defines.dll")
//			oAssembly := TypeOf(SDK_Defines.Functions):Assembly
			oAssembly := TypeOf(SDK_Defines.Dummy):Assembly
			
			LOCAL oType AS Type
//			oType := oAssembly:GetType("SDK_Defines.Functions")
			oType := oAssembly:GetType("Functions")
			LOCAL aFields AS FieldInfo[]
			aFields := oType:GetFields()
			_aSDKDefines := Dictionary<STRING,STRING>{aFields:Length}
			FOREACH oField AS FieldInfo IN aFields
				IF (oField:FieldType == TypeOf(INT) .or. oField:FieldType == TypeOf(DWORD)).and. ;
					.not. _aSDKDefines:ContainsKey(oField:Name:ToUpper())
					_aSDKDefines:Add(oField:Name:ToUpper() , oField:GetValue(NULL):ToString())
				END IF
			NEXT
		CATCH e AS Exception
			MessageBox.Show(e:ToString())
		END TRY
		
	STATIC METHOD Reset() AS VOID
		_aFoundDefines:Clear()
	RETURN

	STATIC METHOD Message(cMessage AS STRING) AS VOID
		xPorter.uiForm:SetProgressText(cMessage)
	STATIC METHOD Message(cMessage AS STRING , cMessage2 AS STRING) AS VOID
		xPorter.uiForm:SetProgressText(cMessage + " " + cMessage2)
		

	STATIC METHOD xPort_AppFromFolder(cSourceFolder AS STRING , cOutputFolder AS STRING , cSolutionName AS STRING, cAppName AS STRING) AS VOID
/*		LOCAL cName AS STRING
		cName := cSourceFolder
		LOCAL nIndex AS INT
		nIndex := cName:LastIndexOf("\")
		IF nIndex != -1
			cName := cName:Substring(nIndex + 1)
		END IF*/
		
		xPorter.Reset()
		
		LOCAL oProject AS VOProjectDescriptor
		oProject := VOProjectDescriptor{cSolutionName , NewGuid()}
		LOCAL oApp AS ApplicationDescriptor
		oApp := oProject:AddApplication(cAppName)
		oApp:LoadFromFolder(cSourceFolder)

		IF oApp:Loaded
			oProject:xPort(cOutputFolder)
		ENDIF
	RETURN
		
	STATIC METHOD xPort_AppFromAef(cAefFile AS STRING , cOutputFolder AS STRING , cSolutionName AS STRING , cAppName AS STRING) AS VOID
		xPorter.Reset()		
		xPorter.Options:IgnoreDuplicateDefines := FALSE
		
		LOCAL oApp AS ApplicationDescriptor
		LOCAL oProject AS VOProjectDescriptor
		oProject := VOProjectDescriptor{cSolutionName , NewGuid()}
		oApp := ApplicationDescriptor.CreateFromAef(cAefFile , oProject , cAppName)
//		oProject:SetName(oApp:Name)
		oProject:AddApplication(oApp)
		
		IF oApp:Loaded
			oProject:xPort(cOutputFolder)
		ENDIF
	RETURN

	STATIC METHOD xPort_AppsFromAefsInFolder(cSourceFolder AS STRING , cOutputFolder AS STRING , cSolutionName AS STRING) AS VOID
		xPorter.Reset()
		xPorter.Options:IgnoreDuplicateDefines := FALSE

		LOCAL aAefs AS STRING[]
		aAefs := Directory.GetFiles(cSourceFolder , "*.aef")
		
		LOCAL oProject AS VOProjectDescriptor
//		oProject := VOProjectDescriptor{DirectoryInfo{cSourceFolder}:Name , NewGuid()}
		oProject := VOProjectDescriptor{cSolutionName , NewGuid()}

		xPorter.uiForm:SetProgressBarRange(aAefs:Length)
		FOREACH cAefFile AS STRING IN aAefs
			xPorter.uiForm:AdvanceProgressbar()
			LOCAL oApp AS ApplicationDescriptor
			oApp := ApplicationDescriptor.CreateFromAef(cAefFile , oProject , NULL)
			oProject:AddApplication(oApp)
		NEXT
		
		oProject:xPort(cOutputFolder)
	RETURN

	STATIC METHOD AllowEntity(oEntity AS EntityObject , oModule AS ModuleDescriptor) AS LOGIC
		IF xPorter.Options:IgnoreDuplicateDefines
			IF oEntity:eType == EntityType._Define
				IF _aFoundDefines:ContainsKey(oEntity:cName:ToUpper())
					RETURN FALSE
				ELSE
					_aFoundDefines:Add(oEntity:cName:ToUpper() , oEntity:cName)
	//				RETURN TRUE
				END IF
			END IF
		END IF
	RETURN TRUE

	STATIC METHOD AllowModule(oModule AS ModuleDescriptor) AS LOGIC
		// There were SDK related checks here
	RETURN TRUE
	
		
END CLASS


CLASS VOProjectDescriptor
	PROTECT _cName AS STRING
	PROTECT _cGuid AS STRING
	PROTECT _aApplications AS List<ApplicationDescriptor>
	PROTECT _cProjectFolder AS STRING
	PROTECT _cSolution_SDKDefines_Filename AS STRING
	
	PROPERTY Name AS STRING GET SELF:_cName
	PROPERTY Guid AS STRING GET SELF:_cGuid
	PROPERTY Applications AS List<ApplicationDescriptor> GET SELF:_aApplications
	PROPERTY ProjectFolder AS STRING GET SELF:_cProjectFolder
	PROPERTY Solution_SDKDefines_Filename AS STRING GET SELF:_cSolution_SDKDefines_Filename
	
	CONSTRUCTOR(cName AS STRING , cGuid AS STRING)
		SELF:_cName := cName
		SELF:_cGuid := cGuid
		SELF:_aApplications := List<ApplicationDescriptor>{}
	RETURN

	METHOD SetName(cName AS STRING) AS VOID
		SELF:_cName := cName
	RETURN

	METHOD AddApplication(oApp AS ApplicationDescriptor) AS ApplicationDescriptor
		SELF:_aApplications:Add(oApp)
//		oApp:SetProject(SELF)
	RETURN oApp
	METHOD AddApplication(cName AS STRING) AS ApplicationDescriptor
		LOCAL oApp AS ApplicationDescriptor
		oApp := ApplicationDescriptor{cName , SELF}
		SELF:_aApplications:Add(oApp)
	RETURN oApp
	
	PROTECTED METHOD CountModules() AS INT
		LOCAL nModules := 0 AS INT
		FOREACH oApp AS ApplicationDescriptor IN SELF:_aApplications
			nModules += oApp:ModulesCount
		NEXT
	RETURN nModules
	
	METHOD FindAppByName(cName AS STRING) AS ApplicationDescriptor
		cName := cName:ToUpper()
		FOREACH oApp AS ApplicationDescriptor IN SELF:_aApplications
			IF oApp:Name:ToUpper() == cName
//				MessageBox.Show(oApp:Name)
				RETURN oApp
			END IF
		NEXT
	RETURN NULL
	
		
	METHOD xPort(cOutputFolder AS STRING) AS VOID
		SELF:_cProjectFolder := cOutputFolder
		
		Directory.CreateDirectory(cOutputFolder)
		IF File.Exists(SDKDefines_FileName)
			SELF:_cSolution_SDKDefines_Filename := cOutputFolder + "\" + GetFilename(SDKDefines_FileName)
			IF .not. File.Exists(SELF:_cSolution_SDKDefines_Filename)
				File.Copy(SDKDefines_FileName , SELF:_cSolution_SDKDefines_Filename)
			END IF
		END IF
		
		xPorter.uiForm:SetProgressBarRange(SELF:CountModules() * 2) // analysis and export
		SELF:Analyze()
			
		FOREACH oApp AS ApplicationDescriptor IN SELF:_aApplications
			IF oApp:Loaded
				oApp:Generate()
			END IF
		NEXT
		
		SELF:CreateSolutionFile(cOutputFolder , TRUE)
		SELF:CreateSolutionFile(cOutputFolder , FALSE)
		
		xPorter.Message("Finished xPorting!")
	RETURN

	PROTECTED METHOD Analyze() AS VOID
		FOREACH oApplication AS ApplicationDescriptor IN SELF:_aApplications
			xPorter.Message("Analyzing application/library" , oApplication:Name)
			oApplication:Analyze()
		NEXT
	RETURN
	
	PROTECTED METHOD CreateSolutionFile(cFolder AS STRING , lXide AS LOGIC) AS VOID
		LOCAL oTemplate AS StreamReader
		LOCAL oOutput AS StreamWriter
		LOCAL cTemplate AS STRING
		LOCAL cSolutionName AS STRING
		LOCAL cFileName AS STRING
		
		cSolutionName := SELF:Name //+ "_xported"
		
		xPorter.Message("Creating solution file for " + iif(lXide , "XIDE" , "VS"))

		IF lXide
			cFileName := cFolder + "\" + MakePathLegal( cSolutionName ) + ".viproj"
			
			IF .not. xPorter.OverWriteProjectFiles .and. File.Exists(cFileName)
				xPorter.Message("XIDE solution file already exists.")
				RETURN
			END IF
			
			oTemplate := StreamReader{Application.StartupPath + "\templates\template_XIDE.xiproj" , System.Text.Encoding.Default , TRUE} 
			TRY
				oOutput := StreamWriter{cFileName , FALSE , System.Text.Encoding.Default}
			CATCH
				oOutput := NULL
				oTemplate:Close()
			END TRY
			IF oOutput == NULL // locked by xide
				RETURN
			END IF
		ELSE
			cFileName := cFolder + "\" + MakePathLegal( cSolutionName ) + ".sln"
			IF .not. xPorter.OverWriteProjectFiles .and. File.Exists(cFileName)
				xPorter.Message("VS solution file already exists.")
				RETURN
			END IF

			oTemplate := StreamReader{Application.StartupPath + "\templates\template_VS.sln" , TRUE} 
			oOutput := StreamWriter{cFileName , FALSE , System.Text.Encoding.UTF8}
		END IF
		
		DO WHILE oTemplate:Peek() != -1
			cTemplate := oTemplate:ReadLine()
			DO CASE
			CASE cTemplate:StartsWith(";")
				LOOP
			CASE cTemplate == "%applications%" .or. cTemplate == "%projects%"
				FOREACH oApp AS ApplicationDescriptor IN SELF:_aApplications
					IF .not. oApp:Saved
						LOOP
					END IF
					IF lXide
						LOCAL aText AS STRING[]
						aText := File.ReadAllLines(oApp:AppFile_XIDE , System.Text.Encoding.Default)
						FOREACH cLine AS STRING IN aText
							oOutput:WriteLine(cLine)
						NEXT
					ELSE
//						#warning make path relative
						oOutput:WriteLine(String.Format(e"Project(\"{{{0}}}\") = \"{1}\", \"{2}\", \"{{{3}}}\"" , SELF:Guid , oApp:Name , GetRelativePath(cFolder , oApp:AppFile_VS) , oApp:Guid))
						oOutput:WriteLine("EndProject")
					END IF
				NEXT
			OTHERWISE
				cTemplate := cTemplate:Replace("%solutionname%" , cSolutionName)
				cTemplate := cTemplate:Replace("%solutionguid%" , SELF:Guid)
				cTemplate := cTemplate:Replace("%solutionfolder%" , cFolder)
				IF cTemplate:Contains("%newguid%")
					cTemplate := cTemplate:Replace("%newguid%" , NewGuid())
				END IF
				oOutput:WriteLine(cTemplate)
			ENDCASE
		END DO
		
		oTemplate:Close()
		oOutput:Close()
	RETURN
	
END CLASS

ENUM ApplicationType
	MEMBER Exe := 0
	MEMBER WinExe := 1
	MEMBER Library := 2
END ENUM


CLASS ApplicationDescriptor
	PROTECT _cName AS STRING
	PROTECT _cGuid AS STRING
//	PROTECT _cAppFile_VS AS STRING
//	PROTECT _cAppFile_XIDE AS STRING
	PROTECT _lLoaded AS LOGIC
	PROTECT _lSaved AS LOGIC
	PROTECT _eType AS ApplicationType
	PROTECT _aModules AS List<ModuleDescriptor>
	PROTECT _aReferences AS List<ApplicationDescriptor>
	PROTECT _aGACReferences AS List<STRING>
	PROTECT _aBrowseReferences AS List<STRING>
	PROTECT _aProjectReferences AS List<STRING>
	
	PROTECT _aFunctions AS List<STRING>
	PROTECT _aCallbacks AS List<STRING>
	
	PROTECT _lOptionOverflow AS LOGIC
	PROTECT _lOptionIntDiv AS LOGIC
	
	PROTECT _oProject AS VOProjectDescriptor
	
	CONSTRUCTOR(cName AS STRING , oProject AS VOProjectDescriptor)
		SELF(cName , NewGuid() , oProject)
	RETURN
	CONSTRUCTOR(cName AS STRING , cGuid AS STRING , oProject AS VOProjectDescriptor)
		SELF:_cName := cName
		SELF:_cGuid := cGuid
		SELF:_aModules := List<ModuleDescriptor>{}
		SELF:_aReferences := List<ApplicationDescriptor>{}
		SELF:_aGACReferences := List<STRING>{}
		SELF:_aBrowseReferences := List<STRING>{}
		SELF:_aProjectReferences := List<STRING>{}
		SELF:_eType := ApplicationType.Library
		
		SELF:_aFunctions := List<STRING>{}
		SELF:_aCallbacks := List<STRING>{}
		
		SELF:_oProject := oProject
		
		SELF:xPortOptions := xPorter.Options
		
//		SELF:_cAppFile_XIDE := SELF:AppFolder + "\" + SELF:Name + ".viapp"
//		SELF:_cAppFile_VS := SELF:AppFolder + "\" + SELF:Name + ".xsproj"

		SELF:AddRuntimeReference("VulcanRT")
		SELF:AddRuntimeReference("VulcanRTFuncs")
		
	RETURN
	
	PROPERTY Name AS STRING GET SELF:_cName
	PROPERTY PathValidName AS STRING GET MakePathLegal( SELF:_cName )
	PROPERTY Guid AS STRING GET SELF:_cGuid
	PROPERTY Loaded AS LOGIC GET SELF:_lLoaded
	PROPERTY Saved AS LOGIC GET SELF:_lSaved
	PROPERTY References AS List<ApplicationDescriptor> GET SELF:_aReferences
	PROPERTY GACReferences AS List<STRING> GET SELF:_aGACReferences
	PROPERTY BrowseReferences AS List<STRING> GET SELF:_aBrowseReferences
	PROPERTY ProjectReferences AS List<STRING> GET SELF:_aProjectReferences

//	PROPERTY AppFile_XIDE AS STRING GET SELF:_cAppFile_XIDE
	PROPERTY AppFile_XIDE AS STRING GET SELF:AppFolder + "\" + SELF:PathValidName + ".viapp"
//	PROPERTY AppFile_VS AS STRING GET SELF:_cAppFile_VS
	PROPERTY AppFile_VS AS STRING GET SELF:AppFolder + "\" + SELF:PathValidName + ".xsproj"
		
	PROPERTY OptionOverflow AS LOGIC GET SELF:_lOptionOverflow
	PROPERTY OptionIntDiv AS LOGIC GET SELF:_lOptionIntDiv

	PROPERTY Type AS ApplicationType GET SELF:_eType
	PROPERTY Project AS VOProjectDescriptor GET SELF:_oProject
				
	EXPORT xPortOptions AS xPorterOptions
	
	PROPERTY AppFolder AS STRING GET SELF:Project:ProjectFolder + "\" + SELF:PathValidName
	
	PROPERTY ModulesCount AS INT GET SELF:_aModules:Count
		
/*	METHOD SetProject(oProject AS VOProjectDescriptor) AS VOID
		SELF:_oProject := oProject
	RETURN*/

	METHOD AddRuntimeReference(cReference AS STRING) AS VOID
		LOCAL lAddAsDll := FALSE AS LOGIC
		TRY
			IF .not. String.IsNullOrWhiteSpace(RuntimeFolder) .and. Directory.Exists(RuntimeFolder)
				LOCAL cDll AS STRING
				cDll := RuntimeFolder
				IF .not. cDll:EndsWith("\")
					cDll += "\"
				END IF
				cDll += cReference
				IF .not. cReference:ToLower():EndsWith(".dll")
					cDll += ".dll"
				END IF
				IF File.Exists(cDll)
					SELF:_aBrowseReferences:Add(cDll)
					lAddAsDll := TRUE
				END IF
			END IF
		END TRY
		IF .not. lAddAsDll
			SELF:_aGACReferences:Add(cReference)
		END IF
	RETURN

	METHOD ResolveReferences() AS VOID
		LOCAL nReference := 0 AS INT
		DO WHILE nReference < SELF:_aProjectReferences:Count
			LOCAL cLibrary AS STRING
			cLibrary := SELF:_aProjectReferences[nReference]
			LOCAL oLibrary AS ApplicationDescriptor
			oLibrary := SELF:Project:FindAppByName(cLibrary)
			IF oLibrary != NULL
				SELF:AddReference(oLibrary)
				SELF:_aProjectReferences:RemoveAt(nReference)
			ELSE
				nReference ++
			ENDIF
		END DO
	RETURN
	
	METHOD AddReference(oLibrary AS ApplicationDescriptor) AS VOID
		SELF:_aReferences:Add(oLibrary)
	RETURN
	
	METHOD AddFunction(cFunction AS STRING) AS VOID
		SELF:_aFunctions:Add(cFunction:ToUpper())
	RETURN
	METHOD ContainsFunction(cFunction AS STRING) AS LOGIC
	RETURN SELF:_aFunctions:Contains(cFunction:ToUpper())

	METHOD ContainsModuleName(cModuleName AS STRING) AS LOGIC
		cModuleName := cModuleName:ToUpper()
		FOREACH oModule AS ModuleDescriptor IN SELF:_aModules
			IF oModule:Name:ToUpper() == cModuleName
				RETURN TRUE
			END IF
		NEXT
	RETURN FALSE
	
	METHOD HasDefine(cDefine AS STRING) AS LOGIC
		FOREACH oModule AS ModuleDescriptor IN SELF:_aModules
			IF oModule:HasDefine(cDefine)
				RETURN TRUE
			END IF
		NEXT
	RETURN FALSE
	METHOD GetDefineValue(cDefine AS STRING) AS STRING
		FOREACH oModule AS ModuleDescriptor IN SELF:_aModules
			IF oModule:HasDefine(cDefine)
				RETURN oModule:GetDefineValue(cDefine)
			END IF
		NEXT
	RETURN NULL

	METHOD RegisterCallback(cFunction AS STRING) AS VOID
		IF cFunction:ToUpper() == "__CONNECTFUNC" .or. cFunction:ToUpper() == "WINSOCKEXIT"
			RETURN
		END IF
		SELF:_aCallbacks:Add(cFunction:ToUpper())
	RETURN
	METHOD ContainsCallback(cFunction AS STRING) AS LOGIC
	RETURN SELF:_aCallbacks:Contains(cFunction:ToUpper())
	

	STATIC METHOD CreateFromAef(cAefFile AS STRING , oProject AS VOProjectDescriptor , cAppName AS STRING) AS ApplicationDescriptor
		LOCAL oApp AS ApplicationDescriptor
		LOCAL oAef AS Fab_VO_Entities.FabAEFFile
		LOCAL oMef AS Fab_VO_Entities.FabMEFFile
		LOCAL oEntity AS Fab_VO_Entities.FabMEFEntity
		LOCAL oSource AS System.Text.StringBuilder

		oAef := Fab_VO_Entities.FabAEFFile{cAefFile}
		
		IF String.IsNullOrEmpty(cAppName)
			cAppName := oAef:Name
		END IF
		xPorter.uiForm:SetProgressText("Reading application " + cAppName)
		
		oApp := ApplicationDescriptor{cAppName , oProject}
		DO CASE
		CASE oAef:IsDLL .or. oAef:IsLibrary
			oApp:_eType := ApplicationType.Library
		CASE oAef:IsConsole
			oApp:_eType := ApplicationType.Exe
		OTHERWISE
			oApp:_eType := ApplicationType.WinExe
		END CASE
		
		oApp:_lOptionOverflow := oAef:lOptionOverflow
		oApp:_lOptionIntDiv := oAef:lOptionIntegerDivisions
		
//		LOCAL lGUI,lWin32API,lAnySDK AS LOGIC
		LOCAL lWin32API :=FALSE AS LOGIC
		
		FOREACH cLibrary AS STRING IN oAef:LibraryNameListList
			LOCAL cGAC := NULL AS STRING
			SWITCH cLibrary
			CASE "Console Classes"
				cGAC := "VulcanVOConsoleClasses"
			CASE "Terminal Lite"
				cGAC := "VulcanVOConsoleClasses"
			CASE "GUI Classes"
				cGAC := "VulcanVOGUIClasses"
//				lGUI := TRUE
			CASE "Internet"
				cGAC := "VulcanVOInternetClasses"
			CASE "RDD Classes"
				cGAC := "VulcanVORDDClasses"
			CASE "Report Classes"
				cGAC := "VulcanVOReportClasses"
			CASE "SQL Classes"
				cGAC := "VulcanVOSQLClasses"
			CASE "System Classes"
				cGAC := "VulcanVOSystemClasses"
			CASE "Win32 API Library"
				cGAC := "VulcanVOWin32APILibrary"
				lWin32API := TRUE
			OTHERWISE
				oApp:ProjectReferences:Add(cLibrary)
			END SWITCH
			IF cGAC != NULL
//				lAnySDK := TRUE
				oApp:AddRuntimeReference(cGAC)
			END IF
		NEXT
		
		// looks like VO always uses Win32 library and its defines...argh!
//		IF lGUI .and. .not. lWin32API
//		IF lAnySDK .and. .not. lWin32API
			IF .not. lWin32API
			oApp:AddRuntimeReference("VulcanVOWin32APILibrary")
			ENDIF
//		END IF
		
//		IF lAnySDK
/*			IF .not. String.IsNullOrWhiteSpace(SDKDefines_FileName) .and. File.Exists(SDKDefines_FileName)
				oApp:BrowseReferences:Add(SDKDefines_FileName)
			END IF*/
			oApp:BrowseReferences:Add("%SDKDefinesFilename%")
//		END IF

		oSource := System.Text.StringBuilder{}
		LOCAL oMefList AS List<Fab_VO_Entities.FabMEFFile>
		oMefList := oAef:ModuleListList
		LOCAL nCount AS INT
		nCount := oMefList:Count
//		xPorter.uiForm:SetProgressBarRange(nCount)
		FOR LOCAL n := 0 AS INT UPTO oMefList:Count - 1
			oMef := oMefList[n]
			xPorter.Message(System.String.Format("Reading {0}, Module {1}/{2}: {3}" , cAppName , n+1 , nCount , oMef:Name))
			LOCAL oModule := NULL AS ModuleDescriptor
			IF oMef:IsExternal
				IF File.Exists(oMef:ExternalFile)
//					oApp:AddModule(oMef:Name , File.ReadAllText( oMef:ExternalFile ) )
					oModule := oApp:AddModule(oMef:Name , oMef:ExternalSource )
				END IF
			ELSE
				oSource:Clear()
				FOR LOCAL m := 0 AS INT UPTO oMef:EntityListList:Count - 1
					oEntity := oMef:EntityListList[m]
					oSource:Append(oEntity:Source + e"\r\n")
				NEXT
				oModule := oApp:AddModule(oMef:Name , oSource:ToString())
			END IF
			
			IF oModule != NULL
				FOREACH oDesigner AS Designer IN oMef:aDesigners
					oModule:Designers:Add(oDesigner)
				NEXT
			END IF
			
			oMef:Close()
//			xPorter.uiForm:AdvanceProgressbar()
		NEXT
		oAef:Close()
	RETURN oApp

	METHOD AddModuleFromFile(cFileName AS STRING) AS ModuleDescriptor
		LOCAL cName AS STRING
		LOCAL nIndex AS INT
		cName := FileInfo{cFileName}:Name
		nIndex := cName:LastIndexOf('.')
		IF nIndex != -1
			cName := cName:Substring(0 , nIndex)
		END IF
	RETURN SELF:AddModule(cName , File.ReadAllLines(cFileName))

	METHOD AddModule(cName AS STRING , cCode AS STRING) AS ModuleDescriptor
	RETURN SELF:AddModule(cName , cCode:Split(<STRING>{e"\r\n" , e"\r" , e"\n"} , StringSplitOptions.None))
	METHOD AddModule(cName AS STRING , aCode AS IEnumerable) AS ModuleDescriptor
		LOCAL oModule AS ModuleDescriptor
		DO WHILE SELF:ContainsModuleName(cName)
			cName := cName + "_" // happens when creating the SDK_Defines library, where modules from all libraries are added into a single one
		END DO
		oModule := ModuleDescriptor{cName , SELF , aCode}
		SELF:_aModules:Add(oModule)
		SELF:_lLoaded := TRUE
	RETURN oModule

	METHOD LoadFromFolder(cFolder AS STRING) AS VOID
		LOCAL aFiles AS STRING[]

		aFiles := Directory.GetFiles(cFolder , "*.prg")
		FOREACH cFileName AS STRING IN aFiles
			IF cFileName:ToUpper():EndsWith("Funcs Coder.prg":ToUpper()) .and. File.Exists(cFolder + "\FuncsDeCoder.prg")
				LOOP
			END IF
			SELF:AddModuleFromFile(cFileName)
		NEXT
		
		SELF:_lLoaded := TRUE
		
	RETURN
	
	METHOD Generate() AS VOID
		LOCAL cFolder AS STRING
		cFolder := SELF:AppFolder
		
		xPorter.Message("xPorting application", SELF:Name)
		
		SELF:ResolveReferences()
		
		Directory.CreateDirectory(cFolder)
		
		FOREACH oModule AS ModuleDescriptor IN SELF:_aModules
			
			xPorter.uiForm:AdvanceProgressbar()
			
			IF .not. xPorter.AllowModule(oModule)
				LOOP
			END IF
		
			LOCAL oCode AS OutputCode
			oCode := oModule:Generate()

			IF oModule:Generated
				xPorter.Message("  Generating module :" , oModule:Name)
				File.WriteAllLines(cFolder + "\" + oModule:PathValidName + ".prg" , oCode:GetContents() , System.Text.Encoding.Default)
			END IF
			
			FOREACH oDesigner AS Designer IN oModule:Designers
				IF oDesigner:MustExport
					LOCAL cBinary AS STRING
					LOCAL cModule AS STRING
					LOCAL cPrg AS STRING
					cModule := cFolder + "\" + oModule:PathValidName
					cPrg := cModule + ".prg"
					cBinary := cModule + "." + oDesigner:FileName
					File.WriteAllBytes(cBinary , oDesigner:Bytes)
					IF xPorter.ExportXideBinaries
						TRY
							DO CASE
							CASE oDesigner:Type == 10
								VOWindowEditor.ProjectImportVNFrm(cPrg , cBinary)
							CASE oDesigner:Type == 16
								VOMenuEditor.ProjectImportVNMnu(cPrg , cBinary)
							END CASE
						END TRY
					END IF
				ENDIF
			NEXT

			IF .not. xPorter.Options:ExportOnlyDefines
				LOCAL aResources AS SortedList<STRING,OutputCode>
				aResources := oModule:GenerateResources()
				IF aResources:Count != 0
					LOCAL cResFileName AS STRING
					LOCAL oXideResources, oWedResources AS OutputCode
					oXideResources := OutputCode{}
					oWedResources := OutputCode{}
					FOREACH oPair AS KeyValuePair<STRING , OutputCode> IN aResources

						// For VS:
						LOCAL cName, cUpperName AS STRING
						cName := oPair:Key
						cUpperName := cName:ToUpperInvariant()

						#warning WTF, the following never executes!
						DO CASE
						CASE cUpperName:StartsWith("IDM_")
							cName := cName:Substring(4)
							cUpperName := cName:ToUpperInvariant()
						CASE cUpperName:ToUpper():StartsWith("IDA_")
							cName := cName:Substring(4) + "_Accelerator"
							cUpperName := cName:ToUpperInvariant()
						END CASE

						cResFileName := oModule:PathValidName + "." + cName + ".rc"
						File.WriteAllLines(cFolder + "\" + cResFileName , oPair:Value:GetContents() , System.Text.Encoding.Default)
						oModule:AddVSrc(cResFileName)

						// For XIDE:
						LOCAL lWedRc := FALSE AS LOGIC
						FOREACH oDesigner AS Designer IN oModule:Designers
							IF cUpperName == oDesigner:Name:ToUpperInvariant() .or. cUpperName == oDesigner:Name:ToUpperInvariant() + "_ACCELERATOR"
								lWedRc := TRUE
								EXIT
							END IF
						NEXT
						IF lWedRc
							oWedResources:Combine(oPair:Value)
						ELSE
							oXideResources:Combine(oPair:Value)
						END IF

					NEXT

					// For XIDE:
					IF xPorter.ExportXideBinaries
						IF .not. oXideResources:IsEmpty()
							cResFileName := oModule:PathValidName + ".rc"
							File.WriteAllLines(cFolder + "\" + cResFileName , oXideResources:GetContents() , System.Text.Encoding.Default)
							oModule:AddXIDErc(cResFileName)
						END IF

						IF .not. oWedResources:IsEmpty()
							cResFileName := oModule:PathValidName + ".prg.rc"
							File.WriteAllLines(cFolder + "\" + cResFileName , oWedResources:GetContents() , System.Text.Encoding.Default)
						END IF
					ELSE
						oXideResources:Combine(oWedResources)
						cResFileName := oModule:PathValidName + ".rc"
						File.WriteAllLines(cFolder + "\" + cResFileName , oXideResources:GetContents() , System.Text.Encoding.Default)
						oModule:AddXIDErc(cResFileName)
					END IF
				END IF
			END IF

		NEXT
		
		
		SELF:_lSaved := TRUE
		
		SELF:CreateAppFile(cFolder , TRUE)
		SELF:CreateAppFile(cFolder , FALSE)
	RETURN
	
	METHOD CreateAppFile(cFolder AS STRING , lXide AS LOGIC) AS VOID
		LOCAL oTemplate AS StreamReader
		LOCAL oOutput AS StreamWriter
		LOCAL cTemplate AS STRING
		LOCAL cAppTypeTag AS STRING

		IF lXide
			cAppTypeTag := ((INT)SELF:_eType):ToString()
		ELSE
			cAppTypeTag := SELF:_eType:ToString()
		END IF

		IF .not. xPorter.OverWriteProjectFiles
			IF lXide
				IF File.Exists(SELF:AppFile_XIDE)
					RETURN
				END IF
			ELSE
				IF File.Exists(SELF:AppFile_VS)
					RETURN
				END IF
			END IF
		ENDIF

		IF lXide
			oTemplate := StreamReader{Application.StartupPath + "\templates\template_XIDE.xiapp" , System.Text.Encoding.Default , TRUE} 
			oOutput := StreamWriter{SELF:AppFile_XIDE , FALSE , System.Text.Encoding.Default}
		ELSE
			oTemplate := StreamReader{Application.StartupPath + "\templates\template_VS.xsproj" , TRUE} 
			oOutput := StreamWriter{SELF:AppFile_VS , FALSE , System.Text.Encoding.UTF8}
		END IF
		
		DO WHILE oTemplate:Peek() != -1
			cTemplate := oTemplate:ReadLine()
			DO CASE
			CASE cTemplate:StartsWith(";")
				LOOP
			CASE cTemplate == "%appfiles%"
				FOREACH oModule AS ModuleDescriptor IN SELF:_aModules
					IF .not. oModule:Generated
						LOOP
					END IF
					LOCAL cName AS STRING
					cName := oModule:PathValidName + ".prg"
					IF lXide
						oOutput:WriteLine("File = %AppPath%\" + cName)
						oOutput:WriteLine("FileGUID = " + NewGuid())
						oOutput:WriteLine("FileType = Code")
					ELSE
						oOutput:WriteLine(String.Format(e"<Compile Include=\"{0}\">" , cName))
						oOutput:WriteLine("  <SubType>Code</SubType>")
						oOutput:WriteLine("</Compile>")
					END IF
					
					IF lXide
						IF oModule:XIDErc != NULL
							oOutput:WriteLine("NativeResourceFile = External , %AppPath%\" + oModule:XIDErc)
							oOutput:WriteLine("ResourceFileGuid = " + NewGuid())
						END IF
					ELSE
						IF oModule:VSrc:Count != 0
							FOREACH cResFileName AS STRING IN oModule:VSrc
								oOutput:WriteLine(String.Format(e"<NativeResource Include=\"{0}\">" , cResFileName))
								oOutput:WriteLine(String.Format(e"  <DependentUpon>{0}</DependentUpon>" , cName))
								oOutput:WriteLine(String.Format(e"</NativeResource>" , ""))
							NEXT
						END IF
						IF oModule:Designers:Count != 0
							FOREACH oDesigner AS Designer IN oModule:Designers
								IF oDesigner:MustExport
									oOutput:WriteLine(String.Format(e"<VOBinary Include=\"{0}\">" ,  oModule:PathValidName + "." +oDesigner:Name+oDesigner:Extension))
									oOutput:WriteLine(String.Format(e"  <DependentUpon>{0}</DependentUpon>" , cName))
									oOutput:WriteLine(String.Format(e"</VOBinary>" , ""))
								ENDIF
							NEXT
						END IF
					END IF
					
				NEXT
			CASE cTemplate == "%references%"
				FOREACH oRef AS ApplicationDescriptor IN SELF:_aReferences
					IF lXide
						oOutput:WriteLine(String.Format("ReferenceProject = {0},1,0,{1}" , oRef:Guid , oRef:PathValidName ) )
					ELSE
//						#warning make path relative
						oOutput:WriteLine(String.Format(e"    <ProjectReference Include=\"{0}\">" , GetRelativePath(cFolder , oRef:AppFile_VS) ) )
						oOutput:WriteLine(String.Format(e"      <Name>{0}</Name>" , oRef:PathValidName ) )
						oOutput:WriteLine(String.Format(e"      <Project>{{{0}}}</Project>" , oRef:Guid ) )
						oOutput:WriteLine(String.Format(e"      <Private>True</Private>"))
						oOutput:WriteLine(String.Format(e"    </ProjectReference>"))
/*
;    <ProjectReference Include="..\ClassLibrary1\ClassLibrary1.xsproj">
;      <Name>ClassLibrary1</Name>
;      <Project>{6fd2e2ba-8a74-4c73-9692-111e4a30635a}</Project>
;      <Private>True</Private>
;    </ProjectReference>
*/
					END IF
				NEXT

				FOREACH cReference AS STRING IN SELF:_aGACReferences
					IF lXide
						oOutput:WriteLine(String.Format("ReferenceGAC = CLR4,{0},1,0, 0.0.0.0" , cReference ) )
					ELSE
						oOutput:WriteLine(String.Format(e"    <Reference Include=\"{0}\">" , cReference ) )
						oOutput:WriteLine(String.Format(e"      <SpecificVersion>false</SpecificVersion>") )
						oOutput:WriteLine(String.Format(e"    </Reference>"))
					ENDIF
				NEXT
				
				FOREACH cReference AS STRING IN SELF:_aProjectReferences
					IF lXide
						oOutput:WriteLine(String.Format("ReferenceProject = ,1,0,{0}" , cReference ) )
					ELSE
/*						oOutput:WriteLine(String.Format('    <Reference Include="{0}">' , cReference ) )
						oOutput:WriteLine(String.Format('      <SpecificVersion>false</SpecificVersion>') )
						oOutput:WriteLine(String.Format('    </Reference>'))*/
					ENDIF
				NEXT
				
				FOREACH cReference AS STRING IN SELF:_aBrowseReferences
					LOCAL cFileName AS STRING
					cFileName := cReference
					IF cFileName == "%SDKDefinesFilename%"
						cFileName := SELF:Project:Solution_SDKDefines_Filename
					END IF
					IF lXide
//						#warning XIDE 1.09 did not support relative <browse> references
						oOutput:WriteLine(String.Format("ReferenceBrowse = {0},1,1" , cFileName:Replace(SELF:Project:ProjectFolder , "%ProjectPath%") ) )
//						oOutput:WriteLine(String.Format("ReferenceBrowse = {0},1,1" , cFileName ) )
					ELSE
						LOCAL cName AS STRING
						cName := FileInfo{cFileName}:Name
						oOutput:WriteLine(String.Format(e"    <Reference Include=\"{0}\">" , cName ) )
						oOutput:WriteLine(String.Format(e"      <Name>{0}</Name>" , cName) )
						oOutput:WriteLine(String.Format(e"      <AssemblyName>{0}</AssemblyName>" , cName + ".dll") )
						oOutput:WriteLine(String.Format(e"      <SpecificVersion>false</SpecificVersion>") )
						oOutput:WriteLine(String.Format(e"      <Private>True</Private>") )
//						#warning make path relative
						oOutput:WriteLine(String.Format(e"      <HintPath>{0}</HintPath>" , GetRelativePath(cFolder , cFileName) ) )
						oOutput:WriteLine(String.Format(e"    </Reference>"))
						/*
    <Reference Include="SDK_Defines, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null">
      <Name>SDK_Defines</Name>
      <AssemblyName>SDK_Defines.dll</AssemblyName>
      <SpecificVersion>False</SpecificVersion>
      <Private>True</Private>
      <HintPath>..\..\..\..\..\..\..\xSharp\Test\SDK_Defines.dll</HintPath>
    </Reference>
						*/
					ENDIF
				NEXT
				
			OTHERWISE
				cTemplate := cTemplate:Replace("%appname%" , SELF:PathValidName)
				cTemplate := cTemplate:Replace("%rootnamespace%", SELF:PathValidName:Replace(" ",""))
				cTemplate := cTemplate:Replace("%apptype%" , cAppTypeTag)
				
				IF cTemplate:Contains("%option_overflow%") .or. cTemplate:Contains("%option_intdiv%")
					cTemplate := cTemplate:Replace("%option_overflow%" , iif(SELF:OptionOverflow , iif(lXide , "1" , "true") , iif(lXide , "0" , "false") ) )
					cTemplate := cTemplate:Replace("%option_intdiv%" , iif(SELF:OptionIntDiv , iif(lXide , "1" , "true") , iif(lXide , "0" , "false") ) )
				END IF
				
//				#warning make path relative
//				cTemplate := cTemplate:Replace("%appfolder%" , cFolder)
				IF cTemplate:Contains("%appfolder%")
					IF lXide
						cTemplate := cTemplate:Replace("%appfolder%" , cFolder:Replace(SELF:Project:ProjectFolder , "%ProjectPath%"))
					END IF
				END IF
				cTemplate := cTemplate:Replace("%appguid%" , SELF:Guid)
				IF cTemplate:Contains("%newguid%")
					cTemplate := cTemplate:Replace("%newguid%" , NewGuid())
				END IF
				oOutput:WriteLine(cTemplate)
			ENDCASE
		END DO
		
		oTemplate:Close()
		oOutput:Close()
	RETURN
	
	
	METHOD ClassExistsInOtherModules(cClass AS STRING , oExceptModule AS ModuleDescriptor) AS LOGIC
		cClass := cClass:ToUpper()
		FOREACH oModule AS ModuleDescriptor IN SELF:_aModules
			IF oModule != oExceptModule
				FOREACH oClass AS ClassDescriptor IN oModule:Classes
					IF oClass:Name:ToUpper() == cClass
						RETURN TRUE
					END IF
				NEXT
			END IF
		NEXT
	RETURN FALSE
	
	METHOD ClassDeclarationExists(cClass AS STRING) AS LOGIC
		cClass := cClass:ToUpper()
		FOREACH oModule AS ModuleDescriptor IN SELF:_aModules
			FOREACH oClass AS ClassDescriptor IN oModule:Classes
				IF oClass:Name:ToUpper() == cClass .and. oClass:HasDeclaration
					RETURN TRUE
				END IF
			NEXT
		NEXT
	RETURN FALSE
	
	METHOD ConstructorExists(cClass AS STRING) AS LOGIC
		FOREACH oModule AS ModuleDescriptor IN SELF:_aModules
			IF oModule:ConstructorExists(cClass)
				RETURN TRUE
			END IF
		NEXT
	RETURN FALSE
	
	METHOD Analyze() AS VOID
		FOREACH oModule AS ModuleDescriptor IN SELF:_aModules
			xPorter.Message("Analyzing module" , oModule:Name)
			xPorter.uiForm:AdvanceProgressbar()
			oModule:Analyze()
		NEXT

		IF xPorter.Options:AdjustCallbackFunctions
			FOREACH oModule AS ModuleDescriptor IN SELF:_aModules
				xPorter.Message("Searching for callbacks in module" , oModule:Name)
				oModule:SearchForCallbacks()
			NEXT
		END IF
	RETURN
	
END CLASS


CLASS ModuleDescriptor
	PROTECT _cName AS STRING
	PROTECT _oApp AS ApplicationDescriptor
	PROTECT _aLines AS List<LineObject>
	PROTECT _aClasses AS List<ClassDescriptor>
	PROTECT _aEntities AS List<EntityDescriptor>
	PROTECT _aConstructors AS List<STRING>
	PROTECT _aDefines AS SortedList<STRING , STRING>
	PROTECT _lGenerated AS LOGIC
	PROTECT _aVSrc AS List<STRING>
	PROTECT _cXIDErc AS STRING

	PROTECT _aDesigners AS List<Designer>

	CONSTRUCTOR(cName AS STRING , oApp AS ApplicationDescriptor , aCode AS IEnumerable)
		SELF:_cName := cName
		SELF:_aLines := List<LineObject>{}
		SELF:_oApp := oApp
		SELF:_aClasses := List<ClassDescriptor>{}
		SELF:_aEntities := List<EntityDescriptor>{}
		SELF:_aDefines := SortedList<STRING , STRING>{}
		SELF:_aConstructors := List<STRING>{}
		FOREACH oLine AS OBJECT IN aCode
			SELF:_aLines:Add(LineObject{oLine:ToString()})
		NEXT
		SELF:_aVSrc := List<STRING>{}
		SELF:_cXIDErc := NULL
		
		SELF:_aDesigners := List<Designer>{}
	RETURN
	
	PROPERTY Name AS STRING GET SELF:_cName
	PROPERTY PathValidName AS STRING GET MakePathLegal( SELF:_cName )
	PROPERTY Classes AS List<ClassDescriptor> GET SELF:_aClasses
	PROPERTY Application AS ApplicationDescriptor GET SELF:_oApp
	PROPERTY Generated AS LOGIC GET SELF:_lGenerated
	PROPERTY VSrc AS List<STRING> GET SELF:_aVSrc
	PROPERTY XIDErc AS STRING GET SELF:_cXIDErc

	PROPERTY Designers AS List<Designer> GET SELF:_aDesigners
		
	METHOD HasDefine(cDefine AS STRING) AS LOGIC
	RETURN SELF:_aDefines:ContainsKey(cDefine)
	METHOD GetDefineValue(cDefine AS STRING) AS STRING
	RETURN SELF:_aDefines[cDefine]
	
	METHOD FindClass(cName AS STRING) AS ClassDescriptor
		FOREACH oClass AS ClassDescriptor IN SELF:_aClasses
			IF oClass:Name:ToUpper() == cName:ToUpper()
				RETURN oClass
			END IF
		NEXT
	RETURN NULL
	METHOD ConstructorExists(cClass AS STRING) AS LOGIC
		cClass := cClass:ToUpper()
		FOREACH cConClass AS STRING IN SELF:_aConstructors
			IF cConClass == cClass
				RETURN TRUE
			END IF
		NEXT
	RETURN FALSE
	
	METHOD Analyze() AS VOID
		LOCAL oBuffer AS Xide.EditorBuffer
		oBuffer := Xide.EditorBuffer.CreateBuffer(Xide.FileType.VO , SELF:_aLines)
		oBuffer:FullParse()
		
		LOCAL oCurrentEntity := NULL AS EntityDescriptor
		LOCAL nLine := 0 AS INT
		FOREACH oLine AS LineObject IN SELF:_aLines
			nLine ++
			IF oLine:oEntity != NULL
				IF .not. xPorter.AllowEntity(oLine:oEntity , SELF)
					oCurrentEntity := NULL
					LOOP
				END IF
				
				IF oLine:oEntity:eType == EntityType._Define
					IF SELF:_aDefines:ContainsKey(oLine:oEntity:cName:ToUpper())
						IF xPorter.Options:IgnoreDuplicateDefines
							oCurrentEntity := NULL
							LOOP
						END IF
					ELSE
						LOCAL nIndex AS INT
						nIndex := oLine:LineText:IndexOf(":=")
						IF nIndex != -1
							SELF:_aDefines:Add(oLine:oEntity:cName:ToUpper() , StripCommentsFromCode( oLine:LineText:Substring(nIndex+2)):Trim() )
						END IF
					END IF
				END IF
				IF oLine:oEntity:eType == EntityType._Function .or. oLine:oEntity:eType == EntityType._Procedure
					SELF:_oApp:AddFunction(oLine:oEntity:cName:ToUpper())
				ENDIF

				IF xPorter.Options:AddMissingConstrutors
					IF oLine:oEntity:eType == EntityType._Method .and. oLine:oEntity:cName:ToUpper() == "INIT"
						SELF:_aConstructors:Add(oLine:oEntity:cShortClassName:ToUpper())
					ENDIF
				ENDIF
				
				oCurrentEntity := EntityDescriptor{oLine:oEntity , nLine , SELF}
				IF oCurrentEntity:IsClassOrMember
					LOCAL oClass AS ClassDescriptor
					oClass := SELF:FindClass(oLine:oEntity:cShortClassName)
					IF oClass == NULL
						oClass := ClassDescriptor{oLine:oEntity:cShortClassName , SELF}
						SELF:_aClasses:Add(oClass)
					END IF
					oClass:AddEntity(oCurrentEntity)
				ELSE
					SELF:_aEntities:Add(oCurrentEntity)
				END IF
			END IF
			IF oCurrentEntity != NULL
				oCurrentEntity:AddLine(oLine)
			END IF
		NEXT

	RETURN
	
	METHOD SearchForCallbacks() AS VOID
		LOCAL oBuffer AS Xide.EditorBuffer
		oBuffer := Xide.EditorBuffer.CreateBuffer(Xide.FileType.VO , SELF:_aLines)
		oBuffer:FullParse()
		
		FOREACH oLine AS LineObject IN SELF:_aLines
			IF oLine:LineText:Contains("@")
				LOCAL aWords AS List<WordObject>
				LOCAL cPrev := "" AS STRING
				aWords := EntityDescriptor.AnalyzeLine(oLine:LineText)
				FOREACH oWord AS WordObject IN aWords
					IF cPrev == "@"
						IF SELF:_oApp:ContainsFunction(oWord:cWord)
//							MessageBox.Show(oWord:cWord , "Found callback function")
							SELF:_oApp:RegisterCallback(oWord:cWord)
						END IF
					END IF
					cPrev := oWord:cWord
				NEXT
			END IF
		NEXT

	RETURN
	
	METHOD Generate() AS OutputCode
		LOCAL oCode AS OutputCode
		LOCAL oClasses , oDefines , oTextblocks , oGlobals , oFuncs , oRest AS OutputCode
		
		STATIC oConvertFromCodePageToCodePage := NULL AS EntityDescriptor
		
		oClasses := OutputCode{}
		oDefines := OutputCode{}
		oTextblocks := OutputCode{}
		oGlobals := OutputCode{}
		oFuncs := OutputCode{}
		oRest := OutputCode{}
		
		FOREACH oClass AS ClassDescriptor IN SELF:_aClasses
			oClasses:Combine(oClass:Generate())
		NEXT
		
		FOREACH oEntity AS EntityDescriptor IN SELF:_aEntities
			
			DO CASE
			CASE oEntity:IsClassOrMember
				NOP
			CASE oEntity:Type == EntityType._Resource
				NOP
			CASE oEntity:Type == EntityType._TextBlock
				oTextblocks:Combine(oEntity:Generate())
			CASE oEntity:Type == EntityType._Define
				IF xPorter.Options:ExportOnlyDefines .and. oEntity:IsStatic
					LOOP
				END IF
				oDefines:Combine(oEntity:Generate())
			CASE oEntity:Type == EntityType._Global
				oGlobals:Combine(oEntity:Generate())
			CASE oEntity:Type == EntityType._Function .or. oEntity:Type == EntityType._Procedure
				IF SELF:_oApp:ContainsCallback(oEntity:Name)
					oFuncs:Combine(oEntity:GenerateDelegateFromFunction())
				END IF
				oFuncs:Combine(oEntity:Generate())
			OTHERWISE
				oRest:Combine(oEntity:Generate())
			END CASE
		NEXT
		
		oCode := OutputCode{}

		IF xPorter.Options:ExportOnlyDefines
			oCode:Combine(oDefines)
		ELSE
			oCode:Combine(oTextblocks)
			IF .not. oDefines:IsEmpty()
				oCode:AddLine("#region DEFINES")
				oCode:Combine(oDefines)
				oCode:AddLine("#endregion")
				oCode:AddLine("")
			END IF
			oCode:Combine(oGlobals)
			oCode:Combine(oClasses)
			oCode:Combine(oFuncs)
			oCode:Combine(oRest)
		END IF

		IF .not. (xPorter.Options:DontGenerateEmptyFiles .and. oCode:IsEmpty() .and. .not. SELF:HasResources())
			SELF:_lGenerated := TRUE
		ENDIF
		
	RETURN oCode
	
	METHOD HasResources() AS LOGIC
		FOREACH oEntity AS EntityDescriptor IN SELF:_aEntities
			DO CASE
			CASE oEntity:Type == EntityType._Resource
				IF oEntity:Name:ToUpper() == "VS_VERSION_INFO"
					LOOP
				END IF
				RETURN TRUE
			END CASE
		NEXT
	RETURN FALSE
	METHOD GenerateResources() AS SortedList<STRING,OutputCode>
		LOCAL oCode AS OutputCode
		LOCAL aResources AS SortedList<STRING,OutputCode>
		
		aResources := SortedList<STRING,OutputCode>{}
		
		FOREACH oEntity AS EntityDescriptor IN SELF:_aEntities
			DO CASE
			CASE oEntity:Type == EntityType._Resource
				IF oEntity:Name:ToUpper() == "VS_VERSION_INFO"
					LOOP
				END IF
				oCode := oEntity:Generate()
				aResources:Add(oEntity:Name , oCode)
			END CASE
		NEXT
	RETURN aResources
	
	METHOD AddVSrc(cResFileName AS STRING) AS VOID
		SELF:_aVSrc:Add(cResFileName)
	RETURN
	METHOD AddXIDErc(cResFileName AS STRING) AS VOID
		SELF:_cXIDErc := cResFileName
	RETURN
	
END CLASS

CLASS ClassDescriptor
	PROTECT _cName AS STRING
	PROTECT _oDeclaration AS EntityDescriptor
	PROTECT _aMembers AS List<EntityDescriptor>
	PROTECT _oModule AS ModuleDescriptor
	CONSTRUCTOR(cName AS STRING , oModule AS ModuleDescriptor)
		SELF:_cName := cName
		SELF:_oModule := oModule
		SELF:_aMembers := List<EntityDescriptor>{}
	RETURN
	PROPERTY Name AS STRING GET SELF:_cName
	PROPERTY Members AS List<EntityDescriptor> GET SELF:_aMembers
	PROPERTY HasDeclaration AS LOGIC GET SELF:_oDeclaration != NULL
	
	METHOD AddEntity(oEntity AS EntityDescriptor) AS VOID
		IF oEntity:Type == EntityType._Class
			SELF:_oDeclaration := oEntity
		ELSE
			SELF:_aMembers:Add(oEntity)
		END IF
	RETURN

	METHOD Generate() AS OutputCode
		LOCAL oCode AS OutputCode
		LOCAL lPartial AS LOGIC
		lPartial := SELF:_oModule:Application:ClassExistsInOtherModules(SELF:Name , SELF:_oModule)
		IF xPorter.Options:AllClassesPartial
			lPartial := TRUE
		END IF
		
		oCode := OutputCode{}

		IF SELF:_cName == "_CODEBLOCK"
			RETURN oCode
		END IF
		
		IF SELF:_oDeclaration != NULL
			SELF:_oDeclaration:PartialClass := lPartial
			oCode:Combine(SELF:_oDeclaration:Generate())
			IF xPorter.Options:AddMissingConstrutors
				IF SELF:_oDeclaration:HasInherit .and. .not. SELF:_oModule:Application:ConstructorExists(SELF:Name)
//					oCode:AddLine("")
					oCode:AddLine("// constructor inserted by xPorter, remove superfluous arguments")
					oCode:AddLine("CONSTRUCTOR(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9) CLIPPER")
					oCode:AddLine("SUPER(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9)")
//					oCode:AddLine("")
				END IF
			END IF
		ELSE
			IF SELF:_cName:ToUpper() == "APP"
				SELF:_cName := "X" + SELF:_cName
				
				oCode:AddLine(e"[STAThread];")
				oCode:AddLine(e"FUNCTION Start() AS INT")
				oCode:AddLine(e"\tLOCAL oXApp AS " + SELF:_cName)
				oCode:AddLine(e"\tTRY")
				oCode:AddLine(e"\t\toXApp := " + SELF:_cName + "{}")
				oCode:AddLine(e"\t\toXApp:Start()")
				oCode:AddLine(e"\tCATCH e AS Exception")
				oCode:AddLine(e"\t\tLOCAL cMessage AS STRING")
				oCode:AddLine(e"\t\tcMessage := e:Message")
				oCode:AddLine(e"\t\tDO WHILE e:InnerException != NULL_OBJECT")
				oCode:AddLine(e"\t\t\te := e:InnerException")
				oCode:AddLine(e"\t\t\tcMessage += CRLF+e:Message")
				oCode:AddLine(e"\t\tENDDO")
				oCode:AddLine(e"\t\tErrorBox{NIL, cMessage}:Show()")
				oCode:AddLine(e"\tEND TRY")
				oCode:AddLine(e"RETURN 0")
				oCode:AddLine(e"")
				oCode:AddLine(iif(lPartial , "PARTIAL " , "") + "CLASS " + SELF:_cName + " INHERIT App")
			ELSE
				IF SELF:_oModule:Application:ClassDeclarationExists(SELF:Name)
					oCode:AddLine(iif(lPartial , "PARTIAL " , "") + "CLASS " + SELF:_cName)
				ELSE
					oCode:AddLine("#warning The following method did not include a CLASS declaration")
					oCode:AddLine(iif(lPartial , "PARTIAL " , "") + "CLASS " + SELF:_cName + "_external_class" + " INHERIT " + SELF:_cName)
				ENDIF
			END IF
		END IF
		FOREACH oEntity AS EntityDescriptor IN SELF:_aMembers
			oCode:Combine(oEntity:Generate())
		NEXT
		oCode:AddLine("END CLASS")
		
	RETURN oCode
	
END CLASS

CLASS EntityDescriptor
	PROTECT _aLines AS List<LineObject>
	PROTECT _eType AS EntityType
	PROTECT _cName AS STRING
	PROTECT _cClass AS STRING
	PROTECT _cInherit AS STRING
	PROTECT _nStartLine AS INT
	PROTECT _oModule AS ModuleDescriptor
	PROTECT _lIsStatic AS LOGIC

	CONSTRUCTOR(oEntity AS EntityObject , nLine AS INT , oModule AS ModuleDescriptor)
		SELF:_aLines := List<LineObject>{}
		SELF:_cName := oEntity:cName
		SELF:_eType := oEntity:eType
		SELF:_cClass := oEntity:cShortClassName
		SELF:_cInherit := oEntity:cInherit
		SELF:_nStartLine := nLine
		SELF:_oModule := oModule

		SELF:_lIsStatic := oEntity:lStatic
	RETURN
	
	PROPERTY Name AS STRING GET SELF:_cName
	PROPERTY Type AS EntityType GET SELF:_eType
	PROPERTY Lines AS List<LineObject> GET SELF:_aLines
	PROPERTY IsClassOrMember AS LOGIC GET;
			SELF:_eType == EntityType._Class .or. SELF:_eType == EntityType._Method .or. ;
			SELF:_eType == EntityType._Access .or. SELF:_eType == EntityType._Assign
	PROPERTY PartialClass AS LOGIC AUTO
	PROPERTY IsStatic AS LOGIC GET SELF:_lIsStatic
	PROPERTY HasInherit AS LOGIC GET .not. (System.String.IsNullOrWhiteSpace(SELF:_cInherit) .or. SELF:_cInherit:ToUpper() == "VOBJECT")
	
	METHOD AddLine(oLine AS LineObject) AS VOID
		SELF:_aLines:Add(oLine)
	RETURN

	METHOD GenerateDelegateFromFunction() AS OutputCode
		LOCAL oCode AS OutputCode
		oCode := OutputCode{}
		FOREACH oLine AS LineObject IN SELF:_aLines
			IF oLine:oEntity == NULL .and. .not. oLine:lInAmpersand
				EXIT
			END IF
			LOCAL cLine AS STRING
			cLine := oLine:LineText
			cLine := cLine:Replace(" STRICT",""):Replace(" PASCAL",""):Replace("_WINCALL",""):Replace(" CALLBACK","")
			IF oLine:oEntity != NULL
				LOCAL nIndex AS INT
				nIndex := cLine:IndexOf('(')
				IF nIndex != -1
					cLine := "DELEGATE " + SELF:_cName + "_delegate" + cLine:Substring(nIndex)
				END IF
			END IF
			oCode:AddLine(cLine)
		NEXT
	RETURN oCode
	
	METHOD Generate() AS OutputCode
		DO CASE
		CASE SELF:_eType == EntityType._TextBlock// .or. SELF:_eType == EntityType._Resource
			RETURN SELF:Generate_Textblock()
		CASE SELF:_eType == EntityType._Resource
			RETURN SELF:Generate_Resource()
		END CASE
		
		LOCAL oCode AS OutputCode
		oCode := OutputCode{}
		
		LOCAL lCommentEntity := FALSE AS LOGIC
		LOCAL oCurrentEntity := NULL AS EntityObject
		
		FOREACH oLine AS LineObject IN SELF:_aLines
			LOCAL lAddEndif := FALSE AS LOGIC
			LOCAL cLine AS STRING

			LOCAL cCallBack := NULL AS STRING
//			cLine := oLine:LineText
			cLine := ConvertLine(oLine , REF cCallBack)
			IF cCallBack != NULL
//				oCode:AddLine("// " + cLine + " // XPORTER:DELEGATE")
				oCode:AddLine("#warning Callback function modified to use a DELEGATE by xPorter. Please review.")
				oCode:AddLine("// " + cLine)
				LOCAL cLocal AS STRING
				cLocal := "o" + cCallBack + "Delegate"
				LOCAL cTemp AS STRING
				cTemp := "STATIC LOCAL " + cLocal + " := " + cCallBack + " AS " + cCallBack + "_Delegate"
				oCode:AddLine(cTemp)
				cTemp := "System.Runtime.InteropServices.Marshal.GetFunctionPointerForDelegate(" + cLocal + ")"
				cLine := cLine:Replace("@" + cCallBack + "()" , cTemp)
				oCode:AddLine(cLine)
				LOOP
			END IF

			IF oLine:oEntity != NULL
				lCommentEntity := FALSE
				oCurrentEntity := oLine:oEntity
			ENDIF

			IF xPorter.Options:RemoveDeclareMethod .and. oCurrentEntity != NULL .and. ;
				oCurrentEntity:eType == EntityType._Class
				#warning DECLAREs recognition needs to be improved as well
				IF cLine:ToUpper():Contains("DECLARE METHOD") .or. ;
					cLine:ToUpper():Contains("DECLARE ACCESS") .or. ;
					cLine:ToUpper():Contains("DECLARE ASSIGN")
					LOOP
				END IF
			END IF

/*			IF (xPorter.Options:RemoveInitAxitReturnClause .or. xPorter.Options:RemoveAssignReturnClause) .and. ;
				oLine:eType == LineType.Return
				IF oCurrentEntity != NULL
					IF (xPorter.Options:RemoveAssignReturnClause .and. oCurrentEntity:eType == EntityType._Assign) .or. ;
						(xPorter.Options:RemoveInitAxitReturnClause .and. oCurrentEntity:eType == EntityType._Method .and. ;
									(oCurrentEntity:cName:ToUpper() == "INIT" .or. oCurrentEntity:cName:ToUpper() == "AXIT"))
						LOCAL nIndex AS INT
						nIndex := cLine:ToUpper():IndexOf("RETURN")
						IF nIndex != -1
							cLine := cLine:Substring(0 , nIndex + 6)
						END IF
					END IF
				END IF
			ENDIF*/

			#warning Ouch, Init/Axit/Super tokens are still specified like that? Need changing ASAP!
			IF xPorter.Options:ChangeInitAxitToCtorDtor
				IF oLine:oEntity == NULL
					LOCAL cUpper AS STRING
					LOCAL nIndex AS INT
					cUpper := cLine:ToUpper()
					nIndex := cUpper:IndexOf("SUPER:INIT(")
					IF nIndex != -1
						cLine := cLine:Substring(0 , nIndex) + "SUPER(" + cLine:Substring(nIndex + 11)
					END IF
				ELSE
					IF oLine:oEntity:eType == EntityType._Method .and. ;
						(oLine:oEntity:cName:ToUpper() == "INIT" .or. oLine:oEntity:cName:ToUpper() == "AXIT")
						LOCAL cUpper AS STRING
						LOCAL nIndex AS INT
						cUpper := cLine:ToUpper()
						IF oLine:oEntity:cName:ToUpper() == "INIT"
							nIndex := cUpper:IndexOf("METHOD INIT")
							IF nIndex != -1
								cLine := cLine:Substring(0 , nIndex) + "CONSTRUCTOR" + cLine:Substring(nIndex + 11)
							END IF
						ELSE
							nIndex := cUpper:IndexOf("METHOD AXIT")
							IF nIndex != -1
								cLine := cLine:Substring(0 , nIndex) + "DESTRUCTOR" + cLine:Substring(nIndex + 11)
							END IF
						END IF
					END IF
				END IF
			ENDIF
			
//			cLine := cline:Replace(" _code " , " @@_code ")
			
			IF lCommentEntity
				oCode:AddLine("// " + cLine)
			ELSE
				IF SELF:_eType == EntityType._Define .and. String.IsNullOrWhiteSpace(cLine)
					NOP
				ELSE
					oCode:AddLine(cLine)
				END IF
			END IF

			IF lAddEndif
				oCode:AddLine("#endif")
			END IF
		NEXT
		
	RETURN oCode

	PROTECTED METHOD ConvertLine(oLine AS LineObject , cCallBack REF STRING) AS STRING
		LOCAL aWords AS List<WordObject>
		LOCAL cLine , cLineUpper AS STRING
		LOCAL sLine AS System.Text.StringBuilder

		cCallBack := NULL
		
		cLine := oLine:LineText
		cLineUpper := cLine:ToUpper():Trim()
		
		IF cLineUpper:StartsWith("VTRACE") .or. cLineUpper:StartsWith("VMETHOD")
			cLine := "// " + cLine
			RETURN cLine
		END IF
		
		IF xPorter.Options:RemoveExportLocalClause .and. oLine:HasExportLocalClause
			IF oLine:oMoreInfo:ExportLocalClause:Begins != 0
				IF oLine:oMoreInfo:ExportLocalClause:Ends != 0
					cLine := cLine:Substring(0 , oLine:oMoreInfo:ExportLocalClause:Begins - 1) + cLine:Substring(oLine:oMoreInfo:ExportLocalClause:Ends - 1)
				END IF
			END IF
		END IF
		IF xPorter.Options:RemoveClassClause .and. oLine:HasClassClause
			IF oLine:oMoreInfo:ClassClause:Begins != 0
				IF oLine:oMoreInfo:ClassClause:Ends != 0
					cLine := cLine:Substring(0 , oLine:oMoreInfo:ClassClause:Begins - 1) + cLine:Substring(oLine:oMoreInfo:ClassClause:Ends - 1)
				ELSE
					cLine := cLine:Substring(0 , oLine:oMoreInfo:ClassClause:Begins - 1)
				END IF
			ELSE
				cLine := cLine:Substring(oLine:oMoreInfo:ClassClause:Ends - 1)
			END IF
		END IF
/*		IF xPorter.Options:ChangePascalToStrict .and. oLine:HasPascalClause
			cLine := cLine:Substring(0 , oLine:oMoreInfo:PascalClause:Begins - 1) + "STRICT" + cLine:Substring(oLine:oMoreInfo:PascalClause:Begins + 6 - 1)
		END IF*/
		
		aWords := AnalyzeLine(cLine)
		sLine := System.Text.StringBuilder{cLine:Length + 5}
		
		IF SELF:PartialClass .and. oLine:oEntity != NULL .and. oLine:oEntity:eType == EntityType._Class
			sLine:Append("PARTIAL ")
		END IF
		
		LOCAL lInBracketString := FALSE AS LOGIC
		LOCAL lLastWasColon := FALSE AS LOGIC
		LOCAL lInIndexedProperty := FALSE AS LOGIC
		
		FOR LOCAL nWord := 0 AS INT UPTO aWords:Count - 1
			LOCAL oWord , oNextWord , oNextNextWord , oPrevWord AS WordObject
			oWord := aWords[nWord]
			IF nWord < aWords:Count - 1
				oNextWord := aWords[nWord + 1]
			ELSE
				oNextWord := NULL
			END IF
			IF nWord < aWords:Count - 2
				oNextNextWord := aWords[nWord + 2]
			ELSE
				oNextNextWord := NULL
			END IF
			IF nWord > 0
				oPrevWord := aWords[nWord - 1]
			ELSE
				oPrevWord := NULL
			END IF
			
			LOCAL cWord, cWordUpper AS STRING
			cWord := oWord:cWord

			DO CASE

			CASE cWord == "%" .and. oNextNextWord != NULL .and. oNextNextWord:cWord == "%" .and. oNextWord:cWord:ToUpper() == "CAVOSAMPLESROOTDIR"
				cWord := ""
				oNextWord:cWord := VOFolder.Get() + "\Samples"
				oNextNextWord:cWord := ""
				
			CASE oWord:eStatus == WordStatus.Text // no literals or comments
				cWordUpper := cWord:ToUpper()
				DO CASE
				CASE cWordUpper:StartsWith("STRU") .and. oWord:eSubStatus == WordSubStatus.TextReserved
					cWord := "VOSTRUCT"
				CASE cWordUpper == "THROW" .and. (oPrevWord == NULL .or. .not. oPrevWord:cWord:StartsWith("@"))
					cWord := "@@" + cWord
				CASE cWordUpper == "_NC" .or. cWordUpper == "_CO"
					cWord := ""
				CASE cWordUpper == "@" .and. (oNextWord != NULL .and. SELF:_oModule:Application:ContainsCallback(oNextWord:cWord))
//					MessageBox.Show(oNextWord:cWord , "Callback!")
					cCallBack := oNextWord:cWord
				END CASE

			CASE oWord:eStatus == WordStatus.Literal
				IF lInBracketString
					IF cWord == "]"
						lInBracketString := FALSE
						cWord := e"\""
					ELSE
						IF cWord == e"\"" .or. cWord == "\"
							cWord := "\" + cWord
						END IF
					END IF
				ELSE
					IF cWord == "["
						IF oPrevWord == NULL .or. oPrevWord:eStatus != WordStatus.Literal
							lInBracketString := TRUE
							cWord := e"e\""
						END IF
					END IF
				ENDIF
			END CASE
			
			IF lInIndexedProperty .and. oWord:eStatus == WordStatus.Text .and. oWord:cWord == ","
				sLine:Append('[')
				lInIndexedProperty := FALSE
				lLastWasColon := FALSE
				LOOP // don't add the first comma
			ENDIF
			IF lLastWasColon .and. oWord:eStatus == WordStatus.Text .and. oWord:cWord == "["
				lInIndexedProperty := TRUE
				lLastWasColon := FALSE
				LOOP // do not add the "[" now
			END IF
			IF oWord:eStatus == WordStatus.Text .and. oWord:cWord == ":"
				lLastWasColon := TRUE
			ELSEIF .not. oWord:IsWhiteSpace
				lLastWasColon := FALSE
			END IF
			
			sLine:Append(cWord)
			
			// not needed anymore?
/*			IF oWord:eStatus == WordStatus.Literal
				IF oWord:cWord == "#" .and. oNextWord != NULL .and. oNextWord:cWord:ToUpper() == "NIL"
					sLine:Append((Char)' ')
				END IF
			END IF*/
		NEXT

		cLine := sLine:ToString()
			
	RETURN cLine
	

	PROTECTED METHOD Generate_Textblock() AS OutputCode
		LOCAL oCode AS OutputCode
		LOCAL lFirst := TRUE AS LOGIC
		LOCAL cLine AS STRING
		oCode := OutputCode{}
		FOREACH oLine AS LineObject IN SELF:_aLines
			cLine := oLine:LineText
			IF lFirst
				lFirst := FALSE
				oCode:AddLine("/*")
			END IF
			cLine := cLine:Replace("/*" , "/-*")
			cLine := cLine:Replace("*/" , "*-/")
			oCode:AddLine(cLine)
		NEXT
		oCode:AddLine("*/")
	RETURN oCode

	PROTECTED METHOD Generate_Resource() AS OutputCode
		LOCAL oCode AS OutputCode
		LOCAL cLine AS STRING
		LOCAL lFirstLine := TRUE AS LOGIC
		LOCAL lStringTableLine := FALSE AS LOGIC
		
		LOCAL tag := "__T_A_G__" AS STRING

		oCode := OutputCode{}
		FOREACH oLine AS LineObject IN SELF:_aLines

			LOCAL lHasWizDir := FALSE AS LOGIC
			cLine := oLine:LineText
			lHasWizDir := cLine:ToUpper():Contains("%APPWIZDIR%")
			cLine := cLine:Replace("%" , tag) // so that %something% is treated as one word

			LOCAL aWords AS List<WordObject>
			LOCAL cPrevWord := "" AS STRING
			LOCAL cNextWord := "" AS STRING
			LOCAL lInFilename := FALSE AS LOGIC
			aWords := AnalyzeLine(cLine)
			cLine := ""

			FOR LOCAL nWord := 0 AS INT UPTO aWords:Count - 1
				
				LOCAL oWord AS WordObject 
				LOCAL cWord, cUpper AS STRING
				
				oWord := aWords[nWord]
				cWord := oWord:cWord
				cUpper := cWord:ToUpper()
				cNextWord := iif(nWord < aWords:Count - 1 , aWords[nWord+1]:cWord , "")
				
				IF cWord == ":" .and. cNextWord == "\"
					lInFilename := TRUE
				END IF

				IF oLine:oEntity != NULL .and. cUpper == "RESOURCE"
					cPrevWord := cWord
					LOOP
				END IF

				DO CASE

				CASE oWord:eStatus == WordStatus.Text // no literals or comments
					STATIC LOCAL tag_version := tag + "VERSION" + tag AS STRING
					STATIC LOCAL tag_appwiz := tag + "APPWIZDIR" + tag AS STRING
					STATIC LOCAL tag_samples := tag + "CAVOSAMPLESROOTDIR" + tag AS STRING
					STATIC LOCAL tag_cavodir := tag + "CAVODIR" + tag AS STRING
					DO CASE
					CASE cUpper == "STRINGTABLE" .and. lFirstLine
						lStringTableLine := TRUE
						EXIT
					CASE cUpper == "__VERSION__" .or. cUpper == tag_version
						cWord := "1"
					CASE cUpper == "__APPWIZDIR__" .or. cUpper == tag_appwiz
						cWord := VOFolder.Get() + "\Appwiz"
					CASE cUpper == "__CAVOSAMPLESROOTDIR__" .or. cUpper == tag_samples
						cWord := VOFolder.Get() + "\Samples"
					CASE cUpper == "__CAVODIR__" .or. cUpper == tag_cavodir
						cWord := VOFolder.Get()
//					CASE lFirstLine .and. (cPrevWord == "\" .or. cPrevWord == "." .or. cPrevWord == ":")
					CASE lFirstLine .and. lInFilename
						// do not allow below to replace defines in part of filenames that are not surrounded by quotes
						NOP
					CASE SELF:_oModule:Application:HasDefine(cUpper)
						cLine += SELF:TranslateDefineInResource(cUpper)
						cPrevWord := cWord
						LOOP
/*					CASE SELF:_oModule:HasDefine(cUpper)
						cLine += SELF:_oModule:GetDefineValue(cUpper):Replace(e"\"" , ""):Replace("'" , "")
						cPrevWord := cWord
						LOOP
					CASE SELF:_oModule:Application:HasDefine(cUpper)
						cLine += SELF:_oModule:Application:GetDefineValue(cUpper):Replace(e"\"" , ""):Replace("'" , "")
						cPrevWord := cWord
						LOOP*/
					CASE xPorter.SDKDefines:ContainsKey(cUpper)
						cLine += xPorter.SDKDefines[cUpper]:Replace(e"\"" , ""):Replace("'" , "")
						cPrevWord := cWord
						LOOP
					END CASE

				CASE oWord:eStatus == WordStatus.Literal
					IF cUpper == "_VOOLECONTAINER" .and. .not. String.IsNullOrEmpty(Replace_VOOleContainer)
						cWord := Replace_VOOleContainer
					END IF
					
				END CASE

				cLine += cWord:Replace(tag , "%") // bring back the original text, if we have not modified it
/*
In resource headers like:

RESOURCE IconEntity Icon "C:\some\folder\testico.ico"

when file paths are included in quotes we need to double the slashes, because those are escaped 
strings and if there are escape sequences inside (like \t), the resource compiler will not find the file
Probably we need to do that also to every other string in every line in the resource?
*/
				IF lFirstLine .and. cWord == "\" .and. oWord:eStatus == WordStatus.Literal .and. cPrevWord != "\" .and. cNextWord != "\"
					cLine += "\"
				END IF

				cPrevWord := cWord

			NEXT

			IF lStringTableLine
				// rc.exe does not allow names before the STRINGTABLE identifier
				cLine := "STRINGTABLE"
				lStringTableLine := FALSE
			END IF

			oCode:AddLine(cLine)
			lFirstLine := FALSE

		NEXT
	RETURN oCode
	
	METHOD TranslateDefineInResource(cUpper AS STRING) AS STRING
	RETURN SELF:TranslateDefineInResource(cUpper , 0)
	METHOD TranslateDefineInResource(cUpper AS STRING , nNestLevel AS INT) AS STRING
		nNestLevel ++
		IF nNestLevel > 5
			RETURN cUpper
		END IF
		LOCAL cNewDefine := NULL AS STRING
		IF SELF:_oModule:HasDefine(cUpper)
			cNewDefine := SELF:_oModule:GetDefineValue(cUpper)
		ELSEIF SELF:_oModule:Application:HasDefine(cUpper)
			cNewDefine := SELF:_oModule:Application:GetDefineValue(cUpper)
		ENDIF
		
		IF String.IsNullOrEmpty(cNewDefine)
			RETURN ""
		END IF

		LOCAL aWords AS List<WordObject>
		aWords := AnalyzeLine(cNewDefine)
		cNewDefine := ""
		FOREACH oWord AS WordObject IN aWords
			LOCAL cWord AS STRING
			cWord := oWord:cWord
			IF oWord:eStatus == WordStatus.Text
				cUpper := cWord:ToUpper()
				IF SELF:_oModule:Application:HasDefine(cUpper)
					cNewDefine += SELF:TranslateDefineInResource(cUpper , nNestLevel)
					LOOP
				ENDIF
			ENDIF
			cNewDefine += cWord
		NEXT
		cNewDefine := cNewDefine:Replace(e"\"" , ""):Replace("'" , "")
	RETURN cNewDefine
	
	STATIC METHOD AnalyzeLine(cLine AS STRING) AS List<WordObject>
		LOCAL aWords AS List<WordObject>
		aWords := List<WordObject>{}
		FOREACH oWord AS WordObject IN EditorBuffer.ParseLine(FileType.VO , cLine)
			IF oWord:cWord:Length != 0
				aWords:Add(oWord)
			END IF
		NEXT
	RETURN aWords
	
END CLASS



CLASS OutputCode
	PROTECT _aLines AS List<STRING>
	CONSTRUCTOR()
		SELF:_aLines := List<STRING>{}
	RETURN
	
	PROPERTY Lines AS List<STRING> GET SELF:_aLines
	
	METHOD AddLine(cLine AS STRING) AS VOID
		SELF:_aLines:Add(cLine)
	RETURN
	METHOD InsertLine(cLine AS STRING) AS VOID
		SELF:_aLines:Insert(0 , cLine)
	RETURN
	METHOD Combine(oCode AS OutputCode) AS VOID
		FOREACH cLine AS STRING IN oCode:Lines
			SELF:_aLines:Add(cLine)
		NEXT
	RETURN
	PROPERTY AsString AS STRING
		GET
			LOCAL s AS System.Text.StringBuilder
			s := System.Text.StringBuilder{SELF:_aLines:Count * 50}
			FOREACH cLine AS STRING IN SELF:_aLines
				s:AppendLine(cLine)
			NEXT
			RETURN s:ToString()
		END GET
	END PROPERTY
	METHOD GetContents() AS IEnumerable<STRING>
	RETURN SELF:_aLines
	METHOD IsEmpty() AS LOGIC
	RETURN SELF:_aLines:Count == 0 .or. (SELF:_aLines:Count == 1 .and. SELF:_aLines[0] == "")
	
END CLASS

FUNCTION NewGuid() AS STRING
RETURN System.Guid.NewGuid():ToString():ToUpper()

FUNCTION GetRelativePath(cSourceDir AS STRING , cTarget AS STRING) AS STRING
	LOCAL oSource , oTarget , oRelative AS Uri
	LOCAL cRelative AS STRING
	IF .not. cSourceDir:EndsWith("\")
		cSourceDir += "\"
	END IF
	oSource := System.Uri{cSourceDir}
	oTarget := Uri{cTarget}
	oRelative := oSource:MakeRelativeUri(oTarget)
	cRelative := oRelative:ToString()
	cRelative := Uri.UnescapeDataString(cRelative)
	cRelative := cRelative:Replace("/" , "\")
RETURN cRelative

FUNCTION GetFilename(cFullPath AS STRING) AS STRING
RETURN FileInfo{cFullPath}:Name
FUNCTION GetFilenameNoExt(cFullPath AS STRING) AS STRING
	LOCAL cFileName AS STRING
	cFileName := FileInfo{cFullPath}:Name
	LOCAL nAt AS INT
	nAt := cFileName:LastIndexOf('.')
	IF nAt != -1
		cFileName := cFileName:Substring(0 , nAt)
	END IF
RETURN cFileName

FUNCTION StripCommentsFromCode(cCode AS STRING) AS STRING
	LOCAL aWords AS List<WordObject>
	LOCAL sCode AS StringBuilder
	aWords := EntityDescriptor.AnalyzeLine(cCode)
	sCode := StringBuilder{cCode:Length}
	FOREACH oWord AS WordObject IN aWords
		IF oWord:eStatus != WordStatus.Comment
			sCode:Append(oWord:cWord)
		END IF
	NEXT
RETURN sCode:ToString()

FUNCTION MakePathLegal(cPath AS STRING) AS STRING
// vary neat piece of code below, found in stackoverflow!
RETURN String.Join("_" , cPath:Split( Path.GetInvalidFileNameChars() ) )

FUNCTION SafeFileExists(cFileName AS STRING) AS LOGIC
	LOCAL lRet := FALSE AS LOGIC
	TRY
		lRet := File.Exists(cFileName)
	END TRY
RETURN lRet
FUNCTION SafeFolderExists(cFolder AS STRING) AS LOGIC
	LOCAL lRet := FALSE AS LOGIC
	TRY
		lRet := Directory.Exists(cFolder)
	END TRY
RETURN lRet

