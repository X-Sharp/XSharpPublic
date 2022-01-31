USING System.IO
USING System.Collections
USING System.Collections.Generic
USING System.Windows.Forms
USING System.Reflection
USING System.Text
USING System.Drawing
USING System.Linq
USING Xide

GLOBAL glCreateFilePerClass := FALSE AS LOGIC

GLOBAL gaNewKeywordsInXSharp := <STRING>{;
	"EVENT","INT64","ENUM","DELEGATE","PARTIAL","INTERFACE",;
	"CONSTRUCTOR","DESTRUCTOR","FINALLY","TRY","CATCH","THROW","SEALED","ABSTRACT",;
	"CONST","INITONLY","VIRTUAL","OPERATOR","EXPLICIT","IMPLICIT","PROPERTY","IMPLIED","DEBUG","TRACE";
	} AS STRING[]

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
	oOptions:SortEntitiesByName := TRUE
	oOptions:UseXSharpRuntime := TRUE
    oOptions:CopyResourcesToProjectFolder := TRUE
    oOptions:ReplaceResourceDefinesWithValues := TRUE
    oOptions:CheckForIVarAndPropertyConflicts := FALSE
    oOptions:IgnoreCodeInside_ifdef_endif := TRUE
    oOptions:RemoveONLYEARLYpragmas := TRUE
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
				IF SafeFileExists(cFileName) .OR. SafeFolderExists(cFileName)
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
		CATCH
			NOP
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
				SWITCH cKey
					CASE "OUTPUTFOLDER"
						DefaultOutputFolder := cValue
					CASE "SOURCEFOLDER"
						DefaultSourceFolder := cValue
					CASE "VOFOLDER"
						VOFolder.Set(cValue)
					CASE "SDKDEFINESDLL"
						SDKDefines_FileName := cValue
					CASE "NOWARNINGSCREEN"
						NoWarningScreen := cValue == "1"
					CASE "_VOOLECONTAINER"
						Replace_VOOleContainer := cValue
					CASE "RUNTIMELOCATION"
						RuntimeFolder := cValue
					CASE "EXTRAKEYWORDS"
						TRY
							LOCAL aKeywords AS STRING[]
							aKeywords := cValue:Split(<Char>{',',';',' '})
							FOREACH cKeyword AS STRING IN aKeywords
								IF .not. String.IsNullOrWhiteSpace(cKeyword)
									IF .not. xPorter.NewKeywordsInXSharp:ContainsKey(cKeyword:ToUpperInvariant())
										xPorter.NewKeywordsInXSharp:Add(cKeyword:ToUpperInvariant() , cKeyword)
									ENDIF
								END IF
							NEXT
						CATCH
							NOP
						END TRY
					CASE "USEWINFORMSIVARPREFIX"
						xPorter.UseWinFormsIVarPrefix := cValue == "1"
					CASE "EXPORTTOIDE"
						cValue := cValue:ToUpper()
						xPorter.ExportToXide := cValue:Contains("XIDE") .or. .not. cValue:Contains("VS")
						xPorter.ExportToVS := cValue:Contains("VS") .or. .not. cValue:Contains("XIDE")
					CASE "WEDXML"
						xPorter.ExportWedToXml := cValue == "1"
				END SWITCH
			END IF
		NEXT
	END IF

	IF String.IsNullOrWhiteSpace(SDKDefines_FileName) .OR. .NOT. File.Exists(SDKDefines_FileName)
		IF File.Exists(Application.StartupPath + "\SDK_Defines.dll")
			SDKDefines_FileName := Application.StartupPath + "\SDK_Defines.dll"
		END IF
	END IF

	TRY
		IF .NOT. VOFolder.IsValid()
			LOCAL vo,ver AS Microsoft.Win32.RegistryKey
			vo := Microsoft.Win32.Registry.LocalMachine:OpenSubKey("Software\GrafX\Visual Objects")
			IF vo != NULL
				FOREACH subkey AS STRING IN vo:GetSubKeyNames()
					ver := vo:OpenSubKey(subkey)
					IF ver:GetValue("InstallPath") != NULL
						VOFolder.Set(ver:GetValue("InstallPath"):ToString())
					END IF
				NEXT
			ENDIF
		END IF
	CATCH
		NOP
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
		SELF:oTimer:Interval := 1000
		SELF:oTimer:Tick += SELF:Timer_tick
		SELF:oTimer:Start()
	RETURN
	PROTECTED METHOD Timer_tick(sender AS OBJECT , e AS EventArgs) AS VOID
		SELF:lAllowClose := TRUE
		SELF:oTimer:Dispose()
	RETURN
	PROTECTED OVERRIDE  METHOD OnClosing(e AS System.ComponentModel.CancelEventArgs) AS VOID
		SUPER:OnClosing(e)
		IF .NOT. SELF:lAllowClose .AND. Control.ModifierKeys != Keys.Control
			e:Cancel := TRUE
			SELF:oTimer:Stop()
			ShowMessage("Please read this important information!")
			SELF:oTimer:Interval := 500
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
	CATCH
		NOP
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
			lValid := .NOT. (String.IsNullOrEmpty(_scFolder) .OR. .NOT. Directory.Exists(_scFolder))
		CATCH
			NOP
		END TRY
	RETURN lValid
	STATIC METHOD Get() AS STRING
		STATIC LOCAL lChecked := FALSE AS LOGIC
		IF .NOT. lChecked
			lChecked := TRUE
			IF .NOT. VOFolder.IsValid()
				ShowWarning(e"Could not find VO installation folder, macros in code like %cavosamplesrootdir% will not be translated directly.\r\n\r\nYou can manually set the VO folder in xPorter.ini.")
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
	EXPORT SortEntitiesByName AS LOGIC
	EXPORT UseXSharpRuntime AS LOGIC
    EXPORT CopyResourcesToProjectFolder AS LOGIC
    EXPORT ReplaceResourceDefinesWithValues AS LOGIC
    EXPORT CheckForIVarAndPropertyConflicts AS LOGIC
    EXPORT IgnoreCodeInside_ifdef_endif AS LOGIC
    EXPORT RemoveONLYEARLYpragmas AS LOGIC
END STRUCTURE

INTERFACE IProgressBar
	METHOD SetProgressBarRange(nValue AS INT) AS VOID
	METHOD SetProgressBarValue(nValue AS INT) AS VOID
	METHOD AdvanceProgressbar() AS VOID
END INTERFACE

CLASS xPorter
	STATIC EXPORT Options AS xPorterOptions
	STATIC EXPORT uiForm AS xPorterUI

	STATIC EXPORT ExportToXide := TRUE AS LOGIC
	STATIC EXPORT ExportToVS := TRUE AS LOGIC
	STATIC EXPORT NewKeywordsInXSharp AS Dictionary<STRING,STRING>

	STATIC EXPORT ExportingSingleFile := FALSE AS LOGIC
	STATIC EXPORT ExportWedToXml := FALSE AS LOGIC

	STATIC PROPERTY OverWriteProjectFiles AS LOGIC AUTO
	STATIC PROPERTY GenerateWinForms AS LOGIC AUTO
	STATIC PROPERTY UseWinFormsIVarPrefix AS LOGIC AUTO

	STATIC PROTECT _aFoundDefines := SortedList<STRING,STRING>{} AS SortedList<STRING,STRING>
	STATIC PROTECT _aSDKDefines AS Dictionary<STRING,STRING>
	STATIC PROPERTY SDKDefines AS Dictionary<STRING,STRING> GET _aSDKDefines

	STATIC CONSTRUCTOR()

//		MessageBox.Show("loading file : " + Environment.CurrentDirectory + "\SDK_Defines.dll")
		NewKeywordsInXSharp := Dictionary<STRING,STRING>{}
		FOREACH cKeyword AS STRING IN gaNewKeywordsInXSharp
			NewKeywordsInXSharp:Add(cKeyword:ToUpper(),cKeyword)
		NEXT


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
				IF (oField:FieldType == TypeOf(INT) .OR. oField:FieldType == TypeOf(DWORD)).AND. ;
					.NOT. _aSDKDefines:ContainsKey(oField:Name:ToUpper())
					_aSDKDefines:Add(oField:Name:ToUpper() , oField:GetValue(NULL):ToString())
				END IF
			NEXT
			TRY
				// those were not included in the SDK_Defines.dll
				_aSDKDefines:Add("VOVER_FILE_VERSION", e"\"0,0,0,0\"")
				_aSDKDefines:Add("VOVER_PROD_VERSION", e"\"0,0,0,0\"")
				_aSDKDefines:Add("VOVER_COMPANY", e"\"\"")
				_aSDKDefines:Add("VOVER_FILE_VERSION_TXT", e"\"\"")
				_aSDKDefines:Add("VOVER_PROD_VERSION_TXT", e"\"\"")
				_aSDKDefines:Add("VOVER_COPYRIGHT", e"\"\"")
				_aSDKDefines:Add("VOVER_PROD_NAME", e"\"\"")
				_aSDKDefines:Add("VOVER_DEVELOPMENT", e"\"\"")
				_aSDKDefines:Add("VOVER_DEVWEBSITE", e"\"\"")
				_aSDKDefines:Add("VOVER_WEBSITE", e"\"\"")
			CATCH
				NOP
			END TRY
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


	STATIC METHOD xPort_AppFromFolder(cSourceFolder AS STRING , cOutputFolder AS STRING , cSolutionName AS STRING, cAppName AS STRING) AS LOGIC
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
			oProject:xPortApps(cOutputFolder)
		ENDIF
	RETURN TRUE

	STATIC METHOD xPort_AppFromAef(cAefFile AS STRING , cOutputFolder AS STRING , cSolutionName AS STRING , cAppName AS STRING) AS LOGIC
		xPorter.Reset()
		xPorter.Options:IgnoreDuplicateDefines := FALSE

		LOCAL oApp AS ApplicationDescriptor
		LOCAL oProject AS VOProjectDescriptor
		oProject := VOProjectDescriptor{cSolutionName , NewGuid()}
		oApp := ApplicationDescriptor.CreateFromAef(cAefFile , oProject , cAppName)
//		oProject:SetName(oApp:Name)
		oProject:AddApplication(oApp)

		IF oApp:Loaded
			oProject:xPortApps(cOutputFolder)
		ENDIF
	RETURN TRUE

	STATIC METHOD xPort_AppsFromAefsInFolder(cSourceFolder AS STRING , cOutputFolder AS STRING , cSolutionName AS STRING) AS LOGIC
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

		oProject:xPortApps(cOutputFolder)
	RETURN TRUE

	STATIC METHOD xPort_PrgFromMef(cMefFile AS STRING , cOutputFolder AS STRING) AS LOGIC
		xPorter.Reset()
		Directory.CreateDirectory(cOutputFolder)

		LOCAL oMef AS Fab_VO_Entities.FabMEFFile
		LOCAL oEntity AS Fab_VO_Entities.FabMEFEntity
		LOCAL oSource AS System.Text.StringBuilder

		xPorter.uiForm:SetProgressText("Reading module " + cMefFile)

		oSource := System.Text.StringBuilder{}

		oMef := Fab_VO_Entities.FabMEFFile{cMefFile}
		IF xPorter.Options:SortEntitiesByName
			oMef:SortByName()
		ENDIF
		LOCAL oModule := NULL AS ModuleDescriptor
		LOCAL aCode AS STRING[]
		LOCAL cCode AS STRING

		oSource:Clear()
		FOR LOCAL m := 0 AS INT UPTO oMef:EntityListList:Count - 1
			oEntity := oMef:EntityListList[m]
			oSource:Append(oEntity:Source + e"\r\n")
		NEXT
		cCode := oSource:ToString()

		aCode := cCode:ToString():Split(<STRING>{e"\r\n" , e"\r" , e"\n"} , StringSplitOptions.None)
		oModule := ModuleDescriptor{oMef:Name , NULL , aCode}

		oModule:SetDummyApp()
		oModule:Analyze()

		oMef:Close()

		LOCAL oCode AS OutputCode
		oCode := oModule:Generate()

		IF oModule:Generated
			xPorter.Message("  Generating module :" , oModule:Name)
			File.WriteAllLines(cOutputFolder + "\" + oModule:PathValidName + ".prg" , oCode:GetContents() , System.Text.Encoding.UTF8)
		END IF
	RETURN TRUE

	STATIC METHOD xPort_PrgFromPrg(cPrgFile AS STRING , cOutputFolder AS STRING) AS LOGIC
		xPorter.Reset()
		Directory.CreateDirectory(cOutputFolder)

		xPorter.Message("Reading file " + cPrgFile)

		LOCAL oModule := NULL AS ModuleDescriptor
		LOCAL aCode AS STRING[]

		aCode := File.ReadAllLines(cPrgFile, Encoding.Default)
		oModule := ModuleDescriptor{FileInfo{cPrgFile}:Name , NULL , aCode}

		oModule:SetDummyApp()
		oModule:Analyze()

		LOCAL oCode AS OutputCode
		oCode := oModule:Generate()

		IF oModule:Generated
			xPorter.Message("  Generating module :" , oModule:Name)
			File.WriteAllLines(cOutputFolder + "\" + oModule:PathValidName, oCode:GetContents() , System.Text.Encoding.UTF8)
		END IF
	RETURN TRUE

	STATIC METHOD xPort_PrgFromClipboard(cPrgFile AS STRING , cOutputFolder AS STRING) AS LOGIC
		xPorter.Reset()
		Directory.CreateDirectory(cOutputFolder)

		xPorter.Message("Reading code from CLipboard")

		LOCAL oModule := NULL AS ModuleDescriptor
		LOCAL aCode AS STRING[]

		LOCAL cClipboard := "" AS STRING
		TRY
			IF Clipboard.ContainsText()
				cClipboard := Clipboard.GetText()
			END IF
		CATCH
			NOP
		END TRY
		LOCAL cUpper AS STRING
		cUpper := cClipboard:ToUpper()
		IF .not. (cUpper:Contains("CLASS") .or. cUpper:Contains("PROC") .or. cUpper:Contains("FUNC") .or. cUpper:Contains("GLOBAL") .or. cUpper:Contains("DEFINE"))
			ShowWarning("Clipboard does not contain VO code")
			RETURN FALSE
		ENDIF

		aCode := cClipboard:Split(<STRING>{e"\r\n" , e"\r" , e"\n"} , StringSplitOptions.None)
		IF String.IsNullOrEmpty( FileInfo{cPrgFile}:Extension )
			cPrgFile += ".prg"
		END IF
		oModule := ModuleDescriptor{cPrgFile , NULL , aCode}

		oModule:SetDummyApp()
		oModule:Analyze()

		LOCAL oCode AS OutputCode
		oCode := oModule:Generate()

		IF oModule:Generated
			xPorter.Message("  Generating module :" , oModule:Name)
			File.WriteAllLines(cOutputFolder + "\" + oModule:PathValidName, oCode:GetContents() , System.Text.Encoding.UTF8)
			TRY
				LOCAL sCode AS StringBuilder
				sCode := StringBuilder{}
				FOREACH cLine AS STRING IN oCode:GetContents()
					sCode:Append(cLine)
					sCode:Append(e"\r\n")
				NEXT
				Clipboard.SetText(sCode:ToString())
			CATCH
				NOP
			END TRY
		END IF
	RETURN TRUE



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
	PROPERTY WinFormsFolder AS STRING GET SELF:_cProjectFolder + "\Windows.Forms"
	PROPERTY Solution_SDKDefines_Filename AS STRING GET SELF:_cSolution_SDKDefines_Filename

	CONSTRUCTOR(cName AS STRING , cGuid AS STRING)
		SELF:_cName := cName
		SELF:_cGuid := cGuid
		SELF:_aApplications := List<ApplicationDescriptor>{}
	RETURN

	METHOD SetName(cName AS STRING) AS VOID
		SELF:_cName := cName
	RETURN
	METHOD SetProjectFolder(cFolder AS STRING) AS VOID
		SELF:_cProjectFolder := cFolder
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
				RETURN oApp
			END IF
		NEXT
	RETURN NULL


	METHOD xPortApps(cOutputFolder AS STRING) AS VOID
		SELF:_cProjectFolder := cOutputFolder

		Directory.CreateDirectory(cOutputFolder)
		IF File.Exists(SDKDefines_FileName)
			SELF:_cSolution_SDKDefines_Filename := cOutputFolder + "\" + GetFilename(SDKDefines_FileName)
			IF .NOT. File.Exists(SELF:_cSolution_SDKDefines_Filename) .AND. .NOT. xPorter.Options:UseXSharpRuntime
				File.Copy(SDKDefines_FileName , SELF:_cSolution_SDKDefines_Filename)
			END IF
		END IF

		IF xPorter.GenerateWinForms
			Directory.CreateDirectory(SELF:WinFormsFolder)
			Directory.CreateDirectory(SELF:WinFormsFolder + "\tmp")
		END IF

		xPorter.uiForm:SetProgressBarRange(SELF:CountModules() * 2) // analysis and export
		SELF:Analyze()

		FOREACH oApp AS ApplicationDescriptor IN SELF:_aApplications
			IF oApp:Loaded
				oApp:Generate()
			END IF
		NEXT

		IF xPorter.GenerateWinForms
			WinFormsConverter.Convert(SELF, SELF:WinFormsFolder)
		END IF

		IF xPorter.ExportToXide
			SELF:CreateSolutionFile(cOutputFolder , TRUE)
		ENDIF
		IF xPorter.ExportToVS
			SELF:CreateSolutionFile(cOutputFolder , FALSE)
		ENDIF

		xPorter.Message("Finished xPorting!")
	RETURN

	PROTECTED METHOD Analyze() AS VOID
		FOREACH oApplication AS ApplicationDescriptor IN SELF:_aApplications
			xPorter.Message("Analyzing application/library" , oApplication:Name)
			oApplication:Analyze()
		NEXT
	RETURN

	METHOD CreateSolutionFile(cFolder AS STRING , lXide AS LOGIC) AS VOID
		LOCAL oTemplate AS StreamReader
		LOCAL oOutput AS StreamWriter
		LOCAL cTemplate AS STRING
		LOCAL cSolutionName AS STRING
		LOCAL cFileName AS STRING

		cSolutionName := SELF:Name //+ "_xported"

		xPorter.Message("Creating solution file for " + IIF(lXide , "XIDE" , "VS"))

		IF lXide
			cFileName := cFolder + "\" + MakePathLegal( cSolutionName ) + ".viproj"

			IF .NOT. xPorter.OverWriteProjectFiles .AND. File.Exists(cFileName)
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
			IF .NOT. xPorter.OverWriteProjectFiles .AND. File.Exists(cFileName)
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
			CASE cTemplate == "%applications%" .OR. cTemplate == "%projects%"
				FOREACH oApp AS ApplicationDescriptor IN SELF:_aApplications
					IF .NOT. oApp:Saved
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
	PROTECT _lLoaded AS LOGIC
	PROTECT _lSaved AS LOGIC
	PROTECT _eType AS ApplicationType
	PROTECT _aModules AS List<ModuleDescriptor>
	PROTECT _aReferences AS List<ApplicationDescriptor>
	PROTECT _aGACReferences AS List<STRING>
	PROTECT _aBrowseReferences AS List<STRING>
	PROTECT _aProjectReferences AS List<STRING>
    PROTECT _aOtherFiles AS List<STRING>

	PROTECT _aFunctions AS List<STRING>
	PROTECT _aCallbacks AS List<STRING>

	PROTECT _lOptionOverflow AS LOGIC
	PROTECT _lOptionIntDiv AS LOGIC

	PROTECT _oProject AS VOProjectDescriptor

	PROTECT _lIsWinForms AS LOGIC
	PROTECT _cAppSubFolder AS STRING

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
        SELF:_aOtherFiles := List<STRING>{}

		SELF:_oProject := oProject

		SELF:xPortOptions := xPorter.Options

		IF .not. SELF:_lIsWinForms
			IF xPorter.Options:UseXSharpRuntime
				SELF:AddRuntimeReference("XSharp.Core")
	            SELF:AddRuntimeReference("XSharp.RT")
				SELF:AddRuntimeReference("XSharp.VO")
			ELSE
				SELF:AddRuntimeReference("VulcanRT")
				SELF:AddRuntimeReference("VulcanRTFuncs")
			END IF
		END IF

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

	PROPERTY AppFile_XIDE AS STRING GET SELF:AppFolder + "\" + SELF:PathValidName + ".viapp"
	PROPERTY AppFile_VS AS STRING GET SELF:AppFolder + "\" + SELF:PathValidName + ".xsproj"

	PROPERTY OptionOverflow AS LOGIC GET SELF:_lOptionOverflow
	PROPERTY OptionIntDiv AS LOGIC GET SELF:_lOptionIntDiv

	PROPERTY Type AS ApplicationType GET SELF:_eType
	PROPERTY Project AS VOProjectDescriptor GET SELF:_oProject

	EXPORT xPortOptions AS xPorterOptions

	PROPERTY AppFolder AS STRING
		GET
			IF SELF:_cAppSubFolder == NULL
				SELF:_cAppSubFolder := SELF:PathValidName
			END IF
			RETURN SELF:Project:ProjectFolder + "\" + SELF:_cAppSubFolder
		END GET
	END PROPERTY

	PROPERTY ModulesCount AS INT GET SELF:_aModules:Count

	METHOD SetWinForms() AS VOID
		SELF:_lSaved := TRUE
		SELF:_lIsWinForms := TRUE
	RETURN

	METHOD AddRuntimeReference(cReference AS STRING) AS VOID
		LOCAL lAddAsDll := FALSE AS LOGIC
		TRY
			IF .NOT. String.IsNullOrWhiteSpace(RuntimeFolder) .AND. Directory.Exists(RuntimeFolder)
				LOCAL cDll AS STRING
				cDll := RuntimeFolder
				IF .NOT. cDll:EndsWith("\")
					cDll += "\"
				END IF
				cDll += cReference
				IF .NOT. cReference:ToLower():EndsWith(".dll")
					cDll += ".dll"
				END IF
				IF File.Exists(cDll)
					SELF:_aBrowseReferences:Add(cDll)
					lAddAsDll := TRUE
				END IF
			END IF
		CATCH
			NOP
		END TRY
		IF .NOT. lAddAsDll
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
		IF cFunction:ToUpper() == "__CONNECTFUNC" .OR. cFunction:ToUpper() == "WINSOCKEXIT"
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
		CASE oAef:IsDLL .OR. oAef:IsLibrary
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
				cGAC := IIF(xPorter.Options:UseXSharpRuntime, "VOConsoleClasses", "VulcanVOConsoleClasses")
			CASE "Terminal Lite"
				cGAC := IIF(xPorter.Options:UseXSharpRuntime, "VOConsoleClasses", "VulcanVOConsoleClasses")
			CASE "GUI Classes"
				cGAC := IIF(xPorter.Options:UseXSharpRuntime, "VOGUIClasses", "VulcanVOGUIClasses")
//				lGUI := TRUE
			CASE "Internet"
				cGAC := IIF(xPorter.Options:UseXSharpRuntime, "VOInternetClasses", "VulcanVOInternetClasses")
			CASE "RDD Classes"
				cGAC := IIF(xPorter.Options:UseXSharpRuntime, "VORDDClasses", "VulcanVORDDClasses")
			CASE "Report Classes"
				cGAC := IIF(xPorter.Options:UseXSharpRuntime, "VOReportClasses", "VulcanVOReportClasses")
			CASE "SQL Classes"
				cGAC := IIF(xPorter.Options:UseXSharpRuntime, "VOSQLClasses", "VulcanVOSQLClasses")
			CASE "System Classes"
				cGAC := IIF(xPorter.Options:UseXSharpRuntime, "VOSystemClasses", "VulcanVOSystemClasses")
			CASE "Win32 API Library"
				cGAC := IIF(xPorter.Options:UseXSharpRuntime, "VOWin32APILibrary", "VulcanVOWin32APILibrary")
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
			IF .NOT. lWin32API
			oApp:AddRuntimeReference(IIF(xPorter.Options:UseXSharpRuntime, "VOWin32APILibrary", "VulcanVOWin32APILibrary"))
			ENDIF
//		END IF

//		IF lAnySDK
/*			IF .not. String.IsNullOrWhiteSpace(SDKDefines_FileName) .and. File.Exists(SDKDefines_FileName)
				oApp:BrowseReferences:Add(SDKDefines_FileName)
			END IF*/
			IF .NOT. xPorter.Options:UseXSharpRuntime
				oApp:BrowseReferences:Add("%SDKDefinesFilename%")
			ENDIF
//		END IF

		oSource := System.Text.StringBuilder{}
		LOCAL oMefList AS List<Fab_VO_Entities.FabMEFFile>
		oMefList := oAef:ModuleListList
		LOCAL nCount AS INT
		nCount := oMefList:Count
//		xPorter.uiForm:SetProgressBarRange(nCount)
		FOR LOCAL n := 0 AS INT UPTO oMefList:Count - 1
			oMef := oMefList[n]
			IF oApp:xPortOptions:SortEntitiesByName
				oMef:SortByName()
			ENDIF
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
	METHOD AddModule(cName AS STRING , aCode AS STRING[]) AS ModuleDescriptor
		LOCAL oModule AS ModuleDescriptor
		cName := cName:TrimStart()
		FOREACH c AS CHAR IN e"/?:&\*\"<>|#%"
			cName := cName:Replace(c,'_')
		NEXT
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
			IF cFileName:ToUpper():EndsWith("Funcs Coder.prg":ToUpper()) .AND. File.Exists(cFolder + "\FuncsDeCoder.prg")
				LOOP
			END IF
			SELF:AddModuleFromFile(cFileName)
		NEXT

		SELF:_lLoaded := TRUE

	RETURN

    METHOD CopyWedFiles(cFolder AS STRING) AS VOID
        // Copy INF and TPL files to properties folder
        VAR  cVOFolder := System.IO.Path.Combine(VoFolder.Get(),"Bin")
        Directory.CreateDirectory(cFolder+"\Properties")
        VAR aFiles := List<STRING>{}
        aFiles:Add("cavowed.inf")
        aFiles:Add("cavowed.tpl")
        aFiles:Add("cavoded.tpl")
        aFiles:Add("cavofed.tpl")
        FOREACH VAR cFile IN aFiles
            VAR cSource := System.IO.Path.Combine(cVoFolder, cFile)
            VAR cDest   := System.IO.Path.Combine(cFolder+"\Properties", cFile)
            IF System.IO.File.Exists(cSource) .AND. ! System.IO.File.Exists(cDest)
                System.IO.File.Copy(cSource, cDest, TRUE)
                SELF:_aOtherFiles:Add("Properties\"+System.IO.Path.GetFileName(cDest))
            ENDIF
            IF cFile == "cavowed.inf" .AND. System.IO.File.Exists(cSource)
                // open file and read [SupplementalFiles] section
                VAR aLines := System.IO.File.ReadAllLines(cSource)
                VAR lSupplemental := FALSE
                FOREACH VAR cLine IN aLines
                    IF cLine:Trim():ToUpper() == "[SUPPLEMENTALFILES]"
                        lSupplemental := TRUE
                    ELSEIF cLine:Trim():Startswith("[")
                        // end of section
                        lSupplemental := FALSE
                    ENDIF
                    IF lSupplemental
                        IF cLine:Trim():ToUpper():Startswith("FILE")
                            VAR aElements := cLine:Trim():Split(<CHAR>{'='})
                            IF aElements:Length >= 2
                                VAR cAdditional := aElements[2]
                                cSource := System.IO.Path.Combine(cVoFolder, cAdditional)
                                cDest   := System.IO.Path.Combine(cFolder+"\Properties", cAdditional)
                                IF System.IO.File.Exists(cSource) .AND. ! System.IO.File.Exists(cDest)
                                    System.IO.File.Copy(cSource, cDest, TRUE)
                                ENDIF
                                IF System.IO.File.Exists(cDest)
                                    SELF:_aOtherFiles:Add("Properties\"+cAdditional)
                                ENDIF
                            ENDIF
                        ENDIF
                    ENDIF

                NEXT

            ENDIF

        NEXT

	METHOD Generate() AS VOID
		LOCAL cFolder AS STRING
		cFolder := SELF:AppFolder

		xPorter.Message("xPorting application", SELF:Name)

		SELF:ResolveReferences()

		Directory.CreateDirectory(cFolder)
		Directory.CreateDirectory(cFolder+"\Resources")
        SELF:CopyWedFiles(cFolder)
		FOREACH oModule AS ModuleDescriptor IN SELF:_aModules

			xPorter.uiForm:AdvanceProgressbar()

			IF .NOT. xPorter.AllowModule(oModule)
				LOOP
			END IF

			LOCAL oCode AS OutputCode
			oCode := oModule:Generate()

			IF oModule:Generated
				xPorter.Message("  Generating module :" , oModule:Name)
				
				IF glCreateFilePerClass
					LOCAL cModuleSubFolder AS STRING
					cModuleSubFolder := MakePathLegal( oModule:Name )
					Directory.CreateDirectory(cFolder + "\" + cModuleSubFolder)
					FOREACH oClass AS KeyValuePair<STRING, OutputCode> IN oModule:GeneratedClasses
						LOCAL cFileName AS STRING
						cFileName := MakePathLegal( oClass:Key ) + ".prg"
						File.WriteAllLines(cFolder + "\" + cModuleSubFolder + "\" + cFileName , oClass:Value:Lines , System.Text.Encoding.UTF8)
						oModule:GeneratedSubFiles:Add(cModuleSubFolder + "\" + cFileName)
					NEXT
					
					LOCAL oNonClass AS OutputCode
					oNonClass := OutputCode{}
					oNonClass:Combine(oModule:GeneratedDefines)
					oNonClass:Combine(oModule:GeneratedGlobals)
					oNonClass:Combine(oModule:GeneratedFuncs)
					oNonClass:Combine(oModule:GeneratedRest)
					IF oNonClass:Lines:Count != 0
						LOCAL cFileName AS STRING
						cFileName := "(Non-class)" + ".prg"
						File.WriteAllLines(cFolder + "\" + cModuleSubFolder + "\" + cFileName , oNonClass:Lines , System.Text.Encoding.UTF8)
						oModule:GeneratedSubFiles:Add(cModuleSubFolder + "\" + cFileName)
					END IF
					
					LOOP
					
				ELSE
					File.WriteAllLines(cFolder + "\" + oModule:PathValidName + ".prg" , oCode:GetContents() , System.Text.Encoding.UTF8)
				ENDIF
				
			END IF

			IF xPorter.ExportToXide
				TRY
					LOCAL cWedFile AS STRING
					cWedFile := cFolder + "\" + oModule:PathValidName + ".prg.wed"
					IF File.Exists(cWedFile)
						File.Delete(cWedFile)
					END IF
				CATCH
					NOP
				END TRY
			ENDIF

			BEGIN SCOPE // designers
				LOCAL oModuleFieldSpecs AS XSharp.VOEditors.VOFieldSpecDescription
				LOCAL aXideFieldSpecs AS List<STRING>
				LOCAL aXideDBServers AS List<STRING>
				LOCAL aFilesToDel AS List<STRING>
				LOCAL cModule AS STRING
				LOCAL cPrg AS STRING
				cModule := cFolder + "\" + oModule:PathValidName
				cPrg := cModule + ".prg"
				oModuleFieldSpecs := XSharp.VOEditors.VOFieldSpecDescription{}
				aXideFieldSpecs := List<STRING>{}
				aXideDBServers := List<STRING>{}
				aFilesToDel := List<STRING>{}

				LOCAL aDBServers AS SortedList<STRING,BYTE[]>
				aDBServers := SortedList<STRING,BYTE[]>{}

				FOREACH oDesigner AS Designer IN oModule:Designers
					IF oDesigner:MustExport .or. oDesigner:IsDedHelper
						LOCAL cBinary AS STRING
						cBinary := cModule + "." + oDesigner:FileName
						IF oDesigner:Type == BINARY_FED // FieldSpec
							oModuleFieldSpecs:LoadFromBinary(oDesigner:Bytes, oDesigner:Name)
						ELSEIF oDesigner:Type == BINARY_FLD
//							MessageBox.Show(oDesigner:Name , "FIELD")
							XSharp.VOEditors.DBServerBinary.Add(oDesigner:Name , XSharp.VOEditors.DBServerItemType.Field , oDesigner:Bytes)
						ELSEIF oDesigner:Type == BINARY_IND
//							MessageBox.Show(oDesigner:Name , "INDEX")
							XSharp.VOEditors.DBServerBinary.Add(oDesigner:Name , XSharp.VOEditors.DBServerItemType.Index , oDesigner:Bytes)
						ELSEIF oDesigner:Type == BINARY_ORD
//							MessageBox.Show(oDesigner:Name , "ORDER")
							XSharp.VOEditors.DBServerBinary.Add(oDesigner:Name , XSharp.VOEditors.DBServerItemType.Order , oDesigner:Bytes)
						ELSEIF oDesigner:Type == BINARY_DED
//							MessageBox.Show(oDesigner:Name , "DBSERVER")
							aDBServers:Add(cBinary, oDesigner:Bytes)
						ELSEIF oDesigner:Type == BINARY_WED
							IF xPorter.ExportWedToXml
								BinaryEntity.SaveWindowToXml(cBinary, oDesigner:Bytes)
							ELSE
								File.WriteAllBytes(cBinary , oDesigner:Bytes)
							END IF
						ELSEIF oDesigner:Type == BINARY_MED
							IF xPorter.ExportWedToXml
								BinaryEntity.SaveMenuToXml(cBinary, oDesigner:Bytes)
							ELSE
								File.WriteAllBytes(cBinary , oDesigner:Bytes)
							END IF
						ELSE
							// Necessary for both VS and XIDE, deleteded later if not needed anymore for VS
							File.WriteAllBytes(cBinary , oDesigner:Bytes)
						ENDIF
						IF xPorter.ExportToXide
							TRY
								DO CASE
								CASE oDesigner:Type == BINARY_WED
//									VOWindowEditor.ProjectImportVNFrm(cPrg , cBinary)
									BinaryEntity.SaveWindowToWed(oDesigner:Bytes, cPrg , cBinary)
								CASE oDesigner:Type == BINARY_MED
//									VOMenuEditor.ProjectImportVNMnu(cPrg , cBinary)
									BinaryEntity.SaveMenuToWed(oDesigner:Bytes, cPrg , cBinary)
								CASE oDesigner:Type == BINARY_FED
									File.WriteAllBytes(cBinary , oDesigner:Bytes)
									aXideFieldSpecs:Add(cBinary)
									aFilesToDel:Add(cBinary)
								CASE oDesigner:Type == BINARY_DED
									File.WriteAllBytes(cBinary , oDesigner:Bytes)
									aXideDBServers:Add(cBinary)
								CASE oDesigner:IsDedHelper
									File.WriteAllBytes(cBinary , oDesigner:Bytes)
									aFilesToDel:Add(cBinary)
								END CASE
							CATCH
								NOP
							END TRY
						END IF
						IF xPorter.GenerateWinForms
							IF oDesigner:Type == BINARY_WED
								LOCAL cWF AS STRING
								cWf := SELF:Project:WinFormsFolder + "\tmp\" + SELF:Name + "." + oDesigner:Name
								SafeFileDelete(cWf + ".wed")
								BinaryEntity.SaveWindowToWed(oDesigner:Bytes, cWf , cBinary)
							END IF
						END IF
						IF .not. xPorter.ExportToVS
							aFilesToDel:Add(cBinary)
						END IF
					ENDIF
				NEXT

				// FieldSpecs:
				IF .not. oModuleFieldSpecs:IsEmpty
					IF xPorter.ExportToVS
						LOCAL cBinary AS STRING
						cBinary := cFolder + "\" + oModule:PathValidName + ".FieldSpecs.xsfs"
						oModuleFieldSpecs:SaveToDocument(cBinary)
					END IF
					IF xPorter.ExportToXide .and. aXideFieldSpecs:Count != 0
						VOFieldSpecEditor.ProjectImportVNFs(cPrg , aXideFieldSpecs:ToArray())
					END IF
				END IF

				// DBServers:
				IF xPorter.ExportToVS
					FOREACH oDBServer AS KeyValuePair<STRING,BYTE[]> IN aDBServers
						LOCAL oDBDescr AS XSharp.VOEditors.VODBServerDescription
						oDBDescr := XSharp.VOEditors.VODBServerDescription.LoadFromBinary(oDBServer:Value)
						oDBDescr:SaveToDocument(oDBServer:Key)
					NEXT
				END IF
				IF xPorter.ExportToXide .and. aXideDBServers:Count != 0
					FOREACH cDBServer AS STRING IN aXideDBServers
						VODBServerEditor.ProjectImportVNDbs(cPrg , cDBServer)
					NEXT
				ENDIF
				FOREACH cFileName AS STRING IN aFilesToDel
					SafeFileDelete(cFileName)
				NEXT
			END SCOPE

			IF .NOT. xPorter.Options:ExportOnlyDefines
				LOCAL aResources AS SortedList<STRING,OutputCode>
				aResources := oModule:GenerateResources()
				IF aResources:Count != 0
					LOCAL oXideResources, oWedResources AS OutputCode
					oXideResources := OutputCode{}
					oWedResources := OutputCode{}
                    FOREACH oPair AS KeyValuePair<STRING , OutputCode> IN aResources

						LOCAL cName, cUpperName AS STRING
                        LOCAL cRcSource AS STRING
						cName := oPair:Key
						cUpperName := cName:ToUpperInvariant()

						#warning Need to investigate, the following never executes
						DO CASE
						CASE cUpperName:StartsWith("IDM_")
							cName := cName:Substring(4)
							cUpperName := cName:ToUpperInvariant()
						CASE cUpperName:ToUpper():StartsWith("IDA_")
							cName := cName:Substring(4) + "_Accelerator"
							cUpperName := cName:ToUpperInvariant()
						END CASE

						// For VS:
						IF xPorter.ExportToVS
							LOCAL cResFileName AS STRING
							cResFileName := oModule:PathValidName + "." + cName + ".rc"
	                        VAR aContents := oPair:Value:GetContents():ToArray()

	                        LOCAL nLineWithResource := 1 AS INT
	                        IF aContents[1]:StartsWith("#define") .and. aContents:Length >= 3 .and. aContents[3]:StartsWith("CREATEPROCESS_MANIFEST_RESOURCE_ID")
	                        	// line 1 and 2 are #defines for CREATEPROCESS_MANIFEST_RESOURCE_ID and RC_RT_MANIFEST
	                        	nLineWithResource := 3
	                        END IF

	                        cRcSource     := aContents[nLineWithResource]
							cRcSource := SELF:AdjustResource(cRcSource,cFolder,TRUE)
							aContents[nLineWithResource] := cRcSource

							File.WriteAllLines(cFolder + "\" + cResFileName , aContents , System.Text.Encoding.Default)
							oModule:AddVSrc(cResFileName)
						END IF

						// For XIDE:
						IF xPorter.ExportToXide
							LOCAL lWedRc := FALSE AS LOGIC
							FOREACH oDesigner AS Designer IN oModule:Designers
								IF cUpperName == oDesigner:Name:ToUpperInvariant() .OR. cUpperName == oDesigner:Name:ToUpperInvariant() + "_ACCELERATOR"
									lWedRc := TRUE
									EXIT
								END IF
							NEXT
							IF lWedRc
								oWedResources:Combine(oPair:Value)
							ELSE
								oXideResources:Combine(oPair:Value)
							END IF
						END IF

					NEXT

					// For XIDE:
					IF xPorter.ExportToXide
						IF TRUE // if we generate XIDE binaries
							LOCAL cResFileName AS STRING
							cResFileName := oModule:PathValidName + ".rc"
							IF .NOT. oXideResources:IsEmpty()
	                            LOCAL aResult   := List<STRING>{} AS List<STRING>
	                            FOREACH cLinex AS STRING IN oXideResources:GetContents():ToArray()
	                            	// in XIDE, all resource of one file are included in a single
	                            	// buffer, so we need to check every line for resource markers
	                            	// Probably need to redesign this...
	                            	VAR cLine := SELF:AdjustResource(cLinex, cFolder , FALSE)
	                                aResult:Add(cLine)
	                            NEXT
								File.WriteAllLines(cFolder + "\" + cResFileName , aResult , System.Text.Encoding.Default)
								oModule:AddXIDErc(cResFileName)
							END IF

							IF .NOT. oWedResources:IsEmpty()
								cResFileName := oModule:PathValidName + ".prg.rc"
								File.WriteAllLines(cFolder + "\" + cResFileName , oWedResources:GetContents() , System.Text.Encoding.Default)
							END IF
	/*					ELSE // otherwise make it simple:
							oXideResources:Combine(oWedResources)
							cResFileName := oModule:PathValidName + ".rc"
							File.WriteAllLines(cFolder + "\" + cResFileName , oXideResources:GetContents() , System.Text.Encoding.Default)
							oModule:AddXIDErc(cResFileName)*/
						END IF
					END IF
				END IF
			END IF

		NEXT


		SELF:_lSaved := TRUE

		IF xPorter.ExportToXide
			SELF:CreateAppFile(cFolder , TRUE)
		ENDIF
		IF xPorter.ExportToVS
			SELF:CreateAppFile(cFolder , FALSE)
		ENDIF
	RETURN

    METHOD AdjustResource(cLine AS STRING,cFolder AS STRING,lAddToOtherFiles AS LOGIC) AS STRING
        LOCAL aElements AS STRING[]
        LOCAL lHasFile  AS LOGIC
        LOCAL cResourcesFolder := cFolder+"\Resources" AS STRING
        aElements := cLine:Split(<CHAR>{' '},StringSplitOptions.RemoveEmptyEntries)
        IF aElements:Length > 2
            // Element 1 = Name
            // Element 2 = Type
            // Element 3 = FileName
            SWITCH aElements[2]:ToUpper()
            CASE "DIALOG"
            CASE "DIALOGEX"
            CASE "MENU"
            CASE "MENUEX"
            CASE "VERSIONINFO"
            CASE "STRINGTABLE"
                lHasFile := FALSE
            CASE "BITMAP"
            CASE "ICON"
            CASE "CURSOR"
            CASE "RC_RT_MANIFEST"
                lHasFile := TRUE
            CASE "24" // RC_RT_MANIFEST
                lHasFile := cLine:StartsWith("1 24") // "CREATEPROCESS_MANIFEST_RESOURCE_ID RC_RT_MANIFEST" with defines already replaced with literals
            OTHERWISE
                lHasFile := FALSE
            END SWITCH
            IF lHasFile
            	// space below is needed, to make sure we do not get the first "icon" in: myicon icon "filename"
                LOCAL nPos       := cLine:IndexOf(" " + aElements[2]) + aElements[2]:Length + 1 AS INT
                LOCAL cFileName  := cLine:SubString(nPos) AS STRING
                LOCAL lHasQuotes := FALSE AS LOGIC
                cFileName := cFileName:Trim()
                IF cFileName:StartsWith(e"\"") .AND. cFileName:EndsWith(e"\"") .AND. cFileName:Length > 2
                	lHasQuotes := TRUE
                    cFileName := cFileName:Substring(1, cFileName:Length-2)
                ENDIF
                IF SafeFileExists(cFileName)
					LOCAL cDestName AS STRING
                	IF xPorter.Options:CopyResourcesToProjectFolder
						cDestName := Path.Combine(cResourcesFolder, Path.GetFileName(cFileName))
	                    IF ! SafeFileExists(cDestName)
	                        File.Copy(cFileName, cDestName)
	                    ENDIF
	                    // change to relative path
	                    cDestName := cDestName:Replace(cFolder+"\", "")
	                    IF lAddToOtherFiles
		                    SELF:_aOtherFiles:Add(cDestName)
	                    END IF
                	ELSE
                		cDestName := Path.GetFullPath(cFileName)
                	ENDIF
                	// always surround filenames with quotes, to make sure we have no issues with spaces in filenames
					IF .NOT. lHasQuotes
						cDestName := e"\"" + cDestName + e"\""
					ENDIF
					// when filenames are inside quotes, then backslashes need to be doubled, otherwise are being seen as escape sequences
					cDestName := cDestName:Replace("\" , "\\")
                    cLine  := cLine:Replace(cFileName, cDestName)
                ENDIF
            ENDIF
        ENDIF
       RETURN cLine //:TrimEnd() // why trim?

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

		IF .NOT. xPorter.OverWriteProjectFiles
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
					IF .NOT. oModule:Generated
						IF .not. SELF:_lIsWinForms
							LOOP
						END IF
					END IF
					IF lXide .and. oModule:IsDesignerChild
						LOOP
					END IF
					LOCAL cName AS STRING
					cName := oModule:PathValidName + ".prg"
					IF lXide
						IF glCreateFilePerClass
							LOCAL cFolderGuid AS STRING
							cFolderGuid := NewGuid()
							oOutput:WriteLine("FileFolder = " + oModule:PathValidName)
							oOutput:WriteLine("FileGroupGUID = " + cFolderGuid)
							FOREACH cFileName AS STRING IN oModule:GeneratedSubFiles
								oOutput:WriteLine("File = %AppPath%\" + cFileName)
//								oOutput:WriteLine("File = " + cFileName)
								oOutput:WriteLine("FileGUID = " + NewGuid())
								oOutput:WriteLine("FileFileGroup = " + cFolderGuid)
								oOutput:WriteLine("FileType = Code")
							NEXT
						ELSE
							oOutput:WriteLine("File = %AppPath%\" + cName)
							oOutput:WriteLine("FileGUID = " + NewGuid())
							oOutput:WriteLine("FileType = Code")
							IF oModule:HasDesignerChild
								oOutput:WriteLine("DesignerFileGUID = " + NewGuid())
							END IF
						END IF
					ELSE
						IF glCreateFilePerClass
							FOREACH cFileName AS STRING IN oModule:GeneratedSubFiles
								oOutput:WriteLine(String.Format(e"<Compile Include=\"{0}\">" , cFileName))
								oOutput:WriteLine("</Compile>")
							NEXT
						ELSE
							oOutput:WriteLine(String.Format(e"<Compile Include=\"{0}\">" , cName))
							IF SELF:_lIsWinForms
								IF oModule:IsDesignerChild
									oOutput:WriteLine(String.Format("  <DependentUpon>{0}</DependentUpon>", cName:Replace(".Designer.",".")) )
								ELSE
									oOutput:WriteLine("  <SubType>Form</SubType>")
								END IF
							ELSE
								oOutput:WriteLine("  <SubType>Code</SubType>")
							END IF
							oOutput:WriteLine("</Compile>")
						ENDIF
					END IF
					
					IF glCreateFilePerClass
						LOOP
					ENDIF

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
							LOCAL lAddedFieldSpec := FALSE AS LOGIC
							FOREACH oDesigner AS Designer IN oModule:Designers
								IF oDesigner:MustExport
									LOCAL cFileName AS STRING
									IF oDesigner:Type == 14 // FieldSpec
										IF lAddedFieldSpec
											LOOP
										ELSE
											lAddedFieldSpec := TRUE
										END IF
										cFileName := oModule:PathValidName + ".FieldSpecs" + oDesigner:Extension
									ELSE
										cFileName := oModule:PathValidName + "." +oDesigner:Name+oDesigner:Extension
									END IF
									oOutput:WriteLine(String.Format(e"<VOBinary Include=\"{0}\">" , cFileName))
									oOutput:WriteLine(String.Format(e"  <DependentUpon>{0}</DependentUpon>" , cName))
									oOutput:WriteLine(String.Format(e"</VOBinary>" , ""))
								ENDIF
							NEXT
						END IF
                    END IF

				NEXT

	            FOREACH VAR cLine IN SELF:_aOtherFiles
	                oOutPut:WriteLine(String.Format(e"<None Include=\"{0}\" />", cLine:Trim()))
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

				IF cTemplate:Contains("%option_overflow%") .OR. cTemplate:Contains("%option_intdiv%")
					cTemplate := cTemplate:Replace("%option_overflow%" , IIF(SELF:OptionOverflow , IIF(lXide , "1" , "true") , IIF(lXide , "0" , "false") ) )
					cTemplate := cTemplate:Replace("%option_intdiv%" , IIF(SELF:OptionIntDiv , IIF(lXide , "1" , "true") , IIF(lXide , "0" , "false") ) )
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
				IF oClass:Name:ToUpper() == cClass .AND. oClass:HasDeclaration
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
	
	PROTECT _aGeneratedSubFiles AS List<STRING>

	CONSTRUCTOR(cName AS STRING , oApp AS ApplicationDescriptor , aCode AS STRING[])
		SELF:_cName := cName
		SELF:_aLines := List<LineObject>{}
		SELF:_oApp := oApp
		SELF:_aClasses := List<ClassDescriptor>{}
		SELF:_aEntities := List<EntityDescriptor>{}
		SELF:_aDefines := SortedList<STRING , STRING>{}
		SELF:_aConstructors := List<STRING>{}
		SELF:Adjust_VXP_Tags(aCode)
		FOREACH cLine AS STRING IN aCode
			SELF:_aLines:Add(LineObject{cLine})
		NEXT
		SELF:_aVSrc := List<STRING>{}
		SELF:_cXIDErc := NULL

		SELF:_aDesigners := List<Designer>{}
		SELF:_aGeneratedSubFiles := List<STRING>{}
	RETURN

	PROPERTY Name AS STRING GET SELF:_cName
	PROPERTY PathValidName AS STRING GET MakePathLegal( SELF:_cName )
	PROPERTY Classes AS List<ClassDescriptor> GET SELF:_aClasses
	PROPERTY Application AS ApplicationDescriptor GET SELF:_oApp
	PROPERTY Generated AS LOGIC GET SELF:_lGenerated
	PROPERTY VSrc AS List<STRING> GET SELF:_aVSrc
	PROPERTY XIDErc AS STRING GET SELF:_cXIDErc
	PROPERTY HasDesignerChild AS LOGIC AUTO
	PROPERTY IsDesignerChild AS LOGIC AUTO
	PROPERTY GeneratedSubFiles AS List<STRING> GET SELF:_aGeneratedSubFiles

	PROPERTY Designers AS List<Designer> GET SELF:_aDesigners

	METHOD SetDummyApp() AS VOID
		SELF:_oApp := ApplicationDescriptor{"Dummy",NULL}
	RETURN


	METHOD HasDefine(cDefine AS STRING) AS LOGIC
	RETURN SELF:_aDefines:ContainsKey(cDefine)
	METHOD GetDefineValue(cDefine AS STRING) AS STRING
	RETURN SELF:_aDefines[cDefine]

	PROTECTED METHOD Adjust_VXP_Tags(aCode AS STRING[]) AS VOID
		FOR LOCAL n := 1 AS INT UPTO aCode:Length
			LOCAL cLine, cUpper AS STRING
			LOCAL nAt AS INT
			cLine := aCode[n]
			cUpper := cLine:ToUpperInvariant()
			nAt := nAt := cUpper:IndexOf("VXP-")
			IF nAt != -1
				LOCAL nCommentMarker := -1 AS INT
				LOCAL nTest := 0 AS INT
				nTest := nAt
				DO WHILE nTest > 0
					nTest --
					IF cLine[nTest] == '/' .AND. cLine[nTest + 1] == '/'
						nCommentMarker := nTest
						DO WHILE nCommentMarker > 0 .AND. (cLine[nCommentMarker - 1] == ' ' .OR. cLine[nCommentMarker - 1] == '\t')
							nCommentMarker --
						END DO
						EXIT
					END IF
				END DO
				IF nCommentMarker != -1
					DO CASE
					CASE cUpper:IndexOf("VXP-DEL") == nAt
						aCode[n] := ""
					CASE cUpper:IndexOf("VXP-COM") == nAt
						cLine := "// " + cLine:Substring(0 , nCommentMarker)
						aCode[n] := cLine
					CASE cUpper:IndexOf("VXP-UNC") == nAt
						cLine := cLine:Substring(0 , nCommentMarker)
						nCommentMarker := cLine:IndexOf("//")
						IF nCommentMarker != -1
							cLine := cLine:Substring(0,nCommentMarker) + cLine:Substring(nCommentMarker + 2)
							aCode[n] := cLine
						END IF
					END CASE
				END IF
			END IF
		NEXT
	RETURN

	METHOD ContainsEntity(cName AS STRING, cClass AS STRING) AS LOGIC
		cName := cName:ToUpper()
		cClass := cClass:ToUpper()
		FOREACH oClass AS ClassDescriptor IN SELF:Classes
			IF oClass:Name:ToUpper() == cClass
				FOREACH oEntity AS EntityDescriptor IN oClass:Members
					IF oEntity:Name:ToUpper() == cName
						RETURN TRUE
					END IF
				NEXT
			END IF
		NEXT

	RETURN FALSE

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
				IF .NOT. xPorter.AllowEntity(oLine:oEntity , SELF)
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
				IF oLine:oEntity:eType == EntityType._Function .OR. oLine:oEntity:eType == EntityType._Procedure
					SELF:_oApp:AddFunction(oLine:oEntity:cName:ToUpper())
				ENDIF

				IF xPorter.Options:AddMissingConstrutors
					IF oLine:oEntity:eType == EntityType._Method .AND. oLine:oEntity:cName:ToUpper() == "INIT"
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

	PROPERTY GeneratedClasses AS SortedList<STRING, OutputCode> AUTO
	PROPERTY GeneratedDefines AS OutputCode AUTO
	PROPERTY GeneratedGlobals AS OutputCode AUTO
	PROPERTY GeneratedFuncs AS OutputCode AUTO
	PROPERTY GeneratedRest AS OutputCode AUTO

	METHOD Generate() AS OutputCode
		LOCAL oCode AS OutputCode
		LOCAL oClasses , oDefines , oTextblocks , oGlobals , oFuncs , oRest AS OutputCode

		//STATIC oConvertFromCodePageToCodePage := NULL AS EntityDescriptor

		oClasses := OutputCode{}
		oDefines := OutputCode{}
		oTextblocks := OutputCode{}
		oGlobals := OutputCode{}
		oFuncs := OutputCode{}
		oRest := OutputCode{}
		
		SELF:GeneratedClasses := SortedList<STRING, OutputCode> {SELF:_aClasses:Count}

		FOREACH oClassDescr AS ClassDescriptor IN SELF:_aClasses
			LOCAL oClass AS OutputCode
			oClass := oClassDescr:Generate()
			oClasses:Combine(oClass)
			SELF:GeneratedClasses:Add(oClassDescr:Name, oClass)
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
				IF xPorter.Options:ExportOnlyDefines .AND. oEntity:IsStatic
					LOOP
				END IF
				oDefines:Combine(oEntity:Generate())
			CASE oEntity:Type == EntityType._Global
				oGlobals:Combine(oEntity:Generate())
			CASE oEntity:Type == EntityType._Function .OR. oEntity:Type == EntityType._Procedure
				IF SELF:_oApp:ContainsCallback(oEntity:Name)
					oFuncs:Combine(oEntity:GenerateDelegateFromFunction())
				END IF
				oFuncs:Combine(oEntity:Generate())
			OTHERWISE
				oRest:Combine(oEntity:Generate())
			END CASE
		NEXT
		
		SELF:GeneratedFuncs := oFuncs
		SELF:GeneratedGlobals := oGlobals
		SELF:GeneratedDefines := oDefines
		SELF:GeneratedRest := oRest

		oCode := OutputCode{}

		IF xPorter.Options:ExportOnlyDefines
			oCode:Combine(oDefines)
		ELSE
			oCode:Combine(oTextblocks)
			IF .NOT. oDefines:IsEmpty()
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

		IF .NOT. (xPorter.Options:DontGenerateEmptyFiles .AND. oCode:IsEmpty() .AND. .NOT. SELF:HasResources())
			SELF:_lGenerated := TRUE
		ENDIF

	RETURN oCode

	METHOD HasResources() AS LOGIC
		FOREACH oEntity AS EntityDescriptor IN SELF:_aEntities
			DO CASE
			CASE oEntity:Type == EntityType._Resource
/*				IF oEntity:Name:ToUpper() == "VS_VERSION_INFO"
					LOOP
				END IF*/
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
/*				IF oEntity:Name:ToUpper() == "VS_VERSION_INFO"
					LOOP
				END IF*/
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
	PROTECT _aNonPublicIVars AS Dictionary<STRING,STRING>
	CONSTRUCTOR(cName AS STRING , oModule AS ModuleDescriptor)
		SELF:_cName := cName
		SELF:_oModule := oModule
		SELF:_aMembers := List<EntityDescriptor>{}
		SELF:_aNonPublicIVars := Dictionary<STRING,STRING>{}
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
		oEntity:ClassDesc := SELF
	RETURN

	METHOD DeclareNonPublicIVar(cIVar AS STRING) AS VOID
		IF .not. SELF:_aNonPublicIVars:ContainsKey(cIVar:ToUpper())
			SELF:_aNonPublicIVars:Add(cIVar:ToUpper() , cIVar)
		END IF
	RETURN
	METHOD HasNonPublicIVar(cIVar AS STRING) AS LOGIC
	RETURN SELF:_aNonPublicIVars:ContainsKey(cIVar:ToUpper())

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
				IF SELF:_oDeclaration:HasInherit .AND. .NOT. SELF:_oModule:Application:ConstructorExists(SELF:Name)
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
				oCode:AddLine(e"\tCATCH oException AS Exception")
				IF xPorter.Options:UseXSharpRuntime
					oCode:AddLine(e"\t\tErrorDialog(oException)")
				ELSE
					// not calling ErrorDialog(), because the vulcan version needs a Windows.Forms reference
					oCode:AddLine(e"\t\tLOCAL cMessage AS STRING")
					oCode:AddLine(e"\t\tcMessage := oException:Message")
					oCode:AddLine(e"\t\tDO WHILE oException:InnerException != NULL_OBJECT")
					oCode:AddLine(e"\t\t\toException := oException:InnerException")
					oCode:AddLine(e"\t\t\tcMessage += CRLF+oException:Message")
					oCode:AddLine(e"\t\tENDDO")
					oCode:AddLine(e"\t\tErrorBox{NIL, cMessage}:Show()")
				END IF
				oCode:AddLine(e"\tEND TRY")
				oCode:AddLine(e"RETURN 0")
				oCode:AddLine(e"")
				oCode:AddLine(IIF(lPartial , "PARTIAL " , "") + "CLASS " + SELF:_cName + " INHERIT App")
			ELSE
				IF SELF:_oModule:Application:ClassDeclarationExists(SELF:Name) .or. xPorter.ExportingSingleFile
					oCode:AddLine(IIF(lPartial , "PARTIAL " , "") + "CLASS " + SELF:_cName)
				ELSE
					oCode:AddLine("#warning The following method did not include a CLASS declaration")
					oCode:AddLine(IIF(lPartial , "PARTIAL " , "") + "CLASS " + SELF:_cName + "_external_class" + " INHERIT " + SELF:_cName)
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
	PROPERTY ClassName AS STRING GET SELF:_cClass
	PROPERTY ClassDesc AS ClassDescriptor AUTO
	PROPERTY Lines AS List<LineObject> GET SELF:_aLines
	PROPERTY IsClassOrMember AS LOGIC GET;
			SELF:_eType == EntityType._Class .OR. SELF:_eType == EntityType._Method .OR. ;
			SELF:_eType == EntityType._Access .OR. SELF:_eType == EntityType._Assign
	PROPERTY PartialClass AS LOGIC AUTO
	PROPERTY IsStatic AS LOGIC GET SELF:_lIsStatic
	PROPERTY HasInherit AS LOGIC GET .NOT. (System.String.IsNullOrWhiteSpace(SELF:_cInherit) .OR. SELF:_cInherit:ToUpper() == "VOBJECT")

	METHOD AddLine(oLine AS LineObject) AS VOID
		SELF:_aLines:Add(oLine)
	RETURN

	METHOD GenerateDelegateFromFunction() AS OutputCode
		LOCAL oCode AS OutputCode
		oCode := OutputCode{}
		FOREACH oLine AS LineObject IN SELF:_aLines
			IF oLine:oEntity == NULL .AND. .NOT. oLine:lInAmpersand
				EXIT
			END IF
			LOCAL cLine AS STRING
			cLine := oLine:LineText

//			cLine := cLine:Replace(" STRICT",""):Replace(" PASCAL",""):Replace("_WINCALL",""):Replace(" CALLBACK","")
			LOCAL aWords AS List<WordObject>
			LOCAL lAfterCloseParen := FALSE AS LOGIC
			aWords := EntityDescriptor.AnalyzeLine(cLine)
			cLine := ""
			FOREACH oWord AS WordObject IN aWords
				LOCAL cUpper AS STRING
				cUpper := oWord:cWord:ToUpper()
				IF cUpper == ")"
					lAfterCloseParen := TRUE // we're after the parameters list
				ELSEIF lAfterCloseParen
					IF cUpper == "STRICT" .OR. cUpper == "PASCAL" .OR. cUpper == "_WINCALL" .OR. cUpper == "CALLBACK"
						LOOP
					END IF
				ENDIF
				cLine += oWord:cWord
			NEXT


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

		LOCAL nIfDefLevel := 0 AS INT
		LOCAL lCommentEntity := FALSE AS LOGIC
		LOCAL oCurrentEntity := NULL AS EntityObject

		FOREACH oLine AS LineObject IN SELF:_aLines
			LOCAL cCallBack := NULL AS STRING
			LOCAL lAddEndif := FALSE AS LOGIC
			LOCAL cLine AS STRING

			IF xPorter.Options:IgnoreCodeInside_ifdef_endif .and. .not. (oLine:lInAmpersand .or. oLine:lInBlockComment)
				LOCAL lDoIgnore := FALSE AS LOGIC
				LOCAL cLower AS STRING
				cLine := oLine:LineText
				cLower := cLine:Trim():ToLowerInvariant()
				IF cLower:StartsWith("#ifdef") .or. cLower:StartsWith("#ifndef")
					nIfDefLevel ++
					lDoIgnore := TRUE
				ELSEIF cLower:StartsWith("#endif")
					nIfDefLevel --
					lDoIgnore := TRUE
				END IF
				IF nIfDefLevel > 0 .or. lDoIgnore
					oCode:AddLine(cLine)
					LOOP
				END IF
			END IF

			cLine := SELF:ConvertLine(oLine , REF cCallBack)
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

			IF xPorter.Options:RemoveDeclareMethod .AND. oCurrentEntity != NULL .AND. ;
				oCurrentEntity:eType == EntityType._Class
				#warning Need to improve DECLAREs recognition
				IF cLine:ToUpper():Contains("DECLARE METHOD") .OR. ;
					cLine:ToUpper():Contains("DECLARE ACCESS") .OR. ;
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

			#warning Need to improve recognition of Init/Axit/Super tokens
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
					IF oLine:oEntity:eType == EntityType._Method .AND. ;
						(oLine:oEntity:cName:ToUpper() == "INIT" .OR. oLine:oEntity:cName:ToUpper() == "AXIT")
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
				IF SELF:_eType == EntityType._Define .AND. String.IsNullOrWhiteSpace(cLine)
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
		LOCAL lInNonPublicIVar := FALSE AS LOGIC
		LOCAL lCanBeNonPublicName := FALSE AS LOGIC

		cCallBack := NULL

		cLine := oLine:LineText
		cLineUpper := cLine:ToUpper():Trim()

		IF cLineUpper:StartsWith("VTRACE") .OR. cLineUpper:StartsWith("VMETHOD")
			cLine := "// " + cLine
			RETURN cLine
		ENDIF
		IF xPorter.Options:RemoveONLYEARLYpragmas .and. cLineUpper:StartsWith("~") .and. cLineUpper:Contains("ONLYEARLY") .and. .not. oLine:lInBlockComment
			cLine := "// " + cLine
			RETURN cLine
		ENDIF
		IF SELF:Type== EntityType._Class .AND. cLineUpper:StartsWith("INSTANCE")
			LOCAL cTemp AS STRING
			cTemp := cLineUpper:Trim()
			IF cTemp:Length > 9
				cTemp := cTemp:Substring(9):Trim()
				// The WED generates single instance vars per line, we do not want to touch user created ones
				IF cTemp:Length > 0 .AND. cTemp:IndexOf(',') == -1 .AND. cTemp:IndexOf(' ') == -1
					IF SELF:_oModule:ContainsEntity(cTemp, SELF:Name)
						cLine := "// " + cLine
						RETURN cLine
					END IF
				END IF
			END IF
		ENDIF
		IF xPorter.Options:CheckForIVarAndPropertyConflicts .and. SELF:Type== EntityType._Class .AND. ;
			(cLineUpper:StartsWith("PROTECT") .or. cLineUpper:StartsWith("HIDDEN") .or. cLineUpper:StartsWith("INSTANCE"))
			lInNonPublicIVar := TRUE
			lCanBeNonPublicName := TRUE
		ENDIF

		IF xPorter.Options:RemoveExportLocalClause .AND. oLine:HasExportLocalClause
			IF oLine:oMoreInfo:ExportLocalClause:Begins != 0
				IF oLine:oMoreInfo:ExportLocalClause:Ends != 0
					cLine := cLine:Substring(0 , oLine:oMoreInfo:ExportLocalClause:Begins - 1) + cLine:Substring(oLine:oMoreInfo:ExportLocalClause:Ends - 1)
				END IF
			END IF
		END IF
		IF xPorter.Options:RemoveClassClause .AND. oLine:HasClassClause
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

		IF SELF:PartialClass .AND. oLine:oEntity != NULL .AND. oLine:oEntity:eType == EntityType._Class
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

			LOCAL cWord AS STRING
			cWord := oWord:cWord

			DO CASE

			CASE cWord == "%" .AND. oNextNextWord != NULL .AND. oNextNextWord:cWord == "%" .AND. oNextWord:cWord:ToUpper() == "CAVOSAMPLESROOTDIR"
				cWord := ""
				oNextWord:cWord := VOFolder.Get() + "\Samples"
				oNextNextWord:cWord := ""

			CASE cWord == "." .AND. oNextNextWord != NULL .AND. oNextNextWord:cWord == "." .AND. (oNextWord:cWord:ToUpper() == "AND" .OR. oNextWord:cWord:ToUpper() == "OR") .AND. oWord:eStatus != WordStatus.Comment // can be literal, if the parser thinks 1.and.2 is a numeric
				cWord := " " + cWord
			CASE cWord == "." .AND. oPrevWord != NULL .AND. (oPrevWord:cWord:ToUpper() == "AND" .OR. oPrevWord:cWord:ToUpper() == "OR") .AND. oNextWord != NULL .AND. oNextWord:cWord:Trim():Length != 0 .AND. oWord:eStatus != WordStatus.Comment // as above
				cWord := cWord + " "

			CASE oWord:eStatus == WordStatus.Text // no literals or comments
				LOCAL cWordUpper AS STRING
				cWordUpper := cWord:ToUpper()
				DO CASE
				CASE cWordUpper:StartsWith("STRU") .AND. oWord:eSubStatus == WordSubStatus.TextReserved .AND. oLine:oEntity != NULL
					cWord := "VOSTRUCT"
				CASE xPorter.NewKeywordsInXSharp:ContainsKey(cWordUpper) .AND. (oPrevWord == NULL .OR. .NOT. oPrevWord:cWord:StartsWith("@"))
					cWord := "@@" + cWord
				CASE cWordUpper == "_NC" .OR. cWordUpper == "_CO"
					cWord := ""
				CASE cWordUpper == "@" .AND. (oNextWord != NULL .AND. SELF:_oModule:Application:ContainsCallback(oNextWord:cWord))
//					MessageBox.Show(oNextWord:cWord , "Callback!")
					cCallBack := oNextWord:cWord
				CASE lInNonPublicIVar
					IF .not. String.IsNullOrWhiteSpace(cWord)
						IF oWord:eSubStatus == WordSubStatus.TextReserved .and. cWordUpper == "AS"
							lInNonPublicIVar := FALSE
						ELSEIF Char.IsLetter(cWord[0])
							IF lCanBeNonPublicName .and. SELF:_oModule:ContainsEntity(cWord, SELF:Name)
								SELF:ClassDesc:DeclareNonPublicIVar(cWord)
								cWord := SELF:PrefixIVar(cWord)
							ENDIF
						ELSE
							// if it's a comma, then next word must be an IVar name. Otherwise it must be an assignment expression
							lCanBeNonPublicName := cWord == ","
						ENDIF
					END IF
				CASE oLine:oEntity == NULL .and. SELF:ClassHasNonPublicIVar(cWord)
					cWord := SELF:PrefixIVar(cWord)
				END CASE

			CASE oWord:eStatus == WordStatus.Literal
				IF lInBracketString
					IF cWord == "]"
						lInBracketString := FALSE
						cWord := e"\""
					ELSE
						IF cWord == e"\"" .OR. cWord == "\"
							cWord := "\" + cWord
						END IF
					END IF
				ELSE
					IF cWord == "["
						IF oPrevWord == NULL .OR. oPrevWord:eStatus != WordStatus.Literal
							lInBracketString := TRUE
							cWord := e"e\""
						END IF
					END IF
				ENDIF
			END CASE

			IF lInIndexedProperty .AND. oWord:eStatus == WordStatus.Text .AND. oWord:cWord == ","
				sLine:Append('[')
				lInIndexedProperty := FALSE
				lLastWasColon := FALSE
				LOOP // don't add the first comma
			ENDIF
			IF lLastWasColon .AND. oWord:eStatus == WordStatus.Text .AND. oWord:cWord == "["
				lInIndexedProperty := TRUE
				lLastWasColon := FALSE
				LOOP // do not add the "[" now
			END IF
			IF oWord:eStatus == WordStatus.Text .AND. oWord:cWord == ":"
				lLastWasColon := TRUE
			ELSEIF .NOT. oWord:IsWhiteSpace
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

	PROTECTED METHOD PrefixIVar(cWord AS STRING) AS STRING
	RETURN "_" + cWord

	PROTECTED METHOD ClassHasNonPublicIVar(cWord AS STRING) AS LOGIC
		IF SELF:ClassDesc != NULL .and. SELF:ClassDesc:HasNonPublicIVar(cWord)
			RETURN TRUE
		ENDIF
	RETURN FALSE

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
		LOCAL lFirstLine := TRUE AS LOGIC
		LOCAL lStringTableLine := FALSE AS LOGIC
		LOCAL IMPLIED aDefines := Dictionary<STRING,STRING>{}

		LOCAL tag := "__T_A_G__" AS STRING

		oCode := OutputCode{}
		FOREACH oLine AS LineObject IN SELF:_aLines

			LOCAL cLine AS STRING
			//LOCAL lHasWizDir := FALSE AS LOGIC
			cLine := oLine:LineText
			//lHasWizDir := cLine:ToUpper():Contains("%APPWIZDIR%")
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
				cNextWord := IIF(nWord < aWords:Count - 1 , aWords[nWord+1]:cWord , "")

				IF cWord == ":" .AND. cNextWord == "\"
					lInFilename := TRUE
				END IF

				IF oLine:oEntity != NULL .AND. cUpper == "RESOURCE"
					cPrevWord := cWord
					LOOP
				END IF

				DO CASE

				CASE oWord:cWord == " " .OR. oWord:cWord == e"\t"
					NOP

				CASE oWord:eStatus == WordStatus.Text // no literals or comments
					STATIC LOCAL tag_version := tag + "VERSION" + tag AS STRING
					STATIC LOCAL tag_appwiz := tag + "APPWIZDIR" + tag AS STRING
					STATIC LOCAL tag_samples := tag + "CAVOSAMPLESROOTDIR" + tag AS STRING
					STATIC LOCAL tag_cavodir := tag + "CAVODIR" + tag AS STRING
					DO CASE
					CASE cUpper == "STRINGTABLE" .AND. lFirstLine
						lStringTableLine := TRUE
						EXIT
					CASE cUpper == "__VERSION__" .OR. cUpper == tag_version
						cWord := "1"
/*					CASE cUpper == "VS_VERSION_INFO"
						cWord := "1"*/
					CASE cUpper == "__APPWIZDIR__" .OR. cUpper == tag_appwiz
						cWord := VOFolder.Get() + "\Appwiz"
					CASE cUpper == "__CAVOSAMPLESROOTDIR__" .OR. cUpper == tag_samples
						cWord := VOFolder.Get() + "\Samples"
					CASE cUpper == "__CAVODIR__" .OR. cUpper == tag_cavodir
						cWord := VOFolder.Get()
//					CASE lFirstLine .and. (cPrevWord == "\" .or. cPrevWord == "." .or. cPrevWord == ":")
					CASE lFirstLine .AND. lInFilename
						// do not allow below to replace defines in part of filenames that are not surrounded by quotes
						NOP
					CASE SELF:_oModule:Application:HasDefine(cUpper)
						LOCAL cValue AS STRING
						cValue := SELF:TranslateDefineInResource(cUpper)
						IF xPorter.Options:ReplaceResourceDefinesWithValues
							cLine += cValue
						ELSE
							cLine += cWord
							IF .NOT. aDefines:ContainsKey(cUpper)
								aDefines:Add(cUpper , String.Format("#define {0} {1}" , cWord , cValue))
							END IF
						END IF
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
						LOCAL cValue AS STRING
						cValue := xPorter.SDKDefines[cUpper]
						IF xPorter.Options:ReplaceResourceDefinesWithValues .OR. (cValue:Contains("'") .OR. cValue:Contains('"'))
							#warning Need to review handling of VERSION resource
							// need to check where this is needed and when not
							// as it is now, it replaces defines for VERSION resource like VOVER_FILE_VERSION_TXT
							// from '""' as defined manually above, to an empty string without quotes
							cLine += cValue:Replace(e"\"" , ""):Replace("'" , "")
						ELSE
							cLine += cWord
							IF .NOT. aDefines:ContainsKey(cUpper)
								aDefines:Add(cUpper , String.Format("#define {0} {1}" , cWord , cValue))
							END IF
						ENDIF
						cPrevWord := cWord
						LOOP
					END CASE

				CASE oWord:eStatus == WordStatus.Literal
					IF cUpper == "_VOOLECONTAINER" .AND. .NOT. String.IsNullOrEmpty(Replace_VOOleContainer)
						cWord := Replace_VOOleContainer
					END IF

				END CASE

				IF .not. (cWord == " " .and. nWord == 1 .and. aWords[0]:cWord:ToUpperInvariant() == "RESOURCE") // do not include the space after the removed RESOURCE token
					cLine += cWord:Replace(tag , "%") // bring back the original text, if we have not modified it
				ENDIF
/*
In resource headers like:

RESOURCE IconEntity Icon "C:\some\folder\testico.ico"

when file paths are included in quotes we need to double the slashes, because those are escaped
strings and if there are escape sequences inside (like \t), the resource compiler will not find the file
Probably we need to do that also to every other string in every line in the resource?
*/
				IF lFirstLine .AND. cWord == "\" .AND. oWord:eStatus == WordStatus.Literal .AND. cPrevWord != "\" .AND. cNextWord != "\"
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

		LOCAL nLine := 0 AS INT
		FOREACH cLine AS STRING IN aDefines:Values
			oCode:InsertLine(cLine , nLine)
			nLine ++
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
	METHOD InsertLine(cLine AS STRING , nAfter AS INT) AS VOID
		SELF:_aLines:Insert(nAfter , cLine)
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
	RETURN SELF:_aLines:Count == 0 .OR. (SELF:_aLines:Count == 1 .AND. SELF:_aLines[0] == "")

END CLASS

FUNCTION NewGuid() AS STRING
RETURN System.Guid.NewGuid():ToString():ToUpper()

FUNCTION GetRelativePath(cSourceDir AS STRING , cTarget AS STRING) AS STRING
	LOCAL oSource , oTarget , oRelative AS Uri
	LOCAL cRelative AS STRING
	IF .NOT. cSourceDir:EndsWith("\")
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
	CATCH
		NOP
	END TRY
RETURN lRet
FUNCTION SafeFolderExists(cFolder AS STRING) AS LOGIC
	LOCAL lRet := FALSE AS LOGIC
	TRY
		lRet := Directory.Exists(cFolder)
	CATCH
		NOP
	END TRY
RETURN lRet
PROCEDURE SafeFileDelete(cFileName AS STRING)
	TRY
		File.Delete(cFileName)
	CATCH
		NOP
	END TRY
RETURN
PROCEDURE SafeDirectoryDelete(cFolder AS STRING)
	TRY
		FOREACH cFileName AS STRING IN Directory.GetFiles(cFolder, "*.wed")
			SafeFileDelete(cFileName)
		NEXT
		Directory.Delete(cFolder)
	CATCH
		NOP
	END TRY
RETURN

PROCEDURE ShowMessage(cText AS STRING)
	MessageBox.Show(cText, ToolName , MessageBoxButtons.OK , MessageBoxIcon.Information)
RETURN
PROCEDURE ShowWarning(cText AS STRING)
	MessageBox.Show(cText, ToolName , MessageBoxButtons.OK , MessageBoxIcon.Warning)
RETURN
PROCEDURE ShowError(cText AS STRING)
	MessageBox.Show(cText, ToolName , MessageBoxButtons.OK , MessageBoxIcon.Error)
RETURN
