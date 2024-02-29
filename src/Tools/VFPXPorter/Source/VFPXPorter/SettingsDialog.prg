USING System
USING System.Collections.Generic
USING System.ComponentModel
USING System.Data
USING System.Drawing
USING System.Text
USING System.Windows.Forms
USING System.IO
USING VFPXPorterLib

BEGIN NAMESPACE VFPXPorter
	PUBLIC PARTIAL CLASS SettingsDialog	;
		INHERIT System.Windows.Forms.Form
		PRIVATE iniSettings AS ExporterSettings

		PUBLIC CONSTRUCTOR( ini AS ExporterSettings ) STRICT //SettingsDialog
			InitializeComponent()
			SELF:iniSettings := ini

			RETURN
		PRIVATE METHOD outputButton_Click(sender AS OBJECT, e AS System.EventArgs) AS VOID STRICT
			LOCAL fbd AS FolderBrowserDialog
			//
			fbd := FolderBrowserDialog{}
			fbd:RootFolder := Environment.SpecialFolder.Desktop //System.Environment.SpecialFolder.MyComputer
			fbd:SelectedPath := SELF:outputPathTextBox:Text
			fbd:ShowNewFolderButton := TRUE
			IF ( fbd:ShowDialog() == DialogResult.OK )
				SELF:outputPathTextBox:Text := fbd:SelectedPath
			ENDIF
			RETURN
		PRIVATE METHOD okButton_Click(sender AS OBJECT, e AS System.EventArgs) AS VOID STRICT
			SELF:iniSettings:Output := SELF:outputPathTextBox:Text
			SELF:iniSettings:Items := SELF:vfpPathTextBox:Text
			//SELF:iniSettings:WriteValue( "Items", "SeparateFile", SELF:checkItemAsFile:Checked )
			SELF:iniSettings:LibInSubFolder := SELF:checkLibraryAsSubFolder:Checked
			SELF:iniSettings:IgnoreErrors := SELF:checkIgnoreExportError:Checked
			SELF:iniSettings:StoreInFolders := SELF:checkStoreInFolders:Checked
			SELF:iniSettings:EmptyFolder := SELF:checkEmptyFolder:Checked
			SELF:iniSettings:ConvertHandlers := SELF:checkConvertHandlers:Checked
			SELF:iniSettings:ConvertUserDef := SELF:checkConvertUserDef:Checked
			SELF:iniSettings:ConvertThisObject := SELF:checkConvertThisObject:Checked
			SELF:iniSettings:ConvertStatement := SELF:checkStatement:Checked
			SELF:iniSettings:ConvertStatementOnlyIfLast := SELF:checkStatementLast:Checked
			SELF:iniSettings:PrefixClassFile := SELF:checkPrefix:Checked
			SELF:iniSettings:KeepOriginal := SELF:checkKeepOriginal:Checked
			SELF:iniSettings:Modifier := SELF:comboModifiers:SelectedItem:ToString()
			SELF:iniSettings:NameUDF := SELF:checkNameUDF:Checked
			SELF:iniSettings:RemoveSet := SELF:checkSetEvent:Checked
			SELF:iniSettings:PrefixEvent := SELF:checkPrefixEventInForm:Checked
			SELF:iniSettings:KeepFoxProEventName := SELF:checkKeepFoxProEventName:Checked
			SELF:iniSettings:RessourcesFolder := SELF:ressourcesTextBox:Text
			SELF:iniSettings:GenerateOnlyHandledEvent := SELF:checkGenerateOnlyHandledEvent:Checked
			//
			VAR defFolders := ""
			FOREACH lvi AS ListViewItem IN SELF:listFolders:Items
				defFolders += lvi:SubItems[1]:Text + "=" + lvi:SubItems[0]:Text + ";"
			NEXT
			// Remove the trailing ";"
			defFolders := defFolders:Substring( 0, defFolders:Length-1)
			SELF:iniSettings:ItemsType := defFolders
			//
			SELF:DialogResult := DialogResult.OK
			RETURN
		END METHOD


		PRIVATE METHOD SettingsDialog_Load(sender AS OBJECT, e AS System.EventArgs) AS VOID STRICT
			SELF:LoadSettings()
		END METHOD


		PROTECTED METHOD LoadSettings() AS VOID
			SELF:outputPathTextBox:Text := SELF:iniSettings:Output
			SELF:vfpPathTextBox:Text := SELF:iniSettings:Items
			SELF:ressourcesTextBox:Text := SELF:iniSettings:RessourcesFolder
			SELF:checkLibraryAsSubFolder:Checked := SELF:iniSettings:LibInSubFolder
			//SELF:checkItemAsFile:Checked := SELF:iniSettings:ReadValue( "Items", "SeparateFile", FALSE )
			SELF:checkIgnoreExportError:Checked := SELF:iniSettings:IgnoreErrors
			SELF:checkStoreInFolders:Checked := SELF:iniSettings:StoreInFolders
			SELF:checkEmptyFolder:Checked := SELF:iniSettings:EmptyFolder
			SELF:checkConvertHandlers:Checked := SELF:iniSettings:ConvertHandlers
			SELF:checkConvertUserDef:Checked := SELF:iniSettings:ConvertUserDef
			SELF:checkConvertThisObject:Checked := SELF:iniSettings:ConvertThisObject
			SELF:checkStatement:Checked := SELF:iniSettings:ConvertStatement
			SELF:checkStatementLast:Checked := SELF:iniSettings:ConvertStatementOnlyIfLast
			SELF:checkPrefix:Checked := SELF:iniSettings:PrefixClassFile
			SELF:checkKeepOriginal:Checked := SELF:iniSettings:KeepOriginal
			SELF:comboModifiers:SelectedItem := SELF:iniSettings:Modifier
			SELF:checkNameUDF:Checked := SELF:iniSettings:NameUDF
			SELF:checkSetEvent:Checked := SELF:iniSettings:RemoveSet
			SELF:checkPrefixEventInForm:Checked := SELF:iniSettings:PrefixEvent
			SELF:checkKeepFoxProEventName:Checked := SELF:iniSettings:KeepFoxProEventName
			SELF:checkGenerateOnlyHandledEvent:Checked := SELF:iniSettings:GenerateOnlyHandledEvent
			// Fill Folders Name
			// The order is : "Forms,Libs,Menus,Code,Databases,FreeTables,Others"
			VAR defFolders := SELF:iniSettings:DefaultFolders
			VAR folderNames := SELF:iniSettings:ItemsType
			VAR names := folderNames:Split( <CHAR>{ ';' } )
			IF names:Length != 7
				folderNames := defFolders
				names := folderNames:Split( <CHAR>{ ';' } )
			ENDIF
			FOREACH VAR name IN names
				VAR data := name:Split( <CHAR>{ '=' } )
				VAR lvi := ListViewItem{ data[2] }
				lvi:SubItems:Add( data[1])
				SELF:listFolders:Items:Add( lvi )
			NEXT

			RETURN

		END METHOD


		PRIVATE METHOD vfpButton_Click(sender AS OBJECT, e AS System.EventArgs) AS VOID STRICT
			LOCAL fbd AS FolderBrowserDialog
			//
			fbd := FolderBrowserDialog{}
			fbd:RootFolder := Environment.SpecialFolder.Desktop //System.Environment.SpecialFolder.MyComputer
			fbd:SelectedPath := SELF:vfpPathTextBox:Text
			fbd:ShowNewFolderButton := TRUE
			IF ( fbd:ShowDialog() == DialogResult.OK )
				SELF:vfpPathTextBox:Text := fbd:SelectedPath
			ENDIF
			RETURN
		PRIVATE METHOD cancelButton1_Click(sender AS OBJECT, e AS System.EventArgs) AS VOID STRICT
			SELF:DialogResult := DialogResult.Cancel
			RETURN
		PRIVATE METHOD itemsButton_Click(sender AS OBJECT, e AS System.EventArgs) AS VOID STRICT
			IF SELF:itemsButton:Text == "<<"
				SELF:listFolders:Visible := FALSE
				SELF:itemsButton:Text := ">>"
				RETURN
			ENDIF
			// Show the itemsType List
			SELF:listFolders:Visible := TRUE
			SELF:itemsButton:Text := "<<"
			RETURN
		PRIVATE METHOD checkConvertHandlers_CheckedChanged(sender AS System.Object, e AS System.EventArgs) AS VOID STRICT
			IF SELF:checkConvertHandlers:Checked
				SELF:checkConvertThisObject.Checked := false
			ENDIf
			RETURN
		PRIVATE METHOD checkConvertThisObject_CheckedChanged(sender AS System.Object, e AS System.EventArgs) AS VOID STRICT
			if SELF:checkConvertThisObject.Checked
				SELF:checkConvertHandlers:Checked := false
				self:checkConvertUserDef:Checked := false
			ENDIf
			RETURN
		PRIVATE METHOD checkConvertUserDef_CheckedChanged(sender AS System.Object, e AS System.EventArgs) AS VOID STRICT
			IF SELF:checkConvertUserDef:Checked
				SELF:checkConvertThisObject.Checked := false
			ENDIf
			RETURN
		PRIVATE METHOD editButton_Click(sender AS System.Object, e AS System.EventArgs) AS VOID STRICT
			VAR statementsFile := Path.Combine( Path.GetDirectoryName( Application.ExecutablePath ), XPorterSettings.StatementsFile )
			System.Diagnostics.Process.Start( "notepad.exe", statementsFile )
			RETURN
		END METHOD
		PRIVATE METHOD buttonReset_Click(sender AS System.Object, e AS System.EventArgs) AS VOID STRICT
			IF MessageBox.Show( "Are you sure ?", "Reset Settings", MessageBoxButtons.YesNo, MessageBoxIcon.Warning ) == DialogResult.Yes
				SELF:iniSettings:Reset()
				SELF:LoadSettings()
				// Force read to reset
				VAR dummy := SELF:iniSettings:Warning
			ENDIF
			RETURN
		END METHOD
		PRIVATE METHOD buttonFolder_Click(sender AS System.Object, e AS System.EventArgs) AS VOID STRICT
			VAR outputPath := Path.GetDirectoryName( SELF:iniSettings:FullPath )
			System.Diagnostics.Process.Start( "explorer.exe", outputPath )
			RETURN
		END METHOD
		PRIVATE METHOD ressourcesButton_Click(sender AS System.Object, e AS System.EventArgs) AS VOID STRICT
			LOCAL fbd AS FolderBrowserDialog
			//
			fbd := FolderBrowserDialog{}
			//fbd:RootFolder := Environment.SpecialFolder.Desktop
			fbd:SelectedPath := SELF:ressourcesTextBox:Text
			fbd:ShowNewFolderButton := TRUE
			IF ( fbd:ShowDialog() == DialogResult.OK )
				SELF:ressourcesTextBox:Text := fbd:SelectedPath

			ENDIF
			RETURN
		END METHOD
PRIVATE METHOD checkKeepFoxProEventName_CheckedChanged(sender AS System.Object, e AS System.EventArgs) AS VOID STRICT
			RETURN
		END METHOD

	END CLASS
END NAMESPACE
