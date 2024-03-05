USING System
USING System.Collections.Generic
USING System.ComponentModel
USING System.Data
USING System.Drawing
USING System.Linq
USING System.Text
USING System.Threading.Tasks
USING System.Windows.Forms
USING VFPXPorterLib
USING System.IO

BEGIN NAMESPACE VFPXPorter
		PUBLIC PARTIAL CLASS MainWindow	;
		INHERIT System.Windows.Forms.Form
		PRIVATE jsonSettings AS ExporterSettings
		PUBLIC CONSTRUCTOR()   STRICT//Form1
			InitializeComponent()
			//VAR storingPath := Path.GetDirectoryName( Application.ExecutablePath )
			//
			// !!! Settings of VFPXPorter.exe are stored in %userfolder%/AppData/Roaming/XSharp
			VAR roamingPath := Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData )
			roamingPath := Path.Combine( roamingPath, "XSharp" )
			IF !Directory.Exists(roamingPath)
				Directory.CreateDirectory( roamingPath )
			ENDIF
			// !!! ALL Ressources of the XPorterLib are in <VFXPorter.exe folder>/.Data
			// !!! This can be changed through ExporterSettings.RessourcesFolder
			jsonSettings := ExporterSettings{ Path.Combine( roamingPath, "VFPXPorter.json" ) }
			//
		PRIVATE METHOD quitToolStripMenuItem_Click(sender AS OBJECT, e AS System.EventArgs) AS VOID STRICT
			SELF:QuitApp()
			RETURN

		PROPERTY Settings AS XPorterSettings GET jsonSettings:ToXPorterSettings()

		PRIVATE METHOD openToolStripMenuItem_Click(sender AS OBJECT, e AS System.EventArgs) AS VOID STRICT

		PRIVATE METHOD toolStripButtonQuit_Click(sender AS OBJECT, e AS System.EventArgs) AS VOID STRICT
			SELF:QuitApp()
			RETURN

		PRIVATE METHOD QuitApp() AS VOID
			SELF:Close()

		PRIVATE METHOD formToolStripMenuItem_Click(sender AS OBJECT, e AS System.EventArgs) AS VOID STRICT
			LOCAL mdi := ExportItemWindow{} AS ExportItemWindow
			mdi:Settings := SELF:Settings
            mdi:xPorter := XPorterSCXVCX{}
			mdi:extension := "scx"
			mdi:Text := "Export Form"
			mdi:MdiParent := SELF
			mdi:outputPathTextBox:Text := mdi:Settings:OutputPath
			mdi:scxPathTextBox:Text := mdi:Settings:ItemsPath
			mdi:Show()
			RETURN
		PRIVATE METHOD aboutToolStripMenuItem_Click(sender AS OBJECT, e AS System.EventArgs) AS VOID STRICT
			LOCAL dlg AS AboutDialog
			//
			dlg := AboutDialog{}
			dlg:ShowDialog()
			RETURN
		PRIVATE METHOD MainWindow_FormClosing(sender AS OBJECT, e AS System.Windows.Forms.FormClosingEventArgs) AS VOID STRICT
			IF MessageBox.Show( "Are you sure ?", "Close Application", MessageBoxButtons.YesNo, MessageBoxIcon.Question ) != DialogResult.Yes
				e:Cancel := TRUE
			ENDIF
			RETURN
		PRIVATE METHOD settingsToolStripMenuItem_Click(sender AS OBJECT, e AS System.EventArgs) AS VOID STRICT
			LOCAL oDlg AS SettingsDialog
			//
			oDlg := SettingsDialog{ SELF:jsonSettings }
			oDlg:ShowDialog()
			RETURN
		PRIVATE METHOD projectToolStripMenuItem_Click(sender AS OBJECT, e AS System.EventArgs) AS VOID STRICT
			LOCAL mdi := ExportProjectWindow{} AS ExportProjectWindow
			mdi:Settings := SELF:Settings
			mdi:MdiParent := SELF
			mdi:outputPathTextBox:Text := SELF:Settings:OutputPath
			mdi:pjxPathTextBox:Text := SELF:Settings:ItemsPath
			mdi:Settings := SELF:Settings
			mdi:Show()
			RETURN
		PRIVATE METHOD libraryToolStripMenuItem_Click(sender AS OBJECT, e AS System.EventArgs) AS VOID STRICT
			LOCAL mdi := ExportItemWindow{} AS ExportItemWindow
			mdi:Settings := SELF:Settings
            mdi:xPorter := XPorterSCXVCX{}
			mdi:extension := "vcx"
			mdi:MdiParent := SELF
			mdi:Text := "Export Library"
			mdi:outputPathTextBox:Text := mdi:Settings:OutputPath
			mdi:scxPathTextBox:Text := mdi:Settings:ItemsPath
			mdi:Show()
			RETURN

		PRIVATE METHOD menuToolStripMenuItem_Click(sender AS OBJECT, e AS System.EventArgs) AS VOID STRICT
			LOCAL mdi := ExportItemWindow{} AS ExportItemWindow
			mdi:Settings := SELF:Settings
			mdi:xPorter := VFPXPorterMenu{ }
			mdi:extension := "mnx"
			mdi:MdiParent := SELF
			mdi:Text := "Export Menu"
			mdi:outputPathTextBox:Text := mdi:Settings:OutputPath
			mdi:scxPathTextBox:Text := mdi:Settings:ItemsPath
			mdi:Show()
			RETURN
		PRIVATE METHOD reportToolStripMenuItem_Click(sender AS OBJECT, e AS System.EventArgs) AS VOID STRICT
			LOCAL mdi := ExportItemWindow{} AS ExportItemWindow
			mdi:Settings := SELF:Settings
			mdi:xPorter := VFPXPorterReport{ SELF:Settings }
			mdi:extension := "frx"
			mdi:MdiParent := SELF
			mdi:Text := "Export Report"
			mdi:outputPathTextBox:Text := mdi:Settings:OutputPath
			mdi:scxPathTextBox:Text := mdi:Settings:ItemsPath
			mdi:Show()
			RETURN
PRIVATE METHOD MainWindow_Load(sender AS System.Object, e AS System.EventArgs) AS VOID STRICT
			IF SELF:jsonSettings:Warning
				VAR warn := WarningDialog{}
				warn:ShowDialog()
				IF warn:checkDontShow:Checked
					SELF:jsonSettings:Warning := FALSE
				ENDIF
			ENDIF
			RETURN
		END METHOD


END CLASS
END NAMESPACE
