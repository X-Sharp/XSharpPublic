USING System
USING System.Collections.Generic
USING System.ComponentModel
USING System.Data
USING System.Drawing
USING System.Text
USING System.Windows.Forms
USING System.IO
USING Newtonsoft.Json
USING VFPXPorterLib
BEGIN NAMESPACE VFPXPorter
	PUBLIC PARTIAL CLASS ExportProjectWindow	;
			INHERIT System.Windows.Forms.Form
		PUBLIC xPorter AS VFPXPorterLib.XPorterProject

		PROPERTY Settings AS XPorterSettings AUTO

		PUBLIC CONSTRUCTOR() STRICT //ExportWindow
			InitializeComponent()
			RETURN

		PRIVATE METHOD exportButton_Click(sender AS OBJECT, e AS System.EventArgs) AS VOID
			//
			SELF:infoStripLabel:Text := ""
			IF !SELF:CheckFileAndFolder()
				RETURN
			ENDIF
			// Analyze First
			IF !SELF:xPorter:ProcessPJX()
				MessageBox.Show( "Error during analyzing operation.", "Analyzing Project", MessageBoxButtons.OK, MessageBoxIcon.Error )
				RETURN
			ENDIF
			//
			// DoBackup, ProcessFirst
			SELF:Processing( TRUE )
			SELF:backgroundExport:RunWorkerAsync()
			RETURN

		METHOD Processing( state AS LOGIC ) AS VOID
			SELF:exportButton:Enabled := !state
			SELF:cancelBtn:Visible := state



		PRIVATE METHOD scxButton_Click(sender AS OBJECT, e AS System.EventArgs) AS VOID STRICT
			LOCAL ofd AS OpenFileDialog
			//
			ofd := OpenFileDialog{}
			IF Path.GetExtension(SELF:pjxPathTextBox:Text):ToLower() == "pjx"
				ofd:InitialDirectory := Path.GetDirectoryName(SELF:pjxPathTextBox:Text)
			ELSE
				ofd:InitialDirectory := SELF:pjxPathTextBox:Text
			ENDIF
			ofd:CheckFileExists := TRUE
			ofd:DefaultExt := "PJX"
			ofd:Filter := "Pjx files (*.pjx)|*.pjx|All files (*.*)|*.*"
			IF ( ofd:ShowDialog() == DialogResult.OK )
				SELF:pjxPathTextBox:Text := ofd:FileName
			ENDIF
			RETURN
		PRIVATE METHOD outputButton_Click(sender AS OBJECT, e AS System.EventArgs) AS VOID STRICT
			LOCAL fbd AS FolderBrowserDialog
			//
			fbd := FolderBrowserDialog{}
			fbd:RootFolder := System.Environment.SpecialFolder.MyComputer
			fbd:SelectedPath := SELF:outputPathTextBox:Text
			fbd:ShowNewFolderButton := TRUE
			IF ( fbd:ShowDialog() == DialogResult.OK )
				SELF:outputPathTextBox:Text := fbd:SelectedPath
			ENDIF
			RETURN
		PRIVATE METHOD CheckFileAndFolder() AS LOGIC
			//
			SELF:infoStripLabel:Text := ""
			SELF:infoStripError:Text := ""
			SELF:infoStripLabel:ForeColor := Color.Black
			VAR pjxFilePath := SELF:pjxPathTextBox:Text
			VAR outputPath := SELF:outputPathTextBox:Text
			//
			IF !File.Exists( pjxFilePath)
				SELF:infoStripLabel:ForeColor := Color.Red
				SELF:infoStripLabel:Text := "Error : Input Project doesn't exist."
				RETURN FALSE
			ENDIF
			IF !Directory.Exists( outputPath )
				SELF:infoStripLabel:ForeColor := Color.Red
				SELF:infoStripLabel:Text := "Error : Output Path doesn't exist."
				RETURN FALSE
			ENDIF
			// Get the PJX File name, and use it as a SubFolder
			LOCAL destFile AS STRING
			destFile := Path.GetFileNameWithoutExtension( pjxFilePath )
			outputPath := Path.Combine( outputPath, destFile )
			// Warning, we may NOT be able to create the Directory
			TRY
				IF Directory.Exists( outputPath )
					SELF:EraseFolder( outputPath, FALSE )
				ENDIF
				Directory.CreateDirectory( outputPath )
			CATCH e AS Exception
				//
				SELF:resultText:Text := "Cannot delete Folder : " + e.Message
				IF !SELF:Settings:IgnoreErrors
					THROW e
				ENDIF
			END TRY
			//
			SELF:xPorter := XPorterProject{ pjxFilePath, outputPath }
			RETURN TRUE
		PRIVATE METHOD analysisButton_Click(sender AS OBJECT, e AS System.EventArgs) AS VOID STRICT
			IF !SELF:CheckFileAndFolder()
				RETURN
			ENDIF
			//
			SELF:xPorter:Settings := SELF:Settings
			SELF:xPorter:ProcessPJX()
			SELF:resultText:Text := SELF:xPorter:ResultText
			RETURN
		PRIVATE METHOD openButton_Click(sender AS OBJECT, e AS System.EventArgs) AS VOID STRICT
			VAR pjxFilePath := SELF:pjxPathTextBox:Text
			VAR outputPath := SELF:outputPathTextBox:Text
			// Get the SCX File name, and use it as a SubFolder
			LOCAL destFile AS STRING
			destFile := Path.GetFileNameWithoutExtension( pjxFilePath )
			outputPath := Path.Combine( outputPath, destFile )
			IF !Directory.Exists( outputPath )
				outputPath := SELF:outputPathTextBox:Text
			ENDIF
			System.Diagnostics.Process.Start( "explorer.exe", outputPath )
			RETURN
		PRIVATE METHOD backgroundExport_DoWork(sender AS OBJECT, e AS System.ComponentModel.DoWorkEventArgs) AS VOID STRICT
            SELF:xPorter:Settings := SELF:Settings
			e:Result := SELF:xPorter:ExportProject( TRUE, SELF:backgroundExport )
			RETURN
		PRIVATE METHOD backgroundExport_ProgressChanged(sender AS OBJECT, e AS System.ComponentModel.ProgressChangedEventArgs) AS VOID STRICT
			LOCAL percent AS INT
			percent := e:ProgressPercentage
			IF percent > 100
				percent := 100
			ENDIF
            SELF:infoStripBar:Value := percent
            SELF:infoStripLabel:Text := Path.GetFileName( SELF:xPorter:CurrentFileName )
			RETURN
		PRIVATE METHOD backgroundExport_RunWorkerCompleted(sender AS OBJECT, e AS System.ComponentModel.RunWorkerCompletedEventArgs) AS VOID STRICT
			//
			IF e:Error != NULL
				SELF:infoStripLabel:ForeColor := Color.Red
                SELF:infoStripLabel:Text := "Exception : "
                VAR errorText := e:Error:Message:Trim()
                errorText := errorText:Substring(0, Math.Min(errorText:Length,50) )
				SELF:infoStripError:Text := errorText
			ELSEIF e:Cancelled
				SELF:infoStripLabel:ForeColor := Color.Red
				SELF:infoStripLabel:Text := "Export Canceled"
			ELSE
				IF e:Result IS LOGIC
					IF (LOGIC)e:Result != TRUE
						SELF:infoStripLabel:ForeColor := Color.Red
						SELF:infoStripLabel:Text := "Error : "
						SELF:infoStripError:Text := SELF:xPorter:ErrorText
					ELSE
						SELF:infoStripLabel:Text := "Export Done"
					ENDIF
				ENDIF
			ENDIF
			SELF:Processing(FALSE)
			RETURN
		PRIVATE METHOD cancelBtn_Click(sender AS OBJECT, e AS System.EventArgs) AS VOID STRICT
			SELF:backgroundExport:CancelAsync()
			RETURN

		private method EraseFolder( folderPath AS STRING, eraseTop AS LOGIC ) AS VOID
			foreach cFile AS String in Directory.GetFiles(folderPath)
				System.IO.File.Delete( cFile )
			NEXT

			foreach subfolder AS String in Directory.GetDirectories(folderPath)
				EraseFolder(subfolder, TRUE)
			NEXT
			IF eraseTop
				Directory.Delete( folderPath )
			ENDIF
			RETURN

	END CLASS
END NAMESPACE
