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
            SELF:InitializeComponent()
            SELF:TypeComboBox:SelectedIndex := 0
			RETURN

		PRIVATE METHOD exportButton_Click(sender AS OBJECT, e AS System.EventArgs) AS VOID
            SELF:Settings:OutputType := (ProjectType)SELF:TypeComboBox:SelectedIndex
            SELF:Settings:AppendToSolution := SELF:AppendCheckBox:Checked
            SELF:Settings:SolutionName := SELF:SolutionName:Text
            SELF:Settings:PlaceSolutionInSameDirectory := SELF:PlaceSolutionInSameDirectory:Checked
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

                IF String.IsNullOrWhiteSpace(SELF:SolutionName:Text)
                    // Auto-Complete: if solution name is empty, we set it to the PJX folder
                    SELF:SolutionName:Text := Path.GetFileNameWithoutExtension(ofd:FileName)
                ENDIF
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
            VAR baseOutputPath := SELF:outputPathTextBox:Text

			//
			IF !File.Exists(pjxFilePath)
				SELF:infoStripLabel:ForeColor := Color.Red
				SELF:infoStripLabel:Text := "Error : Input Project doesn't exist."
				RETURN FALSE
            ENDIF

			IF !Directory.Exists(baseOutputPath)
				SELF:infoStripLabel:ForeColor := Color.Red
				SELF:infoStripLabel:Text := "Error : Output Path doesn't exist."
				RETURN FALSE
            ENDIF
            //

			// Get the PJX File name, and use it as a SubFolder
			VAR projectName := Path.GetFileNameWithoutExtension(pjxFilePath)
            LOCAL projectPath AS STRING

            IF !SELF:Settings:PlaceSolutionInSameDirectory
                projectPath := Path.Combine(baseOutputPath, projectName)
            ELSE
                projectPath := baseOutputPath
            ENDIF

			// Warning, we may NOT be able to create the Directory
			TRY
                IF Directory.Exists(projectPath)
                    // If Append: do not delete the folder. We might delete the solution.
                    // or sibling project folders. We just delete if not Append.
                    IF !SELF:Settings:AppendToSolution
                        SELF:EraseFolder(projectPath, FALSE )
                    ELSE
                        // If Append: we just delete sub-folders of the current project
                        // to ensure a clean export of the current project.
                        VAR codeDir := Path.Combine(projectPath, "Code")
                        IF Directory.Exists(codeDir)
                            SELF:EraseFolder(codeDir, TRUE)
                        ENDIF
                        var xsharpDir := Path.Combine(projectPath, "XSharp")
                        IF Directory.Exists(xsharpDir)
                            SELF:EraseFolder(xsharpDir, TRUE)
                        ENDIF
                    ENDIF
                ELSE
				    Directory.CreateDirectory(projectPath)
				ENDIF
			CATCH e AS Exception
				//
                SELF:resultText:Text := "Warning during folder cleanup: " + e:Message
                IF !SELF:Settings:IgnoreErrors
				    THROW e
				ENDIF
			END TRY
			//
            SELF:xPorter := XPorterProject{ pjxFilePath, projectPath }
            SELF:xPorter:SolutionPath := baseOutputPath

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
				SELF:EraseFolder(subfolder, TRUE)
			NEXT
			IF eraseTop
				Directory.Delete( folderPath )
			ENDIF
			RETURN

	END CLASS
END NAMESPACE
