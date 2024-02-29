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
PUBLIC PARTIAL CLASS ExportItemWindow	;
        INHERIT System.Windows.Forms.Form
    PROPERTY xPorter AS IXPorter AUTO
    PROPERTY extension AS STRING AUTO

    PROPERTY Settings AS XPorterSettings AUTO

    PUBLIC CONSTRUCTOR() STRICT //ExportWindow
        InitializeComponent()
        extension := "scx"
        RETURN

    PRIVATE METHOD exportButton_Click(sender AS OBJECT, e AS System.EventArgs) AS VOID
        // Check what we try to export
        SELF:infoStripError:Text := ""
        SELF:infoStripLabel:Text := ""
        IF !SELF:CheckFileAndFolder()
            RETURN
        ENDIF
        //
        IF !SELF:xPorter:Analyze(TRUE)
            MessageBox.Show( "Error during Component processing.", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error )
            RETURN
        ENDIF
        DoExport()

    PUBLIC METHOD DoExport() AS VOID
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
        IF Path.GetExtension(SELF:scxPathTextBox:Text):ToLower() == extension
            ofd:InitialDirectory := Path.GetDirectoryName(SELF:scxPathTextBox:Text)
        ELSE
            ofd:InitialDirectory := SELF:scxPathTextBox:Text
        ENDIF
        ofd:CheckFileExists := TRUE
        ofd:DefaultExt := extension
        ofd:Filter := extension + " files (*." + extension + ")|*." + extension + "|All files (*.*)|*.*"
        IF ( ofd:ShowDialog() == DialogResult.OK )
            SELF:scxPathTextBox:Text := ofd:FileName
        ENDIF
        RETURN
    PRIVATE METHOD outputButton_Click(sender AS OBJECT, e AS System.EventArgs) AS VOID STRICT
        LOCAL fbd AS FolderBrowserDialog
        //
        fbd := FolderBrowserDialog{}
        fbd:SelectedPath := SELF:outputPathTextBox:Text
        fbd:ShowNewFolderButton := TRUE
        IF ( fbd:ShowDialog() == DialogResult.OK )
            SELF:outputPathTextBox:Text := fbd:SelectedPath
        ENDIF
        RETURN
    PRIVATE METHOD CheckFileAndFolder() AS LOGIC
        //
        SELF:infoStripLabel:Text := ""
        SELF:infoStripLabel:ForeColor := Color.Black
        VAR scxFilePath := SELF:scxPathTextBox:Text
        VAR outputPath := SELF:outputPathTextBox:Text
        //
        IF !File.Exists( scxFilePath)
            SELF:infoStripLabel:ForeColor := Color.Red
            SELF:infoStripLabel:Text := "Error : Input File doesn't exist."
            RETURN FALSE
        ENDIF
        IF !Directory.Exists( outputPath )
            SELF:infoStripLabel:ForeColor := Color.Red
            SELF:infoStripLabel:Text := "Error : Output Path doesn't exist."
            RETURN FALSE
        ENDIF
        // Get the SCX File name, and use it as a SubFolder
        LOCAL destFile AS STRING
        destFile := Path.GetFileNameWithoutExtension( scxFilePath )
        outputPath := Path.Combine( outputPath, destFile )
        // Warning, we may NOT be able to create the Directory
        Directory.CreateDirectory( outputPath )
        //
        SELF:xPorter:Initialize( scxFilePath, outputPath, SELF:Settings )
        RETURN TRUE
    PRIVATE METHOD analysisButton_Click(sender AS OBJECT, e AS System.EventArgs) AS VOID STRICT
        IF !SELF:CheckFileAndFolder()
            RETURN
        ENDIF
        //
        //SELF:xPorter:Settings := SELF:Settings
        VAR dummy := SELF:xPorter:ResultText // This will clear the ResultText
        SELF:xPorter:Analyze(TRUE)
        SELF:resultText:Text := SELF:xPorter:ResultText
        RETURN
    PRIVATE METHOD openButton_Click(sender AS OBJECT, e AS System.EventArgs) AS VOID STRICT
        VAR scxFilePath := SELF:scxPathTextBox:Text
        VAR outputPath := SELF:outputPathTextBox:Text
        // Get the SCX File name, and use it as a SubFolder
        LOCAL destFile AS STRING
        destFile := Path.GetFileNameWithoutExtension( scxFilePath )
        outputPath := Path.Combine( outputPath, destFile )
        IF !Directory.Exists( outputPath )
            outputPath := SELF:outputPathTextBox:Text
        ENDIF
        System.Diagnostics.Process.Start( "explorer.exe", outputPath )
        RETURN
    PRIVATE METHOD backgroundExport_DoWork(sender AS OBJECT, e AS System.ComponentModel.DoWorkEventArgs) AS VOID STRICT
        //SELF:xPorter:Settings := SELF:Settings
        SELF:xPorter:Worker := SELF:backgroundExport
        e:Result := SELF:xPorter:Export(TRUE)
        RETURN
    PRIVATE METHOD backgroundExport_ProgressChanged(sender AS OBJECT, e AS System.ComponentModel.ProgressChangedEventArgs) AS VOID STRICT
        LOCAL percent AS INT
        percent := e:ProgressPercentage
        IF percent > 100
            percent := 100
        ENDIF
        SELF:infoStripBar:Value := percent
        RETURN
    PRIVATE METHOD backgroundExport_RunWorkerCompleted(sender AS OBJECT, e AS System.ComponentModel.RunWorkerCompletedEventArgs) AS VOID STRICT
        //
        IF e:Error != NULL
            SELF:infoStripLabel:ForeColor := Color.Red
            SELF:infoStripLabel:Text := "Exception : "
            SELF:infoStripError:Text := e.Error.Message
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


END CLASS
END NAMESPACE
