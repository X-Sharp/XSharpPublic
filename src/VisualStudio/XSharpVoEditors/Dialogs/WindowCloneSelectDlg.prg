USING System
USING System.Collections.Generic
USING System.ComponentModel
USING System.Data
USING System.Drawing

USING System.Text

USING System.Windows.Forms

USING XSharpModel
USING XSharp.Settings

BEGIN NAMESPACE XSharp.VOEditors.Dialogs

    PUBLIC PARTIAL CLASS WindowCloneSelectDlg ;
        INHERIT System.Windows.Forms.Form

        PRIVATE cSelected := NULL AS STRING

        public constructor(file AS XFile, cFileName AS STRING) strict
            InitializeComponent()

            VAR fileNames := file:Project:GetFilesOfType(XFileType.VOForm, TRUE)
            FOREACH VAR filename IN fileNames
                // Suppress the frm that we are adding !
                IF !String.Equals(filename, cFileName,StringComparison.OrdinalIgnoreCase)
                    SELF:oFilesList:Items:Add(filename)
                ENDIF
            NEXT

            return
        end constructor

        PROPERTY SelectedFile AS STRING GET SELF:cSelected

        PRIVATE METHOD oOKButton_Click(sender AS System.Object, e AS System.EventArgs) AS VOID STRICT
            IF SELF:oFilesList:SelectedIndex == -1
                SELF:DialogResult := System.Windows.Forms.DialogResult.Cancel
                RETURN
            END IF
            SELF:cSelected := SELF:oFilesList:SelectedItem:ToString()
            SELF:DialogResult := System.Windows.Forms.DialogResult.OK
            RETURN
        END METHOD
    END CLASS
END NAMESPACE
