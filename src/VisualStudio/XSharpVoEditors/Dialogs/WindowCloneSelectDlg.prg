USING System

USING System.IO

USING System.Collections.Generic
USING System.ComponentModel
USING System.Data
USING System.Drawing

USING System.Text

USING System.Windows.Forms

USING XSharpModel
USING XSharp.Settings

BEGIN NAMESPACE XSharp.VOEditors.Dialogs


PUBLIC PARTIAL CLASS WindowCloneSelectDlg	;
        INHERIT System.Windows.Forms.Form
    PRIVATE listedFileNames as  IList<string>
    PRIVATE cSelected := NULL AS STRING
    PRIVATE cCompareFileName AS STRING
    Private cStartUpDir as STRING


    public constructor(file AS XFile, cFileName AS STRING) strict
        SELF:InitializeComponent()
        Self:cCompareFileName := cFileName
        Self:listedFileNames:=  List<string>{}
        local fiProjectFile := FileInfo{file:Project:FileName} as FileInfo

        Self:cStartUpDir := fiProjectFile:DirectoryName

        //save the first loadet filelist
        listedFileNames := file:Project:GetFilesOfType(XFileType.VOForm, TRUE)

        FOREACH VAR FileName IN listedFileNames
            // Suppress the frm that we are adding !
            IF !String.Equals(FileName, Self:cCompareFileName,StringComparison.OrdinalIgnoreCase)
                SELF:oFilesList:Items:Add(FileName)
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
    PRIVATE METHOD SearchButton_Click(sender AS System.Object, e AS System.EventArgs) AS VOID STRICT
        SELF:ListBoxSearch()
        RETURN
    END METHOD


    PRIVATE METHOD ListBoxSearch() AS VOID
        local searchString:=self:oSearchTextbox:Text as String

        //clear list
        SELF:oFilesList:Items:Clear()

        //on empty string, reset the liste to original only
        if String.IsNullOrEmpty(searchString)
            FOREACH VAR FileName IN listedFileNames
                // Suppress the frm that we are adding !
                SELF:oFilesList:Items:Add(FileName)
            NEXT
            return
        end if
        //filter list
        local nl as IList<string>
        nl:= List<string>{}
        foreach var li  in  listedFileNames
            IF !String.Equals(li, Self:cCompareFileName,StringComparison.OrdinalIgnoreCase)
                if li:ToUpper():Contains(searchString:ToUpper())
                    nl:Add(li)
                endif
            endif
        next

        //set listitems
        FOREACH VAR FileName IN nl
            // Suppress the frm that we are adding !
            SELF:oFilesList:Items:Add(FileName)
        NEXT



        RETURN

    PRIVATE METHOD SelectFolder_Click(sender AS System.Object, e AS System.EventArgs) AS VOID STRICT

        begin using var fbd := FolderBrowserDialog{}

            local result as DialogResult
            fbd:SelectedPath := Self:cStartUpDir
            result := fbd:ShowDialog()
            if  result == DialogResult.OK .AND. !String.IsNullOrWhiteSpace(fbd:SelectedPath)
                //replace or add the files....
                listedFileNames := Directory.GetFiles(fbd:SelectedPath, "*.xsfrm",System.IO.SearchOption.AllDirectories )

                SELF:oFilesList:Items:Clear()
                //load files
                FOREACH VAR FileName IN listedFileNames
                    // Suppress the frm that we are adding !
                    IF !String.Equals(FileName, Self:cCompareFileName,StringComparison.OrdinalIgnoreCase)
                        SELF:oFilesList:Items:Add(FileName)
                    ENDIF
                NEXT
            endif

        end using
        RETURN
    END METHOD

END CLASS

END NAMESPACE
