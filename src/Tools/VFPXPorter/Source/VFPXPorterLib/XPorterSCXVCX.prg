// XPorterSCXVCX.prg
// Created by    : fabri
// Creation Date : 9/26/2023 11:04:42 PM
// Created for   :
// WorkStation   : FABXPS


USING System
USING System.Collections.Generic
USING System.Text
USING System.IO
USING System.ComponentModel

BEGIN NAMESPACE VFPXPorterLib

    /// <summary>
    /// The XPorterSCXVCX class.
    /// </summary>
    CLASS XPorterSCXVCX INHERIT XPorter

        PRIVATE _file AS SCXVCXFile

        PRIVATE _fileName AS STRING
        PRIVATE _outPath AS STRING

        PROPERTY DependsOn AS HashSet<STRING> AUTO GET PRIVATE SET
        PROPERTY DefiningControls AS Dictionary<STRING, SCXVCXItem> AUTO GET PRIVATE SET
        PROPERTY CustomControls AS Dictionary<STRING, SCXVCXItem> AUTO
        PROPERTY GeneratedFiles AS List<GeneratedFile> AUTO GET PRIVATE SET
        PROPERTY NamespaceDefinition AS STRING AUTO GET PRIVATE SET
        NEW PROPERTY FileName AS STRING GET _fileName

        CONSTRUCTOR(  )

        METHOD Initialize(filePath AS STRING, destPath AS STRING, settings AS XPorterSettings  ) AS VOID
            SELF:_fileName := filePath
            SELF:_outPath := destPath
            SELF:Settings := settings
            SELF:CustomControls := Dictionary<STRING, SCXVCXItem>{}
            RETURN

        /// <summary>
        /// Read the SCX/VCX File.
        /// Enum all items and childs.
        /// Log the Items and Childs Names.
        /// </summary>
        /// <param name="doBackup">Do a Backup of Code</param>
        /// <returns>True if the analysis was correct</returns>
        METHOD Analyze( doBackup := FALSE AS LOGIC ) AS LOGIC
            //
            IF SELF:_fileName == NULL  .OR. !File.Exists(SELF:_fileName)
                XPorterLogger.Instance:Error( "Unknown File : " )
                XPorterLogger.Instance:Error( IIF(SELF:_fileName==NULL, "NULL", SELF:_fileName) )
                RETURN FALSE
            ENDIF
            //
            SELF:_file := SCXVCXFile{ SELF:_fileName }
            //
            SELF:DependsOn := HashSet<STRING>{}
            SELF:DefiningControls := Dictionary<STRING, SCXVCXItem>{}
            //
            VAR success := SELF:_file:Analyze()
            IF success
                IF doBackup
                    SELF:_file:Backup( SELF:_outPath )
                ENDIF
                // Now, we have all needed informations in each Entity
                FOREACH VAR entity IN SELF:_file:Entities
                    SELF:DependsOn:AddRange(entity:DependsOn)
                    SELF:DefiningControls:AddRangeNewOnly<STRING,SCXVCXItem>( entity:DefiningControls )
                NEXT
            ENDIF
            //
            RETURN success
        END METHOD


        METHOD Export( doBackup := TRUE AS LOGIC ) AS LOGIC
            LOCAL lOk AS LOGIC
            lOk := FALSE
            //
            IF !SELF:Analyze( doBackup )
                RETURN lOk
            ENDIF
            // Ok, so Analysis has been done and is ok
            VAR xPorter := XPorterCtrlForm{ SELF:Settings }
            xPorter:Initialize( SELF:_fileName, SELF:_outPath )
            xPorter:Worker := SELF:_worker
            //
            IF SELF:_file:IsLibrary
                // Set the NameSpace for the Library
                xPorter:NamespaceDefinition  := Path.GetFileNameWithoutExtension( SELF:_fileName )
                SELF:NamespaceDefinition := xPorter:NamespaceDefinition
            ELSE
                SELF:NamespaceDefinition := ""
                //                 // Not a library ? So a Form => The name of the Form is the name of the File (??? WHY ???)
                //                 xPorter:FormNameOverride := Path.GetFileNameWithoutExtension( SELF:_fileName )
            ENDIF
            //
            SELF:GeneratedFiles := List<GeneratedFile>{}
            VAR totalItems := SELF:CountItems( SELF:_file:Entities )
            xPorter:ResetProgress(totalItems)
            FOREACH VAR entity IN SELF:_file:Entities
                //
                xPorter:ProcessAttachedCode( entity )
                xPorter:CustomControls := SELF:CustomControls
                IF entity:Item:IsForm .OR. entity:Item:IsContainer
                    // file.prg & file.designer.prg
                    lOk := xPorter:ExportAsWindowAndDesigner( entity )
                ELSE
                    // single file.prg
                    lOk := xPorter:ExportAsSingleFile( entity )
                ENDIF
                IF !lOk
                    EXIT
                ENDIF
            NEXT
            // Keep a trace of all Generated Files
            SELF:GeneratedFiles:AddRange( xPorter:GeneratedFiles )
            //
            RETURN lOk


        PROTECTED METHOD CountItems( entities AS List<SCXVCXEntity> ) AS INT
            LOCAL total := 0 AS INT
            FOREACH VAR entity IN entities
                //
                total++
                FOREACH VAR subItem IN entity:Item:Childs
                    total += SELF:CountItems( subItem:Childs )
                NEXT
            NEXT
            RETURN total

        PROTECTED METHOD CountItems( itemList AS List<BaseItem> ) AS INT
            LOCAL total := 0 AS INT
            FOREACH VAR item IN itemList
                //
                total++
                FOREACH VAR subItem IN item:Childs
                    total += SELF:CountItems( subItem:Childs )
                NEXT
            NEXT
            RETURN total

    END CLASS
END NAMESPACE // VFPXPorterLib
