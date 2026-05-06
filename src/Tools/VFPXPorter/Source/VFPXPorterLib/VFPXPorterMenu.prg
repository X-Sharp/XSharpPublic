// VFPXPorterMenu.prg
// Created by    : fabri
// Creation Date : 11/15/2020 7:44:12 PM
// Created for   :
// WorkStation   : FABPORTABLE


USING System
USING System.Collections.Generic
USING System.Text
USING System.IO
USING Newtonsoft.Json
USING System.Xml.Serialization
USING System.ComponentModel

BEGIN NAMESPACE VFPXPorterLib

    /// <summary>
    /// The VFPXPorterMenu class.
    /// </summary>
    CLASS VFPXPorterMenu INHERIT XPorter

        PROTECTED MenuContainerFile AS STRING
        PROPERTY MenuContainer AS STRING GET SELF:GetTemplateFromCache( MenuContainerFile )

        PROTECTED ConvertTableFile AS STRING
        PROPERTY ConvertTable AS STRING GET SELF:GetTemplateFromCache( ConvertTableFile )

        PRIVATE _templateCache AS Dictionary<STRING, STRING>
        PRIVATE _menuConverter AS CodeConverter

        /// <summary>
        /// The List of Items that exist in the current file (Form/Library)
        /// </summary>
        PROPERTY Items AS List<MNXItem> AUTO
        PROPERTY GeneratedFiles AS List<GeneratedFile> AUTO

        CONSTRUCTOR( )
            // Default Setting files
            SELF:MenuContainerFile := XPorterSettings.MenuContainerFile
            SELF:ConvertTableFile := XPorterSettings.ConvertTableFile
            //
            SELF:Items := List<MNXItem>{ }
            SELF:GeneratedFiles := List<GeneratedFile>{}
            SELF:_templateCache := Dictionary<STRING, STRING>{}
            //
            RETURN

        PRIVATE METHOD GetTemplateFromCache( filePath AS STRING ) AS STRING
            IF SELF:_templateCache:ContainsKey( filePath )
                RETURN SELF:_templateCache[ filePath ]
            ENDIF
            LOCAL content AS STRING
            content := File.ReadAllText( filePath )
            SELF:_templateCache[ filePath ] := content
            RETURN content


        METHOD Analyze( doBackup AS LOGIC ) AS LOGIC
            LOCAL success AS LOGIC
            //
            success := TRUE
            //
            VAR tmpItems := List<MNXItem>{}
            TRY
                // Open the MNX (DBF) File
                DbUseArea(TRUE, "DBFVFP", SELF:Settings:ItemsPath, SELF:Settings:ItemsPath,FALSE,TRUE )
                SetDeleted(FALSE)
                // Now load with data
                DbGoTop()
                DO WHILE ! Eof()
                    LOCAL item AS MNXItem
                    //
                    item := MNXItem{ TRUE }
                    //item:FileName := Path.GetFileName(SELF:inputFilePath)
                    tmpItems:Add( item )
                    // Move to next record
                    DbSkip()
                ENDDO
            CATCH e AS Exception
                success := FALSE
                XPorterLogger.Instance:Error("Analyze: Failed to analyze menu file: " + SELF:Settings:ItemsPath)
                XPorterLogger.Instance:Error("Exception: " + e.Message)
            FINALLY
                DbCloseArea()
            END TRY
            // Ok ?
            IF success
                // Now, move Childs to their parent's list if any
                SELF:Items := List<MNXItem>{}
                //
                LOCAL parentItem := NULL AS MNXItem
                LOCAL currentItem := NULL AS MNXItem
                VAR parentStack := Stack<MNXItem>{}
                //
                FOREACH VAR itm IN tmpItems
                    // The item is a TopBar ?
                    IF ( itm:OBJTYPE == MenuObjType.Menu ) .OR. ( itm:OBJTYPE == MenuObjType.SdiMenu )
                        parentItem := itm
                        SELF:Items:Add( parentItem )
                    ELSEIF ( parentItem != NULL )
                        IF ( itm:OBJTYPE == MenuObjType.Item )
                            IF ( itm:OBJCODE == MenuObjCode.SubMenu ) .OR. ;
                                    ( itm:OBJCODE == MenuObjCode.Command ) .OR. ;
                                    ( itm:OBJCODE == MenuObjCode.Procedure ) .OR. ;
                                    ( itm:OBJCODE == MenuObjCode.SystemBar )
                                // We have a SubMenu Item
                                currentItem := itm
                                currentItem:ParentName := parentItem:Name
                                parentItem:Childs:Add( currentItem )
                                IF ( parentItem:SubItemsToAdd > 0 )
                                    parentItem:SubItemsToAdd--
                                    IF ( parentItem:SubItemsToAdd == 0 ) .AND. ( parentStack:Count > 0 )
                                        parentItem := parentStack:Pop()
                                    ENDIF
                                ENDIF
                            ENDIF
                        ELSEIF ( itm:OBJTYPE == MenuObjType.SubMenu ) .AND. ( itm:OBJCODE== MenuObjCode.Info )
                            // Is the Name stored here ?
                            IF !String.IsNullOrEmpty( itm:_Name ) .AND. String.IsNullOrEmpty( currentItem:_Name )
                                currentItem:Name := itm:_Name
                            ENDIF
                            // How many SubItems ?
                            currentItem:SubItemsToAdd := itm:NUMITEMS
                            IF itm:NUMITEMS > 0
                                parentStack:Push( parentItem )
                                parentItem := currentItem
                            ENDIF
                        ENDIF
                    ENDIF
                NEXT
                //
                XPorterLogger.Instance:Information( "Processing " + Path.GetFileName(SELF:Settings:ItemsPath ) )
                XPorterLogger.Instance:Information( "High Level Items : "+ Items:Count:ToString() )
                XPorterLogger.Instance:Information( "Total Items : "+ tmpItems:Count:ToString() )
                // Make some corrections if needed
                SELF:PostProcessItems( SELF:Items )
                // For the Analyze text
                SELF:EnumItems( SELF:Items, "" )
                IF doBackup
                    // Backup the MNX file to an XML file
                    SELF:Serialize( SELF:Items )
                ENDIF
            ENDIF
            RETURN success

        METHOD Export( doBackup AS LOGIC ) AS LOGIC
            //
            IF !SELF:Analyze( doBackup )
                RETURN FALSE
            ENDIF
            VAR totalItems := SELF:EnumItems( SELF:Items )
            SELF:ResetProgress(totalItems)
            // This will convert the Command/Procedure
            IF SELF:ProcessAttachedCode()
                SELF:ResetProgress(totalItems)
                SELF:ExportSingleFile(  )
            ENDIF
            RETURN !SELF:Canceled

        PROTECT METHOD ExportSingleFile() AS VOID
            LOCAL dest AS StreamWriter
            // The File to be created
            LOCAL destFile AS STRING
            destFile := Path.GetFileName( SELF:Settings:ItemsPath )
            destFile := Path.Combine(SELF:Settings:OutputPath, destFile )
            destFile := Path.ChangeExtension( destFile, ".prg")
            dest := StreamWriter{ destFile }
            SELF:GeneratedFiles:Add( GeneratedFile{destFile})
            // Build replacements dictionary for Menu template
            VAR menuReplacements := Dictionary<STRING, STRING>{}
            menuReplacements["MenuName"] := Path.GetFileNameWithoutExtension(SELF:Settings:ItemsPath)
            //
            LOCAL initMenu AS STRING
            initMenu := SELF:ProcessInits( SELF:Items )
            IF !SELF:Canceled
                menuReplacements["MenuInit"] := initMenu
                LOCAL events AS STRING
                events := ""
                events := SELF:ProcessEvents( SELF:Items )
                IF !SELF:Canceled
                    menuReplacements["MenuCode"] := events
                    //
                    LOCAL code AS STRING
                    code := SELF:ReplaceAndValidate(SELF:MenuContainer, "MenuContainer", menuReplacements)
                    dest:Write( code )
                ENDIF
            ENDIF
            //
            dest:Close()
            RETURN

        PROTECTED METHOD ProcessAttachedCode() AS LOGIC
            // ConvertHandlerCode recurses into Childs, so only iterate top-level pads here
            FOREACH item AS MNXItem IN SELF:Items
                IF SELF:Canceled
                    EXIT
                ENDIF
                SELF:ConvertHandlerCode( item )
            NEXT
            RETURN	!SELF:Canceled

        PROTECTED METHOD ConvertHandlerCode( item AS MNXItem ) AS VOID
            // Lazy-init the code converter for menu handler code
            IF SELF:_menuConverter == NULL
                LOCAL sttmnts AS List<STRING>
                sttmnts := JsonConvert.DeserializeObject<List<STRING>>( File.ReadAllText(XPorterSettings.StatementsFile) )
                LOCAL vfpElts AS Dictionary<STRING,STRING>
                vfpElts := JsonConvert.DeserializeObject<Dictionary<STRING,STRING>>( File.ReadAllText(XPorterSettings.VFPElementsFile) )
                // Only apply THISFORM/THISFORMSET in menu context — not this./Parent. (no "this" object in menus)
                LOCAL menuElts AS Dictionary<STRING,STRING>
                menuElts := Dictionary<STRING,STRING>{}
                FOREACH VAR elt IN vfpElts
                    IF elt:Key == "thisform." .OR. elt:Key == "thisformset."
                        menuElts:Add(elt:Key, elt:Value)
                    ENDIF
                NEXT
                SELF:_menuConverter := CodeConverter{ FALSE, FALSE, TRUE, TRUE, FALSE }
                SELF:_menuConverter:Statements := sttmnts
                SELF:_menuConverter:VFPElements := menuElts
            ENDIF
            // Convert COMMAND (single-line inline VFP code)
            IF !String.IsNullOrEmpty(item:COMMAND)
                SELF:_menuConverter:ProcessMenuCode(item:COMMAND)
                item:COMMAND := String.Join(Environment.NewLine, SELF:_menuConverter:Source:ToArray())
            ENDIF
            // Convert PROCEDURE (multi-line block — strip PROCEDURE/ENDPROC wrapper first)
            IF !String.IsNullOrEmpty(item:PROCEDURE)
                SELF:_menuConverter:ProcessMenuCode(SELF:StripProcedureWrapper(item:PROCEDURE))
                item:PROCEDURE := String.Join(Environment.NewLine, SELF:_menuConverter:Source:ToArray())
            ENDIF
            // Recurse into nested sub-menu children
            FOREACH child AS MNXItem IN item:Childs
                SELF:ConvertHandlerCode(child)
            NEXT

        PRIVATE METHOD StripProcedureWrapper( code AS STRING ) AS STRING
            LOCAL lines AS List<STRING>
            lines := ReadSource(code)
            IF lines:Count == 0
                RETURN code
            ENDIF
            // Strip leading PROCEDURE/FUNCTION declaration line
            VAR firstUp := lines[0]:Trim():ToUpper()
            IF firstUp:StartsWith("PROCEDURE ") .OR. firstUp:StartsWith("FUNCTION ")
                lines:RemoveAt(0)
            ENDIF
            // Strip trailing blank lines, then the closing RETURN/ENDPROC/ENDFUNC
            DO WHILE lines:Count > 0
                VAR lastTrimmed := lines[lines:Count-1]:Trim()
                IF String.IsNullOrWhiteSpace(lastTrimmed)
                    lines:RemoveAt(lines:Count-1)
                ELSEIF String.Compare(lastTrimmed, "RETURN",  TRUE) == 0 .OR. ;
                       String.Compare(lastTrimmed, "ENDPROC", TRUE) == 0 .OR. ;
                       String.Compare(lastTrimmed, "ENDFUNC", TRUE) == 0
                    lines:RemoveAt(lines:Count-1)
                    EXIT
                ELSE
                    EXIT
                ENDIF
            ENDDO
            VAR sb := StringBuilder{}
            FOREACH VAR line IN lines
                sb:AppendLine(line)
            NEXT
            RETURN sb:ToString()

        PROTECTED METHOD ProcessDeclarations( itemList AS List<MNXItem>, typeList AS Dictionary<STRING,STRING[]> ) AS STRING
            LOCAL declaration AS StringBuilder
            declaration := StringBuilder{}
            //
            FOREACH VAR menuItem IN itemList
                //
                IF SELF:Canceled
                    RETURN ""
                ENDIF
                SELF:UpdateProgress()
                //
                LOCAL itemClassName AS STRING
                itemClassName := "unknown"
                // set default item ClassName and apply conversion
                IF ( menuItem:OBJTYPE == MenuObjType.Menu ) .OR. ( menuItem:OBJTYPE == MenuObjType.SdiMenu )
                    itemClassName := "xsPorterMenuStrip"
                ELSEIF ( menuItem:OBJTYPE == MenuObjType.Item )
                    itemClassName := "xsPorterMenuItem"
                    IF menuItem:PROMPT == "\-"
                        itemClassName := "xsPorterMenuSeparator"
                    ENDIF
                ENDIF
                itemClassName := SELF:ConvertClassName( itemClassName, typeList )
                // The Class will exist also in a .MPR version, so members must be visible, so PROTECT instead of PRIVATE
                declaration:Append("PROTECTED ")
                declaration:Append(menuItem:GeneratedName)
                declaration:Append(" AS ")
                declaration:Append(itemClassName)
                declaration:Append(Environment.NewLine)
                //
                IF menuItem:Childs:Count > 0
                    declaration:Append(SELF:ProcessDeclarations( menuItem:Childs, typeList  ))
                ENDIF
            NEXT
            RETURN declaration:ToString()

        PROTECTED METHOD ProcessInits( itemList AS List<MNXItem> ) AS STRING
            LOCAL initCode AS StringBuilder
            initCode := StringBuilder{}
            //
            FOREACH VAR pad IN itemList
                IF SELF:Canceled
                    RETURN ""
                ENDIF
                SELF:UpdateProgress()
                //
                // Emit AddPad for each top-level menu entry
                initCode:Append('oPad := SELF:AddPad( "')
                initCode:Append(pad:Name)
                initCode:Append('", "')
                initCode:Append(pad:PROMPT)
                initCode:Append('"')
                initCode:Append(" )")
                initCode:Append(Environment.NewLine)
                //
                IF pad:Childs:Count > 0
                    initCode:AppendLine("oPopup := XSharp.VFP.UI.Popup{}")
                    SELF:AppendPopupInit( initCode, pad:Childs, "oPopup" )
                    initCode:AppendLine("oPad:Popup := oPopup")
                ENDIF
                initCode:Append(Environment.NewLine)
            NEXT
            RETURN initCode:ToString()

        // Recursive helper: emits AddBar calls, sub-popup wiring and vfpClick
        // assignments for a list of bars into an already-declared popup variable.
        PRIVATE METHOD AppendPopupInit( code AS StringBuilder, bars AS List<MNXItem>, popupVar AS STRING ) AS VOID
            // Pass 1 — AddBar + sub-popups
            FOREACH VAR bar IN bars
                IF bar:PROMPT == "\-"
                    code:Append(popupVar)
                    code:Append(':AddBar( "')
                    code:Append(bar:Name)
                    code:AppendLine('", "--" )')
                ELSE
                    code:Append(popupVar)
                    code:Append(':AddBar( "')
                    code:Append(bar:Name)
                    code:Append('", "')
                    code:Append(bar:PROMPT)
                    code:AppendLine('" )')
                    IF bar:Childs:Count > 0
                        // Sub-menu: create a named popup variable and recurse
                        LOCAL subVar := "oPopup_" + bar:Name AS STRING
                        code:Append("LOCAL ")
                        code:Append(subVar)
                        code:AppendLine(" AS XSharp.VFP.UI.Popup")
                        code:Append(subVar)
                        code:AppendLine(" := XSharp.VFP.UI.Popup{}")
                        SELF:AppendPopupInit( code, bar:Childs, subVar )
                        // Attach sub-popup to the bar (barIndex tracked below via Bars[])
                        // We reference by name since AddBar registered it as a dynamic property
                        code:Append(popupVar)
                        code:Append(':')
                        code:Append(bar:Name)
                        code:Append(':Popup := ')
                        code:AppendLine(subVar)
                    ENDIF
                ENDIF
            NEXT
            // Pass 2 — wire vfpClick for action bars (non-separator, no sub-menu)
            LOCAL barIndex := 0 AS INT
            FOREACH VAR bar IN bars
                IF bar:PROMPT != "\-"
                    barIndex++
                    IF bar:Childs:Count == 0
                        IF !String.IsNullOrEmpty(bar:COMMAND) .OR. !String.IsNullOrEmpty(bar:PROCEDURE)
                            code:Append(popupVar)
                            code:Append(":Bars[")
                            code:Append(barIndex:ToString())
                            code:Append(']:vfpClick := "')
                            code:Append(bar:Name)
                            code:AppendLine('"')
                        ENDIF
                    ENDIF
                ENDIF
            NEXT

        PROTECTED METHOD ProcessEvents( itemList AS List<MNXItem> ) AS STRING
            LOCAL eventCode AS StringBuilder
            eventCode := StringBuilder{}
            //
            FOREACH VAR pad IN itemList
                IF SELF:Canceled
                    RETURN ""
                ENDIF
                SELF:UpdateProgress()
                SELF:AppendBarMethods(eventCode, pad:Childs)
            NEXT
            RETURN eventCode:ToString()

        PRIVATE METHOD AppendBarMethods( code AS StringBuilder, bars AS List<MNXItem> ) AS VOID
            FOREACH VAR bar IN bars
                IF SELF:Canceled
                    RETURN
                ENDIF
                VAR hasCode := !String.IsNullOrEmpty(bar:COMMAND) .OR. !String.IsNullOrEmpty(bar:PROCEDURE)
                IF hasCode
                    code:Append("METHOD ")
                    code:Append(bar:Name)
                    code:AppendLine("() AS USUAL STRICT")
                    // PROCEDURE takes priority (more complete); fall back to COMMAND
                    IF !String.IsNullOrEmpty(bar:PROCEDURE)
                        code:AppendLine(bar:PROCEDURE)
                    ELSE
                        code:AppendLine(bar:COMMAND)
                    ENDIF
                    code:AppendLine("    RETURN NIL")
                    code:Append(Environment.NewLine)
                ENDIF
                // Recurse for nested sub-menus
                IF bar:Childs:Count > 0
                    SELF:AppendBarMethods(code, bar:Childs)
                ENDIF
            NEXT


        VIRTUAL PROTECT METHOD PostProcessItems( itemList AS List<MNXItem> ) AS VOID

        PROTECT METHOD ConvertClassName( className AS STRING, ConversionList AS Dictionary<STRING,STRING[]> ) AS STRING
            LOCAL result := "unknown" AS STRING
            //
            IF ConversionList:ContainsKey( className )
                LOCAL data AS STRING[]
                // Convert a Fox ClassName to a .NET ClassName
                data := ConversionList:Item[ className ]
                // Retrieve the corresponding ClassName to Use
                result := data[1]
            ENDIF
            RETURN result

        VIRTUAL PROTECT METHOD EnumItems( itemList AS List<MNXItem>, indent AS STRING ) AS VOID
            FOREACH VAR item IN itemList
                XPorterLogger.Instance:Verbose(indent + "Item : "+ item:Name + "/" + item:PROMPT )
                IF item:Childs:Count > 0
                    XPorterLogger.Instance:Verbose( indent + "  Contains "+ item:Childs:Count:ToString() + " item(s)" )
                    SELF:EnumItems( item:Childs, indent + "  " )
                ENDIF
            NEXT
            //
            RETURN

        /// <summary>
        /// Backup the MNX Items : Create an XML File with Items info, and export the associated Code
        /// </summary>
        PROTECTED METHOD Serialize( itemList AS List<MNXItem> ) AS VOID
            // First, Clone the List
            LOCAL newList AS List<MNXItem>
            newList := CloneItemList( itemList )
            // Now, extract all Code into this Folder
            VAR tempPath := Path.Combine( SELF:Settings:OutputPath, "Backup" )
            // Warning, we may NOT be able to create the Directory
            Directory.CreateDirectory( tempPath )
            // Now, extract code :
            // This will copy code to an External file, and replace the RawCode property with the FullPath of the External File
            SELF:ExtractCode( tempPath, newList, "" )
            // Serialize the Items List
            //VAR result := JsonConvert.SerializeObject( newList )
            // Save the Serialization
            LOCAL destFile AS STRING
            destFile := Path.GetFileName( SELF:Settings:ItemsPath )
            destFile := Path.Combine(tempPath, destFile )
            //destFile := Path.ChangeExtension( destFile, "json")
            destFile := Path.ChangeExtension( destFile, "xml")
            //
            LOCAL writer AS StreamWriter
            writer := StreamWriter{ destFile }
            VAR result := XmlSerializer{ TYPEOF( XmlExportMenu ) }
            VAR xPorter := XmlExportMenu{ newList }
            result:Serialize( writer, xPorter )
            writer:Close()
            //File.WriteAllText( destFile, result )
            RETURN

        /// <summary>
        /// This will copy code to an External file, and replace the RawCode property with the FullPath of the External File
        // Used by the Serialize Method.
        /// </summary>
        PROTECTED METHOD ExtractCode( tempPath AS STRING, itemList AS List<MNXItem>, parent AS STRING  ) AS VOID
            LOCAL destFile AS STRING
            // For each Item, Build a FileName based on Itme Name and Parent Name
            // Then Save the source in this file
            // And replace the source with the FileName
            FOREACH VAR item IN itemList
                IF !String.IsNullOrEmpty( item:COMMAND )
                    destFile := Path.Combine(tempPath, IIF( String.IsNullOrEmpty(parent), "", parent + "_" ) + item:Name + "_command" )
                    destFile := Path.ChangeExtension( destFile, "prg")
                    File.WriteAllText( destFile, item:COMMAND )
                    item:COMMAND := destFile
                ENDIF
                IF !String.IsNullOrEmpty( item:PROCEDURE )
                    destFile := Path.Combine(tempPath, IIF( String.IsNullOrEmpty(parent), "", parent + "_" ) + item:Name + "_procedure" )
                    destFile := Path.ChangeExtension( destFile, "prg")
                    File.WriteAllText( destFile, item:PROCEDURE )
                    item:PROCEDURE := destFile
                ENDIF
                IF item:Childs:Count > 0
                    SELF:ExtractCode( tempPath, item:Childs, IIF( String.IsNullOrEmpty(parent), "", parent + "_" ) + item:Name )
                ENDIF
            NEXT
            //
            RETURN

        PROTECTED METHOD EnumItems( itemList AS List<MNXItem> ) AS INT
            LOCAL total := 0 AS INT
            FOREACH VAR item IN itemList
                //
                total++
                FOREACH VAR subItem IN item:Childs
                    total += SELF:EnumItems( subItem:Childs )
                NEXT
            NEXT
            RETURN total


    END CLASS
END NAMESPACE // VFPXPorterLib
