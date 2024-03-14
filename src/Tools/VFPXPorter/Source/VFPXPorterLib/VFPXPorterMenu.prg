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
        PROPERTY MenuContainer AS STRING GET File.ReadAllText( MenuContainerFile )

        PROTECTED ConvertTableFile AS STRING
        PROPERTY ConvertTable AS STRING GET File.ReadAllText( ConvertTableFile )

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
            //
            RETURN


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
                XPorterLogger.Instance:Error( e.Message )
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
            //LOCAL newList AS List<MNXItem>
            //newList := CloneItemList( SELF:Items )
            // Load Convertion table
            LOCAL typeList AS Dictionary<STRING,STRING[]>
            typeList := DeserializeJSONSimpleArray( SELF:ConvertTable )
            // The File to be created
            LOCAL destFile AS STRING
            destFile := Path.GetFileName( SELF:Settings:ItemsPath )
            destFile := Path.Combine(SELF:Settings:OutputPath, destFile )
            destFile := Path.ChangeExtension( destFile, ".prg")
            dest := StreamWriter{ destFile }
            SELF:GeneratedFiles:Add( GeneratedFile{destFile})
            // The Code template file
            LOCAL code AS STRING
            code := SELF:MenuContainer
            // The name of the Menu, is the name of the .mnx file
            code := code:Replace( "<@MenuName@>", Path.GetFileNameWithoutExtension(SELF:Settings:ItemsPath) )
            // The declaration part to be generated
            LOCAL declaration AS STRING
            // Create the List of Declaration(s)
            declaration := SELF:ProcessDeclarations( SELF:Items, typeList )
            IF !SELF:Canceled
                code := code:Replace( "<@MenuDeclaration@>", declaration )
                LOCAL initMenu AS STRING
                initMenu := ""
                initMenu := SELF:ProcessInits( SELF:Items, typeList )
                IF !SELF:Canceled
                    code := code:Replace( "<@MenuInit@>", initMenu )
                    LOCAL events AS STRING
                    events := ""
                    events := SELF:ProcessEvents( SELF:Items )
                    IF !SELF:Canceled
                        code := code:Replace( "<@MenuCode@>", events )
                        //
                        dest:Write( code )
                    ENDIF
                ENDIF
            ENDIF
            //
            dest:Close()
            RETURN

        PROTECTED METHOD ProcessAttachedCode() AS LOGIC
            // The TopLevel is the ToolStripMenu
            FOREACH item AS MNXItem IN SELF:Items
                //
                FOREACH subItem AS MNXItem IN item:Childs
                    IF SELF:Canceled
                        EXIT
                    ENDIF
                    SELF:ConvertHandlerCode( subItem )
                NEXT
                IF SELF:Canceled
                    EXIT
                ENDIF
                // Don't forget to process the Item itself
                SELF:ConvertHandlerCode( item )
            NEXT
            RETURN	!SELF:Canceled

        PROTECTED METHOD ConvertHandlerCode( subItem AS MNXItem ) AS VOID
            // Code in COMMAND
            // Code in PROCEDURE
            // Code in SETUP
            // Code in CLEANUP

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

        PROTECTED METHOD ProcessInits( itemList AS List<MNXItem>, typeList AS Dictionary<STRING,STRING[]> ) AS STRING
            LOCAL initCode AS StringBuilder
            LOCAL strips AS List<STRING>
            LOCAL menuText := NULL AS STRING
            LOCAL itemName AS STRING
            initCode := StringBuilder{}
            strips := List<STRING>{}
            itemName := "SubItems" // Non existing property
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
                    strips:Add( menuItem:GeneratedName)
                    itemName := "Items"
                    menuText := NULL
                ELSEIF ( menuItem:OBJTYPE == MenuObjType.Item )
                    itemClassName := "xsPorterMenuItem"
                    itemName := "DropDownItems"
                    IF menuItem:PROMPT == "\-"
                        itemClassName := "xsPorterMenuSeparator"
                        menuText := NULL
                    ELSE
                        menuText := menuItem:PROMPT
                    ENDIF
                ENDIF
                itemClassName := SELF:ConvertClassName( itemClassName, typeList )
                // Should be "THIS." ? Create the Object
                initCode:Append("SELF:")
                initCode:Append(menuItem:GeneratedName)
                initCode:Append(" := ")
                initCode:Append(itemClassName)
                initCode:Append("{" )
                IF ( menuText != NULL )
                    initCode:Append('"')
                    initCode:Append(menuText)
                    initCode:Append('"')
                ENDIF
                initCode:Append("}")
                initCode:Append(Environment.NewLine)
                // Menu Name
                initCode:Append("SELF:")
                initCode:Append(menuItem:GeneratedName)
                initCode:Append(":Name := ")
                initCode:Append('"')
                initCode:Append(menuItem:Name)
                initCode:Append('"')
                initCode:Append(Environment.NewLine)
                // Menu Text
                initCode:Append("SELF:")
                initCode:Append(menuItem:GeneratedName)
                initCode:Append(":Text := ")
                initCode:Append('"')
                initCode:Append(menuItem:PROMPT)
                initCode:Append('"')
                initCode:Append(Environment.NewLine)
                // Add the EventHandlers
                IF !String.IsNullOrEmpty( menuItem:COMMAND )
                    initCode:Append("SELF:" )
                    initCode:Append(menuItem:GeneratedName )
                    initCode:Append(":Click += System.EventHandler{ SELF, @" )
                    initCode:Append(menuItem:GeneratedName )
                    initCode:Append("_Command() }" )
                    initCode:Append(Environment.NewLine)
                ENDIF
                IF !String.IsNullOrEmpty( menuItem:PROCEDURE )
                    initCode:Append("SELF:")
                    initCode:Append(menuItem:GeneratedName)
                    initCode:Append(":Click += System.EventHandler{ SELF, @")
                    initCode:Append(menuItem:GeneratedName)
                    initCode:Append("_Procedure() }")
                    initCode:Append(Environment.NewLine)
                ENDIF
                // Add SubItems
                IF menuItem:Childs:Count > 0
                    initCode:Append(SELF:ProcessInits( menuItem:Childs, typeList  ))
                    FOREACH VAR menuChild IN menuItem:Childs
                        initCode:Append("SELF:" )
                        initCode:Append(menuItem:GeneratedName)
                        initCode:Append(":")
                        initCode:Append(itemName)
                        initCode:Append(":Add( ")
                        initCode:Append(menuChild:GeneratedName)
                        initCode:Append(" )")
                        initCode:Append(Environment.NewLine)
                    NEXT
                ENDIF
            NEXT
            // Create a list of MenuStrips
            initCode:Append("//")
            initCode:Append(Environment.NewLine)
            FOREACH VAR menuStrip IN strips
                initCode:Append("MenuStrips:Add( " )
                initCode:Append(menuStrip)
                initCode:Append(" )")
                initCode:Append(Environment.NewLine)
            NEXT
            RETURN initCode:ToString()

        PROTECTED METHOD ProcessEvents( itemList AS List<MNXItem> ) AS STRING
            LOCAL eventCode AS StringBuilder
            eventCode := StringBuilder{}
            //
            FOREACH VAR menuItem IN itemList
                //
                IF SELF:Canceled
                    RETURN ""
                ENDIF
                SELF:UpdateProgress()
                //
                // Add the EventHandlers
                IF !String.IsNullOrEmpty( menuItem:COMMAND )
                    eventCode:Append("PRIVATE METHOD ")
                    eventCode:Append(menuItem:GeneratedName)
                    eventCode:Append("_Command( sender AS OBJECT, e AS System.EventArgs) AS VOID")
                    eventCode:Append(Environment.NewLine)
                    eventCode:Append(menuItem:COMMAND)
                    eventCode:Append(Environment.NewLine)
                ENDIF
                IF !String.IsNullOrEmpty( menuItem:PROCEDURE )
                    eventCode:Append("PRIVATE METHOD ")
                    eventCode:Append(menuItem:GeneratedName)
                    eventCode:Append("_Procedure( sender AS OBJECT, e AS System.EventArgs) AS VOID")
                    eventCode:Append(Environment.NewLine)
                    eventCode:Append(menuItem:PROCEDURE)
                    eventCode:Append(Environment.NewLine)
                ENDIF
                // Add SubItems
                IF menuItem:Childs:Count > 0
                    eventCode:Append(SELF:ProcessEvents( menuItem:Childs ))
                ENDIF
            NEXT
            RETURN eventCode:ToString()


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
                XPorterLogger.Instance:Information(indent + "Item : "+ item:Name + "/" + item:PROMPT )
                IF item:Childs:Count > 0
                    XPorterLogger.Instance:Information( indent + "----------" )
                    XPorterLogger.Instance:Information( indent + "Contains "+ item:Childs:Count:ToString() + " item(s)." )
                    SELF:EnumItems( item:Childs, indent + "   " )
                    XPorterLogger.Instance:Information( indent + "----------" )
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
            ExtractCode( tempPath, newList, "" )
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
                    ExtractCode( tempPath, item:Childs, IIF( String.IsNullOrEmpty(parent), "", parent + "_" ) + item:Name )
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
                    total += EnumItems( subItem:Childs )
                NEXT
            NEXT
            RETURN total


    END CLASS
END NAMESPACE // VFPXPorterLib
