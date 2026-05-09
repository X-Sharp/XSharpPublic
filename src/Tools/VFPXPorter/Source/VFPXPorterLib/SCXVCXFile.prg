// SCXVCXFile.prg
// Created by    : fabri
// Creation Date : 9/24/2023 6:53:43 PM
// Created for   :
// WorkStation   : FABXPS


USING System
USING System.Collections.Generic
USING System.Text
USING System.IO
USING System.Xml.Serialization

BEGIN NAMESPACE VFPXPorterLib

    /// <summary>
    /// The SCXVCXFile class.
    /// </summary>
    CLASS SCXVCXFile

        STATIC PUBLIC ignoreTypeList AS STRING[]

        STATIC CONSTRUCTOR()
            // Elements to Ignore during SCX,VCX, read
            //VAR typeList := "dataenvironment,,otherstuff"
            VAR typeList := "otherstuff"
            SCXVCXFile.ignoreTypeList := typeList:Split( <CHAR>{','} ) //AS STRING[]
            //



        PRIVATE _fileName AS STRING
        PROPERTY FileName AS STRING GET SELF:_fileName

        PRIVATE _Items AS List<SCXVCXItem>

        PROPERTY Entities AS List<SCXVCXEntity> AUTO

        PRIVATE _isLibrary AS LOGIC
        PROPERTY IsLibrary AS LOGIC GET SELF:_isLibrary

        PROPERTY HasFormSet AS LOGIC AUTO GET PROTECTED SET

        CONSTRUCTOR( fileName AS STRING )
            SELF:_fileName := fileName
            SELF:_isLibrary := ( String.Compare( System.IO.Path.GetExtension( SELF:_fileName ), ".vcx", TRUE ) == 0 )
            //
            SELF:Entities := List<SCXVCXEntity>{}
            SELF:_Items := List<SCXVCXItem>{}
            SELF:HasFormSet := FALSE
            RETURN


        /// <summary>
        /// Analyze the SCX File, and create the list of Items
        /// </summary>
        METHOD Analyze( ) AS LOGIC
            LOCAL success AS LOGIC
            //
            success := TRUE
            //
            VAR tmpitems := List<SCXVCXItem>{}
            TRY
                // Open the SCX (DBF) File
                VAR alias := System.IO.Path.GetFileNameWithoutExtension(SELF:_fileName)
                IF !DbUseArea(TRUE, "DBFVFP", SELF:_fileName, alias, FALSE, TRUE )
                    XPorterLogger.Instance:Error("LoadFile: DbUseArea failed to open: " + SELF:_fileName)
                    RETURN FALSE
                ENDIF
                SetDeleted(TRUE)
                // Now load with data
                DbGoTop()
                DO WHILE ! Eof()
                    LOCAL itemInfo AS STRING
                    LOCAL item AS SCXVCXItem
                    //
                    // Special Item ??
                    itemInfo := FieldGet( FieldPos("PLATFORM") )
                    IF String.Compare( itemInfo, "COMMENT ", TRUE )==0
                        // Ok, skip
                        DbSkip()
                        LOOP
                    ENDIF
                    // Fill With Data
                    item := SCXVCXItem{ TRUE }
                    // Control Type ?
                    IF !SELF:IgnoreItemType( item:ClassName )
                        //
                        item:FileName := Path.GetFileName(SELF:_fileName)
                        tmpitems:Add( item )
                        //
                    ENDIF
                    // Move to next record
                    DbSkip()
                ENDDO
            CATCH e AS Exception
                success := FALSE
                XPorterLogger.Instance:Error( "Process DBF" )
                XPorterLogger.Instance:Error( e.Message)
            FINALLY
                DbCloseArea()
            END TRY
            //
            IF success
                // Now, move Childs to their parent's list
                SELF:_Items := List<SCXVCXItem>{}

                TRY
                    // Put the Childs, inside the parent
                    FOREACH VAR itm IN tmpitems
                        // The item has a Parent ?
                        IF !String.IsNullOrEmpty( itm:Parent )
                            // Look into the Items we have already seen, if we can get that Parent
                            VAR parent := tmpitems:Find( { it => String.Compare(it:FullName, itm:Parent, TRUE) == 0 })
                            // Yes !
                            IF ( parent != NULL )
                                // Tell that Parent, he has a child
                                parent:Childs:Add( itm )
                            ELSE
                                // No direct parent record found — the parent path contains a virtual
                                // container (Grid Column, PageFrame Page, etc.) that has no SCX record.
                                // Walk backwards through the path until a real ancestor is found.
                                VAR parentPath := itm:Parent
                                DO WHILE !String.IsNullOrEmpty( parentPath )
                                    VAR ancestor := tmpitems:Find( { it => String.Compare(it:FullName, parentPath, TRUE) == 0 })
                                    IF ancestor != NULL
                                        ancestor:Childs:Add( itm )
                                        EXIT
                                    ENDIF
                                    VAR lastDot := parentPath:LastIndexOf('.')
                                    IF lastDot < 0
                                        EXIT
                                    ENDIF
                                    parentPath := parentPath:Substring(0, lastDot)
                                ENDDO
                            ENDIF
                        ENDIF
                    NEXT
                    // Synthesize virtual Column items for every Grid so that column children
                    // are attached to typed synthetic items rather than dumped flat on the Grid.
                    FOREACH VAR itm IN tmpitems
                        IF String.Compare( itm:BaseClassName, "grid", TRUE ) == 0
                            SELF:SynthesizeGridColumns( itm )
                        ENDIF
                    NEXT
                    // Now, we should only have "top" level Items
                    FOREACH VAR itm IN tmpitems
                        // The item has a Parent ?
                        IF String.IsNullOrEmpty( itm:Parent )
                            itm:IsTopLevel := TRUE
                            SELF:_Items:Add( itm )
                        ELSE
                            VAR parent := tmpitems:Find( { it => String.Compare(it:FullName, itm:Parent, TRUE) == 0 })
                            // Yes !
                            IF ( parent != NULL )
                                // The parent is a FormSet ?
                                IF parent:IsFormSet
                                    // Mark the Component, indicating it is a FormSet
                                    SELF:HasFormSet := TRUE
                                    // Now, set the item as TopLevel(Container) (It should be a Form, should we check ??)
                                    itm:IsTopLevel := TRUE
                                    // Add a clone of the Item to the list of TopLevel Items
                                    // So it will generate a Form for it
                                    SELF:_Items:Add( SCXVCXItem{ itm } )
                                    // Now, remove the EventHandlers to the Items, so they won't appear in the FormSet code
                                    itm:XPortedCode := NULL
                                    itm:METHODS := ""
                                    // And Properties
                                    itm:PropertiesDict:Clear()
                                    // and UserDefItems
                                    itm:UserDefItems:Clear()
                                    // Now the Item is just a Type&Object that is inside the FormSet
                                    // Add a Property to the "link" the Form to the FormSet
                                    itm:PropertiesDict:Add("ThisFormSet", "SELF")
                                    // Now, we must set the type to parent:Name + "_" + item:Name
                                    itm:IsContainer := TRUE
                                    itm:AddToControls := TRUE
                                    itm:ClassName := parent:Name + "_" + itm:Name
                                    itm:BaseClassName := parent:Name + "_" + itm:Name
                                ENDIF
                            ENDIF
                        ENDIF
                    NEXT
                    //
                    XPorterLogger.Instance:Information( "Processing " + Path.GetFileName(SELF:_fileName ) )
                    XPorterLogger.Instance:Information( "High Level Items : "+ _Items:Count:ToString() )
                    //
                    VAR ent := SCXVCXEntity{ SELF }
                    LOCAL dataEnvItem AS SCXVCXItem
                    dataEnvItem := NULL
                    FOREACH VAR itm IN SELF:_Items
                        IF ( String.Compare( itm:BaseClassName, "dataenvironment", TRUE ) == 0 )
                            dataEnvItem := itm
                        ELSE
                            // Assign DataEnvironment to every form entity (simple SCX and all
                            // FormSet sub-forms). The FormSet wrapper (baseclass "formset") is
                            // intentionally excluded — it has no WinForms presence.
                            IF dataEnvItem != NULL .AND. ;
                               String.Compare( itm:BaseClassName, "form", TRUE ) == 0
                                ent:DataEnvironment := dataEnvItem
                            ENDIF
                            ent:Item := itm
                            ent:Analyze()
                            SELF:Entities:Add( ent )
                            ent := SCXVCXEntity{ SELF }
                        ENDIF
                    NEXT
                    //
                CATCH e AS Exception
                    success := FALSE
                    XPorterLogger.Instance:Error( "Process Enum" )
                    XPorterLogger.Instance:Error( e.Message )
                END TRY
            ENDIF
            RETURN success


        /// <summary>
        /// For a Grid item, creates synthetic SCXVCXItem objects (one per column) and
        /// re-parents column children from the Grid to the correct synthetic column.
        /// Column sub-properties (Column1.Width, Column2.ControlSource…) are moved
        /// from the Grid's PropertiesDict into each synthetic column's PropertiesDict.
        /// </summary>
        PRIVATE METHOD SynthesizeGridColumns( grid AS SCXVCXItem ) AS VOID
            // Step 1: collect all column indices referenced by dotted keys in PropertiesDict
            VAR colIndices := SortedSet<INT>{}
            FOREACH VAR kv IN grid:PropertiesDict
                VAR dotPos := kv:Key:IndexOf('.')
                IF dotPos > 6 .AND. kv:Key:StartsWith("Column", StringComparison.OrdinalIgnoreCase)
                    LOCAL n AS INT
                    IF Int32.TryParse( kv:Key:Substring(6, dotPos - 6), OUT n )
                        colIndices:Add( n )
                    ENDIF
                ENDIF
            NEXT
            IF colIndices:Count == 0
                RETURN
            ENDIF
            // Step 2: create one synthetic SCXVCXItem per column index
            VAR synthCols := Dictionary<INT, SCXVCXItem>{}
            FOREACH VAR idx IN colIndices
                VAR col := SCXVCXItem{}
                col:Name         := "Column" + idx:ToString()
                col:BaseClassName := "column"
                col:ClassName    := "column"
                col:Parent       := grid:FullName
                col:FileName     := grid:FileName
                synthCols:Add( idx, col )
            NEXT
            // Step 3: move Column\d+.* properties from grid to the matching synthetic column
            VAR toRemove := List<STRING>{}
            FOREACH VAR kv IN grid:PropertiesDict
                VAR dotPos := kv:Key:IndexOf('.')
                IF dotPos > 6 .AND. kv:Key:StartsWith("Column", StringComparison.OrdinalIgnoreCase)
                    LOCAL n AS INT
                    IF Int32.TryParse( kv:Key:Substring(6, dotPos - 6), OUT n ) .AND. synthCols:ContainsKey(n)
                        VAR propName := kv:Key:Substring(dotPos + 1)
                        IF !synthCols[n]:PropertiesDict:ContainsKey(propName)
                            synthCols[n]:PropertiesDict:Add( propName, kv:Value )
                        ENDIF
                        toRemove:Add( kv:Key )
                    ENDIF
                ENDIF
            NEXT
            FOREACH VAR key IN toRemove
                grid:PropertiesDict:Remove(key)
            NEXT
            // Step 4: re-parent column sub-controls from Grid to their synthetic column
            VAR leftover := List<BaseItem>{}
            FOREACH VAR baseChild IN grid:Childs
                VAR child := (SCXVCXItem) baseChild
                LOCAL colIdx AS INT
                IF SELF:TryExtractColumnIndex( child:Parent, OUT colIdx ) .AND. synthCols:ContainsKey(colIdx)
                    synthCols[colIdx]:Childs:Add( child )
                ELSE
                    leftover:Add( child )
                ENDIF
            NEXT
            // Step 5: replace Grid.Childs with synthetic columns (sorted) + any leftovers
            grid:Childs := List<BaseItem>{}
            FOREACH VAR idx IN colIndices
                grid:Childs:Add( synthCols[idx] )
            NEXT
            FOREACH VAR item IN leftover
                grid:Childs:Add( item )
            NEXT
        END METHOD

        // Returns TRUE and sets colIndex when parentPath ends in ".Column<n>" or is "Column<n>".
        PRIVATE METHOD TryExtractColumnIndex( parentPath AS STRING, colIndex OUT INT ) AS LOGIC
            colIndex := 0
            IF String.IsNullOrEmpty(parentPath)
                RETURN FALSE
            ENDIF
            VAR lastDot := parentPath:LastIndexOf('.')
            VAR lastSeg := IIF( lastDot >= 0, parentPath:Substring(lastDot + 1), parentPath )
            IF lastSeg:Length > 6 .AND. lastSeg:StartsWith("Column", StringComparison.OrdinalIgnoreCase)
                RETURN Int32.TryParse( lastSeg:Substring(6), OUT colIndex )
            ENDIF
            RETURN FALSE
        END METHOD

        PROTECTED METHOD IgnoreItemType( typeOfItem AS STRING ) AS LOGIC
            //
            IF String.IsNullOrEmpty( typeOfItem )
                RETURN TRUE
            ENDIF
            //
            LOCAL pos := Array.IndexOf( SCXVCXFile.ignoreTypeList, typeOfItem ) AS INT
            RETURN (pos >=0 )

        /// <summary>
        /// Backup the SCX Items : Create an XML File with Items info, and export the associated Code
        /// </summary>
        METHOD Backup( outputPath AS STRING ) AS VOID
            // First, Clone the List
            LOCAL newList AS List<BaseItem>
            newList := CloneItemList( SELF:_Items )
            // Now, extract all Code into this Folder
            VAR tempPath := Path.Combine( outputPath, "Backup" )
            // Warning, we may NOT be able to create the Directory
            Directory.CreateDirectory( tempPath )
            // Create a SubFolder with the FileName we are processing
            tempPath := Path.Combine( tempPath, Path.GetFileName( SELF:_fileName ) )
            Directory.CreateDirectory( tempPath )
            // Now, extract code :
            // This will copy code to an External file, and replace the RawCode property with the FullPath of the External File
            SELF:ExtractCode( tempPath, newList, "" )
            // Serialize the Items List
            //VAR result := JsonConvert.SerializeObject( newList )
            // Save the Serialization
            LOCAL destFile AS STRING
            destFile := Path.GetFileName( SELF:_fileName )
            destFile := Path.Combine(tempPath, destFile )
            //destFile := Path.ChangeExtension( destFile, "json")
            destFile := Path.ChangeExtension( destFile, "xml")
            //
            LOCAL writer AS StreamWriter
            writer := StreamWriter{ destFile }
            VAR result := XmlSerializer{ TYPEOF( XmlExportForm ) }
            VAR xPorter := XmlExportForm{ (List<BaseItem>)newList }
            result:Serialize( writer, xPorter )
            writer:Close()
            //File.WriteAllText( destFile, result )
            RETURN
        END METHOD

        PROTECTED METHOD ExtractCode( tempPath AS STRING, itemList AS List<BaseItem>, parent AS STRING ) AS VOID
            LOCAL destFile AS STRING
            // For each Item, Build a FileName based on Itme Name and Parent Name
            // Then Save the source in this file
            // And replace the source with the FileName
            FOREACH VAR item IN itemList
                IF !String.IsNullOrEmpty( item:RawCode )
                    destFile := Path.Combine(tempPath, IIF( String.IsNullOrEmpty(parent), "", parent + "_" ) + item:Name )
                    destFile := Path.ChangeExtension( destFile, "prg")
                    File.WriteAllText( destFile, item:RawCode )
                    item:RawCode := destFile
                ENDIF
                IF item IS SCXVCXItem
                    VAR oneItem := (SCXVCXItem)item
                    IF oneItem:UserDefItems != NULL
                        FOREACH VAR userDef IN oneItem:UserDefItems
                            IF userDef:Kind == UserDefinition.ItemKind.Method
                                destFile := Path.Combine(tempPath, userDef:Kind:ToString() + "_" + userDef:Name )
                                destFile := Path.ChangeExtension( destFile, "prg")
                                IF userDef:Code != NULL
                                    File.WriteAllText( destFile, userDef:Code:ToString() )
                                ELSE
                                    File.WriteAllText( destFile, "" )
                                ENDIF
                            ENDIF
                        NEXT
                    ENDIF
                ENDIF
                IF item:Childs:Count > 0
                    SELF:ExtractCode( tempPath, item:Childs, IIF( String.IsNullOrEmpty(parent), "", parent + "_" ) + item:Name )
                ENDIF
            NEXT
            //
            RETURN
        END METHOD

    END CLASS
END NAMESPACE // VFPXPorterLib
