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
                DbUseArea(TRUE, "DBFVFP", SELF:_fileName, SELF:_fileName,FALSE,TRUE )
                SET DELETED ON
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
                                VAR parentName := itm:Parent:ToLower()
                                // Ok, or the parent doesn't exist, or it "might" be a Grid....
                                IF parentName:Contains( ".grid" )
                                    // This is certainly a Header, or a TextBox, inside a Column
                                    // ok, extract the Grid name
                                    VAR gridName := String.Empty
                                    VAR startG := parentName:IndexOf( ".grid" )
                                    VAR endG := parentName:IndexOf( ".", startG+5 ) // +5 == ".grid":Length
                                    IF ( endG > -1 )
                                        gridName := parentName:Substring( startG+1, endG - startG -1 )
                                    ENDIF
                                    // Do it again
                                    parent := tmpitems:Find( { it => String.Compare(it:Name, gridName, TRUE) == 0 })
                                    // Yes !
                                    IF ( parent != NULL )
                                        // Tell that Parent, he has a child
                                        parent:Childs:Add( itm )
                                    ENDIF
                                ENDIF
                            ENDIF
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
                    FOREACH VAR itm IN SELF:_Items
                        IF ( String.Compare( itm:BaseClassName, "dataenvironment", TRUE ) == 0 )
                            ent:DataEnvironment := itm
                        ELSE
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