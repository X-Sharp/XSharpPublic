// VFPXPorterReport.prg
// Created by    : fabri
// Creation Date : 2/11/2021 10:22:04 PM
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
    /// The VFPXPorterReport class.
    /// </summary>
    CLASS VFPXPorterReport INHERIT XPorter

        PROPERTY Items AS List<FRXItem> AUTO

        CONSTRUCTOR( xportSettings AS XPorterSettings)
            //
            SELF:Items := List<FRXItem>{ }
            SELF:Settings := xportSettings
            //
            RETURN

        METHOD Analyze( doBackup AS LOGIC ) AS LOGIC
            LOCAL success AS LOGIC
            //
            success := TRUE
            //
            VAR tmpitems := List<FRXItem>{}
            TRY
                // Open the MNX (DBF) File
                DbUseArea(TRUE, "DBFVFP", SELF:Settings:ItemsPath, SELF:Settings:ItemsPath,FALSE,TRUE )
                SetDeleted(TRUE)
                // Now load with data
                DbGoTop()
                DO WHILE ! Eof()
                    LOCAL item AS FRXItem
                    //
                    item := FRXItem{}
                    tmpitems:Add( item )
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
                SELF:Items := tmpitems //List<FRXItem>{}
                //
                XPorterLogger.Instance:Information( "Processing " + Path.GetFileName(SELF:Settings:ItemsPath ) )
                XPorterLogger.Instance:Information( "High Level Items : "+ Items:Count:ToString() )
                XPorterLogger.Instance:Information( "Total Items : "+ tmpitems:Count:ToString() )
                // Make some corrections if needed
                PostProcessItems( SELF:Items )
                // For the Analyze text
                EnumItems( SELF:Items, "" )
                IF doBackup
                    // Backup the MNX file to an XML file
                    Serialize( SELF:Items )
                ENDIF
            ENDIF
            RETURN success

        METHOD Export( doBackup AS LOGIC ) AS LOGIC
            //
            IF !SELF:Analyze( doBackup )
                RETURN FALSE
            ENDIF
            RETURN FALSE


        VIRTUAL PROTECT METHOD PostProcessItems( itemList AS List<FRXItem> ) AS VOID

        VIRTUAL PROTECT METHOD EnumItems( itemList AS List<FRXItem>, indent AS STRING ) AS VOID
            RETURN

        /// <summary>
        /// Backup the FRX Items : Create an XML File with Items info, and export the associated Code
        /// </summary>
        PROTECTED METHOD Serialize( itemList AS List<FRXItem> ) AS VOID
            // First, Clone the List
            LOCAL newList AS List<FRXItem>
            newList := CloneItemList( itemList )
            // Now, extract all Code into this Folder
            VAR tempPath := Path.Combine( SELF:Settings:OutputPath, "Backup" )
            // Warning, we may NOT be able to create the Directory
            Directory.CreateDirectory( tempPath )
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
            VAR result := XmlSerializer{ TYPEOF( XmlExportReport ) }
            VAR xPorter := XmlExportReport{ newList }
            result:Serialize( writer, xPorter )
            writer:Close()
            //File.WriteAllText( destFile, result )
            RETURN

    END CLASS
END NAMESPACE // VFPXPorterLib
