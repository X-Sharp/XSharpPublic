// ExportProjectTool.prg
// Created by    : fabri
// Creation Date : 11/5/2019 1:31:48 PM
// Created for   :
// WorkStation   : FABPORTABLE


USING System
USING System.Collections.Generic
USING System.Text
USING System.IO
USING System.Xml.Serialization
USING System.Xml
USING System.ComponentModel
USING Serilog
USING Newtonsoft.Json
USING System.Linq


BEGIN NAMESPACE VFPXPorterLib


    /// <summary>
    /// The XPorterProject class.
    /// </summary>
    CLASS XPorterProject



        PRIVATE pjxFilePath AS STRING
        PRIVATE outputPath AS STRING

        PROPERTY Project AS VFPProject AUTO

        /// <summary>
        /// Folder where all "injected" files are
        /// </summary>
        PROPERTY ToolsFolder AS STRING AUTO

        PROPERTY Settings AS XPorterSettings AUTO

        /// <summary>
        /// The list of all files that are included in the generated Project
        /// </summary>
        PROPERTY GeneratedFiles AS List<GeneratedFile> AUTO

        /// <summary>
        /// The list of all files that comes from external libraries in the generated Project
        /// </summary>
        PROPERTY GeneratedLibFiles AS List<GeneratedFile> AUTO
        PROPERTY ReferenceLibFiles AS List<Reference> AUTO

        PROPERTY ReferenceFiles AS List<Reference> AUTO

        PROTECTED StartBlockFile AS STRING
        PROPERTY StartBlock AS STRING GET File.ReadAllText( StartBlockFile )

        PROPERTY CurrentFileName AS STRING AUTO GET PRIVATE SET


        CONSTRUCTOR( filePath AS STRING, destPath AS STRING )
            //
            SELF:pjxFilePath := filePath
            SELF:outputPath := destPath
            //
            SELF:ToolsFolder := XPorterSettings.ToolsFolder
            SELF:StartBlockFile := XPorterSettings.StartFile
            //
            SELF:GeneratedFiles := List<GeneratedFile>{}
            SELF:GeneratedLibFiles := List<GeneratedFile>{}
            SELF:ReferenceFiles := List<Reference>{}
            SELF:ReferenceLibFiles := List<Reference>{}
            //
            //SELF:SetLoggerToFile( NULL )

        METHOD _SetLoggerToFile( fileLog AS STRING ) AS VOID
            XPorterLogger.SetLoggerToFile( fileLog )

        PROPERTY ResultText AS STRING
            GET
                VAR log := BufferedSink.Instance:Log
                BufferedSink.Instance:Clear()
                RETURN log
            END GET
        END PROPERTY

        PROPERTY ErrorText AS STRING
            GET
                VAR log := BufferedSink.Instance:Errors
                RETURN log
            END GET
        END PROPERTY

        /// <summary>
        /// Process the PJX files :
        /// Enumerate the content
        /// Export the content
        /// Serialize/Save the .pjx content
        /// </summary>
        METHOD ProcessPJX() AS LOGIC
            LOCAL success AS LOGIC
            //
            success := TRUE
            VAR items := List<PJXItem>{}
            //
            TRY
                // Open the PJX (DBF) File
                DbUseArea(TRUE, "DBFVFP", SELF:pjxFilePath, SELF:pjxFilePath,FALSE,TRUE )
                SetDeleted(TRUE)
                // Now load with data
                DbGoTop()
                DO WHILE ! Eof()
                    LOCAL item AS PJXItem
                    //
                    item := PJXItem{}
                    // Items can be Excluded from the Projects
                    IF "dD":Contains( item:TYPE ) .OR. !item:EXCLUDE
                        //
                        items:Add( item )
                    ENDIF
                    // Move to next record
                    DbSkip()
                ENDDO
            CATCH ex AS Exception
                success := FALSE
                XPorterLogger.Instance:Error( "Use PJX File" )
                XPorterLogger.Instance:Error( ex:Message )
            FINALLY
                DbCloseArea()
            END TRY
            IF success
                // Create an internal Project object that will map the ItemsList
                SELF:Project := SELF:CreateVFPProject( items )
                // Export the PJX file in an XML file
                SELF:SerializePJX(items)
                // Build some Result Info
                XPorterLogger.Instance:Information( "Project : " + SELF:Project:Name )
                XPorterLogger.Instance:Information( "HomeDir : " + SELF:Project:HomeDir )
                XPorterLogger.Instance:Information( "Forms : " )
                FOREACH form AS ProjectItem IN SELF:Project:Forms
                    XPorterLogger.Instance:Information( form:Name )
                NEXT
                //
                XPorterLogger.Instance:Information( "Libraries/Dependencies : " )
                FOREACH lib AS ProjectItem IN SELF:Project:Libraries
                    XPorterLogger.Instance:Information( lib:Name )
                NEXT
                //
                XPorterLogger.Instance:Information( "Menus : " )
                FOREACH lib AS ProjectItem IN SELF:Project:Menus
                    XPorterLogger.Instance:Information( lib:Name )
                NEXT
                //
                XPorterLogger.Instance:Information( "Reports : " )
                FOREACH report AS ProjectItem IN SELF:Project:Reports
                    XPorterLogger.Instance:Information( report:Name )
                NEXT
                //
                XPorterLogger.Instance:Information( "Programs : " )
                FOREACH prg AS ProjectItem IN SELF:Project:Programs
                    XPorterLogger.Instance:Information( prg:Name )
                NEXT
                //
                XPorterLogger.Instance:Information( "DataBases : " )
                FOREACH dbc AS ProjectItem IN SELF:Project:Databases
                    XPorterLogger.Instance:Information( dbc:Name )
                NEXT
                //
                XPorterLogger.Instance:Information( "FreeTables : " )
                FOREACH dbf AS ProjectItem IN SELF:Project:FreeTables
                    XPorterLogger.Instance:Information( dbf:Name )
                NEXT
            ENDIF
            RETURN success

        PRIVATE METHOD CreateVFPProject( items AS List<PJXItem> ) AS VFPProject
            LOCAL project AS VFPProject
            project := VFPProject{}
            //
            FOREACH item AS PJXItem IN items
                VAR pjItem := ProjectItem{ project, item:NAME }
                IF item:MAINPROG
                    project:Main := pjItem
                ENDIF
                //
                SWITCH item:TYPE
                    /*
                    "Q"  // Query (.QPR)
                    "B"  // Label (.LBX)
                    "L"  // API Library (.FLL)
                    "Z"  // Application (.APP)
                    "T"  // Text (.TXT, .H., etc.)
                    */
                    CASE "V"
                        // Class Library (.VCX)
                        project:Libraries:Add( pjItem )
                    CASE "K"
                        // Form (.SCX)
                        project:Forms:Add( pjItem )
                    CASE "M"
                        // Menu (.MNX)
                        project:Menus:Add( pjItem )
                    CASE "H"
                        // Project (.PJX)
                        project:Name := item:NAME
                        // Relocate to the PJX File Directory
                        project:HomeDir := Path.GetDirectoryName( SELF:pjxFilePath ) // item:HomeDir
                        //
                    CASE "P"
                        // External PRG (.PRG)
                        project:Programs:Add( pjItem )
                        //
                    CASE "x"
                        // Others
                        project:Others:Add( pjItem )
                        //
                    CASE "i"
                        // Icon -> Others
                        project:Others:Add( pjItem )
                        //
                    CASE "d"
                        // Database (.dbc)
                        project:Databases:Add( pjItem )
                        //
                    CASE "D"
                        // Free table (.DBF)
                        project:FreeTables:Add( pjItem )
                        //
                    CASE "R"
                        // Report (.FRX)
                        project:Reports:Add( pjItem )
                END
            NEXT
            RETURN project


        METHOD ExportProject( doBackup AS LOGIC, asyncWorker AS BackgroundWorker ) AS LOGIC
            LOCAL result := TRUE AS LOGIC
            LOCAL exitExport := FALSE AS LOGIC
            VAR Dependencies := EnumerateDependencies( )
            // All strings written to ResultText will be written to the log File
            XPorterLogger.SetLoggerToFile( Path.Combine( SELF:outputPath, "VFPXPorter.log" )  )
            TRY
                // Before, we need to enumerate all Controls that belongs to Libraries
                // These definitions will be used when converting Control Properties
                VAR newControls := Dictionary<STRING, SCXVCXItem>{}
                VAR generatedNamespaces := List<STRING>{}
                FOREACH libName AS STRING IN Dependencies
                    //
                    VAR xPorter := XPorterSCXVCX{}
                    xPorter:Worker := asyncWorker
                    VAR output := SELF:outputPath
                    IF SELF:Settings:StoreInFolders
                        output := Path.Combine( output, SELF:Settings:FolderNames["Libs"])
                    ENDIF
                    IF ( SELF:Settings:LibInSubFolder )
                        output := Path.Combine( output, Path.GetFileNameWithoutExtension(libName ) )
                        Directory.CreateDirectory( output )
                    ENDIF
                    xPorter:Initialize( libName, output, SELF:Settings )
                    SELF:CurrentFileName := xPorter:FileName
                    IF xPorter:Analyze(FALSE)
                        // Add all the new Custom Controls
                        newControls:AddRangeNewOnly<STRING,SCXVCXItem>( xPorter:DefiningControls )
                    ENDIF
                NEXT

                // First, the Forms
                FOREACH form AS ProjectItem IN SELF:Project:Forms
                    //
                    VAR xPorter := XPorterSCXVCX{ }
                    xPorter:Worker := asyncWorker
                    VAR output := SELF:outputPath
                    IF SELF:Settings:StoreInFolders
                        output := Path.Combine( output, SELF:Settings:FolderNames["Forms"])
                    ENDIF
                    xPorter:Initialize( Path.Combine( SELF:Project:HomeDir, form:Name), output, SELF:Settings )
                    SELF:CurrentFileName := xPorter:FileName
                    XPorterLogger.Instance:Information( "Export Form " + Path.Combine( SELF:Project:HomeDir, form:Name))
                    xPorter:CustomControls := newControls
                    result := xPorter:Export( doBackup )
                    XPorterLogger.Instance:Information( xPorter:ResultText )
                    IF (!result .AND. !SELF:Settings:IgnoreErrors) .OR. ( xPorter:Canceled )
                        exitExport := TRUE
                        EXIT
                    ENDIF
                    IF result
                        IF !String.IsNullOrEmpty( xPorter:NamespaceDefinition )
                            generatedNamespaces:Add( xPorter:NamespaceDefinition )
                        ENDIF
                        SELF:GeneratedFiles:AddRange( xPorter:GeneratedFiles )
                    ELSE
                        XPorterLogger.Instance:Error( form:Name + " : Processing raised an Error." )
                    ENDIF
                NEXT
                IF !exitExport
                    // Then the libraries
                    FOREACH libName AS STRING IN Dependencies
                        //
                        VAR xPorter := XPorterSCXVCX{ }
                        xPorter:Worker := asyncWorker
                        VAR output := SELF:outputPath
                        IF SELF:Settings:StoreInFolders
                            output := Path.Combine( output, SELF:Settings:FolderNames["Libs"])
                        ENDIF
                        IF ( SELF:Settings:LibInSubFolder )
                            output := Path.Combine( output, Path.GetFileNameWithoutExtension(libName ) )
                            Directory.CreateDirectory( output )
                        ENDIF
                        xPorter:Initialize( libName, output, SELF:Settings )
                        SELF:CurrentFileName := xPorter:FileName
                        XPorterLogger.Instance:Information( "Export Lib " + libName )
                        xPorter:CustomControls := newControls
                        result := xPorter:Export( doBackup )
                        XPorterLogger.Instance:Information( xPorter:ResultText )
                        IF (!result .AND. !SELF:Settings:IgnoreErrors) .OR. ( xPorter:Canceled )
                            exitExport := TRUE
                            EXIT
                        ENDIF
                        IF result
                            IF !String.IsNullOrEmpty( xPorter:NamespaceDefinition )
                                generatedNamespaces:Add( xPorter:NamespaceDefinition )
                            ENDIF
                            SELF:GeneratedLibFiles:AddRange( xPorter:GeneratedFiles )
                        ELSE
                            XPorterLogger.Instance:Error( libName + " : Processing raised an Error." )
                        ENDIF
                    NEXT
                    IF !exitExport
                        // Export Menus
                        FOREACH menu AS ProjectItem IN SELF:Project:Menus
                            //
                            VAR xPorter := VFPXPorterMenu{ }
                            xPorter:Worker := asyncWorker
                            VAR output := SELF:Settings:OutputPath
                            IF SELF:Settings:StoreInFolders
                                output := Path.Combine( output, SELF:Settings:FolderNames["Menus"])
                            ENDIF
                            xPorter:Initialize( Path.Combine( SELF:Project:HomeDir, menu:Name), output, SELF:Settings )
                            SELF:CurrentFileName := xPorter:FileName
                            XPorterLogger.Instance:Information( "Export Menu " + Path.Combine( SELF:Project:HomeDir, menu:Name))
                            result := xPorter:Export( doBackup )
                            XPorterLogger.Instance:Information( xPorter:ResultText )
                            IF (!result .AND. !SELF:Settings:IgnoreErrors) .OR. ( xPorter:Canceled )
                                exitExport := TRUE
                                EXIT
                            ENDIF
                            IF result
                                SELF:GeneratedFiles:AddRange( xPorter:GeneratedFiles )
                            ENDIF
                        NEXT
                        IF !exitExport
                            // Export Prgs
                            FOREACH prg AS ProjectItem IN SELF:Project:Programs
                                //
                                VAR output := SELF:outputPath
                                LOCAL orgFile AS STRING
                                IF SELF:Settings:StoreInFolders
                                    output := Path.Combine( output, SELF:Settings:FolderNames["Code"])
                                    Directory.CreateDirectory(output)
                                ENDIF
                                orgFile := Path.Combine( SELF:Project:HomeDir, prg:Name)
                                LOCAL destFile AS STRING
                                destFile := Path.GetFileName( prg:Name )
                                destFile := Path.Combine(output, destFile )
                                IF File.Exists( orgFile )
                                    LOCAL sttmnts AS List<STRING>
                                    // Todo Use a Extension Method, in order to centralize
                                    sttmnts := JsonConvert.DeserializeObject<List<STRING>>( File.ReadAllText(XPorterSettings.StatementsFile) )
                                    // Now, copy
                                    VAR converter := CodeConverter{ SELF:Settings:KeepOriginal, FALSE, FALSE, SELF:Settings:ConvertStatement, SELF:Settings:ConvertStatementOnlyIfLast }
                                    converter:Statements := sttmnts
                                    converter:ProcessProcedure( File.ReadAllText(orgFile), Path.GetFileNameWithoutExtension( orgFile ) )
                                    File.WriteAllText( destFile, converter:ToString() )
                                    //File.Copy(orgFile, destFile, TRUE )
                                    // and add to Project
                                    SELF:GeneratedFiles:Add( GeneratedFile{destFile })
                                ELSE
                                    XPorterLogger.Instance:Information( "Unknown file " + orgFile )
                                ENDIF
                            NEXT
                            // Export Databases
                            FOREACH dbc AS ProjectItem IN SELF:Project:Databases
                                //
                                VAR output := SELF:outputPath
                                IF SELF:Settings:StoreInFolders
                                    output := Path.Combine( output, SELF:Settings:FolderNames["Databases"])
                                    Directory.CreateDirectory(output)
                                ENDIF
                                LOCAL orgFile AS STRING
                                orgFile := Path.Combine( SELF:Project:HomeDir, dbc:Name)
                                LOCAL destFile AS STRING
                                destFile := Path.GetFileName( dbc:Name )
                                destFile := Path.Combine(output, destFile )
                                IF File.Exists( orgFile )
                                    // Now, copy
                                    File.Copy(orgFile, destFile, TRUE )
                                    // and add to Project
                                    SELF:GeneratedFiles:Add( GeneratedFile{destFile, FileAction.CopyAlways })
                                ELSE
                                    XPorterLogger.Instance:Information( "Unknown file " + orgFile )
                                ENDIF
                            NEXT
                            // Free Tables
                            FOREACH dbf AS ProjectItem IN SELF:Project:FreeTables
                                //
                                VAR output := SELF:outputPath
                                IF SELF:Settings:StoreInFolders
                                    output := Path.Combine( output, SELF:Settings:FolderNames["FreeTables"])
                                    Directory.CreateDirectory(output)
                                ENDIF
                                LOCAL orgFile AS STRING
                                orgFile := Path.Combine( SELF:Project:HomeDir, dbf:Name)
                                LOCAL destFile AS STRING
                                destFile := Path.GetFileName( dbf:Name )
                                destFile := Path.Combine(output, destFile )
                                IF File.Exists( orgFile )
                                    // Now, copy
                                    File.Copy(orgFile, destFile, TRUE )
                                    // and add to Project
                                    SELF:GeneratedFiles:Add( GeneratedFile{destFile, FileAction.CopyAlways })
                                ELSE
                                    XPorterLogger.Instance:Information( "Unknown file " + orgFile )
                                ENDIF
                            NEXT
                            // Others
                            FOREACH other AS ProjectItem IN SELF:Project:Others
                                //
                                VAR output := SELF:outputPath
                                IF SELF:Settings:StoreInFolders
                                    output := Path.Combine( output, SELF:Settings:FolderNames["Others"])
                                    Directory.CreateDirectory(output)
                                ENDIF
                                LOCAL orgFile AS STRING
                                orgFile := Path.Combine( SELF:Project:HomeDir, other:Name)
                                LOCAL destFile AS STRING
                                destFile := Path.GetFileName( other:Name )
                                destFile := Path.Combine(output, destFile )
                                IF File.Exists( orgFile )
                                    // Now, copy
                                    TRY
                                        File.Copy(orgFile, destFile, TRUE )
                                        // and add to Project
                                        SELF:GeneratedFiles:Add( GeneratedFile{destFile, FileAction.CopyAlways })
                                    CATCH e AS Exception
                                        // Log any trouble
                                        XPorterLogger.Instance:Error( "Copy Other Files" )
                                        XPorterLogger.Instance:Error( e.Message )
                                    END TRY
                                ELSE
                                    XPorterLogger.Instance:Information( "Unknown file " + orgFile )
                                ENDIF
                            NEXT
                            // Now, inject external Tools/Helper if any
                            VAR toolFiles := List<STRING>{}
                            toolFiles:AddRange( Directory.GetFiles( SELF:ToolsFolder, "*.prg") )
                            toolFiles:AddRange( Directory.GetFiles( SELF:ToolsFolder, "*.xh") )
                            toolFiles:AddRange( Directory.GetFiles( SELF:ToolsFolder, "*.dll") )
                            //
                            LOCAL stdDef := "" AS STRING
                            LOCAL vfpxporterPath := "" AS STRING
                            FOREACH VAR toolFile IN toolFiles
                                VAR output := SELF:outputPath
                                IF SELF:Settings:StoreInFolders
                                    output := Path.Combine( output, "XSharp")
                                    Directory.CreateDirectory(output)
                                ENDIF
                                LOCAL destFile AS STRING
                                destFile := Path.GetFileName( toolFile )
                                destFile := Path.Combine(output, destFile )
                                // Now, copy
                                File.Copy(toolFile, destFile, TRUE )
                                IF toolFile:EndsWith( ".xh" )
                                    IF !stdDef:ToLower():EndsWith( "vfpxporter.xh" )
                                        stdDef := "$(Solutiondir)"+GetRelativePath(SELF:outputPath,destFile)
                                        vfpxporterPath := destFile
                                    ENDIF
                                ENDIF
                                // and add to Project
                                IF toolFile:EndsWith( ".prg" )
                                    SELF:GeneratedFiles:Add( GeneratedFile{destFile })
                                    // and to libs
                                    IF SELF:GeneratedLibFiles:Count > 0
                                        SELF:GeneratedLibFiles:Add( GeneratedFile{destFile})
                                    ENDIF
                                ELSEIF toolFile:EndsWith( ".dll" )
                                    SELF:ReferenceFiles:Add( Reference{GetRelativePath( SELF:outputPath,destFile),FALSE,TRUE })
                                    IF SELF:GeneratedLibFiles:Count > 0
                                        SELF:ReferenceLibFiles:Add( Reference{GetRelativePath( SELF:outputPath,destFile),FALSE,TRUE })
                                    ENDIF
                                ELSE
                                    SELF:GeneratedFiles:Add( GeneratedFile{destFile, FileAction.DoNotCopy })
                                    // Add that also the libs if needed
                                    IF SELF:GeneratedLibFiles:Count > 0
                                        SELF:GeneratedLibFiles:Add( GeneratedFile{destFile, FileAction.DoNotCopy })
                                    ENDIF
                                ENDIF
                            NEXT
                            // If we have generated some Namespaces, add them at the end of the VFPXPorter.xh
                            IF !String.IsNullOrEmpty( vfpxporterPath )
                                VAR uniqueNamespaces := generatedNamespaces:Distinct():ToList()
                                VAR headerText := File.ReadAllText( vfpxporterPath )
                                //
                                headerText += Environment.NewLine
                                headerText += Environment.NewLine + "// VFPXPorter - Generated Namespaces"
                                FOREACH VAR definition IN uniqueNamespaces
                                    headerText += Environment.NewLine + "USING " + definition
                                NEXT
                                headerText += Environment.NewLine
                                File.WriteAllText( vfpxporterPath, headerText )
                            ENDIF
                            // And Don't forget the Starting block
                            IF SELF:Project:Main != NULL
                                // The File to be created
                                LOCAL destFile AS STRING
                                destFile := Path.GetFileName( SELF:StartBlockFile )
                                destFile := Path.Combine(SELF:outputPath, destFile  )
                                destFile := Path.ChangeExtension( destFile, ".prg")
                                VAR dest := StreamWriter{ destFile }
                                // The Code template file
                                LOCAL code AS StringBuilder
                                code := StringBuilder{}
                                code:Append( SELF:StartBlock )
                                //
                                LOCAL start AS StringBuilder
                                start := StringBuilder{}
                                // A prg with a Procedure ?
                                IF SELF:Project:Main:Name:EndsWith(".prg")
                                    start:Append( Path.GetFileNameWithoutExtension( SELF:Project:Main:Name ) )
                                    start:AppendLine( "()" )
                                ELSE
                                    // A Form ?
                                    start:AppendLine( "Application.EnableVisualStyles()" )
                                    start:AppendLine( "Application.SetCompatibleTextRenderingDefault( FALSE )" )
                                    start:Append( "Application.Run( " )
                                    start:Append( Path.GetFileNameWithoutExtension( SELF:Project:Main:Name ) )
                                    start:AppendLine( "{} )" )
                                ENDIF
                                //
                                code := code:Replace( "<@startcode@>", start:ToString())
                                dest:Write( code:ToString() )
                                dest:Close()
                                //
                                SELF:GeneratedFiles:Add( GeneratedFile{destFile})
                            ENDIF
                            // Export the informations in an Visual Studio xsproj file
                            SELF:GenerateSolution( stdDef )
                        ENDIF
                    ENDIF
                ENDIF
            CATCH e AS Exception
                //
                XPorterLogger.Instance:Error("Export Project")
                XPorterLogger.Instance:Error( e.Message)
                THROW e
            FINALLY
                XPorterLogger.SetLoggerToFile( NULL )
            END TRY

            RETURN !exitExport


        PRIVATE METHOD AddStandardReferences( xsprj AS VSProject ) AS VOID
            xsprj:AddReference( "mscorlib" )
            xsprj:AddReference("System" )
            xsprj:AddReference("System.Core" )
            xsprj:AddReference("System.Data" )
            xsprj:AddReference("System.Data.DataSetExtensions" )
            xsprj:AddReference("System.Xml" )
            xsprj:AddReference("System.Xml.Linq" )
            xsprj:AddReference("System.Drawing" )
            xsprj:AddReference("System.Windows.Forms" )
            xsprj:AddReference("XSharp.Core",TRUE)
            xsprj:AddReference("XSharp.RT",TRUE)
            xsprj:AddReference("XSharp.RDD",TRUE)
            xsprj:AddReference("XSharp.Data",TRUE)
            xsprj:AddReference("XSharp.VFP",TRUE)
        END METHOD

        // Generate a MSBuild file
        PRIVATE METHOD GenerateSolution( stdDef AS STRING ) AS VOID
            LOCAL destFile AS STRING
            LOCAL xsLibs := NULL AS VSProject
            // First, do we have any "Libraries" ?
            IF SELF:GeneratedLibFiles:Count > 0
                xsLibs := VSProject{ "ClassLibraries" }
                xsLibs:IsLibrary := TRUE
                SELF:AddStandardReferences( xsLibs )
                //
                FOREACH refFile AS Reference IN SELF:ReferenceLibFiles
                    xsLibs:AddReference( refFile )
                NEXT
                //
                FOREACH codeFile AS GeneratedFile IN SELF:GeneratedLibFiles
                    //
                    IF codeFile:Action != FileAction.Compile
                        xsLibs:AddFile( GetRelativePath( SELF:outputPath,codeFile:FileName), codeFile:Action )
                    ELSE
                        IF String.IsNullOrEmpty( codeFile:DependsOn )
                            xsLibs:AddFile( GetRelativePath( SELF:outputPath,codeFile:FileName), codeFile:Type )
                        ELSE
                            xsLibs:AddFile( GetRelativePath( SELF:outputPath,codeFile:FileName), codeFile:Type, GetRelativePath( SELF:outputPath,codeFile:DependsOn) )
                        ENDIF
                    ENDIF
                NEXT
                //
                destFile := Path.Combine( SELF:outputPath, "ClassLibraries" )
                destFile := Path.ChangeExtension( destFile, "xsproj")
                // Save the MSBuild file for the Libraries
                xsLibs:Save( destFile, stdDef )
            ENDIF
            // Now the Main Project
            destFile := Path.GetFileName( SELF:pjxFilePath )
            destFile := Path.Combine( SELF:outputPath, destFile )
            destFile := Path.ChangeExtension( destFile, "xsproj")
            // The imported Project : We will add "App" at the end of the ProjectName to avoid conflicts in the Name Property
            VAR xsProj := VSProject{ Path.GetFileNameWithoutExtension( SELF:pjxFilePath )}
            SELF:AddStandardReferences( xsProj )
            //
            FOREACH refFile AS Reference IN SELF:ReferenceFiles
                xsProj:AddReference( refFile )
            NEXT
            //
            FOREACH codeFile AS GeneratedFile IN SELF:GeneratedFiles
                //
                IF codeFile:Action != FileAction.Compile
                    xsProj:AddFile( GetRelativePath( SELF:outputPath,codeFile:FileName), codeFile:Action )
                ELSE
                    IF String.IsNullOrEmpty( codeFile:DependsOn )
                        xsProj:AddFile( GetRelativePath( SELF:outputPath,codeFile:FileName), codeFile:Type )
                    ELSE
                        xsProj:AddFile( GetRelativePath( SELF:outputPath,codeFile:FileName), codeFile:Type, GetRelativePath( SELF:outputPath,codeFile:DependsOn) )
                    ENDIF
                ENDIF
            NEXT
            //
            IF xsLibs != NULL
                xsProj:ProjectReferenceList:Add( xsLibs )
            ENDIF
            // Save the MSBuild file for the "main" Project
            xsProj:Save( destFile, stdDef )
            // Now the Solution
            VAR xsSolution := VSSolution{}
            xsSolution:Projects:Add( xsProj )
            IF xsLibs != NULL
                xsSolution:Projects:Add( xsLibs )
            ENDIF
            destFile := Path.GetFileName( SELF:pjxFilePath )
            destFile := Path.Combine( SELF:outputPath, destFile )
            destFile := Path.ChangeExtension( destFile, "sln")
            xsSolution:Save( destFile )
            //
            RETURN

            // Backup the PJX Items : Create an XML File with Items info, and export the associated Code
        METHOD SerializePJX( itemList AS List<PJXItem> ) AS VOID
            // Now, extract all Code into this Folder
            VAR tempPath := Path.Combine( SELF:outputPath, "Backup" )
            // Warning, we may NOT be able to create the Directory
            Directory.CreateDirectory( tempPath )
            // Save the Serialization
            LOCAL destFile AS STRING
            destFile := Path.GetFileName( SELF:pjxFilePath )
            destFile := Path.Combine(tempPath, destFile )
            destFile := Path.ChangeExtension( destFile, "xml")
            //
            LOCAL writer AS StreamWriter
            writer := StreamWriter{ destFile }
            VAR result := XmlSerializer{ TYPEOF( XmlExportProject ) }
            VAR xPorter := XmlExportProject{ itemList }
            result:Serialize( writer, xPorter )
            writer:Close()
            RETURN

        /// <summary>
        /// For each Form, retrieve the Libraries it depends on.
        /// Then enumerate the Libraries : For each Library, retrieve the Libraries it depends on.
        /// </summary>
        /// <returns>Returns a HashSet<String> with all the dependencies.</returns>
        PRIVATE METHOD EnumerateDependencies() AS HashSet<STRING>
            LOCAL Dependencies AS HashSet<STRING>
            Dependencies := HashSet<STRING>{}
            // Enumerate Forms and search for Dependencies
            FOREACH form AS ProjectItem IN SELF:Project:Forms
                //
                VAR xPorter := XPorterSCXVCX{  }
                xPorter:Initialize( Path.Combine( SELF:Project:HomeDir, form:Name), SELF:outputPath, SELF:Settings )
                xPorter:Analyze(FALSE)
                Dependencies:UnionWith( xPorter:DependsOn )
            NEXT
            // Then the libraries
            FOREACH lib AS ProjectItem IN SELF:Project:Libraries
                // The Library is a dependency
                Dependencies:Add( lib:Name )
            NEXT
            // Ok, then, look for All dependencies, and Dependencies of dependencies, and ...
            Dependencies := EnumerateDependencies( Dependencies )
            RETURN Dependencies

        PRIVATE METHOD EnumerateDependencies( Dependencies AS HashSet<STRING> ) AS HashSet<STRING>
            LOCAL toCheck AS HashSet<STRING>
            LOCAL finalList AS HashSet<STRING>

            finalList := HashSet<STRING>{  }
            //
            DO WHILE Dependencies:Count > 0
                toCheck := HashSet<STRING>{}
                FOREACH VAR classLib IN Dependencies
                    //
                    VAR xPorter := XPorterSCXVCX{ }
                    //
                    VAR libPath  := classLib
                    IF ! IsFullPath( classLib )
                        libPath := Path.Combine( SELF:Project:HomeDir, libPath )
                    ENDIF
                    finalList:Add( libPath )
                    //
                    xPorter:Initialize( libPath, SELF:outputPath,SELF:Settings )
                    IF xPorter:Analyze(FALSE)
                        // Add Found Dependencies
                        toCheck:UnionWith( xPorter:DependsOn )
                    ENDIF
                NEXT
                // Save what we have done
                //finalList:UnionWith( Dependencies )
                // Build the list we need to check
                // Are in toCheck and not in finalList
                toCheck:ExceptWith( finalList )
                // Done ?
                IF ( toCheck:Count == 0 )
                    EXIT
                ENDIF
                // So, the new list is ...
                Dependencies := toCheck
            ENDDO
            //
            RETURN finalList
    END CLASS

    ENUM FileAction
        MEMBER Compile
        MEMBER DoNotCopy
        MEMBER PreserveNewest
        MEMBER CopyAlways
    END ENUM

    CLASS GeneratedFile
        PROPERTY FileName AS STRING AUTO
        PROPERTY Type AS STRING AUTO
        PROPERTY DependsOn AS STRING AUTO
        PROPERTY Action AS FileAction AUTO

        CONSTRUCTOR( fName AS STRING )
            SELF:FileName := fName
            SELF:Type := ""
            SELF:DependsOn := ""
            SELF:Action := FileAction.Compile

        CONSTRUCTOR( fName AS STRING, faAction AS FileAction )
            SELF:FileName := fName
            SELF:Type := ""
            SELF:DependsOn := ""
            SELF:Action := faAction

        CONSTRUCTOR( fName AS STRING, fType AS STRING )
            SELF:FileName := fName
            SELF:Type := fType
            SELF:DependsOn := ""
            SELF:Action := FileAction.Compile

        CONSTRUCTOR( fName AS STRING, fType AS STRING, depends AS STRING )
            SELF:FileName := fName
            SELF:Type := fType
            SELF:DependsOn := depends
            SELF:Action := FileAction.Compile

    END CLASS


END NAMESPACE // FabVFPXPorter
