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


        /// <summary>
        /// File path to the .pjx file to process
        /// </summary>
        PRIVATE pjxFilePath AS STRING

        /// <summary>
        /// Output "Root" Folder where the generated files will be stored
        /// </summary>
        PRIVATE outputPath AS STRING

        PROPERTY Project AS VFPProject AUTO

        /// <summary>
        /// Folder where all "injected" files are coming from (like the StartBlock, or any helper Tools)
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

        // Per-VCX dependency map: absolute VCX path → set of absolute VCX paths it directly depends on.
        // Populated during ExportProject(); consumed by GenerateSolution() when SeparateLibraryProjects=TRUE.
        PRIVATE _vcxDependencies AS Dictionary<STRING, HashSet<STRING>>

        // Per-VCX generated files: absolute VCX path → files produced by exporting that VCX.
        // Populated during ExportProject(); consumed by GenerateSolution() when SeparateLibraryProjects=TRUE.
        PRIVATE _libFilesByVCX AS Dictionary<STRING, List<GeneratedFile>>

        PROPERTY ReferenceFiles AS List<Reference> AUTO

        PROTECTED StartBlockFile AS STRING
        PROPERTY StartBlock AS STRING GET File.ReadAllText( StartBlockFile )

        PROPERTY CurrentFileName AS STRING AUTO GET PRIVATE SET

        CONSTRUCTOR( projectFullFilePath AS STRING, destinationRootFolder AS STRING )
            //
            SELF:pjxFilePath := projectFullFilePath
            SELF:outputPath := destinationRootFolder
            //
            SELF:ToolsFolder := XPorterSettings.ToolsFolder
            SELF:StartBlockFile := XPorterSettings.StartFile
            //
            SELF:GeneratedFiles := List<GeneratedFile>{}
            SELF:GeneratedLibFiles := List<GeneratedFile>{}
            SELF:ReferenceFiles := List<Reference>{}
            SELF:ReferenceLibFiles := List<Reference>{}
            SELF:_vcxDependencies := Dictionary<STRING, HashSet<STRING>>{ StringComparer.OrdinalIgnoreCase }
            SELF:_libFilesByVCX   := Dictionary<STRING, List<GeneratedFile>>{ StringComparer.OrdinalIgnoreCase }
            //

        METHOD ClearResultText() AS VOID
            BufferedSink.Instance:Clear()
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
                VAR alias := System.IO.Path.GetFileNameWithoutExtension(SELF:pjxFilePath)
                IF !DbUseArea(TRUE, "DBFVFP", SELF:pjxFilePath, alias, FALSE, TRUE )
                    XPorterLogger.Instance:Error("LoadProject: DbUseArea failed to open: " + SELF:pjxFilePath)
                    RETURN FALSE
                ENDIF
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
                XPorterLogger.Instance:Error("LoadProject: Failed to process PJX file: " + SELF:pjxFilePath)
                XPorterLogger.Instance:Error("Exception: " + ex:Message)
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
                XPorterLogger.Instance:Information( "Forms : " + SELF:Project:Forms:Count:ToString() )
                FOREACH form AS ProjectItem IN SELF:Project:Forms
                    XPorterLogger.Instance:Verbose( "  - " + form:Name )
                NEXT
                //
                XPorterLogger.Instance:Information( "Libraries/Dependencies : " + SELF:Project:Libraries:Count:ToString() )
                FOREACH lib AS ProjectItem IN SELF:Project:Libraries
                    XPorterLogger.Instance:Verbose( "  - " + lib:Name )
                NEXT
                //
                XPorterLogger.Instance:Information( "Menus : " + SELF:Project:Menus:Count:ToString() )
                FOREACH lib AS ProjectItem IN SELF:Project:Menus
                    XPorterLogger.Instance:Verbose( "  - " + lib:Name )
                NEXT
                //
                XPorterLogger.Instance:Information( "Reports : " + SELF:Project:Reports:Count:ToString() )
                FOREACH report AS ProjectItem IN SELF:Project:Reports
                    XPorterLogger.Instance:Verbose( "  - " + report:Name )
                NEXT
                //
                XPorterLogger.Instance:Information( "Programs : " + SELF:Project:Programs:Count:ToString() )
                FOREACH prg AS ProjectItem IN SELF:Project:Programs
                    XPorterLogger.Instance:Verbose( "  - " + prg:Name )
                NEXT
                //
                XPorterLogger.Instance:Information( "DataBases : " + SELF:Project:Databases:Count:ToString() )
                FOREACH dbc AS ProjectItem IN SELF:Project:Databases
                    XPorterLogger.Instance:Verbose( "  - " + dbc:Name )
                NEXT
                //
                XPorterLogger.Instance:Information( "FreeTables : " + SELF:Project:FreeTables:Count:ToString() )
                FOREACH dbf AS ProjectItem IN SELF:Project:FreeTables
                    XPorterLogger.Instance:Verbose( "  - " + dbf:Name )
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
            VAR Dependencies := SELF:EnumerateDependencies( )
            //
            TRY
                // All strings written to ResultText will be written to the log File
                XPorterLogger.SetLoggerToFile( Path.Combine( SELF:outputPath, "VFPXPorter.log" )  )
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
                        // Capture per-VCX dependency set for graph-based project generation
                        _vcxDependencies:Add(libName, xPorter:DependsOn)
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
                            XPorterLogger.Instance:Error("ExportProject: Failed to export form: " + form:Name)
                            XPorterLogger.Instance:Error("Details: " + xPorter:ResultText)
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
                            // Bucket files by VCX for per-library project generation
                            _libFilesByVCX:Add(libName, xPorter:GeneratedFiles)
                        ELSE
                            XPorterLogger.Instance:Error("ExportProject: Failed to export library: " + libName)
                            XPorterLogger.Instance:Error("Details: " + xPorter:ResultText)
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
                                    LOCAL colorProps AS List<STRING>
                                    colorProps := JsonConvert.DeserializeObject<List<STRING>>( File.ReadAllText(XPorterSettings.ColorPropertiesFile) )
                                    // Now, copy
                                    VAR converter := CodeConverter{ SELF:Settings:KeepOriginal, FALSE, SELF:Settings:ConvertStatement, SELF:Settings:ConvertStatementOnlyIfLast }
                                    converter:Statements := sttmnts
                                    converter:ColorProperties := colorProps
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
                                        XPorterLogger.Instance:Error("ExportProject: Failed to copy file: " + orgFile)
                                        XPorterLogger.Instance:Error("Destination: " + destFile)
                                        XPorterLogger.Instance:Error("Exception: " + e:Message)
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
                                        stdDef := "$(projectdir)"+GetRelativePath(SELF:outputPath,destFile)
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
                                VAR comment := "// "
                                IF Settings:AddLibraryNamespace
                                    comment := ""
                                ENDIF
                                //
                                headerText += Environment.NewLine
                                headerText += Environment.NewLine + "// VFPXPorter - Generated Namespaces"
                                FOREACH VAR definition IN uniqueNamespaces
                                    headerText += Environment.NewLine + comment + "USING " + definition
                                NEXT
                                headerText += Environment.NewLine
                                File.WriteAllText( vfpxporterPath, headerText )
                            ENDIF
                            // And Don't forget the Starting block
                            IF SELF:Project:Main != NULL .AND. SELF:Settings:OutputType == ProjectType.WindowsExe
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
                                VAR projectReplacements := Dictionary<STRING, STRING>{}
                                LOCAL start AS StringBuilder
                                start := StringBuilder{}
                                // A prg with a Procedure ?
                                IF SELF:Project:Main:Name:EndsWith(".prg")
                                    start:Append( Path.GetFileNameWithoutExtension( SELF:Project:Main:Name ) )
                                    start:Append( "()" )
                                    projectReplacements["startcode"] := start:ToString()
                                    projectReplacements["startform"] := ""
                                    projectReplacements["startmenu"] := ""
                                ELSE
                                    // A Form ? A Menu ?
                                    start:Append( Path.GetFileNameWithoutExtension( SELF:Project:Main:Name ) )
                                    start:Append( "{}" )
                                    projectReplacements["startcode"] := ""
                                    IF SELF:Project:Main:Name:EndsWith(".mnx")
                                        projectReplacements["startmenu"] := start:ToString()
                                        projectReplacements["startform"] := ""
                                    ELSE
                                        projectReplacements["startmenu"] := ""
                                        projectReplacements["startform"] := start:ToString()
                                    ENDIF
                                ENDIF
                                //

                                VAR resultCode := TemplateHelper.ReplaceAndValidate(code:ToString(), "StartBlock", projectReplacements)
                                dest:Write( resultCode )
                                dest:Close()
                                //
                                SELF:GeneratedFiles:Add( GeneratedFile{destFile})
                            ENDIF
                            // Export the informations in an Visual Studio xsproj file
                            SELF:GenerateSolution( stdDef, vfpxporterPath )
                        ENDIF
                    ENDIF
                ENDIF
            CATCH e AS Exception
                //
                XPorterLogger.Instance:Error("ExportProject: Failed to complete project export")
                XPorterLogger.Instance:Error("Exception: " + e.Message)
                THROW e
            FINALLY
                XPorterLogger.CloseLogger()
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

        // Returns VCX paths in dependency-first order (a library comes before every library that depends on it).
        // Uses Kahn's BFS algorithm. Logs a warning and appends remaining nodes if a cycle is detected.
        PRIVATE METHOD TopologicalSort(deps AS Dictionary<STRING, HashSet<STRING>>) AS List<STRING>
            // Seed every node (both keys and their dependencies)
            VAR inDegree := Dictionary<STRING, INT>{ StringComparer.OrdinalIgnoreCase }
            VAR outEdges := Dictionary<STRING, List<STRING>>{ StringComparer.OrdinalIgnoreCase }
            FOREACH VAR kv IN deps
                IF !inDegree:ContainsKey(kv:Key)
                    inDegree:Add(kv:Key, 0)
                ENDIF
                FOREACH dep AS STRING IN kv:Value
                    IF !inDegree:ContainsKey(dep)
                        inDegree:Add(dep, 0)
                    ENDIF
                NEXT
            NEXT
            // Set in-degree of each node = number of libraries it depends on;
            // build outEdges so that processing dep decrements its dependents.
            FOREACH VAR kv IN deps
                inDegree[kv:Key] := kv:Value:Count
                FOREACH dep AS STRING IN kv:Value
                    IF !outEdges:ContainsKey(dep)
                        outEdges:Add(dep, List<STRING>{})
                    ENDIF
                    outEdges[dep]:Add(kv:Key)
                NEXT
            NEXT
            // BFS from all zero-in-degree nodes (libraries with no dependencies)
            VAR queue := Queue<STRING>{}
            FOREACH VAR kv IN inDegree
                IF kv:Value == 0
                    queue:Enqueue(kv:Key)
                ENDIF
            NEXT
            VAR result := List<STRING>{}
            DO WHILE queue:Count > 0
                VAR node := queue:Dequeue()
                result:Add(node)
                IF outEdges:ContainsKey(node)
                    FOREACH dependent AS STRING IN outEdges[node]
                        inDegree[dependent] := inDegree[dependent] - 1
                        IF inDegree[dependent] == 0
                            queue:Enqueue(dependent)
                        ENDIF
                    NEXT
                ENDIF
            ENDDO
            // If nodes remain, a cycle exists — append them with a warning
            IF result:Count < inDegree:Count
                XPorterLogger.Instance:Warning("TopologicalSort: circular dependency detected among VCX files; build order may be incorrect.")
                FOREACH VAR kv IN inDegree
                    IF !result:Contains(kv:Key)
                        result:Add(kv:Key)
                    ENDIF
                NEXT
            ENDIF
            RETURN result
        END METHOD

        // Generate a MSBuild file
        PRIVATE METHOD GenerateSolution( stdDef AS STRING, vfpxporterPath AS STRING ) AS VOID
            LOCAL xsLibs := NULL AS VSProject
            VAR vcxProjects := Dictionary<STRING, VSProject>{ StringComparer.OrdinalIgnoreCase }

            // Compute solution location upfront — needed for per-library RelativePath calculation
            LOCAL solutionBasePath AS STRING
            IF SELF:Settings:PlaceSolutionInSameDirectory
                solutionBasePath := SELF:outputPath
            ELSE
                solutionBasePath := Path.GetDirectoryName( SELF:outputPath )
            ENDIF

            IF SELF:GeneratedLibFiles:Count > 0
                IF SELF:Settings:SeparateLibraryProjects
                    // ── Per-library projects (one .xsproj per VCX) ─────────────
                    // Identify shared tool .prg files: in GeneratedLibFiles but not owned by any VCX bucket.
                    // These mirror what ClassLibraries received in monolithic mode.
                    VAR allVcxFiles := HashSet<STRING>{ StringComparer.OrdinalIgnoreCase }
                    FOREACH VAR bucket IN _libFilesByVCX:Values
                        FOREACH f AS GeneratedFile IN bucket
                            allVcxFiles:Add( f:FileName )
                        NEXT
                    NEXT
                    VAR toolLibFiles := SELF:GeneratedLibFiles:Where({ f => f:Action == FileAction.Compile .AND. !allVcxFiles:Contains( f:FileName ) }):ToList()

                    VAR sortedVCXs := SELF:TopologicalSort( _vcxDependencies )
                    FOREACH libPath AS STRING IN sortedVCXs
                        IF !_libFilesByVCX:ContainsKey( libPath )
                            LOOP  // no generated files for this VCX (export failed)
                        ENDIF
                        VAR libName := Path.GetFileNameWithoutExtension( libPath ):Replace(" ", "_")
                        VAR xsLib := VSProject{ libName }
                        xsLib:IsLibrary := TRUE
                        SELF:AddStandardReferences( xsLib )

                        // Compute library output folder early — needed for all path rebasing below
                        VAR libFolder := SELF:outputPath
                        IF SELF:Settings:StoreInFolders
                            libFolder := Path.Combine( libFolder, SELF:Settings:FolderNames["Libs"] )
                        ENDIF
                        IF SELF:Settings:LibInSubFolder
                            libFolder := Path.Combine( libFolder, libName )
                        ENDIF

                        // Add local references rebased to this library's project folder
                        FOREACH refFile AS Reference IN SELF:ReferenceLibFiles
                            IF refFile:IsLocal
                                VAR absRef := Path.GetFullPath( Path.Combine( SELF:outputPath, refFile:Include ) )
                                xsLib:AddReference( Reference{ GetRelativePath( libFolder, absRef ), refFile:IsXSharp, TRUE } )
                            ELSE
                                xsLib:AddReference( refFile )
                            ENDIF
                        NEXT

                        // Add this VCX's generated files (paths relative to the library's project folder)
                        FOREACH codeFile AS GeneratedFile IN _libFilesByVCX[libPath]
                            IF codeFile:Action != FileAction.Compile
                                xsLib:AddFile( GetRelativePath( libFolder, codeFile:FileName ), codeFile:Action )
                            ELSEIF String.IsNullOrEmpty( codeFile:DependsOn )
                                xsLib:AddFile( GetRelativePath( libFolder, codeFile:FileName ), codeFile:Type )
                            ELSE
                                xsLib:AddFile( GetRelativePath( libFolder, codeFile:FileName ), codeFile:Type, GetRelativePath( libFolder, codeFile:DependsOn ) )
                            ENDIF
                        NEXT

                        // Add shared tool .prg files (rebased to the library's project folder)
                        FOREACH toolFile AS GeneratedFile IN toolLibFiles
                            xsLib:AddFile( GetRelativePath( libFolder, toolFile:FileName ), toolFile:Type )
                        NEXT

                        // Wire ProjectReferences to direct deps (topo order guarantees they are already in vcxProjects)
                        IF _vcxDependencies:ContainsKey( libPath )
                            FOREACH depPath AS STRING IN _vcxDependencies[libPath]
                                IF vcxProjects:ContainsKey( depPath )
                                    xsLib:ProjectReferenceList:Add( vcxProjects[depPath] )
                                ENDIF
                            NEXT
                        ENDIF

                        // Compute per-library stdDef so VFPXPorter.xh resolves correctly from this subfolder
                        LOCAL libStdDef AS STRING
                        IF !String.IsNullOrEmpty( vfpxporterPath ) .AND. File.Exists( vfpxporterPath )
                            libStdDef := "$(projectdir)" + GetRelativePath( libFolder, vfpxporterPath )
                        ELSE
                            libStdDef := stdDef
                        ENDIF

                        VAR libProjPath := Path.Combine( libFolder, libName + ".xsproj" )
                        xsLib:Save( libProjPath, libStdDef )
                        xsLib:RelativePath := GetRelativePath( solutionBasePath, libProjPath )
                        vcxProjects:Add( libPath, xsLib )
                    NEXT
                ELSE
                    // ── Monolithic ClassLibraries (existing behaviour) ───────────
                    xsLibs := VSProject{ "ClassLibraries" }
                    xsLibs:IsLibrary := TRUE
                    SELF:AddStandardReferences( xsLibs )
                    FOREACH refFile AS Reference IN SELF:ReferenceLibFiles
                        xsLibs:AddReference( refFile )
                    NEXT
                    FOREACH codeFile AS GeneratedFile IN SELF:GeneratedLibFiles
                        IF codeFile:Action != FileAction.Compile
                            xsLibs:AddFile( GetRelativePath( SELF:outputPath, codeFile:FileName ), codeFile:Action )
                        ELSE
                            IF String.IsNullOrEmpty( codeFile:DependsOn )
                                xsLibs:AddFile( GetRelativePath( SELF:outputPath, codeFile:FileName ), codeFile:Type )
                            ELSE
                                xsLibs:AddFile( GetRelativePath( SELF:outputPath, codeFile:FileName ), codeFile:Type, GetRelativePath( SELF:outputPath, codeFile:DependsOn ) )
                            ENDIF
                        ENDIF
                    NEXT
                    VAR libProjPath := Path.Combine( SELF:outputPath, "ClassLibraries.xsproj" )
                    xsLibs:Save( libProjPath, stdDef )
                    IF SELF:Settings:PlaceSolutionInSameDirectory
                        xsLibs:RelativePath := "ClassLibraries.xsproj"
                    ELSE
                        xsLibs:RelativePath := Path.Combine( Path.GetFileNameWithoutExtension( SELF:outputPath ), "ClassLibraries.xsproj" )
                    ENDIF
                ENDIF
            ENDIF

            // ── Main project ─────────────────────────────────────────────────
            VAR projectName := Path.GetFileNameWithoutExtension( SELF:pjxFilePath )
            VAR projectPath := Path.Combine( SELF:outputPath, projectName + ".xsproj" )
            VAR xsProj := VSProject{ projectName }
            xsProj:ProjectType := SELF:Settings:OutputType
            SELF:AddStandardReferences( xsProj )
            FOREACH refFile AS Reference IN SELF:ReferenceFiles
                xsProj:AddReference( refFile )
            NEXT
            FOREACH codeFile AS GeneratedFile IN SELF:GeneratedFiles
                IF codeFile:Action != FileAction.Compile
                    xsProj:AddFile( GetRelativePath( SELF:outputPath, codeFile:FileName ), codeFile:Action )
                ELSE
                    IF String.IsNullOrEmpty( codeFile:DependsOn )
                        xsProj:AddFile( GetRelativePath( SELF:outputPath, codeFile:FileName ), codeFile:Type )
                    ELSE
                        xsProj:AddFile( GetRelativePath( SELF:outputPath, codeFile:FileName ), codeFile:Type, GetRelativePath( SELF:outputPath, codeFile:DependsOn ) )
                    ENDIF
                ENDIF
            NEXT
            // Reference the monolithic lib project (legacy) or all per-library projects
            IF xsLibs != NULL
                xsProj:ProjectReferenceList:Add( xsLibs )
            ENDIF
            FOREACH prj AS VSProject IN vcxProjects:Values
                xsProj:ProjectReferenceList:Add( prj )
            NEXT
            xsProj:Save( projectPath, stdDef )

            // ── Solution ──────────────────────────────────────────────────────
            VAR xsSolution := VSSolution{}

            LOCAL solutionName AS STRING
            IF !String.IsNullOrWhiteSpace( SELF:Settings:SolutionName )
                solutionName := SELF:Settings:SolutionName
            ELSE
                solutionName := projectName
            ENDIF

            VAR solutionFile := Path.Combine( solutionBasePath, solutionName + ".sln" )

            IF SELF:Settings:AppendToSolution .AND. File.Exists( solutionFile )
                xsSolution:Load( solutionFile )
            ENDIF

            VAR existing := xsSolution:Projects:Find({ p => String.Compare( p:Name, xsProj:Name, TRUE ) == 0 })
            IF existing != NULL
                xsSolution:Projects:Remove( existing )
            ENDIF

            LOCAL relativeProjPath AS STRING
            IF SELF:Settings:PlaceSolutionInSameDirectory
                relativeProjPath := projectName + ".xsproj"
            ELSE
                relativeProjPath := Path.Combine( Path.GetFileNameWithoutExtension( SELF:outputPath ), projectName + ".xsproj" )
            ENDIF
            xsProj:RelativePath := relativeProjPath
            xsSolution:Projects:Add( xsProj )

            IF xsLibs != NULL .AND. !xsSolution:Projects:Any({ p => String.Compare( p:Name, xsLibs:Name, TRUE ) == 0 })
                xsSolution:Projects:Add( xsLibs )
            ENDIF
            FOREACH prj AS VSProject IN vcxProjects:Values
                IF !xsSolution:Projects:Any({ p => String.Compare( p:Name, prj:Name, TRUE ) == 0 })
                    xsSolution:Projects:Add( prj )
                ENDIF
            NEXT

            xsSolution:Save( solutionFile )

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
            Dependencies := SELF:EnumerateDependencies( Dependencies )
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
