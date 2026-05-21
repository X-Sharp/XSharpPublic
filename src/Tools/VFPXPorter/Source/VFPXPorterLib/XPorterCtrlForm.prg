// XPorterCtrlForm.prg
// Created by    : fabri
// Creation Date : 9/25/2023 10:59:09 AM
// Created for   :
// WorkStation   : FABXPS

USING System
USING System.Collections.Generic
USING System.Text
USING System.IO
USING System.Text.RegularExpressions
USING Newtonsoft.Json
USING System.Xml.Serialization
USING System.ComponentModel

BEGIN NAMESPACE VFPXPorterLib

    /// <summary>
    /// The XPorterCtrlForm class.
    /// </summary>
    CLASS XPorterCtrlForm IMPLEMENTS IXPorterWorker

        PRIVATE PropRulesFile AS STRING
        PRIVATE EventRulesFile AS STRING
        PRIVATE ConvertTableFile AS STRING
        PRIVATE StatementsFile AS STRING
        PRIVATE VFPElementsFile AS STRING

        PRIVATE DesignerPrefixFile AS STRING
        PRIVATE DesignerStartTypeFile AS STRING
        PRIVATE DesignerEndTypeFile AS STRING
        PRIVATE DesignerInitTypeFile AS STRING
        PRIVATE FormPrefixFile AS STRING
        PRIVATE FormStartTypeFile AS STRING
        PRIVATE FormEndTypeFile AS STRING
        PRIVATE FormInitTypeFile AS STRING
        PRIVATE BindingCodeFile AS STRING

        // Template cache dictionaries to avoid repeated disk I/O
        PRIVATE _templateCache AS Dictionary<STRING, STRING>
        PRIVATE _jsonCache AS Dictionary<STRING, Dictionary<STRING, Dictionary<STRING,STRING>>>


        PROPERTY DesignerPrefix AS STRING GET SELF:GetTemplateFromCache(DesignerPrefixFile)
        PROPERTY DesignerStartType AS STRING GET SELF:GetTemplateFromCache(DesignerStartTypeFile)
        PROPERTY DesignerEndType AS STRING GET SELF:GetTemplateFromCache(DesignerEndTypeFile)
        PROPERTY DesignerInitType AS STRING GET SELF:GetTemplateFromCache(DesignerInitTypeFile)
        PROPERTY FormPrefix AS STRING GET SELF:GetTemplateFromCache(FormPrefixFile)
        PROPERTY FormStartType AS STRING GET SELF:GetTemplateFromCache(FormStartTypeFile)
        PROPERTY FormEndType AS STRING GET SELF:GetTemplateFromCache(FormEndTypeFile)
        PROPERTY FormInitType AS STRING GET SELF:GetTemplateFromCache(FormInitTypeFile)
        PROPERTY BindingCode AS STRING GET SELF:GetTemplateFromCache(BindingCodeFile)

        PROPERTY NamespaceDefinition AS STRING AUTO

        /// <summary>
        /// In a library, all elements are exported in a single file, named with the scx file
        /// </summary>
        PROPERTY ExportInOneFile AS LOGIC AUTO

        PROPERTY GeneratedFiles AS List<GeneratedFile> AUTO

        //PROPERTY FormNameOverride AS STRING AUTO


            // Used during export process
        PRIVATE _typeList AS Dictionary<STRING,STRING[]>

        PRIVATE _propertiesRules AS Dictionary<STRING, Dictionary<STRING,STRING> >

        PRIVATE _defaultValues AS Dictionary<STRING,STRING>



        /// <summary>
        /// A Dictionary of CustomControls defined in the current Library
        /// </summary>
        PROPERTY DefiningControls AS Dictionary<STRING, SCXVCXItem> AUTO


        /// <summary>
        /// Rules that applies to Properties (Caption->Text, ...)
        /// </summary>
        /// <value></value>
        PROPERTY PropRules AS STRING GET SELF:GetJsonFromCache( PropRulesFile )

        /// <summary>
        /// Rules that applies to Events (DblClick->DoubleClick, ...)
        /// </summary>
        /// <value></value>
        PROPERTY EventRules AS STRING GET SELF:GetJsonFromCache( EventRulesFile )

        /// <summary>
        /// Type convertion table (Button->VFPCommandButton, ...)
        /// </summary>
        /// <value></value>
        PROPERTY ConvertTable AS STRING GET SELF:GetJsonFromCache( ConvertTableFile )

        /// <summary>
        /// List of Statements than will need () at the end (.Refresh->.Refresh(), ...)
        /// </summary>
        /// <value></value>
        PROPERTY Statements AS STRING GET SELF:GetJsonFromCache( StatementsFile )

        /// <summary>
        /// List of VFP language elements and their traduction (Parent->_Parent,this->thisObject, ...)
        /// </summary>
        /// <value></value>
        PROPERTY VFPElements AS STRING GET SELF:GetJsonFromCache( VFPElementsFile )


        /// <summary>
        /// The List of Items that exist in the current file (Form/Library)
        /// </summary>
        PROPERTY Items AS List<BaseItem> AUTO

        /// <summary>
        /// The current File
        /// </summary>
        PROPERTY CurrentFile AS STRING GET SELF:Settings:ItemsPath

        /// <summary>
        /// The list of Controls that are defined in Libraries that the Form/Control might depend on
        /// </summary>
        PROPERTY CustomControls AS Dictionary<STRING, SCXVCXItem> AUTO SET PRIVATE GET

        /// <summary>
        /// Does this component contains a FormSet ?
        /// </summary>
        /// <value></value>
        PROPERTY HasFormSet AS LOGIC AUTO

        PROPERTY Settings AS XPorterSettings AUTO

        CONSTRUCTOR( xportSettings AS XPorterSettings)
            SELF:Settings := xportSettings
            // Default Setting files
            SELF:EventRulesFile		:= XPorterSettings.EventRulesFile
            SELF:PropRulesFile		:= XPorterSettings.PropRulesFile
            SELF:ConvertTableFile	:= XPorterSettings.ConvertTableFile
            SELF:StatementsFile		:= XPorterSettings.StatementsFile
            SELF:VFPElementsFile	:= XPorterSettings.VFPElementsFile
            // Initialize caches
            SELF:_templateCache := Dictionary<STRING, STRING>{}
            SELF:_jsonCache := Dictionary<STRING, Dictionary<STRING, Dictionary<STRING,STRING>>>{}
            //
            SELF:Items := List<BaseItem>{ }
            SELF:CustomControls := Dictionary<STRING, SCXVCXItem>{}
            SELF:HasFormSet := FALSE
            SELF:InitElementsFormDesigner()
            SELF:ExportInOneFile := FALSE
            SELF:GeneratedFiles := List<GeneratedFile>{}
            //
            RETURN

        PROPERTY IsLibrary AS LOGIC GET String.Compare( System.IO.Path.GetExtension( SELF:Settings:ItemsPath ), ".vcx", TRUE ) == 0


        /// <summary>
        /// This will copy code to an External file, and replace the RawCode property with the FullPath of the External File
        // Used by the Serialize Method.
        /// </summary>
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

        /// <summary>
        /// For each Item, and for each Childs of these Item :
        ///  - Convert the VFP Type to a WinForms Type, based on ConvertTable rules (TypeConvert.json)
        ///  - Convert the VFP Events to WinForms Events, based on EventRules rules (EventRules.json)
        /// </summary>
        METHOD ProcessAttachedCode( entity AS SCXVCXEntity ) AS LOGIC
            // Load Convertion table
            LOCAL typeList AS Dictionary<STRING,STRING[]>
            typeList := DeserializeJSONSimpleArray( SELF:ConvertTable )
            LOCAL eventList AS Dictionary<STRING, Dictionary<STRING,STRING[]> >
            eventList := DeserializeJSONArray( SELF:EventRules )
            LOCAL sttmnts AS List<STRING>
            sttmnts := JsonConvert.DeserializeObject<List<STRING>>( SELF:Statements )
            LOCAL vfpElts AS Dictionary<STRING,STRING>
            vfpElts := JsonConvert.DeserializeObject<Dictionary<STRING,STRING>>( SELF:VFPElements )
            LOCAL colorProps AS List<STRING>
            colorProps := JsonConvert.DeserializeObject<List<STRING>>( File.ReadAllText(XPorterSettings.ColorPropertiesFile) )
            //
            LOCAL items AS List<BaseItem>
            items := List<BaseItem>{}
            IF entity:DataEnvironment != NULL
                items:Add( entity:DataEnvironment )
            ENDIF
            items:Add( entity:Item )
            // The TopLevel is the Form
            FOREACH item AS SCXVCXItem IN items
                //
                FOREACH subItem AS SCXVCXItem IN item:Childs
                    IF SELF:Canceled
                        EXIT
                    ENDIF
                    SELF:ConvertHandlerCode( subItem, typeList, eventList, sttmnts, vfpElts, colorProps, TRUE )
                    // Also process children of non-grid containers (e.g., PageFrame page controls)
                    IF String.Compare( subItem:BaseClassName, "grid", TRUE ) != 0 .AND. subItem:Childs:Count > 0
                        FOREACH grandSubItem AS SCXVCXItem IN subItem:Childs
                            IF SELF:Canceled
                                EXIT
                            ENDIF
                            SELF:ConvertHandlerCode( grandSubItem, typeList, eventList, sttmnts, vfpElts, colorProps, TRUE )
                        NEXT
                    ENDIF
                NEXT
                IF SELF:Canceled
                    EXIT
                ENDIF
                // Don't forget to process the Item itself
                SELF:ConvertHandlerCode( item, typeList, eventList, sttmnts, vfpElts, colorProps, FALSE )
            NEXT
            RETURN !SELF:Canceled

        /// <summary>
        /// Extract the code from the SCXVCXItem item.
        /// Then, create the converting rules.
        /// ANd using these rules, converts Events to their Windows Forms counterparts
        /// </summary>
        /// <param name="subItem"></param>
        /// <param name="typeList"></param>
        /// <param name="eventList"></param>
        /// <returns></returns>
        PROTECTED METHOD ConvertHandlerCode( subItem AS SCXVCXItem, typeList AS Dictionary<STRING,STRING[]>, eventList AS Dictionary<STRING, Dictionary<STRING,STRING[]>>, sttmnt AS List<STRING>, vfpElt AS Dictionary<STRING,STRING>, colorProps AS List<STRING>, isChild AS LOGIC ) AS VOID
            subItem:ConvertClassName( typeList )
            // Extract the code, and split it to Events
            subItem:XPortedCode := ItemCode{ subItem, isChild }
            // Set of Rules
            LOCAL evtRules AS Dictionary<STRING,STRING[]>
            // Use the Rendering ClassName in order to get the Events name
            evtRules := SELF:BuildEventRules( eventList, subItem:BaseClassName )
            // Apply Rules and Create EventHandlers
            subItem:ConvertEvents( evtRules, sttmnt, vfpElt, colorProps, SELF:Settings )
            //
            SELF:UpdateProgress()
            RETURN

        /// <summary>
        /// Extract the EventRules for a specific VFP Control, and add the Default ones
        /// </summary>
        PROTECTED METHOD BuildEventRules( objJson AS Dictionary<STRING, Dictionary<STRING,STRING[]> >, foxClassName AS STRING  ) AS Dictionary<STRING,STRING[]>
            LOCAL ctrlRules AS Dictionary<STRING,STRING[]>
            //
            IF objJson:ContainsKey( foxClassName )
                // Get the rules for this control Type
                ctrlRules := objJson[ foxClassName ]
                // And add the ones from Common if they does't exist
                LOCAL defRules AS Dictionary<STRING,STRING[]>
                IF objJson:ContainsKey( "Common" )
                    defRules := objJson[ "Common" ]
                    FOREACH conversion AS KeyValuePair<STRING, STRING[]> IN defRules
                        IF !ctrlRules:ContainsKey( conversion:Key )
                            ctrlRules:Add( conversion:Key, conversion:Value )
                        ENDIF
                    NEXT
                ENDIF
            ELSEIF objJson:ContainsKey( "Common" )
                ctrlRules := objJson[ "Common" ]
            ELSE
                ctrlRules := Dictionary<STRING,STRING[]>{}
            ENDIF
            RETURN ctrlRules

        /// <summary>
        /// Extract the PropRules (Property convertion) for a specific VFP Control, and add the Default ones
        /// </summary>
        PROTECTED METHOD BuildControlRules( objJson AS Dictionary<STRING, Dictionary<STRING,STRING> >, elementClassName AS STRING  ) AS Dictionary<STRING,STRING>
            LOCAL ctrlRules AS Dictionary<STRING,STRING>
            // Looking for a Control
            elementClassName := elementClassName:ToLower()
            IF !objJson:ContainsKey( elementClassName )
                // This is an unknown control...
                // It might be a CustomControl that is defined in a Library
                // We will try to climb the Inheritance tree to find a suitable control
                VAR searchFor := elementClassName
                VAR candidate := String.Empty
                DO WHILE SELF:CustomControls:ContainsKey( searchFor )
                    VAR item := SELF:CustomControls[ searchFor ]
                    searchFor := item:FullyQualifiedName
                    candidate := searchFor
                ENDDO
                IF !String.IsNullOrEmpty( candidate )
                    elementClassName := candidate
                ENDIF
            ENDIF
            IF objJson:ContainsKey( elementClassName )
                // Get his properties
                ctrlRules := objJson[ elementClassName]

                LOCAL defRules AS Dictionary<STRING,STRING>
                // and add the Common if any, except for "form"
                IF String.Compare(elementClassName,"form",TRUE)!=0 .AND. objJson:ContainsKey( "Common" )
                    defRules := objJson[ "Common" ]
                    FOREACH conversion AS KeyValuePair<STRING, STRING> IN defRules
                        IF !ctrlRules:ContainsKey( conversion:Key )
                            ctrlRules:Add( conversion:Key, conversion:Value )
                        ENDIF
                    NEXT
                ENDIF
            ELSEIF objJson:ContainsKey( "Common" )
                ctrlRules := objJson[ "Common" ]
            ELSE
                ctrlRules := Dictionary<STRING,STRING>{}
            ENDIF
            RETURN ctrlRules

        METHOD Initialize( filePath AS STRING, destPath AS STRING ) AS VOID
            //
            SELF:Settings:ItemsPath := filePath
            SELF:Settings:OutputPath := destPath
            //
            IF !Directory.Exists( SELF:Settings:OutputPath )
                // Let's try to create the destination Path
                Directory.CreateDirectory( SELF:Settings:OutputPath )
            ENDIF
            // Per default, the name of the generated File AND the name of the class is based on the name of the current File (.scx)
            //SELF:FormNameOverride := Path.GetFileNameWithoutExtension(SELF:Settings:ItemsPath )
            //
            SELF:InitExportContext()
            RETURN


        PROTECTED METHOD InitExportContext() AS VOID
            // Read the TypeConversion table, and create a Dictionary with it
            SELF:_typeList := DeserializeJSONSimpleArray( SELF:ConvertTable )
            // Load JSON definitions for conversions Rules
            SELF:_propertiesRules := DeserializeJSON( SELF:PropRules )
            IF SELF:_propertiesRules:ContainsKey( "Default" )
                SELF:_defaultValues := SELF:_propertiesRules[ "Default" ]
            ELSE
                SELF:_defaultValues := Dictionary<STRING,STRING>{}
            ENDIF
        END METHOD



        METHOD ExportAsWindowAndDesigner( entity AS SCXVCXEntity ) AS LOGIC
            LOCAL lOk AS LOGIC
            // Get the Window .prg filename
            LOCAL fileName AS STRING
            fileName := SELF:GetOutputFilename( entity:Item )
            // what are we writing
            SELF:GeneratedFiles:Add( GeneratedFile{ fileName,"Form"})
            // Where do we write
            LOCAL dest AS StreamWriter
            dest := StreamWriter{ fileName }
            // First Export the Window
            SELF:InitElementsFormDesigner()
            TRY
                lOk := TRUE
                SELF:ExportWindow( dest, entity:Item, entity:DataEnvironment )
            CATCH e AS Exception
                lOk := FALSE
                XPorterLogger.Instance:Error("ExportAsWindowAndDesigner: Failed to export window form")
                XPorterLogger.Instance:Error("Exception: " + e:Message)
            FINALLY
                dest:Close()
            END TRY
            //
            IF !SELF:Canceled .AND. lOk
                // Now, export Designer
                // What is the name of the Form.Designer.prg ?
                VAR destFile := Path.ChangeExtension( fileName, "designer.prg")
                SELF:GeneratedFiles:Add( GeneratedFile{ destFile,"", fileName })
                //
                dest := StreamWriter{ destFile }
                TRY
                    lOk := TRUE
                    SELF:ExportDesigner( dest, entity:Item, entity:DataEnvironment )
                CATCH e AS Exception
                    lOk := FALSE
                    XPorterLogger.Instance:Error("ExportAsWindowAndDesigner: Failed to export designer for form")
                    XPorterLogger.Instance:Error("Exception: " + e:Message)
                FINALLY
                    dest:Close()
                END TRY
            ENDIF
            RETURN !SELF:Canceled .AND. lOk

        METHOD ExportAsSingleFile( entity AS SCXVCXEntity ) AS LOGIC
            LOCAL lOk AS LOGIC
            // Get the Window .prg filename
            LOCAL fileName AS STRING
            fileName := SELF:GetOutputFilename( entity:Item )
            // what are we writing
            SELF:GeneratedFiles:Add( GeneratedFile{ fileName,"Form"})
            // Where do we write
            LOCAL dest AS StreamWriter
            dest := StreamWriter{ fileName }
            // First Export the Window
            SELF:InitElementsSingleForm( entity:Item:IsContainer )
            TRY
                lOk := TRUE
                SELF:ExportSingleFile( dest, entity:Item, entity:DataEnvironment )
            CATCH e AS Exception
                lOk := FALSE
                XPorterLogger.Instance:Error("ExportAsSingleFile: Failed to export single file form")
                XPorterLogger.Instance:Error("Exception: " + e:Message)
            FINALLY
                dest:Close()
            END TRY
            //
            RETURN !SELF:Canceled .AND. lOk


        /// <summary>
        /// Export the Window file : Constructor & Event Handlers
        /// </summary>
        PROTECTED METHOD ExportWindow( dest AS StreamWriter, orgItem AS SCXVCXItem, orgDataEnvItem AS SCXVCXItem ) AS VOID
            // Retrieve the Form Conversion rule
            LOCAL formRules AS Dictionary<STRING,STRING>
            // As we will change Properties/Events, working on Clone
            VAR oneItem := SCXVCXItem{ orgItem }
            LOCAL dataEnvItem AS SCXVCXItem
            IF orgDataEnvItem != NULL
                dataEnvItem := SCXVCXItem{ orgDataEnvItem }
            ELSE
                dataEnvItem := NULL
            ENDIF
            VAR declareDataEnv := StringBuilder{}
            VAR setDataEnv := StringBuilder{}
            // Create the code for a DataEnvironment Object, with the attached Cursors
            SELF:GenerateDataEnvironment( dataEnvItem, setDataEnv, declareDataEnv )
            //
            dest:Write( SELF:FormPrefix )
            IF !String.IsNullOrEmpty( SELF:NamespaceDefinition )
                dest.WriteLine("")
                dest.WriteLine( "BEGIN NAMESPACE " + SELF:NamespaceDefinition )
                dest.WriteLine("")
            ENDIF
            SELF:UpdateProgress()
            IF SELF:Canceled
                RETURN
            ENDIF
            // Form class definition
            VAR code := StringBuilder{SELF:FormStartType}
            oneItem:ConvertClassName( SELF:_typeList )
            // We can be here from a Library, and this item is maybe not a Form....
            formRules := SELF:BuildControlRules( SELF:_propertiesRules, oneItem:FullyQualifiedFoxClassName )
            oneItem:ConvertProperties( formRules, SELF:_defaultValues )
            //formProp := ""
            // Declaration of User-Defined Properties (If Any)
            VAR declaration := StringBuilder{}
            IF oneItem:UserDefItems != NULL
                FOREACH VAR userDefItem IN oneItem:UserDefItems
                    // Here, only Field and FieldArray
                    IF userDefItem:Kind != UserDefinition.ItemKind.Method
                        declaration:Append( userDefItem:Declaration )
                        declaration:Append(Environment.NewLine)
                    ENDIF
                NEXT

            ENDIF
            //FOREACH VAR prop IN item:Properties
            //	formProp += "SELF:" + prop:Key + " := " + prop:Value + Environment.NewLine
            //NEXT
//             IF String.IsNullOrEmpty( SELF:FormNameOverride )
            VAR formStartReplacements := Dictionary<STRING, STRING>{}
            formStartReplacements["formName"] := SELF:GetFormClassName( oneItem )
            formStartReplacements["superName"] := oneItem:FullyQualifiedName
            formStartReplacements["dataenvironment"] := declareDataEnv:ToString()
            formStartReplacements["childsDeclaration"] := declaration:ToString()
            code := StringBuilder{TemplateHelper.ReplaceAndValidate(SELF:FormStartType, "FormStartType", formStartReplacements)}
//             ELSE
//                 code:Replace( "<@formName@>", SELF:FormNameOverride )
//             ENDIF
            //
            dest:Write( code:ToString() )
            // Now, the Core-Code
            code := StringBuilder{SELF:FormInitType}
            //
            VAR windowDescendants := SELF:CollectContainerDescendants( oneItem )
            LOCAL handlers AS StringBuilder
            handlers := StringBuilder{}
            FOREACH VAR subItem IN oneItem:Childs
                SELF:UpdateProgress()
                IF SELF:Canceled
                    EXIT
                ENDIF
                // Add EventHandlers
                handlers:AppendLine( SELF:BuildEventHandlers( subItem ) )
            NEXT
            // Also add event handlers from container descendants
            FOREACH VAR pair IN windowDescendants
                handlers:AppendLine( SELF:BuildEventHandlers( pair:Item1 ) )
            NEXT
            IF SELF:Canceled
                RETURN
            ENDIF
            //
            VAR Grids := List<SCXVCXItem>{ }
            VAR containerHandlers := StringBuilder{}
            //
            FOREACH VAR subItem IN oneItem:Childs
                LOCAL scxSubItem AS SCXVCXItem
                //
                scxSubItem := (SCXVCXItem) subItem
                SELF:UpdateProgress()
                IF SELF:Canceled
                    EXIT
                ENDIF
                IF scxSubItem:XPortedCode != NULL
                    containerHandlers:Append( scxSubItem:CreateEventHandlers( TRUE, SELF:Settings ) )
                ENDIF
                // Grid ?
                IF String.Compare( scxSubItem:BaseClassName, "grid", TRUE ) == 0
                    Grids:Add( scxSubItem )
                ENDIF
            NEXT
            // Also collect containerHandlers and grids from descendants
            FOREACH VAR pair IN windowDescendants
                VAR scxDescItem := pair:Item1
                IF scxDescItem:XPortedCode != NULL
                    containerHandlers:Append( scxDescItem:CreateEventHandlers( TRUE, SELF:Settings ) )
                ENDIF
                IF String.Compare( scxDescItem:BaseClassName, "grid", TRUE ) == 0
                    Grids:Add( scxDescItem )
                ENDIF
            NEXT
            IF SELF:Canceled
                RETURN
            ENDIF
            // Any event for the Form ?
            handlers:AppendLine( SELF:BuildEventHandlers( oneItem ) )
            //
            VAR formInitReplacements := Dictionary<STRING, STRING>{}
            formInitReplacements["InitContainers"] := containerHandlers:ToString()
            VAR columnSettings := SELF:GenerateGrids( Grids )
            formInitReplacements["InitGrids"] := columnSettings:ToString()
            formInitReplacements["setdataenvironment"] := IIF(dataEnvItem != NULL, setDataEnv:ToString(), "")
            formInitReplacements["DoBindings"] := IIF(dataEnvItem != NULL, "DoBindings()", "")
            IF oneItem:UserDefItems != NULL
                VAR userdefProp := StringBuilder{}
                userdefProp:Append(oneItem:ApplyPropertiesRules( FALSE, FALSE, 2 ))
                formInitReplacements["userdefProps"] := userdefProp:ToString()
            ELSE
                formInitReplacements["userdefProps"] := ""
            ENDIF
            formInitReplacements["EventHandlers"] := handlers:ToString()
            code := StringBuilder{TemplateHelper.ReplaceAndValidate(SELF:FormInitType, "FormInitType", formInitReplacements)}
            dest:Write( code:ToString() )
            //
            dest:Write( SELF:FormEndType )
            IF !String.IsNullOrEmpty( SELF:NamespaceDefinition )
                dest.WriteLine( "END NAMESPACE " )
            ENDIF
            //
            RETURN



        /// <summary>
        /// Export the Window Designer file : Control declarations, InitComponent()
        /// </summary>
        PROTECTED METHOD ExportDesigner( dest AS StreamWriter, orgItem AS SCXVCXItem, orgDataEnvItem AS SCXVCXItem ) AS VOID
            // We will look Controls conversion Rules when enumerating
            LOCAL ctrlRules AS Dictionary<STRING,STRING>
            // As we will change Properties/Events, working on Clone
            VAR oneItem := SCXVCXItem{ orgItem }
//             LOCAL dataEnvItem AS SCXVCXItem
//             IF orgDataEnvItem != NULL
//                 dataEnvItem := SCXVCXItem{ orgDataEnvItem }
//             ENDIF
//             //
            // Build DBC field property cache from the original (unmodified) DataEnvironment
            VAR dbcCache := DbcFieldCache{Path.GetDirectoryName(SELF:Settings:ItemsPath), orgDataEnvItem}
            dest:Write( SELF:DesignerPrefix )
            IF !String.IsNullOrEmpty( SELF:NamespaceDefinition )
                dest.WriteLine("")
                dest.WriteLine( "BEGIN NAMESPACE " + SELF:NamespaceDefinition )
                dest.WriteLine("")
            ENDIF
             LOCAL code AS StringBuilder
             VAR formProp := StringBuilder{}
             //
             SELF:UpdateProgress()
             // Form class definition
             code := StringBuilder{ SELF:DesignerStartType }
             oneItem:ConvertClassName( SELF:_typeList )
             // We can be here from a Library, and this item is maybe not a Form....
             // Retrieve the Form Conversion rule
             VAR formRules := SELF:BuildControlRules( SELF:_propertiesRules, oneItem:FullyQualifiedFoxClassName ) //:FoxClassName )
             oneItem:ConvertProperties( formRules, SELF:_defaultValues )
             formProp:Append(oneItem:ApplyPropertiesRules( FALSE, FALSE, 1 ))

             // Build replacements for DesignerStartType
             VAR designerStartReplacements := Dictionary<STRING, STRING>{}
             designerStartReplacements["formName"] := SELF:GetFormClassName( oneItem )
             designerStartReplacements["superName"] := oneItem:FullyQualifiedName
             // Declaration of Childrens (Sub-Controls)
             VAR declaration := StringBuilder{}
             FOREACH VAR subItem IN oneItem:Childs
                 LOCAL scxSubItem AS SCXVCXItem
                 scxSubItem := (SCXVCXItem) subItem
                 scxSubItem:ConvertClassName( SELF:_typeList )
                 declaration:Append( SELF:Settings:Modifier )
                 declaration:Append(" ")
                 declaration:Append(subItem:Name)
                 declaration:Append(" AS ")
                 // NameSpace addition ??
                 declaration:Append(subItem:FullyQualifiedName)
                 declaration:Append(Environment.NewLine)
             NEXT
             // Also declare descendants of non-grid containers (e.g., controls inside PageFrame pages)
             VAR designerDescendants := SELF:CollectContainerDescendants( oneItem )
             FOREACH VAR pair IN designerDescendants
                 VAR scxDescItem := pair:Item1
                 scxDescItem:ConvertClassName( SELF:_typeList )
                 declaration:Append( SELF:Settings:Modifier )
                 declaration:Append(" ")
                 declaration:Append(scxDescItem:Name)
                 declaration:Append(" AS ")
                 declaration:Append(scxDescItem:FullyQualifiedName)
                 declaration:Append(Environment.NewLine)
             NEXT
             designerStartReplacements["childsDeclaration"] := declaration:ToString()
             code := StringBuilder{TemplateHelper.ReplaceAndValidate(SELF:DesignerStartType, "DesignerStartType", designerStartReplacements)}
             dest:Write( code:ToString() )
             // Now, Instantiation of these Childrens
             VAR controlStack := Stack<STRING>{}
             VAR instantiate := StringBuilder{}
             VAR initChilds := StringBuilder{}
             code := StringBuilder{ SELF:DesignerInitType }
             FOREACH VAR subItem IN oneItem:Childs
                 LOCAL scxSubItem AS SCXVCXItem
                 scxSubItem := (SCXVCXItem) subItem
                 SELF:UpdateProgress()
                 //
                 instantiate:Append("SELF:")
                 instantiate:Append(scxSubItem:Name)
                 instantiate:Append(" := ")
                 instantiate:Append(scxSubItem:FullyQualifiedName)
                 instantiate:Append("{}")
                 instantiate:Append(Environment.NewLine)

                 // Set of Rules
                 VAR memberFactoryDes := SELF:BuildMemberFactory( scxSubItem )
                 ctrlRules := SELF:BuildControlRules( SELF:_propertiesRules, scxSubItem:FullyQualifiedFoxClassName )
                 // Inject DBC-derived properties (Caption/InputMask/Format) not already in SCX
                 dbcCache:InjectMissingProperties(scxSubItem)
                 // Apply Rules to Properties
                 scxSubItem:ConvertProperties( ctrlRules, SELF:_defaultValues, TRUE )
                 IF !String.IsNullOrEmpty(memberFactoryDes)
                     initChilds:Append(memberFactoryDes)
                 ENDIF
                 initChilds:Append(scxSubItem:ApplyPropertiesRules( TRUE ))
                 // Add EventHandlers
                 IF scxSubItem:XPortedCode != NULL
                     initChilds:Append( scxSubItem:CreateEventHandlers( FALSE, SELF:Settings  ) )
                 ENDIF
                 initChilds:Append("//")
                 initChilds:Append(Environment.NewLine)

                 IF scxSubItem:AddToControls
                     controlStack:Push(scxSubItem:Name)
                 ENDIF
             NEXT
             // Also instantiate and init descendants of non-grid containers
             FOREACH VAR pair IN designerDescendants
                 VAR scxDescItem := pair:Item1
                 VAR descContainer := pair:Item2
                 instantiate:Append("SELF:")
                 instantiate:Append(scxDescItem:Name)
                 instantiate:Append(" := ")
                 instantiate:Append(scxDescItem:FullyQualifiedName)
                 instantiate:Append("{}")
                 instantiate:Append(Environment.NewLine)
                 ctrlRules := SELF:BuildControlRules( SELF:_propertiesRules, scxDescItem:FullyQualifiedFoxClassName )
                 // Inject DBC-derived properties (Caption/InputMask/Format) not already in SCX
                 dbcCache:InjectMissingProperties(scxDescItem)
                 scxDescItem:ConvertProperties( ctrlRules, SELF:_defaultValues, TRUE )
                 initChilds:Append( scxDescItem:ApplyPropertiesRules( TRUE ) )
                 IF scxDescItem:XPortedCode != NULL
                     initChilds:Append( scxDescItem:CreateEventHandlers( FALSE, SELF:Settings ) )
                 ENDIF
                 initChilds:Append("//")
                 initChilds:Append(Environment.NewLine)
             NEXT
             // Controls must be added in reverse order
             VAR addCtrl := StringBuilder{}
             WHILE controlStack:Count > 0
                 addCtrl:Append("SELF:Controls:Add(SELF:")
                 addCtrl:Append(controlStack:Pop())
                 addCtrl:Append(")")
                 addCtrl:Append(Environment.NewLine)
             ENDDO
             // Add page-level descendants to their virtual containers
             FOREACH VAR pair IN designerDescendants
                 addCtrl:Append( SELF:GetPageAddStatement( pair:Item1, pair:Item2 ) )
                 addCtrl:Append(Environment.NewLine)
             NEXT
             // Build replacements for DesignerInitType
             VAR designerInitReplacements := Dictionary<STRING, STRING>{}
             designerInitReplacements["childsInstantiate"] := instantiate:ToString()
             designerInitReplacements["childsInitialize"] := initChilds:ToString()
             designerInitReplacements["addChildsToParent"] := addCtrl:ToString()
             // Add EventHandlers
             IF oneItem:XPortedCode != NULL
                 formProp:Append( oneItem:CreateEventHandlers( FALSE, SELF:Settings  ) )
             ENDIF
             designerInitReplacements["formProps"] := formProp:ToString()
             code := StringBuilder{TemplateHelper.ReplaceAndValidate(SELF:DesignerInitType, "DesignerInitType", designerInitReplacements)}
             dest:Write( code:ToString() )
            //
            dest:Write( SELF:DesignerEndType )
            //
            IF !String.IsNullOrEmpty( SELF:NamespaceDefinition )
                dest.WriteLine( "END NAMESPACE " )
            ENDIF
            RETURN




        /// <summary>
        /// Export everything in a single Window file : Constructor & Event Handlers, Control declarations, InitComponent()
        /// </summary>
        PROTECTED METHOD ExportSingleFile( dest AS StreamWriter, orgItem AS SCXVCXItem, orgDataEnvItem AS SCXVCXItem ) AS VOID
            // Retrieve the Form Conversion rule
            LOCAL formRules AS Dictionary<STRING,STRING>
            // We will look Controls conversion Rules when enumerating
            LOCAL ctrlRules AS Dictionary<STRING,STRING>
            // As we will change Properties/Events, working on Clone
            VAR oneItem := SCXVCXItem{ orgItem }
            LOCAL dataEnvItem AS SCXVCXItem
            IF orgDataEnvItem != NULL
                dataEnvItem := SCXVCXItem{ orgDataEnvItem }
            ELSE
                dataEnvItem := NULL
            ENDIF
            // Build DBC field property cache from the original (unmodified) DataEnvironment
            VAR dbcCache := DbcFieldCache{Path.GetDirectoryName(SELF:Settings:ItemsPath), orgDataEnvItem}
            VAR declareDataEnv := StringBuilder{}
            VAR setDataEnv := StringBuilder{}
            // Create the code for a DataEnvironment Object, with the attached Cursors
            SELF:GenerateDataEnvironment( dataEnvItem, setDataEnv, declareDataEnv )
            //
            dest:Write( SELF:FormPrefix )
            IF !String.IsNullOrEmpty( SELF:NamespaceDefinition )
                dest.WriteLine("")
                dest.WriteLine( "BEGIN NAMESPACE " + SELF:NamespaceDefinition )
                dest.WriteLine("")
            ENDIF
            //
            LOCAL code AS StringBuilder
            VAR formProp := StringBuilder{}
            VAR userdefProp := StringBuilder{}
            LOCAL needBinding := FALSE AS LOGIC
            IF oneItem:PropertiesDict:ContainsKey("ControlSource")
                needBinding := TRUE
            ENDIF
            SELF:UpdateProgress()
            IF SELF:Canceled
                RETURN
            ENDIF
            //
            // Form class definition
            code := StringBuilder{SELF:FormStartType}
            oneItem:ConvertClassName( SELF:_typeList )
            // We can be here from a Library, and this item is maybe not a Form....
            formRules := SELF:BuildControlRules( SELF:_propertiesRules, oneItem:FullyQualifiedFoxClassName )
            // Check if, in the Form Properties, we could ahve some Childs intialization
            // If so, it comes from inheritance..... ( Custom Controls in Library )
            VAR propertiesFromInheritedControls := SELF:CheckInheritedProperties( oneItem )
            // The previous call has removed the Properties from InheritedControls, so here we only have the new ones in the Form
            oneItem:ConvertProperties( formRules, SELF:_defaultValues )
            formProp:Append(oneItem:ApplyPropertiesRules( FALSE, FALSE, 1 ) )
            userdefProp:Append(oneItem:ApplyPropertiesRules( FALSE, FALSE, 2 ) )
            // Do we have some inherited Controls ?
            LOCAL processingItem := NULL AS SCXVCXItem
            FOREACH VAR propInheritedCtrl IN propertiesFromInheritedControls
                // Each tuple pairs a dotted property (e.g. "Text1.InputMask") with the SCXVCXItem of the child it belongs to.
                VAR scxSubItem := SCXVCXItem{ propInheritedCtrl:Item2 }
                IF processingItem != NULL
                    IF ( processingItem:Name != scxSubItem:Name )
                        // Flush the completed batch for the previous child
                        ctrlRules := SELF:BuildControlRules( SELF:_propertiesRules, processingItem:FullyQualifiedFoxClassName )
                        processingItem:ConvertProperties( ctrlRules, SELF:_defaultValues, TRUE )
                        FOREACH VAR prop IN processingItem:PropertiesDict
                            IF !prop:Key:StartsWith("_")
                                formProp:Append("SELF:")
                                formProp:Append(processingItem:Name)
                                formProp:Append(":")
                                formProp:Append(prop:Key)
                                formProp:Append(" := ")
                                formProp:Append(prop:Value)
                                formProp:Append(Environment.NewLine)
                            ENDIF
                        NEXT
                        processingItem := scxSubItem
                        processingItem:PropertiesDict:Clear()
                        // Fall through to add the first property of the new child (shared with ELSE branch below)
                    ELSE
                        // Same child — add the property and move on
                        VAR propItem := propInheritedCtrl:Item1
                        VAR propName := propItem:Key
                        VAR dotPos := propName:IndexOf('.')
                        propName := propName:Substring( dotPos+1 )
                        processingItem:PropertiesDict:Add( propName, propItem:Value )
                        LOOP  // skip the add-first-property block below
                    ENDIF
                ELSE
                    processingItem := scxSubItem
                    processingItem:PropertiesDict:Clear()
                ENDIF
                // Add the first property of a newly started child batch.
                // This runs when processingItem was just set (new child or very first child).
                VAR firstPropItem := propInheritedCtrl:Item1
                VAR firstPropName := firstPropItem:Key
                VAR firstDotPos   := firstPropName:IndexOf('.')
                firstPropName := firstPropName:Substring( firstDotPos+1 )
                processingItem:PropertiesDict:Add( firstPropName, firstPropItem:Value )
            NEXT
            IF processingItem != NULL
                // Set of Rules
                ctrlRules := SELF:BuildControlRules( SELF:_propertiesRules, processingItem:FullyQualifiedFoxClassName )
                // Apply Rules to Properties
                processingItem:ConvertProperties( ctrlRules, SELF:_defaultValues, TRUE )
                // There should be only one Property in the Dict here...
                FOREACH VAR prop IN processingItem:PropertiesDict
                    IF !prop:Key:StartsWith("_")
                        formProp:Append("SELF:")
                        formProp:Append(processingItem:Name)
                        formProp:Append(":")
                        formProp:Append(prop:Key)
                        formProp:Append(" := ")
                        formProp:Append(prop:Value)
                        formProp:Append(Environment.NewLine)
                    ENDIF
             NEXT
             ENDIF
             //
             // Build replacements for FormStartType
             VAR singleFileStartReplacements := Dictionary<STRING, STRING>{}
             singleFileStartReplacements["formName"] := SELF:GetFormClassName( oneItem )
             singleFileStartReplacements["superName"] := oneItem:ClassName
             // Declaration of Childrens (Sub-Controls)
             VAR declaration := StringBuilder{}
             FOREACH VAR subItem IN oneItem:Childs
                 LOCAL scxSubItem AS SCXVCXItem
                 scxSubItem := (SCXVCXItem) subItem
                 scxSubItem:ConvertClassName( SELF:_typeList )
                 declaration:Append( SELF:Settings:Modifier )
                 declaration:Append(" ")
                 declaration:Append(scxSubItem:Name)
                 declaration:Append(" AS ")
                 declaration:Append(scxSubItem:FullyQualifiedName)
                 declaration:Append(Environment.NewLine)
             NEXT
             // Also declare descendants of non-grid containers (e.g., controls inside PageFrame pages)
             VAR descendants := SELF:CollectContainerDescendants( oneItem )
             FOREACH VAR pair IN descendants
                 VAR scxDescItem := pair:Item1
                 scxDescItem:ConvertClassName( SELF:_typeList )
                 declaration:Append( SELF:Settings:Modifier )
                 declaration:Append(" ")
                 declaration:Append(scxDescItem:Name)
                 declaration:Append(" AS ")
                 declaration:Append(scxDescItem:FullyQualifiedName)
                 declaration:Append(Environment.NewLine)
             NEXT
             // Declaration of User-Defined Properties (If Any)
             IF oneItem:UserDefItems != NULL
                 FOREACH VAR userDefItem IN oneItem:UserDefItems
                     // Here, only Field and FieldArray
                     IF userDefItem:Kind != UserDefinition.ItemKind.Method
                         declaration:Append( userDefItem:Declaration )
                         declaration:Append(Environment.NewLine)
                     ENDIF
                 NEXT
             ENDIF
             singleFileStartReplacements["dataenvironment"] := declareDataEnv:ToString()
             singleFileStartReplacements["childsDeclaration"] := declaration:ToString()
             code := StringBuilder{TemplateHelper.ReplaceAndValidate(SELF:FormStartType, "FormStartType", singleFileStartReplacements)}
             dest:Write( code:ToString() )
            // Now, Instantiation of these Childrens
            VAR containerHandlers := StringBuilder{}
            VAR Grids := List<SCXVCXItem>{ }
            VAR instantiate := StringBuilder{}
            VAR initChilds := StringBuilder{}
            VAR addCtrl := StringBuilder{}
            VAR controlStack := Stack<STRING>{}
            code := StringBuilder{ SELF:FormInitType }
            FOREACH VAR subItem IN oneItem:Childs
                LOCAL scxSubItem AS SCXVCXItem
                //
                scxSubItem := (SCXVCXItem) subItem
                // Check for some Extra source to inject
                IF scxSubItem:PropertiesDict:ContainsKey("ControlSource")
                    needBinding := TRUE
                ENDIF
                SELF:UpdateProgress()
                IF SELF:Canceled
                    EXIT
                ENDIF
                //
                instantiate:Append("SELF:")
                instantiate:Append(scxSubItem:Name)
                instantiate:Append(" := ")
                instantiate:Append(scxSubItem:FullyQualifiedName)
                instantiate:Append("{}")
                instantiate:Append(Environment.NewLine)

                // Set of Rules
                VAR memberFactorySF := SELF:BuildMemberFactory( scxSubItem )
                ctrlRules := SELF:BuildControlRules( SELF:_propertiesRules, scxSubItem:FullyQualifiedFoxClassName )
                // Inject DBC-derived properties (Caption/InputMask/Format) not already in SCX
                dbcCache:InjectMissingProperties(scxSubItem)
                // Apply Rules to Properties
                scxSubItem:ConvertProperties( ctrlRules, SELF:_defaultValues, TRUE )
                IF !String.IsNullOrEmpty(memberFactorySF)
                    initChilds:Append(memberFactorySF)
                ENDIF
                initChilds:Append( scxSubItem:ApplyPropertiesRules( TRUE ) )
                // Add EventHandlers
                IF scxSubItem:XPortedCode != NULL
                    initChilds:Append( scxSubItem:CreateEventHandlers( FALSE, SELF:Settings  ) )
                    containerHandlers:Append( scxSubItem:CreateEventHandlers( TRUE, SELF:Settings  ) )
                ENDIF
                // Grid ?
                IF String.Compare( scxSubItem:BaseClassName, "grid", TRUE ) == 0
                    Grids:Add( scxSubItem )
                ENDIF
                initChilds:Append("//")
                initChilds:Append(Environment.NewLine)
                // In order to Add Controls to Forms, or Forms to Formset
                IF scxSubItem:AddToControls .OR. oneItem:IsFormSet
                    controlStack:Push(scxSubItem:Name)
                ENDIF
                // Controls must be added in reverse order
                WHILE controlStack:Count > 0
                    addCtrl:Append("SELF:Controls:Add(SELF:")
                    addCtrl:Append(controlStack:Pop())
                    addCtrl:Append(")")
                    addCtrl:Append(Environment.NewLine)
                ENDDO
                //
                IF SELF:Canceled
                    RETURN
                ENDIF
                //
             NEXT
             // Also process descendants of non-grid containers (e.g., controls in PageFrame pages)
             FOREACH VAR pair IN descendants
                 VAR scxDescItem := pair:Item1
                 VAR container := pair:Item2
                 IF SELF:Canceled
                     EXIT
                 ENDIF
                 instantiate:Append("SELF:")
                 instantiate:Append(scxDescItem:Name)
                 instantiate:Append(" := ")
                 instantiate:Append(scxDescItem:FullyQualifiedName)
                 instantiate:Append("{}")
                 instantiate:Append(Environment.NewLine)
                 ctrlRules := SELF:BuildControlRules( SELF:_propertiesRules, scxDescItem:FullyQualifiedFoxClassName )
                 // Inject DBC-derived properties (Caption/InputMask/Format) not already in SCX
                 dbcCache:InjectMissingProperties(scxDescItem)
                 scxDescItem:ConvertProperties( ctrlRules, SELF:_defaultValues, TRUE )
                 initChilds:Append( scxDescItem:ApplyPropertiesRules( TRUE ) )
                 IF scxDescItem:XPortedCode != NULL
                     initChilds:Append( scxDescItem:CreateEventHandlers( FALSE, SELF:Settings ) )
                     containerHandlers:Append( scxDescItem:CreateEventHandlers( TRUE, SELF:Settings ) )
                 ENDIF
                 initChilds:Append("//")
                 initChilds:Append(Environment.NewLine)
                 addCtrl:Append( SELF:GetPageAddStatement( scxDescItem, container ) )
                 addCtrl:Append(Environment.NewLine)
                 IF String.Compare( scxDescItem:BaseClassName, "grid", TRUE ) == 0
                     Grids:Add( scxDescItem )
                 ENDIF
             NEXT
             IF oneItem:XPortedCode != NULL
                 formProp:Append( oneItem:CreateEventHandlers( FALSE , SELF:Settings ) )
             ENDIF
             // Build replacements for FormInitType
             VAR singleFileInitReplacements := Dictionary<STRING, STRING>{}
             singleFileInitReplacements["childsInstantiate"] := instantiate:ToString()
             singleFileInitReplacements["childsInitialize"] := initChilds:ToString()
             singleFileInitReplacements["addChildsToParent"] := addCtrl:ToString()
             singleFileInitReplacements["formProps"] := formProp:ToString()
             singleFileInitReplacements["userdefProps"] := userdefProp:ToString()
             singleFileInitReplacements["setdataenvironment"] := IIF(dataEnvItem != NULL, setDataEnv:ToString(), "")
             singleFileInitReplacements["DoBindings"] := IIF(dataEnvItem != NULL, "DoBindings()", "")

             // Add EventHandlers
             VAR handlers := StringBuilder{}
             FOREACH VAR subItem IN oneItem:Childs
                 //
                 handlers:AppendLine( SELF:BuildEventHandlers( subItem ) )
             NEXT
             // Also collect event handlers from container descendants
             FOREACH VAR pair IN descendants
                 handlers:AppendLine( SELF:BuildEventHandlers( pair:Item1 ) )
             NEXT
             // Any event for the Form ?
             handlers:AppendLine( SELF:BuildEventHandlers( oneItem ) )
             //
             singleFileInitReplacements["InitContainers"] := containerHandlers:ToString()
             // Now, check the Grids
             VAR columnSettings := SELF:GenerateGrids( Grids )
             singleFileInitReplacements["InitGrids"] := columnSettings:ToString()
             singleFileInitReplacements["EventHandlers"] := handlers:ToString()
             code := StringBuilder{TemplateHelper.ReplaceAndValidate(SELF:FormInitType, "FormInitType", singleFileInitReplacements)}
             dest:Write( code:ToString() )
             // Do we need to push some Extra Code ?
             IF needBinding
                 dest:Write( SELF:BindingCode )
             ENDIF
             // Now, push the Closing definition
             dest:Write( SELF:FormEndType )
             IF !String.IsNullOrEmpty( SELF:NamespaceDefinition )
                 dest.WriteLine( "END NAMESPACE " )
             ENDIF
             //
             RETURN
         END METHOD


        /// <summary>
        /// Returns the X# class name for a form/control entity.
        /// - VCX: use the OBJNAME (each entry is an explicitly named class definition).
        /// - SCX, simple form: use the SCX filename — that is the DO FORM identity in VFP.
        /// - SCX, FormSet sub-form: use scxName_formOBJNAME to make each sub-form unique.
        /// </summary>
        PRIVATE METHOD GetFormClassName( item AS SCXVCXItem ) AS STRING
            IF SELF:IsLibrary
                RETURN item:Name
            ENDIF
            VAR scxName := Path.GetFileNameWithoutExtension( SELF:Settings:ItemsPath ):Replace(" ", "_")
            IF item:IsForm .AND. !String.IsNullOrEmpty( item:Parent )
                RETURN scxName + "_" + item:Name
            ENDIF
            RETURN scxName
        END METHOD

        PROTECTED METHOD GetOutputFilename( item AS BaseItem ) AS STRING
            LOCAL outFileName AS STRING
            IF item != NULL
                VAR className := SELF:GetFormClassName( (SCXVCXItem)item )
                // For VCX items, PrefixClassFile optionally groups files under the VCX name.
                // For SCX items the class name already embeds the SCX filename, so no extra prefix.
                IF SELF:IsLibrary .AND. SELF:Settings:PrefixClassFile
                    outFileName := Path.GetFileNameWithoutExtension( SELF:Settings:ItemsPath ):Replace(" ", "_") + "_" + className
                ELSE
                    outFileName := className
                ENDIF
            ELSE
                outFileName := Path.GetFileNameWithoutExtension( SELF:Settings:ItemsPath ):Replace(" ", "_")
            ENDIF
            RETURN Path.Combine(SELF:Settings:OutputPath, outFileName ) + ".prg"
        END METHOD

        /// <summary>
        /// Generate METHOD code for each EventHandler AND UserDef code
        /// </summary>
        /// <param name="subItem"></param>
        /// <returns></returns>
        PROTECTED METHOD BuildEventHandlers( subItem AS BaseItem ) AS STRING
            VAR handlers := StringBuilder{}
            VAR thisObjectDecl := StringBuilder{}
            IF subItem:XPortedCode != NULL
                FOREACH cdeBlock AS EventCode IN subItem:XPortedCode:Events
                    //
                    IF !String.IsNullOrEmpty(cdeBlock:Help)
                        handlers:AppendLine("/// <summary>")
                        handlers:Append("/// ")
                        handlers:AppendLine( cdeBlock:Help )
                        handlers:AppendLine("/// </summary>")
                    ELSE
                        handlers:AppendLine("//" )
                    ENDIF
                    handlers:Append("METHOD ")
                    // No Prototype ? Or this is a UserDef Method... Use the defined UDF Name
                    IF String.IsNullOrEmpty( cdeBlock:EventHandlerPrototype ) .OR. ( !SELF:Settings:NameUDF .AND. cdeBlock:IsUserDef )
                        handlers:Append( cdeBlock:Name )
                        handlers:Append( "()" )
                    ELSE
                        handlers:Append(cdeBlock:EventHandlerPrototype)
                    ENDIF
                    handlers:Append(Environment.NewLine)
                    // Declare a "thisObject" var that will contain the current object in context
                    thisObjectDecl:Clear()
                    thisObjectDecl:AppendLine("* VFPXPorter thisObject definition")
                    thisObjectDecl:Append("* IsChild:")
                    thisObjectDecl:Append( cdeBlock:Owner:IsChild )
                    thisObjectDecl:Append(" Owner:")
                    thisObjectDecl:Append( cdeBlock:Owner:Name )
                    thisObjectDecl:Append(" Block:")
                    thisObjectDecl:Append( cdeBlock:Definition )
                    thisObjectDecl:AppendLine()
                    thisObjectDecl:Append("VAR thisObject := ")
                    IF cdeBlock:Owner:IsChild
                        VAR subPos := cdeBlock:Definition:LastIndexOf('.')
                        IF subPos != -1
                            // We may have a control inside a container, remove the last part that should be the Event
                            thisObjectDecl:Append( cdeBlock:Owner:Name )
                            thisObjectDecl:Append( ":" )
                            thisObjectDecl:Append( cdeBlock:Definition:Substring( 0, subPos ) )
                        ELSE
                            thisObjectDecl:Append( cdeBlock:Owner:Name )
                        ENDIF
                    ELSE
                        thisObjectDecl:Append( "this" )
                    ENDIF
                    thisObjectDecl:Append(Environment.NewLine)
                    //					thisObjectDecl:Append("LOCAL thisForm := " )
                    //					IF cdeBlock:Owner:IsChild
                    //						thisObjectDecl:Append( "this.FindForm()" )
                    //					ELSE
                    //						thisObjectDecl:Append( "this" )
                    //					ENDIF
                    //					thisObjectDecl:Append(" AS OBJECT" )
                    //					thisObjectDecl:Append(Environment.NewLine)
                    // Get the first line of SourceCode
                    LOCAL line AS STRING
                    IF cdeBlock:Source:Count > 0
                        line := cdeBlock:Source[0]
                        IF line:TrimStart():ToUpper():StartsWith("LPARAMETERS") .OR. line:TrimStart():ToUpper():StartsWith("PARAMETERS")
                            handlers:AppendLine( line )
                            handlers:AppendLine( thisObjectDecl:ToString() )
                        ELSE
                            handlers:AppendLine( thisObjectDecl:ToString() )
                            handlers:AppendLine( line )
                        ENDIF
                    ENDIF
                    //
                    FOR VAR i:= 1 TO cdeBlock:Source:Count-1
                        line := cdeBlock:Source[i]
                        IF ( i == cdeBlock:Source:Count-1 )
                            IF ( line == "ENDPROC" )
                                line := "END METHOD"
                            ENDIF
                        ENDIF
                        //
                        handlers:AppendLine(line)
                    NEXT
                    handlers:AppendLine()
                NEXT
            ENDIF
            RETURN handlers:ToString()

        /// <summary>
        /// Create the code for a DataEnvironment Object, with the attached Cursors
        /// </summary>
        /// <param name="dataEnvItem"></param>
        /// <returns></returns>
        PROTECTED METHOD GenerateDataEnvironment( dataEnvItem AS SCXVCXItem, setDataEnv AS StringBuilder, declareDataEnv AS StringBuilder  ) AS VOID
            LOCAL dataRules AS Dictionary<STRING,STRING>
            // Collect unique DBC paths referenced by cursors (insertion-ordered via List)
            VAR dbcPaths := List<STRING>{}
            //
            IF dataEnvItem != NULL
                // For the DataEnvironment (one only!?)
                // Use the VFPDataEnvironment class -> Look a the TypeConvert rules
                dataEnvItem:ConvertClassName( SELF:_typeList )
                //
                setDataEnv:Append("SELF:")
                setDataEnv:Append(dataEnvItem:Name)
                setDataEnv:Append(" := ")
                setDataEnv:Append(dataEnvItem:ClassName)
                setDataEnv:Append("{}")
                setDataEnv:Append(Environment.NewLine)
                // Set of Rules
                dataRules := SELF:BuildControlRules( SELF:_propertiesRules, dataEnvItem:FoxClassName )
                // Apply Rules to Properties
                dataEnvItem:ConvertProperties( dataRules, SELF:_defaultValues )
                setDataEnv:Append( dataEnvItem:ApplyPropertiesRules( TRUE ) )
                // Pass 1 — Cursors
                FOREACH VAR dataCursor IN dataEnvItem:Childs
                    LOCAL cursorItem AS SCXVCXItem
                    cursorItem := (SCXVCXItem) dataCursor
                    IF String.Compare( cursorItem:BaseClassName, "cursor", TRUE ) != 0
                        LOOP
                    ENDIF
                    cursorItem:ConvertClassName( SELF:_typeList )
                    declareDataEnv:Append( SELF:Settings:Modifier )
                    declareDataEnv:Append(" ")
                    declareDataEnv:Append(cursorItem:Name)
                    declareDataEnv:Append(" AS ")
                    declareDataEnv:Append(cursorItem:ClassName)
                    declareDataEnv:Append(Environment.NewLine)
                    //
                    setDataEnv:Append("SELF:")
                    setDataEnv:Append(cursorItem:Name)
                    setDataEnv:Append(" := ")
                    setDataEnv:Append(cursorItem:ClassName)
                    setDataEnv:Append("{}")
                    setDataEnv:Append(Environment.NewLine)
                    // Collect DBC path before ConvertProperties strips/transforms it
                    LOCAL dbRaw AS STRING
                    IF cursorItem:PropertiesDict:TryGetValue("Database", OUT dbRaw)
                        dbRaw := dbRaw:Trim()
                        IF dbRaw:Length >= 2 .AND. dbRaw[0] == '"' .AND. dbRaw[dbRaw:Length-1] == '"'
                            dbRaw := dbRaw:Substring(1, dbRaw:Length-2)
                        ENDIF
                        IF !String.IsNullOrEmpty(dbRaw) .AND. !dbcPaths:Contains(dbRaw)
                            dbcPaths:Add(dbRaw)
                        ENDIF
                    ENDIF
                    // Set of Rules
                    dataRules := SELF:BuildControlRules( SELF:_propertiesRules, cursorItem:FoxClassName )
                    // Apply Rules to Properties
                    cursorItem:ConvertProperties( dataRules, SELF:_defaultValues )
                    setDataEnv:Append( cursorItem:ApplyPropertiesRules( TRUE ) )
                    setDataEnv:Append("SELF:")
                    setDataEnv:Append(dataEnvItem:Name)
                    setDataEnv:Append(":Cursors:Add( ")
                    setDataEnv:Append(cursorItem:Name)
                    setDataEnv:Append(" )")
                    setDataEnv:Append(Environment.NewLine)
                NEXT
                // Pass 2 — Relations
                FOREACH VAR dataRel IN dataEnvItem:Childs
                    LOCAL relItem AS SCXVCXItem
                    relItem := (SCXVCXItem) dataRel
                    IF String.Compare( relItem:BaseClassName, "relation", TRUE ) != 0
                        LOOP
                    ENDIF
                    relItem:ConvertClassName( SELF:_typeList )
                    declareDataEnv:Append( SELF:Settings:Modifier )
                    declareDataEnv:Append(" ")
                    declareDataEnv:Append(relItem:Name)
                    declareDataEnv:Append(" AS ")
                    declareDataEnv:Append(relItem:ClassName)
                    declareDataEnv:Append(Environment.NewLine)
                    //
                    setDataEnv:Append("SELF:")
                    setDataEnv:Append(relItem:Name)
                    setDataEnv:Append(" := ")
                    setDataEnv:Append(relItem:ClassName)
                    setDataEnv:Append("{}")
                    setDataEnv:Append(Environment.NewLine)
                    // Set of Rules
                    dataRules := SELF:BuildControlRules( SELF:_propertiesRules, relItem:FoxClassName )
                    // Apply Rules to Properties
                    relItem:ConvertProperties( dataRules, SELF:_defaultValues )
                    setDataEnv:Append( relItem:ApplyPropertiesRules( TRUE ) )
                    setDataEnv:Append("SELF:")
                    setDataEnv:Append(dataEnvItem:Name)
                    setDataEnv:Append(":Relations:Add( ")
                    setDataEnv:Append(relItem:Name)
                    setDataEnv:Append(" )")
                    setDataEnv:Append(Environment.NewLine)
                NEXT
                // Emit OPEN DATABASE for each DBC referenced by a cursor.
                // Path is built at runtime from AppDomain.CurrentDomain.BaseDirectory so it
                // works regardless of the working directory when the app starts.
                FOREACH VAR dbcPath IN dbcPaths
                    VAR dbcFileName := Path.GetFileName(dbcPath)
                    setDataEnv:Append("OPEN DATABASE System.IO.Path.Combine(AppDomain.CurrentDomain.BaseDirectory, ")
                    IF SELF:Settings:StoreInFolders
                        VAR dbFolder := SELF:Settings:FolderNames["Databases"]
                        setDataEnv:Append(e"\"" + dbFolder + e"\", ")
                    ENDIF
                    setDataEnv:Append(e"\"" + dbcFileName + e"\")")
                    setDataEnv:Append(Environment.NewLine)
                NEXT
                // Now, init (if needed) all Cursors
                // TODO : It would be better to put that in the template, no ?
                setDataEnv:Append("SELF:")
                setDataEnv:Append(dataEnvItem:Name)
                setDataEnv:Append(":Init( )")
                setDataEnv:Append(Environment.NewLine)
                setDataEnv:Append("SELF:FormClosing += System.Windows.Forms.FormClosingEventHandler{ SELF:")
                setDataEnv:Append(dataEnvItem:Name)
                setDataEnv:Append(", @DataEnvironment.DataEnvironment_FormClosing() }")
                setDataEnv:Append(Environment.NewLine)
                //
                //setDataEnv:Append("DoBindings()") Do The Binding in the Template
                setDataEnv:Append(Environment.NewLine)
                //
            ENDIF
        END METHOD


        PROTECTED METHOD GenerateGrids( Grids AS List<SCXVCXItem> ) AS StringBuilder
            VAR columnSettings := StringBuilder{}
            FOREACH VAR grid IN Grids
                // grid:Childs now contains synthetic Column items (BaseClassName="column")
                // produced by SCXVCXFile.SynthesizeGridColumns.  Each synthetic column
                // carries its own PropertiesDict and a Childs list with Header/Text/Spinner.
                FOREACH VAR subItem IN grid:Childs
                    VAR col := (SCXVCXItem) subItem
                    IF String.Compare( col:BaseClassName, "column", TRUE ) != 0
                        LOOP
                    ENDIF
                    // Extract 1-based index from "Column<n>"
                    LOCAL colIdx AS INT
                    IF col:Name:Length <= 6 .OR. !col:Name:StartsWith("Column", StringComparison.OrdinalIgnoreCase)
                        LOOP
                    ENDIF
                    IF !Int32.TryParse( col:Name:Substring(6), OUT colIdx )
                        LOOP
                    ENDIF
                    VAR colPrefix := "SELF:" + grid:Name + ":Column(" + colIdx:ToString() + "):"
                    // Detect non-text CurrentControl (ComboBox, CheckBox, etc.)
                    LOCAL currentControl := "" AS STRING
                    LOCAL currentControlItem AS SCXVCXItem
                    currentControlItem := NULL_OBJECT
                    IF col:PropertiesDict:ContainsKey("CurrentControl")
                        currentControl := col:PropertiesDict["CurrentControl"]:Trim():Trim( <CHAR>{ '"' } ):Trim()
                    ENDIF
                    IF String.IsNullOrEmpty(currentControl)
                        // Fall back: look for a non-Header, non-TextBox child
                        FOREACH VAR childItem IN col:Childs
                            VAR child := (SCXVCXItem)childItem
                            VAR childBase := child:BaseClassName:ToLowerInvariant()
                            IF childBase != "header" .AND. childBase != "textbox"
                                currentControl := childBase
                                currentControlItem := child
                                EXIT
                            ENDIF
                        NEXT
                    ELSE
                        // Find the matching child item by name for property extraction
                        FOREACH VAR childItem IN col:Childs
                            VAR child := (SCXVCXItem)childItem
                            IF String.Compare(child:Name, currentControl, TRUE) == 0 .OR. ;
                               String.Compare(child:BaseClassName, currentControl, TRUE) == 0
                                currentControlItem := child
                                EXIT
                            ENDIF
                        NEXT
                    ENDIF
                    IF !String.IsNullOrEmpty(currentControl)
                        LOCAL colType AS INT
                        colType := -1
                        SWITCH currentControl:ToLowerInvariant()
                        CASE "checkbox" ; colType := 3
                        CASE "check1"   ; colType := 3
                        CASE "combobox" ; colType := 5
                        CASE "combo1"   ; colType := 5
                        END SWITCH
                        IF colType > 0
                            columnSettings:Append(colPrefix)
                            columnSettings:AppendLine("ColumnType := " + colType:ToString())
                            IF colType == 5 .AND. currentControlItem != NULL_OBJECT
                                LOCAL rowSourceType := 0 AS INT
                                IF currentControlItem:PropertiesDict:ContainsKey("RowSourceType")
                                    Int32.TryParse(currentControlItem:PropertiesDict["RowSourceType"]:Trim(), OUT rowSourceType)
                                ENDIF
                                columnSettings:Append(colPrefix)
                                columnSettings:AppendLine("RowSourceType := " + rowSourceType:ToString())
                                IF currentControlItem:PropertiesDict:ContainsKey("RowSource")
                                    VAR rowSource := currentControlItem:PropertiesDict["RowSource"]:Trim()
                                    IF !String.IsNullOrEmpty(rowSource) .AND. rowSource != "Nil"
                                        columnSettings:Append(colPrefix)
                                        columnSettings:AppendLine("RowSource := " + rowSource)
                                    ENDIF
                                ENDIF
                            ENDIF
                        ELSE
                            VAR msg := "Column " + colIdx:ToString() + " of grid '" + grid:Name + "' uses '" + currentControl + "' as CurrentControl — ColumnType not yet mapped"
                            XPorterLogger.Instance:Warning(msg)
                            columnSettings:AppendLine("// TODO VFPXPorter: " + msg)
                        ENDIF
                    ENDIF
                    // Emit column properties from the synthetic column's PropertiesDict
                    FOREACH VAR kv IN col:PropertiesDict
                        VAR mapped := SELF:MapColumnProperty( kv:Key )
                        IF mapped == NULL
                            LOOP  // removed by rule
                        ENDIF
                        columnSettings:Append(colPrefix)
                        columnSettings:Append(mapped)
                        columnSettings:Append(" := ")
                        IF ( String.Compare(kv:Key, "ForeColor", TRUE) == 0 .OR. ;
                             String.Compare(kv:Key, "BackColor", TRUE) == 0 )
                            columnSettings:Append("VFPTools.ColorFromVFP(")
                            columnSettings:Append(kv:Value)
                            columnSettings:AppendLine(")")
                        ELSE
                            columnSettings:AppendLine(kv:Value)
                        ENDIF
                    NEXT
                    // Emit HeaderText and Header style properties from the Header grandchild
                    FOREACH VAR grandSubItem IN col:Childs
                        VAR grandChild := (SCXVCXItem) grandSubItem
                        IF String.Compare( grandChild:BaseClassName, "header", TRUE ) == 0
                            VAR hdrPrefix := colPrefix + "Header:"
                            // Caption → column HeaderText (convenience shortcut on the column itself)
                            IF grandChild:PropertiesDict:ContainsKey("Caption")
                                VAR caption := grandChild:PropertiesDict["Caption"]
                                IF !String.IsNullOrEmpty(caption) .AND. caption != "Nil"
                                    columnSettings:Append(colPrefix)
                                    columnSettings:Append("HeaderText := ")
                                    columnSettings:AppendLine(caption)
                                ENDIF
                            ENDIF
                            // Color properties — wrap with ColorFromVFP
                            FOREACH VAR colorKey IN <STRING>{ "ForeColor", "BackColor" }
                                IF grandChild:PropertiesDict:ContainsKey(colorKey)
                                    VAR val := grandChild:PropertiesDict[colorKey]:Trim()
                                    IF !String.IsNullOrEmpty(val) .AND. val != "Nil"
                                        columnSettings:Append(hdrPrefix)
                                        columnSettings:Append(colorKey)
                                        columnSettings:Append(" := VFPTools.ColorFromVFP(")
                                        columnSettings:Append(val)
                                        columnSettings:AppendLine(")")
                                    ENDIF
                                ENDIF
                            NEXT
                            // Style properties — direct assignment
                            FOREACH VAR propKey IN <STRING>{ "Alignment", "FontBold", "FontName", "FontSize", "FontItalic", "FontUnderline" }
                                IF grandChild:PropertiesDict:ContainsKey(propKey)
                                    VAR val := grandChild:PropertiesDict[propKey]:Trim()
                                    IF !String.IsNullOrEmpty(val) .AND. val != "Nil"
                                        columnSettings:Append(hdrPrefix)
                                        columnSettings:Append(propKey)
                                        columnSettings:Append(" := ")
                                        columnSettings:AppendLine(val)
                                    ENDIF
                                ENDIF
                            NEXT
                        ENDIF
                    NEXT
                NEXT
            NEXT
            RETURN columnSettings
        END METHOD

        // Maps a raw VFP column property name to its .NET counterpart.
        // Returns NULL to indicate the property should be dropped.
        // Returns the original name for unknown properties (pass-through).
        PRIVATE METHOD MapColumnProperty( propName AS STRING ) AS STRING
            SWITCH propName:ToUpperInvariant()
            CASE "CONTROLSOURCE" ; RETURN "ControlSource"
            CASE "WIDTH"         ; RETURN "Width"
            CASE "ALIGNMENT"     ; RETURN "Alignment"
            CASE "COLUMNORDER"   ; RETURN "ColumnOrder"
            CASE "SPARSE"        ; RETURN "Sparse"
            CASE "RESIZABLE"     ; RETURN "Resizable"
            CASE "FORMAT"        ; RETURN "Format"
            CASE "INPUTMASK"     ; RETURN "InputMask"
            CASE "FORECOLOR"     ; RETURN "ForeColor"
            CASE "BACKCOLOR"     ; RETURN "BackColor"
            CASE "CURRENTCONTROL"
            CASE "TEXTALIGN"
            CASE "TEXTUALIGN"
            CASE "MOUSEPOINTER"   // no per-column cursor in WinForms DataGridViewColumn
                RETURN NULL  // removed
            END SWITCH
            RETURN propName  // unknown — pass through
        END METHOD

        // Collect all non-virtual descendants of non-grid containers within item.
        // Returns pairs of (descendant, real-parent-container).
        // Grid sub-controls (column headers, edit controls) are excluded — they are virtual.
        PRIVATE METHOD CollectContainerDescendants( item AS SCXVCXItem ) AS List<Tuple<SCXVCXItem,SCXVCXItem>>
            VAR result := List<Tuple<SCXVCXItem,SCXVCXItem>>{}
            FOREACH VAR subItem IN item:Childs
                VAR child := (SCXVCXItem) subItem
                IF String.Compare( child:BaseClassName, "grid", TRUE ) == 0
                    LOOP
                ENDIF
                IF child:Childs:Count > 0
                    FOREACH VAR grandSubItem IN child:Childs
                        result:Add( Tuple.Create( (SCXVCXItem)grandSubItem, child ) )
                    NEXT
                ENDIF
            NEXT
            RETURN result
        END METHOD

        // Build the Controls:Add statement for a descendant whose direct parent is a virtual
        // Page<n> inside the given real ancestor container.
        PRIVATE METHOD GetPageAddStatement( child AS SCXVCXItem, ancestor AS SCXVCXItem ) AS STRING
            VAR prefix := ancestor:FullName + "."
            IF child:Parent:Length > prefix:Length .AND. child:Parent:StartsWith(prefix, StringComparison.OrdinalIgnoreCase)
                VAR virtualSeg := child:Parent:Substring(prefix:Length)
                VAR dotPos := virtualSeg:IndexOf('.')
                IF dotPos > 0
                    virtualSeg := virtualSeg:Substring(0, dotPos)
                ENDIF
                IF virtualSeg:Length > 4 .AND. virtualSeg:StartsWith("Page", StringComparison.OrdinalIgnoreCase)
                    LOCAL pageNum AS INT
                    IF Int32.TryParse( virtualSeg:Substring(4), OUT pageNum )
                        RETURN "SELF:" + ancestor:Name + ":Page(" + pageNum:ToString() + "):Controls:Add(SELF:" + child:Name + ")"
                    ENDIF
                ENDIF
                RETURN "SELF:" + ancestor:Name + ":Controls:Add(SELF:" + child:Name + ")"
            ENDIF
            RETURN "SELF:" + ancestor:Name + ":Controls:Add(SELF:" + child:Name + ")"
        END METHOD

        /// <summary>
        /// Returns a ButtonFactory assignment line for CommandGroup/OptionGroup items whose
        /// MemberClass is a non-default custom type. Must be called BEFORE ConvertProperties
        /// so MemberClass is still in PropertiesDict. Returns empty string when not applicable.
        /// </summary>
        PRIVATE METHOD BuildMemberFactory( item AS SCXVCXItem ) AS STRING
            VAR baseClass := item:BaseClassName
            IF String.Compare(baseClass, "commandgroup", TRUE) != 0 .AND. ;
               String.Compare(baseClass, "optiongroup",  TRUE) != 0
                RETURN ""
            ENDIF
            IF !item:PropertiesDict:ContainsKey("MemberClass")
                RETURN ""
            ENDIF
            VAR memberClass := item:PropertiesDict["MemberClass"]:Trim()
            IF String.IsNullOrEmpty(memberClass)
                RETURN ""
            ENDIF
            VAR defaultMember := IIF(String.Compare(baseClass, "commandgroup", TRUE) == 0, "commandbutton", "optionbutton")
            IF String.Compare(memberClass, defaultMember, TRUE) == 0
                RETURN ""
            ENDIF
            // Map VFP class name to X# type via TypeConvert, fall back to as-is
            VAR typeName := memberClass
            IF SELF:_typeList != NULL .AND. SELF:_typeList:ContainsKey(memberClass)
                typeName := SELF:_typeList[memberClass][0]  // take the first mapped type if multiple
            ENDIF
            RETURN "SELF:" + item:Name + ":ButtonFactory := { => " + typeName + "{} }" + Environment.NewLine
        END METHOD

        PROTECTED METHOD ProcessUserDefinition( userDefs AS List<UserDefinition>, userDefCode AS STRING ) AS VOID
            LOCAL sourceLines AS List<STRING>
            LOCAL userCodes AS List<BlockCode>
            // Create a List of Lines
            sourceLines := SELF:ReadSource( userDefCode )
            userCodes := List<BlockCode>{}
            //
            LOCAL code := NULL AS BlockCode
            FOREACH line AS STRING IN sourceLines
                IF line:StartsWith("PROCEDURE ")
                    IF code != NULL
                        userCodes:Add( code )
                    ENDIF
                    // "PROCEDURE ":Length == 10
                    LOCAL lne AS STRING
                    lne := line:Substring( 10 )
                    lne := lne:Trim()
                    // Retrieve the Name of the Procedure ( So, the Name of the Event)
                    VAR words := lne:Split( <CHAR>{ '(', ' ' } )
                    // Create an BlockCode with the EventName, and the Definition Line
                    code := BlockCode{ words[1] , line }
                ELSE
                    IF code != NULL
                        // Add all Lines associated with this event
                        code:Source:Add( line )
                    ENDIF
                ENDIF
            NEXT
            //
            IF code != NULL
                userCodes:Add( code )
            ENDIF
            // Now, associate Code and Definition
            FOREACH VAR userCode IN userCodes
                //
                VAR userDef := userDefs:Find( { x => String.Compare( x:Name, userCode:Name , TRUE ) == 0 } )
                IF userDef != NULL
                    userDef:Code := userCode
                ENDIF
            NEXT
            RETURN


            // Read all Lines of Source Code
        PRIVATE METHOD ReadSource( source AS STRING ) AS List<STRING>
            LOCAL strReader AS StringReader
            LOCAL lines AS List<STRING>
            LOCAL oneLine AS STRING
            //
            lines := List<STRING>{}
            strReader := StringReader{ source }
            //
            REPEAT
                oneLine := strReader:ReadLine()
                IF ( oneLine != NULL )
                    lines:Add( oneLine )
                ENDIF
            UNTIL oneLine == NULL
            //
            RETURN lines

        /// <summary>
        /// Look into the Properties Dictionary if there some inherited Properties
        /// </summary>
        PRIVATE METHOD CheckInheritedProperties( item AS SCXVCXItem ) AS List<Tuple<KeyValuePair<STRING,STRING>,SCXVCXItem>>
            LOCAL lFound AS LOGIC
            VAR found := List<STRING>{}
            VAR tempDict := SerializableDictionary<STRING,STRING>{StringComparer.InvariantCultureIgnoreCase}
            VAR InheritedControls := List<Tuple<KeyValuePair<STRING,STRING>,SCXVCXItem>>{}
            // Get Each Property Definition ( Name / Value )
            FOREACH VAR prop IN item:PropertiesDict
                VAR propName := prop:Key
                // Is there a dot in the Name
                IF propName:IndexOf('.') > 0
                    VAR elements := propName:Split( <CHAR>{'.'} )
                    // Get the first element / Object Name
                    VAR childName := elements[1]:Trim()
                    // Already Found ?
                    IF ! found:Contains( childName )
                        lFound := FALSE
                        // Does it comes from a "super" class ?
                        VAR searchFor := item:FullyQualifiedFoxClassName
                        //VAR candidate := String.Empty
                        WHILE SELF:CustomControls:ContainsKey( searchFor )
                            VAR foundItem := SELF:CustomControls[ searchFor ]
                            // Do we have a corresponding child there ?
                            FOREACH VAR childItem IN foundItem:Childs
                                IF String.Compare(childName, childItem:OBJNAME, TRUE ) == 0
                                    // Got it !
                                    InheritedControls.Add( Tuple<KeyValuePair<STRING,STRING>,SCXVCXItem>{prop, (SCXVCXItem) childItem } )
                                    found:Add(childName )
                                    lFound := TRUE
                                    EXIT
                                ENDIF
                            NEXT
                            IF lFound
                                EXIT
                            ENDIF
                            // Search on next Level
                            searchFor := foundItem:FullyQualifiedName
                            //candidate := searchFor
                        ENDDO
                        // Not found ? Put back in Dictionary
                        IF !lFound
                            tempDict:Add( prop:Key, prop:Value )
                        ENDIF
                    ELSE
                        // Already checked, just add
                        // Find the Couple Property-Item
                        VAR itemPos := InheritedControls:FindIndex( { x => x:Item1:Key:StartsWith(childName+".") } )
                        IF itemPos != -1
                            // Get it
                            VAR elt := InheritedControls[ itemPos ]
                            // Create a new couple, with the new Prop and the Item
                            InheritedControls.Add( Tuple<KeyValuePair<STRING,STRING>,SCXVCXItem>{prop,(SCXVCXItem) elt:Item2 } )
                        ENDIF
                    ENDIF
                ELSE
                    tempDict:Add( prop:Key, prop:Value )
                ENDIF
            NEXT
            // The new Properties Dictionary, if we have some InheritedControls
            IF tempDict:Count > 0
                item:PropertiesDict := tempDict
            ENDIF
            RETURN InheritedControls

        /// <summary>
        /// Point the decoration elements to Designer and Form
        /// </summary>
        METHOD InitElementsFormDesigner() AS VOID
            DesignerPrefixFile		:= XPorterSettings.DesignerPrefixFile
            DesignerStartTypeFile	:= XPorterSettings.DesignerStartTypeFile
            DesignerEndTypeFile		:= XPorterSettings.DesignerEndTypeFile
            DesignerInitTypeFile	:= XPorterSettings.DesignerInitTypeFile
            FormPrefixFile			:= XPorterSettings.FormPrefixFile
            FormStartTypeFile		:= XPorterSettings.FormStartTypeFile
            FormEndTypeFile			:= XPorterSettings.FormEndTypeFile
            FormInitTypeFile		:= XPorterSettings.FormInitTypeFile

        /// <summary>
        /// Point the decoration elements to single Form file
        /// </summary>
        METHOD InitElementsSingleForm( container AS LOGIC ) AS VOID
            IF container
                FormPrefixFile			:= XPorterSettings.SingleFormPrefixFile
                FormStartTypeFile		:= XPorterSettings.SingleFormStartTypeFile
                FormEndTypeFile			:= XPorterSettings.SingleFormEndTypeFile
                FormInitTypeFile		:= XPorterSettings.SingleFormInitTypeFile
            ELSE
                FormPrefixFile			:= XPorterSettings.SingleNoContainerFormPrefixFile
                FormStartTypeFile		:= XPorterSettings.SingleNoContainerFormStartTypeFile
                FormEndTypeFile			:= XPorterSettings.SingleNoContainerFormEndTypeFile
                FormInitTypeFile		:= XPorterSettings.SingleNoContainerFormInitTypeFile
            ENDIF
            BindingCodeFile			:= XPorterSettings.BindingCodeFile
            RETURN

        /// <summary>
        /// Gets template file content from cache, loading from disk only if not cached.
        /// Caching significantly reduces disk I/O for repeated template access.
        /// </summary>
        PROTECTED METHOD GetTemplateFromCache(filePath AS STRING) AS STRING
            LOCAL content AS STRING
            IF SELF:_templateCache:ContainsKey(filePath)
                RETURN SELF:_templateCache[filePath]
            ENDIF
            content := File.ReadAllText(filePath)
            SELF:_templateCache[filePath] := content
            RETURN content

        /// <summary>
        /// Gets JSON configuration file content from cache, loading from disk only if not cached.
        /// Reduces disk I/O when the same JSON files are accessed repeatedly across conversions.
        /// </summary>
        PROTECTED METHOD GetJsonFromCache(filePath AS STRING) AS STRING
            LOCAL content AS STRING
            IF SELF:_templateCache:ContainsKey(filePath)
                RETURN SELF:_templateCache[filePath]
            ENDIF
            content := File.ReadAllText(filePath)
            SELF:_templateCache[filePath] := content
            RETURN content

        /// <summary>
        /// Extracts all placeholders from a template string.
        /// Placeholders are in the format <@placeholderName@>
        /// </summary>
        /// <param name="template">The template string to analyze</param>
        /// <returns>HashSet of placeholder names found (without the <@ @> delimiters)</returns>
#region vfpxporter
        PROTECTED _worker AS BackgroundWorker
        PROPERTY Worker AS BackgroundWorker
            SET
                SELF:_worker := VALUE
            END SET
        END PROPERTY

        PROTECTED _progress AS INT
        PROTECTED _totalItems AS INT

        PROPERTY Canceled AS LOGIC GET IIF( _worker != NULL, _worker:CancellationPending, FALSE )

        METHOD ResetProgress( total AS INT ) AS VOID
            SELF:_progress := 0
            SELF:_totalItems := total

        METHOD UpdateProgress() AS VOID
            IF ( SELF:_worker != NULL )
                SELF:_progress ++
                VAR percentComplete := (INT)Math.Round((REAL8)(100 * _progress) / _totalItems)
                IF percentComplete > 100
                    percentComplete := 100
                ENDIF
                SELF:_worker:ReportProgress(percentComplete)
            ENDIF
#endregion

    END CLASS
END NAMESPACE // FabVFPXPorter
