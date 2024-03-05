// XPorterCtrlForm.prg
// Created by    : fabri
// Creation Date : 9/25/2023 10:59:09 AM
// Created for   :
// WorkStation   : FABXPS

USING System
USING System.Collections.Generic
USING System.Text
USING System.IO
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


        PROPERTY DesignerPrefix AS STRING GET File.ReadAllText(DesignerPrefixFile)
        PROPERTY DesignerStartType AS STRING GET File.ReadAllText(DesignerStartTypeFile)
        PROPERTY DesignerEndType AS STRING GET File.ReadAllText(DesignerEndTypeFile)
        PROPERTY DesignerInitType AS STRING GET File.ReadAllText(DesignerInitTypeFile)
        PROPERTY FormPrefix AS STRING GET File.ReadAllText(FormPrefixFile)
        PROPERTY FormStartType AS STRING GET File.ReadAllText(FormStartTypeFile)
        PROPERTY FormEndType AS STRING GET File.ReadAllText(FormEndTypeFile)
        PROPERTY FormInitType AS STRING GET File.ReadAllText(FormInitTypeFile)
        PROPERTY BindingCode AS STRING GET File.ReadAllText(BindingCodeFile)

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
        PROPERTY PropRules AS STRING GET File.ReadAllText( PropRulesFile )

        /// <summary>
        /// Rules that applies to Events (DblClick->DoubleClick, ...)
        /// </summary>
        /// <value></value>
        PROPERTY EventRules AS STRING GET File.ReadAllText( EventRulesFile )

        /// <summary>
        /// Type convertion table (Button->VFPCommandButton, ...)
        /// </summary>
        /// <value></value>
        PROPERTY ConvertTable AS STRING GET File.ReadAllText( ConvertTableFile )

        /// <summary>
        /// List of Statements than will need () at the end (.Refresh->.Refresh(), ...)
        /// </summary>
        /// <value></value>
        PROPERTY Statements AS STRING GET File.ReadAllText( StatementsFile )

        /// <summary>
        /// List of VFP language elements and their traduction (Parent->_Parent,this->thisObject, ...)
        /// </summary>
        /// <value></value>
        PROPERTY VFPElements AS STRING GET File.ReadAllText( VFPElementsFile )


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
                    ExtractCode( tempPath, item:Childs, IIF( String.IsNullOrEmpty(parent), "", parent + "_" ) + item:Name )
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
                    SELF:ConvertHandlerCode( subItem, typeList, eventList, sttmnts, vfpElts,TRUE )
                NEXT
                IF SELF:Canceled
                    EXIT
                ENDIF
                // Don't forget to process the Item itself
                SELF:ConvertHandlerCode( item, typeList, eventList, sttmnts, vfpElts,FALSE )
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
        PROTECTED METHOD ConvertHandlerCode( subItem AS SCXVCXItem, typeList AS Dictionary<STRING,STRING[]>, eventList AS Dictionary<STRING, Dictionary<STRING,STRING[]>>, sttmnt AS List<STRING>, vfpElt AS Dictionary<STRING,STRING>, isChild AS LOGIC ) AS VOID
            subItem:ConvertClassName( typeList )
            // Extract the code, and split it to Events
            subItem:XPortedCode := ItemCode{ subItem, isChild }
            // Set of Rules
            LOCAL evtRules AS Dictionary<STRING,STRING[]>
            // Use the Rendering ClassName in order to get the Events name
            evtRules := BuildEventRules( eventList, subItem:BaseClassName )
            // Apply Rules and Create EventHandlers
            subItem:ConvertEvents( evtRules, sttmnt, vfpElt, SELF:Settings )
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
            CATCH
                lOk := FALSE
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
                CATCH
                    lOk := FALSE
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
            CATCH
                lOk := FALSE
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
                code:Replace( "<@formName@>", oneItem.FullName )
//             ELSE
//                 code:Replace( "<@formName@>", SELF:FormNameOverride )
//             ENDIF
            code:Replace( "<@superName@>", oneItem:FullyQualifiedName )   //<- FoxClassName
            code:Replace( "<@dataenvironment@>", declareDataEnv:ToString() )
            code:Replace( "<@childsDeclaration@>", declaration:ToString() )
            //
            dest:Write( code:ToString() )
            // Now, the Core-Code
            code := StringBuilder{SELF:FormInitType}
            //
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
            IF SELF:Canceled
                RETURN
            ENDIF
            code:Replace("<@InitContainers@>", containerHandlers:ToString() )
            // Now, check the Grids
            VAR columnSettings := SELF:GenerateGrids( Grids )
            code:Replace("<@InitGrids@>", columnSettings:ToString() )

            // Any event for the Form ?
            handlers:AppendLine( SELF:BuildEventHandlers( oneItem ) )
            //
            IF oneItem:IsForm
                code:Replace( "<@setdataenvironment@>", setDataEnv:ToString() )
            ELSE
                code:Replace( "<@setdataenvironment@>", "" )
            ENDIF
            // Add User-defined code, if any
            IF oneItem:UserDefItems != NULL
                VAR userdefProp := StringBuilder{}
                userdefProp:Append(oneItem:ApplyPropertiesRules( FALSE, FALSE, 2 ))
                code:Replace( "<@userdefProps@>", userdefProp:ToString() )
            ENDIF
            //
            code:Replace( "<@EventHandlers@>", handlers:ToString() )
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

//             IF String.IsNullOrEmpty( SELF:FormNameOverride )
                code:Replace( "<@formName@>", IIF( String.IsNullOrEmpty(oneItem:Parent),oneItem:Name, oneItem:Parent + "_" + oneItem:Name ))
//             ELSE
//                 code:Replace( "<@formName@>",  SELF:FormNameOverride )
//             ENDIF
            // NameSpace addition ??
            code:Replace( "<@superName@>", oneItem:FullyQualifiedName )
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
            code:Replace( "<@childsDeclaration@>", declaration:ToString() )
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
                ctrlRules := SELF:BuildControlRules( SELF:_propertiesRules, scxSubItem:FullyQualifiedFoxClassName )
                // Apply Rules to Properties
                scxSubItem:ConvertProperties( ctrlRules, SELF:_defaultValues, TRUE )
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
            // Controls must be added in reverse order
            VAR addCtrl := StringBuilder{}
            WHILE controlStack:Count > 0
                addCtrl:Append("SELF:Controls:Add(SELF:")
                addCtrl:Append(controlStack:Pop())
                addCtrl:Append(")")
                addCtrl:Append(Environment.NewLine)
            ENDDO
            // Now, put the code at the right places
            code:Replace( "<@childsInstantiate@>", instantiate:ToString())
            code:Replace( "<@childsInitialize@>", initChilds:ToString())
            code:Replace( "<@addChildsToParent@>", addCtrl:ToString() )
            // Now, set the Parent Properties

            // Add EventHandlers
            IF oneItem:XPortedCode != NULL
                formProp:Append( oneItem:CreateEventHandlers( FALSE, SELF:Settings  ) )
            ENDIF
            code:Replace( "<@formProps@>", formProp:ToString() )
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
                // So here, we have Tuple with a KeyValuePair which is Property/Value and the SCXVCXItem that is the inherited Control
                // Create a copy of the Item
                VAR scxSubItem := SCXVCXItem{ propInheritedCtrl:Item2 }
                IF processingItem != NULL
                    IF ( processingItem:Name != scxSubItem:Name )
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
                        processingItem := scxSubItem
                        processingItem:PropertiesDict:Clear()
                    ELSE
                        // And set its Property
                        VAR propItem := propInheritedCtrl:Item1
                        VAR propName := propItem:Key
                        VAR dotPos := propName:IndexOf('.')
                        propName := propName:Substring( dotPos+1 )
                        //processingItem:PropertiesDict:Remove( propName )
                        processingItem:PropertiesDict:Add( propName, propItem:Value )
                    ENDIF
                ELSE
                    processingItem := scxSubItem
                    processingItem:PropertiesDict:Clear()
                ENDIF
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
            code:Replace( "<@formName@>", oneItem:Name )
            code:Replace( "<@superName@>", oneItem:ClassName )
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
            //
            code:Replace( "<@dataenvironment@>", declareDataEnv:ToString() )
            code:Replace( "<@childsDeclaration@>", declaration:ToString() )
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
                ctrlRules := SELF:BuildControlRules( SELF:_propertiesRules, scxSubItem:FullyQualifiedFoxClassName )
                // Apply Rules to Properties
                scxSubItem:ConvertProperties( ctrlRules, SELF:_defaultValues, TRUE )
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
            IF oneItem:XPortedCode != NULL
                formProp:Append( oneItem:CreateEventHandlers( FALSE , SELF:Settings ) )
            ENDIF
            // Now, put the code at the right places
            code:Replace( "<@childsInstantiate@>", instantiate:ToString())
            code:Replace( "<@childsInitialize@>", initChilds:ToString())
            code:Replace( "<@addChildsToParent@>", addCtrl:ToString() )
            // Now, set the Parent Properties, usually in InitializeComponent()
            code:Replace( "<@formProps@>", formProp:ToString() )
            code:Replace( "<@userdefProps@>", userdefProp:ToString() )
            //
            IF oneItem:IsForm
                code:Replace( "<@setdataenvironment@>", setDataEnv:ToString() )
            ELSE
                code:Replace( "<@setdataenvironment@>", "" )
            ENDIF
            // Add EventHandlers
            VAR handlers := StringBuilder{}
            FOREACH VAR subItem IN oneItem:Childs
                //
                handlers:AppendLine( BuildEventHandlers( subItem ) )
            NEXT
            // Any event for the Form ?
            handlers:AppendLine( SELF:BuildEventHandlers( oneItem ) )
            //
            code:Replace("<@InitContainers@>", containerHandlers:ToString() )
            // Now, check the Grids
            VAR columnSettings := SELF:GenerateGrids( Grids )
            code:Replace("<@InitGrids@>", columnSettings:ToString() )
            //
            code:Replace( "<@EventHandlers@>", handlers:ToString() )
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


        PROTECTED METHOD GetOutputFilename( item AS BaseItem ) AS STRING
            LOCAL outFileName AS STRING
            //
//             IF String.IsNullOrEmpty( SELF:FormNameOverride )
                // Only one Form ?
                IF item != NULL
                    VAR itemName := item:Name
                    // We could be in a FormSet, add the Parent as a Prefix
                    IF item:IsForm .AND. !String.IsNullOrEmpty(item:Parent)
                        itemName := item:Parent + "_" + itemName
                    ENDIF
                    IF SELF:Settings:PrefixClassFile
                        outFileName := Path.GetFileNameWithoutExtension( SELF:Settings:ItemsPath ) + "_" + itemName
                    ELSE
                        outFileName := itemName
                    ENDIF
                    outFileName := Path.GetFileNameWithoutExtension(SELF:Settings:ItemsPath ) + "_" + outFileName

                ELSE
                    outFileName := Path.GetFileNameWithoutExtension( SELF:Settings:ItemsPath )
                ENDIF
//             ELSE
//                 outFileName := SELF:FormNameOverride
//             ENDIF
            // Create the File based on the Form Name, or the scx file in case of trouble...
            LOCAL destFile AS STRING
            destFile := Path.Combine(SELF:Settings:OutputPath, outFileName )
            destFile := Path.ChangeExtension( destFile, "prg")
            RETURN destFile
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
                // For each Cursor
                FOREACH VAR dataCursor IN dataEnvItem:Childs
                    LOCAL cursorItem AS SCXVCXItem
                    cursorItem := (SCXVCXItem) dataCursor
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
                    // Set of Rules
                    dataRules := BuildControlRules( SELF:_propertiesRules, cursorItem:FoxClassName )
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
                setDataEnv:Append("DoBindings()")
                setDataEnv:Append(Environment.NewLine)
                //
            ENDIF
        END METHOD


        PROTECTED METHOD GenerateGrids( Grids AS List<SCXVCXItem> ) AS StringBuilder
            VAR columnSettings := StringBuilder{}
            VAR locals := List<STRING>{}
            LOCAL ctrlRules AS Dictionary<STRING,STRING>
            FOREACH VAR grid IN Grids
                // Todo : If we have more than one grid, prefix the Column Setting with the Grid Name ??
                // Column settings are Childs of the Grid
                FOREACH VAR subItem IN grid:Childs
                    LOCAL columnSetting AS SCXVCXItem
                    columnSetting := (SCXVCXItem) subItem
                    columnSetting:ConvertClassName( SELF:_typeList )
                    // Set of Rules
                    ctrlRules := SELF:BuildControlRules( SELF:_propertiesRules, columnSetting:FullyQualifiedFoxClassName )
                    columnSetting:ConvertProperties( ctrlRules, SELF:_defaultValues, TRUE )
                    // Var Already defined ?
                    IF !locals:Contains( columnSetting:Name:ToLower() )
                        columnSettings:Append("LOCAL ")
                        columnSettings:Append(columnSetting:Name)
                        columnSettings:Append(" AS ")
                        // NameSpace addition ??
                        columnSettings:Append(columnSetting:FullyQualifiedName)
                        columnSettings:Append(Environment.NewLine)
                        //
                        locals:Add( columnSetting:Name:ToLower() )
                    ENDIF
                    //
                    columnSettings:Append(columnSetting:Name)
                    columnSettings:Append(" := ")
                    columnSettings:Append(columnSetting:ClassName)
                    columnSettings:Append("{}")
                    columnSettings:Append(Environment.NewLine)
                    //
                    // true => to get the Item name: true => as Local, so no SELF prefix
                    columnSettings:Append( columnSetting:ApplyPropertiesRules( TRUE, TRUE ) )
                    // The parentName contains the Grid and the Column, on form of form1.grid1.column1
                    // so extract the grid and column grid1.column1
                    VAR gridName := String.Empty
                    VAR startG := columnSetting:Parent:ToLower():IndexOf( ".grid" )
                    gridName := columnSetting:Parent:Substring( startG+1 )
                    // then turn it to grid1.column(1)
                    VAR columnNumber := StringBuilder{}
                    FOR VAR i := gridName:Length-1 TO 0 STEP -1
                        IF !Char.IsDigit( gridName[i] )
                            gridName := gridName:Substring( 0, i+1 )
                            EXIT
                        ELSE
                            columnNumber:Insert( 0, gridName[i] )
                        ENDIF
                    NEXT
                    //
                    columnSettings:Append("SELF:")
                    columnSettings:Append(gridName)
                    columnSettings:Append("(")
                    columnSettings:Append(columnNumber:ToString())
                    columnSettings:Append("):")
                    columnSettings:Append(columnSetting:BASECLASS)
                    columnSettings:Append(" := ")
                    columnSettings:Append(columnSetting:Name)
                    columnSettings:Append(Environment.NewLine)
                    columnSettings:Append(Environment.NewLine)
                NEXT
            NEXT
            //
            RETURN columnSettings
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
