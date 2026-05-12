// SCXVCXEntity.prg
// Created by    : fabri
// Creation Date : 9/24/2023 7:36:47 PM
// Created for   :
// WorkStation   : FABXPS


USING System
USING System.Collections.Generic
USING System.Text
USING System.IO

BEGIN NAMESPACE VFPXPorterLib

    /// <summary>
    /// The SCXVCXEntity class.
    /// </summary>
    CLASS SCXVCXEntity

        PROPERTY DataEnvironment AS SCXVCXItem AUTO

        PROPERTY Item AS SCXVCXItem AUTO

        PRIVATE _container AS SCXVCXFile

        CONSTRUCTOR( container AS SCXVCXFile )
            SELF:DataEnvironment := NULL
            SELF:Item := NULL
            //
            SELF:_container := container
            RETURN


        PROPERTY IsForm AS LOGIC GET ( String.Compare( SELF:Item:BaseClassName, "form", TRUE ) == 0 )

        PROPERTY IsFormSet AS LOGIC GET ( String.Compare( SELF:Item:BaseClassName, "formset", TRUE ) == 0 )

        /// <summary>
        /// The list of Files that this Entity are depending on
        /// </summary>
        PROPERTY DependsOn AS HashSet<STRING> AUTO GET PRIVATE SET

        /// <summary>
        /// A Dictionary of CustomControls defined in the current Entity
        /// </summary>
        PROPERTY DefiningControls AS Dictionary<STRING, SCXVCXItem> AUTO GET PRIVATE SET


        /// <summary>
        /// List all Dependencies for this entity.
        /// Check the visibility of User Defined elements that belongs to this item.
        /// Report Item and Childs name to XPorterLogger.
        /// </summary>
        METHOD Analyze() AS VOID
            SELF:DefiningControls := Dictionary<STRING, SCXVCXItem>{}
            SELF:DependsOn := HashSet<STRING>{}
            //
            SELF:AnalyzeItem( SELF:Item )
            //
            SELF:EnumEntity()

        PRIVATE METHOD AnalyzeItem( item AS SCXVCXItem ) AS VOID
            // Do we have User Defined elements that belongs to this item ? ( Method, Field )
            IF !String.IsNullOrEmpty(item:UserDefined)
                VAR lines := item:UserDefined:Split( <STRING>{ Environment.NewLine }, StringSplitOptions.RemoveEmptyEntries )
                FOREACH VAR line IN lines
                    item:UserDefItems:Add( UserDefinition{ line } )
                NEXT
                // Do we have User Defined elements that belongs to this item ? ( Method, Field )
                // If there, they are at least Protected (or more)
                IF !String.IsNullOrEmpty(item:Protected)
                    lines := item:Protected:Split( <STRING>{ Environment.NewLine }, StringSplitOptions.RemoveEmptyEntries )
                    FOREACH VAR line IN lines
                        VAR Name := line
                        VAR Vis := UserDefinition.Visibility.Protected
                        IF Name:EndsWith( "^" )
                            Name := Name:Substring(0, Name:Length -1 )
                            Vis := UserDefinition.Visibility.Private
                        ENDIF
                        VAR userDef := item:UserDefItems:Find( { x => String.Compare( x:Name, Name, TRUE ) == 0 } )
                        IF userDef != NULL
                            userDef:Modifier := Vis
                        ENDIF
                    NEXT
                ENDIF
            ENDIF
            // Now, check for dependencies (Some Controls that comes from Libraries)
            IF !String.IsNullOrEmpty(item:ClassLocation)
                // This Item depends on a class lib ?
                IF ! IsFullPath( item.ClassLocation )
                    // Does it depends on the current file ??
                    VAR inputPath := Path.GetDirectoryName( SELF:_container:FileName )
                    VAR itemPath := Path.GetFullPath( Path.Combine( inputPath, item.ClassLocation  ))
                    IF String.Compare( SELF:_container:FileName, itemPath, TRUE )!=0
                        // NO :)
                        SELF:DependsOn:Add( itemPath )
                    ENDIF
                ELSE
                    SELF:DependsOn:Add( item:ClassLocation )
                ENDIF
            ENDIF
            // Don't Forget Childs...
            IF item:Childs:Count > 0
                FOREACH VAR baseitm IN item:Childs
                    IF baseitm IS SCXVCXItem VAR child
                        SELF:AnalyzeItem( child )
                    ENDIF
                NEXT
            ENDIF
            // We are processing a Library File ?
            IF SELF:_container:IsLibrary
                VAR nameSpace := Path.GetFileNameWithoutExtension( SELF:_container:FileName )
                VAR FQN := nameSpace + "."
                // Ok, we may define some new controls
                IF String.IsNullOrEmpty( item:Parent )
                    FQN += item:OBJNAME
                    // In the library FQN ( myvcx.mycontrol ), we define Item:ClassName
                    SELF:DefiningControls:Add( FQN, item ) // item:FullyQualifiedName )
                ELSE
                    FQN += item:Parent + "."
                    FQN += item:OBJNAME
                    SELF:DefiningControls:Add( FQN, item ) // item:FullyQualifiedName )
                ENDIF
            ENDIF

        END METHOD

        PROTECTED METHOD EnumEntity( ) AS VOID
            //
            XPorterLogger.Instance:Information( "Entity : " + SELF:Item:Name )
            XPorterLogger.Instance:Information( " Has DataEnvironment : " + IIF( SELF:DataEnvironment == NULL, "False", "True") )
            IF SELF:Item:Childs:Count > 0
                XPorterLogger.Instance:Information( "----------" )
                XPorterLogger.Instance:Information( "Contains "+ SELF:Item:Childs:Count:ToString() + " item(s)." )
                SELF:EnumChilds( SELF:Item:Childs, "   " )
                XPorterLogger.Instance:Information( "----------" )
            ENDIF
            SELF:EnumDependencies()
            RETURN

        PROTECTED METHOD EnumChilds( itemList AS List<BaseItem>, indent AS STRING ) AS VOID
            FOREACH VAR item IN itemList
                //
                XPorterLogger.Instance:Information( indent + "Item : "+ item:Name )
                IF item:Childs:Count > 0
                    XPorterLogger.Instance:Information( indent + "----------" )
                    XPorterLogger.Instance:Information( indent + "Contains "+ item:Childs:Count:ToString() + " item(s)." )
                    SELF:EnumChilds( item:Childs, indent + "   " )
                    XPorterLogger.Instance:Information( indent + "----------" )
                ENDIF
            NEXT
        END METHOD

        PROTECTED METHOD EnumDependencies() AS VOID
            IF ( SELF:DependsOn:Count > 0 )
                XPorterLogger.Instance:Information( "Depends On : " )
                FOREACH VAR externalFile IN DependsOn
                    XPorterLogger.Instance:Information( externalFile )
                NEXT
            ENDIF
        END METHOD

    END CLASS
END NAMESPACE // VFPXPorterLib