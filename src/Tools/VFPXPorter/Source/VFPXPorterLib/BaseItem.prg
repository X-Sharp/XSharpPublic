USING System
USING System.Collections.Generic
USING System.Text
USING System.Xml.Serialization

BEGIN NAMESPACE VFPXPorterLib

    /// <summary>
    /// The BaseItem class, used during the XMLExport
    /// </summary>
    [XmlInclude( TYPEOF(SCXVCXItem))];
    CLASS BaseItem


        PRIVATE _className AS STRING
        PRIVATE _isConverted AS LOGIC

        VIRTUAL PROPERTY Childs AS List<BaseItem> AUTO


        CONSTRUCTOR()
            SELF:Init()
            RETURN

        PRIVATE METHOD Init() AS VOID
            SELF:Childs := List<BaseItem>{}
            SELF:PropertiesDict := SerializableDictionary<STRING,STRING>{StringComparer.InvariantCultureIgnoreCase}
            SELF:_isConverted := FALSE
            RETURN

        /// <summary>
        /// ClassName of the Item.
        /// The original one is kept into the FoxClassName property
        /// This one can contains the ClassName after convertion to the equivalent .NET Type
        /// </summary>
        PROPERTY ClassName AS STRING
            GET
                RETURN SELF:_className
            END GET

            SET
                SELF:_className := VALUE
                IF String.IsNullOrEmpty( SELF:FoxClassName )
                    SELF:FoxClassName := VALUE
                ELSE
                    // It's been the second time we set a value, so ... a conversion to .NET Type
                    SELF:_isConverted := TRUE
                ENDIF
            END SET
        END PROPERTY

        [XmlIgnore];
        PROPERTY IsConverted AS LOGIC GET SELF:_isConverted

        [XmlIgnore];
        PROPERTY IsForm AS LOGIC GET ( String.Compare( SELF:BaseClassName, "form", TRUE ) == 0 )

        [XmlIgnore];
        PROPERTY IsFormSet AS LOGIC GET ( String.Compare( SELF:BaseClassName, "formset", TRUE ) == 0 )

            [XmlIgnore];
        PROPERTY FullyQualifiedName AS STRING
            GET
                LOCAL fqn := SELF:ClassName AS STRING
                // If Converted, let's guess the name is already FullyQualified
                IF SELF:_isConverted
                    // We are looking for a Fully-Qualified-Name...Don't forget to add the Namespace coming from the Settings
                    IF !String.IsNullOrEmpty(XPorterSettings.SuppportLib)
                        fqn := XPorterSettings.SuppportLib + "." + fqn
                    ENDIF
                ELSE
                    // Ok, we must check the ClassLocation, if not empty, that is a ClassLibrary file
                    // and we will use it as a Namespace
                    LOCAL nSpace := "" AS STRING
                    IF !String.IsNullOrEmpty( SELF:ClassLocation )
                        nSpace := System.IO.Path.GetFileNameWithoutExtension( SELF:ClassLocation ) + "."
                    ENDIF
                    fqn := nSpace + SELF:ClassName
                ENDIF
                //
                RETURN fqn
            END GET
        END PROPERTY

        /// <summary>
        /// ClassName of the Item (Keeps the original CLASSNAME after processing)
        /// </summary>
        PROPERTY FoxClassName AS STRING AUTO //GET PROTECTED SET

            [XmlIgnore];
        PROPERTY FullyQualifiedFoxClassName AS STRING
            GET
                // Ok, we check the ClassLocation, if not empty, that is a ClassLibrary file
                // and we will use it as a Namespace
                LOCAL nSpace := "" AS STRING
                IF !String.IsNullOrEmpty( SELF:ClassLocation )
                    nSpace := System.IO.Path.GetFileNameWithoutExtension( SELF:ClassLocation ) + "."
                ENDIF
                RETURN nSpace + SELF:FoxClassName
            END GET
        END PROPERTY

        /// <summary>
        /// The Path to the VCX That contains the Definition :
        /// So the "ClassName" is defined in the VCX in the ObjName Field/ Name Property here
        /// </summary>
        PROPERTY ClassLocation AS STRING GET SELF:CLASSLOC SET SELF:CLASSLOC := VALUE

        /// <summary>
        /// The SCX (Form)/VCX (Visual Control Library) File that contains this Item
        /// </summary>
        PROPERTY FileName AS STRING AUTO

        /// <summary>
        /// Indicate if the file is a VCX (Visual Control Library)
        /// </summary>
        PROPERTY IsInLibrary AS LOGIC GET String.Compare( System.IO.Path.GetExtension( SELF:FileName ), ".vcx", TRUE ) == 0

        /// <summary>
        /// Base ClassName of the Item (The one used to render in the designer)
        /// </summary>
        PROPERTY BaseClassName AS STRING GET SELF:BASECLASS SET SELF:BASECLASS := VALUE

        /// <summary>
        /// A Dictionary with Property Pair : Name / Value
        /// </summary>
        PROPERTY PropertiesDict AS SerializableDictionary<STRING,STRING> AUTO

        /// <summary>
        /// Object Name ( OBJNAME field )
        /// </summary>
        PROPERTY Name AS STRING GET SELF:OBJNAME SET SELF:OBJNAME := VALUE

        /// <summary>
        /// Parent Object Name of the current Item (Fully Qualified Name)
        /// For eg, if the object is inside a panel that is inside a Form, we will have formName.panelName
        /// </summary>
        PROPERTY Parent AS STRING AUTO

        [XmlIgnore];
        PROPERTY FullName AS STRING GET IIF( String.IsNullOrEmpty(SELF:Parent),SELF:Name, SELF:Parent + "." + SELF:Name )

        /// <summary>
        /// The VFP Code that belongs to this Object ( METHODS field )
        /// </summary>
        PROPERTY RawCode AS STRING GET SELF:METHODS SET SELF:METHODS := VALUE

        /// <summary>
        /// The XSharp Ported Code that belongs to this Object
        /// </summary>
        PROPERTY XPortedCode AS ItemCode AUTO

        /// <summary>
        /// The list of User-Defined elements, that are non-public.
        /// One element per line :
        /// Private members have a ^ terminating
        /// </summary>
        PROPERTY Protected AS STRING AUTO

        /// <summary>
        /// The help for this class ( RESERVED7 field )
        /// </summary>
        PROPERTY ClassHelp AS STRING GET SELF:RESERVED7 SET SELF:RESERVED7 := VALUE

        /// <summary>
        /// The list of User-Defined elements for this class ( RESERVED3 field )
        /// One element per line :
        /// Private members have a ^ terminating
        /// </summary>
        PROPERTY UserDefined AS STRING GET SELF:RESERVED3 SET SELF:RESERVED3 := VALUE





            // PLATFORM,C,8,0
        PROPERTY PLATFORM AS STRING AUTO
            // UNIQUEID,C,10,0
        PROPERTY UNIQUEID AS STRING AUTO
            // TIMESTAMP,N,10,0
        PROPERTY TIMESTAMP AS INT AUTO
            // CLASSLOC,M,4,0
        PROPERTY CLASSLOC AS STRING AUTO
            // BASECLASS,M,4,0
        PROPERTY BASECLASS AS STRING AUTO
            // OBJNAME,M,4,0
        PROPERTY OBJNAME AS STRING AUTO
            // PROPERTIES,M,4,0
        PROPERTY PROPERTIES AS STRING AUTO
            // METHODS,M,4,0
        PROPERTY METHODS AS STRING AUTO
            // OLE,M,4,0
        PROPERTY OLE AS STRING AUTO
            // OLE2,M,4,0
        PROPERTY OLE2 AS STRING AUTO
            // RESERVED1,M,4,0
        PROPERTY RESERVED1 AS STRING AUTO
            // RESERVED2,M,4,0
        PROPERTY RESERVED2 AS STRING AUTO
            // RESERVED3,M,4,0
        PROPERTY RESERVED3 AS STRING AUTO
            // RESERVED4,M,4,0
        PROPERTY RESERVED4 AS STRING AUTO
            // RESERVED5,M,4,0
        PROPERTY RESERVED5 AS STRING AUTO
            // RESERVED6,M,4,0
        PROPERTY RESERVED6 AS STRING AUTO
            // RESERVED7,M,4,0
        PROPERTY RESERVED7 AS STRING AUTO
            // RESERVED8,M,4,0
        PROPERTY RESERVED8 AS STRING AUTO
            // USER,M,4,0
        PROPERTY USER AS STRING AUTO


        /// <summary>
        /// Copy Constructor
        /// </summary>
        CONSTRUCTOR( itemToCopy AS BaseItem )
            //
            SELF:Init()
            // Copy all Attributes
            SELF:PLATFORM := itemToCopy:PLATFORM
            SELF:UNIQUEID := itemToCopy:UNIQUEID
            SELF:TIMESTAMP := itemToCopy:TIMESTAMP
            // !!! WARNING !!! This may incorrectly set the FoxClassName
            SELF:ClassName := itemToCopy:ClassName
            SELF:FoxClassName := itemToCopy:FoxClassName
            //
            SELF:CLASSLOC := itemToCopy:CLASSLOC
            SELF:BASECLASS := itemToCopy:BASECLASS
            SELF:OBJNAME := itemToCopy:OBJNAME
            SELF:Parent := itemToCopy:Parent
            SELF:PROPERTIES := itemToCopy:PROPERTIES
            SELF:Protected := itemToCopy:Protected
            SELF:METHODS := itemToCopy:METHODS
            SELF:OLE := itemToCopy:OLE
            SELF:OLE2 := itemToCopy:OLE2
            SELF:RESERVED1 := itemToCopy:RESERVED1
            SELF:RESERVED2 := itemToCopy:RESERVED2
            SELF:RESERVED3 := itemToCopy:RESERVED3
            SELF:RESERVED4 := itemToCopy:RESERVED4
            SELF:RESERVED5 := itemToCopy:RESERVED5
            SELF:RESERVED6 := itemToCopy:RESERVED6
            SELF:RESERVED7 := itemToCopy:RESERVED7
            SELF:RESERVED8 := itemToCopy:RESERVED8
            SELF:USER := itemToCopy:USER
            // The File that contains the Item
            SELF:FileName := itemToCopy:FileName
            // Don't forget code that have already been processed
            IF itemToCopy:XPortedCode != NULL
                SELF:XPortedCode := ItemCode{ itemToCopy:XPortedCode }
            ENDIF
            // Copy the Item Properties
            FOREACH VAR Prop IN itemToCopy:PropertiesDict
                SELF:PropertiesDict:Add( Prop:Key, Prop:Value )
            NEXT
            // And don't forget Childs....
            FOREACH VAR child IN itemToCopy:Childs
                SELF:Childs:Add( SCXVCXItem{ child } )
            NEXT
            RETURN

        /// <summary>
        /// DBF Reading Constructor
        /// </summary>
        CONSTRUCTOR( lFillWithDB AS LOGIC )
            //
            SELF:Init()
            IF lFillWithDB
                SELF:PLATFORM := FieldGet(FieldPos("PLATFORM"))
                SELF:UNIQUEID := FieldGet(FieldPos("UNIQUEID"))
                SELF:TIMESTAMP := FieldGet(FieldPos("TIMESTAMP"))
                SELF:ClassName := FieldGet(FieldPos("CLASS"))
                SELF:CLASSLOC := FieldGet(FieldPos("CLASSLOC"))
                SELF:BASECLASS := FieldGet(FieldPos("BASECLASS"))
                SELF:OBJNAME := FieldGet(FieldPos("OBJNAME"))
                SELF:Parent := FieldGet(FieldPos("PARENT"))
                SELF:PROPERTIES := FieldGet(FieldPos("PROPERTIES"))
                SELF:Protected := FieldGet(FieldPos("PROTECTED"))
                SELF:METHODS := FieldGet(FieldPos("METHODS"))
                SELF:OLE := FieldGet(FieldPos("OLE"))
                SELF:OLE2 := FieldGet(FieldPos("OLE2"))
                SELF:RESERVED1 := FieldGet(FieldPos("RESERVED1"))
                SELF:RESERVED2 := FieldGet(FieldPos("RESERVED2"))
                SELF:RESERVED3 := FieldGet(FieldPos("RESERVED3"))
                SELF:RESERVED4 := FieldGet(FieldPos("RESERVED4"))
                SELF:RESERVED5 := FieldGet(FieldPos("RESERVED5"))
                SELF:RESERVED6 := FieldGet(FieldPos("RESERVED6"))
                SELF:RESERVED7 := FieldGet(FieldPos("RESERVED7"))
                SELF:RESERVED8 := FieldGet(FieldPos("RESERVED8"))
                SELF:USER := FieldGet(FieldPos("USER"))
            ENDIF
            RETURN



    END CLASS
END NAMESPACE