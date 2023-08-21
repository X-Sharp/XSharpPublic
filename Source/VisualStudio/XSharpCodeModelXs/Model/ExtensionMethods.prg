//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.Linq
USING System.Collections.Generic
USING LanguageService.CodeAnalysis.XSharp
USING LanguageService.SyntaxTree
USING XSharp.Settings

BEGIN NAMESPACE XSharpModel


    STATIC CLASS SynchronizedExtensionMethods
        STATIC METHOD AddRange<T>(SELF items as ICollection<T> ,  newItems as IEnumerable<T> ) AS VOID
            if newItems != null
                FOREACH VAR item in newItems
                    items:Add(item)
                NEXT
            endif
            RETURN
    END CLASS

    STATIC CLASS ExtensionMethods

        static method GetTickedname(SELF name as STRING) AS STRING
             IF name:StartsWith("@@")
                name := name:Substring(2)
            ENDIF
            var pos := name:IndexOf('<')
            if pos > 0
                var rest := name.Substring(pos)
                name := name.Substring(0, pos )
                var types := rest.Split(<char>{',','>','<'}, StringSplitOptions.RemoveEmptyEntries)
                name += "`" + types.Length.ToString()
            endif
            return name

        STATIC METHOD AddRange<T>(SELF collection as HashSet<T>, newItems as IEnumerable<T>) as VOID
            if newItems != null
                FOREACH VAR item in newItems
                    collection:Add(item)
                NEXT
            endif
            RETURN

        STATIC METHOD CleanText(SELF token as IToken) AS STRING
            var result := token:Text
            if result:StartsWith("@@")
                result := result:Substring(2)
            ENDIF
            RETURN result

        //		STATIC METHOD IsEmpty( SELF cType AS CompletionType) AS LOGIC
        //			RETURN cType == NULL .OR. ! cType:IsInitialized

        STATIC METHOD AddUnique<TKey, TValue>( SELF dict AS Dictionary<TKey, TValue>, key AS TKey, VALUE AS TValue) AS TValue
            IF dict != NULL .AND. key != NULL
                IF ! dict:ContainsKey(key)
                    dict:Add(key, VALUE)
                    RETURN VALUE
                ENDIF
                RETURN dict:Item[key]
            ENDIF
        RETURN DEFAULT (TValue)
        STATIC METHOD AddUnique<TKey>( SELF list AS IList<TKey>, key AS TKey) AS VOID
            IF list != NULL .AND. key != NULL
                IF ! list:Contains(key)
                    list:Add(key)
                    RETURN
                ENDIF
            ENDIF
            RETURN

        STATIC METHOD DisplayName( SELF elementKind AS Kind) AS STRING
            SWITCH elementKind
                CASE Kind.VOGlobal
                    RETURN "GLOBAL"
                CASE Kind.VODefine
                    RETURN "DEFINE"
                CASE Kind.EnumMember
                    RETURN "MEMBER"
                CASE Kind.LocalFunc
                    RETURN "LOCAL FUNCTION"
                CASE Kind.LocalProc
                    RETURN "LOCAL PROCEDURE"
                CASE Kind.VODLL
                    RETURN "DLL FUNCTION"
                CASE Kind.Define
                    RETURN "#define"
                CASE Kind.Undefine
                    RETURN "#undef"
                CASE Kind.Command
                    RETURN "#command"
                CASE Kind.XCommand
                    RETURN "#xcommand"
                CASE Kind.Translate
                    RETURN "#translate"
                CASE Kind.XTranslate
                    RETURN "#xtranslate"
                CASE Kind.Include
                    RETURN "#include"
                CASE Kind.Undeclared
                    var cName := "UNDECLARED VARIABLE"
                    IF ModelWalker.IsRunning .or. ModelWalker.HasWork
                        cName += " (building Intellisense database not completed"
                    ENDIF
                    RETURN cName
            END SWITCH
        RETURN elementKind:ToString()

        STATIC METHOD HasEndKeyword(SELF elementKind as Kind) AS LOGIC
            SWITCH elementKind
            CASE Kind.Class
            CASE Kind.Structure
            CASE Kind.Interface
            CASE Kind.Enum
            CASE Kind.Namespace
            CASE Kind.Event
            CASE Kind.Property
            CASE Kind.LocalFunc
            CASE Kind.LocalProc
                RETURN TRUE
            END SWITCH
            RETURN FALSE

        STATIC METHOD HasParameters( SELF elementKind AS Kind) AS LOGIC
            SWITCH elementKind
                CASE Kind.Constructor
                CASE Kind.Method
                CASE Kind.Assign
                CASE Kind.Access
                CASE Kind.Property
                CASE Kind.Function
                CASE Kind.Procedure
                CASE Kind.Event
                CASE Kind.Operator
                CASE Kind.Delegate
                CASE Kind.VODLL
                CASE Kind.LocalFunc
                CASE Kind.LocalProc
                    //
                    RETURN TRUE
            END SWITCH
        RETURN FALSE

        STATIC METHOD HasReturnType( SELF elementKind AS Kind ) AS LOGIC
            SWITCH elementKind
                CASE Kind.Method
                CASE Kind.Access
                CASE Kind.Property
                CASE Kind.Function
                CASE Kind.Field
                CASE Kind.Local
                CASE Kind.Parameter
                CASE Kind.MemVar
                CASE Kind.DbField
                CASE Kind.Operator
                CASE Kind.Delegate
                CASE Kind.VOGlobal
                CASE Kind.VODefine
                CASE Kind.LocalFunc
                    RETURN TRUE
            END SWITCH
        RETURN FALSE

        STATIC METHOD HasReturnType( SELF elementKind AS Kind, inDialect AS XSharpDialect) AS LOGIC
            IF inDialect == XSharpDialect.FoxPro .and. elementKind == Kind.Procedure
                RETURN TRUE
            ENDIF
        RETURN HasReturnType(elementKind)

        STATIC METHOD IsGlobalTypeMember(SELF elementKind AS Kind) AS LOGIC
            SWITCH elementKind
                CASE Kind.VOGlobal
                CASE Kind.VODefine
                CASE Kind.Function
                CASE Kind.Procedure
                CASE Kind.VODLL
                CASE Kind.Define
                CASE Kind.Undefine
                CASE Kind.Command
                CASE Kind.XCommand
                CASE Kind.Translate
                CASE Kind.XTranslate
                CASE Kind.Attribute
                CASE Kind.Include
                    RETURN TRUE
            END SWITCH
        RETURN FALSE

        STATIC METHOD IsPPSymbol(SELF elementKind AS Kind) AS LOGIC
            SWITCH elementKind
            CASE Kind.Attribute
            CASE Kind.Define
            CASE Kind.Undefine
            CASE Kind.Command
            CASE Kind.XCommand
            CASE Kind.Translate
            CASE Kind.XTranslate
            CASE Kind.Include
                RETURN TRUE
            END SWITCH
        RETURN FALSE

        STATIC METHOD IsClassMember( SELF elementKind AS Kind, inDialect AS XSharpDialect ) AS LOGIC
            SWITCH elementKind
                CASE Kind.Constructor
                CASE Kind.Destructor
                CASE Kind.Method
                CASE Kind.Access
                CASE Kind.Assign
                CASE Kind.Property
                CASE Kind.Event
                CASE Kind.Operator
                CASE Kind.Field
                    RETURN TRUE
                OTHERWISE
                    IF ( inDialect == XSharpDialect.FoxPro )
                        SWITCH elementKind
                            CASE Kind.Function
                            CASE Kind.Procedure
                                RETURN TRUE
                        END SWITCH
                    ENDIF
            END SWITCH
        RETURN FALSE

        STATIC METHOD IsClassMethod( SELF elementKind AS Kind, inDialect AS XSharpDialect ) AS LOGIC
            SWITCH elementKind
                CASE Kind.Method
                    RETURN TRUE
                OTHERWISE
                    IF ( inDialect == XSharpDialect.FoxPro )
                        SWITCH elementKind
                            CASE Kind.Function
                            CASE Kind.Procedure
                                RETURN TRUE
                        END SWITCH
                    ENDIF
            END SWITCH
        RETURN FALSE

        STATIC METHOD IsField( SELF elementKind AS Kind) AS LOGIC
            SWITCH elementKind
                CASE Kind.Field
                CASE Kind.VOGlobal
                CASE Kind.VODefine
                CASE Kind.EnumMember
                CASE Kind.Define
                CASE Kind.Undefine
                CASE Kind.Command
                CASE Kind.XCommand
                CASE Kind.Translate
                CASE Kind.XTranslate
                CASE Kind.Attribute
                CASE Kind.Include
                CASE Kind.Using
                    RETURN TRUE
            END SWITCH
        RETURN FALSE

        STATIC METHOD IsProperty( SELF elementKind AS Kind) AS LOGIC
            SWITCH elementKind
                CASE Kind.Access
                CASE Kind.Assign
                CASE Kind.Property
                    RETURN TRUE
            END SWITCH
        RETURN FALSE


        STATIC METHOD IsMethod( SELF elementKind AS Kind) AS LOGIC
            SWITCH elementKind
                CASE Kind.Method
                CASE Kind.Function
                CASE Kind.Procedure
                    RETURN TRUE
            END SWITCH
        RETURN FALSE


        STATIC METHOD IsType( SELF elementKind AS Kind) AS LOGIC
            SWITCH elementKind
                CASE Kind.Class
                CASE Kind.Structure
                CASE Kind.Interface
                CASE Kind.Delegate
                CASE Kind.Enum
                CASE Kind.VOStruct
                CASE Kind.Union
                    RETURN TRUE
            END SWITCH
        RETURN FALSE

        STATIC METHOD HasBody( SELF elementKind AS Kind) AS LOGIC
            SWITCH elementKind
                CASE Kind.Function
                CASE Kind.Procedure
                CASE Kind.Method
                CASE Kind.Access
                CASE Kind.Assign
                CASE Kind.Property
                CASE Kind.Event
                CASE Kind.Operator
                CASE Kind.Constructor
                CASE Kind.Destructor
                CASE Kind.LocalFunc
                CASE Kind.LocalProc
                    RETURN TRUE
            END SWITCH
        RETURN FALSE

        STATIC METHOD IsLocal(SELF eKind AS Kind) AS LOGIC
            SWITCH eKind
                CASE Kind.LocalFunc
                CASE Kind.LocalProc
                    RETURN TRUE
            END SWITCH
        RETURN FALSE


        STATIC METHOD HasMembers(SELF eKind AS Kind) AS LOGIC
            SWITCH eKind
                CASE Kind.Class
                CASE Kind.Structure
                CASE Kind.Interface
                CASE Kind.Enum
                CASE Kind.VOStruct
                CASE Kind.Union
                    RETURN TRUE
            END SWITCH
        RETURN FALSE


        STATIC METHOD HasChildren(SELF eKind AS Kind) AS LOGIC
            SWITCH eKind
                CASE Kind.Namespace
                CASE Kind.Class
                CASE Kind.Structure
                CASE Kind.Interface
                    RETURN TRUE
            END SWITCH
        RETURN FALSE




        //list exstensions
        STATIC METHOD AddUnique( SELF list AS List<STRING>, item AS STRING) AS VOID
            IF !list:Contains(item, System.StringComparer.OrdinalIgnoreCase)
                list:Add(item)
        ENDIF

        STATIC METHOD Expanded( SELF source AS IEnumerable<STRING>) AS IList<STRING>
            LOCAL list AS List<STRING>
            LOCAL item AS STRING
            list := List<STRING>{}
            list:AddRange(source)
            FOREACH str AS STRING IN source
                item := str
                WHILE (item:Contains("."))
                item := item:Substring(0, item:LastIndexOf("."))
                IF (! list:Contains(item))
                    list:Add(item)
                ENDIF
            ENDDO
            NEXT
        RETURN list:AsReadOnly()

        STATIC METHOD GetGlyph( SELF kind as Kind, visibility as Modifiers) AS LONG


            VAR imgK := ImageListKind.Class
            VAR imgO := ImageListOverlay.Public
            SWITCH kind
                CASE Kind.Class
                    imgK := ImageListKind.Class
                CASE Kind.Function
                CASE Kind.Procedure
                CASE Kind.LocalFunc
                CASE Kind.LocalProc
                    imgK := ImageListKind.Overload
                CASE Kind.Constructor
                CASE Kind.Destructor
                CASE Kind.Method
                    imgK := ImageListKind.Method
                CASE Kind.Union
                    imgK := ImageListKind.Union
                CASE Kind.Structure
                    imgK := ImageListKind.Structure
                CASE Kind.VOStruct
                    imgK := ImageListKind.Type
                CASE Kind.Access
                CASE Kind.Assign
                CASE Kind.Property
                    imgK := ImageListKind.Property
                CASE Kind.Event
                    imgK := ImageListKind.Event
                CASE Kind.Delegate
                    imgK := ImageListKind.Delegate
                CASE Kind.Operator
                    imgK := ImageListKind.Operator
                CASE Kind.VODefine
                CASE Kind.Define
                CASE Kind.Undefine
                    imgK := ImageListKind.Const
                CASE Kind.Enum
                    imgK := ImageListKind.Enum
                CASE Kind.EnumMember
                    imgK := ImageListKind.EnumValue
                CASE Kind.Interface
                    imgK := ImageListKind.Interface
                CASE Kind.Namespace
                    imgK := ImageListKind.Namespace
                CASE Kind.VOGlobal
                CASE Kind.Field
                    imgK := ImageListKind.Field
                CASE Kind.Parameter
                CASE Kind.Local
                CASE Kind.MemVar
                CASE Kind.DbField
                CASE Kind.Undeclared
                CASE Kind.TypeParameter
                    imgK := ImageListKind.Local
                CASE Kind.Command
                CASE Kind.XCommand
                CASE Kind.Translate
                CASE Kind.XTranslate
                    imgK := ImageListKind.Macro
                CASE Kind.Include
                    imgK := ImageListKind.Library
                CASE Kind.Keyword
                    imgK := ImageListKind.Keyword
                CASE Kind.Attribute
                    imgK := ImageListKind.XmlAttribute
            END SWITCH
            SWITCH visibility
                CASE Modifiers.Public
                    imgO := ImageListOverlay.Public
                CASE Modifiers.Protected
                    imgO := ImageListOverlay.Protected
                CASE Modifiers.Private
                    imgO := ImageListOverlay.Private
                CASE Modifiers.Internal
                    imgO := ImageListOverlay.Internal
                CASE Modifiers.ProtectedInternal
                    imgO := ImageListOverlay.ProtectedInternal
            END SWITCH
        RETURN (LONG) imgK + (LONG)imgO

        STATIC METHOD AddRange<T>(SELF list AS IList<T>, items AS IEnumerable<T> ) AS VOID WHERE T IS CLASS
            IF list IS List<T> VAR ListT
                ListT:AddRange(items)
            ELSE
                FOREACH VAR item IN items
                    list:Add(item)
                NEXT
            ENDIF
            RETURN
 STATIC METHOD ToDisplayString(SELF mods AS Modifiers) AS STRING
            // remove EXTERNAL since we do not have that in our language
            mods := _AND(mods, ~Modifiers.External)
            if (mods == Modifiers.None)
                return ""
            endif
            VAR result := mods:ToString():Replace(","," ")
            if mods:HasFlag(Modifiers.ProtectedInternal)
                result := "PROTECTED INTERNAL"
            elseif mods:HasFlag(Modifiers.Public)
                switch XSettings.CodeGeneratorPublicStyle
                    case PublicStyle.Public
                        result := result.Replace("Export","Public")
                    case PublicStyle.Export
                        result := result.Replace("Public","Export")
                    case PublicStyle.None
                        result := result.Replace("Public","")
                        result := result.Replace("Export","")
                end switch

            ELSEIF mods:HasFlag(Modifiers.Private)
                switch XSettings.CodeGeneratorPrivateStyle
                    case PrivateStyle.Private
                        result := result.Replace("Hidden","Private")
                    case PrivateStyle.Hidden
                        result := result.Replace("Private","Hidden")
                end switch
            ENDIF

        RETURN XLiterals.Capitalize(result)

        STATIC METHOD ToDisplayString(SELF kind as Kind) AS STRING
            VAR result := kind:DisplayName()
            RETURN XLiterals.Capitalize(result)

    END CLASS
END NAMESPACE




INTERNAL FUNCTION RemoveGenericParameters(typeName as STRING) AS STRING
    var pos := typeName:IndexOf('<')
    IF pos > 0
        typeName := typeName:Substring(0, pos)
    ENDIF
RETURN typeName

