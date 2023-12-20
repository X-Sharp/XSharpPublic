//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.Linq
USING System.Collections.Generic
USING LanguageService.CodeAnalysis.Text
USING LanguageService.CodeAnalysis.XSharp
USING System.Diagnostics
USING LanguageService.SyntaxTree

BEGIN NAMESPACE XSharpModel
   /// <summary>
      /// Model for Namespace, Class, Interface, Structure, Enum
   /// </summary>
   [DebuggerDisplay("{ToString(),nq}")];
   CLASS XSourceTypeSymbol INHERIT XSourceEntity IMPLEMENTS IXTypeSymbol,IXSourceEntity
      PRIVATE _isPartial      AS LOGIC
      PRIVATE _members        AS List<XSourceMemberSymbol>
      PRIVATE _basemembers    AS List<IXMemberSymbol>
      PRIVATE _baseType       AS IXTypeSymbol
      PRIVATE _children       AS List<XSourceTypeSymbol>
      PRIVATE _signature      AS XTypeSignature
      PRIVATE _isClone        AS LOGIC
      PROPERTY SourceCode     AS STRING AUTO
      PROPERTY ShortName      AS STRING GET IIF(!SELF:IsGeneric, SELF:Name, SELF:Name:Substring(0, SELF:Name:IndexOf("<")-1))
      PROPERTY GenericName    AS STRING AUTO
      PROPERTY TickedName     AS STRING
        GET
            IF SELF:IsGeneric .and. ! SELF:Name:Contains('`')
                RETURN SELF:Name+"`"+SELF:TypeParameters:Count:ToString()
            ENDIF
            RETURN SELF:Name
        END GET
      END PROPERTY

      CONSTRUCTOR(name AS STRING, kind AS Kind, attributes AS Modifiers, span AS TextRange, position AS TextInterval, oFile AS XFile, modifiers AS IList<IToken>)
         SUPER(name, kind, attributes, span, position)
         SELF:_members     := List<XSourceMemberSymbol>{}
         SELF:_basemembers := List<IXMemberSymbol>{}
         SELF:_children    := List<XSourceTypeSymbol>{}
         SELF:_signature   := XTypeSignature{""}
         SELF:ClassType    := XSharpDialect.Core
         SELF:Namespace    := ""
         IF attributes:HasFlag(Modifiers.Static)
            SELF:IsStatic := TRUE
         ENDIF
         IF attributes:HasFlag(Modifiers.Partial)
            SELF:_isPartial := TRUE
         ENDIF
         SELF:File := oFile
         IF modifiers?:Count > 0
             SELF:BlockTokens:AddRange(modifiers)
         ENDIF

        CONSTRUCTOR(dbresult AS XDbResult, file AS XFile)
            SELF(dbresult:TypeName, dbresult:Kind, dbresult:Attributes, dbresult:TextRange, dbresult:TextInterval, file,NULL)
            SELF:CopyValuesFrom(dbresult)


         /// <summary>
            /// Duplicate the current Object, so we have the same properties in another object
            /// </summary>
         /// <returns></returns>
      CONSTRUCTOR( oOther AS XSourceTypeSymbol)
         SELF(oOther:Name, oOther:Kind, oOther:Attributes, oOther:Range, oOther:Interval, oOther:File,oOther:BlockTokens)
         SELF:Parent    := oOther:Parent
         SELF:BaseTypeName  := oOther:BaseTypeName
         SELF:IsPartial := oOther:IsPartial
         SELF:IsStatic  := oOther:IsStatic
         SELF:Namespace := oOther:Namespace
         SELF:AddMembers(oOther:XMembers)
         SELF:_isClone  := TRUE
         RETURN

      METHOD AddChild(oChild AS XSourceTypeSymbol) AS VOID
         BEGIN LOCK SELF:_children
            SELF:_children:Add(oChild)
            oChild:Parent := SELF
         END LOCK

      METHOD AddMember(oMember AS XSourceMemberSymbol) AS VOID
         BEGIN LOCK SELF:_members
            SELF:_members:Add(oMember)
            oMember:Parent        := SELF
            oMember:DeclaringType := SELF:FullName
         END LOCK

      METHOD AddMembers(members AS IEnumerable<XSourceMemberSymbol>) AS VOID
         BEGIN LOCK SELF:_members
            FOREACH VAR oMember IN members
               SELF:_members:Add(oMember)
               oMember:Parent        := SELF
               oMember:DeclaringType := SELF:FullName
            NEXT
         END LOCK

      INTERNAL METHOD SetInterfaces (interfaces as IList<String>) AS VOID
         SELF:_signature:Interfaces:Clear()
         FOREACH var interf in interfaces
                SELF:_signature:Interfaces:Add(interf:Trim())
         NEXT

      METHOD AddInterface(sInterface AS STRING) AS VOID
         SELF:_signature:AddInterface(sInterface:Trim())

      METHOD AddTypeParameter(name AS STRING) AS VOID
         SELF:_signature:TypeParameters:Add(name:Trim())

      METHOD AddConstraints(name AS STRING) AS VOID
         SELF:_signature:TypeParameterContraints:Add(name:Trim())

      PROPERTY Interfaces  AS IList<STRING>              GET SELF:_signature:Interfaces:ToArray()
      PROPERTY InterfaceList AS STRING                   GET SELF:_signature:InterfaceList
      PROPERTY IsGlobal    AS LOGIC                      GET SELF:Name == XLiterals.GlobalName
      PROPERTY TypeParameters as IList<STRING>           GET SELF:_signature:TypeParameters:ToArray()
      PROPERTY TypeParameterList AS STRING               GET SELF:_signature:TypeParameterList
      PROPERTY TypeParameterConstraints as IList<STRING> GET SELF:_signature:TypeParameterContraints:ToArray()
      PROPERTY TypeParameterConstraintsList  AS STRING   GET SELF:_signature:TypeParameterConstraintsList
      PROPERTY OriginalTypeName              AS STRING   GET SELF:TypeName
      PROPERTY IsFunctionsClass              AS LOGIC    GET SELF:Name == XLiterals.GlobalName
      METHOD ClearMembers() AS VOID
         SELF:_members:Clear()

      PROPERTY Members AS IList<IXMemberSymbol>
         GET
            BEGIN LOCK SELF:_members
               RETURN SELF:_members:ToArray()
            END LOCK
         END GET
      END PROPERTY

        PROPERTY XMembers AS IList<XSourceMemberSymbol>
            GET
                BEGIN LOCK SELF:_members
                    RETURN SELF:_members:ToArray()
                END LOCK
            END GET
        END PROPERTY

      PROPERTY AllMembers AS IList<IXMemberSymbol>
         GET
            SELF:ForceComplete()
            BEGIN LOCK SELF:_members
               VAR members := List<IXMemberSymbol>{}
               members:AddRange(SELF:_members)
               members:AddRange(SELF:_basemembers)
               RETURN members:ToArray()
            END LOCK
         END GET
      END PROPERTY


      INTERNAL METHOD SetMembers( list AS  IList<XSourceMemberSymbol>) AS VOID
        SELF:_members:Clear()
        SELF:_members:AddRange(list)
        RETURN

      METHOD GetMembers(elementName AS STRING) AS IList<IXMemberSymbol>
         VAR tempMembers := List<IXMemberSymbol>{}
         If ! String.IsNullOrEmpty(elementName)
            elementName := elementName:GetTickedname()
            tempMembers:AddRange(SELF:_members:Where({ m => m.TickedName:StartsWith(elementName, StringComparison.OrdinalIgnoreCase)} ))
         ELSE
            tempMembers:AddRange(SELF:_members)
         ENDIF
         RETURN tempMembers

      METHOD GetMembers(elementName AS STRING, lExact as LOGIC) AS IList<IXMemberSymbol>
         elementName := elementName:GetTickedname()
         IF lExact
            VAR result := List<IXMemberSymbol>{}
            result:AddRange(SELF:_members:Where ({ m => m.TickedName:Equals(elementName, StringComparison.OrdinalIgnoreCase)} ))
            RETURN result
         ELSE
            RETURN SELF:GetMembers(elementName)
         ENDIF

      METHOD ForceComplete as VOID
        IF SELF:Attributes:HasFlag(Modifiers.Partial)
             // Find all other parts to find the base typename
             var oClone := SELF:Clone
             SELF:_signature:BaseType := oClone:BaseTypeName
             var aIF := oClone:Interfaces:ToArray()
             SELF:SetInterfaces(aIF)
        ENDIF
        IF SELF:_baseType == NULL .and. SELF:BaseTypeName != NULL .and. SELF:File != NULL
            SELF:_baseType := SELF:File:Project:FindType(SELF:BaseTypeName, SELF:File:Usings)
            if self:_baseType != NULL
                self:_basemembers:Clear()
                self:_basemembers:AddRange(self:_baseType:AllMembers:Where( { m => m:IsMethodVisibleInSubclass() }) )
            endif
        ENDIF
        RETURN

      PROPERTY FullName  AS STRING
            GET
                IF SELF:IsGeneric .and. String.IsNullOrEmpty(SELF:GenericName)
                    SELF:GenericName :=  SELF:_GetGenericName()
                ENDIF
                RETURN SELF:GetFullName()
            END GET
      END PROPERTY
      PROPERTY IsGeneric as LOGIC    GET SELF:TypeParameters:Count > 0

      /// <summary>
         /// Merge two XSourceTypeSymbol Objects : Used to create the resulting  XSourceTypeSymbol from 2 or more partial classes
         /// </summary>
      /// <param name="otherType"></param>
      METHOD Merge(otherType AS XSourceTypeSymbol) AS XSourceTypeSymbol
         LOCAL clone AS XSourceTypeSymbol
         IF ! self:_isClone
            clone := XSourceTypeSymbol{SELF}
         ELSE
            clone := SELF
         ENDIF
         VAR otherFile := otherType:File?:FullPath:ToLower() DEFAULT ""
         VAR thisFile  := SELF:File?:FullPath:ToLower()  DEFAULT ""
         IF otherFile != thisFile
            SELF:IsPartial := TRUE
            IF otherType != NULL
               clone:AddMembers(otherType:XMembers)
               IF clone:Parent == NULL .AND. otherType:Parent != NULL
                  clone:Parent := otherType:Parent
               ELSE
                  IF String.IsNullOrEmpty(clone:BaseTypeName) .AND. !String.IsNullOrEmpty(otherType:BaseTypeName)
                     clone:BaseTypeName := otherType:BaseTypeName
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
         RETURN clone

     METHOD CopyValuesFrom(dbresult AS XDbResult) AS VOID
         SUPER:CopyValuesFrom(dbresult)
         SELF:Namespace  := dbresult:Namespace
         SELF:Id         := dbresult:IdType
         SELF:ClassType   := (XSharpDialect) dbresult:ClassType


      /// <summary>
      /// If this XSourceTypeSymbol is a Partial type, return a Copy of it, merged with all other informations
      /// coming from other files.
      /// </summary>

      PROPERTY Clone AS XSourceTypeSymbol
         GET
            IF SELF:IsPartial .AND. SELF:File != NULL
               RETURN SUPER:File:Project:Lookup(SELF:FullName, SELF:File:Usings:ToArray())
            ENDIF
            RETURN SELF
         END GET
      END PROPERTY

      PROPERTY BaseType          AS IXTypeSymbol GET SELF:_baseType
      PROPERTY BaseTypeName      AS STRING GET SELF:_signature:BaseType SET SELF:_signature:BaseType := @@value
      PROPERTY ComboPrototype    AS STRING GET SELF:FullName
      PROPERTY Description       AS STRING GET SELF:GetDescription()
      PROPERTY IsPartial         AS LOGIC  GET SELF:_isPartial SET SELF:_isPartial := VALUE
      PROPERTY IsNested          AS LOGIC  GET SELF:Parent IS XSourceTypeSymbol
      PROPERTY ClassType         AS XSharpDialect AUTO

      STATIC METHOD CreateGlobalType(xfile AS XFile) AS XSourceTypeSymbol
         VAR globalType := XSourceTypeSymbol{XLiterals.GlobalName, Kind.Class, Modifiers.Public+Modifiers.Static, TextRange.Empty, TextInterval.Empty, xfile, NULL}
         globalType:IsPartial:=TRUE
         RETURN globalType

      STATIC METHOD IsGlobalType(type AS IXSymbol) AS LOGIC
         RETURN type != NULL .AND. type is IXTypeSymbol .and. type:Name == XLiterals.GlobalName

      PROPERTY Children   AS IList<IXTypeSymbol>
         GET
            BEGIN LOCK SELF:_children
                var children := List<IXTypeSymbol>{}
                children:AddRange(SELF:_children)
                return children
            END LOCK
         END GET
      END PROPERTY

      PROPERTY XChildren   AS IList<XSourceTypeSymbol>
         GET
            BEGIN LOCK SELF:_children
               return SELF:_children:ToArray()
            END LOCK
         END GET
      END PROPERTY
      PROPERTY XMLSignature   AS STRING GET SELF:GetXmlSignature()



   METHOD ToString() AS STRING
      var result := i"{Kind} {Name}"
      if SELF:_signature != NULL .and. SELF:_signature:TypeParameters:Count > 0
         result += self:_signature:ToString()
      ENDIF
      RETURN result

    STATIC METHOD GetTypeSource(element AS XDbResult, members AS IList<XDbResult>, file as XFile) AS STRING
         VAR sb := System.Text.StringBuilder{}
         if file != NULL
            foreach var strusing in file:Usings
                sb:AppendLine("USING "+strusing)
             next
             foreach var strusing in file:StaticUsings
                sb:AppendLine("USING STATIC "+strusing)
            next
         ENDIF
         if element != null
            var xml    := element:XmlCommentsAsSource
            if !String.IsNullOrEmpty(xml)
                sb:AppendLine(xml)
            endif
            sb:AppendLine(element:SourceCode)
         endif
         FOREACH VAR xmember IN members
            var xml    := xmember:XmlCommentsAsSource
            var source := xmember:SourceCode
            // replace private with hidden to avoid confusion
            if source:ToLower():StartsWith("private")
               source := "HIDDEN "+source:Substring(7)
            endif
            if source:ToLower():StartsWith("public")
               source := "EXPORT "+source:Substring(6)
            endif
            if !String.IsNullOrEmpty(xml)
                sb:AppendLine(xml)
            endif
            sb:AppendLine(source)
            SWITCH xmember:Kind
            CASE Kind.Property
               source := xmember:SourceCode:ToLower():Replace('\t',' ')
               IF source:Contains(" get") .OR. ;
                  source:Contains(" set") .OR. ;
                  source:Contains(" auto") .or. ;
                  source:Contains("=>")
                  // single line
                  NOP
               ELSE
                   sb:AppendLine("END PROPERTY")
               ENDIF
            CASE Kind.Event
               source := xmember:SourceCode:ToLower():Replace('\t',' ')
               IF source:Contains(" add") .OR. ;
                  source:Contains(" remove")
                  // single line
                  NOP
               ELSE
                  sb:AppendLine("END EVENT")
               ENDIF
            END SWITCH
         NEXT
         IF element != NULL
             SWITCH element:Kind
             CASE Kind.Class
                IF element:ClassType == (INT) XSharpDialect.XPP
                   sb:AppendLine("ENDCLASS")
                ELSEIF element:ClassType == (INT) XSharpDialect.FoxPro
                   sb:AppendLine("ENDDEFINE")
                ELSE
                   sb:AppendLine("END CLASS")
                ENDIF
             CASE Kind.Structure
                sb:AppendLine("END STRUCTURE")
             CASE Kind.Interface
                sb:AppendLine("END INTERFACE")
             END SWITCH
         ENDIF
         RETURN sb:ToString()
        METHOD NameEquals(sName AS STRING) AS LOGIC
            var tickedName := SELF:TickedName
            if sName:Contains('.') .and. !String.IsNullOrEmpty(SELF:Namespace)
                // compare full name
                
                IF SELF:FullName:Equals(sName, StringComparison.OrdinalIgnoreCase)
                    RETURN TRUE
                ENDIF
                tickedName := SELF:Namespace+"." + TickedName
                RETURN tickedName:Equals(sName, StringComparison.OrdinalIgnoreCase)
            else
                if SELF:Name:Equals(sName, StringComparison.OrdinalIgnoreCase)
                    RETURN TRUE
                ENDIF
                RETURN tickedName:Equals(sName, StringComparison.OrdinalIgnoreCase)
            ENDIF
        END METHOD

END CLASS

END NAMESPACE

