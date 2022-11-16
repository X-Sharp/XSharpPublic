using System.Collections.Generic

BEGIN NAMESPACE XSharpModel
   /// <summary>Properties shared by all objects in the codemodel</summary>
   INTERFACE IXSymbol
      PROPERTY Name         as STRING GET
      PROPERTY Kind         as Kind   GET
      PROPERTY KindKeyword  AS STRING GET
      PROPERTY TypeName     AS STRING GET SET
      PROPERTY Parent       AS IXSymbol GET SET
      PROPERTY IsPublic     AS LOGIC GET
      PROPERTY IsExternalVisible AS LOGIC GET
      PROPERTY Description  AS STRING GET
      PROPERTY ModVis       AS STRING GET
      PROPERTY Modifiers    AS Modifiers GET
      PROPERTY ModifiersKeyword as STRING GET
      PROPERTY Visibility   AS Modifiers GET
      PROPERTY VisibilityKeyword as STRING GET
      PROPERTY IsStatic     AS LOGIC     GET
      PROPERTY Prototype    AS STRING    GET
      PROPERTY IsArray      AS LOGIC GET
      PROPERTY ElementType  AS STRING GET
      PROPERTY Namespace    AS STRING GET
      PROPERTY FullName     AS STRING GET
      PROPERTY ResolvedType AS IXTypeSymbol AUTO
      PROPERTY Location     AS STRING GET

      METHOD ForceComplete() AS VOID STRICT

   END INTERFACE


   /// <summary>Properties shared by types (both internal and external) </summary>
   INTERFACE IXTypeSymbol INHERIT IXSymbol
      PROPERTY ShortName   AS STRING GET
      PROPERTY Children    AS IList<IXTypeSymbol> GET
      PROPERTY IsNested    AS LOGIC GET
      PROPERTY Members     AS IList<IXMemberSymbol> GET
      PROPERTY AllMembers  AS IList<IXMemberSymbol> GET
      PROPERTY IsTyped     AS LOGIC  GET
      PROPERTY BaseType    AS IXTypeSymbol GET
      PROPERTY BaseTypeName AS STRING GET
      PROPERTY GenericName AS STRING GET SET
      PROPERTY TickedName  AS STRING GET
      PROPERTY Interfaces  AS IList<STRING> GET
      PROPERTY IsGeneric   AS LOGIC GET
      PROPERTY OriginalTypeName  AS STRING GET
      PROPERTY IsFunctionsClass as LOGIC GET

      METHOD   AddTypeParameter(name AS STRING) AS VOID
      METHOD   AddConstraints(name AS STRING) AS VOID
      METHOD   GetMembers(elementName AS STRING) AS IList<IXMemberSymbol>
      METHOD   GetMembers(elementName AS STRING, lExact as LOGIC) AS IList<IXMemberSymbol>
      PROPERTY TypeParameters  as IList<STRING> GET
      PROPERTY TypeParameterList AS STRING GET
      PROPERTY TypeParameterConstraints as IList<STRING> GET
      PROPERTY XMLSignature   AS STRING GET

   END INTERFACE

   /// <summary>Properties shared by members (both internal and external) </summary>
   INTERFACE IXMemberSymbol INHERIT IXSymbol
      PROPERTY IsTyped        AS LOGIC  GET
      PROPERTY ParentType     AS IXTypeSymbol GET
      PROPERTY ParameterList  as STRING GET
      PROPERTY Value          AS STRING GET
      PROPERTY Parameters     AS IList<IXParameterSymbol> GET
      PROPERTY TypeParameters AS IList<STRING> GET
      PROPERTY DeclaringType  AS STRING GET
      PROPERTY IsExtension    AS LOGIC GET
      PROPERTY XMLSignature   AS STRING GET
      PROPERTY OriginalTypeName  AS STRING GET
      PROPERTY IsGeneric      AS LOGIC GET
      PROPERTY CallingConvention AS CallingConvention GET
      METHOD Clone()          AS IXMemberSymbol
      METHOD WithName(newName as STRING) AS IXMemberSymbol

     // METHOD WithGenericArgs(args as List<String>) AS IXMemberSymbol
   END INTERFACE

   /// <summary>Properties shared by variables (locals, parameters, both internal and external) </summary>
   INTERFACE IXVariableSymbol   INHERIT IXSymbol
      PROPERTY IsParameter    AS LOGIC GET
      PROPERTY IsTyped        AS LOGIC  GET
      PROPERTY ShortTypeName  AS STRING GET
      PROPERTY Value          AS STRING GET
      METHOD Clone() AS IXVariableSymbol

      END INTERFACE

    INTERFACE IXParameterSymbol   INHERIT IXVariableSymbol
      PROPERTY ParamType      AS ParamType  GET SET
      PROPERTY ParamTypeDesc  AS STRING GET
    END INTERFACE


   INTERFACE IXSourceSymbol
      PROPERTY File AS XFile               GET
      PROPERTY Range AS TextRange          GET
      PROPERTY Interval AS TextInterval    GET
    END INTERFACE
    INTERFACE IXSourceEntity
        PROPERTY SourceCode AS STRING    GET
    END INTERFACE





END NAMESPACE
