using System.Collections.Generic

BEGIN NAMESPACE XSharpModel
   /// <summary>Properties shared by all objects in the codemodel</summary>
   INTERFACE IXElement
      PROPERTY Name        as STRING GET
      PROPERTY Kind        as Kind   GET
      PROPERTY TypeName    AS STRING GET SET
      PROPERTY Parent      AS IXEntity GET SET
   END INTERFACE
      
   /// <summary>Properties shared by all entities (types and members, internal and external) in the codemodel</summary>
   INTERFACE IXEntity INHERIT IXElement
      PROPERTY Description AS STRING GET
      PROPERTY ModVis      AS STRING GET
      PROPERTY Modifiers   AS Modifiers GET
      PROPERTY Visibility  AS Modifiers GET
      PROPERTY IsStatic    AS LOGIC     GET
      PROPERTY Prototype   AS STRING    GET         
      PROPERTY FileUsings  AS IList<String> GET
      PROPERTY IsArray     AS LOGIC GET
      PROPERTY Namespace   AS STRING GET
      PROPERTY FullName    AS STRING GET
     
      METHOD ForceComplete() AS VOID STRICT
   
   END INTERFACE
   
   
   /// <summary>Properties shared by types (both internal and external) </summary>
   INTERFACE IXType INHERIT IXEntity
      PROPERTY Children    AS IList<IXType> GET
      PROPERTY IsNested    AS LOGIC GET
      PROPERTY Members     AS IList<IXMember> GET
      PROPERTY IsTyped     AS LOGIC  GET 
      PROPERTY BaseType    AS STRING GET
      PROPERTY Interfaces  AS IList<STRING> GET   
      PROPERTY IsGeneric   AS LOGIC GET
      PROPERTY OriginalTypeName  AS STRING GET 
      PROPERTY Location    AS STRING GET
         
      METHOD   AddTypeParameter(name AS STRING) AS VOID
      METHOD   AddConstraints(name AS STRING) AS VOID
      METHOD   GetMembers(elementName AS STRING) AS IList<IXMember>
      METHOD   GetMembers(elementName AS STRING, lExact as LOGIC) AS IList<IXMember>
      PROPERTY TypeParameters  as IList<STRING> GET
      PROPERTY TypeParameterConstraints as IList<STRING> GET
      PROPERTY XMLSignature   AS STRING GET        
         
   END INTERFACE

   /// <summary>Properties shared by members (both internal and external) </summary>
   INTERFACE IXMember INHERIT IXEntity
      PROPERTY IsTyped        AS LOGIC  GET 
      PROPERTY ParentType     AS IXType GET
      PROPERTY ParameterList  as STRING GET
      PROPERTY Value          AS STRING GET
      PROPERTY Parameters     AS IList<IXVariable> GET
      PROPERTY DeclaringType  AS STRING GET  
      PROPERTY IsExtension    AS LOGIC GET
      PROPERTY XMLSignature   AS STRING GET   
      PROPERTY OriginalTypeName  AS STRING GET
      PROPERTY Location    AS STRING GET
   END INTERFACE
   
   /// <summary>Properties shared by variables (locals, parameters, both internal and external) </summary>
   INTERFACE IXVariable   INHERIT IXElement
      PROPERTY IsParameter    AS LOGIC GET
      PROPERTY IsArray        AS LOGIC GET
      PROPERTY IsTyped        AS LOGIC  GET 
      PROPERTY ParamType      AS ParamType  GET   
      PROPERTY ShortTypeName  AS STRING GET
      PROPERTY ParamTypeDesc  AS STRING GET
      PROPERTY Value          AS STRING GET
         
   END INTERFACE
END NAMESPACE   