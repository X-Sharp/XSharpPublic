using System.Collections.Generic

BEGIN NAMESPACE XSharpModel
   INTERFACE IXElement
      PROPERTY Name        as STRING GET
      PROPERTY FullName    AS STRING GET
      PROPERTY Kind        as Kind   GET
      PROPERTY Description AS STRING GET
      PROPERTY Parent      AS IXElement GET SET
      PROPERTY NameSpace   AS STRING GET
      PROPERTY IsTyped     AS LOGIC  GET 
      PROPERTY ModVis      AS STRING GET
      PROPERTY Modifiers   AS Modifiers GET
      PROPERTY Visibility  AS Modifiers GET
      PROPERTY IsStatic    AS LOGIC     GET
      PROPERTY Prototype   AS STRING    GET         
      PROPERTY FileUsings  AS IList<String> GET
      PROPERTY TypeName    AS STRING GET
         
      METHOD ForceComplete() AS VOID STRICT
   
   END INTERFACE
   
   
   INTERFACE IXType INHERIT IXElement
      PROPERTY Children    AS IList<IXType> GET
      PROPERTY IsNested    AS LOGIC GET
      PROPERTY Members     AS IList<IXTypeMember> GET
      
      PROPERTY BaseType    AS STRING GET
      PROPERTY Interfaces  AS IList<STRING> GET   
      PROPERTY IsGeneric   AS LOGIC GET
      METHOD   AddInterface(sInterface AS STRING) AS VOID
      METHOD   AddTypeParameter(name AS STRING) AS VOID
      METHOD   AddConstraints(name AS STRING) AS VOID
      
      PROPERTY TypeParameters  as IList<STRING> GET
      PROPERTY TypeParameterConstraints as IList<STRING> GET
        
         
   END INTERFACE

   INTERFACE IXTypeMember INHERIT IXElement
      PROPERTY ParentType     AS IXType GET
      PROPERTY ParameterList  as STRING GET
      PROPERTY Value          AS STRING GET
      PROPERTY ComboParameterList AS STRING GET
      PROPERTY Parameters     AS IList<IXVariable> GET
      PROPERTY DeclaringType AS STRING GET  
      PROPERTY IsExtension   AS LOGIC GET
         
   END INTERFACE
   
   INTERFACE IXVariable   INHERIT IXElement
      PROPERTY IsParameter    AS LOGIC GET
      PROPERTY ParamType      AS ParamType  GET   
      PROPERTY ShortTypeName  AS STRING GET
      PROPERTY ParamTypeDesc  AS STRING GET
      PROPERTY IsArray        AS LOGIC GET
         
   END INTERFACE
END NAMESPACE   