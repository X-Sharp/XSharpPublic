//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections.Generic
USING System.Diagnostics
USING XSharpModel
USING LanguageService.CodeAnalysis.XSharp
USING LanguageService.SyntaxTree
USING LanguageService.CodeAnalysis.XSharp.SyntaxParser

BEGIN NAMESPACE XSharpModel

   [DebuggerDisplay("{ToString(),nq}")];
   CLASS XMemberDefinition INHERIT XEntityDefinition IMPLEMENTS IXMember
      // Fields
      PRIVATE _signature    AS XMemberSignature 
      PROPERTY InitExit     AS STRING AUTO
      PROPERTY SubType      AS Kind AUTO
      PROPERTY DeclaringType  AS STRING AUTO
      PROPERTY ReturnType   AS STRING GET TypeName SET TypeName := value
      PROPERTY SourceCode   AS STRING AUTO      
      #region constructors
      
      CONSTRUCTOR(name AS STRING, kind AS Kind, attributes AS Modifiers, ;
            span AS TextRange, position AS TextInterval, returnType AS STRING, isStatic := FALSE AS LOGIC)
         SUPER(name, kind, attributes, span, position)
         SELF:Parent       := NULL
         SELF:ReturnType   := returnType
         SELF:IsStatic     := isStatic
         SELF:_signature   := XMemberSignature{}
         
      CONSTRUCTOR(sig as XMemberSignature, kind AS Kind, attributes AS Modifiers,  ;
            span AS TextRange, position AS TextInterval, isStatic := FALSE AS LOGIC)
         SUPER(sig:Id, kind, attributes, span, position)
         SELF:Parent       := NULL
         SELF:ReturnType   := sig:DataType
         SELF:IsStatic     := isStatic
         SELF:_signature   := sig
         FOREACH var par in sig:Parameters
            par:Parent := SELF
         NEXT         
         #endregion
      
      
      METHOD AddParameters( list AS IList<IXVariable>) AS VOID
         IF list != NULL
            FOREACH VAR par IN list
               SELF:AddParameter(par)
            NEXT
         ENDIF
         RETURN
         
      METHOD AddParameter(oVar AS IXVariable) AS VOID
         oVar:Parent := SELF
         IF oVar IS XVariable VAR xVar
            xVar:File   := SELF:File
         ENDIF
         _signature:Parameters:Add(oVar)
         oVar:Parent := SELF
         RETURN
         
         
      #region Properties. Some are implemented as Extension methods, others forwarded to the signature
      PROPERTY Description AS STRING GET SELF:GetDescription()
 		PROPERTY FullName AS STRING GET SELF:GetFullName()
      
      PROPERTY HasParameters     AS LOGIC GET _signature:HasParameters
      PROPERTY ParameterCount    AS INT   GET _signature:ParameterCount
      
      PROPERTY ParameterList      AS STRING GET _signature:ParameterList
      
      PROPERTY ComboParameterList AS STRING	GET _signature:ComboParameterList
      PROPERTY Parameters         AS IList<IXVariable> GET _signature:Parameters:ToArray()
      
      PROPERTY Signature         AS XMemberSignature  GET _signature SET _signature := @@value
      PROPERTY CallingConvention AS CallingConvention GET _signature:CallingConvention SET _signature:CallingConvention := @@value
      
      
      PROPERTY Prototype      AS STRING GET SELF:GetProtoType()
      
      PROPERTY ComboPrototype AS STRING GET SELF:GetComboProtoType()
         
      PROPERTY ParentType     AS IXType   GET SELF:Parent ASTYPE IXType
      PROPERTY IsExtension    AS LOGIC    GET _signature:IsExtension
      PROPERTY XMLSignature   AS STRING GET SELF:GetXmlSignature()
      PROPERTY OriginalTypeName  AS STRING               GET SELF:TypeName
      PROPERTY TypeParameters as IList<STRING>           GET SELF:_signature:TypeParameters:ToArray()
      PROPERTY TypeParametersList AS STRING              GET SELF:_signature:TypeParametersList
      PROPERTY TypeParameterConstraints as IList<STRING> GET SELF:_signature:TypeParameterContraints:ToArray()
      PROPERTY TypeParameterConstraintsList AS STRING    GET SELF:_signature:TypeParameterConstraintsList
         
      METHOD ToString() AS STRING
         var result := i"{Kind} {Name}"
         if SELF:_signature != NULL .and. SELF:_signature:TypeParameters:Count > 0
            result += self:_signature:ToString()
         ENDIF
         RETURN result


      /*
	         stmt  	:=  "Create Table Members ("
	         stmt	   +=  " Id integer NOT NULL PRIMARY KEY, IdType integer NOT NULL , IdFile integer NOT NULL, "
	         stmt	   +=  " Name text COLLATE NOCASE, Kind integer , Attributes integer , "
	         stmt	   +=  " Value text, ReturnType text, StartLine integer , StartColumn integer ,  "
	         stmt	   +=  " EndLine integer , EndColumn integer , Start integer , Stop integer , "
	         stmt	   +=  " FOREIGN KEY (idType) REFERENCES Types (Id) ON DELETE CASCADE ON UPDATE CASCADE, " 
	         stmt     +=  " FOREIGN KEY (idFile) REFERENCES Files (Id) ON DELETE CASCADE ON UPDATE CASCADE"
	         stmt	   += ")"

            stmt := "Create View TypeMembers as Select m.*, t.Name as TypeName, t.Namespace, t.BaseTypeName from members m join Types t on m.IdType = t.Id"

            stmt := "Create View ProjectMembers as Select m.*, p.IdProject from TypeMembers m join ProjectFiles p on m.IdFile = p.IdFile"

      */


      STATIC PROPERTY DbSelectClause as STRING GET " SELECT Id, IdType, IdFile, Name, Kind, Attributes, Value, ReturnType,  "+;
                                                   " StartLine, StartColumn, EndLine, EndColumn, Start, Stop " + ;
                                                   " FROM ProjectMembers WHERE %whereclause%"

      STATIC METHOD FromDb(aValues as object[]) AS XMemberDefinition
//         var name       := XDatabase.DbToString(aValues[4])
//         var kind       := (Kind)      XDatabase.DbToInt(aValues[5])
//         var attributes := (Modifiers) XDatabase.DbToInt(aValues[6])
//         var value      := XDatabase.DbToString(aValues[7])    // default value for fields
//         var returntype := XDatabase.DbToString(aValues[8])
//         var span       := TextRange{ XDatabase.DbToInt(aValues[9]), XDatabase.DbToInt(aValues[10]),XDatabase.DbToInt(aValues[11]),XDatabase.DbToInt(aValues[12])} 
//         var position   := TextInterval{XDatabase.DbToInt(aValues[13]),XDatabase.DbToInt(aValues[14])}
//         var xmember    := XMemberDefinition{name, kind, attributes, span, position, returntype, kind== Kind.Function .or. kind == Kind.Procedure}
//         xmember:Id     := XDatabase.DbToInt(aValues[1])
//         RETURN xmember
         RETURN NULL

      #endregion
   END CLASS
   
END NAMESPACE

