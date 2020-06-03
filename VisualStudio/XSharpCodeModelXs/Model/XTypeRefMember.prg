//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections.Generic
USING System.Diagnostics
USING Mono.Cecil
BEGIN NAMESPACE XSharpModel

	[DebuggerDisplay("{Kind}, {Name,nq}")];
	CLASS XTypeRefMember       INHERIT XRefElement IMPLEMENTS IXTypeMember
		// Fields
        PRIVATE _signature    AS XMemberSignature 
        PRIVATE _methoddef    as MethodDefinition
        PRIVATE _fielddef     as FieldDefinition
        PRIVATE _propdef      as PropertyDefinition
        PRIVATE _eventdef     as EventDefinition
        PROPERTY SubType      AS Kind AUTO
        PROPERTY DeclaringType  AS STRING AUTO            

		#region constructors
		
  		CONSTRUCTOR(def AS MethodDefinition, asm as XAssembly)
			SUPER(def:Name, Kind.Method, def:Attributes,  asm)
			SELF:Parent          := NULL
         SELF:DeclaringType   := def:DeclaringType:FullName
         IF def:DeclaringType:FullName:EndsWith("Functions")
            SELF:Modifiers := _AND(SELF:Modifiers, ~Modifiers.Static)
            SELF:Kind      := Kind.Function
         ENDIF
         SELF:_signature      := XMemberSignature{}
			SELF:_signature:DataType := def:ReturnType:FullName
         SELF:_methoddef      := def
         
         IF def:HasCustomAttributes
            FOREACH var attr in def:CustomAttributes
               IF attr:AttributeType:FullName == "System.Runtime.CompilerServices.ExtensionAttribute"
                  SELF:_signature:IsExtension := TRUE
                  EXIT
               ENDIF
            NEXT
         ENDIF
      
		CONSTRUCTOR(def AS PropertyDefinition, asm as XAssembly)
			SUPER(def:Name, Kind.Property, def:Attributes,  asm)
			SELF:Parent          := NULL
         SELF:DeclaringType   := def:DeclaringType:FullName
         SELF:_signature      := XMemberSignature{}
         SELF:_propdef        := def
			SELF:_signature:DataType := def:PropertyType:FullName

     
		CONSTRUCTOR(def AS FieldDefinition, asm as XAssembly)
			SUPER(def:Name, Kind.Field, def:Attributes,  asm)
			SELF:Parent          := NULL
         SELF:DeclaringType   := def:DeclaringType:FullName
         SELF:_signature      := XMemberSignature{}
         SELF:_fielddef       := def
			SELF:_signature:DataType      := def:FieldType:FullName

		CONSTRUCTOR(def AS EventDefinition, asm as XAssembly)
			SUPER(def:Name, Kind.Event, (FieldAttributes) 0,  asm)
			SELF:Parent          := NULL
         SELF:DeclaringType   := def:DeclaringType:FullName
         SELF:_signature      := XMemberSignature{}
         SELF:_eventdef       := def
			SELF:_signature:DataType      := def:EventType:FullName


      #endregion


      METHOD AddParameters( list AS IList<XVariable>) AS VOID
         IF list != NULL
            FOREACH VAR par IN list
               SELF:AddParameter(par)
            NEXT
         ENDIF
         RETURN
            
		METHOD AddParameter(oVar AS XVariable) AS VOID
			oVar:Parent := SELF
			_signature:Parameters:Add(oVar)
			RETURN


			//
			#region Properties
      PROPERTY Description AS STRING GET SELF:GetDescription()

		
		PROPERTY FullName AS STRING GET SELF:GetFullName()
		
		PROPERTY HasParameters     AS LOGIC GET _signature:HasParameters
		PROPERTY ParameterCount    AS INT   GET _signature:ParameterCount

		PROPERTY ParameterList     AS STRING GET _signature:ParameterList
		
      PROPERTY ComboParameterList AS STRING	GET _signature:ComboParameterList
      PROPERTY Parameters         AS IList<IXVariable> GET _signature:Parameters:ToArray()

      PROPERTY Signature         AS XMemberSignature  GET _signature SET _signature := @@value
      PROPERTY CallingConvention AS CallingConvention GET _signature:CallingConvention SET _signature:CallingConvention := @@value

      PROPERTY ParentType     AS IXType   GET SELF:Parent ASTYPE IXType		
		PROPERTY Prototype AS STRING GET SELF:GetProtoType()
		
		PROPERTY ComboPrototype AS STRING GET SELF:GetComboProtoType()
      PROPERTY IsExtension    AS LOGIC    GET _signature:IsExtension
      
		#endregion
	END CLASS
	
END NAMESPACE

