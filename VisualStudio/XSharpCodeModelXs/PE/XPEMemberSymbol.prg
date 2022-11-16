//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections.Generic
USING System.Diagnostics
USING Mono.Cecil
BEGIN NAMESPACE XSharpModel


   CLASS XPEPropertySymbol INHERIT XPEMemberSymbol
        PRIVATE _propdef        as PropertyDefinition
        PROPERTY Accessors      AS AccessorKind AUTO
        PROPERTY IsSpecialName  AS LOGIC GET _propdef:IsSpecialName

		CONSTRUCTOR(def AS PropertyDefinition, asm AS XAssembly)
			SUPER(def:Name, Kind.Property, Modifiers.Public,  asm)
			SELF:Parent          := NULL
         SELF:DeclaringType   := def:DeclaringType:GetXSharpTypeName()
         SELF:_propdef        := def
			SELF:OriginalTypeName   := RemoveGenericParameters(def:PropertyType:FullName)
         SELF:TypeName        := SELF:Signature:DataType := def:PropertyType:GetXSharpTypeName()
         if def:GetMethod != NULL
            VAR xMethod := XPEMethodSymbol{def:GetMethod, asm}
            SELF:Attributes := xMethod:Attributes
         ELSEIF def:SetMethod != NULL
            VAR xMethod := XPEMethodSymbol{def:SetMethod, asm}
            SELF:Attributes := xMethod:Attributes
         ENDIF
         IF def:HasCustomAttributes
            SELF:_custatts       := _propdef:CustomAttributes
         ENDIF
         IF def:GetMethod != NULL
            SELF:Accessors |= AccessorKind.Get
         ENDIF
         IF def:SetMethod != NULL
            SELF:Accessors |= AccessorKind.Set
         ENDIF
    PROPERTY ClassGenText as STRING
            GET
                var result := SELF:VisibilityKeyword + " "
                result += SELF:ModifiersKeyword + " "
                result += SELF:KindKeyword + " "
                result += SELF:Prototype
                result += IIF(Accessors.HasFlag(AccessorKind.Get), " GET","")
                result += IIF(Accessors.HasFlag(AccessorKind.Set), " SET","")
                RETURN result:Replace("  ", " ")
            END GET
      END PROPERTY

      PROTECTED OVERRIDE METHOD Resolve() AS VOID
         IF ! _resolved
            IF _propdef:HasParameters
               SELF:AddParameters(_propdef:Parameters)
            ENDIF
            SUPER:Resolve()
         ENDIF
         RETURN
   END CLASS

   CLASS XPEFieldSymbol INHERIT XPEMemberSymbol
        PRIVATE _fielddef       as FieldDefinition
        PROPERTY IsSpecialName  AS LOGIC GET _fielddef:IsSpecialName

   STATIC METHOD ConvertAttributes (attributes AS FieldAttributes) as Modifiers
         var modifiers := Modifiers.None
         var visattributes := _AND(attributes, FieldAttributes.FieldAccessMask)
         SWITCH visattributes
         CASE FieldAttributes.Private    // 1
            modifiers :=   Modifiers.Private
         CASE FieldAttributes.Assembly  // 3
            modifiers :=   Modifiers.Internal
         CASE FieldAttributes.Family     // 4
            modifiers :=   Modifiers.Protected
         CASE FieldAttributes.FamORAssem // 5
         CASE FieldAttributes.FamANDAssem // 5
            modifiers :=   Modifiers.ProtectedInternal
         CASE FieldAttributes.Public     // 6
            modifiers :=   Modifiers.Public
         END SWITCH
         IF attributes:HasFlag(FieldAttributes.InitOnly)
            modifiers |= Modifiers.InitOnly
         ENDIF
         IF attributes:HasFlag(FieldAttributes.Literal)
            modifiers |= Modifiers.Const
         ENDIF
         IF attributes:HasFlag(FieldAttributes.Static)
            modifiers |= Modifiers.Static
         ENDIF
          RETURN modifiers

		CONSTRUCTOR(def AS FieldDefinition, asm AS XAssembly)
			SUPER(def:Name, Kind.Field, ConvertAttributes(def:Attributes),  asm)
         SELF:DeclaringType   := def:DeclaringType:GetXSharpTypeName()
         SELF:_fielddef       := def
         SELF:OriginalTypeName := RemoveGenericParameters(def:FieldType:FullName)
         SELF:TypeName        := SELF:Signature:DataType      := def:FieldType:GetXSharpTypeName()
         IF def:IsLiteral .AND. def:Constant != NULL
            SELF:Value := def:Constant:ToString()
         endif
         IF def:HasCustomAttributes
            SELF:_custatts       := def:CustomAttributes
         ENDIF
            IF DeclaringType == asm:GlobalClassName
                if self:Modifiers:HasFlag(Modifiers.Const) .or. self:Modifiers:HasFlag(Modifiers.InitOnly)
                    self:Kind := Kind.VODefine
                else
                    self:Kind := Kind.VOGlobal
                endif
            ENDIF

      PROTECTED OVERRIDE METHOD Resolve() AS VOID
         SUPER:Resolve()
         RETURN

     PROPERTY ClassGenText as STRING
            GET
            if SELF:Kind == Kind.VODefine
                var result := "DEFINE "+ SELF:Prototype
                RETURN result:Replace("  ", " ")
            elseif SELF:Kind == Kind.VOGlobal
                var result := "GLOBAL "+ SELF:Prototype
                RETURN result:Replace("  ", " ")
            else
                    var result := SELF:VisibilityKeyword + " "
                    result += SELF:ModifiersKeyword + " "
                    IF SELF:Kind != Kind.Field
                        result += SELF:KindKeyword + " "
                    ENDIF
                    result += SELF:Prototype
                    RETURN result:Replace("  ", " ")
                 endif
            END GET
      END PROPERTY

   END CLASS

   CLASS XPEEventSymbol INHERIT XPEMemberSymbol
        PRIVATE _eventdef       as EventDefinition
        PROPERTY Accessors      AS AccessorKind AUTO
        PROPERTY IsSpecialName  AS LOGIC GET _eventdef:IsSpecialName
		CONSTRUCTOR(def AS EventDefinition, asm AS XAssembly)
			SUPER(def:Name, Kind.Event, Modifiers.Public,  asm)
         SELF:DeclaringType         := def:DeclaringType:GetXSharpTypeName()
         SELF:_eventdef             := def
         SELF:OriginalTypeName      := RemoveGenericParameters(def:EventType:FullName)
         SELF:TypeName              := SELF:Signature:DataType    := def:EventType:GetXSharpTypeName()
         if def:AddMethod != NULL
            VAR xMethod := XPEMethodSymbol{def:AddMethod, asm}
            SELF:Attributes := xMethod:Attributes
         elseif def:RemoveMethod != null
            VAR xMethod := XPEMethodSymbol{def:RemoveMethod, asm}
            SELF:Attributes := xMethod:Attributes
         endif
         IF def:HasCustomAttributes
            SELF:_custatts       := def:CustomAttributes
         ENDIF
         IF def:AddMethod != NULL
            SELF:Accessors |= AccessorKind.Add
         ENDIF
         IF def:RemoveMethod != NULL
            SELF:Accessors |= AccessorKind.Remove
         ENDIF

      PROTECTED OVERRIDE Method Resolve() AS VOID
         SUPER:Resolve()
         RETURN

        PROPERTY ClassGenText as STRING
            GET
                var result := SELF:VisibilityKeyword + " "
                result += SELF:ModifiersKeyword + " "
                result += SELF:KindKeyword + " "
                result += SELF:Prototype
                result += IIF(Accessors.HasFlag(AccessorKind.Add), " ADD","")
                result += IIF(Accessors.HasFlag(AccessorKind.Remove), " REMOVE","")
                RETURN result:Replace("  ", " ")
            END GET
      END PROPERTY

END CLASS

   CLASS XPEMethodSymbol  INHERIT XPEMemberSymbol
       PRIVATE _methoddef       as MethodDefinition
       PRIVATE _ccAttrib        AS Mono.Cecil.CustomAttribute
       PROPERTY IsSpecialName   AS LOGIC GET _methoddef:IsSpecialName
   STATIC METHOD ConvertAttributes (attributes AS MethodAttributes) as Modifiers
         var modifiers := Modifiers.None
         var visattributes := _AND(attributes, MethodAttributes.MemberAccessMask)
         SWITCH visattributes
         CASE MethodAttributes.Private    // 1
            modifiers :=  Modifiers.Private
         CASE MethodAttributes.Assembly  // 3
            modifiers :=  Modifiers.Internal
         CASE MethodAttributes.Family     // 4
            modifiers :=  Modifiers.Protected
         CASE MethodAttributes.FamORAssem // 5
         CASE MethodAttributes.FamANDAssem // 5
            modifiers :=  Modifiers.ProtectedInternal
         CASE MethodAttributes.Public     // 6
            modifiers :=  Modifiers.Public
         OTHERWISE
            modifiers := Modifiers.Public
         END SWITCH
         IF attributes:HasFlag(MethodAttributes.Abstract)
            modifiers |= Modifiers.Abstract
         ENDIF
         IF attributes:HasFlag(MethodAttributes.Final)
            modifiers |= Modifiers.Sealed
         ENDIF
         IF attributes:HasFlag(MethodAttributes.PInvokeImpl)
            modifiers |= Modifiers.External
         ENDIF
         IF attributes:HasFlag(MethodAttributes.UnmanagedExport)
            modifiers |= Modifiers.External
         ENDIF
         IF attributes:HasFlag(MethodAttributes.Virtual)
            modifiers |= Modifiers.Virtual
         ENDIF
         IF attributes:HasFlag(MethodAttributes.NewSlot)
            modifiers |= Modifiers.New
         ENDIF
         IF attributes:HasFlag(MethodAttributes.Static)
            modifiers |= Modifiers.Static
         ENDIF
         RETURN modifiers

  		 CONSTRUCTOR(def AS MethodDefinition, asm AS XAssembly)
			SUPER(def:Name, Kind.Method, ConvertAttributes(def:Attributes),  asm)
             SELF:DeclaringType   := def:DeclaringType:GetXSharpTypeName()
             IF DeclaringType:EndsWith("Functions")
                SELF:Attributes := _AND(SELF:Attributes, ~Modifiers.Static)
                SELF:Kind      := Kind.Function
             ENDIF
             SELF:OriginalTypeName   := RemoveGenericParameters(def:ReturnType:FullName)
             SELF:TypeName           := SELF:Signature:DataType := def:ReturnType:GetXSharpTypeName()
             SELF:_methoddef         := def
             IF def:HasCustomAttributes
                SELF:_custatts       := def:CustomAttributes
                FOREACH VAR attr IN def:CustomAttributes
                   SWITCH attr:AttributeType:FullName
                   CASE "System.Runtime.CompilerServices.ExtensionAttribute"
                        SELF:Signature:IsExtension := TRUE
                        if asm:Types:ContainsKey(SELF:DeclaringType)
                            SELF:DeclaringTypeSym := asm:Types[SELF:DeclaringType]
                        endif
                   CASE "XSharp.Internal.ClipperCallingConventionAttribute"
                   CASE "Vulcan.Internal.ClipperCallingConventionAttribute"
                      SELF:CallingConvention := CallingConvention.Clipper
                      _ccAttrib := attr
                   END SWITCH
                NEXT
                ENDIF
                IF def:HasGenericParameters
                    SELF:_generic := def:HasGenericParameters
                    SELF:_signature:ReadGenericParameters(def:GenericParameters)
                ENDIF
         PROTECTED OVERRIDE METHOD Resolve() AS VOID
            IF ! _resolved
               // Add Generic parameters first so have that info when processing the parameters
               IF _methoddef:HasGenericParameters
                  SELF:AddTypeParameters(_methoddef:GenericParameters)
               ENDIF
               IF _methoddef:HasParameters
                  IF SELF:CallingConvention = CallingConvention.Clipper
                     SELF:AddParameters(_ccAttrib)
                  ELSE
                     SELF:AddParameters(_methoddef:Parameters)
                  ENDIF
               ENDIF
               SUPER:Resolve()
            ENDIF

            RETURN
        PROPERTY ClassGenText as STRING
            GET
                var result := SELF:VisibilityKeyword + " "
                result += SELF:ModifiersKeyword + " "
                result += SELF:KindKeyword + " "
                var temp := SELF:GetProtoType(TRUE)
                if (SELF:Kind == Kind.Constructor)
                    temp := temp.Replace('}',')')
                    var pos := temp.IndexOf('{')
                    temp := "("+temp:Substring(pos+1)
                    result += temp
                ELSE
                    result += temp
                endif
                if self:CallingConvention == CallingConvention.Clipper
                    result += " CLIPPER"
                endif
                RETURN result:Replace("  ", " ")
            END GET
      END PROPERTY

        METHOD Clone() AS IXMemberSymbol
            SELF:Resolve()
            var  clone := (XPEMethodSymbol) SUPER:Clone()
            clone:_methoddef := NULL
            clone:_resolved := TRUE
            RETURN clone


   END CLASS

   [DebuggerDisplay("{ToString(),nq}")];
	ABSTRACT CLASS XPEMemberSymbol     INHERIT XPESymbol IMPLEMENTS IXMemberSymbol
		// Fields
        PROTECTED  _signature    AS XMemberSignature
        PROTECTED  _resolved    AS LOGIC
        PROTECTED  _generic     AS LOGIC
        PROPERTY  SubType      AS Kind AUTO
        PROPERTY  DeclaringType  AS STRING AUTO
        PROPERTY  DeclaringTypeSym as XPETypeSymbol AUTO
        PROPERTY  Signature     AS XMemberSignature  GET _signature INTERNAL SET _signature := value
        PROPERTY  IsGeneric    AS LOGIC GET _generic
        ABSTRACT PROPERTY  ClassGenText      AS STRING GET

		#region constructors

      CONSTRUCTOR(name AS STRING, kind AS Kind, attributes AS Modifiers, asm AS XAssembly)
			SUPER(name, kind, attributes,  asm)
         SELF:_signature      := XMemberSignature{}
         SELF:_resolved       := FALSE
         SELF:_custatts       := NULL
         SELF:_generic        := FALSE
         RETURN


      #endregion

      PROTECTED VIRTUAL METHOD Resolve() AS VOID
         IF SELF:_custatts != NULL
            FOREACH VAR custatt IN SELF:_custatts
               SWITCH custatt:AttributeType:FullName
               CASE "System.Diagnostics.DebuggerBrowsableAttribute"
                  VAR cvalue := custatt:ConstructorArguments[0]
                  IF cvalue:Type:FullName == typeof(DebuggerBrowsableState):FullName
                     VAR state := (DebuggerBrowsableState) cvalue:Value
                     IF state == DebuggerBrowsableState.Never
                        // hide these
                        SELF:Attributes := _OR(SELF:Modifiers, Modifiers.Internal)
                     ENDIF
                  ENDIF
               END SWITCH
            NEXT
         ENDIF
         SELF:_custatts := NULL
         RETURN

      PRIVATE METHOD DoResolve() AS VOID
         IF ! self:_resolved
            SELF:Resolve()
            self:_resolved := TRUE
         ENDIF

      METHOD AddParameters( oAttr as Mono.Cecil.CustomAttribute) AS VOID
         var arg    := oAttr:ConstructorArguments[0]
         var type   := arg:Type
         if type:FullName == "System.String[]"
            var args := (Mono.Cecil.CustomAttributeArgument[]) arg:Value
            FOREACH VAR argName in args
               var parRef := XPEParameterSymbol{self, argName:Value:ToString(), "USUAL"}
               parRef:ParamType := ParamType.As
               parRef:OriginalTypeName := "XSharp.__Usual"
               SELF:_signature:Parameters:Add(parRef)
            NEXT
         endif


      METHOD AddParameters (aPars as Mono.Collections.Generic.Collection<ParameterDefinition>) AS VOID
         SELF:_signature:Parameters:Clear()
         FOREACH var oPar in aPars
            var name  := oPar:Name
            var index := oPar:Index
            var type  := oPar:ParameterType:GetXSharpTypeName()
            var defValue := oPar:Constant
            var custatt := ""
            LOCAL parType as ParamType
            IF oPar:IsOut .and. oPar:IsIn
               parType := ParamType.Ref
            ELSEIF oPar:IsOut
               parType := ParamType.Out
            ELSEIF oPar:IsIn
               parType := ParamType.As
            ELSE
               parType := ParamType.As
            ENDIF
            if oPar:HasCustomAttributes
               defValue := SELF:DecodeCustomAttributes(oPar:CustomAttributes, oPar:ParameterType)
            ENDIF
            var parRef := XPEParameterSymbol{self, name, type}
            parRef:OriginalTypeName := RemoveGenericParameters(oPar:ParameterType:FullName)
            if defValue != NULL
               parRef:Value := defValue:ToString()
            ENDIF
            IF parRef:OriginalTypeName:Contains("&") .AND. parType == ParamType.As
               parType := ParamType.Ref
            ENDIF
            parRef:ParamType := parType
            IF parRef:OriginalTypeName:Contains("`")
               VAR count := SELF:TypeParameters:Count
               parRef:TypeName := parRef:OriginalTypeName:Replace("`"+count:ToString(),"")
            ENDIF
            SELF:_signature:Parameters:Add(parRef)
         NEXT
         RETURN

      METHOD DecodeCustomAttributes( attributes as Mono.Collections.Generic.Collection<CustomAttribute>, oType as TypeReference) AS STRING
         local result as STRING
         local done   as LOGIC
         FOREACH var attr in attributes
               SWITCH attr:AttributeType:FullName
               CASE "XSharp.Internal.DefaultParameterValueAttribute"
               CASE "Vulcan.Internal.DefaultParameterValueAttribute"
                  var arg1     := attr:ConstructorArguments[0]
                  IF arg1:Value is CustomAttributeArgument VAR arg
                     arg1 := arg
                  ENDIF
                  var arg2 := (Int32) attr:ConstructorArguments[1]:Value
                  switch arg2
                  case 1   // NIL
                     result := "NIL"
                  case 2   // Arg1 is date in ticks
                     var ticks := (Int64)  arg1:Value
                     var dt    := DateTime{ticks}
                     if dt == DateTime.MinValue
                         result := "NULL_DATE"
                     else
                        result    := dt:ToString("yyyy.MM.dd")
                     endif
                  case 3   // Arg1 is Symbol , when NULL then NULL_SYMBOL
                     var sym := (STRING)  arg1:Value
                     if (sym == NULL)
                        result := "NULL_SYMBOL"
                     else
                        result := "#" + sym:ToString()
                     endif
                  case 4   // Arg1 is PSZ, when NULL then NULL_SYMBOL
                     var psz1 := (STRING)  arg1:Value
                     if (psz1 == NULL)
                        result := "NULL_PSZ"
                     else
                        result := psz1:ToString()
                     endif
                  case 5   // Arg1 is NULL_PTR
                     VAR p := IntPtr{ (INT64) arg1:Value}
                     if (p == IntPtr.Zero)
                        result := "NULL_PTR"
                     ELSE
                        result := "0x"+p:ToString("X")
                     ENDIF

                  case 0   // Normal .Net value
                     var obj := arg1:Value
                     if (obj != null)
                        result := obj:ToString()
                        if arg1:Type:FullName == "System.String"
                           result := e"\""+result+e"\""
                        endif
                     else
                        switch oType:FullName
                        case "System.String"
                            return "NULL_STRING"
                        otherwise
                            return "NULL_OBJECT"
                        end switch
                     endif
                  end switch
                  done := TRUE
               END SWITCH
               if (done)
                  EXIT
               ENDIF

         NEXT
         return result


      METHOD AddTypeParameters(aPars AS Mono.Collections.Generic.Collection<GenericParameter>) AS VOID
         SELF:_signature:TypeParameters:Clear()
         FOREACH typeParam AS Mono.Cecil.GenericParameter IN aPars
            SELF:_signature:TypeParameters:Add(typeParam:Name)
         NEXT
         RETURN

//
			#region Properties
      PROPERTY Description AS STRING GET SELF:GetDescription()


	  PROPERTY FullName AS STRING GET SELF:GetFullName()

      PROPERTY IsStatic AS LOGIC GET SELF:Modifiers:HasFlag(Modifiers.Static)
      PROPERTY Location AS STRING GET SELF:Assembly:DisplayName

		PROPERTY HasParameters     AS LOGIC
         GET
            SELF:DoResolve()
            RETURN _signature:HasParameters
         END GET
      END PROPERTY

		PROPERTY ParameterCount    AS INT
         GET
            SELF:DoResolve()
            RETURN _signature:ParameterCount
         END GET
      END PROPERTY

		PROPERTY ParameterList     AS STRING
         GET
            SELF:DoResolve()
            RETURN _signature:ParameterList
         END GET
      END PROPERTY

      PROPERTY TypeParameters as IList<STRING>           GET SELF:_signature:TypeParameters:ToArray()
      PROPERTY TypeParameterList AS STRING               GET SELF:_signature:TypeParameterList
      PROPERTY TypeParameterConstraints as IList<STRING> GET SELF:_signature:TypeParameterContraints:ToArray()
      PROPERTY TypeParameterConstraintsList AS STRING    GET SELF:_signature:TypeParameterConstraintsList


      PROPERTY Parameters         AS IList<IXParameterSymbol>
         GET
            SELF:DoResolve()
            RETURN _signature:Parameters:ToArray()
         END GET
      END PROPERTY

      PROPERTY CallingConvention AS CallingConvention GET _signature:CallingConvention SET _signature:CallingConvention := @@value

      PROPERTY ParentType     AS IXTypeSymbol GET SELF:Parent ASTYPE IXTypeSymbol
		PROPERTY Prototype      AS STRING
         GET
            SELF:DoResolve()
            RETURN SELF:GetProtoType()
         END GET
      END PROPERTY

      PROPERTY IsExtension    AS LOGIC  GET _signature:IsExtension
      PROPERTY XMLSignature   AS STRING
         GET
            SELF:DoResolve()
            RETURN SELF:GetXmlSignature()
         END GET
      END PROPERTY

      METHOD ToString() AS STRING
         var result := i"{Kind} {Name}"
         if SELF:_signature != NULL .and. SELF:_signature:TypeParameters:Count > 0
            result += self:_signature:ToString()
         ENDIF
         RETURN result

     METHOD Clone() AS IXMemberSymbol
        SELF:Resolve()
        var clone := (XPEMemberSymbol) SELF:MemberwiseClone()
        clone:_signature := SELF:_signature:Clone()
        RETURN (IXMemberSymbol) clone

     METHOD WithName(newName as STRING) AS IXMemberSymbol
        var clone := (XPEMemberSymbol) SELF:MemberwiseClone()
        clone:Name := newName
        clone:_signature := SELF:_signature:Clone()
        RETURN (IXMemberSymbol) clone

		#endregion
	END CLASS

END NAMESPACE

