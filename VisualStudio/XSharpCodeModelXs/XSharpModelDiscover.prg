using System.Collections.Generic.

USING LanguageService.CodeAnalysis.XSharp.SyntaxParser
BEGIN NAMESPACE XSharpModel
    INTERNAL CLASS XSharpModelDiscover INHERIT XSharpBaseListener
        // Fields
        INITONLY PRIVATE _classVars AS List<XTypeMember>
        PROTECTED _currentMethod AS XTypeMember
        INITONLY PRIVATE _currentNSpaces AS Stack<XType>
        INITONLY PRIVATE _currentTypes AS Stack<XType>
        PRIVATE _currentVarStatic AS Logic
        PRIVATE _currentVarVisibility AS Modifiers
        PRIVATE _defaultNS AS string
        PRIVATE _errors AS IEnumerable<XError>
        INITONLY PROTECTED _file AS XFile
        INITONLY PRIVATE _globalType AS XType
        INITONLY PRIVATE _options AS XSharpParseOptions
        INITONLY PROTECTED _staticusings AS List<string>
        INITONLY PROTECTED _types AS Dictionary<string, XType>
        INITONLY PROTECTED _usings AS List<string>
        PRIVATE _xSource AS XSharpParser.SourceContext

        // Methods
         CONSTRUCTOR(@@file AS XFile, ctx AS XSharpParser.SourceContext, errors AS IEnumerable<XError>)
			SUPER()
            //
            SELF:_file := @@file
            SELF:_xSource := ctx
            SELF:_errors := errors
            SELF:_currentTypes := Stack<XType>{}
            SELF:_currentNSpaces := Stack<XType>{}
            SELF:_options := @@file:Project:ProjectNode:ParseOptions
            SELF:_types := Dictionary<string, XType>{}
            SELF:_usings := List<string>{}
            SELF:_staticusings := List<string>{}
            SELF:_globalType := XType.CreateGlobalType(SELF:_file)
            SELF:_classVars := List<XTypeMember>{}
            SELF:_types:Add(SELF:_globalType:Name, SELF:_globalType)
            IF SELF:_file != null .AND. SELF:_file:Project != null
				// this will fail if the project file is already unloaded
				IF SELF:_file:Project:Loaded 
					IF SELF:_file:Project:ProjectNode:PrefixClassesWithDefaultNamespace)
            	    //
		                SELF:_defaultNS := SELF:_file:Project:ProjectNode:RootNameSpace
					ENDIF
	            ENDIF
			ENDIF
        PRIVATE METHOD addGlobalMember(oMember AS XTypeMember) AS void
            //
            oMember:@@File := SELF:_file
            oMember:Parent := SELF:_globalType
            SELF:_globalType:AddMember(oMember)
            SELF:_currentMethod := oMember

        PRIVATE METHOD addMember(oMember AS XTypeMember) AS void
            LOCAL currentType AS XType
            //
            currentType := SELF:currentType
            oMember:@@File := SELF:_file
            IF (currentType != null)
                //
                oMember:Parent := currentType
                currentType:AddMember(oMember)
                SELF:_currentMethod := oMember
            ENDIF
            IF (ModelWalker.IsSuspended .AND. System.Threading.Thread.CurrentThread:IsBackground)
                //
                System.Threading.Thread.Sleep(100)
            ENDIF

        PRIVATE METHOD addParameters(ctxtParams AS IList<XSharpParser.ParameterContext>, newMethod AS XTypeMember) AS void
            LOCAL item AS XVariable
            //
            IF (ctxtParams != null)
                //
                FOREACH context AS XSharpParser.ParameterContext IN ctxtParams
                    //
                    item := XVariable{newMethod, context:Id:GetText(), Kind.@@Parameter, Modifiers.@@Public, TextRange{context}, TextInterval{context}, context:Type:GetText(), TRUE} 
					item:File:=SELF:_file
                    newMethod:@@Parameters:Add(item)
                NEXT
            ENDIF

        PRIVATE METHOD addType(newType AS XType) AS XType
            IF (! SELF:_types:ContainsKey(newType:FullName))
                SELF:_types:Add(newType:FullName, newType)
            ENDIF
            RETURN newType

        PRIVATE METHOD addUsing(name AS string, staticUsing AS Logic) AS void
            LOCAL list AS List<string>
            IF (staticUsing)
               list := SELF:_staticusings
            ELSE
                list := SELF:_usings
            ENDIF
            FOREACH str AS string IN list
                IF (String.Compare(str, name, TRUE) == 0)
                    RETURN
                ENDIF
            NEXT
            list:Add(name)

        PROTECTED VIRTUAL METHOD addVariables( context AS ParserRuleContext) AS void


        PROTECTED METHOD buildDataType(context AS XSharpParser.DatatypeContext) AS XSharpModelDiscover.XCodeTypeReference
            LOCAL typeName AS XSharpParser.TypeNameContext
            LOCAL context3 AS XSharpParser.PtrDatatypeContext
            LOCAL context4 AS XSharpParser.ArrayDatatypeContext
            LOCAL context5 AS XSharpParser.SimpleDatatypeContext
            LOCAL context6 AS XSharpParser.NullableDatatypeContext
            LOCAL reference AS XSharpModelDiscover.XCodeTypeReference
            //
            typeName := null
            IF ((context IS XSharpParser.PtrDatatypeContext))
                //
                context3 := (XSharpParser.PtrDatatypeContext)context 
                typeName := context3:TypeName
            ELSEIF ((context IS XSharpParser.ArrayDatatypeContext))
                    //
                    context4 := (XSharpParser.ArrayDatatypeContext)context 
                    typeName := context4:TypeName
			ELSEIF ((context IS XSharpParser.SimpleDatatypeContext))
                        //
                        context5 := (XSharpParser.SimpleDatatypeContext)context 
                        typeName := context5:TypeName
            ELSEIF ((context IS XSharpParser.NullableDatatypeContext))
                            //
                            context6 := (XSharpParser.NullableDatatypeContext)(context)
                            typeName := context6:TypeName
            reference := null
            IF (typeName:NativeType != null)
                //
                RETURN SELF:buildNativeType(typeName:NativeType)
            ENDIF
            IF (typeName:XType != null)
                //
                RETURN SELF:buildXBaseType(typeName:XType)
            ENDIF
            IF (typeName:Name != null)
                //
                reference := SELF:buildName(typeName:Name)
            ENDIF
            RETURN reference

        METHOD buildLiteralValue(context as XSharpParser.LiteralValueContext ) as string
            var value := ""
            switch context:Token:Type
                case XSharpParser.BIN_CONST
                case XSharpParser.HEX_CONST
                case XSharpParser.INT_CONST
                    value := "Int"
                case XSharpParser.REAL_CONST
                    value :=  "Double"
                case XSharpParser.TRUE_CONST
                case XSharpParser.FALSE_CONST
                    value :=  "Logic"
                case XSharpParser.STRING_CONST
                case XSharpParser.ESCAPED_STRING_CONST
                    value :=  "String"
                case XSharpParser.CHAR_CONST
                case XSharpParser.NULL
                case XSharpParser.NIL
                case XSharpParser.NULL_ARRAY
                case XSharpParser.NULL_CODEBLOCK
                case XSharpParser.NULL_DATE
                case XSharpParser.NULL_OBJECT
                case XSharpParser.NULL_PSZ
                case XSharpParser.NULL_PTR
                case XSharpParser.NULL_STRING
                case XSharpParser.NULL_SYMBOL
                case XSharpParser.SYMBOL_CONST
                case XSharpParser.DATE_CONST
                otherwise
						NOP
				END SWITCH
            return value;


        PROTECTED METHOD buildName(context AS XSharpParser.NameContext) AS XSharpModelDiscover.XCodeTypeReference
            LOCAL reference AS XSharpModelDiscover.XCodeTypeReference
            LOCAL text AS string
            LOCAL context2 AS XSharpParser.QualifiedNameContext
            LOCAL context3 AS XSharpParser.SimpleOrAliasedNameContext
            LOCAL name AS XSharpParser.AliasedNameContext
            LOCAL context5 AS XSharpParser.AliasQualifiedNameContext
            LOCAL context6 AS XSharpParser.GlobalQualifiedNameContext
            LOCAL context7 AS XSharpParser.IdentifierOrGenericNameContext
            //
            reference := null
            text := context:GetText()
            IF ((context IS XSharpParser.QualifiedNameContext))
                //
                context2 := (XSharpParser.QualifiedNameContext)context 
                reference := SELF:buildName(context2:Left)
                RETURN SELF:buildTypeReference(String.Concat(reference:TypeName, ".", SELF:buildSimpleName(context2:Right):TypeName))
            ENDIF
            IF ((context IS XSharpParser.SimpleOrAliasedNameContext))
                //
                context3 := (XSharpParser.SimpleOrAliasedNameContext)(context)
                name := context3:Name
                IF ((name IS XSharpParser.AliasQualifiedNameContext))
                    //
                    context5 := (XSharpParser.AliasQualifiedNameContext)name 
                    reference := SELF:buildSimpleName(context5:Right)
                    RETURN SELF:buildTypeReference(String.Concat(context5:Alias:GetText(), "::", reference:TypeName))
                ENDIF
                IF ((name IS XSharpParser.GlobalQualifiedNameContext))
                    //
                    context6 := (XSharpParser.GlobalQualifiedNameContext)(name)
                    reference := SELF:buildSimpleName(context6:Right)
                    RETURN SELF:buildTypeReference(String.Concat("global::", reference:TypeName))
                ENDIF
                IF ((name IS XSharpParser.IdentifierOrGenericNameContext))
                    //
                    context7 := (XSharpParser.IdentifierOrGenericNameContext)(name)
                    reference := SELF:buildSimpleName(context7:Name)
                ENDIF
            ENDIF
            RETURN reference

        PROTECTED METHOD buildNativeType(nativeType AS XSharpParser.NativeTypeContext) AS XSharpModelDiscover.XCodeTypeReference
            LOCAL @@type AS System.Type
            LOCAL text AS string
            //
            SWITCH nativeType:Token:@@Type
            CASE 0x9b
                //
                @@type := typeof(Byte)
                EXIT

            CASE 0x9e
                //
                @@type := typeof(DWord)
                EXIT

            CASE  ( 160 .OR. 0xa2 ) 
                //
                @@type := typeof(Long)
                EXIT

            CASE 0xa1
                //
                @@type := typeof(Logic)
                EXIT

            CASE 0xa3
                //
                @@type := typeof(Object)
                EXIT

            CASE 0xa6
                //
                @@type := typeof(real4)
                EXIT

            CASE 0xa7
                //
                @@type := typeof(real8)
                EXIT

            CASE 0xa9
                //
                @@type := typeof(Short)
                EXIT

            CASE 170
                //
                @@type := typeof(string)
                EXIT

            CASE 0xad
                //
                @@type := typeof(void)
                EXIT

            CASE 0xae
                //
                @@type := typeof(Word)
                EXIT

            CASE 0xb0
                //
                @@type := typeof(Int64)
                EXIT

            CASE 0xb1
                //
                @@type := typeof(UInt64)
                EXIT

            CASE 0xb2
                //
                @@type := typeof(Object)
                EXIT

            OTHERWISE
                //
                text := nativeType:Token:Text
                RETURN SELF:buildTypeReference(text)
            END SWITCH
            RETURN XSharpModelDiscover.XCodeTypeReference{@@type}

        PROTECTED METHOD buildSimpleName(simpleName AS XSharpParser.SimpleNameContext) AS XSharpModelDiscover.XCodeTypeReference
            LOCAL text AS string
            LOCAL str2 AS string
            LOCAL str3 AS string
            LOCAL num AS Long
            LOCAL reference2 AS XSharpModelDiscover.XCodeTypeReference
            //
            text := simpleName:Id:GetText()
            str2 := ""
            IF (simpleName:GenericArgList != null)
                //
                str3 := ""
                num := 0
                FOREACH context AS XSharpParser.DatatypeContext IN simpleName:GenericArgList:_GenericArgs
                    //
                    IF (num > 0)
                        //
                        str3 := String.Concat(str3, ",")
                    ENDIF
                    reference2 := SELF:buildDataType(context)
                    str3 := String.Concat(str3, reference2:TypeName)
                    num++
                NEXT
                str2 := String.Concat("<", str3, ">")
            ENDIF
            RETURN SELF:buildTypeReference(String.Concat(text, str2))

        PROTECTED METHOD buildTypeReference(name AS string) AS XSharpModelDiscover.XCodeTypeReference
            //
            RETURN XSharpModelDiscover.XCodeTypeReference{name}

        PROTECTED METHOD buildXBaseType(xbaseType AS XSharpParser.XbaseTypeContext) AS XSharpModelDiscover.XCodeTypeReference
            //
            RETURN XSharpModelDiscover.XCodeTypeReference{xbaseType:Token:Text}

        PRIVATE METHOD decodeModifiers(tokens AS IList<IToken>) AS Modifiers
            LOCAL none AS Modifiers
            //
            none := Modifiers.None
            IF (tokens != null)
                //
                FOREACH token AS IToken IN tokens
                    //
                    SWITCH token:@@Type
                    CASE 0x52
                        //
                        none := (none | Modifiers.Abstract)
                        EXIT

                    CASE 0x67
                        //
                        none := (none | Modifiers.New)
                        EXIT

                    CASE 110
                        //
                        none := (none | Modifiers.Sealed)
                        EXIT

                    CASE 0x93
                        //
                        none := (none | Modifiers.Unsafe)
                        EXIT

                    END SWITCH
                NEXT
            ENDIF
            RETURN none

        PRIVATE METHOD decodeNamespace(nameDot AS XSharpParser.NameDotContext, currentNamespace AS string) AS string
            LOCAL text AS string
            //
            text := currentNamespace
            IF (nameDot != null)
                //
                text := nameDot:GetText()
                text := text:Substring(0, (text:Length - 1))
            ENDIF
            RETURN text

        PRIVATE METHOD decodeVisibility(tokens AS IList<IToken>) AS Modifiers
            LOCAL public AS Modifiers
            LOCAL flag2 AS Logic
            LOCAL flag3 AS Logic
            LOCAL @@type AS Long
            //
            public := Modifiers.@@Public
            IF (tokens != null)
                //
                flag2 := FALSE
                flag3 := FALSE
                FOREACH token AS IToken IN tokens
                    //
                    @@type := token:@@Type
                    IF (@@type <= 0x24)
                        //
                        SWITCH @@type
                        CASE 0x1c
                            //
                            goto Label_0097
                        CASE 0x24
                            //
                            goto Label_0093
                        END SWITCH
                    ELSE
                        //
                        SWITCH @@type
                        CASE 0x38
                            //
                            goto Label_0093
                        CASE  ( 0x39 .OR. 0x3a ) 
                            //
                            LOOP

                        CASE 0x3b
                            //
                            flag2 := TRUE
                            IF (! flag3)
                                //
                                goto Label_008E
                            ENDIF
                            public := Modifiers.ProtectedInternal
                            LOOP

                        CASE 60
                            //
                            goto Label_0097
                        END SWITCH
                        IF (@@type == 100)
                            //
                            flag3 := TRUE
                            IF (flag2)
                                //
                                public := Modifiers.ProtectedInternal
                            ELSE
                                //
                                public := Modifiers.Internal
                            ENDIF
                        ENDIF
                    ENDIF
                    LOOP


                Label_008E:
                    public := Modifiers.Protected
                    LOOP


                Label_0093:
                    public := Modifiers.Private
                    LOOP


                Label_0097:
                    public := Modifiers.Public
                NEXT
            ENDIF
            RETURN public

        PROTECTED VIRTUAL METHOD endMember(context AS ParserRuleContext) AS void
            //
            SELF:_currentMethod := null

        VIRTUAL METHOD EnterClass_( context AS XSharpParser.Class_Context) AS void
            LOCAL tokens AS IList<IToken>
            LOCAL newType AS XType
            //
            TRY
                //
                tokens := IIF((context:Modifiers == null),null,context:Modifiers:_Tokens)
                newType := XType{context:Id:GetText(), Kind.@@Class, SELF:decodeModifiers(tokens), SELF:decodeVisibility(tokens), TextRange{context}, TextInterval{context}} NameSpace:=SELF:decodeNamespace(context:Namespace, SELF:currentNamespace), IsStatic:=SELF:isStatic(tokens), IsPartial:=SELF:isPartial(tokens)
                IF (context:BaseType != null)
                    //
                    newType:ParentName := context:BaseType:GetText()
                ENDIF
                IF ((context:_Implements != null) .AND. (context:_Implements:Count > 0))
                    //
                    FOREACH context2 AS XSharpParser.DatatypeContext IN context:_Implements

                    NEXT
                ENDIF
                IF ((newType != null) .AND. (newType:FullName != null))
                    //
                    newType := SELF:addType(newType)
                    SELF:pushType(newType)
                ENDIF
            CATCH obj1 as Object

            END TRY

        VIRTUAL METHOD EnterClassVarList( context AS XSharpParser.ClassVarListContext) AS void
            LOCAL context2 AS XSharpParser.ClassVarListContext
            LOCAL parent AS XSharpParserRuleContext
            LOCAL count AS Long
            LOCAL num2 AS Long
            LOCAL modifiers AS Modifiers
            LOCAL start AS IToken
            LOCAL stop AS IToken
            LOCAL position AS TextInterval
            LOCAL typeName AS string
            LOCAL item AS XTypeMember
            LOCAL flag AS Logic
            //
            context2 := context
            parent := (XSharpParserRuleContext)(context:Parent)
            count := context2:_Var:Count
            num2 := 0
            FOREACH context4 AS XSharpParser.ClassvarContext IN context2:_Var
                //
                modifiers := IIF(SELF:_currentVarStatic,Modifiers.Static,Modifiers.None)
                num2++
                start := parent:Start
                stop := context2:Stop
                IF (num2 > 1)
                    //
                    start := context4:Start
                ENDIF
                IF (num2 < count)
                    //
                    stop := context4:Stop
                ENDIF
                position := TextInterval{start:StartIndex, stop:StopIndex}
                typeName := IIF((context2:DataType != null),context2:DataType:GetText(),"USUAL")
                item := XTypeMember{context4:Id:GetText(), Kind.@@Field, modifiers, SELF:_currentVarVisibility, TextRange{start:Line, start:Column, stop:Line, (stop:Column + stop:Text:Length)}, position, typeName, SELF:_currentVarStatic} @@File:=SELF:_file, IsArray:=(context4:Dim != null)
                flag := TRUE
                IF (SELF:_errors != null)
                    //
                    FOREACH error AS XError IN SELF:_errors
                        //
                        IF (error:Span:Start:Line == (item:Range:StartLine - 1))
                            //
                            flag := FALSE
                        ENDIF
                    NEXT
                ENDIF
                IF (flag)
                    //
                    SELF:_classVars:Add(item)
                ENDIF
            NEXT

        VIRTUAL METHOD EnterClassvarModifiers( context AS XSharpParser.ClassvarModifiersContext) AS void
            LOCAL tokens AS IList<IToken>
            //
            tokens := context:_Tokens
            SELF:_currentVarVisibility := SELF:decodeVisibility(tokens)
            SELF:_currentVarStatic := SELF:isStatic(tokens)

        VIRTUAL METHOD EnterConstructor( context AS XSharpParser.ConstructorContext) AS void
            LOCAL tokens AS IList<IToken>
            LOCAL newMethod AS XTypeMember
            //
            tokens := IIF((context:Modifiers == null),null,context:Modifiers:_Tokens)
            newMethod := XTypeMember{"Constructor", Kind.@@Constructor, SELF:decodeModifiers(tokens), SELF:decodeVisibility(tokens), TextRange{context}, TextInterval{context}, SELF:isStatic(tokens)}
            SELF:addParameters(context:Params, newMethod)
            SELF:addMember(newMethod)

        VIRTUAL METHOD EnterDelegate_( context AS XSharpParser.Delegate_Context) AS void
            LOCAL tokens AS IList<IToken>
            LOCAL newMethod AS XTypeMember
            //
            tokens := IIF((context:Modifiers == null),null,context:Modifiers:_Tokens)
            newMethod := XTypeMember{context:Id:GetText(), Kind.@@Delegate, SELF:decodeModifiers(tokens), SELF:decodeVisibility(tokens), TextRange{context}, TextInterval{context}, SELF:isStatic(tokens)}
            SELF:addParameters(context:Params, newMethod)
            SELF:addGlobalMember(newMethod)

        VIRTUAL METHOD EnterDestructor( context AS XSharpParser.DestructorContext) AS void
            LOCAL tokens AS IList<IToken>
            LOCAL newMethod AS XTypeMember
            //
            tokens := IIF((context:Modifiers == null),null,context:Modifiers:_Tokens)
            newMethod := XTypeMember{"Destructor", Kind.@@Destructor, SELF:decodeModifiers(tokens), SELF:decodeVisibility(tokens), TextRange{context}, TextInterval{context}, "Void", FALSE}
            SELF:addParameters(context:Params, newMethod)
            SELF:addMember(newMethod)

        VIRTUAL METHOD EnterEnum_( context AS XSharpParser.Enum_Context) AS void
            LOCAL tokens AS IList<IToken>
            LOCAL newType AS XType
            //
            tokens := IIF((context:Modifiers == null),null,context:Modifiers:_Tokens)
            newType := XType{context:Id:GetText(), Kind.@@Enum, SELF:decodeModifiers(tokens), SELF:decodeVisibility(tokens), TextRange{context}, TextInterval{context}} NameSpace:=SELF:currentNamespace
            newType := SELF:addType(newType)
            SELF:pushType(newType)

        VIRTUAL METHOD EnterEnummember( context AS XSharpParser.EnummemberContext) AS void
            LOCAL oMember AS XTypeMember
            //
            oMember := XTypeMember{context:Id:GetText(), Kind.@@EnumMember, Modifiers.None, Modifiers.None, TextRange{context}, TextInterval{context}, "", FALSE}
            SELF:addMember(oMember)

        VIRTUAL METHOD EnterEvent_( context AS XSharpParser.Event_Context) AS void
            LOCAL tokens AS IList<IToken>
            LOCAL newMethod AS XTypeMember
            //
            tokens := IIF((context:Modifiers == null),null,context:Modifiers:_Tokens)
            newMethod := XTypeMember{context:ShortName, Kind.@@Event, SELF:decodeModifiers(tokens), SELF:decodeVisibility(tokens), TextRange{context}, TextInterval{context}, IIF((context:ReturnType == null),"Void",context:ReturnType:GetText()), SELF:isStatic(tokens)}
            SELF:addParameters(context:Params, newMethod)
            SELF:addMember(newMethod)

        VIRTUAL METHOD EnterEventAccessor( context AS XSharpParser.EventAccessorContext) AS void


        VIRTUAL METHOD EnterFunction( context AS XSharpParser.FunctionContext) AS void
            LOCAL tokens AS IList<IToken>
            LOCAL newMethod AS XTypeMember
            //
            tokens := IIF((context:Modifiers == null),null,context:Modifiers:_Tokens)
            newMethod := XTypeMember{context:Id:GetText(), Kind.@@Function, SELF:decodeModifiers(tokens), SELF:decodeVisibility(tokens), TextRange{context}, TextInterval{context}, IIF((context:Type == null),"Void",context:Type:GetText()), SELF:isStatic(tokens)}
            SELF:addParameters(context:Params, newMethod)
            SELF:addGlobalMember(newMethod)

        VIRTUAL METHOD EnterInterface_( context AS XSharpParser.Interface_Context) AS void
            LOCAL tokens AS IList<IToken>
            LOCAL newType AS XType
            //
            tokens := IIF((context:Modifiers == null),null,context:Modifiers:_Tokens)
            newType := XType{context:Id:GetText(), Kind.@@Interface, SELF:decodeModifiers(tokens), SELF:decodeVisibility(tokens), TextRange{context}, TextInterval{context}} NameSpace:=SELF:decodeNamespace(context:Namespace, SELF:currentNamespace), IsStatic:=SELF:isStatic(tokens), IsPartial:=SELF:isPartial(tokens)
            newType := SELF:addType(newType)
            SELF:pushType(newType)

        VIRTUAL METHOD EnterMethod( context AS XSharpParser.MethodContext) AS void
            LOCAL method AS Kind
            LOCAL tokens AS IList<IToken>
            LOCAL newMethod AS XTypeMember
            //
            method := Kind.@@Method
            IF (context:Name:Contains(":Access"))
                //
                method := Kind.@@Access
            ELSE
                //
                IF (context:Name:Contains(":Assign"))
                    //
                    method := Kind.@@Assign
                ENDIF
            ENDIF
            tokens := IIF((context:Modifiers == null),null,context:Modifiers:_Tokens)
            newMethod := XTypeMember{context:Id:GetText(), method, SELF:decodeModifiers(tokens), SELF:decodeVisibility(tokens), TextRange{context}, TextInterval{context}, IIF((context:Type == null),"Void",context:Type:GetText()), SELF:isStatic(tokens)}
            SELF:addParameters(context:Params, newMethod)
            SELF:addMember(newMethod)

        VIRTUAL METHOD EnterNamespace_( context AS XSharpParser.Namespace_Context) AS void
            LOCAL context2 AS XSharpParser.Namespace_Context
            LOCAL item AS XType
            //
            context2 := context
            item := XType{context2:Name:GetText(), Kind.Namespace, Modifiers.None, Modifiers.Public, TextRange{context}, TextInterval{context}}
            SELF:_currentNSpaces:Push(item)

        VIRTUAL METHOD EnterOperator_( context AS XSharpParser.Operator_Context) AS void
            LOCAL tokens AS IList<IToken>
            LOCAL newMethod AS XTypeMember
            //
            tokens := IIF((context:Modifiers == null),null,context:Modifiers:_Tokens)
            newMethod := XTypeMember{context:ShortName, Kind.Operator, SELF:decodeModifiers(tokens), SELF:decodeVisibility(tokens), TextRange{context}, TextInterval{context}, IIF((context:ReturnType == null),"Void",context:ReturnType:GetText()), SELF:isStatic(tokens)}
            SELF:addParameters(context:Params, newMethod)
            SELF:addMember(newMethod)

        VIRTUAL METHOD EnterProcedure( context AS XSharpParser.ProcedureContext) AS void
            LOCAL tokens AS IList<IToken>
            LOCAL newMethod AS XTypeMember
            //
            tokens := IIF((context:Modifiers == null),null,context:Modifiers:_Tokens)
            newMethod := XTypeMember{context:Id:GetText(), Kind.Procedure, SELF:decodeModifiers(tokens), SELF:decodeVisibility(tokens), TextRange{context}, TextInterval{context}, FALSE}
            SELF:addParameters(context:Params, newMethod)
            SELF:addGlobalMember(newMethod)

        VIRTUAL METHOD EnterProperty( context AS XSharpParser.PropertyContext) AS void
            LOCAL name AS string
            LOCAL tokens AS IList<IToken>
            LOCAL newMethod AS XTypeMember
            //
            name := ""
            IF (context:Id != null)
                //
                name := context:Id:GetText()
            ENDIF
            IF (context:SELF() != null)
                //
                name := context:SELF():GetText()
            ENDIF
            tokens := IIF((context:Modifiers == null),null,context:Modifiers:_Tokens)
            newMethod := XTypeMember{name, Kind.Property, SELF:decodeModifiers(tokens), SELF:decodeVisibility(tokens), TextRange{context}, TextInterval{context}, IIF((context:Type == null),"Void",context:Type:GetText()), SELF:isStatic(tokens)}
            SELF:addParameters(context:Params, newMethod)
            SELF:addMember(newMethod)

        VIRTUAL METHOD EnterPropertyAccessor( context AS XSharpParser.PropertyAccessorContext) AS void


        VIRTUAL METHOD EnterSource( context AS XSharpParser.SourceContext) AS void
            //
            SELF:addUsing("System", FALSE)
            IF ((SELF:_file:Project:ProjectNode:ParseOptions != null) .AND. SELF:_file:Project:ProjectNode:ParseOptions:IsDialectVO)
                //
                SELF:addUsing("Vulcan", FALSE)
            ENDIF

        VIRTUAL METHOD EnterStructure_( context AS XSharpParser.Structure_Context) AS void
            LOCAL tokens AS IList<IToken>
            LOCAL newType AS XType
            //
            tokens := IIF((context:Modifiers == null),null,context:Modifiers:_Tokens)
            newType := XType{context:Id:GetText(), Kind.Structure, SELF:decodeModifiers(tokens), SELF:decodeVisibility(tokens), TextRange{context}, TextInterval{context}} NameSpace:=SELF:decodeNamespace(context:Namespace, SELF:currentNamespace), IsStatic:=SELF:isStatic(tokens), IsPartial:=SELF:isPartial(tokens)
            IF ((context:_Implements != null) .AND. (context:_Implements:Count > 0))
                //
                FOREACH context2 AS XSharpParser.DatatypeContext IN context:_Implements

                NEXT
            ENDIF
            newType := SELF:addType(newType)
            SELF:pushType(newType)

        VIRTUAL METHOD EnterUsing_( context AS XSharpParser.Using_Context) AS void
            LOCAL context2 AS XSharpParser.Using_Context
            //
            context2 := context
            SELF:addUsing(context2:Name:GetText(), (context2:Static != null))

        VIRTUAL METHOD EnterVodefine( context AS XSharpParser.VodefineContext) AS void
            LOCAL tokens AS IList<IToken>
            LOCAL oMember AS XTypeMember
            //
            tokens := IIF((context:Modifiers == null),null,context:Modifiers:_Tokens)
            oMember := XTypeMember{context:ShortName, Kind.VODefine, SELF:decodeModifiers(tokens), SELF:decodeVisibility(tokens), TextRange{context}, TextInterval{context}, IIF((context:ReturnType == null),"Void",context:ReturnType:GetText()), SELF:isStatic(tokens)}
            IF (context:Expr != null)
                //
                oMember:Suffix := String.Concat(" := ", context:Expr:GetText())
            ENDIF
            SELF:addGlobalMember(oMember)

        VIRTUAL METHOD EnterVodll( context AS XSharpParser.VodllContext) AS void
            LOCAL tokens AS IList<IToken>
            LOCAL newMethod AS XTypeMember
            //
            tokens := IIF((context:Modifiers == null),null,context:Modifiers:_Tokens)
            newMethod := XTypeMember{context:ShortName, Kind.VODLL, SELF:decodeModifiers(tokens), SELF:decodeVisibility(tokens), TextRange{context}, TextInterval{context}, IIF((context:ReturnType == null),"Void",context:ReturnType:GetText()), SELF:isStatic(tokens)}
            SELF:addParameters(context:Params, newMethod)
            SELF:addGlobalMember(newMethod)

        VIRTUAL METHOD EnterVoglobal( context AS XSharpParser.VoglobalContext) AS void
            LOCAL tokens AS IList<IToken>
            //
            tokens := IIF((context:Modifiers == null),null,context:Modifiers:_Tokens)
            SELF:_currentVarVisibility := SELF:decodeVisibility(tokens)
            SELF:_currentVarStatic := SELF:isStatic(tokens)

        VIRTUAL METHOD EnterVostruct( context AS XSharpParser.VostructContext) AS void
            LOCAL tokens AS IList<IToken>
            LOCAL newType AS XType
            //
            tokens := IIF((context:Modifiers == null),null,context:Modifiers:_Tokens)
            newType := XType{context:Id:GetText(), Kind.VOStruct, SELF:decodeModifiers(tokens), SELF:decodeVisibility(tokens), TextRange{context}, TextInterval{context}} NameSpace:=SELF:decodeNamespace(context:Namespace, SELF:currentNamespace), IsStatic:=SELF:isStatic(tokens)
            newType := SELF:addType(newType)
            SELF:pushType(newType)

        VIRTUAL METHOD EnterVostructmember( context AS XSharpParser.VostructmemberContext) AS void
            LOCAL oMember AS XTypeMember
            //
            oMember := XTypeMember{context:Id:GetText(), Kind.Field, Modifiers.Public, Modifiers.Public, TextRange{context}, TextInterval{context}, FALSE} IsArray:=(context:Dim != null)
            SELF:addMember(oMember)

        VIRTUAL METHOD EnterVounion( context AS XSharpParser.VounionContext) AS void
            LOCAL tokens AS IList<IToken>
            LOCAL newType AS XType
            //
            tokens := IIF((context:Modifiers == null),null,context:Modifiers:_Tokens)
            newType := XType{context:Id:GetText(), Kind.Union, SELF:decodeModifiers(tokens), SELF:decodeVisibility(tokens), TextRange{context}, TextInterval{context}} NameSpace:=SELF:decodeNamespace(context:Namespace, SELF:currentNamespace)
            newType := SELF:addType(newType)
            SELF:pushType(newType)

        VIRTUAL METHOD ExitClass_( context AS XSharpParser.Class_Context) AS void
            //
            SELF:popType()

        VIRTUAL METHOD ExitClassvars( context AS XSharpParser.ClassvarsContext) AS void
            LOCAL currentType AS XType
            //
            currentType := SELF:currentType
            IF (currentType != null)
                //
                FOREACH oMember AS XTypeMember IN SELF:_classVars
                    //
                    oMember:Parent := currentType
                    currentType:AddMember(oMember)
                NEXT
            ENDIF
            SELF:_classVars:Clear()

        VIRTUAL METHOD ExitConstructor( context AS XSharpParser.ConstructorContext) AS void
            //
            SELF:endMember(context)

        VIRTUAL METHOD ExitDelegate_( context AS XSharpParser.Delegate_Context) AS void
            //
            SELF:endMember(context)

        VIRTUAL METHOD ExitDestructor( context AS XSharpParser.DestructorContext) AS void
            //
            SELF:endMember(context)

        VIRTUAL METHOD ExitEnum_( context AS XSharpParser.Enum_Context) AS void
            //
            SELF:popType()

        VIRTUAL METHOD ExitEnummember( context AS XSharpParser.EnummemberContext) AS void
            //
            SELF:endMember(context)

        VIRTUAL METHOD ExitEvent_( context AS XSharpParser.Event_Context) AS void
            //
            SELF:endMember(context)

        VIRTUAL METHOD ExitEventAccessor( context AS XSharpParser.EventAccessorContext) AS void


        VIRTUAL METHOD ExitForeachStmt( context AS XSharpParser.ForeachStmtContext) AS void
            //
            SUPER:ExitForeachStmt(context)

        VIRTUAL METHOD ExitFunction( context AS XSharpParser.FunctionContext) AS void
            //
            SELF:endMember(context)

        VIRTUAL METHOD ExitInterface_( context AS XSharpParser.Interface_Context) AS void
            //
            SELF:popType()

        VIRTUAL METHOD ExitMethod( context AS XSharpParser.MethodContext) AS void
            //
            SELF:endMember(context)

        VIRTUAL METHOD ExitNamespace_( context AS XSharpParser.Namespace_Context) AS void
            LOCAL newType AS XType
            //
            IF (SELF:_currentNSpaces:Count > 0)
                //
                newType := SELF:_currentNSpaces:Peek()
                SELF:addType(newType)
                SELF:_currentNSpaces:Pop()
            ENDIF

        VIRTUAL METHOD ExitOperator_( context AS XSharpParser.Operator_Context) AS void
            //
            SELF:endMember(context)

        VIRTUAL METHOD ExitProcedure( context AS XSharpParser.ProcedureContext) AS void
            //
            SELF:endMember(context)

        VIRTUAL METHOD ExitProperty( context AS XSharpParser.PropertyContext) AS void
            //
            SELF:endMember(context)

        VIRTUAL METHOD ExitPropertyAccessor( context AS XSharpParser.PropertyAccessorContext) AS void


        VIRTUAL METHOD ExitSource( context AS XSharpParser.SourceContext) AS void
            //
            SELF:@@File:SetTypes(SELF:_types, SELF:_usings, SELF:_staticusings, FALSE)

        VIRTUAL METHOD ExitStructure_( context AS XSharpParser.Structure_Context) AS void
            //
            SELF:popType()

        VIRTUAL METHOD ExitUsing_( context AS XSharpParser.Using_Context) AS void


        VIRTUAL METHOD ExitVodefine( context AS XSharpParser.VodefineContext) AS void
            //
            SELF:endMember(context)

        VIRTUAL METHOD ExitVodll( context AS XSharpParser.VodllContext) AS void
            //
            SELF:endMember(context)

        VIRTUAL METHOD ExitVoglobal( context AS XSharpParser.VoglobalContext) AS void
            LOCAL member2 AS XTypeMember
            //
            FOREACH oMember AS XTypeMember IN SELF:_classVars
                //
                member2 := XTypeMember{oMember:Name, Kind.VOGlobal, oMember:Modifiers, oMember:Visibility, oMember:Range, oMember:Interval, oMember:TypeName, SELF:_currentVarStatic}
                SELF:addGlobalMember(member2)
            NEXT
            SELF:_classVars:Clear()
            SELF:endMember(context)

        VIRTUAL METHOD ExitVostruct( context AS XSharpParser.VostructContext) AS void
            //
            SELF:popType()

        VIRTUAL METHOD ExitVostructmember( context AS XSharpParser.VostructmemberContext) AS void
            //
            SELF:endMember(context)

        VIRTUAL METHOD ExitVounion( context AS XSharpParser.VounionContext) AS void
            //
            SELF:popType()

        PRIVATE METHOD isPartial(modifiers AS IList<IToken>) AS Logic
            LOCAL flag AS Logic
            //
            flag := FALSE
            IF (modifiers != null)
                //
                flag := (System.Linq.Enumerable.Count<IToken>((FROM t IN modifiers
                    WHERE (t:@@Type == 0x6a)
                    SELECT t)) > 0)
            ENDIF
            RETURN flag

        PRIVATE METHOD isStatic(modifiers AS IList<IToken>) AS Logic
            LOCAL flag AS Logic
            //
            flag := FALSE
            IF (modifiers != null)
                //
                flag := (System.Linq.Enumerable.Count<IToken>((FROM t IN modifiers
                    WHERE (t:@@Type == 0x42)
                    SELECT t)) > 0)
            ENDIF
            RETURN flag

        PRIVATE METHOD popType() AS void
            //
            IF (SELF:_currentTypes:Count > 0)
                //
                SELF:_currentTypes:Pop()
            ENDIF

        PRIVATE METHOD pushType(type AS XType) AS void
            type:File := SELF:_file
            SELF:_currentTypes:Push(type)


        // Properties
        PRIVATE PROPERTY currentNamespace AS string
            GET
                var str := ""
                IF (SELF:_currentNSpaces:Count > 0)
                    RETURN SELF:_currentNSpaces:Peek():Name
                ENDIF
                IF ! String.IsNullOrEmpty(SELF:_defaultNS)
                    str := SELF:_defaultNS
                ENDIF
                RETURN str
            END GET
        END PROPERTY

        PRIVATE PROPERTY currentType AS XType
            GET
                IF (SELF:_currentTypes:Count > 0)
                    RETURN SELF:_currentTypes:Peek()
                ENDIF
                RETURN null
            END GET
        END PROPERTY

        PROPERTY File AS XFile
            GET
                RETURN SELF:_file
            END GET
        END PROPERTY


        INTERNAL CLASS XCodeTypeReference

            INTERNAL  CONSTRUCTOR(typeName AS string)
				SUPER()
                SELF:TypeName := typeName

            INTERNAL  CONSTRUCTOR(type AS System.Type)
				SUPER()
                SELF:TypeName := type:Name

            INTERNAL PROPERTY TypeName AS string AUTO 
        END CLASS


    END CLASS

END NAMESPACE // XSharpModel

