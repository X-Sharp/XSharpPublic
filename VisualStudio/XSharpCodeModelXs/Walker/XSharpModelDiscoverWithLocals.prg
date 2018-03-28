USING System.Collections.Generic
USING XSharpModel
BEGIN NAMESPACE XSharpModel
    INTERNAL CLASS XSharpModelDiscoverWithLocals INHERIT XSharpModelDiscover
        // Fields
        INITONLY PRIVATE _localDecls AS System.Collections.Generic.Stack<XSharpParser.LocalvarContext>

        // Methods
         CONSTRUCTOR(file AS XFile, ctx AS XSharpParser.SourceContext, errors AS IEnumerable<XError>)//Inline call to base() in C#
        SUPER(file, ctx, errors)
            //
            SELF:_localDecls := Stack<XSharpParser.LocalvarContext>{}

        PROTECTED VIRTUAL METHOD addVariables( context AS ParserRuleContext) AS void
            if super:_currentMethod != null .AND. ;
				(context IS XSharpParser.ConstructorContext .OR. context IS XSharpParser.DestructorContext ;
				 .OR. context IS XSharpParser.MethodContext .OR. context IS XSharpParser.PropertyContext)
                //
                var item := XVariable{super:_currentMethod, "Self", Kind.@@Local, Modifiers.@@Public, TextRange{context}, TextInterval{context}, super:_currentMethod:ParentName, false} 
				item:File:=SUPER:_file
                SUPER:_currentMethod:Locals:Add(item)
                IF (! String.IsNullOrEmpty(SUPER:_currentMethod:Parent:ParentName))
                    //
                    item := XVariable{SUPER:_currentMethod, "Super", Kind.@@Local, Modifiers.@@Public, TextRange{context}, TextInterval{context}, SUPER:_currentMethod:Parent:ParentName, FALSE}
					item:File := SUPER:_file
                    SUPER:_currentMethod:Locals:Add(item)
                ENDIF
            ENDIF

        PROTECTED METHOD buildValueName(name AS string) AS string
            LOCAL typeName AS string
            LOCAL variable AS XVariable
            //
            typeName := ""
            IF (SUPER:_currentMethod > null)
                //
                variable := SELF:findLocal(name)
                IF (variable > null)
                    //
                    typeName := variable:TypeName
                ENDIF
            ENDIF
            RETURN typeName

        PROTECTED VIRTUAL METHOD endMember(context AS ParserRuleContext) AS void
            //
            SELF:addVariables(context)
            SUPER:endMember(context)

        VIRTUAL METHOD EnterImpliedvar( context AS XSharpParser.ImpliedvarContext) AS void
            LOCAL expression AS XSharpParser.PrimaryExpressionContext
            LOCAL expr AS XSharpParser.PrimaryContext
            LOCAL context4 AS XSharpParser.LiteralExpressionContext
            LOCAL typeName AS string
            LOCAL text AS string
            LOCAL item AS XVariable
            LOCAL variable2 AS XVariable
            LOCAL context5 AS XSharpParser.NameExpressionContext
            LOCAL name AS string
            LOCAL str4 AS string
            LOCAL str5 AS string
            LOCAL variable3 AS XVariable
            LOCAL context6 AS XSharpParser.CtorCallContext
            LOCAL str6 AS string
            LOCAL str7 AS string
            LOCAL variable4 AS XVariable
            LOCAL context7 AS XSharpParser.MethodCallContext
            LOCAL context8 AS XSharpParser.ExpressionContext
            LOCAL str8 AS string
            LOCAL str9 AS string
            LOCAL variable5 AS XVariable
            LOCAL o AS Object[]
            //
            TRY
                //
                IF ((context:Expression IS XSharpParser.PrimaryExpressionContext))
                    //
                    expression := (XSharpParser.PrimaryExpressionContext)context:Expression 
                    expr := expression:Expr
                    IF ((expr IS XSharpParser.LiteralExpressionContext))
                        //
                        context4 := (XSharpParser.LiteralExpressionContext)expr 
                        typeName := SUPER:buildLiteralValue(context4:Literal)
                        text := context:Id:GetText()
                        item := XVariable{SUPER:_currentMethod, text, Kind.@@Local, Modifiers.@@Public, TextRange{context}, TextInterval{context}, typeName, FALSE} 
						item:File:=SUPER:_file
						item:IsArray:=FALSE
                        IF (SUPER:_currentMethod > null)
                            //
                            SUPER:_currentMethod:Locals:Add(item)
                        ENDIF
                    ELSE
                        //
                        IF ((expr IS XSharpParser.NameExpressionContext))
                            //
                            context5 := (XSharpParser.NameExpressionContext)expr 
                            name := context5:Name:Id:GetText()
                            str4 := context:Id:GetText()
                            str5 := SELF:buildValueName(name)
                            IF (str5 == XVariable.VarType)
                                //
                                variable3 := SELF:findLocal(name)
                                variable2 := XVariable{SUPER:_currentMethod, str4, Kind.@@Local, Modifiers.@@Public, TextRange{context}, variable3:Interval, XVariable.VarType, FALSE}
                            ELSE
                                //
                                variable2 := XVariable{SUPER:_currentMethod, str4, Kind.@@Local, Modifiers.@@Public, TextRange{context}, TextInterval{context}, str5, FALSE}
                            ENDIF
                            variable2:@@File := SUPER:_file
                            variable2:IsArray := FALSE
                            IF (SUPER:_currentMethod > null)
                                //
                                SUPER:_currentMethod:Locals:Add(variable2)
                            ENDIF
                        ELSE
                            //
                            IF ((expr IS XSharpParser.CtorCallContext))
                                //
                                context6 := (XSharpParser.CtorCallContext)expr 
                                str6 := SUPER:buildDataType(context6:Type):TypeName
                                str7 := context:Id:GetText()
                                variable4 := XVariable{SUPER:_currentMethod, str7, Kind.@@Local, Modifiers.@@Public, TextRange{context}, TextInterval{context}, str6, FALSE} 
								variable4:File:=SUPER:_file
								variable4:IsArray:=FALSE
                                IF (SUPER:_currentMethod != null)
                                    //
                                    SUPER:_currentMethod:Locals:Add(variable4)
                                ENDIF
                            ENDIF
                        ENDIF
                    ENDIF
                ELSE
                    //
                    IF ((context:Expression IS XSharpParser.MethodCallContext))
                        //
                        context7 := (XSharpParser.MethodCallContext)context:Expression 
                        context8 := context7:Expr
                        str8 := context8:GetText()
                        str9 := context:Id:GetText()
                        variable5 := XVariable{SUPER:_currentMethod, str9, Kind.@@Local, Modifiers.@@Public, TextRange{context}, TextInterval{context8}, XVariable.VarType, FALSE} 
						variable5:File:=SUPER:_file
						variable5:IsArray:=FALSE
                        IF (SUPER:_currentMethod > null)
                            //
                            SUPER:_currentMethod:Locals:Add(variable5)
                        ENDIF
                    ENDIF
                ENDIF
            CATCH exception as System.Exception
                //
                o := <Object>{SUPER:@@File:Name, context:Start:Line, context:Start:Column}
                Support.Debug(String.Concat("EnterImpliedvar : Error Walking {0}, at {1}/{2} : ", exception:Message), o)
            END TRY

        VIRTUAL METHOD EnterLocalvar( context AS XSharpParser.LocalvarContext) AS void
            LOCAL text AS string
            LOCAL context2 AS XSharpParser.LocalvarContext
            LOCAL name AS string
            LOCAL item AS XVariable
            LOCAL o AS Object[]
            //
            TRY
                //
                IF (context:DataType > null)
                    //
                    text := context:DataType:GetText()
                    SELF:_localDecls:Push(context)
                    WHILE ((SELF:_localDecls:Count > 0))
                        //
                        context2 := SELF:_localDecls:Pop()
                        name := context2:Id:GetText()
                        item := XVariable{SUPER:_currentMethod, name, Kind.@@Local, Modifiers.@@Public, TextRange{context2}, TextInterval{context2}, text, FALSE} 
						item:File:=SUPER:_file
						item:IsArray:=(context2:Dim != null)
                        IF (SUPER:_currentMethod != null)
                            //
                            SUPER:_currentMethod:Locals:Add(item)
                        ENDIF
                    ENDDO
                ELSE
                    //
                    SELF:_localDecls:Push(context)
                ENDIF
            CATCH exception as System.Exception
                //
                o := <Object>{SUPER:@@File:Name, context:Start:Line, context:Start:Column}
                Support.Debug(String.Concat("EnterLocalvar : Error Walking {0}, at {1}/{2} : ", exception:Message), o)
            END TRY

        VIRTUAL METHOD ExitSource( context AS XSharpParser.SourceContext) AS void
            //
            SUPER:@@File:SetTypes(SUPER:_types, SUPER:_usings, SUPER:_staticusings, TRUE)

        PROTECTED METHOD findLocal(name AS string) AS XVariable
            LOCAL variable AS XVariable
            //
            variable := null
            IF (SUPER:_currentMethod > null)
                //
                variable := SUPER:_currentMethod:Locals:Find(x => String.Equals(x:Name, name, System.StringComparison.InvariantCultureIgnoreCase))
            ENDIF
            RETURN variable


    END CLASS

END NAMESPACE // XSharpModel

