//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
// Based on original work for XIDE
//
using System.Linq
using System.Collections.Generic
using Mono.Cecil
USING XSharp.Settings
BEGIN NAMESPACE XSharpModel

DELEGATE XmlDocProvider (entry as IXSymbol) AS STRING

CLASS XClassCreator
	STATIC PROTECT _includehidden AS LOGIC
	STATIC PROTECT _showbeginnamespace AS LOGIC
	STATIC PROTECT _indent AS STRING

	PROTECT aLines AS List<STRING>
	PROTECT aNameSpaces AS SortedList<STRING,STRING>
	PROTECT nSectionLine AS INT
    PROTECT cLocation AS STRING
    PROTECT oDocProvider as XmlDocProvider

	CONSTRUCTOR(oType AS XPETypeSymbol, cLocation AS STRING, provider as XmlDocProvider)
		SELF:aLines := List<STRING>{}
        SELF:cLocation := cLocation
		SELF:aNameSpaces := SortedList<STRING,STRING>{}
        SELF:oDocProvider:= provider
        SELF:CreateCode(oType)
        IF XEditorSettings.TabsAsSpaces
            _indent := " "
        ELSE
            _indent := e"\t"
        ENDIF
	RETURN

    STATIC METHOD Create(oPeType as XPETypeSymbol, provider := null as XmlDocProvider) AS List<STRING>
        var asm := oPeType:Assembly
        var file := asm:FileName
        RETURN Create(oPeType,file, provider)

	STATIC METHOD Create(oType AS XPETypeSymbol, cLocation as STRING, provider as XmlDocProvider) AS List<STRING>
		TRY
			var oCreator := XClassCreator{oType, cLocation,provider}
			return  oCreator:GetLines()


        CATCH e as Exception
            var result := List<String>{}
            result:Add("// Exception occurred during reading type information: ")
            result:Add("// "+e:Message)
            XSettings.Exception(e, __FUNCTION__)
            return result
		END TRY

    PROTECTED METHOD GetDoc(sym as IXSymbol,nNest as long) AS VOID
        IF SELF:oDocProvider != NULL .and. XSettings.CodeGeneratorShowXmlComments
            LOCAL cDoc as STRING
            cDoc := SELF:oDocProvider(sym)
            IF ! String.IsNullOrWhiteSpace(cDoc)
                var docLines := cDoc:Split(<char>{'\r','\n'},StringSplitOptions.RemoveEmptyEntries)
                foreach var docLine in docLines
                    SELF:AddLine("/// "+docLine, nNest)
                next
            ENDIF
        ENDIF
        return

	PROTECTED METHOD GetLines() AS List<STRING>
	RETURN SELF:aLines
	PROTECTED METHOD AddLine(cLine AS STRING , nNest AS INT) AS VOID
		LOCAL n AS INT
		IF nNest > 0
			FOR n := 1 UPTO nNest
				cLine := _indent + cLine
			NEXT
		END IF
		SELF:aLines:Add(cLine)
	RETURN

	PROTECTED STATIC METHOD AddLine(cLine AS STRING , nNest AS INT , aLines AS List<STRING>) AS VOID
		LOCAL n AS INT
		IF nNest > 0
			FOR n := 1 UPTO nNest
				cLine := _indent + cLine
			NEXT
		END IF
		aLines:Add(cLine)
	RETURN

    PROTECTED METHOD BeginSection() AS VOID
		SELF:nSectionLine := SELF:aLines:Count
	RETURN

	PROTECTED METHOD EndSection(nNest AS INT) AS VOID
		IF SELF:nSectionLine+2 != SELF:aLines:Count
			SELF:AddLine("" , nNest)
        ELSE
            DO WHILE SELF:aLines:Count > SELF:nSectionLine
                SELF:aLines:RemoveAt(SELF:aLines:Count-1)
            ENDDO
		END IF
	RETURN

	PROTECTED METHOD CreateCode(oType AS XPETypeSymbol) AS VOID
		LOCAL nNest AS INT

		SELF:aLines:Clear()
		SELF:aNameSpaces:Clear()
		IF oType == NULL
			RETURN
		END IF

		IF .not. String.IsNullOrEmpty(oType:@@Namespace) .and. _showbeginnamespace
			SELF:AddLine("BEGIN NAMESPACE " + oType:@@Namespace , nNest)
			SELF:AddLine("" , nNest)
			nNest ++
		END IF

		IF oType:Kind == Kind.Delegate
			//SELF:AddDelegate(oType , nNest)
            NOP
		ELSEIF oType:IsPublic .or. oType:IsNested
			SELF:AddType(oType , nNest)
		END IF

		IF .not. String.IsNullOrEmpty(oType:@@Namespace) .and. _showbeginnamespace
			nNest --
			SELF:AddLine("" , nNest)
			SELF:AddLine("END NAMESPACE" , nNest)
		END IF

		IF SELF:aNameSpaces:Count != 0
			LOCAL n AS INT
			SELF:aLines:Insert(0 , "")
			FOR n := aNameSpaces:Count - 1 DOWNTO 0
				SELF:aLines:Insert(0 , "using " + aNameSpaces:Values[n])
			NEXT

		END IF

		IF SELF:aLines:Count != 0
			SELF:aLines:Insert(0 , "// Metadata taken from assembly: " + oType:Assembly:FullName)
            SELF:aLines:Insert(1 , "// Location                    : " + SELF:cLocation)
			SELF:aLines:Insert(2 , "")
		END IF

	RETURN

	PROTECTED METHOD AddType(oType AS XPETypeSymbol , nNest AS INT, cParentName := "" AS STRING) AS VOID
		LOCAL cLine AS STRING
		LOCAL cInterfaces AS STRING
        var isFunctionsClass := oType:IsFunctionsClass
        var aMethods := oType:GetMethods(TRUE):ToList()
        var aOperators := aMethods:Where( { m => m:Name:StartsWith("op_") }):ToList()
        var aSpecial   := aMethods:Where( ;
                { m => m:Name:StartsWith("get_") .or. ;
                  m:Name:StartsWith("set_") .or. ;
                  m:Name:StartsWith("add_") .or. ;
                  m:Name:StartsWith("remove_") }):ToList()
        foreach var m in aOperators
            aMethods:Remove(m)
        next
        foreach var m in aSpecial
            aMethods:Remove(m)
        next
        local isVisible as @@Func<IXMemberSymbol, LOGIC>
        isVisible := { m as IXMemberSymbol => m:IsExternalVisible .and. m:Name:IndexOfAny ( <char> {'$','<'}) == -1}
        IF isFunctionsClass
            isVisible := { m as IXMemberSymbol => m:IsPublic .and. m:Name:IndexOfAny ( <char> {'$','<'}) == -1}
        ELSE
            SELF:GetDoc(oType,nNest)
            SELF:AddAttributes(oType, aLines, nNest)
            cLine := oType:VisibilityKeyword+" "+oType:ModifiersKeyword+" "+ oType:KindKeyword + " "
            IF String.IsNullOrEmpty(cParentName)
                cLine += oType:FullName
            ELSE
                cLine += oType:FullName:Replace(cParentName+".","")
            ENDIF
            IF ! String.IsNullOrWhiteSpace(oType:BaseTypeName)
                cLine += " INHERIT "+oType:BaseTypeName
            ENDIF
    		cInterfaces := SELF:GetInterfaces(oType)

		    IF cInterfaces:Length != 0
			    IF oType:Kind == Kind.Interface
				    cLine += " INHERIT "
			    ELSE
				    cLine += " IMPLEMENTS "
			    END IF
			    cLine += cInterfaces
		    END IF

    		SELF:AddLine(cLine , nNest)
	    	nNest ++
        ENDIF

		IF oType:Kind == Kind.Enum
			var fields := oType:GetFields()
            FOREACH var item in fields
                if (item:Name == "value_")
                    LOOP
                ENDIF
                SELF:GetDoc(item,nNest)
                SELF:AddLine("MEMBER "+item:Name, nNest)
            NEXT


		ELSE
			SELF:AddLine("" , nNest)

            var coll := oType:GetConstructors(TRUE)
            IF coll:Length > 0
			    SELF:BeginSection()
                SELF:AddLine("#region Constructors", nNest)
                var list := coll:ToList()
                list:Sort( {a,b => String.Compare(a:Prototype, b:Prototype)})

                FOREACH item AS XPEMethodSymbol in list
                    cLine := item:ClassGenText
                    cLine := cLine:Replace(oType:FullName+"{","(")
                    cLine := cLine:Replace("}",")")
                    SELF:GetDoc(item,nNest)
                    SELF:AddLine(cLine, nNest)
                NEXT
                SELF:AddLine("#endregion Constructors", nNest)
			    SELF:EndSection(nNest)
            ENDIF
            coll := oType:GetFields():Where ( isVisible ):ToArray()

            IF coll:Length > 0
			    SELF:BeginSection()
                SELF:AddLine("#region Fields", nNest)
                SELF:AddMemberLines(coll, nNest)

                SELF:AddLine("#endregion Fields", nNest)
			    SELF:EndSection(nNest)
            ENDIF
            coll := oType:GetEvents(TRUE):Where ( isVisible):ToArray()
            IF coll:Length > 0
			    SELF:BeginSection()
                SELF:AddLine("#region Events", nNest)
                SELF:AddMemberLines(coll, nNest)
                SELF:AddLine("#endregion Event", nNest)
			    SELF:EndSection(nNest)
            ENDIF

            coll := oType:GetProperties(TRUE):Where ( isVisible):ToArray()
            IF coll:Length > 0
			    SELF:BeginSection()
                SELF:AddLine("#region Properties", nNest)
                SELF:AddMemberLines(coll, nNest)
                SELF:AddLine("#endregion Properties", nNest)
			    SELF:EndSection(nNest)
            ENDIF

            coll := aMethods:Where( isVisible):ToArray()
            IF coll:Length > 0
			    SELF:BeginSection()
                SELF:AddLine("#region Methods", nNest)
                SELF:AddMemberLines(coll, nNest)
                SELF:AddLine("#endregion Methods", nNest)
			    SELF:EndSection(nNest)
            ENDIF
		END IF
		nNest --

        IF ! isFunctionsClass
		    VAR aNested := oType:XChildren:Where({ t=> t:IsExternalVisible .and. t:Name:IndexOfAny ( <char> {'$','<'}) == -1} ):ToArray()
		    IF aNested:Length != 0
                SELF:AddLine("#region Nested Classes", nNest)
			    FOREACH VAR oChild in aNested
				    SELF:AddLine("" , nNest)
				    IF oChild:Kind == Kind.Delegate
					    SELF:AddDelegate(oChild , nNest + 1, oType:FullName)
				    ELSE
					    SELF:AddType(oChild , nNest + 1, oType:FullName)
				    END IF
                NEXT
                SELF:AddLine("#endregion Nested Classes", nNest)
		    END IF
//
		    SELF:AddLine("END " + oType:KindKeyword, nNest)
        ENDIF

	RETURN

    PROTECTED METHOD AddMemberLines(icoll as IList<IXMemberSymbol>, nNest as INT) AS VOID
        var coll := icoll:ToList()
        coll:Sort( {a,b => String.Compare(a:Prototype, b:Prototype)})
        // static members
        FOREACH item AS XPEMemberSymbol in coll
            if item.Name:IndexOf("$") >= 0
                LOOP
            ENDIF
            if item:Modifiers:HasFlag(Modifiers.Static)
                SELF:GetDoc(item,nNest)
                SELF:AddLine(item:ClassGenText, nNest)
            ENDIF
        NEXT
        // instance members
        FOREACH item AS XPEMemberSymbol in coll
            if item.Name:IndexOf("$") >= 0
                LOOP
            ENDIF
            if !item:Modifiers:HasFlag(Modifiers.Static)
                SELF:GetDoc(item,nNest)
                SELF:AddLine(item:ClassGenText, nNest)
            ENDIF
        NEXT

	PROTECTED METHOD AddDelegate(oType AS XPETypeSymbol , nNest AS INT, cParentName := "" AS STRING) AS VOID

		SELF:AddAttributes(oType , SELF:aLines , nNest)
        var cLine := oType:VisibilityKeyword+" "+oType:ModifiersKeyword+" "+ oType:KindKeyword + " "
        cLine += oType:Prototype
        SELF:AddLine(cLine, nNest)

	RETURN

    PRIVATE CONST AttribName := "Attribute" AS STRING
	PROTECTED METHOD AddAttributes(oInfo AS XPESymbol , aLines AS List<STRING> , nNest AS INT) AS STRING
		LOCAL cRet AS STRING
        FOREACH attribute as CustomAttribute in oInfo:CustomAttributes
            var type   := attribute:AttributeType
            LOCAL strArgs := "" AS STRING
            var args := attribute:ConstructorArguments
            FOREACH arg as CustomAttributeArgument in args
                if strArgs:Length > 0
                    strArgs += ", "
                endif
                var argType  := arg:Type
                var argValue := arg:Value
                if argValue IS String VAR str
                    strArgs += e"\"" + str +e"\""
                else
                    strArgs += argValue:ToString()
                endif
            NEXT
            LOCAL cAttribute as STRING
            cAttribute := "["+type:Name
            if cAttribute:EndsWith(AttribName)
                cAttribute := cAttribute:Substring(0, cAttribute:Length - AttribName:Length)
            endif
            if strArgs:Length > 0
               cAttribute +="("+strArgs+")"
            endif
            cAttribute += "]"
            AddLine(cAttribute + ";" , nNest , aLines)
        NEXT
	RETURN cRet

	// Type:GetInterfaces() returns declared and inherited interfaces, we need declared only
	PROTECTED METHOD GetInterfaces(oType AS XPETypeSymbol) AS STRING
		LOCAL cRet AS STRING
        var interf := oType:Interfaces
        cRet := ""
        FOREACH var interface in oType:Interfaces
            if cRet:Length > 0
                cRet += ", "
            ENDIF
            cRet += interface
        NEXT
	    RETURN cRet


	STATIC CONSTRUCTOR()
		_includehidden := FALSE
		_showbeginnamespace := FALSE
	RETURN


END CLASS


END NAMESPACE // XSharpModel
