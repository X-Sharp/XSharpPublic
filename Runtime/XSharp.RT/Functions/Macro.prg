//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System.Reflection
USING System.Globalization

USING XSharp.Internal		
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/evaluate/*" />
/// <seealso cref="T:XSharp.Internal.NeedsAccessToLocalsAttribute" />
[NeedsAccessToLocals];
    FUNCTION Evaluate(cString AS STRING) AS USUAL
RETURN Evaluate(cString, TRUE) 

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/evaluate/*" />	
/// <param name="lAllowSingleQuotes">Should single quotes be allowed as string delimiters.</param>
/// <seealso cref="T:XSharp.Internal.NeedsAccessToLocalsAttribute" />
[NeedsAccessToLocals];
    FUNCTION Evaluate(cString AS STRING, lAllowSingleQuotes AS LOGIC) AS USUAL
    LOCAL oMacro AS XSharp._Codeblock 
    LOCAL uRes   AS USUAL
    oMacro := MCompile(cString, lAllowSingleQuotes)
    IF oMacro != NULL_OBJECT .AND. ! oMacro:IsBlock
        uRes := oMacro:EvalBlock()
    ELSE
        // strange but evaluate on a codeblock returns the block in stead of evaluating it
        uRes := oMacro
    ENDIF
RETURN uRes

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/mcompile/*" />	
/// <remarks>MCompile() allows you to use the macro compiler to compile a string and store the compiled results for later execution.  Instead of invoking the macro compiler each time an expression is evaluated, you could speed up your application by compiling an expression only once and executing the compiled form as often as desired.</remarks>
/// <note type="caution">MCompile returns a STRING in VO. It returns a XSharp._Codeblock in .Net.</note>
/// <seealso cref="T:XSharp._Codeblock" />
FUNCTION MCompile(cString AS STRING) AS XSharp._Codeblock
    RETURN MCompile(cString, TRUE)
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/mcompile/*" />	
/// <param name="lAllowSingleQuotes">Should single quotes be allowed as string delimiters</param>
/// <seealso cref="T:XSharp._Codeblock" />
FUNCTION MCompile(cString AS STRING, lAllowSingleQuotes AS LOGIC) AS XSharp._Codeblock
    VAR oMC := XSharp.RuntimeState.MacroCompiler
    IF oMC != NULL_OBJECT
        VAR oMod := XSharp.RuntimeState.AppModule
        IF oMod == NULL_OBJECT
            XSharp.RuntimeState.AppModule := TYPEOF(XSharp.Core.Functions):Module
        ENDIF
        IF XSharp.RuntimeState:MacroResolver == NULL
            XSharp.RuntimeState:MacroResolver := DefaultMacroAmbigousMatchResolver
        ENDIF
        LOCAL oResult AS XSharp._Codeblock
        //cString := MPrepare(cString)
        IF oMC IS IMacroCompilerUsual VAR oMCU
            oResult := oMCU:CompileCodeblock(cString, lAllowSingleQuotes, oMod)
        ELSE
            LOCAL iResult AS ICodeblock
            iResult := oMC:Compile(cString, lAllowSingleQuotes, oMod, OUT VAR lIsCodeblock, OUT VAR addsMemVars)
            oResult := XSharp._Codeblock{iResult, cString, lIsCodeblock, addsMemVars}
        ENDIF
        RETURN oResult
    ENDIF
    RETURN NULL_OBJECT	
    
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/mexec/*" />	
/// <note type="caution">MCompile returns a STRING containing PCode tokens in VO.
/// It returns a XSharp._Codeblock in .Net. Therefore the parameter of MExec is a Codeblock</note>
/// <seealso cref="T:XSharp._Codeblock" />
/// <seealso cref="O:XSharp.RT.Functions.MCompile" />
/// <seealso cref="T:XSharp.Internal.NeedsAccessToLocalsAttribute" />
[NeedsAccessToLocals];
    FUNCTION MExec(oBlock AS CODEBLOCK) AS USUAL
RETURN Eval(oBlock)




// Copied from the Roslyn Lexer and Macro Lexer
INTERNAL FUNCTION _IsIdentifierStartChar(cChar AS CHAR) AS LOGIC
    IF cChar >= c'A' .AND. cChar <= c'Z'
        RETURN TRUE
    ENDIF        
    IF cChar >= c'a' .AND. cChar <= c'z'
        RETURN TRUE
    ENDIF
    IF cChar == '_'
        RETURN TRUE
    ENDIF
    IF cChar < 0x7F
        // All other lower ascii are not allowed
        RETURN FALSE
    ENDIF
    VAR cat := CharUnicodeInfo.GetUnicodeCategory(cChar)
    SWITCH cat
        CASE UnicodeCategory.UppercaseLetter
        CASE UnicodeCategory.LowercaseLetter
        CASE UnicodeCategory.TitlecaseLetter
        CASE UnicodeCategory.ModifierLetter
        CASE UnicodeCategory.OtherLetter
        CASE UnicodeCategory.LetterNumber
            RETURN TRUE
    END SWITCH
RETURN FALSE

INTERNAL FUNCTION _IsIdentifierPartChar(cChar AS CHAR) AS LOGIC
    IF _IsIdentifierStartChar(cChar)
        RETURN TRUE
    ENDIF
    VAR cat := CharUnicodeInfo.GetUnicodeCategory(cChar)
    SWITCH cat
        CASE UnicodeCategory.DecimalDigitNumber
        CASE UnicodeCategory.ConnectorPunctuation
        CASE UnicodeCategory.NonSpacingMark
        CASE UnicodeCategory.SpacingCombiningMark
            RETURN TRUE
        CASE UnicodeCategory.Format
            RETURN cChar > 127 
    END SWITCH
RETURN FALSE

INTERNAL FUNCTION _IsIdentifier(cName AS STRING) AS LOGIC
    IF cName:Length=0 .OR. ! _IsIdentifierStartChar(cName[0])
        RETURN FALSE
    ENDIF
    FOR VAR nChar := 1 TO cName:Length-1 
        IF ! _IsIdentifierPartChar(cName[nChar])
            RETURN FALSE
        ENDIF
    NEXT
RETURN TRUE

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/type/*" />
/// <seealso cref="T:XSharp.Internal.NeedsAccessToLocalsAttribute" />
[NeedsAccessToLocals];
    FUNCTION Type(cString AS STRING) AS STRING
    LOCAL uValue AS USUAL
    LOCAL cRet	 AS STRING
    IF String.IsNullOrEmpty(cString)	
        cRet := "UE"
    ELSE
        TRY
                // do not use the macro compiler to find "simple" memvar or local names
                IF _IsIdentifier(cString) .AND. MemVarTryGet(cString, OUT uValue)
                    cRet   := ValType(uValue)
                ELSE
                    // Ok, this is not a memvar or local name. Let's evaluate it
                    uValue := Evaluate(cString)
                    cRet   := ValType(uValue)
            ENDIF
        CATCH  AS Exception
            cRet  := "UE" 
        END TRY
    ENDIF
RETURN cRet



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/strevaluate/*" />
/// <seealso cref="T:XSharp.Internal.NeedsAccessToLocalsAttribute" />
[NeedsAccessToLocals];
    FUNCTION StrEvaluate( cString AS STRING ) AS STRING
    IF cString:IndexOf("&") > 0
        LOCAL cVariableName AS STRING
        LOCAL lInVariable   AS LOGIC
        LOCAL evalMacro     AS LOGIC
        LOCAL lAddChar      AS LOGIC
        lInVariable := evalMacro := FALSE
        cVariableName := ""
        VAR sb := System.Text.StringBuilder{cString:Length}
        FOREACH VAR cChar IN cString
            lAddChar := TRUE
            SWITCH cChar
                CASE '&'
                    lInVariable   := TRUE
                    cVariableName := ""
                    lAddChar     := FALSE
                CASE ' '
                CASE '\t'
                    IF lInVariable
                        lInVariable := FALSE
                        evalMacro   := TRUE
                    ENDIF
                CASE '.'
                    IF lInVariable
                        lInVariable := FALSE
                        evalMacro   := TRUE
                        lAddChar     := FALSE
                    ENDIF
                OTHERWISE
                    IF lInVariable
                        IF Char.IsLetterOrDigit(cChar)
                                cVariableName += cChar:ToString()
                            lAddChar     := FALSE
                        ELSE
                            lInVariable := FALSE
                            evalMacro   := TRUE
                        ENDIF
                    ENDIF
            END SWITCH
            IF evalMacro 
                VAR result := StrEvaluateMemVarGet(cVariableName)
                sb:Append(result)
                evalMacro := FALSE
            ENDIF
            IF lAddChar
                sb:Append(cChar)
            ENDIF
        NEXT
        IF lInVariable
            VAR result := StrEvaluateMemVarGet(cVariableName)
            sb:Append(result)
        ENDIF
        cString := sb:ToString()
    ENDIF
RETURN cString


INTERNAL FUNCTION StrEvaluateMemVarGet(cVariableName AS STRING) AS STRING
    TRY
            VAR oMemVar := XSharp.MemVar.PrivateFind(cVariableName)
            IF oMemVar == NULL
                oMemVar := XSharp.MemVar.PublicFind(cVariableName)
            ENDIF
            IF oMemVar != NULL   
                RETURN oMemVar:Value:ToString()
        ENDIF
    CATCH
        // Memvar not found ?
    END TRY
RETURN cVariableName

INTERNAL FUNCTION DefaultMacroAmbigousMatchResolver(m1 AS MemberInfo, m2 AS MemberInfo, args AS System.Type[]) AS LONG
    LOCAL comp1 := GetCompany(m1:Module:Assembly) AS STRING
    LOCAL comp2 := GetCompany(m2:Module:Assembly) AS STRING
    IF comp1 == XSharp.Constants.Company
        IF ! (comp2 == XSharp.Constants.Company)
            RETURN 2
        ENDIF
    ENDIF
    IF comp2 == XSharp.Constants.Company
        IF  ! (comp1== XSharp.Constants.Company)
            RETURN 1
        ENDIF
    ENDIF
    
RETURN 0

INTERNAL FUNCTION GetCompany(asm AS System.Reflection.Assembly) AS STRING
    FOREACH attr AS CustomAttributeData IN asm:CustomAttributes
        IF attr:AttributeType== typeof(AssemblyCompanyAttribute)
            RETURN (STRING) attr:ConstructorArguments[0]:Value
        ENDIF
    NEXT
RETURN NULL


INTERNAL GLOBAL _fullMacroCompiler AS Assembly

/// <include file="VFPDocs.xml" path="Runtimefunctions/execscript/*" />
FUNCTION ExecScript( cExpression, eParameters ) AS USUAL CLIPPER
    LOCAL result := NIL AS USUAL
    IF _fullMacroCompiler == NULL_OBJECT
        
        AssemblyHelper.Load("XSharp.Scripting")
        AssemblyHelper.Load("XSharp.CodeAnalysis")
        _fullMacroCompiler := AssemblyHelper.Load("XSharp.MacroCompiler.Full")
        VAR oImm := AssemblyHelper.FindLoadedAssembly("System.Collections.Immutable")
        IF oImm == NULL_OBJECT
            oImm := Assembly.Load("System.Collections.Immutable, Version=1.2.3.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a")
        ENDIF
        IF oImm == NULL_OBJECT
            VAR oErr := Error{"Could not load 'System.Collections.Immutable'"}
            THROW oErr
        ENDIF
    ENDIF
    IF _fullMacroCompiler != NULL_OBJECT
        LOCAL aFuncs AS MethodInfo[]
        aFuncs := OOPHelpers.FindClipperFunctions(#_ExecScript)
        IF aFuncs:Length == 1                
            result := _CallClipFunc(#_ExecScript,_Args())
        ENDIF
    ENDIF
    RETURN result
    
    
    
