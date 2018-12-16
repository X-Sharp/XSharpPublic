//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING XSharp
USING System.Collections.Generic
USING System.Text
/// <summary>
/// Get the type of the class that is used to compile macros
/// </summary>
/// <returns>The type of the currently defined MacroCompiler. This may be NULL if no type has been set yet and no macros have been compiled.</returns>
/// <seealso cref="T:XSharp.IMacroCompiler"/>
FUNCTION GetMacroCompiler () AS System.Type
	RETURN XSharp.RuntimeState._macroCompilerType
	
/// <summary>
/// Set the type of the class that must be used to compile macros
/// </summary>
/// <param name="oCompiler">The type of the class that implements the macro compiler. This type MUST implement IMacroCompiler.</param>
/// <returns>The type of the previously defined MacroCompiler. This may be NULL if no type has been set yet and no macros have been compiled.</returns>
/// <seealso cref="T:XSharp.IMacroCompiler"/>
FUNCTION SetMacroCompiler (oCompiler AS System.Type) AS System.Type
VAR old := XSharp.RuntimeState._macroCompilerType
XSharp.RuntimeState._macroCompilerType := oCompiler
XSharp.RuntimeState._macroCompiler := NULL
RETURN old



/// <summary>
/// Set the class that must be used to compile macros
/// </summary>
/// <param name="oCompiler">The object that implements the macro compiler.</param>
/// <returns>The previously defined MacroCompiler. This may be NULL if no compiler has been set yet and no macros have been compiled.</returns>
/// <seealso cref="T:XSharp.IMacroCompiler"/>
FUNCTION SetMacroCompiler (oCompiler AS IMacroCompiler) AS IMacroCompiler
VAR old := XSharp.RuntimeState._macroCompiler
XSharp.RuntimeState._macroCompiler := oCompiler
XSharp.RuntimeState._macroCompilerType := oCompiler:GetType()
RETURN old



INTERNAL CLASS MacroPrecompiler IMPLEMENTS XSharp.IMacroCompiler
    PRIVATE INITONLY originalCompiler AS IMacroCompiler
    CONSTRUCTOR (oCompiler AS IMacroCompiler)
      originalCompiler := oCompiler
    METHOD Compile(macro AS STRING , lAllowSingleQuotes AS LOGIC, module AS System.Reflection.Module, isCodeblock REF LOGIC) AS ICodeblock
        LOCAL newmacro AS STRING
        newmacro := PrepareMacro(macro,lAllowSingleQuotes)

        RETURN originalCompiler:Compile(newmacro, lAllowSingleQuotes, module, REF isCodeblock)
        
    PRIVATE STATIC METHOD PrepareMacro(cMacro AS STRING, lAllowSingleQuotes AS LOGIC) AS STRING
        VAR sbResult := StringBuilder{cMacro:Length * 2} 
        VAR sbId     := StringBuilder{cMacro:Length}
        LOCAL   aParams := NULL AS STRING[]
        LOCAL aElements AS STRING[]
        cMacro := cMacro:Trim()
        IF cMacro:StartsWith("{") .AND. cMacro:EndsWith("}")
            // {|a,b| a+b}"  => 3 elements: "{", "a+b", "a+b}". Maybe more if there is a pipe in a string in the rhs
            aElements := cMacro:Split(<CHAR>{'|'})
            IF aElements:Length >= 3
                sbResult:Append(aElements[0])  // "{"
                sbResult:Append('|')
                sbResult:Append(aElements[1]) // parameters if any
                sbResult:Append('|')
                // remove whilespace from parameters list
                cMacro   := aElements[1]:ToUpper():Replace(" ", ""):Replace(e"\t","")
                aParams  := cMacro:Split(<CHAR>{','},StringSplitOptions.RemoveEmptyEntries)
                cMacro   := aElements[2]
                // remaining pipes were most likely inside strings or syntax errors
                FOR VAR i := 3 TO aElements:Length -1
                    cMacro += "|"
                    cMacro += aElements[i]
                NEXT
            ENDIF
        ELSE
            aParams   := NULL
            aElements := <STRING>{cMacro}
        ENDIF
            
        // make sure we remove the _FIELD-> from the existing key
        cMacro := cMacro:Replace("_FIELD->","")
        LOCAL lInString := FALSE AS LOGIC
        LOCAL delimChar := '\0' AS CHAR
        LOCAL lInComplexName := FALSE AS LOGIC
        LOCAL lastWasZero := FALSE AS LOGIC
        LOCAL lHex        := FALSE AS LOGIC
        FOREACH cChar AS CHAR IN cMacro
#region Handle Strings inside macros            
            IF lInString
                sbResult:Append(cChar)
                IF cChar == delimChar
                    // end of string
                    lInString := FALSE
                ENDIF
                LOOP
            ENDIF
            IF cChar == '"'
                // start of double quoted string
                lInComplexName := FALSE
                lInString := TRUE
                delimChar := cChar
                GetIdName(sbResult, sbId, aParams)
                sbResult:Append(cChar)
                LOOP
            ENDIF
            IF cChar == '\'' .AND. lAllowSingleQuotes
                // start of singlequoted string
                lInComplexName := FALSE
                lInString := TRUE
                delimChar := cChar
                GetIdName(sbResult, sbId, aParams)
                sbResult:Append(cChar)
                LOOP
            ENDIF
#endregion           
            IF Char.IsLetter(cChar) .OR. cChar = '_'
                // add char to id
                IF lastWasZero
                    IF cChar:ToString() $ "XxBb"
                        sbResult:Append(cChar)
                        lHex := cChar:ToString() $ "Xx"
                    ENDIF
                ELSEIF lHex .AND. cChar:ToString() $ "AaBbCcDdEeFf"
                    sbResult:Append(cChar)
                ELSE
                    sbId:Append(cChar)
                    lHex := FALSE
                ENDIF
            ELSEIF Char.IsDigit(cChar)
                // after letter or _ then it is part of the id
                IF sbid:Length > 0
                    sbId:Append(cChar)
                ELSE
                    sbResult:Append(cChar)
                ENDIF
            ELSEIF cChar == '(' .OR. cChar == '{'
                lInComplexName := FALSE
                IF sbid:Length > 0
                    // function or constructor call, no _FIELD prefix
                    sbResult:Append(sbId:ToString())
                    sbId:Clear()
                ENDIF
                sbResult:Append(cChar)
            ELSEIF cChar == '.' .OR. cChar == ':' // namespace delimiter or send operator
                lInComplexName := TRUE
                IF sbid:Length > 0
                    // function or constructor call, no _FIELD prefix
                    sbResult:Append(sbId:ToString())
                    sbId:Clear()
                ENDIF
                sbResult:Append(cChar)
            ELSEIF lInComplexName
                sbResult:Append(sbId:ToString())
                sbId:Clear()
                sbResult:Append(cChar)
                lInComplexName := FALSE
            ELSEIF cChar == ' ' .OR. cChar == '\t'
                IF sbId:Length > 0
                    sbId:Append(cChar)
                ELSE
                    sbResult:Append(cChar)
                ENDIF
            ELSE
                // another non character or digit, so add name if it exists
                GetIdName(sbResult, sbId, aParams)
                sbResult:Append(cChar)
            ENDIF
            IF cChar == '0' .AND. ! lHex
                lastWasZero := TRUE
            ELSE
                IF lastwaszero .AND. cChar:ToString() $ "Xx"
                    lHex := TRUE
                ELSEIF lHex .AND. cChar:ToString() $ "AaBbCcDdEeFf"
                    NOP
                ELSE
                    lHex := FALSE
                ENDIF
                lastwasZero := FALSE
            ENDIF
        NEXT
        // when we end with an id then append to the end of the result
        GetIdName(sbResult, sbID, aParams)
        RETURN sbResult:ToString()
        
    STATIC METHOD GetIdName(sbResult AS StringBuilder, sbId AS StringBuilder, aParams AS STRING[]) AS VOID
        IF sbid:Length > 0
            VAR sName := sbid:ToString():ToUpper()
            IF aParams == NULL .OR. Array.IndexOf(aParams, sName) < 0
                SWITCH sName
                CASE "NIL"
                CASE "NULL_ARRAY"
                CASE "NULL_CODEBLOCK"
                CASE "NULL_DATE"
                CASE "NULL_OBJECT"
                CASE "NULL_PSZ"
                CASE "NULL_PTR"
                CASE "NULL_STRING"
                CASE "NULL_SYMBOL"
                    NOP
                OTHERWISE
                    sbResult:Append("_FIELD->")
                END SWITCH
            ENDIF
            sbResult:Append(sbid:ToString())
            sbId:Clear()
        ENDIF
        RETURN 
        
END CLASS
