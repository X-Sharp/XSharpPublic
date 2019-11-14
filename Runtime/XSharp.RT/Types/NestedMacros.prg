//
// Copyright (c) XSharp B.V.  All Rights Reserved. 
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System.Collections.Generic
USING System.Text


INTERNAL CLASS NestedMacroHandler

INTERNAL STATIC METHOD IsNested(cTemp AS STRING) AS LOGIC
    LOCAL nCount AS LONG
    LOCAL inString AS LOGIC
    LOCAL endChar AS CHAR
    LOCAL lastChar AS CHAR
    nCount := 0
    inString := FALSE
    endChar  := '\0'
    lastChar := '\0'
    FOREACH VAR c IN cTemp
        SWITCH c
            CASE ' '
            CASE '\t'
                LOOP
            CASE '|' 
                IF ! InString
                    nCount++
                    IF nCount > 2
                        RETURN TRUE
                    ENDIF
                ENDIF
            CASE '"'
            CASE '\''
                IF inString
                    IF endChar == c
                        inString := FALSE
                        endChar := '\0'
                    ENDIF
                ELSE
                    inString := TRUE
                    endChar  := c
                ENDIF
            CASE '['
                IF ! inString
                    IF IsBlockedString(lastchar)
                        inString := TRUE
                        endChar := ']'
                    ENDIF
                ENDIF
            CASE ']'
                IF inString .AND. endChar == c
                    inString := FALSE
                    endChar := '\0'
                ENDIF
        END SWITCH
        lastChar := c
    NEXT
    RETURN FALSE

INTERNAL STATIC METHOD IsBlockedString( cLast AS CHAR) AS LOGIC
    SWITCH cLast
    CASE '}'
    CASE ']'
    CASE ')'
        RETURN FALSE
    END SWITCH
    IF Char.IsLetterOrDigit(cLast)
        RETURN FALSE
    ELSE
        RETURN TRUE
    ENDIF


INTERNAL STATIC METHOD Expand(cMacro AS STRING) AS STRING
    LOCAL sb AS StringBuilder
    LOCAL lastChar AS CHAR
    LOCAL endChar AS CHAR
    LOCAL nestLevel AS LONG
    LOCAL inString AS LOGIC
    sb := StringBuilder{cMacro:Length * 2}
    VAR elements := List<STRING>{}
    lastChar  := '\0'
    nestLevel := 0
    inString  := FALSE
    endChar   := '\0'
    FOREACH VAR c IN cMacro
        sb:Append(c)
        SWITCH c
            CASE ' '
            CASE '\t'
                LOOP
            CASE '"'
            CASE '\''
                IF inString
                    IF endChar == c
                        inString := FALSE
                        endChar := '\0'
                    ENDIF
                ELSE
                    inString := TRUE
                    endChar  := c
                ENDIF                
            CASE '['
                IF ! inString 
                    IF IsBlockedString(lastchar)
                        inString := TRUE
                        endChar := ']'
                    ENDIF
                ENDIF
            CASE '{'
                IF ! inString
                    nestLevel += 1
                ENDIF
            CASE '}'
                IF ! inString
                    nestLevel -= 1
                    IF nestLevel == 1 
                        elements:Add(sb:ToString())
                        sb:Clear()
                    ENDIF
                ENDIF
            CASE '|'
                IF ! inString
                    IF lastChar == '{'
                        IF nestLevel == 1
                            // Do nothing
                        ELSE
                            VAR cPart := sb:ToString()
                            sb:Clear()
                            // Remove the '{|' from cPart and move it back into the stringbuilder
                            cPart := cPart:TrimEnd()
                            cPart := cPart:Substring(0, cPart:Length-1)
                            cPart := cPart:TrimEnd()
                            cPart := cPart:Substring(0, cPart:Length-1)
                            elements:Add(cPart)
                            sb:Append("{|")
                        ENDIF
                    ENDIF
                ENDIF
            OTHERWISE
                NOP
        END SWITCH
        lastChar := c
    NEXT
    elements:Add(sb:ToString())
    IF elements:Count == 1
        RETURN cMacro
    ENDIF
    sb:Clear()
    FOREACH VAR s IN elements
        IF s:StartsWith("{|")
            IF sb:Length == 0
                sb:Append(s)
            ELSE
                sb:Append(e"MCompile( e\"")
                sb:Append(ConvertToExtended(s))
                sb:Append(e"\")")
            ENDIF
        ELSE
            sb:Append(s)
        ENDIF
    NEXT
    RETURN sb:ToString()

INTERNAL STATIC METHOD ConvertToExtended(cMacro AS STRING) AS STRING
    LOCAL sb AS stringbuilder
    LOCAL hasSpecial := FALSE AS LOGIC
    sb := StringBuilder{cMacro:Length*2}
    FOREACH VAR c IN cMacro
        SWITCH c
        CASE '\\'
        CASE '\''
        CASE '"'
            hasSpecial := TRUE
            sb:Append("\")
        END SWITCH
        sb:Append(c)
    NEXT
    IF hasSpecial
        cMacro := sb:ToString()
    ENDIF
    RETURN cMacro
END CLASS


