//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING LanguageService.SyntaxTree
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING System.Threading.Tasks
USING System.Diagnostics
USING System
USING System.Runtime.InteropServices
USING System
BEGIN NAMESPACE XSharpModel
/// <summary>
/// 1 based TextRange
/// </summary>
[DebuggerDisplay("{DebuggerDisplay(),nq}")];
STRUCTURE TextRange
    // 0 based line numbers and columns
    INITONLY PRIVATE _EndColumn AS LONG
    INITONLY PRIVATE _EndLine AS LONG
    INITONLY PRIVATE _StartColumn AS LONG
    INITONLY PRIVATE _StartLine AS LONG

    // Methods
    CONSTRUCTOR(context AS ParserRuleContext)
        SELF(context:Start:Line, context:Start:Column, context:Stop:Line, context:Stop:Column)
        //

    CONSTRUCTOR(startToken AS IToken, endToken AS IToken)
        SELF:_StartLine     := Math.Max(startToken:Line-1,0)
        SELF:_StartColumn   := Math.Max(startToken:Column,0)
        SELF:_EndLine       := Math.Max(endToken:Line-1,0)
        SELF:_EndColumn     := Math.Max(endToken:Column+endToken:Text:Length-1,0)

    /// <summary>
    /// Create a TextRange
    /// </summary>
    /// <param name="sl">Zero based start line</param>
    /// <param name="sc">Zero based start column</param>
    /// <param name="el">Zero based end line</param>
    /// <param name="ec">Zero based end column</param>
      CONSTRUCTOR(sl AS LONG, sc AS LONG, el AS LONG, ec AS LONG)
         //
         SELF:_StartLine   := Math.Max(sl,0)
         SELF:_StartColumn := Math.Max(sc,0)
         SELF:_EndLine     := Math.Max(el,0)
         SELF:_EndColumn   := Math.Max(ec,0)

    /// <summary>
    /// Return new range that is clone of this range with a different start
    /// </summary>
    /// <param name="startToken">Token to read line and column from</param>
    /// <returns></returns>
    METHOD WithStart(startToken AS IToken) AS TextRange
        RETURN TextRange{startToken:Line-1, startToken:Column, _EndLine, _EndColumn}


    /// <summary>
    /// Return new range that is clone of this range with a different end
    /// </summary>
    /// <param name="startToken">Token to read line and column from</param>
    /// <returns></returns>
    METHOD WithEnd(endToken AS IToken) AS TextRange
        RETURN TextRange{_StartLine,_StartColumn,endToken:Line-1,endToken:Column+endToken:Text:Length}

      METHOD AddLine(line AS INT) AS TextRange
         RETURN TextRange{_StartLine+line,_StartColumn,_EndLine+line, _EndColumn}

    /// <summary>
    /// Return an empty TextRange
    /// </summary>
    /// <value></value>
    STATIC PROPERTY Empty AS TextRange GET TextRange{0, 0, 0, 0}

    /// <summary>
    /// 0 based Start Line
    /// </summary>
    PROPERTY StartLine AS LONG GET SELF:_StartLine
    /// <summary>
    /// 0 based End Line
    /// </summary>
    PROPERTY EndLine AS LONG GET SELF:_EndLine
    /// <summary>
    /// 0 based Start Column
    /// </summary>
    PROPERTY StartColumn AS LONG GET SELF:_StartColumn
    /// <summary>
    /// 0 based End Column
    /// </summary>
    PROPERTY EndColumn AS LONG GET SELF:_EndColumn


    METHOD ContainsExclusive(line AS LONG, col AS LONG) AS LOGIC
        IF ((line > SELF:_StartLine) .AND. (line < SELF:_EndLine))
            RETURN TRUE
        ENDIF
        IF (line == SELF:_StartLine)
            IF (col > SELF:_StartColumn)
                IF (line < SELF:_EndLine)
                    RETURN TRUE
                ENDIF
                IF (line == SELF:_EndLine)
                    RETURN (col < SELF:_EndColumn)
                ENDIF
            ENDIF
            RETURN FALSE
        ENDIF
        RETURN ((line == SELF:_EndLine) .AND. (col < SELF:_EndColumn))

    METHOD ContainsInclusive(line AS LONG, col AS LONG) AS LOGIC
        IF ((line > SELF:_StartLine) .AND. (line < SELF:_EndLine))
            RETURN TRUE
        ENDIF
        IF (line == SELF:_StartLine)
            IF (col >= SELF:_StartColumn)
                //
                IF (line < SELF:_EndLine)
                    RETURN TRUE
                ENDIF
                IF (line == SELF:_EndLine)
                    RETURN (col <= SELF:_EndColumn)
                ENDIF
            ENDIF
            RETURN FALSE
        ENDIF
        RETURN ((line == SELF:_EndLine) .AND. (col <= SELF:_EndColumn))

    METHOD DebuggerDisplay() AS STRING
        RETURN SELF:ToString()

    OVERRIDE METHOD ToString() AS STRING
        RETURN i"{StartLine}.{StartColumn}-{EndLine}.{EndColumn}"

END STRUCTURE

END NAMESPACE

