//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using XSharp.Internal
using System.Collections.Generic
#command GETSTATE <type> <SET>	      =>				 ;
RETURN XSharp.RuntimeState.GetValue\< <type> \>( <SET> )

#command SETSTATE <type> <SET>	<VALUE> =>				 ;
RETURN XSharp.RuntimeState.SetValue\< <type> \>( <SET> , <VALUE> )


/// <summary>
/// Retrieve the setting that determines if TextMerge is active
/// </summary>
/// <param name="lSet"></param>
/// <returns>The current setting</returns>
function SetTextMerge() as logic
    getstate logic Set.TextMerge


/// <summary>
/// Change the setting that determines if TextMerge is active
/// </summary>
/// <param name="lSet">TRUE when Merge must be used for TEXT .. ENDTEXT expressions without TEXTMERGE clause and for the \ and \\ commands.</param>
/// <returns>The current setting</returns>
function SetTextMerge(lSet as logic) as logic
    setstate logic Set.TextMerge lSet


/// <summary>
/// Change the TextMerge delimiters
/// </summary>
/// <param name="cLeft">Left delimiter for expressions.</param>
/// <param name="cRight">Left delimiter for expressions.</param>
/// <returns>The current setting</returns>
function SetTextMergeDelimiters(cLeft as string, cRight as string) as string[]
    return XSharp.RuntimeState.SetValue<string[]>(Set.TextMergeDelimiters, <string>{cLeft, cRight})

/// <summary>
/// Read the TextMerge delimiters
/// </summary>
/// <returns>The current setting</returns>
function SetTextMergeDelimiters() as string[]
    return XSharp.RuntimeState.GetValue<string[]>(Set.TextMergeDelimiters)

/// <summary>
/// Open/Close the file to which TEXT .. ENDTEXT values are written
/// </summary>
/// <param name="cFile">Name of the file to create. When this name is NULL or Empty then the file will be closed when it is open</param>
/// <returns>TRUE when the file was succesfully opened / closed.</returns>
/// <remarks>The file handle for the file will be stored in the _TEXT system variable.</remarks>
/// <seealso cref='_TEXT' />
function SetTextFile(cFile as string) as logic
    local hFile as IntPtr
    if _TEXT != -1
        hFile := IntPtr{_TEXT}
        FClose(hFile)
        _TEXT := -1
    endif
    if !String.IsNullOrEmpty(cFile)
        hFile := FCreate(cFile)
        if FError() == 0
            _TEXT := hFile:ToInt64()
        endif
    endif
    return _TEXT != -1

function __TextOut(cText as string, lNewLine as logic) as void
    FoxTextOut.NoShow := false
    cText := FoxTextOut.MergeLine(cText)
    FoxTextOut.Write(cText, lNewLine)
    return


internal static class FoxTextOut
    static lTextMerge as logic
    static nPreText as long
    static cPreText as string
    static NoShow   as logic
    static Flags    as long
    static DeleteSpaces := false as logic
    static DeleteTabs   := false as logic
    static DeleteCR     := false as logic
    static DeleteLF     := false as logic


    static method Init(lMerge as logic, lNoShow as logic, nFlags as long, uPreText as usual) as void
        lTextMerge := lMerge
        if IsString(uPreText)
            cPreText := uPreText
        else
            cPreText := _PRETEXT
        endif
        if IsNumeric(uPreText)
            nPreText        := uPreText
            DeleteSpaces   := _and(nPreText, 1) == 1
            DeleteTabs     := _and(nPreText, 2) == 2
            DeleteCR       := _and(nPreText, 4) == 4
            DeleteLF       := _and(nPreText, 8) == 8
        else
            nPreText := -1
        endif
        NoShow := lNoShow
        Flags  := nFlags
        return

    static method WriteLine(cText as string) as string
        return Write(cText, true)

    static method Write(cText as string, lNewLine as logic) as string
        var result := System.Text.StringBuilder{cText:Length}
        local lStartOfLine as logic
        lStartOfLine := true
        result:Append(cPreText)
        foreach var cChar in cText
            switch cChar
                case c' '
                    if lStartOfLine .and. DeleteSpaces
                        nop
                    else
                        result:Append(cChar)
                    endif
                case c'\t'
                    if lStartOfLine .and. DeleteTabs
                        nop
                    else
                        result:Append(cChar)
                    endif
                case c'\r'   // CR
                    if DeleteCR
                        nop
                    else
                        result:Append(cChar)
                    endif
                    lStartOfLine := true
                case c'\n'   // LF
                    if DeleteLF
                        nop
                    else
                        result:Append(cChar)
                        result:Append(cPreText)
                    endif
                    lStartOfLine := true
                otherwise
                    result:Append(cChar)
                    lStartOfLine := false
            end switch
        next
        cText := result:ToString()
        if ! NoShow
            if lNewLine
                QOut(cText)
            else
                QQOut(cText)
            endif
        endif
        if _and(Flags ,1) != 1
            // Output to _TEXT
            if _TEXT != -1
                local hFile as IntPtr
                hFile := IntPtr{ _TEXT}
                if NoShow .and. _and(Flags,2) == 2
                    FWrite(hFile, System.Environment.NewLine)
                endif
                FWrite(hFile, cText)
            endif
        endif
        if lNewLine
            result:AppendLine()
            cText := result:ToString()
        endif
        return cText

    static method MergeLine(cText as string) as string
        var aDelim := SetTextMergeDelimiters()
        var pattern := "("+aDelim[1]+")|("+aDelim[2]+")"      // "(<<)|(>>)"
        var aElements := System.Text.RegularExpressions.Regex.Split(cText, pattern)
        if aElements:Length > 1
            // "aaa << bbb >> ccc << ddd >>"
            // should return an array
            // {"aaa ","<<"," bbb ", ">>", " cccc ","<<"," ddd ",">>"}
            // validate that the elements 2,6 etc are "<<"
            // validate that the elements 4, 8 etc are "<<"
            local valid := true as logic
            for var i := 2 to aElements:Length step 4
                if aElements[i] != aDelim[1]
                    valid := false
                elseif i+2 <= aElements:Length
                    if aElements[i+2] != aDelim[2]
                        valid := false
                    endif
                    local cMacro := aElements[i+1] as string
                    local uResult as usual
                    aElements[i]   := ""
                    aElements[i+2] := ""
                    try
                        // the macro compiler does not accept SELF and THIS
                        // we replace this with _THIS
                        // when merging the compiler adds a local _THIS with the value of SELF/This
                        if cMacro:IndexOfAny(<char>{c'.',c':'}) >= 0
                            var pattern2 := "(:)|(\.)"
                            var aTokens := System.Text.RegularExpressions.Regex.Split(cMacro, pattern2)
                            for var j := 1 to aTokens:Length
                                if aTokens[j]:Length == 4
                                    switch aTokens[j]:ToLower()
                                    case "self"
                                    case "this"
                                         aTokens[j] := "_THIS"
                                    end switch
                                endif
                            next
                            cMacro := String.Concat(aTokens)
                        endif
                        uResult := &(cMacro)
                    catch
                        uResult := "** Error in expression (" +cMacro+") **"
                    end try
                    aElements[i+1] := AsString(uResult)
                else
                    valid := false
                endif
            next
            if valid
                cText := String.Concat(aElements)
            endif
        endif
        return cText

    static method TextMerge(cText as string) as string
        var lines := cText:Split(<string>{e"\r\n"},StringSplitOptions.None)
        var result := List<string>{}
        if ! lTextMerge
            result:AddRange(lines)
        else
            foreach var line in lines
                result:Add(MergeLine(line))
            next
        endif
        var sb := System.Text.StringBuilder{cText:Length}
        foreach var line in result
            sb:Append(WriteLine(line))
        next
        return sb:ToString()

end class



function __TextInit(lMerge as logic, lNoShow as logic, nFlags as long, uPreText as usual) as logic
    FoxTextOut.Init(lMerge, lNoShow, nFlags, uPreText)
    return true

function __TextLine(cText as string) as string
    return cText+CRLF


[NeedsAccessToLocals(FALSE)];
function __TextEnd(cString as string) as string
    cString := FoxTextOut.TextMerge(cString)
    if cString:EndsWith(e"\r\n\r\n")
        cString := cString:Substring(0, cString:Length-2)
    endif
    return cString



/// <include file="VFPDocs.xml" path="Runtimefunctions/textmerge/*" />
[NeedsAccessToLocals(FALSE)];
function TextMerge( cExpression , lRecursive , cLeftDelim , cRightDelim) as string clipper
    var delims := SetTextMergeDelimiters()
    if ! IsString(cExpression)
        throw Error.ArgumentError( __function__ , NAMEOF(cExpression), 1, <object>{ cExpression} )
    endif
    if IsNil(lRecursive)
        lRecursive := false
    elseif ! IsLogic(lRecursive)
        throw Error.ArgumentError( __function__ , NAMEOF(lRecursive), 2, <object>{ lRecursive} )
    endif
    if IsNil(cLeftDelim)
        cLeftDelim := delims[1]
    elseif ! IsString(cLeftDelim)
        throw Error.ArgumentError( __function__ , NAMEOF(cLeftDelim), 3, <object>{ cLeftDelim} )
    endif
    if IsNil(cRightDelim)
        cLeftDelim := delims[2]
    elseif ! IsString(cRightDelim)
        throw Error.ArgumentError( __function__ , NAMEOF(cRightDelim), 4, <object>{ cRightDelim} )
    endif
    try
        SetTextMergeDelimiters(cLeftDelim, cRightDelim)
        cExpression := FoxTextOut.TextMerge(cExpression)
    finally
        SetTextMergeDelimiters(delims[1], delims[2])
    end try
    return cExpression
