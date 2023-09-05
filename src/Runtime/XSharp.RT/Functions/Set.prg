//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


/// <summary>
/// Changes and/or reads a system setting.
/// </summary>
/// <param name="nDefine">Is a positive integer identifying a system setting or system variable.
/// This should match the values from the Set enumerated type.</param>
/// <param name="newValue">The optional expression can specify a new value for a system setting.
/// The data type is dependent on the system setting designated by <paramref name="nDefine" />.</param>
/// <returns>When Set() is called without the argument <paramref name="newValue" /> ,
/// the function returns the current system setting designated by <paramref name="nDefine" /> .
/// If <paramref name="newValue" /> is specified, the corresponding system setting is set to <paramref name="newValue" />
/// and the value of the old setting is returned.
/// </returns>
/// <seealso cref="T:XSharp.Set" />
/// <remarks>If you are coming from XHarbour or Xbase++ please don't use set.ch for the value of <paramref name="nDefine" />
/// because there are some differences between the values in this header file and the values used inside X#. </remarks>
FUNCTION Set(nDefine, newValue) AS USUAL CLIPPER
    LOCAL state AS XSharp.RuntimeState
    LOCAL oOld  := NULL AS OBJECT
    LOCAL nSetting AS XSharp.Set
    IF IsString(nDefine)
        local cDefine := nDefine as STRING
        IF ! Enum.TryParse(cDefine, TRUE, OUT nSetting)
            // match start of define ?
            LOCAL found := FALSE AS LOGIC
            FOREACH setting AS STRING IN Enum.GetNames(TYPEOF(XSharp.Set))
                IF setting:StartsWith(cDefine,StringComparison.OrdinalIgnoreCase)
                    Enum.TryParse(setting, TRUE, OUT nSetting)
                    found := TRUE
                    EXIT
                ENDIF
            NEXT
            IF ! found
                THROW Error.ArgumentError(__FUNCTION__,NAMEOF(nDefine), "Invalid argument: "+cDefine+". This could not be translated to a valid setting in the Set enum")
            ENDIF
        ENDIF
    ELSEIF ! IsNumeric(nDefine)
        throw Error.ArgumentError(__FUNCTION__, nameof(nDefine), "Invalid argument: "+AsString(nDefine)+". This should be a number, Set Enum value or a string")
    ELSE
        nSetting := nDefine
    ENDIF


    IF PCount() > 1
        // Some settings are not simply written but have side effects
        // These are handled here
        SWITCH nSetting
        CASE Set.DateFormat
            // Special handling, because we want to keep DateFormat, DateFormatNet and DateFormatEmpty in sync
            IF IsString(newValue)
                RETURN SetDateFormat(newValue)
            ENDIF
        CASE Set.Default
            // Special handling, validation on path for FoxPro and this also resets the path array
            IF IsString(newValue)
                RETURN SetDefault(newValue)
            ENDIF
        CASE Set.Path
            // Special handling, clear path array
            IF IsString(newValue)
                RETURN SetPath(newValue)
            ENDIF
        CASE Set.CharSet
            // Map SET_CHARSET to SetAnsi
            IF IsNumeric(newValue)
                RETURN SetAnsi(newValue == 0)  // Ansi = 0, Oem == 1
            ENDIF
        CASE Set.Ansi
            // Map SET_CHARSET to SetAnsi
            IF IsLogic(newValue)
                RETURN SetAnsi(newValue )
            ENDIF
        CASE Set.Century
            // Keep date format in sync
            IF IsString(newValue)
                RETURN SetCentury(Upper(newValue) == "ON" )
            ELSEIF IsLogic(newValue)
                RETURN SetCentury(newValue)
            ENDIF
        CASE Set.DateCountry
            IF IsNumeric(newValue)
                RETURN SetDateCountry(newValue )
            ENDIF
        CASE Set.DecimalSep
            IF IsNumeric(newValue)
                RETURN SetDecimalSep(newValue)
            ENDIF
        CASE Set.ThousandSep
            IF IsNumeric(newValue)
                RETURN SetThousandSep(newValue)
            ENDIF
        CASE Set.Epoch
            IF IsNumeric(newValue)
                RETURN SetEpoch(newValue)
            ENDIF
        CASE Set.Collation
            IF IsString(newValue) .OR. IsSymbol(newValue)
                RETURN SetCollation(newValue)
            ENDIF
        CASE Set.NatDLL
            IF IsString(newValue) .OR. IsSymbol(newValue)
                RETURN SetNatDLL(newValue)
            ENDIF
        CASE Set.International
            IF IsString(newValue) .OR. IsSymbol(newValue)
                RETURN SetInternational(newValue)
            ENDIF

        END SWITCH
    ENDIF


    state := XSharp.RuntimeState.GetInstance()
    IF state:Settings:ContainsKey(nSetting)
        oOld := state:Settings[nSetting]
    ENDIF
    IF PCount() > 1
        LOCAL oNew := newValue AS OBJECT
        IF oOld != NULL_OBJECT
            IF oOld IS LOGIC .AND. oNew IS STRING VAR cNew
                // XBase++ uses ON / OFF strings in stead of TRUE and FALSE
                state:Settings[nSetting] := cNew:ToUpper() == "ON"
            ELSE
                TRY
                    oNew := System.Convert.ChangeType( oNew, oOld:GetType())
                    state:Settings[nSetting] := oNew
                CATCH
                    NOP // can't convert, so ignore assignment
                END TRY
            ENDIF
        ENDIF
    ENDIF
    RETURN oOld



FUNCTION SetAltFile() AS STRING
    RETURN RuntimeState.AltFile

FUNCTION SetAltFile(cFileName as STRING, lAdditive := FALSE as LOGIC) AS STRING
    var old := RuntimeState.AltFile
    IF String.IsNullOrEmpty(cFileName)
        SetAlternate(FALSE)
        AltFileClose()
    ENDIF
    RuntimeState.AltFile := cFileName
    IF ! String.IsNullOrEmpty(cFileName)
        AltFileOpen(lAdditive)
        SetAlternate(TRUE)
    ENDIF
    RETURN old

FUNCTION SetAlternate(lNewSetting AS LOGIC) AS LOGIC
    VAR lOld := RuntimeState.Alternate
    RuntimeState.Alternate := lNewSetting
    RETURN lOld

FUNCTION SetAlternate() AS LOGIC
    RETURN RuntimeState.Alternate

FUNCTION SetConsole() AS LOGIC
    RETURN RuntimeState.Console

FUNCTION SetConsole(lNewSetting AS LOGIC) AS LOGIC
    VAR lOld := RuntimeState.Console
    RuntimeState.Console := lNewSetting
    RETURN lOld


FUNCTION SetTextFile() AS STRING
    RETURN ConsoleHelpers.TextFile

FUNCTION SetTextFile(cFileName as STRING, lAdditive := FALSE as LOGIC) AS STRING
    var old := ConsoleHelpers.TextFile
    IF String.IsNullOrEmpty(cFileName)
        SetTextOutPut(FALSE)
        TextFileClose()
    ENDIF
    ConsoleHelpers.TextFile := cFileName
    IF ! String.IsNullOrEmpty(cFileName)
        TextFileOpen(lAdditive)
        SetTextOutPut(TRUE)
    ENDIF
    RETURN old

FUNCTION SetTextOutPut(lNewSetting AS LOGIC) AS LOGIC
    VAR lOld := ConsoleHelpers.TextOutPut
    ConsoleHelpers.TextOutPut := lNewSetting
    RETURN lOld


FUNCTION SetTextOutPut() AS LOGIC
    RETURN ConsoleHelpers.TextOutPut


