//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Collections.Generic
USING System.Threading
USING System.Diagnostics
USING System.Reflection
USING XSharp.RDD
USING XSharp.RDD.Enums
USING System.Text


BEGIN NAMESPACE System.Runtime.CompilerServices
    INTERNAL STATIC CLASS IsExternalInit
    END CLASS
END NAMESPACE

/// <include file="XSharp.Core.Docs.xml" path="doc/XSharp.StateChanged/*" />
DELEGATE XSharp.StateChanged (e AS StateChangedEventArgs) AS VOID
/// <include file="XSharp.Core.Docs.xml" path="doc/XSharp.DialectChanged/*" />
DELEGATE XSharp.DialectChanged(oldDialect as XSharpDialect, newDialect as XSharpDialect) AS VOID

/// <include file="XSharp.Core.Docs.xml" path="doc/StateChangedEventArgs/*" />
CLASS XSharp.StateChangedEventArgs
    /// <include file="XSharp.Core.Docs.xml" path="doc/StateChangedEventArgs.Setting/*" />
    PROPERTY Setting  AS XSharp.Set AUTO GET INIT
    /// <include file="XSharp.Core.Docs.xml" path="doc/StateChangedEventArgs.OldValue/*" />
    PROPERTY OldValue AS OBJECT AUTO GET INIT
    /// <include file="XSharp.Core.Docs.xml" path="doc/StateChangedEventArgs.NewValue/*" />
    PROPERTY NewValue AS OBJECT AUTO GET INIT
    INTERNAL CONSTRUCTOR()
        RETURN
END CLASS
/// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState/*" />
CLASS XSharp.RuntimeState

	// Static Fields
    PRIVATE INITONLY STATIC _onWindows AS LOGIC
	PRIVATE INITONLY STATIC _initialState  AS RuntimeState
	PRIVATE INITONLY _thread AS Thread
	PRIVATE STATIC _shutdown := FALSE AS LOGIC  // To prevent creating state when shutting down
	// Static Methods and Constructor
	PRIVATE STATIC currentState := ThreadLocal<RuntimeState>{ {=>  _initialState:Clone()},TRUE }  AS ThreadLocal<RuntimeState>
    STATIC CONSTRUCTOR
        #ifdef NET5_0_OR_GREATER
        // Next line is needed for codepage support
            Encoding.RegisterProvider(CodePagesEncodingProvider.Instance)
        #endif

        AutoLock        := NULL
        AutoUnLock      := NULL
        MacroCompilerErrorHandler := NULL
        detectDialect()
        SWITCH System.Environment.OSVersion:Platform
        CASE System.PlatformID.Win32NT
        CASE System.PlatformID.Win32S               // No longer in use
        CASE System.PlatformID.Win32Windows         // No longer in use
        CASE System.PlatformID.WinCE                // No longer in use
            _onWindows := TRUE
        OTHERWISE
            _onWindows := FALSE
        END SWITCH
        // This depends on the _onWindow flag so it must be last
        _initialState	:= RuntimeState{TRUE}



    PRIVATE STATIC METHOD detectDialect() AS VOID
        LOCAL asm := Assembly.GetEntryAssembly() AS Assembly
        VAR att := TYPEOF( XSharp.Internal.CompilerVersionAttribute )
        IF asm != NULL .AND. asm:IsDefined(att, FALSE)
            FOREACH VAR attr IN asm:GetCustomAttributes(att, FALSE)
                VAR compilerversion := (XSharp.Internal.CompilerVersionAttribute) attr
                VAR vers := compilerversion:Version
                var pos := vers:IndexOf("dialect:")
                if pos >= 0
                    vers := vers:Substring(pos + 8)
                    SWITCH vers:ToLower()
                    CASE "vo"
                        Dialect := XSharpDialect.VO
                    CASE "vulcan"
                        Dialect := XSharpDialect.Vulcan
                    CASE "core"
                        Dialect := XSharpDialect.Core
                    CASE "foxpro"
                        Dialect := XSharpDialect.FoxPro
                    CASE "harbour"
                        Dialect := XSharpDialect.Harbour
                    CASE "xpp"
                        Dialect := XSharpDialect.XPP
                    END SWITCH
                endif
            NEXT
        ENDIF

    PUBLIC STATIC PROPERTY RunningOnWindows as LOGIC GET _onWindows

 /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.GetInstance/*" />
	PUBLIC STATIC METHOD GetInstance() AS RuntimeState
		RETURN currentState:Value
    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.StateChanged/*" />
    PUBLIC STATIC EVENT StateChanged AS StateChanged
    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.DialectChanged/*" />
    PUBLIC STATIC EVENT DialectChanged as DialectChanged
    [DebuggerBrowsable(DebuggerBrowsableState.Never)];
	PRIVATE oSettings AS Dictionary<XSharp.Set, OBJECT>
    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.Settings/*" />
    PUBLIC PROPERTY Settings AS Dictionary<XSharp.Set, OBJECT> GET oSettings
	PRIVATE CONSTRUCTOR(initialize AS LOGIC)
		VAR oThread := Thread.CurrentThread
        SELF:_thread := oThread
		oSettings := Dictionary<XSharp.Set, OBJECT>{}
		IF initialize
			SELF:BreakLevel := 0
            // Set default values
            FOREACH nSet AS XSharp.Set IN System.Enum.GetValues(typeof(XSharp.Set))
                VAR def := RuntimeStateDefaultValue(nSet)
                IF def != NULL
                    SELF:_SetThreadValue(nSet, def)
                ENDIF
            NEXT
            // Some values that are not in the dictionary
            RuntimeState.FileException := NULL
            RuntimeState.FileError     := 0
            RuntimeState.LastRddError  := NULL

            SELF:_SetThreadValue<Exception>(Set.Patharray,NULL)
            SELF:_SetThreadValue<BYTE[]>(Set.CollationTable, NULL )
			IF RuntimeState.RunningOnWindows
                SELF:_SetThreadValue(Set.DosCodepage, Win32.GetDosCodePage())
                SELF:_SetThreadValue(Set.WinCodepage, Win32.GetWinCodePage())
            ELSE
                SELF:_SetThreadValue(Set.DosCodepage, 437L)  // US American
                SELF:_SetThreadValue(Set.WinCodepage, 1252L) // Latin 1 / Western Europe
                SELF:_SetThreadValue(Set.CollationMode, CollationMode.Unicode )
            ENDIF
			// Date and time settings
			SELF:_SetInternationalWindows()


		ENDIF
		RETURN
    /// <exclude />
	DESTRUCTOR()
		// What do we need to clean ?
        IF SELF == _initialState
            _shutdown := TRUE
        ENDIF


	PRIVATE METHOD Clone() AS RuntimeState
		LOCAL oNew AS RuntimeState
        IF Thread.CurrentThread == _initialState:_thread .OR. _shutdown
            RETURN _initialState
        ENDIF
		oNew := RuntimeState{FALSE}
		BEGIN LOCK oSettings
			// Copy all values from Current State to New state
			FOREACH VAR element IN oSettings
				oNew:oSettings[element:Key] := element:Value
			NEXT
		END LOCK
		RETURN oNew

 /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.Name/*" />
	PUBLIC PROPERTY Name AS STRING
    GET
        IF SELF:_thread == NULL
            RETURN "ThreadState for Unknown Thread"
        ELSE
            RETURN "ThreadState for Thread "+SELF:_thread:ManagedThreadId:ToString()
        ENDIF
    END GET
    END PROPERTY
 /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.BreakLevel/*" />
	PUBLIC PROPERTY BreakLevel AS INT AUTO
 /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.ToString/*" />
	PUBLIC OVERRIDE METHOD ToString() AS STRING
		RETURN SELF:Name

	/// <summary>Retrieve a value from the state of the current Thread.</summary>
	/// <param name="nSetting">Setting number to retrieve. Must be defined in the SET enum.</param>
	/// <typeparam name="T">The return type expected for this setting.</typeparam>
	/// <returns>The current value, or a default value of type T.</returns>
    [MethodImpl(MethodImplOptions.AggressiveInlining)];
	PUBLIC STATIC METHOD GetValue<T> (nSetting AS XSharp.Set) AS T
        IF _shutdown
            // There is no RuntimeState when shutting down
            RETURN DEFAULT(T)
        ENDIF
		RETURN currentState:Value:_GetThreadValue<T>(nSetting);

	/// <summary>Set a value for the state of the current Thread.</summary>
	/// <param name="nSetting">Setting number to retrieve. Must be defined in the SET enum.</param>
	/// <param name="oValue">The new value for the setting.</param>
	/// <typeparam name="T">The return type expected for this setting.</typeparam>
	/// <returns>The previous value, or a default value of type T when the setting was not yetr defined.</returns>
    [MethodImpl(MethodImplOptions.AggressiveInlining)];
	PUBLIC STATIC METHOD SetValue<T> (nSetting AS XSharp.Set, oValue AS T) AS T
        IF _shutdown
            // Suppress updating RuntimeState when shutting down
            RETURN oValue
        ENDIF
		RETURN currentState:Value:_SetThreadValue<T>(nSetting, oValue)
    [MethodImpl(MethodImplOptions.AggressiveInlining)];
	PRIVATE METHOD _GetThreadValue<T> (nSetting AS XSharp.Set) AS T
		BEGIN LOCK oSettings
            IF oSettings:TryGetValue(nSetting, OUT VAR result)
				RETURN (T) result
			ENDIF
		END LOCK
		RETURN DEFAULT(T)
    [MethodImpl(MethodImplOptions.AggressiveInlining)];
	PRIVATE METHOD _SetThreadValue<T>(nSetting AS XSharp.Set, oValue AS T) AS T
		LOCAL result AS T
        BEGIN LOCK oSettings
            LOCAL oResult AS object
			IF oSettings:TryGetValue(nSetting, OUT oResult)
				result := (T) oResult
			ELSE
				result := DEFAULT(T)
			ENDIF
            IF StateChanged != NULL
                VAR args := StateChangedEventArgs{} {Setting := nSetting, OldValue := oResult, NewValue := oValue}
                StateChanged( args )
            ENDIF
			oSettings[nSetting] := oValue
		END LOCK
		RETURN	result

	#region properties FROM the Vulcan RuntimeState that are emulated

 /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.CompilerOptionFox2/*" />
	STATIC PROPERTY CompilerOptionFox2 AS LOGIC AUTO

 /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.CompilerOptionAllowOldStyleAssignments/*" />
	STATIC PROPERTY CompilerOptionAllowOldStyleAssignments AS LOGIC AUTO
 /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.CompilerOptionAZ/*" />
	STATIC PROPERTY CompilerOptionAZ AS LOGIC AUTO
 /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.CompilerOptionVO4/*" />
	STATIC PROPERTY CompilerOptionVO4 AS LOGIC AUTO
 /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.CompilerOptionVO6/*" />
	STATIC PROPERTY CompilerOptionVO6 AS LOGIC AUTO
 /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.CompilerOptionVO7/*" />
	STATIC PROPERTY CompilerOptionVO7 AS LOGIC AUTO
 /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.CompilerOptionVO10/*" />
	STATIC PROPERTY CompilerOptionVO10 AS LOGIC AUTO
 /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.CompilerOptionVO11/*" />
	STATIC PROPERTY CompilerOptionVO11 AS LOGIC AUTO

 /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.CompilerOptionVO12/*" />
	STATIC PROPERTY CompilerOptionVO12 AS LOGIC AUTO
 /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.CompilerOptionVO13/*" />
	STATIC PROPERTY CompilerOptionVO13 AS LOGIC AUTO
 /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.CompilerOptionVO14/*" />
	STATIC PROPERTY CompilerOptionVO14 AS LOGIC AUTO
    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.CurrentWorkarea/*" />
    STATIC PROPERTY CurrentWorkarea AS DWORD ;
        GET Workareas:CurrentWorkareaNO ;
        SET Workareas:CurrentWorkareaNO  := value

 /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.CompilerOptionOVF/*" />
	STATIC PROPERTY CompilerOptionOVF AS LOGIC AUTO

    /// <summary>The current compiler setting for the X# Dialect.</summary>
    /// <include file="CoreComments.xml" path="Comments/CompilerOptions/*" />
    /// <value>The default value for the Dialect is 'Core'.</value>
    /// <remarks>When the dialect changes then registered DialectChanged Event Handlers will be called </remarks>
    /// <seealso cref="DialectChanged" />
    STATIC PRIVATE _dialect := XSharpDialect.Core AS XSharpDialect
	STATIC PROPERTY Dialect AS XSharpDialect
        GET
            RETURN _dialect
        END GET
        SET
            var old := _dialect
            _dialect := value
            IF DialectChanged != NULL .and. old != value
                DialectChanged(old, value)
            ENDIF
        END SET
    END PROPERTY

 /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.CompilerOptionFOVF/*" />
	STATIC PROPERTY CompilerOptionFOVF AS LOGIC AUTO

    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.AppModule/*" />
    STATIC PROPERTY AppModule AS  Module AUTO
	#endregion

    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.LastFound/*" />
    STATIC PROPERTY LastFound AS STRING ;
        GET GetValue<STRING>(Set.LastFound);
        SET SetValue<STRING>(Set.LastFound, value)

    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.FileError/*" />
    STATIC PROPERTY FileError AS DWORD AUTO

    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.FileException/*" />
    STATIC PROPERTY FileException AS Exception AUTO

   /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.Alternate/*" />
   static property Alternate as logic ;
        GET GetValue<LOGIC>(Set.Alternate);
        SET SetValue<LOGIC>(Set.Alternate, value)

   /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.AltFile/*" />
   STATIC PROPERTY AltFile AS STRING ;
        GET GetValue<STRING>(Set.AltFile);
        SET SetValue<STRING>(Set.AltFile, value)

   STATIC PROPERTY MemoWidth AS LONG ;
        GET GetValue<LONG>(Set.MemoWidth);
        SET SetValue<LONG>(Set.MemoWidth, value)

    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.Ansi/*" />
    STATIC PROPERTY Ansi AS LOGIC ;
        GET GetValue<LOGIC>(Set.Ansi);
        SET SetValue<LOGIC>(Set.Ansi, value)

    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.AutoOrder/*" />
    STATIC PROPERTY AutoOrder AS LONG ;
        GET GetValue<LONG>(Set.AutoOrder);
        SET SetValue<LONG>(Set.AutoOrder, value)

    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.AutoOpen/*" />
    STATIC PROPERTY AutoOpen AS LOGIC ;
        GET GetValue<LOGIC>(Set.AutoOpen);
        SET SetValue<LOGIC>(Set.AutoOpen, value)

    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.AutoShareMode/*" />
    STATIC PROPERTY AutoShareMode AS LONG ;
        GET (LONG) GetValue<AutoShareMode>(Set.Autoshare);
        SET SetValue<AutoShareMode>(Set.Autoshare, (AutoShareMode)value)


   /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.Compatible/*" />
   STATIC PROPERTY Compatible AS LOGIC ;
        GET GetValue<LOGIC>(Set.Compatible);
        SET SetValue<LOGIC>(Set.Compatible, VALUE)

   /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.Console/*" />
   STATIC PROPERTY Console AS LOGIC ;
        GET GetValue<LOGIC>(Set.Console);
        SET SetValue<LOGIC>(Set.Console, value)

   /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.Century/*" />
   STATIC PROPERTY Century AS LOGIC ;
        GET GetValue<LOGIC>(Set.Century);
        SET SetValue<LOGIC>(Set.Century, value)

   /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.CollationMode/*" />
   static property CollationMode as CollationMode
        get
			return GetValue<CollationMode>(Set.CollationMode)
		END GET
        SET
			SetValue<CollationMode>(Set.CollationMode, value)
			if OnCollationChanged != null
				OnCollationChanged(GetInstance(), EventArgs{})
			ENDIF
		END SET
	END PROPERTY

   /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.DateCountry/*" />
   STATIC PROPERTY DateCountry AS DWORD ;
        GET (DWORD) GetValue<XSharp.DateCountry>(Set.DateCountry);
        SET RuntimeState.GetInstance():_SetDateCountry( (XSharp.DateCountry) value)

    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.DateFormat/*" />
    STATIC PROPERTY DateFormat AS STRING ;
        GET GetValue<STRING>(Set.DateFormat);
        SET RuntimeState.GetInstance():_SetDateFormat(value)

 /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.NullDateString/*" />
	STATIC PROPERTY NullDateString AS STRING GET GetValue<STRING>(Set.DateFormatEmpty)

    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.Decimals/*" />
    STATIC PROPERTY Decimals AS DWORD ;
        GET GetValue<DWORD>(Set.Decimals);
        SET SetValue<DWORD>(Set.Decimals, value)


    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.DecimalSep/*" />
    STATIC PROPERTY DecimalSep AS DWORD ;
        GET GetValue<DWORD>(Set.DecimalSep);
        SET SetValue<DWORD>(Set.DecimalSep, value)

    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.DefaultRDD/*" />
    STATIC PROPERTY DefaultRDD AS STRING ;
        GET GetValue<STRING>(Set.DefaultRdd);
        SET SetValue<STRING>(Set.DefaultRdd, value)


    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.Deleted/*" />
    STATIC PROPERTY Deleted AS LOGIC ;
        GET GetValue<LOGIC>(Set.Deleted);
        SET SetValue<LOGIC>(Set.Deleted, value)

    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.DelimRDD/*" />
    STATIC PROPERTY DelimRDD AS STRING ;
        GET GetValue<STRING>(Set.DelimRDD);
        SET SetValue<STRING>(Set.DelimRDD, value)

    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.FieldDelimiter/*" />
    STATIC PROPERTY FieldDelimiter AS STRING ;
        GET GetValue<STRING>(Set.FieldDelimiter);
        SET SetValue<STRING>(Set.FieldDelimiter, value)

    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.StringDelimiter/*" />
    STATIC PROPERTY StringDelimiter AS STRING ;
        GET GetValue<STRING>(Set.Delimiters);
        SET SetValue<STRING>(Set.Delimiters, value)

    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.Digits/*" />
    STATIC PROPERTY Digits AS DWORD ;
        GET GetValue<DWORD>(Set.Digits);
        SET SetValue<DWORD>(Set.Digits, value)

    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.DigitsFixed/*" />
    STATIC PROPERTY DigitsFixed AS LOGIC ;
        GET GetValue<LOGIC>(Set.DigitFixed);
        SET SetValue<LOGIC>(Set.DigitFixed, value)


    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.DosCodePage/*" />
    STATIC PROPERTY DosCodePage AS LONG
        GET
            RETURN GetValue<LONG>(Set.DosCodepage)
		END GET
        SET
			SetValue<LONG>(Set.DosCodepage, VALUE)
			IF OnCodePageChanged != NULL
				OnCodePageChanged(GetInstance(), EventArgs{})
			ENDIF
		END SET
	END PROPERTY

    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.Eof/*" />
    STATIC PROPERTY Eof AS LOGIC ;
        GET GetValue<LOGIC>(Set.Eof);
        SET SetValue<LOGIC>(Set.Eof, value)


    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.Eol/*" />
    STATIC PROPERTY Eol AS STRING ;
        GET GetValue<STRING>(Set.Eol);
        SET SetValue<STRING>(Set.Eol, value)



    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.Epoch/*" />
    STATIC PROPERTY Epoch AS DWORD ;
        GET Convert.ToUInt32(GetValue<OBJECT>(Set.Epoch));
        SET SetValue<DWORD>(Set.Epoch, value)

    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.EpochYear/*" />
    STATIC PROPERTY EpochYear AS DWORD ;
        GET Convert.ToUInt32(GetValue<OBJECT>(Set.EpochYear));

    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.EpochCent/*" />
    STATIC PROPERTY EpochCent AS DWORD ;
        GET Convert.ToUInt32(GetValue<OBJECT>(Set.EpochCent));


    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.Exact/*" />
    static property Exact as logic ;
        get GetValue<logic>(Set.Exact);
        set SetValue<logic>(Set.Exact, value)

    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.Fixed/*" />
    STATIC PROPERTY Fixed AS LOGIC ;
        GET GetValue<LOGIC>(Set.Fixed);
        SET SetValue<LOGIC>(Set.Fixed, value)



   /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.FloatDelta/*" />
   STATIC PROPERTY FloatDelta AS REAL8 ;
        GET GetValue<REAL8>(Set.Floatdelta);
        SET SetValue<REAL8>(Set.Floatdelta, value)

     /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.International/*" />
     STATIC PROPERTY International AS CollationMode ;
        GET GetValue<CollationMode>(Set.Intl);
        SET SetValue<CollationMode>(Set.Intl, value)

    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.LastRddError/*" />
    STATIC PROPERTY LastRddError AS Exception AUTO

    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.LastScriptError/*" />
    STATIC PROPERTY LastScriptError AS Exception ;
        GET GetValue<Exception>(Set.LastScriptError);
        SET SetValue<Exception>(Set.LastScriptError, value)

    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.LockTries/*" />
    STATIC PROPERTY LockTries AS DWORD ;
        GET Convert.ToUInt32(GetValue<OBJECT>(Set.Locktries));
        SET SetValue<DWORD>(Set.Locktries, value)

    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.HPLocking/*" />
    STATIC PROPERTY HPLocking AS LOGIC ;
        GET GetValue<LOGIC>(Set.HpLocking);
        SET SetValue<LOGIC>(Set.HpLocking, value)

    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.NewIndexLock/*" />
    STATIC PROPERTY NewIndexLock AS LOGIC ;
        GET GetValue<LOGIC>(Set.NewIndexLock);
        SET SetValue<LOGIC>(Set.NewIndexLock, value)


    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.MemoBlockSize/*" />
    STATIC PROPERTY MemoBlockSize AS WORD;
        GET Convert.ToUInt16(GetValue<OBJECT>(Set.MemoBlockSize));
        SET SetValue<WORD>(Set.MemoBlockSize, value)


    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.NetErr/*" />
    STATIC PROPERTY NetErr AS LOGIC;
        GET GetValue<LOGIC>(Set.Neterr);
        SET SetValue<LOGIC>(Set.Neterr, value)

    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.Optimize/*" />
    STATIC PROPERTY Optimize AS LOGIC ;
        GET GetValue<LOGIC>(Set.Optimize);
        SET SetValue<LOGIC>(Set.Optimize, value)


   /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.Refresh/*" />
   STATIC PROPERTY Refresh AS REAL8 ;
       GET GetValue<REAL8>(Set.Refresh);
       SET SetValue<REAL8>(Set.Refresh, value)

    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.Safety/*" />
    STATIC PROPERTY Safety AS LOGIC ;
        GET GetValue<LOGIC>(Set.Safety);
        SET SetValue<LOGIC>(Set.Safety, value)

    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.SoftSeek/*" />
    STATIC PROPERTY SoftSeek AS LOGIC ;
        GET GetValue<LOGIC>(Set.Softseek);
        SET SetValue<LOGIC>(Set.Softseek, value)

    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.ThousandSep/*" />
    STATIC PROPERTY ThousandSep AS DWORD ;
        GET Convert.ToUInt32(GetValue<OBJECT>(Set.ThousandSep));
        SET SetValue<DWORD>(Set.ThousandSep, value)


    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.Unique/*" />
    STATIC PROPERTY Unique AS LOGIC ;
        GET GetValue<LOGIC>(Set.Unique);
        SET SetValue<LOGIC>(Set.Unique, value)

    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.WinCodePage/*" />
    STATIC PROPERTY WinCodePage AS LONG
	GET
		RETURN GetValue<LONG>(Set.WinCodepage)
	END GET
	SET
        SetValue<LONG>(Set.WinCodepage, value)
		IF OnCodePageChanged != NULL
			OnCodePageChanged(GetInstance(), EventArgs{})
		ENDIF
	END SET
	END PROPERTY

    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.WinEncoding/*" />
    STATIC PROPERTY WinEncoding AS System.Text.Encoding ;
        GET System.Text.Encoding.GetEncoding(WinCodePage)

    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.DosEncoding/*" />
    STATIC PROPERTY DosEncoding AS System.Text.Encoding ;
        GET System.Text.Encoding.GetEncoding(DosCodePage)


    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.NoMethod/*" />
    STATIC PROPERTY NoMethod AS STRING ;
        GET GetValue<STRING>(Set.NoMethod);
        SET SetValue<STRING>(Set.NoMethod, value)


    INTERNAL METHOD _SetInternationalClipper() AS VOID
        SELF:_SetThreadValue<DWORD>(Set.DecimalSep, 46)		// DOT .
        SELF:_SetThreadValue<DWORD>(Set.ThousandSep,44)	// COMMA ,
        SELF:_SetThreadValue(Set.Intl, CollationMode.Clipper)
        SELF:_SetThreadValue(Set.Dict, FALSE)
        SELF:_SetThreadValue(Set.Century, FALSE)
        SELF:_SetDateCountry(XSharp.DateCountry.American)
        SELF:_SetTimeFormat("hh:MM:SS")

    INTERNAL METHOD _SetInternationalWindows() AS VOID
        VAR numberformat := System.Globalization.NumberFormatInfo.CurrentInfo
        SELF:_SetThreadValue<DWORD>(Set.DecimalSep, numberformat:NumberDecimalSeparator[0])
        SELF:_SetThreadValue<DWORD>(Set.ThousandSep, numberformat:NumberGroupSeparator[0])
        SELF:_SetThreadValue<DWORD>(Set.Epoch, 1910)
        SELF:_SetThreadValue<DWORD>(Set.EpochYear, 10)
        SELF:_SetThreadValue<DWORD>(Set.EpochCent, 2000)
        SELF:_SetThreadValue(Set.Intl, CollationMode.Windows)
        SELF:_SetThreadValue(Set.Dict, TRUE)
        // FoxPro settings that do not hurt in other dialects
        SELF:_SetDateFormatSystem()
        SELF:_SetTimeFormatSystem()
		RETURN


    INTERNAL METHOD _SetTimeFormat(format AS STRING) AS VOID
        IF String.IsNullOrEmpty(format)
            SELF:_SetTimeFormatSystem()
        ELSE
            format := format:ToUpper()
            IF format:EndsWith("TT")
		        SELF:_SetThreadValue(Set.AmExt, "AM")
		        SELF:_SetThreadValue(Set.PmExt, "PM")
		        SELF:_SetThreadValue(Set.AmPm, TRUE)
                SELF:_SetThreadValue(Set.Hours, 12L)
            ELSE
		        SELF:_SetThreadValue(Set.AmExt, "")
		        SELF:_SetThreadValue(Set.PmExt, "")
		        SELF:_SetThreadValue(Set.AmPm, FALSE)
                SELF:_SetThreadValue(Set.Hours, 24L)
            ENDIF
            SELF:_SetThreadValue(Set.Timesep, Asc(SubStr3(format, 3,1)))
            SELF:_SetThreadValue(Set.Timeformat, format)
       ENDIF

	INTERNAL METHOD _SetDateFormat(format AS STRING) AS VOID
		format := format:ToUpper()
		// ensure we have dd, mm and yy
		IF format:IndexOf("DD") == -1 .OR. format:IndexOf("MM") == -1 .OR. format:IndexOf("YY") == -1
			RETURN
		ENDIF
		SELF:_SetThreadValue(Set.DateFormatNet, format:Replace("D","d"):Replace("Y","y"):Replace("/","'/'"))
		SELF:_SetThreadValue(Set.DateFormatEmpty, format:Replace("D"," "):Replace("Y"," "):Replace("M"," "))
		SELF:_SetThreadValue(Set.Century, format:Contains("YYYY"))
		SELF:_SetThreadValue(Set.DateFormat, format)
 		SWITCH format
		CASE "MM/DD/YY"
		CASE "MM/DD/YYYY"
			SELF:_SetThreadValue(Set.DateCountry, XSharp.DateCountry.American)
		CASE "YY.MM.DD"
		CASE "YYYY.MM.DD"
			SELF:_SetThreadValue(Set.DateCountry, XSharp.DateCountry.Ansi)
		CASE "DD/MM/YY"
		CASE "DD/MM/YYYY"
            // What a laugh, the British & french have an identical format.
			SELF:_SetThreadValue(Set.DateCountry, XSharp.DateCountry.British)
		CASE "DD.MM.YY"
		CASE "DD.MM.YYYY"
			SELF:_SetThreadValue(Set.DateCountry, XSharp.DateCountry.German)
		CASE "DD-MM-YY"
		CASE "DD-MM-YYYY"
			SELF:_SetThreadValue(Set.DateCountry, XSharp.DateCountry.Italian)
		CASE "YY/MM/DD"
		CASE "YYYY/MM/DD"
			SELF:_SetThreadValue(Set.DateCountry, XSharp.DateCountry.Japanese)
		CASE "MM-DD-YY"
		CASE "MM-DD-YYYY"
			SELF:_SetThreadValue(Set.DateCountry, XSharp.DateCountry.USA)
		END SWITCH

    INTERNAL METHOD _SetTimeFormatSystem() AS VOID
 		VAR dtInfo	    := System.Globalization.DateTimeFormatInfo.CurrentInfo
		SELF:_SetThreadValue(Set.AmExt, dtInfo:AMDesignator)
		SELF:_SetThreadValue(Set.PmExt, dtInfo:PMDesignator)
		VAR separator := dtInfo:TimeSeparator
		IF String.IsNullOrEmpty(separator)
			SELF:_SetThreadValue(Set.Timesep, (DWORD) 0)
		ELSE
			SELF:_SetThreadValue(Set.Timesep, (DWORD) separator[0])
		ENDIF
        Var lAmPm := dtInfo:ShortDatePattern:IndexOf("tt") != -1
        SELF:_SetThreadValue(Set.AmPm, lAmPm)
        SELF:_SetThreadValue(Set.Hours, iif(lAmPm, 12L, 24L))
        SELF:_SetThreadValue(Set.Timeformat, dtInfo:LongTimePattern)

    INTERNAL METHOD _SetDateFormatSystem() AS VOID
		LOCAL format    AS STRING
        VAR dtInfo	    := System.Globalization.DateTimeFormatInfo.CurrentInfo
        format  := dtInfo:ShortDatePattern:ToLower()
		// reduce to single m and d
		DO WHILE (format:IndexOf("mm") != -1)
        	format		:= format:Replace("mm", "m")
        ENDDO
	    DO WHILE format:IndexOf("dd") != -1
			format		:= format:Replace("dd", "d")
		ENDDO
		// make sure we have a double mm to get double digit dates
		// change dates to dd and mm and then everything to upper case
		format := format:Replace("d", "dd"):Replace("m","mm"):ToUpper()
        SELF:_SetDateFormat(format)
        RETURN


    INTERNAL METHOD _SetDateCountry(country AS XSharp.DateCountry) AS VOID

		LOCAL format, year AS STRING
        IF country == XSharp.DateCountry.System
            SELF:_SetDateFormatSystem()
        ELSE
		    year := IIF(SELF:_GetThreadValue<LOGIC>(Set.Century) , "YYYY" , "YY")
		    SWITCH (DateCountry) country
			CASE XSharp.DateCountry.American
				format := "MM/DD/" + year
			CASE XSharp.DateCountry.Ansi
				format := year + ".MM.DD"
			CASE XSharp.DateCountry.British
			CASE XSharp.DateCountry.French
                // What a laugh, the British & french have an identical format.
				format := "DD/MM/" + year
			CASE XSharp.DateCountry.German
				format := "DD.MM." + year
			CASE XSharp.DateCountry.Italian
				format := "DD-MM-" + year
			CASE XSharp.DateCountry.Japanese
				format := year + "/MM/DD"
			CASE XSharp.DateCountry.USA
				format := "MM-DD-" + year
			OTHERWISE
				format := "MM/DD/" + year
		    END SWITCH
		    // this will adjust DateFormatNet, DateFormatEmpty etc, but also DateCountry again
            SELF:_SetDateFormat(format)
        ENDIF
        SELF:_SetThreadValue(Set.DateCountry, country)
        RETURN

	PRIVATE _dataSession AS DataSession
    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.Workareas/*" />
    PUBLIC STATIC PROPERTY Workareas AS DataSession GET DataSession
 /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.DataSession/*" />
	PUBLIC STATIC PROPERTY DataSession AS DataSession
	GET
        LOCAL inst AS RuntimeState
        inst := GetInstance()
        IF inst:_dataSession == NULL_OBJECT
            LOCAL name AS STRING
            VAR thread := System.Threading.Thread.CurrentThread
            IF String.IsNullOrEmpty(thread:Name)
                name := thread:ManagedThreadId:ToString()
            ELSE
                name := thread:Name
            ENDIF
            IF inst == _initialState
                inst:_dataSession := DataSession{1, "Global datasession"}
            ELSE
                inst:_dataSession := DataSession{"DataSession for Thread "+name}
            ENDIF
        ENDIF
        RETURN inst:_dataSession
    END GET
    END PROPERTY

    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.SetDataSession/*" />
    STATIC METHOD SetDataSession (session as DataSession) AS DataSession
        LOCAL inst AS RuntimeState
        LOCAL old  as DataSession
        inst := GetInstance()
        old := inst:_dataSession
        inst:_dataSession := session
        RETURN old

    /// <exclude />
    STATIC METHOD PushCurrentWorkarea(dwArea AS DWORD) AS VOID
        RuntimeState.Workareas:PushCurrentWorkarea(dwArea)
    /// <exclude />
    STATIC METHOD PopCurrentWorkarea() AS DWORD
        RETURN RuntimeState.Workareas:PopCurrentWorkarea()

    [DebuggerBrowsable(DebuggerBrowsableState.Never)];
	PRIVATE _collationTable AS BYTE[]

    /// <summary>Delegate that gets called to automatically lock a record in the FoxPro dialect.</summary>
    static AutoLock   as AutoLockMethod
    /// <summary>Delegate that gets called to automatically unlock a record in the FoxPro dialect.</summary>
    static AutoUnLock as AutoLockMethod
    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.AutoLockMethod/*" />
    delegate AutoLockMethod() as void

 /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.CollationTable/*" />
	PUBLIC STATIC PROPERTY CollationTable AS BYTE[]
	GET
		LOCAL coll AS BYTE[]
		coll := GetInstance():_collationTable
		IF coll == NULL .OR. coll :Length < 256
			_SetCollation("Generic")
			coll := GetInstance():_collationTable := GetValue<BYTE[]>(Set.CollationTable)
		ENDIF
		RETURN coll
	END GET
	SET
		GetInstance():_collationTable  := value
		SetValue(Set.CollationTable, value)
		if OnCollationChanged != null
			OnCollationChanged(GetInstance(), EventArgs{})
		ENDIF
	END SET
	END PROPERTY

    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.ScriptCompiler/*" />
    STATIC PUBLIC PROPERTY ScriptCompiler AS IMacroCompiler AUTO := NULL
	STATIC INTERNAL _macrocompilerType   AS System.Type
    STATIC INTERNAL _macrocompiler       AS IMacroCompiler
    STATIC INTERNAL _macroresolver       AS MacroCompilerResolveAmbiguousMatch
    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.MacroCompiler/*" />
    PUBLIC STATIC PROPERTY MacroCompiler AS IMacroCompiler
        GET
            IF _macrocompiler == NULL
                _LoadMacroCompiler()
            ENDIF
            RETURN _macrocompiler
        END GET
        SET
            _macrocompiler := value
        END SET
    END PROPERTY

    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.MacroCompilerIncludeAssemblyInCache/*" />
    PUBLIC STATIC PROPERTY MacroCompilerIncludeAssemblyInCache as MacroCompilerIncludeAssemblyInCache AUTO := { a AS Assembly => TRUE}


    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.MacroResolver/*" />
    PUBLIC STATIC PROPERTY MacroResolver AS MacroCompilerResolveAmbiguousMatch
        GET
            IF _macroresolver == NULL
                IF _macrocompiler IS IMacroCompiler2 VAR mc
                    _macroresolver := mc:Resolver
                ENDIF
            endif
            RETURN _macroresolver
        END GET
        SET
            _macroresolver := value
            IF _macrocompiler IS IMacroCompiler2 VAR mc
                mc:Resolver := value
            ENDIF
        END SET
    END PROPERTY

    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.MacroCompilerErrorHandler/*" />
    PUBLIC STATIC PROPERTY MacroCompilerErrorHandler as MacroCompilerErrorHandler AUTO
 /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.OnCodePageChanged/*" />
	PUBLIC STATIC EVENT OnCodePageChanged AS EventHandler
 /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.OnCollationChanged/*" />
	public static event OnCollationChanged as EventHandler

    PRIVATE STATIC METHOD _LoadMacroCompiler() AS VOID
        IF _macrocompilerType == NULL_OBJECT
            VAR oMacroAsm := AssemblyHelper.Load("XSharp.MacroCompiler")
		    IF oMacroAsm != NULL_OBJECT
			    LOCAL oType AS System.Type
			    oType := oMacroAsm:GetType("XSharp.Runtime.MacroCompiler",FALSE,TRUE)
			    IF oType != NULL_OBJECT
				    // create instance of this type
				    IF TYPEOF(IMacroCompiler):IsAssignableFrom(oType)
					    _macrocompilerType := oType
				    ELSE
					    THROW Error{EG_CORRUPTION, "", "Could not create the macro compiler from the type "+ oType:FullName+" in the assembly "+oMacroAsm:Location}
				    ENDIF
			    ELSE
				    THROW Error{EG_CORRUPTION, "", "Could not load the macro compiler class in the assembly "+oMacroAsm:Location}
                ENDIF
            ELSE
                // AssemblyHelper.Load will throw an exception
                NOP
		    ENDIF
        ENDIF
        IF _macrocompilerType != NULL_OBJECT
            _macrocompiler := Activator.CreateInstance(_macrocompilerType) ASTYPE IMacroCompiler
            IF _macrocompiler IS IMacroCompiler2 VAR mc
                mc:Resolver := _macroresolver
            endif
		ENDIF
		RETURN


    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.StringCompare/*" />
    STATIC METHOD StringCompare(strLHS AS STRING, strRHS AS STRING) AS INT
       LOCAL ret AS INT
        // Only when vo13 is off and SetExact = TRUE
        IF !RuntimeState.CompilerOptionVO13 .AND. RuntimeState.Exact
            RETURN String.Compare( strLHS,  strRHS)
        ENDIF
        IF Object.ReferenceEquals(strLHS, strRHS)
            ret := 0
        ELSEIF strLHS == NULL
            IF strRHS == NULL			// null and null are equal
                ret := 0
            ELSE
                ret := -1				// null precedes a string
            ENDIF
        ELSEIF strRHS == NULL			// a string comes after null
            ret := 1
        ELSE							// both not null
            // With Not Exact comparison we only compare the length of the RHS string
            // and we always use the unicode comparison because that is what vulcan does
            // This is done to make sure that >= and <= will also return TRUE when the LHS is longer than the RHS
            // The ordinal comparison can be done here because when both strings start with the same characters
            // then equality is guaranteed regardless of collations or other rules.
            // collations and other rules are really only relevant when both strings are different
            IF  !RuntimeState.Exact
                LOCAL lengthRHS AS INT
                lengthRHS := strRHS:Length
                IF lengthRHS == 0 .OR. lengthRHS <= strLHS:Length  .AND. String.Compare( strLHS, 0, strRHS, 0, lengthRHS , StringComparison.Ordinal ) == 0
                    RETURN 0
                ENDIF
            ENDIF
            ret := StringCompareCollation(strLHS, strRHS)
        ENDIF
        RETURN ret

    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.StringCompareCollation/*" />
    static method StringCompareCollation(strLHS as string, strRHS as string) as int
       LOCAL ret AS INT
        // either exact or RHS longer than LHS
        VAR mode := RuntimeState.CollationMode
        SWITCH mode
        CASE CollationMode.Windows
            IF RuntimeState.RunningOnWindows
                ret := XSharp.StringHelpers.CompareWindows(strLHS, strRHS)
            ELSE
                ret := String.Compare(strLHS, strRHS)
            ENDIF
        CASE CollationMode.Clipper  // both Clipper and XPP use weight tables
        CASE CollationMode.Xpp
            ret := XSharp.StringHelpers.CompareClipper(strLHS, strRHS)
        CASE CollationMode.Unicode
            ret := String.Compare(strLHS, strRHS)
        OTHERWISE
            ret := String.CompareOrdinal(strLHS, strRHS)
        END SWITCH
        RETURN ret

    /// <include file="XSharp.Core.Docs.xml" path="doc/RuntimeState.StringCompare_2/*" />
    STATIC METHOD StringCompare(aLHS AS BYTE[], aRHS AS BYTE[], nLen AS INT) AS INT
        SWITCH CollationMode
        CASE CollationMode.Clipper
        CASE CollationMode.Xpp
            RETURN XSharp.StringHelpers.CompareClipper(aLHS, aRHS, nLen)
        CASE CollationMode.Windows
            IF RuntimeState.RunningOnWindows
                RETURN XSharp.StringHelpers.CompareWindows(aLHS, aRHS, nLen)
            ELSE
                VAR strLHS := RuntimeState.WinEncoding:GetString(aLHS, 0, nLen)
                VAR strRHS := RuntimeState.WinEncoding:GetString(aRHS, 0, nLen)
                RETURN String.Compare(strLHS, strRHS)
            ENDIF
        CASE CollationMode.Unicode
            VAR strLHS := RuntimeState.WinEncoding:GetString(aLHS, 0, nLen)
            VAR strRHS := RuntimeState.WinEncoding:GetString(aRHS, 0, nLen)
            RETURN String.Compare(strLHS, strRHS)
        OTHERWISE
            NOP
        END SWITCH
        RETURN XSharp.StringHelpers.CompareOrdinal(aLHS, aRHS, nLen)
END CLASS
