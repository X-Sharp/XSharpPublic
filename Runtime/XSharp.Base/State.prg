USING System.Collections.Generic
USING System.Threading
USING XSharp.Runtime
USING XSharp

/// <Summary>
/// Container Class that holds the XSharp Runtime state
/// </Summary>
/// <Remarks>
/// Please note that unlike in Visual Objects and Vulcan.NET every thread has 
/// its own copy of the runtime state.</br>
/// The runtime state from a new thread is a copy of the state of the main thread at that moment.
/// </Remarks>


CLASS XSharp.Runtime.State
	// Static Fields
	PRIVATE STATIC mainState  AS State 
	// Static Methods and Constructor
	PRIVATE STATIC currentState := ThreadLocal<State>{ {=>  mainState:Clone()} }  AS ThreadLocal<State> 
	STATIC CONSTRUCTOR
		mainState	 := State{}
		
	/// <Summary>Retrieve the runtime state for the current thread</Summary>
	PUBLIC STATIC METHOD GetInstance() AS State
		RETURN currentState:Value

	/// <Summary>List of number - value pairs </Summary>
	PRIVATE oSettings AS Dictionary<INT, OBJECT> 

	// Private methods
	PRIVATE CONSTRUCTOR()
		SELF(TRUE)

	PRIVATE CONSTRUCTOR(initialize AS LOGIC)       
		VAR oThread := Thread.CurrentThread
		SELF:Name := "ThreadState for "+oThread:ManagedThreadId:ToString()
		oSettings := Dictionary<INT, OBJECT>{}
		if initialize
			SetValue(Set.DateFormat ,"MM/DD/YYYY")
			SetValue(Set.Epoch, 1900)
			// Initialize the values that are not 'blank'
			// RDD Settings
			State.Ansi      	:= TRUE
			State.AutoOpen      := TRUE
			State.AutoOrder     := TRUE
			State.AutoShareMode	:= AutoShareMode.Auto
			State.Optimize		:= TRUE
			State.LockRetries	:= 1
			State.MemoBlockSize	:= 32
			SetValue(Set.DefaultRDD	, "DBFNTX")
			SetValue(Set.Exclusive, TRUE)
			// Console Settings
			SetValue(Set.Bell, TRUE)
			SetValue(Set.Color, "W/N,N/W,N/N,N/N,N/W")
			// Global Settings
			SetValue(Set.DECIMALS, 2)
			SetValue(Set.Digits,10)
			SetValue(Set.Exact, TRUE)
			SetValue(Set.FLOATDELTA, 0.0000000000001)
			// Date and time settings
			SetInternational(CollationMode.Windows, TRUE)

			// Other settings
			// Collation Table including Compare delegates
			// Nation DLL
			// Macro Compiler
			// WinCodePage ?
			// DosCodePage ?
			// LCID
			// SetDateFormat should also set DateCountry
			// Empty and Internal Dateformat strings 


		ENDIF
		RETURN

	DESTRUCTOR()
		// What do we need to clean ?
		oSettings:Clear()

	PRIVATE METHOD Clone() AS State
		LOCAL oNew AS State
		oNew := State{FALSE}		
		BEGIN LOCK oSettings
			// Copy all values from Current State to New state
			FOREACH VAR element IN oSettings     
				oNew:oSettings[element:Key] := element:Value
			NEXT
		END LOCK
		RETURN oNew     
		
	/// <Summary>Retrieve state name</Summary>
	/// <Returns>String value, such as "State for Thread 123"</Returns>
	PUBLIC PROPERTY Name AS STRING AUTO

	/// <Summary>ToString() override</Summary>
	/// <Returns>String value, such as "State for Thread 123"</Returns>
	PUBLIC VIRTUAL METHOD ToString() AS STRING
		RETURN SELF:Name

	PUBLIC STATIC METHOD GetValue<T> (nSetting AS INT) AS T
		RETURN currentState:Value:_GetThreadValue<T>(nSetting);

	PUBLIC STATIC METHOD SetValue<T> (nSetting AS INT, oValue AS T) AS T
		RETURN currentState:Value:_SetThreadValue<T>(nSetting, oValue)

	/// <Summary>Get the value for a certain setting</Summary>
	/// <param Name="nSetting"> The number of the setting to change</param>
	/// <typeparam Name="T"> The expected return type of the value</typeparam>
	/// <Returns>The new value</Returns>
	PRIVATE METHOD _GetThreadValue<T> (nSetting AS INT) AS T
		BEGIN LOCK oSettings
			IF oSettings.ContainsKey(nSetting)
				RETURN (T) oSettings[nSetting]
			ENDIF
		END LOCK
		RETURN Default(T)

	/// <Summary>Set the value for a certain setting</Summary>
	/// <param Name="nSetting"> The number of the setting to change</param>
	/// <param Name="oValue"> The new value of the setting.</param>
	PRIVATE METHOD _SetThreadValue<T>(nSetting AS INT, oValue AS T) AS T
		LOCAL result AS T
		BEGIN LOCK oSettings
			IF oSettings.ContainsKey(nSetting)
				result := (T) oSettings[nSetting]
			ELSE
				result := Default(T)
			ENDIF
			oSettings[nSetting] := oValue
		END LOCK
		RETURN	result		

    STATIC PROPERTY Ansi AS LOGIC ;
        GET GetValue<LOGIC>(Set.Ansi);
        SET SetValue<LOGIC>(Set.Ansi, VALUE)

    STATIC PROPERTY AutoOrder AS LOGIC ;
        GET GetValue<LOGIC>(Set.AutoOrder);
        SET SetValue<LOGIC>(Set.AutoOrder, VALUE)

    STATIC PROPERTY AutoOpen AS LOGIC ;
        GET GetValue<LOGIC>(Set.AutoOpen);
        SET SetValue<LOGIC>(Set.AutoOpen, VALUE)

    STATIC PROPERTY AutoShareMode AS AutoShareMode ;
        GET GetValue<AutoShareMode>(Set.AutoShare);
        SET SetValue<AutoShareMode>(Set.AutoShare, VALUE)

   STATIC PROPERTY Century AS LOGIC ;
        GET GetValue<LOGIC>(Set.Century);
        SET SetValue<LOGIC>(Set.Century, VALUE)


   STATIC PROPERTY CollationMode AS CollationMode ;
        GET GetValue<CollationMode>(Set.CollationMode);
        SET SetValue<CollationMode>(Set.CollationMode, VALUE)


   STATIC PROPERTY DateCountry AS INT ;
        GET GetValue<INT>(Set.DateCountry);
        SET SetValue<INT>(Set.DateCountry, VALUE)


	/// <Summary>The current Date format</Summary>
	/// <Remarks>This string should contain a combination of DD MM and either YY or YYYY characters.<br>
	/// For example DD-MM-YYYY for italian date format, MM/DD/YYYY for American date format or DD/MM/YYYY for British Date format.
	/// Note that all other characters except the four groups mentioned above are copied to the output string verbatim.
	/// </Remarks>
	/// <Returns>String value</Returns>
    STATIC PROPERTY DateFormat AS STRING ;
        GET GetValue<STRING>(Set.DateFormat);
        SET _SetDateFormat(Value)

	/// <Summary>The default number of decimals for new FLOAT values that are created without explicit decimals</Summary>
	/// <Returns>DWORD value</Returns>
    STATIC PROPERTY Decimals AS LONG ;
        GET GetValue<LONG>(Set.DECIMALS);
        SET SetValue<LONG>(Set.DECIMALS, VALUE)


	/// <Summary>The default number of decimals for new FLOAT values that are created without explicit decimals</Summary>
	/// <Returns>DWORD value</Returns>
    STATIC PROPERTY DecimalSep AS LONG ;
        GET GetValue<LONG>(Set.DecimalSep);
        SET SetValue<LONG>(Set.DecimalSep, VALUE)


	/// <Summary>RDD Deleted Flag that determines whether to ignore or include records that are marked for deletion.</Summary>
	/// <Returns>Logic value</Returns>
    STATIC PROPERTY Deleted AS LOGIC ;
        GET GetValue<LOGIC>(Set.DELETED);
        SET SetValue<LOGIC>(Set.DELETED, VALUE)


	/// <Summary>Date Epoch value that determines how dates without century digits are interpreted.</Summary>
	/// <Returns>DWORD value</Returns>
    STATIC PROPERTY Epoch AS DWORD ;
        GET GetValue<DWORD>(Set.EPOCH);
        SET SetValue<DWORD>(Set.EPOCH, VALUE)

	/// <Summary>String comparison Exact flag that determines how comparisons with the single '=' characters should be done.</Summary>
	/// <Returns>Logic value</Returns>
    STATIC PROPERTY Exact AS LOGIC ;
        GET GetValue<LOGIC>(Set.EXACT);
        SET SetValue<LOGIC>(Set.EXACT, VALUE)

     STATIC PROPERTY International AS CollationMode ;
        GET GetValue<CollationMode>(Set.Intl);
        SET SetValue<CollationMode>(Set.Intl, VALUE)

    STATIC PROPERTY LockRetries AS LONG ;
        GET GetValue<LONG>(Set.LOCKTRIES);
        SET SetValue<LONG>(Set.LOCKTRIES, VALUE)

    STATIC PROPERTY MemoBlockSize AS LONG ;
        GET GetValue<LONG>(Set.MEMOBLOCKSIZE);
        SET SetValue<LONG>(Set.MEMOBLOCKSIZE, VALUE)

	/// <Summary>RDD Optimize Flag</Summary>
	/// <Returns>Logic value</Returns>
    STATIC PROPERTY Optimize AS LOGIC ;
        GET GetValue<LOGIC>(Set.OPTIMIZE);
        SET SetValue<LOGIC>(Set.OPTIMIZE, VALUE)


    STATIC PROPERTY SoftSeek AS LOGIC ;
        GET GetValue<LOGIC>(Set.SoftSeek);
        SET SetValue<LOGIC>(Set.SoftSeek, VALUE)

	/// <Summary>The default number of decimals for new FLOAT values that are created without explicit decimals</Summary>
	/// <Returns>DWORD value</Returns>
    STATIC PROPERTY ThousandSep AS LONG ;
        GET GetValue<LONG>(Set.ThousandSep);
        SET SetValue<LONG>(Set.ThousandSep, VALUE)


    STATIC PROPERTY Unique AS LOGIC ;
        GET GetValue<LOGIC>(Set.Unique);
        SET SetValue<LOGIC>(Set.Unique, VALUE)


	STATIC METHOD SetInternational(mode AS CollationMode, force := FALSE AS LOGIC) AS VOID
		IF mode != state.International	.or. force
			IF mode == CollationMode.Clipper
				SetValue(Set.AMEXT, "")
				SetValue(Set.PMEXT, "")
				SetValue(Set.AMPM, FALSE)
				SetValue(Set.Century, FALSE)
				SetValue(Set.DateCountry, 1)
				SetValue(Set.Decimals, 2)
				SetValue(Set.DECIMALSEP, ".")
				SetValue(Set.THOUSANDSEP, ",")
				SetValue(Set.DateFormat, "MM/DD/YYYY")
			ELSE
				VAR dtInfo	    := System.Globalization.DateTimeFormatInfo.CurrentInfo
				SetValue(Set.AMEXT, dtInfo:AMDesignator)
				SetValue(Set.PMEXT, dtInfo:PMDesignator)
				SetValue(Set.AMPM, dtInfo:ShortDatePattern:IndexOf("tt") != -1)
				VAR dateformat  := dtInfo:ShortDatePattern:ToLower()
				// reduce to single m and d
				if (dateformat.IndexOf("mm") != -1)
					dateformat		:= dateformat:Replace("mmmm", "m")
					dateformat		:= dateformat:Replace("mmm", "m")
					dateformat		:= dateformat:Replace("mm", "m")
				ENDIF
				IF dateformat.IndexOf("dd") != -1
					dateformat		:= dateformat:Replace("dd", "d")
				ENDIF
				SetValue(Set.Century, dateformat:IndexOf("yyyy") != -1)
				dateformat := dateformat:Replace("d", "dd"):Replace("m","mm"):ToUpper()
				State.DateFormat  := dateformat
				//State.DateCountry := 1
				State.Decimals	  := 2
				VAR numberformat := System.Globalization.NumberFormatInfo.CurrentInfo
				SetValue(Set.DECIMALSEP, numberformat:NumberDecimalSeparator[0])
				SetValue(Set.THOUSANDSEP, numberformat:NumberGroupSeparator[0])
				SetValue(Set.EPOCH, 1910)
				
			ENDIF
			state.International := mode
		ENDIF

	PRIVATE STATIC METHOD _SetDateFormat(format AS STRING) AS VOID
		format := format:ToUpper()
		SetValue(SET.CENTURY, format:Contains("YYYY"))
		SetValue(Set.DATEFORMAT, format)
		SWITCH format
			CASE "MM/DD/YY"
			CASE "MM/DD/YYYY"
				SetValue(Set.DATECOUNTRY, 1)	// American
			CASE "YY.MM.DD"
			CASE "YYYY.MM.DD"
				SetValue(Set.DATECOUNTRY, 2)	// Ansi
			CASE "DD/MM/YY"
			CASE "DD/MM/YYYY"
				SetValue(Set.DATECOUNTRY, 3)	// British & french
			CASE "DD.MM.YY"
			CASE "DD.MM.YYYY"
				SetValue(Set.DATECOUNTRY, 5)	// German
			CASE "DD-MM-YY"
			CASE "DD-MM-YYYY"
				SetValue(Set.DATECOUNTRY, 6)	// Italian
			CASE "YY/MM/DD"
			CASE "YYYY/MM/DD"
				SetValue(Set.DATECOUNTRY, 7)	// Japanese
			CASE "MM-DD-YY"
			CASE "MM-DD-YYYY"
				SetValue(Set.DATECOUNTRY, 8)	// USA
			OTHERWISE
				SetValue(Set.DATECOUNTRY, 0)	
		END SWITCH

END CLASS


ENUM AutoShareMode
	MEMBER NoChange		 := 0
	MEMBER Auto	  		 := 1
	MEMBER ForeExclusive := 2
END ENUM

ENUM CollationMode
	MEMBER WINDOWS
	MEMBER CLIPPER
END ENUM