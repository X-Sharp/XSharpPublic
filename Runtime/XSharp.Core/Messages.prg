
USING System.Globalization
USING System.Resources


FUNCTION SetLiteral(dwRes AS DWORD) AS STRING
	RETURN __CavoStr( dwRes )

FUNCTION GetStringDXAX(dwRes AS DWORD) AS STRING
	RETURN __CavoStr( dwRes )

FUNCTION VO_Sprintf( format AS DWORD,  args PARAMS OBJECT[] ) AS STRING
	RETURN VO_Sprintf( __CavoStr( format ), args )

FUNCTION VO_Sprintf( format AS STRING,  args PARAMS OBJECT[] ) AS STRING
	LOCAL ret        AS STRING
	// The format string should start with "%Vn" to indicate a variable number
	// n is the 1 based position in the args list
	// it can also contain %s for other parameters

	IF String.IsNullOrEmpty( format )
		LOCAL nArgs   AS INT
		LOCAL nArg          AS INT
		nArgs := args:Length
		ret := "<missing format string>"
		FOR nArg := 1 UPTO nArgs
			ret := String.Concat( ret, " | ", args[nArg]:ToString() )
		NEXT
	ELSE
		LOCAL hasVars AS LOGIC
		DO CASE
			CASE format:StartsWith("%V1s%V2s%V3s")
				format := format:Substring(13)
				hasvars := TRUE
			CASE format:StartsWith("%V1s%V2s")
				format := format:Substring(9)
				hasvars := TRUE
			CASE format:StartsWith("%V1s")
				format := format:Substring(5)
				hasvars := TRUE
			OTHERWISE
				hasvars := FALSE
		ENDCASE
		IF hasvars
			LOCAL elements AS STRING[]
			elements := format:Split(<STRING>{"%s"},StringSplitOptions.None)
			format   := ""
			FOR VAR nVar := 0 TO elements:Length -2
				format += elements[nVar]
				format += "{" + nVar:ToString()+"}"
			NEXT
			format += elements[elements:Length-1]
		ENDIF
		ret := String.Format( CultureInfo.CurrentCulture, format, args )
	ENDIF

	RETURN ret


FUNCTION __CavoStr( resid AS DWORD ) AS STRING
	// Strings are stored in a Managed resource with a name
	// the name matches the enum names
	// convert the id to the enum and get its name
	LOCAL strId  AS STRING
	LOCAL strMessage AS STRING
	strId := Enum.GetName( TYPEOF(VOErrors) , resId)
	IF !string.IsNullOrEmpty(strId)
			strMessage := Messages.GetString( strId )
			IF String.IsNullOrEmpty( strMessage )
				strMessage := ": canot load string resource '" + strId + "'"
		ENDIF
	ELSE
		strMessage := "Cannot find string for error number "+resid:ToString()
	ENDIF
	RETURN strMessage


INTERNAL CLASS Messages
	STATIC PRIVATE _instance   AS Messages
	STATIC PRIVATE _instanceName AS STRING
	STATIC PRIVATE INITONLY _generic    AS Messages
	STATIC PRIVATE INITONLY _lock		AS OBJECT
	STATIC PRIVATE INITONLY _availableLanguages AS STRING[]
	PRIVATE rm AS ResourceManager


	STATIC CONSTRUCTOR
		_lock := OBJECT{}
		_availableLanguages := <STRING> {;
		"BRAZIL", "CROATIA", "CZECH852", "CZECH895", "DANISH", "DUTCH", "FINNISH", ;
		"FRENCH", "GENERIC", "GERMAN", "GERMAN2", "HUNG852", "HUNGCWI", "ITALIAN", ;
		"NORWEGN", "POL-ISO", "POL-MAZ", "POL852", "PORT850", "PORT860", "ROMANIA", ;
		"RUSSIAN", "SERBIA", "SL-W-95", "SL-W-AS7", "SL-W-EE", "SLOV852", "SLOV895", ;
		"SPANISH", "SWEDISH", "UK"}
		_generic := Messages{"XSharp.Language.Generic"}

	PROTECTED CONSTRUCTOR(name AS STRING)
		SUPER()
		rm := System.Resources.ResourceManager{ name, GetType():Assembly }
		RETURN

	STATIC PROPERTY CurrentLanguageName AS STRING GET _instanceName

	STATIC METHOD SetCurrentLanguage(name AS STRING) AS VOID

		BEGIN LOCK _lock
			IF Array.IndexOf(_availableLanguages,name:ToUpper()) >= 0
				LOCAL alias AS STRING

				// Some string tables were equal in VO, only collations were different
				// so redirect these to their aliases
				SWITCH name:ToUpper()
					CASE "CZECH895"
						alias := "Czech852"
					CASE "GERMAN2"
						alias := "German"
					CASE "HUNGCWI"
						alias := "Hung852"
					CASE "PORT860"
					CASE "BRAZIL"
						alias := "Port850"
					CASE "POL-ISO"
					CASE "POL-MAZ"
						alias := "Pol852"
					CASE "SLOV895"
						alias := "Slov852"
					CASE "SL-W-95"
					CASE "SL-W-AS7"
						alias := "WL-W-EE"
					CASE "UK"
						alias := "Generic"
					OTHERWISE
						alias := name
				END SWITCH
				_instance := Messages{"XSharp.Language."+alias}
				_instanceName := name
			ENDIF
		END LOCK
		RETURN

	PRIVATE STATIC METHOD GetInstance() AS Messages
		IF _instance == NULL
			BEGIN LOCK _lock
				IF _instance == NULL
					_instance := _generic
					_instancename := "generic"
				ENDIF
			END LOCK
		ENDIF

		RETURN _instance

	STATIC METHOD GetString( name AS STRING, args PARAMS OBJECT[] ) AS STRING
		VAR result := GetString(name)

		IF args?:Length > 0
			result :=  String.Format( CultureInfo.CurrentCulture, result, args )
		ENDIF
		RETURN result

	STATIC METHOD GetString( name AS STRING ) AS STRING
		LOCAL msg := GetInstance() AS Messages
		LOCAL sResult AS STRING
		name := name:ToUpper()
		sResult := msg:rm:GetString( name,NULL)
		IF String.IsNullOrEmpty(sResult)
			sResult :=  _generic:rm:GetString(name, NULL)
		ENDIF
		RETURN sResult

END CLASS
