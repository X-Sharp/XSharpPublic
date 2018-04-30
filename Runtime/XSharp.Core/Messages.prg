
using System.Globalization
USING System.Resources




FUNCTION __CavoStr( resid AS DWORD ) AS STRING
   // Strings are stored in a Managed resource with a name
   // the name matches the enum names
   // convert the id to the enum and get its name
   LOCAL strId  AS STRING
   local strMessage as string
   strId := Enum.GetName( TYPEOF(VOErrors) , resId)
   IF !string.IsNullOrEmpty(strId)
      strMessage := Messages.GetString( strId )
      IF String.IsNullOrEmpty( strMessage )
         strMessage := ": canot load string resource '" + strId + "'"
      ENDIF   
   else
	  strMessage := "Cannot find string for error number "+resid:ToString()     
   ENDIF
   RETURN strMessage


CLASS Messages
   STATIC PRIVATE _instance   AS Messages
   static private _instanceName as STRING
   STATIC PRIVATE _generic    AS Messages
   STATIC PRIVATE _lock		AS OBJECT
   static private _availableLanguages as string[]
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

   PROTECTED CONSTRUCTOR(name as string)
      SUPER()
      rm := System.Resources.ResourceManager{ name, GetType():Assembly }
      RETURN
   
   static property CurrentLanguageName as STRING GET _instanceName

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
				otherwise
					alias := name
				end switch
				_instance := Messages{"XSharp.Language."+alias}
				_instanceName := name
			endif
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
     
   STATIC METHOD GetString( name AS STRING, args params OBJECT[] ) AS STRING
      var result := GetString(name)

      IF args?:Length > 0
          result :=  String.Format( CultureInfo.CurrentCulture, result, args )
      ENDIF
      RETURN result

   STATIC METHOD GetString( name AS STRING ) AS STRING
      LOCAL msg := GetInstance() AS Messages
      LOCAL sResult AS STRING
      IF msg == NULL
         sResult := NULL
      ELSE   
         sResult := msg:rm:GetString( name,null)
      ENDIF
	  IF String.IsNullOrEmpty(sResult)
		sResult :=  _generic:rm:GetString(name, null)
	  endif
	  RETURN sResult
	             
end class