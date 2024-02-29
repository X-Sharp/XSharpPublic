// FabIniFileAsJSON.prg
// Created by    : fabri
// Creation Date : 10/8/2020 10:17:46 AM
// Created for   :
// WorkStation   : FABPORTABLE


USING System
USING System.Collections.Generic
USING System.Text
USING System.IO
USING Newtonsoft.Json

BEGIN NAMESPACE VFPXPorter

	/// <summary>
	/// The FabIniFileAsJSON class.
	/// Mimics a Ini file, but using a JSON file.
	/// </summary>
	CLASS FabIniFileAsJSON
		PROTECTED jsonFileFullPath AS STRING
		PROTECTED iniContent AS Dictionary<STRING, Dictionary<STRING,STRING> >

		CONSTRUCTOR( fullPath AS STRING )
			jsonFileFullPath := fullPath
			//
			TRY
				iniContent := JsonConvert.DeserializeObject<Dictionary<STRING, Dictionary<STRING,STRING> >>( File.ReadAllText(jsonFileFullPath))
			CATCH
				SELF:Reset()
			END TRY
			//
			RETURN

		PROPERTY FullPath AS STRING GET SELF:jsonFileFullPath


		METHOD Reset() AS VOID
			iniContent := Dictionary<STRING, Dictionary<STRING,STRING> >{}
		END METHOD


		PROTECTED METHOD WritePrivateProfileString( Section AS STRING , Key AS STRING , newValue AS STRING ) AS VOID
			LOCAL sectionContent AS Dictionary<STRING,STRING>
			LOCAL newSection AS LOGIC
			//
			TRY
				IF iniContent:ContainsKey( Section )
					sectionContent := iniContent[ Section ]
					newSection := FALSE
				ELSE
					sectionContent := Dictionary<STRING,STRING>{}
					newSection := TRUE
				ENDIF
				IF sectionContent:ContainsKey(Key)
					sectionContent[ Key ] := newValue
				ELSE
					sectionContent.Add( Key, newValue )
				ENDIF
				//
				IF newSection
					iniContent.Add( Section, sectionContent )
				ELSE
					iniContent[ Section ] := sectionContent
				ENDIF
            CATCH
                NOP
			END TRY
			//
			File.WriteAllText( SELF:jsonFileFullPath, JsonConvert.SerializeObject( iniContent ) )
			//

		PROTECTED METHOD GetPrivateProfileString( Section AS STRING , Key AS STRING , defValue AS STRING ) AS STRING
			LOCAL sectionContent AS Dictionary<STRING,STRING>
			LOCAL cValue AS STRING
			//
			cValue := defValue
			TRY
				IF iniContent:ContainsKey( Section )
					sectionContent := iniContent[ Section ]
					IF sectionContent:ContainsKey(Key)
						cValue := sectionContent[ Key ]
					ENDIF
				ENDIF
            CATCH
                NOP
			END TRY
			RETURN cValue


		PUBLIC METHOD WriteValue(Section AS STRING , Key AS STRING , newValue AS STRING ) AS VOID
			WritePrivateProfileString(Section, Key, newValue)


		PUBLIC METHOD WriteLogic(Section AS STRING , Key AS STRING , newValue AS LOGIC ) AS VOID
			LOCAL writeValue AS STRING
			//
			writeValue := "false"
			IF (newValue)
				writeValue := "true"
			ENDIF
			WritePrivateProfileString(Section, Key, writeValue)

		PUBLIC METHOD WriteValue(Section AS STRING , Key AS STRING , newValue AS LOGIC ) AS VOID
			SELF:WriteLogic( Section, Key, newValue )

		PUBLIC METHOD WriteLong(Section AS STRING , Key AS STRING , newValue AS LONG ) AS VOID
			WritePrivateProfileString(Section, Key, newValue:ToString())

		PUBLIC METHOD WriteValue(Section AS STRING , Key AS STRING , newValue AS LONG ) AS VOID
			SELF:WriteLong( Section, Key, newValue )

		PUBLIC METHOD ReadValue(Section AS STRING , Key AS STRING ) AS STRING
			RETURN SELF:ReadValue(Section, Key, "")


		PUBLIC METHOD ReadValue(Section AS STRING , Key AS STRING , defValue AS STRING ) AS STRING
			RETURN GetPrivateProfileString(Section, Key, defValue)


		PUBLIC METHOD ReadLogic(Section AS STRING , Key AS STRING , defValue AS LOGIC ) AS LOGIC
			LOCAL defaut2 AS STRING
			//
			defaut2 := "false"
			IF (defValue)
				defaut2 := "true"
			ENDIF
			defaut2 := SELF:ReadValue(Section, Key, defaut2)
			RETURN defaut2:ToLower() == "true"

		PUBLIC METHOD ReadValue(Section AS STRING , Key AS STRING , defValue AS LOGIC ) AS LOGIC
			RETURN SELF:ReadLogic( Section, Key, defValue )

		PUBLIC METHOD ReadLong(Section AS STRING , Key AS STRING , defValue AS LONG ) AS LONG
			LOCAL readValue AS STRING
			LOCAL result AS LONG
			//
			readValue := defValue:ToString()
			readValue := SELF:ReadValue(Section, Key, readValue)
			IF (Int32.TryParse(readValue, OUT result))
				RETURN result
			ENDIF
			RETURN defValue

		PUBLIC METHOD ReadValue(Section AS STRING , Key AS STRING , defValue AS LONG ) AS LONG
			RETURN SELF:ReadLong( Section, Key, defValue )

		PUBLIC METHOD ReadSection( Section AS STRING ) AS List<STRING>
			LOCAL result := NULL AS List<STRING>
			LOCAL sectionContent AS Dictionary<STRING,STRING>
			//
			TRY
				IF iniContent:ContainsKey( Section )
					sectionContent := iniContent[ Section ]
					result := List<STRING>{ sectionContent:Keys }
				ENDIF
            CATCH
                nop
			END TRY
			RETURN result

	END CLASS
END NAMESPACE // VFPXPorter