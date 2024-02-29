// VFPForm_SetAll.prg
// Created by    : fabri
// Creation Date : 10/13/2022 8:17:02 AM
// Created for   :
// WorkStation   : FABXPS


USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE XSharp.VFP.UI

	/// <summary>
	/// The VFPForm class : Support for SetAll Method.
	/// </summary>
	PARTIAL CLASS Form

		// Holds the convertion table between VFP and X#
		PRIVATE _typeConvert AS Dictionary<STRING,STRING>

		METHOD SetAll( cProperty, uValue, cClass) AS VOID CLIPPER
			IF !IsString( cProperty ) .OR. IsNil( uValue )
				RETURN
			ENDIF
			//
			IF SELF:_typeConvert == NULL
				SELF:_typeConvert := Dictionary<STRING,STRING>{}
				SELF:InitSetAll()
			ENDIF
			//
			VAR lookFor := String.Empty
			IF cClass IS STRING VAR strClass
				IF SELF:_typeConvert:ContainsKey( strClass:ToUpper() )
					lookFor := SELF:_typeConvert[ strClass:ToUpper() ]
				ENDIF
			ENDIF
			FOREACH VAR ctl IN SELF:Controls
				// Looking for a specific Type ?
				IF !String.IsNullOrEmpty( lookFor )
					IF lookFor != ctl:GetType():ToString():ToUpper()
						LOOP
					ENDIF
				ENDIF
				// Cast to Usual
				LOCAL uCtl AS USUAL
				uCtl := (USUAL) ctl
				// Try to Set
				TRY
					IVarPut( uCtl, cProperty, uValue )
				CATCH
					NOP
				END TRY
			NEXT
			RETURN
		END METHOD

		PRIVATE METHOD InitSetAll() AS VOID
			LOCAL typeConvert AS STRING
			// Simply Copy/Paste the content of the TypeConvert.json file between TEXT / ENDTEXT
			TEXT TO typeConvert
			{
			"form": [
			"Form",
			"true",
			"false"
			],
			"commandbutton": [
			"CommandButton",
			"false",
			"true"
			],
			"textbox": [
			"TextBox",
			"false",
			"true"
			],
			"editbox": [
			"EditBox",
			"false",
			"true"
			],
			"checkbox": [
			"CheckBox",
			"false",
			"true"
			],
			"optionbutton": [
			"OptionButton",
			"false",
			"true"
			],
			"optiongroup": [
			"OptionGroup",
			"false",
			"true"
			],
			"commandgroup": [
			"CommandGroup",
			"false",
			"true"
			],
			"listbox": [
			"ListBox",
			"false",
			"true"
			],
			"combobox": [
			"ComboBox",
			"false",
			"true"
			],
			"label": [
			"Label",
			"false",
			"true"
			],
			"image": [
			"Image",
			"false",
			"true"
			],
			"timer": [
			"Timer",
			"false",
			"false"
			],
			"container": [
			"Container",
			"true",
			"true"
			],
			"spinner": [
			"Spinner",
			"false",
			"true"
			],
			"grid": [
			"Grid",
			"false",
			"true"
			],
			"header": [
			"Header",
			"false",
			"true"
			],
			"shape": [
			"Container",
			"false",
			"true"
			],
			"line": [
			"VFPContainer",
			"false",
			"true"
			],
			"DataEnvironment": [
			"DataEnvironment",
			"false",
			"false"
			],
			"Cursor": [
			"Cursor",
			"false",
			"false"
			],
			"xsPorterMenuStrip": [
			"System.Windows.Forms.MenuStrip",
			"false",
			"true"
			],
			"xsPorterMenuItem": [
			"System.Windows.Forms.ToolStripMenuItem",
			"false",
			"false"
			],
			"xsPorterMenuSeparator": [
			"System.Windows.Forms.ToolStripSeparator",
			"false",
			"false"
			]
			}
			ENDTEXT
			//
			LOCAL oneChar AS CHAR
			LOCAL charValue AS INT
			VAR reader := System.IO.StringReader{ typeConvert }
			//
			VAR elts := List<STRING>{}
			VAR elt := StringBuilder{}
			VAR inString := FALSE
			// We don't really handle the JSON Format, just looking for strings between ""
			// When we have 4 elements, add the two first to the Dictionary
			// Would be better to use JSON Newtonsoft, but do we really need to add a dependency here ????
			DO WHILE TRUE
				charValue := reader:Read()
				IF charValue == -1
					EXIT
				ENDIF
				oneChar := (CHAR) charValue
				// Space, \r, \n \t, ",", [, ], {, }, :
				IF (charValue == 32) .OR. (charValue == 13) .OR. (charValue == 10) .OR. (charValue == 9) .OR. ;
						(charValue == 44) .OR. (charValue == 91) .OR. (charValue == 93) .OR. (charValue == 123) .OR. ;
						(charValue == 125) .OR. (charValue == 58)
					LOOP
				ENDIF
				// '"'
				IF (charValue == 34)
					IF !inString
						inString := TRUE
						LOOP
					ENDIF
					inString := FALSE
					elts:Add( elt:ToString() )
					elt:Clear()
					IF elts:Count == 4
						// Save both type in Uppercase
						// [0] : FoxPro Type => Form, CommandButton, EditBox, ...
						// [1] : xsLibrary Type => VFPFORM, VFPCommandButton, VFPTEXTBOX
						// Don't forget to add the XSharp.VFP.UI. in front as classname if Fully-Qualified
						SELF:_typeConvert:Add( elts[0]:ToUpper(), "XSharp.VFP.UI." + elts[1]:ToUpper() )
						elts:Clear()
					ENDIF
				ELSE
					elt:Append( oneChar )
				ENDIF
			ENDDO
			//
			RETURN



	END CLASS
END NAMESPACE // XSharp.VFP.UI