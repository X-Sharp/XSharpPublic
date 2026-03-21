// EditBox.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.

USING System
USING System.Collections.Generic
USING System.Text
USING System.Windows.Forms
USING System.Drawing
USING System.ComponentModel

BEGIN NAMESPACE XSharp.VFP.UI
	/// <summary>
	/// The VFP compatible EditBox class.
	/// </summary>
	PARTIAL CLASS EditBox INHERIT TextBox

		// Common properties that all VFP Objects support
		#include "Headers/VFPObject.xh"

		/// <summary>
		/// Backing fields for VFP properties
		/// </summary>
		PRIVATE _uValue AS USUAL

		/// <summary>
		/// Constructor for EditBox control.
		/// </summary>
		CONSTRUCTOR( )
			SUPER()
			SELF:Multiline := TRUE
			SELF:ScrollBars := ScrollBars.Both
			SELF:Size := Size{100,75}
			SELF:_uValue := NIL
			RETURN

		OVERRIDE PROTECTED METHOD OnKeyDown(e AS KeyEventArgs) AS VOID
			// Handle AllowTabs property
			IF e:KeyCode == Keys.Tab .AND. !SELF:AllowTabs
				e:SuppressKeyPress := TRUE
				e:Handled := TRUE
				RETURN
			ENDIF
			SUPER:OnKeyDown(e)
		END METHOD

		#include ".\Headers\ControlProperties.xh"
        #include ".\Headers\ControlFocus.xh"
		#include ".\Headers\ControlSource.xh"

		/// <summary>
		/// Gets or sets the text value of the EditBox.
		/// In VFP, this is the Value property.
		/// </summary>
		/// <value>The text value as USUAL.</value>
		[DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)];
		[EditorBrowsable(EditorBrowsableState.Never)];
		[Bindable(FALSE)];
		[Browsable(FALSE)];
		PROPERTY Value AS USUAL
			GET
				RETURN SELF:Text
			END GET
			SET
				IF !IsNil(VALUE)
					SELF:_uValue := VALUE
					SELF:Text := Str( VALUE )
				ENDIF
			END SET
		END PROPERTY

		/// <summary>
		/// Specifies that the EditBox should insert linefeed characters (CHR(10)) after carriage return characters
		/// (CHR(13)) within the text of an EditBox whenever the Value property is read or whenever the value is
		/// stored to the ControlSource.
		/// </summary>
		/// <value>True to insert linefeeds; otherwise, false.</value>
		[Category("VFP Properties"), Description("Insert linefeeds after carriage returns")];
		[DefaultValue(FALSE)];
		PROPERTY AddLineFeeds AS LOGIC AUTO

		/// <summary>
		/// Specifies whether to allow tabs in an EditBox control.
		/// </summary>
		/// <value>True to allow tabs; otherwise, false.</value>
		[Category("VFP Properties"), Description("Allow tab characters in the edit box")];
		[DefaultValue(FALSE)];
		PROPERTY AllowTabs AS LOGIC AUTO

		/// <summary>
		/// Gets or sets the type of scroll bars to display.
		/// Equivalent to VFP's ScrollBars property.
		/// </summary>
		/// <value>0=None, 1=Horizontal, 2=Vertical, 3=Both. Default is 3.</value>
		[Category("VFP Properties"), Description("Scroll bar type: 0=None, 1=Horizontal, 2=Vertical, 3=Both")];
		[DefaultValue(3)];
		PROPERTY VFPScrollBars AS INT
			GET
				RETURN (INT)SELF:ScrollBars
			END GET
			SET
				SWITCH VALUE
					CASE 0
						SELF:ScrollBars := ScrollBars.None
					CASE 1
						SELF:ScrollBars := ScrollBars.Horizontal
					CASE 2
						SELF:ScrollBars := ScrollBars.Vertical
					CASE 3
						SELF:ScrollBars := ScrollBars.Both
				END SWITCH
			END SET
		END PROPERTY

		/// <summary>
		/// Finds the first occurrence of a search string in the EditBox text.
		/// Equivalent to VFP's FindText method.
		/// </summary>
		/// <param name="cSearchString">The text to search for.</param>
		/// <param name="nStartPosition">Optional starting position (0-based). Default is 0 (from beginning).</param>
		/// <param name="lIgnoreCase">Optional case-sensitivity flag. Default is TRUE (case-insensitive).</param>
		/// <returns>The 0-based position of the found text, or -1 if not found.</returns>
		/// <remarks>
		/// Searches for cSearchString within the EditBox text starting at nStartPosition.
		/// Returns the 0-based character position where the text was found.
		/// </remarks>
		PUBLIC METHOD FindText(cSearchString AS STRING, nStartPosition AS INT := 0, lIgnoreCase AS LOGIC := TRUE) AS INT STRICT
			IF String.IsNullOrEmpty(cSearchString) .OR. String.IsNullOrEmpty(SELF:Text)
				RETURN -1
			ENDIF

			VAR compareOption := IF(lIgnoreCase, System.StringComparison.OrdinalIgnoreCase, System.StringComparison.Ordinal)
			VAR position := SELF:Text.IndexOf(cSearchString, nStartPosition, compareOption)
			RETURN position
		END METHOD

		/// <summary>
		/// Finds and replaces text in the EditBox.
		/// Equivalent to VFP's ReplaceText method.
		/// </summary>
		/// <param name="cSearchString">The text to search for.</param>
		/// <param name="cReplacementString">The replacement text.</param>
		/// <param name="nStartPosition">Optional starting position (0-based). Default is 0 (from beginning).</param>
		/// <param name="lReplaceAll">Optional flag to replace all occurrences. Default is FALSE (first only).</param>
		/// <param name="lIgnoreCase">Optional case-sensitivity flag. Default is TRUE (case-insensitive).</param>
		/// <returns>The number of replacements made.</returns>
		/// <remarks>
		/// Replaces cSearchString with cReplacementString, starting at nStartPosition.
		/// If lReplaceAll is TRUE, replaces all occurrences; otherwise replaces first match only.
		/// </remarks>
		PUBLIC METHOD ReplaceText(cSearchString AS STRING, cReplacementString AS STRING, nStartPosition AS INT := 0, lReplaceAll AS LOGIC := FALSE, lIgnoreCase AS LOGIC := TRUE) AS INT STRICT
			IF String.IsNullOrEmpty(cSearchString) .OR. String.IsNullOrEmpty(SELF:Text)
				RETURN 0
			ENDIF

			VAR text := SELF:Text
			VAR replaceCount := 0
			VAR currentPos := nStartPosition
			VAR compareOption := IF(lIgnoreCase, System.StringComparison.OrdinalIgnoreCase, System.StringComparison.Ordinal)

			DO WHILE currentPos < text:Length
				VAR foundPos := text:IndexOf(cSearchString, currentPos, compareOption)
				IF foundPos == -1
					EXIT
				ENDIF

				text := text:Remove(foundPos, cSearchString:Length):Insert(foundPos, cReplacementString)
				replaceCount++
				currentPos := foundPos + cReplacementString:Length

				IF !lReplaceAll
					EXIT
				ENDIF
			ENDDO

			IF replaceCount > 0
				SELF:Text := text
				SELF:_uValue := text
			ENDIF

			RETURN replaceCount
		END METHOD

	END CLASS

END NAMESPACE
