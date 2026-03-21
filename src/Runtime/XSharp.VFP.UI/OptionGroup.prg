// OptionGroup.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.

USING System
USING System.Collections.Generic
USING System.ComponentModel
USING System.Data
USING System.Drawing

USING System.Text
USING System.ComponentModel
USING System.Windows.Forms

BEGIN NAMESPACE XSharp.VFP.UI
/// <summary>
/// The VFP compatible OptionGroup class.
/// </summary>
	PUBLIC PARTIAL CLASS OptionGroup ;
			INHERIT System.Windows.Forms.UserControl

		PRIVATE buttons AS List<OptionButton>
		PRIVATE _value AS INT

		/// <summary>
		/// Gets or sets the number of buttons in the group.
		/// </summary>
		PROPERTY ButtonCount AS INT
			GET
				return SELF:buttons:Count
			END GET

			SET
				IF ( value < SELF:buttons:Count )
					SELF:buttons:RemoveRange( value, SELF:buttons:Count - value )
				ELSEIF ( value > SELF:buttons:Count )
					FOR VAR i := 1 TO Value - SELF:buttons:Count
						LOCAL rb AS OptionButton
						//
						rb := OptionButton{}
						rb:AutoSize := true
						rb:Location := System.Drawing.Point{6, 11 + (i-1)*(21+6)}
						rb:Name := "Option" + i:ToString()
						rb:Size := System.Drawing.Size{79, 21}
						rb:TabIndex := 0
						rb:TabStop := true
						rb:Text := "Option" + i:ToString()
						rb:UseVisualStyleBackColor := true
					// Subscribe to checked change event
					VAR handler := EventHandler{ SELF, @AnyOption_CheckedChanged() }
					rb:CheckedChanged += handler
					SELF:buttons:Add( rb )
					NEXT
				ENDIF
				//
				SELF:Size := System.Drawing.Size{91, (SELF:buttons:Count)*(21+6)+2}
			END SET

		END PROPERTY





		CONSTRUCTOR(  )
			SELF:InitializeComponent()
			//
            SELF:buttons := List<OptionButton>{}
            SELF:_value := 0
            SELF:Size := Size{10,15}
			RETURN

		/// <summary>
		/// Gets or sets the currently selected button index.
		/// In VFP, this is 1-based index of the selected option button.
		/// </summary>
		/// <value>The selected button index (1-based). 0 if none selected.</value>
		[Category("VFP Properties"), Description("Selected button index (1-based)")];
		[DefaultValue(0)];
		PROPERTY Value AS INT
			GET
				RETURN SELF:_value
			END GET
			SET
				IF VALUE >= 1 .AND. VALUE <= SELF:buttons:Count
					SELF:_value := VALUE
					// Update the checked state of buttons
					FOR VAR i := 0 TO SELF:buttons:Count - 1
						SELF:buttons[i]:Checked := (i + 1) == VALUE
					NEXT
				ENDIF
			END SET
		END PROPERTY

	// TEMPORARILY COMMENTED OUT TO CHECK FOR DUPLICATE
	/// <summary>
	/// Gets a button by index (1-based for VFP compatibility).
	/// </summary>
	/// <param name="i">1-based index of the button.</param>
	/// <returns>The OptionButton at the specified index.</returns>
	//[Category("VFP Properties"), Description("Access button by index (1-based)")];
	//PUBLIC METHOD Buttons( i AS INT ) AS OptionButton
	//	// VFP uses 1-based indexing
	//	IF i >= 1 .AND. i <= SELF:buttons:Count
	//		RETURN SELF:buttons[ i - 1 ]
	//	ENDIF
	//	RETURN NULL

		/// <summary>
		/// Handles the CheckedChange event for any option button.
		/// </summary>
		PRIVATE METHOD AnyOption_CheckedChanged( sender AS OBJECT, e AS System.EventArgs ) AS VOID
			// Find which button was checked
			FOR VAR i := 0 TO SELF:buttons:Count - 1
				IF SELF:buttons[i]:Checked
					SELF:_value := i + 1  // Store as 1-based
					RETURN
				ENDIF
			NEXT

#include "Headers\ControlSource.xh"

	END CLASS
END NAMESPACE
