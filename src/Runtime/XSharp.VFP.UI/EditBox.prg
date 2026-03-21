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
					SELF:Text := VALUE:ToString()
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

	END CLASS

END NAMESPACE
