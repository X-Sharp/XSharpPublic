// VFPEditBox.prg
USING System
USING System.Collections.Generic
USING System.Text
USING System.Windows.Forms
USING System.Drawing
USING System.ComponentModel

BEGIN NAMESPACE XSharp.VFP.UI
	/// <summary>
	/// The VFPEditBox class.
	/// </summary>
	PARTIAL CLASS EditBox INHERIT TextBox

		/// <summary>
		/// Specifies that the EditBox should insert linefeed characters (CHR(10)) after carriage return characters
		/// (CHR(13)) within the text of an EditBox whenever the Value property is read or whenever the value is
		/// stored to the ControlSource.
		/// </summary>
		/// <value></value>
		// TODO: Implement AddLineFeeds
		PROPERTY AddLineFeeds AS LOGIC AUTO
		/// <summary>
		/// Specifies whether to allow tabs in an EditBox control.
		/// </summary>
		/// <value></value>
		// TODO: Implement AllowTabs
		PROPERTY AllowTabs AS LOGIC AUTO

		CONSTRUCTOR( )
			SUPER()
			SELF:Multiline := TRUE
			SELF:ScrollBars := ScrollBars.Both

#include ".\Headers\ControlSource.xh"

	END CLASS

END NAMESPACE
