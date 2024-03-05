// VFPComboBox.prg
// Created by    : fabri
// Creation Date : 9/20/2022 10:38:55 PM
// Created for   :
// WorkStation   : FABXPS

USING System
USING System.Collections.Generic
USING System.Text
USING System.Windows.Forms
USING System.ComponentModel
USING System.Drawing
USING System.ComponentModel

BEGIN NAMESPACE XSharp.VFP.UI

	/// <summary>
    /// The VFPComboBox class.
    /// </summary>
	PARTIAL CLASS ComboBox INHERIT System.Windows.Forms.ComboBox

		// Common properties that all VFP Objects support
		#include "Headers/VFPObject.xh"

		CONSTRUCTOR(  ) STRICT
			SUPER()
			RETURN

#include ".\Headers\ControlProperties.xh"

#include ".\Headers\ControlSource.xh"

		PROPERTY DisplayCount AS LONG AUTO
		PROPERTY InputMask AS STRING AUTO
		PROPERTY NullDisplay AS String AUTO
		PROPERTY OLEDropTextInsertion AS LONG AUTO

		PROPERTY Picture AS STRING AUTO
		PROPERTY PictureSelectionDisplay  AS LONG AUTO
		PROPERTY ReadOnly AS LOGIC AUTO

		PROPERTY SelLength AS LONG GET SELF:SelectionLength SET SELF:SelectionLength := Value
		PROPERTY SelStart AS LONG GET SELF:SelectionStart SET SELF:SelectionStart := Value
		PROPERTY SelText AS STRING GET SELF:SelectedText  SET SelectedText  := Value
		PROPERTY SelectOnEntry AS LOGIC AUTO
		PROPERTY Style AS LONG AUTO


	END CLASS
END NAMESPACE // XSharp.VFP.UI