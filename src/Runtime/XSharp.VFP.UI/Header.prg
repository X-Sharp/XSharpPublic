// VFPColumnHeader.prg
// Created by    : fabri
// Creation Date : 9/2/2022 10:25:55 PM
// Created for   :
// WorkStation   : FABXPS


USING System
USING System.Collections.Generic
USING System.Text
USING System.ComponentModel

BEGIN NAMESPACE XSharp.VFP.UI

	/// <summary>
	/// The VFPColumnHeader class.
	/// </summary>
	CLASS Header INHERIT System.Windows.Forms.DataGridViewColumnHeaderCell

		CONSTRUCTOR(  )
			SUPER()
			RETURN

		CONSTRUCTOR(  source AS System.Windows.Forms.DataGridViewColumnHeaderCell )
			SUPER()
			//
			self:ErrorText := source:ErrorText
            self:Tag := source:Tag
            self:ToolTipText := source:ToolTipText
            self:Value := source:Value
            self:ContextMenuStrip := source:ContextMenuStrip
            self:ValueType := source:ValueType
            if (source:HasStyle)
                self:Style := source:Style
			endif
			RETURN


			/// <summary>
			/// Text is the Value of the HeaderCell
			/// </summary>
			/// <value></value>
		PROPERTY Text AS STRING
			GET
				IF SELF:Value != NULL
					return SELF:Value:ToString()
				ELSE
					RETURN NULL
				ENDIF
			END GET
			SET
				//#warning !!!! This affectation doesn't work when Case Sensitive is ON <<<<<<<<<<<<<<<<<<<<<
				SUPER:Value := (object)value
			END SET
		END PROPERTY

		PROPERTY Name AS STRING AUTO

		PROPERTY FontSize AS LONG AUTO

		PROPERTY TextAlign AS System.Drawing.ContentAlignment AUTO

		OVERRIDE METHOD Clone() AS OBJECT
			LOCAL source AS Header
			//
			source := Header{}
			SELF:ErrorText := source:ErrorText
            self:Tag := source:Tag
            self:ToolTipText := source:ToolTipText
            self:Value := source:Value
            self:ContextMenuStrip := source:ContextMenuStrip
            self:ValueType := source:ValueType

            // Avoid creating a new style object if the Style property has not previously been set.
            if (source:HasStyle)
                self:Style := source:Style
			endif
			source:Name := SELF:Name
			//
			RETURN source

	END CLASS
END NAMESPACE // XSharp.VFP.UI