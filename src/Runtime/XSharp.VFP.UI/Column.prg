// VFPGridColumn.prg
// Created by    : fabri
// Creation Date : 5/4/2022 9:52:42 PM
// Created for   :
// WorkStation   : FABXPS


USING System
USING System.Collections.Generic
USING System.Text
USING System.Windows.Forms
USING System.ComponentModel

BEGIN NAMESPACE XSharp.VFP.UI

	/// <summary>
	/// The VFPGridColumn class.
	/// </summary>
	PARTIAL CLASS Column INHERIT System.Windows.Forms.DataGridViewTextBoxColumn IMPLEMENTS IDynamicProperties, IDynamicProperties2, IVFPOwner

		CONSTRUCTOR(  )
			SUPER()
			RETURN

			// Todo
		PROPERTY ControlSource AS STRING AUTO

		PROPERTY Font AS System.Drawing.Font GET SELF:DefaultCellStyle:Font SET SELF:DefaultCellStyle:Font := VALUE

		PROPERTY Header AS Header
		GET
			IF SELF:HeaderCell IS Header
				RETURN (Header)SELF:HeaderCell
			ENDIF
			VAR oHeader := Header{ SELF:HeaderCell }
			SELF:HeaderCell := oHeader
			RETURN oHeader
		END GET
		SET
			SELF:HeaderCell := VALUE
		END SET
		END PROPERTY

		PROPERTY TextBox AS TextBox AUTO

#include ".\Headers\FontProperties.xh"



	END CLASS
END NAMESPACE // XSharp.VFP.UI