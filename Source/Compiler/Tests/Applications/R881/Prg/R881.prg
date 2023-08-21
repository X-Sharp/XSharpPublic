// https://github.com/X-Sharp/XSharpPublic/issues/1189
USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text

BEGIN NAMESPACE valueError
	CLASS Class1 INHERIT System.Windows.Forms.DataGridViewColumnHeaderCell

		CONSTRUCTOR() STRICT
			RETURN

		PROPERTY Text AS STRING
			GET
				IF SELF:Value != NULL
					return SELF:Value:ToString()
				ELSE
					RETURN NULL
				ENDIF
			END GET
			SET
				SELF:Value := (object)value
			END SET
		END PROPERTY
	END CLASS
END NAMESPACE
