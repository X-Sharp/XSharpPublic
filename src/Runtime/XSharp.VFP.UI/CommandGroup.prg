USING System
USING System.Collections.Generic
USING System.ComponentModel
USING System.Data
USING System.Drawing

USING System.Text
USING System.ComponentModel
USING System.Windows.Forms

BEGIN NAMESPACE XSharp.VFP.UI

	PUBLIC PARTIAL CLASS CommandGroup ;
	INHERIT System.Windows.Forms.UserControl IMPLEMENTS IDynamicProperties, IDynamicProperties2, IVFPOwner
        
		PRIVATE buttons AS List<CommandButton>
            
		PUBLIC PROPERTY ButtonCount AS INT
			GET
				return SELF:buttons:Count
			END GET
            
			SET
				SELF:buttons:Clear()
				FOR VAR i := 1 TO Value
					LOCAL btn AS CommandButton
					//
					btn := CommandButton{}
					btn:AutoSize := TRUE
					btn:Location := System.Drawing.Point{6, 11 + (i-1)*(21+6)}
					btn:Name := "Command" + i:ToString()
					btn:Size := System.Drawing.Size{79, 21}
					btn:TabIndex := 0
					btn:TabStop := TRUE
					btn:Text := "Command" + i:ToString()
					btn:UseVisualStyleBackColor := TRUE
					SELF:buttons:Add( btn )
				NEXT
				//
				SELF:Size := System.Drawing.Size{91, (value)*(21+6)+2}
			END SET
            
		END PROPERTY
        

		CONSTRUCTOR(  ) 
			InitializeComponent()
			//
			SELF:buttons := List<CommandButton>{}
			RETURN
        
		PUBLIC METHOD Button( i AS INT ) AS CommandButton
			RETURN SELF:buttons[ i ]

#include ".\Headers\ControlSource.xh"

	END CLASS 
END NAMESPACE
