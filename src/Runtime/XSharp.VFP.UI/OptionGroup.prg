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

		PUBLIC PROPERTY ButtonCount AS INT
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
						SELF:buttons:Add( rb )
					NEXT
				ENDIF
				//
				SELF:Size := System.Drawing.Size{91, (SELF:buttons:Count)*(21+6)+2}
			END SET

		END PROPERTY





		CONSTRUCTOR(  )
			InitializeComponent()
			//
            SELF:buttons := List<OptionButton>{}
            SELF:Size := Size{10,15}
			RETURN

		PUBLIC METHOD Button( i as int ) AS OptionButton
			RETURN SELF:buttons[ i ]


#include "Headers\ControlSource.xh"

	END CLASS
END NAMESPACE
