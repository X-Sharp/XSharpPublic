// Timer.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.


USING System
USING System.Collections.Generic
USING System.Text
USING System.ComponentModel

BEGIN NAMESPACE XSharp.VFP.UI

	/// <summary>
	/// The VFP compatible Timer class.
	/// </summary>
	PARTIAL CLASS Timer INHERIT System.Windows.Forms.Timer
		PROTECTED firstSet AS LOGIC

		CONSTRUCTOR( )
			// In VFP, Timer is enabled per default
			// So, on first Interval set, we enable the Timer
			SELF:firstSet := TRUE

			RETURN

		PUBLIC NEW PROPERTY Enabled AS LOGIC
			GET
				RETURN SUPER:Enabled
			END GET

			SET
				SUPER:Enabled := VALUE
			END SET
		END PROPERTY

		PUBLIC NEW PROPERTY Interval AS INT
			GET
				RETURN SUPER:Interval
			END GET

			SET
				IF VALUE == 0
					SELF:firstSet := SELF:Enabled
					SELF:Enabled := FALSE
				ELSE
					SUPER:Interval := VALUE
					IF SELF:firstSet
						SELF:Enabled := TRUE
						SELF:firstSet := FALSE
					ENDIF
				ENDIF
			END SET
		END PROPERTY

		METHOD Reset AS VOID Strict
			// TODO: Implement Reset
			RETURN

	END CLASS
END NAMESPACE // XSharp.VFP.UI
