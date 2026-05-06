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

		#include "Headers/VFPObject.xh"

		// ── Non-visual position/size stubs ───────────────────────────────────
		// Timer has no visual representation; these prevent VFP code from erroring.
		PROPERTY Height AS LONG AUTO
		PROPERTY Width  AS LONG AUTO
		PROPERTY Left   AS LONG AUTO
		PROPERTY Top    AS LONG AUTO
		PROPERTY Parent AS OBJECT AUTO

		// ── vfpTimer event ───────────────────────────────────────────────────
		// VFP Timer event: fires at each Interval tick.
		PRIVATE _VFPTimer AS VFPOverride
		[Category("VFP Events"), Description("Occurs at each timer interval.")];
		[DefaultValue(NULL)];
		PROPERTY vfpTimer AS STRING GET _VFPTimer?:SendTo SET Set_VFPTimer( VFPOverride{SELF, VALUE} )

		METHOD Set_VFPTimer( methodCall AS VFPOverride ) AS VOID
			IF SELF:_VFPTimer == NULL
				SELF:Tick += System.EventHandler{ SELF, @OnVFPTimer() }
			ENDIF
			SELF:_VFPTimer := methodCall

		PRIVATE METHOD OnVFPTimer( sender AS OBJECT, e AS System.EventArgs ) AS VOID
			IF SELF:_VFPTimer != NULL
				SELF:_VFPTimer:Call()
			ENDIF

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
			SELF:Enabled := FALSE
			SELF:Enabled := TRUE
			RETURN

	END CLASS
END NAMESPACE // XSharp.VFP.UI
