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
	/// VFP-compatible non-visual timer that wraps <see cref="System.Windows.Forms.Timer"/>.<br/>
	/// Key behavioural differences from the WinForms base:<br/>
	/// • The timer starts automatically on the first <see cref="Interval"/> assignment (matching VFP's
	///   default-enabled behaviour) unless <c>Interval</c> is set to 0, which stops it.<br/>
	/// • <see cref="Reset"/> restarts the timer by toggling <c>Enabled</c>.<br/>
	/// Non-visual stubs (<see cref="Height"/>, <see cref="Width"/>, <see cref="Left"/>,
	/// <see cref="Top"/>, <see cref="Parent"/>) allow generated VFP position code to compile
	/// without errors.
	/// </summary>
	PARTIAL CLASS Timer INHERIT System.Windows.Forms.Timer

		#include "Headers/VFPObject.xh"

		// ── Non-visual position/size stubs ───────────────────────────────────
		/// <summary>
		/// Non-visual stub — stored for source compatibility; has no effect on the timer.
		/// </summary>
		PROPERTY Height AS LONG AUTO
		/// <summary>
		/// Non-visual stub — stored for source compatibility; has no effect on the timer.
		/// </summary>
		PROPERTY Width  AS LONG AUTO
		/// <summary>
		/// Non-visual stub — stored for source compatibility; has no effect on the timer.
		/// </summary>
		PROPERTY Left   AS LONG AUTO
		/// <summary>
		/// Non-visual stub — stored for source compatibility; has no effect on the timer.
		/// </summary>
		PROPERTY Top    AS LONG AUTO
		/// <summary>
		/// Non-visual stub — stored for source compatibility; has no effect on the timer.
		/// </summary>
		PROPERTY Parent AS OBJECT AUTO

		// ── Non-Control property stubs ────────────────────────────────────────
		// System.Windows.Forms.Timer inherits Component (not Control), so the standard
		// WinForms Control properties used by generated designer code must be stubbed.

		/// <summary>VFP AutoScaleMode stub — stored for source compatibility.</summary>
		PROPERTY AutoScaleMode AS System.Windows.Forms.AutoScaleMode AUTO

		/// <summary>
		/// The name of the timer instance. Stored for source compatibility;
		/// System.Windows.Forms.Timer does not expose a Name property.
		/// </summary>
		PROPERTY Name AS STRING AUTO

		/// <summary>
		/// Position of the timer. Backed by <see cref="Left"/> and <see cref="Top"/>;
		/// stored for source compatibility since Timer is a non-visual component.
		/// </summary>
		PROPERTY Location AS System.Drawing.Point
			GET ; RETURN System.Drawing.Point{(INT)SELF:Left, (INT)SELF:Top} ; END GET
			SET ; SELF:Left := VALUE:X ; SELF:Top := VALUE:Y ; END SET
		END PROPERTY

		/// <summary>
		/// Size of the timer. Backed by <see cref="Width"/> and <see cref="Height"/>;
		/// stored for source compatibility since Timer is a non-visual component.
		/// </summary>
		PROPERTY Size AS System.Drawing.Size
			GET ; RETURN System.Drawing.Size{(INT)SELF:Width, (INT)SELF:Height} ; END GET
			SET ; SELF:Width := VALUE:Width ; SELF:Height := VALUE:Height ; END SET
		END PROPERTY

		// ── vfpTimer event ───────────────────────────────────────────────────
		PRIVATE _VFPTimer AS VFPOverride
		/// <summary>
		/// Name of the VFP method called on each timer tick.<br/>
		/// Assigning this property wires the underlying <c>System.Windows.Forms.Timer.Tick</c> event
		/// the first time (lazy wiring — avoids double-firing if reassigned later).
		/// </summary>
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

		/// <summary>
		/// Forwards VFP-style construction arguments; args are accepted for source compatibility
		/// but are not passed to the base (System.Windows.Forms.Timer has no params constructor).
		/// </summary>
		CONSTRUCTOR(args PARAMS USUAL[])
			SELF:firstSet := TRUE
			RETURN

		/// <summary>
		/// Starts (<c>.T.</c>) or stops (<c>.F.</c>) the timer. Shadows the base property to allow the generated code to set it without type-cast issues.
		/// </summary>
		PUBLIC NEW PROPERTY Enabled AS LOGIC
			GET
				RETURN SUPER:Enabled
			END GET

			SET
				SUPER:Enabled := VALUE
			END SET
		END PROPERTY

		/// <summary>
		/// Tick interval in milliseconds. Setting to 0 stops the timer (saves the previous
		/// <c>Enabled</c> state). Setting a non-zero value after 0 restores <c>Enabled</c>
		/// automatically on the first assignment, matching VFP's default-enabled behaviour.
		/// </summary>
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

		/// <summary>
		/// Restarts the timer by toggling <c>Enabled</c> off then on, resetting the interval countdown.
		/// </summary>
		METHOD Reset AS VOID Strict
			SELF:Enabled := FALSE
			SELF:Enabled := TRUE
			RETURN

	END CLASS
END NAMESPACE // XSharp.VFP.UI
