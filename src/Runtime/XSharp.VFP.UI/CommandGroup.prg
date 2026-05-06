// CommandGroup.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.

USING System
USING System.Collections.Generic
USING System.ComponentModel
USING System.Data
USING System.Drawing
USING System.Drawing.Drawing2D
USING System.Text
USING System.Windows.Forms

BEGIN NAMESPACE XSharp.VFP.UI
	/// <summary>
	/// The VFP compatible CommandGroup class.
	/// </summary>
	PUBLIC PARTIAL CLASS CommandGroup ;
	INHERIT System.Windows.Forms.UserControl IMPLEMENTS IDynamicProperties, IDynamicProperties2, IVFPOwner

		#include "ControlProperties.xh"

		// ── ControlCount ──────────────────────────────────────────────────────
		// VFP_CONTROLCOUNT_OVERRIDE suppresses the AUTO stub in VFPContainer.xh.
		// Interface IVFPOwner requires a setter — it is a no-op (ControlCount is read-only in VFP).
		PROPERTY ControlCount AS INT
			GET ; RETURN SELF:Controls:Count ; END GET
			SET ; NOP ; END SET
		END PROPERTY

		// ── BorderColor ───────────────────────────────────────────────────────
		PRIVATE _borderColor AS System.Drawing.Color
		PROPERTY BorderColor AS System.Drawing.Color
			GET ; RETURN _borderColor ; END GET
			SET ; _borderColor := VALUE ; SELF:Invalidate() ; END SET
		END PROPERTY

		// ── BackStyle ─────────────────────────────────────────────────────────
		// VFP: 0=Transparent, 1=Opaque (default)
		PRIVATE _backStyle := 1 AS INT
		PROPERTY BackStyle AS INT
			GET ; RETURN _backStyle ; END GET
			SET
				_backStyle := VALUE
				SELF:BackColor := IIF(VALUE == 0, System.Drawing.Color.Transparent, System.Drawing.SystemColors.Control)
			END SET
		END PROPERTY

		// ── Resize / Moved events ─────────────────────────────────────────────
		PRIVATE _VFPResize AS VFPOverride
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
		PROPERTY vfpResize AS STRING GET _VFPResize?:SendTo SET _VFPResize := VFPOverride{SELF, VALUE}

		PROTECTED OVERRIDE METHOD OnResize(e AS System.EventArgs) AS VOID
			SUPER:OnResize(e)
			IF SELF:_VFPResize != NULL ; SELF:_VFPResize:Call() ; ENDIF

		PRIVATE _VFPMoved AS VFPOverride
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
		PROPERTY vfpMoved AS STRING GET _VFPMoved?:SendTo SET _VFPMoved := VFPOverride{SELF, VALUE}

		PROTECTED OVERRIDE METHOD OnMove(e AS System.EventArgs) AS VOID
			SUPER:OnMove(e)
			IF SELF:_VFPMoved != NULL ; SELF:_VFPMoved:Call() ; ENDIF

		// ── OnPaint — custom BorderColor border ───────────────────────────────
		PROTECTED OVERRIDE METHOD OnPaint(e AS PaintEventArgs) AS VOID
			SUPER:OnPaint(e)
			IF SELF:_borderColor != System.Drawing.Color.Empty
				VAR pen := Pen{_borderColor, (SINGLE)1}
				e:Graphics:DrawRectangle(pen, 0, 0, SELF:ClientSize:Width - 1, SELF:ClientSize:Height - 1)
				pen:Dispose()
			ENDIF

		PRIVATE buttons AS List<CommandButton>

		PUBLIC PROPERTY ButtonCount AS INT
			GET
				RETURN SELF:buttons:Count
			END GET

			SET
				// Remove excess buttons
				IF value < SELF:buttons:Count
					FOR VAR r := SELF:buttons:Count - 1 DOWNTO value
						SELF:gBox:Controls:Remove( SELF:buttons[r] )
						SELF:buttons[r]:Dispose()
					NEXT
					SELF:buttons:RemoveRange( value, SELF:buttons:Count - value )
				// Add missing buttons
				ELSEIF value > SELF:buttons:Count
					FOR VAR i := SELF:buttons:Count + 1 TO value
						LOCAL btn AS CommandButton
						btn := CommandButton{}
						btn:AutoSize := TRUE
						btn:Location := System.Drawing.Point{6, 11 + (i-1)*(21+6)}
						btn:Name := "Command" + i:ToString()
						btn:Size := System.Drawing.Size{79, 21}
						btn:TabIndex := i - 1
						btn:TabStop := TRUE
						btn:Text := "Command" + i:ToString()
						btn:UseVisualStyleBackColor := TRUE
						btn:Tag := i
						btn:Click += OnButtonClick
						SELF:buttons:Add( btn )
						SELF:gBox:Controls:Add( btn )
					NEXT
				ENDIF
				SELF:Size := System.Drawing.Size{91, (value)*(21+6)+22}
			END SET

		END PROPERTY

		// Returns the 1-based index of the last clicked button, or 0 if none.
		PRIVATE _value AS INT
		PUBLIC PROPERTY Value AS INT
			GET
				RETURN SELF:_value
			END GET
			SET
				SELF:_value := VALUE
				// Programmatic change: CommandButton has no CheckedState — just store value
				NOP
				SELF:OnVFPProgrammaticChange(SELF, System.EventArgs.Empty)
			END SET
		END PROPERTY

		// ── ProgrammaticChange ────────────────────────────────────────────────
		PRIVATE _VFPProgrammaticChange AS VFPOverride
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
		PROPERTY vfpProgrammaticChange AS STRING GET _VFPProgrammaticChange?:SendTo SET _VFPProgrammaticChange := VFPOverride{SELF, VALUE}

		PRIVATE METHOD OnVFPProgrammaticChange(sender AS OBJECT, e AS System.EventArgs) AS VOID
			IF SELF:_VFPProgrammaticChange != NULL
				SELF:_VFPProgrammaticChange:Call()
			ENDIF


		CONSTRUCTOR(  )
			SELF:InitializeComponent()
			SELF:buttons := List<CommandButton>{}
			SELF:_value := 0
			RETURN

		PRIVATE METHOD OnButtonClick( sender AS OBJECT, e AS System.EventArgs ) AS VOID
			LOCAL btn AS System.Windows.Forms.Control
			btn := (System.Windows.Forms.Control) sender
			SELF:_value := (INT) btn:Tag
			SELF:OnVFPInteractiveChange( SELF, e )

		// 1-based access to individual buttons — matches VFP CommandGroup.Buttons[i]
		PUBLIC METHOD Button( i AS INT ) AS CommandButton
			RETURN SELF:buttons[ i - 1 ]

		// Buttons[i] — 1-based indexed property alias
		PUBLIC PROPERTY Buttons[ i AS INT ] AS CommandButton
			GET
				RETURN SELF:buttons[ i - 1 ]
			END GET
		END PROPERTY

#include "ControlSource.xh"

	END CLASS
END NAMESPACE
