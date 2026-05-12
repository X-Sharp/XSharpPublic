// PageFrame.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.

USING System
USING System.Collections.Generic
USING System.Windows.Forms
USING System.Drawing
USING System.ComponentModel

BEGIN NAMESPACE XSharp.VFP.UI

	/// <summary>
	/// The VFP compatible PageFrame class.
	/// Maps to System.Windows.Forms.TabControl.
	/// Each VFP Page maps to a TabPage.
	/// </summary>
	PARTIAL CLASS PageFrame INHERIT System.Windows.Forms.TabControl

		// Note: VFPObject.xh and VFPProperties.xh are provided by PageFrame.generated.prg
		// (via VFPContainer.xh) — do not include them again here.

		#include "ControlProperties.xh"

		#include "FontProperties.xh"

		CONSTRUCTOR() STRICT
			SUPER()
			SELF:Size := Size{300, 200}
			SELF:_lastSelectedIndex := -1
			SELF:_tabsVisible := TRUE

		// ── SelectedIndexChanged → Activate/Deactivate ───────────────────────
		PRIVATE _lastSelectedIndex AS INT

		PROTECTED OVERRIDE METHOD OnSelectedIndexChanged( e AS System.EventArgs ) AS VOID STRICT
			// Deactivate the previously selected page
			IF SELF:_lastSelectedIndex >= 0 .AND. SELF:_lastSelectedIndex < SELF:TabPages:Count
				VAR prev := SELF:TabPages[SELF:_lastSelectedIndex] ASTYPE Page
				IF prev != NULL_OBJECT
					prev:FireDeactivate()
				ENDIF
			ENDIF
			SUPER:OnSelectedIndexChanged( e )
			// Activate the newly selected page
			SELF:_lastSelectedIndex := SELF:SelectedIndex
			IF SELF:SelectedIndex >= 0 .AND. SELF:SelectedIndex < SELF:TabPages:Count
				VAR curr := SELF:TabPages[SELF:SelectedIndex] ASTYPE Page
				IF curr != NULL_OBJECT
					curr:FireActivate()
				ENDIF
			ENDIF

		// ── ActivePage ───────────────────────────────────────────────────────
		// VFP ActivePage is 1-based; WinForms SelectedIndex is 0-based.
		PROPERTY ActivePage AS LONG
			GET
				RETURN SELF:SelectedIndex + 1
			END GET
			SET
				IF VALUE >= 1 .AND. VALUE <= SELF:TabPages:Count
					SELF:SelectedIndex := VALUE - 1
				ENDIF
			END SET
		END PROPERTY

		// ── PageCount ────────────────────────────────────────────────────────
		PROPERTY PageCount AS LONG
			GET
				RETURN SELF:TabPages:Count
			END GET
			SET
				DO WHILE SELF:TabPages:Count < VALUE
					VAR pg := Page{}
					pg:Caption := "Page" + (SELF:TabPages:Count + 1):ToString()
					SELF:TabPages:Add( (System.Windows.Forms.TabPage) pg )
				ENDDO
				DO WHILE SELF:TabPages:Count > VALUE .AND. VALUE >= 0
					SELF:TabPages:RemoveAt( SELF:TabPages:Count - 1 )
				ENDDO
			END SET
		END PROPERTY

		// ── Pages ─────────────────────────────────────────────────────────────
		// VFP: PageFrame.Pages(n) is 1-based; returns the nth Page object.
		// We expose a VFPPageCollection wrapper that supports both indexed property
		// access (Pages[n]) and direct call-syntax via the Item default property.
		PROPERTY Pages AS VFPPageCollection
			GET
				RETURN VFPPageCollection{ SELF }
			END GET
		END PROPERTY

		// ── Tabs ─────────────────────────────────────────────────────────────
		// .T. = show tabs normally; .F. = hide tab strip entirely
		PROPERTY Tabs AS LOGIC
			GET
				RETURN SELF:_tabsVisible
			END GET
			SET
				SELF:_tabsVisible := VALUE
				IF VALUE
					SELF:SizeMode  := TabSizeMode.Normal
					SELF:ItemSize  := Size{0, 0}   // restore default
				ELSE
					// Fixed + zero height effectively hides the tab strip on all themes
					SELF:SizeMode  := TabSizeMode.Fixed
					SELF:ItemSize  := Size{0, 1}
				ENDIF
			END SET
		END PROPERTY
		PRIVATE _tabsVisible AS LOGIC

		// ── TabOrientation ───────────────────────────────────────────────────
		// VFP: 0=Top (default), 1=Left, 2=Bottom, 3=Right
		PROPERTY TabOrientation AS LONG
			GET
				SWITCH SELF:Alignment
					CASE TabAlignment.Top    ; RETURN 0
					CASE TabAlignment.Left   ; RETURN 1
					CASE TabAlignment.Bottom ; RETURN 2
					CASE TabAlignment.Right  ; RETURN 3
				END SWITCH
				RETURN 0
			END GET
			SET
				SWITCH VALUE
					CASE 0 ; SELF:Alignment := TabAlignment.Top
					CASE 1 ; SELF:Alignment := TabAlignment.Left
					CASE 2 ; SELF:Alignment := TabAlignment.Bottom
					CASE 3 ; SELF:Alignment := TabAlignment.Right
				END SWITCH
			END SET
		END PROPERTY

		// ── TabStyle ─────────────────────────────────────────────────────────
		// VFP: 0=Rounded (default), 1=Straight
		PROPERTY TabStyle AS LONG
			GET
				RETURN IIF( SELF:Appearance == TabAppearance.Normal, 0, 1 )
			END GET
			SET
				SELF:Appearance := IIF( VALUE == 0, TabAppearance.Normal, TabAppearance.Buttons )
			END SET
		END PROPERTY

		// ── TabStretch ────────────────────────────────────────────────────────
		// VFP: 0=stretch tabs to fill tab bar width (default), 1=don't stretch
		// Maps to TabControl.SizeMode: Normal (stretch) vs Fixed (don't stretch).
		PROPERTY TabStretch AS LONG
			GET
				RETURN IIF(SELF:SizeMode == TabSizeMode.Normal, 0, 1)
			END GET
			SET
				IF VALUE == 0
					SELF:SizeMode := TabSizeMode.Normal
				ELSE
					SELF:SizeMode := TabSizeMode.Fixed
				ENDIF
			END SET
		END PROPERTY

		// ── PageHeight ────────────────────────────────────────────────────────
		// VFP PageHeight is the pixel height of the content area of each tab page.
		// In WinForms this is the TabControl height minus the tab-strip height.
		// Setting it resizes the whole TabControl so the content area matches.
		PROPERTY PageHeight AS LONG
			GET
				// Approximate: total height minus one row of tabs
				RETURN SELF:Height - SELF:ItemSize:Height - 4
			END GET
			SET
				IF VALUE > 0
					SELF:Height := VALUE + SELF:ItemSize:Height + 4
				ENDIF
			END SET
		END PROPERTY

		// ── PageWidth ─────────────────────────────────────────────────────────
		PROPERTY PageWidth AS LONG
			GET
				RETURN SELF:Width
			END GET
			SET
				IF VALUE > 0
					SELF:Width := VALUE
				ENDIF
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

	END CLASS

	/// <summary>
	/// 1-based indexed wrapper for PageFrame.Pages(n) VFP syntax.
	/// </summary>
	CLASS VFPPageCollection
		PRIVATE _owner AS System.Windows.Forms.TabControl

		CONSTRUCTOR( owner AS System.Windows.Forms.TabControl ) STRICT
			SELF:_owner := owner

		// Default indexed property — VFP calls Pages(n) with 1-based n
		PROPERTY Item[ n AS INT ] AS Page
			GET
				IF n >= 1 .AND. n <= SELF:_owner:TabPages:Count
					RETURN (Page) SELF:_owner:TabPages[n - 1]
				ENDIF
				RETURN NULL_OBJECT
			END GET
		END PROPERTY

		PROPERTY Count AS INT
			GET
				RETURN SELF:_owner:TabPages:Count
			END GET
		END PROPERTY
	END CLASS

END NAMESPACE
