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
	/// VFP-compatible tab container that wraps <see cref="System.Windows.Forms.TabControl"/>.<br/>
	/// Each VFP <c>Page</c> corresponds to a <see cref="System.Windows.Forms.TabPage"/> (typed as <see cref="Page"/>).
	/// Supports VFP tab properties: <see cref="ActivePage"/> (1-based), <see cref="PageCount"/> (adds/removes tabs),
	/// <see cref="Tabs"/> (show/hide tab strip), <see cref="TabOrientation"/> (0-3 = Top/Left/Bottom/Right),
	/// <see cref="TabStyle"/> (0=Rounded/1=Straight), <see cref="TabStretch"/> (0=fill/1=fixed),
	/// <see cref="PageHeight"/>, <see cref="PageWidth"/>.<br/>
	/// Fires VFP <c>Activate</c> and <c>Deactivate</c> on each <see cref="Page"/> when the selection changes.
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

		/// <summary>Fires <c>Deactivate</c> on the previously selected <see cref="Page"/> and <c>Activate</c> on the newly selected one, then updates <see cref="_lastSelectedIndex"/>.</summary>
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
		/// <summary>VFP 1-based index of the currently selected tab. Maps to <see cref="System.Windows.Forms.TabControl.SelectedIndex"/> + 1. Ignored if out of range.</summary>
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
		/// <summary>Number of pages (tabs). Setting a larger value adds new <see cref="Page"/> instances with default captions; setting a smaller value removes pages from the end.</summary>
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
		/// <summary>Returns a <see cref="VFPPageCollection"/> wrapper that exposes VFP-style 1-based access to the tab pages via <c>Pages(n)</c> or <c>Pages[n]</c>.</summary>
		PROPERTY Pages AS VFPPageCollection
			GET
				RETURN VFPPageCollection{ SELF }
			END GET
		END PROPERTY

		// ── Tabs ─────────────────────────────────────────────────────────────
		/// <summary>When <c>.T.</c> (default), the tab strip is visible. When <c>.F.</c>, the strip is hidden by setting <c>SizeMode = Fixed</c> and <c>ItemSize.Height = 1</c>.</summary>
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
		/// <summary>Position of the tab strip: 0=Top (default), 1=Left, 2=Bottom, 3=Right. Maps to <see cref="System.Windows.Forms.TabControl.Alignment"/>.</summary>
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
		/// <summary>Tab appearance: 0=Rounded/Normal (default), 1=Straight/Buttons. Maps to <see cref="System.Windows.Forms.TabControl.Appearance"/>.</summary>
		PROPERTY TabStyle AS LONG
			GET
				RETURN IIF( SELF:Appearance == TabAppearance.Normal, 0, 1 )
			END GET
			SET
				SELF:Appearance := IIF( VALUE == 0, TabAppearance.Normal, TabAppearance.Buttons )
			END SET
		END PROPERTY

		// ── TabStretch ────────────────────────────────────────────────────────
		/// <summary>Tab sizing: 0=stretch tabs to fill the tab bar width (default, <c>SizeMode.Normal</c>), 1=fixed width (<c>SizeMode.Fixed</c>).</summary>
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
		/// <summary>Pixel height of the page content area (tab strip excluded). Getting approximates <c>Height − ItemSize.Height − 4</c>; setting adjusts the total control height to match.</summary>
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
		/// <summary>Pixel width of the page content area. Maps directly to the control's <c>Width</c>.</summary>
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
		/// <summary>Name of the VFP method called when the page frame is resized.</summary>
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
		PROPERTY vfpResize AS STRING GET _VFPResize?:SendTo SET _VFPResize := VFPOverride{SELF, VALUE}

		/// <summary>Fires the <c>vfpResize</c> handler when the control is resized.</summary>
		PROTECTED OVERRIDE METHOD OnResize(e AS System.EventArgs) AS VOID
			SUPER:OnResize(e)
			IF SELF:_VFPResize != NULL ; SELF:_VFPResize:Call() ; ENDIF

		PRIVATE _VFPMoved AS VFPOverride
		/// <summary>Name of the VFP method called when the page frame is moved.</summary>
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
		PROPERTY vfpMoved AS STRING GET _VFPMoved?:SendTo SET _VFPMoved := VFPOverride{SELF, VALUE}

		/// <summary>Fires the <c>vfpMoved</c> handler when the control is moved.</summary>
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

		/// <summary>Returns the <see cref="Page"/> at VFP 1-based position <paramref name="n"/>. Returns <c>NULL_OBJECT</c> if out of range.</summary>
		PROPERTY Item[ n AS INT ] AS Page
			GET
				IF n >= 1 .AND. n <= SELF:_owner:TabPages:Count
					RETURN (Page) SELF:_owner:TabPages[n - 1]
				ENDIF
				RETURN NULL_OBJECT
			END GET
		END PROPERTY

		/// <summary>Number of pages in the owning <see cref="PageFrame"/>.</summary>
		PROPERTY Count AS INT
			GET
				RETURN SELF:_owner:TabPages:Count
			END GET
		END PROPERTY
	END CLASS

END NAMESPACE
