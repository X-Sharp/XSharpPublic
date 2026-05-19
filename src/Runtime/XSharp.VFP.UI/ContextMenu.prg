// ContextMenu.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.

USING System
USING System.Collections.Generic
USING System.Windows.Forms

BEGIN NAMESPACE XSharp.VFP.UI

	/// <summary>
	/// VFP-compatible ContextMenu class. Represents a right-click (shortcut) menu
	/// attached to a control. Maps to System.Windows.Forms.ContextMenuStrip.
	/// Assign to a control via ctrl.ContextMenuStrip := oContextMenu.
	/// Add items via AddBar(). A caption of "--" inserts a separator.
	/// Action methods live on a subclass — vfpClick dispatch finds them via
	/// Bar.Owner (which is this ContextMenu instance directly).
	/// </summary>
PARTIAL CLASS ContextMenu INHERIT System.Windows.Forms.ContextMenuStrip


		PRIVATE _bars AS List<Bar>

		CONSTRUCTOR() STRICT
			SUPER()
			SELF:_bars := List<Bar>{}

		// ── BarCount ──────────────────────────────────────────────────────────
		/// <summary>Number of <see cref="Bar"/> items in this menu. Separator entries are not counted.</summary>
		PROPERTY BarCount AS LONG
			GET
				RETURN (LONG) SELF:_bars:Count
			END GET
		END PROPERTY

		// ── Bars[i] ───────────────────────────────────────────────────────────
		/// <summary>1-based indexed access to <see cref="Bar"/> items (separators excluded).</summary>
		PROPERTY Bars[ i AS LONG ] AS Bar
			GET
				RETURN SELF:_bars[ (INT) i - 1 ]
			END GET
		END PROPERTY

		// ── AddBar ────────────────────────────────────────────────────────────
		/// <summary>Adds an item to the menu. When <paramref name="cCaption"/> is <c>"--"</c>, inserts a <see cref="System.Windows.Forms.ToolStripSeparator"/>; otherwise creates a <see cref="Bar"/> and registers it as a dynamic property under <paramref name="cName"/>.</summary>
		METHOD AddBar( cName, cCaption ) AS USUAL CLIPPER
			LOCAL sName    AS STRING
			LOCAL sCaption AS STRING
			sName    := (STRING) cName
			sCaption := (STRING) cCaption
			IF sCaption == "--"
				LOCAL sep AS ToolStripSeparator
				sep      := ToolStripSeparator{}
				sep:Name := sName
				SELF:Items:Add( sep )
			ELSE
				LOCAL oBar AS Bar
				oBar         := Bar{}
				oBar:Name    := sName
				oBar:Caption := sCaption
				SELF:Items:Add( oBar )
				SELF:_bars:Add( oBar )
				SELF:AddProperty( sName, oBar, PropertyVisibility.Public, "AddBar" )
			ENDIF
			RETURN NIL

		// ── Lifecycle stubs ───────────────────────────────────────────────────
		/// <summary>VFP Init lifecycle stub — overridden by the subclass to run initialisation code.</summary>
		VIRTUAL METHOD Init() AS USUAL CLIPPER
			RETURN NIL

		/// <summary>VFP Destroy lifecycle stub — overridden by the subclass to run cleanup code.</summary>
		VIRTUAL METHOD Destroy() AS USUAL CLIPPER
			RETURN NIL

	END CLASS

END NAMESPACE
