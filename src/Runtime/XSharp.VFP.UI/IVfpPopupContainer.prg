// IVfpPopupContainer.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.

USING System

BEGIN NAMESPACE XSharp.VFP.UI

	/// <summary>
	/// Common surface shared by <see cref="Popup"/> (DEFINE POPUP) and <see cref="ContextMenu"/>
	/// (DEFINE POPUP ... SHORTCUT), so the MenuCommands.prg helpers that implement DEFINE BAR,
	/// SET SKIP OF POPUP/BAR, ON SELECTION BAR, and RELEASE POPUPS can operate on either registry
	/// without caring whether the popup was declared SHORTCUT.
	/// </summary>
	INTERFACE IVfpPopupContainer
		PROPERTY Name AS STRING GET
		PROPERTY BarCount AS LONG GET
		PROPERTY Bars[ i AS LONG ] AS Bar GET
		PROPERTY Skip AS LOGIC GET SET
		METHOD AddBar( cName, cCaption ) AS USUAL CLIPPER
		METHOD Release() AS USUAL CLIPPER
	END INTERFACE

END NAMESPACE
