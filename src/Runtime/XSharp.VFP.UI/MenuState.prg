// MenuState.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.

BEGIN NAMESPACE XSharp.VFP.UI

	/// <summary>
	/// Static store for the last-selected menu context.
	/// Updated by Bar.OnVFPClick before dispatching the handler, so that
	/// BAR(), PAD(), POPUP() and MENU() return correct values inside handlers.
	/// </summary>
	STATIC CLASS MenuState

		/// <summary>1-based number of the last selected bar (0 = none / ESC).</summary>
		STATIC PROPERTY LastBar   AS INT    AUTO := 0

		/// <summary>Name of the pad that opened the popup containing the last selected bar.</summary>
		STATIC PROPERTY LastPad   AS STRING AUTO := ""

		/// <summary>Name of the popup that contained the last selected bar.</summary>
		STATIC PROPERTY LastPopup AS STRING AUTO := ""

		/// <summary>Name of the menu bar that owns the pad.</summary>
		STATIC PROPERTY LastMenu  AS STRING AUTO := ""

	END CLASS

END NAMESPACE
