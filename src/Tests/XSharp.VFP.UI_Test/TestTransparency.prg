// TestTransparency.prg
//
// Manual test for the BackStyle=Transparent Container/Shape z-order fix
// (Form.OnLoad -> __VFPFixTransparentZOrder). Reproduces the classic VFP "About
// dialog" bug case: a grouping Container is added to the form BEFORE the labels
// it's meant to sit behind, so without the fix it would paint over them
// (WinForms transparent-sibling compositing only reaches the Parent, not
// siblings). Only Label siblings get elevated in front of the container --
// cmdB1 deliberately reproduces the click-routing half of the original VFP9
// click-through test (see project_vfp_ui_transparency_plan memory): a real VFP9
// transparent container swallows clicks aimed at whatever it visually shows
// through, it doesn't forward them. Our WinForms port can't replicate that
// paint-through without full compositing, so cmdB1 stays exactly where it was
// (behind c1, both invisible AND unclickable there) rather than becoming MORE
// click-permissive than real VFP by accident. c1.cmdB2 (a real child of the
// container) stays visible/clickable regardless, as a baseline.

USING System
USING System.Drawing
USING XSharp.VFP.UI

// NOTE: CommandButton hides the inherited Control.Click *event* behind a plain
// NEW METHOD Click() (see Headers/ControlEventHandlers.xh) for VFP PEM compatibility,
// so it can't be wired with += from outside the class. Not wired here since this
// test is about z-order/paint and click-swallowing, verified visually/manually.
FUNCTION ShowTransparencyTest() AS VOID
	LOCAL oForm AS XSharp.VFP.UI.Form
	oForm := XSharp.VFP.UI.Form{}
	oForm:Text := "Transparency Z-Order Test"
	oForm:Size := Size{ 440, 340 }

	// Container added FIRST, so it lands frontmost by default WinForms z-order --
	// this reproduces the bug ordering. __VFPFixTransparentZOrder (Form.OnLoad)
	// elevates the labels in front of it, but leaves cmdB1 (not a Label) exactly
	// where it started: behind the container, both invisible and unclickable there.
	LOCAL oContainer AS XSharp.VFP.UI.Container
	oContainer := XSharp.VFP.UI.Container{}
	oContainer:Name := "c1"
	oContainer:Location := Point{ 20, 20 }
	oContainer:Size := Size{ 380, 220 }
	oContainer:BackStyle := 0   // Transparent
	oContainer:BackColor := Color.Transparent

	LOCAL oCmdB1 AS XSharp.VFP.UI.CommandButton
	oCmdB1 := XSharp.VFP.UI.CommandButton{}
	oCmdB1:Name := "cmdB1"
	oCmdB1:Text := "cmdB1 (Form child, under c1) - should stay HIDDEN"
	oCmdB1:Location := Point{ 40, 40 }
	oCmdB1:Size := Size{ 260, 30 }

	LOCAL oLabel1 AS XSharp.VFP.UI.Label
	oLabel1 := XSharp.VFP.UI.Label{}
	oLabel1:Name := "Label1"
	oLabel1:Text := "Label 1 - should be VISIBLE (was hidden before the fix)"
	oLabel1:Location := Point{ 40, 90 }
	oLabel1:AutoSize := TRUE
	oLabel1:ForeColor := Color.Blue
	oLabel1:BackColor := Color.Yellow

	LOCAL oLabel2 AS XSharp.VFP.UI.Label
	oLabel2 := XSharp.VFP.UI.Label{}
	oLabel2:Name := "Label2"
	oLabel2:Text := "Label 2 - should be VISIBLE (was hidden before the fix)"
	oLabel2:Location := Point{ 40, 120 }
	oLabel2:AutoSize := TRUE
	oLabel2:ForeColor := Color.Blue
	oLabel2:BackColor := Color.Yellow

	LOCAL oLabel3 AS XSharp.VFP.UI.Label
	oLabel3 := XSharp.VFP.UI.Label{}
	oLabel3:Name := "Label3"
	oLabel3:Text := "Label 3 - should be VISIBLE (was hidden before the fix)"
	oLabel3:Location := Point{ 40, 150 }
	oLabel3:AutoSize := TRUE
	oLabel3:ForeColor := Color.Blue
	oLabel3:BackColor := Color.Yellow

	// A real child of the container -- always visible/clickable regardless of the
	// z-order fix, included as a baseline sanity check.
	LOCAL oCmdB2 AS XSharp.VFP.UI.CommandButton
	oCmdB2 := XSharp.VFP.UI.CommandButton{}
	oCmdB2:Name := "cmdB2"
	oCmdB2:Text := "cmdB2 (real child of c1) - click me"
	oCmdB2:Location := Point{ 40, 180 }
	oCmdB2:Size := Size{ 260, 30 }
	oContainer:Controls:Add( oCmdB2 )

	oForm:Controls:Add( oContainer )
	oForm:Controls:Add( oCmdB1 )
	oForm:Controls:Add( oLabel1 )
	oForm:Controls:Add( oLabel2 )
	oForm:Controls:Add( oLabel3 )

	oForm:Show()
	RETURN
