// TransparentZOrder.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
// Real VFP mostly ignores creation z-order and instead paints by control class:
// shapes first (furthest back), then containers, then labels on top — but this is a
// PAINT-order override only. Hit-testing/click-routing in real VFP still follows the
// object's actual place in front/back order: a transparent Container that sits in
// front of e.g. a CommandButton still swallows clicks aimed at that button, even
// though the button visually shows through the transparent container (confirmed
// against a real VFP9 test — see project_vfp_ui_transparency_plan memory).
//
// So this fix only elevates Label siblings in front of a BackStyle=Transparent
// Container/Shape (the classic "About dialog" grouping-box case: a container box
// drawn around labels that are its form-level siblings, not its children). Every
// other sibling type (interactive controls especially) keeps its original z-order
// relative to the Container/Shape untouched — reordering those too would make ports
// more click-permissive than real VFP, not just more visually correct.
//
// This is a one-time reorder applied at form Load, not a live compositor: it only
// fixes the common static-layout "grouping box" case. Controls added later at
// runtime (AddObject) are not re-sorted. See XSharp CLAUDE.md-adjacent design notes
// for the fuller transparency plan (paint compositing / reparenting) this is a first
// slice of.

USING System
USING System.Collections.Generic
USING System.Windows.Forms

/// <summary>
/// Recursively elevates <see cref="XSharp.VFP.UI.Label"/> siblings in front of any
/// BackStyle=Transparent <see cref="XSharp.VFP.UI.Container"/>/<see cref="XSharp.VFP.UI.Shape"/>
/// under <paramref name="oParent"/>, matching VFP's real "shapes/containers paint
/// first, labels paint on top" rule. Every other sibling's z-order relative to the
/// Container/Shape is left untouched, so click-routing keeps matching real VFP.
/// Call once, after all of a form's controls exist (Form.OnLoad).
/// </summary>
FUNCTION __VFPFixTransparentZOrder( oParent AS System.Windows.Forms.Control ) AS VOID
    IF oParent == NULL_OBJECT .OR. oParent:Controls:Count == 0
        RETURN
    ENDIF

    LOCAL children AS List<System.Windows.Forms.Control>
    children := List<System.Windows.Forms.Control>{ oParent:Controls:Count }
    FOREACH VAR oChild IN oParent:Controls
        children:Add( (System.Windows.Forms.Control) oChild )
    NEXT

    LOCAL labels AS List<System.Windows.Forms.Control>
    labels := List<System.Windows.Forms.Control>{}
    FOREACH VAR oChild IN children
        IF oChild IS XSharp.VFP.UI.Label
            labels:Add( oChild )
        ENDIF
    NEXT

    // Pass 1: Containers, then pass 2: Shapes — processed second so Shapes end up
    // furthest back of the two, behind the containers already fixed in pass 1.
    FOREACH VAR oChild IN children
        IF oChild IS XSharp.VFP.UI.Container VAR oContainer .AND. oContainer:BackStyle == 0
            __VFPElevateLabelsInFrontOf( oParent, oContainer, labels )
        ENDIF
    NEXT
    FOREACH VAR oChild IN children
        IF oChild IS XSharp.VFP.UI.Shape VAR oShape .AND. oShape:BackStyle == 0
            __VFPElevateLabelsInFrontOf( oParent, oShape, labels )
        ENDIF
    NEXT

    // Recurse into every child that can itself host overlapping siblings.
    FOREACH VAR oChild IN children
        __VFPFixTransparentZOrder( oChild )
    NEXT

// Moves every label in `labels` that is currently behind `oTarget` to sit
// immediately in front of it, preserving the labels' own relative order.
// Verified empirically: repeatedly calling SetChildIndex(label, target's current
// index) in original label order is a stable operation — it never disturbs any
// other sibling's position relative to oTarget, only shifts oTarget back by the
// number of labels inserted in front of it.
FUNCTION __VFPElevateLabelsInFrontOf( oParent AS System.Windows.Forms.Control, oTarget AS System.Windows.Forms.Control, labels AS List<System.Windows.Forms.Control> ) AS VOID
    FOREACH VAR oLabel IN labels
        VAR targetIndex := oParent:Controls:GetChildIndex( oTarget )
        VAR labelIndex  := oParent:Controls:GetChildIndex( oLabel )
        IF labelIndex > targetIndex
            oParent:Controls:SetChildIndex( oLabel, targetIndex )
        ENDIF
    NEXT
