//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using SWF := System.Windows.Forms
using System.Collections.Generic
STATIC CLASS Extensions
    STATIC METHOD GetAllChildren(SELF oParent AS SWF.Control, addNested := FALSE AS LOGIC) AS List<SWF.Control>
        VAR aList := List<SWF.Control>{}
        FOREACH oC AS SWF.Control IN oParent:Controls
            aList:Add(oC)
            if addNested
                var aChildren := oC:GetAllChildren(TRUE)
                if aChildren:Count > 0
                    aList:AddRange(aChildren)
                endif
            endif
        NEXT
        RETURN aList

    STATIC METHOD GetFirstEditableChild(SELF oParent AS SWF.Control) AS SWF.Control
        VAR aChildren := oParent:GetAllChildren(FALSE)
        FOREACH VAR oC IN aChildren
            IF oC:CanSelect .AND. ! oC IS VOButton
                RETURN oC
            ENDIF
            IF oC:Controls:Count > 0 .AND. ! oC IS VOGroupBox
                VAR oC2 := oC:GetFirstEditableChild()
                IF oC2 != NULL_OBJECT
                    RETURN oC2
                ENDIF
            ENDIF
        NEXT
        RETURN NULL_OBJECT
END CLASS
