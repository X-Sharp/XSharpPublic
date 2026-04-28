// OptionGroup.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.

USING System
USING System.Collections.Generic
USING System.ComponentModel
USING System.Data
USING System.Drawing
USING System.Text
USING System.Windows.Forms

BEGIN NAMESPACE XSharp.VFP.UI

    /// <summary>
    /// VFP OptionGroup Control - Group of OptionButton (radio) controls
    /// Inherits from UserControl; manages a list of OptionButton objects
    ///
    /// Includes: ControlSource.xh (data binding)
    ///
    /// Base Class: System.Windows.Forms.UserControl
    /// </summary>
    PUBLIC PARTIAL CLASS OptionGroup INHERIT System.Windows.Forms.UserControl

        PRIVATE buttons AS List<OptionButton>

        PUBLIC PROPERTY ButtonCount AS INT
            GET
                RETURN SELF:buttons:Count
            END GET
            SET
                IF VALUE < SELF:buttons:Count
                    SELF:buttons:RemoveRange(VALUE, SELF:buttons:Count - VALUE)
                ELSEIF VALUE > SELF:buttons:Count
                    FOR VAR i := 1 TO VALUE - SELF:buttons:Count
                        LOCAL rb AS OptionButton
                        //
                        rb := OptionButton{}
                        rb:AutoSize := TRUE
                        rb:Location := System.Drawing.Point{6, 11 + (i - 1) * (21 + 6)}
                        rb:Name := "Option" + i:ToString()
                        rb:Size := System.Drawing.Size{79, 21}
                        rb:TabIndex := 0
                        rb:TabStop := TRUE
                        rb:Text := "Option" + i:ToString()
                        rb:UseVisualStyleBackColor := TRUE
                        SELF:buttons:Add(rb)
                    NEXT
                ENDIF
                //
                SELF:Size := System.Drawing.Size{91, (SELF:buttons:Count) * (21 + 6) + 2}
            END SET
        END PROPERTY

        CONSTRUCTOR()
            SELF:InitializeComponent()
            //
            SELF:buttons := List<OptionButton>{}
            SELF:Size := Size{10, 15}
            RETURN

        PUBLIC METHOD Button(i AS INT) AS OptionButton
            RETURN SELF:buttons[i]

        #include "Headers/ControlSource.xh"

    END CLASS

END NAMESPACE // XSharp.VFP.UI
