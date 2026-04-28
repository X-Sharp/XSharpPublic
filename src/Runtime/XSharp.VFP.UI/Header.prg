// Header.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.

USING System
USING System.Collections.Generic
USING System.Text
USING System.ComponentModel

BEGIN NAMESPACE XSharp.VFP.UI

    /// <summary>
    /// VFP Header Control - Grid column header cell
    /// Inherits from DataGridViewColumnHeaderCell; adds copy-constructor and Clone()
    ///
    /// Base Class: System.Windows.Forms.DataGridViewColumnHeaderCell
    /// </summary>
    CLASS Header INHERIT System.Windows.Forms.DataGridViewColumnHeaderCell

        CONSTRUCTOR()
            SUPER()
            RETURN

        CONSTRUCTOR(source AS System.Windows.Forms.DataGridViewColumnHeaderCell)
            SUPER()
            //
            SELF:ErrorText := source:ErrorText
            SELF:Tag := source:Tag
            SELF:ToolTipText := source:ToolTipText
            SELF:Value := source:Value
            SELF:ContextMenuStrip := source:ContextMenuStrip
            SELF:ValueType := source:ValueType
            IF source:HasStyle
                SELF:Style := source:Style
            ENDIF
            RETURN

        /// <summary>
        /// Text is the Value of the HeaderCell
        /// </summary>
        PROPERTY Text AS STRING
            GET
                IF SELF:Value != NULL
                    RETURN SELF:Value:ToString()
                ELSE
                    RETURN NULL
                ENDIF
            END GET
            SET
                SUPER:Value := (OBJECT)VALUE
            END SET
        END PROPERTY

        PROPERTY Name AS STRING AUTO

        PROPERTY FontSize AS LONG AUTO

        PROPERTY TextAlign AS System.Drawing.ContentAlignment AUTO

        OVERRIDE METHOD Clone() AS OBJECT
            LOCAL source AS Header
            //
            source := Header{}
            SELF:ErrorText := source:ErrorText
            SELF:Tag := source:Tag
            SELF:ToolTipText := source:ToolTipText
            SELF:Value := source:Value
            SELF:ContextMenuStrip := source:ContextMenuStrip
            SELF:ValueType := source:ValueType
            // Avoid creating a new style object if the Style property has not previously been set.
            IF source:HasStyle
                SELF:Style := source:Style
            ENDIF
            source:Name := SELF:Name
            //
            RETURN source

    END CLASS

END NAMESPACE // XSharp.VFP.UI
