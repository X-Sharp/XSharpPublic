// Image.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.

USING System
USING System.ComponentModel
USING System.Drawing
USING System.IO
USING System.Windows.Forms

BEGIN NAMESPACE XSharp.VFP.UI

    /// <summary>
    /// VFP Image Control - Image display
    /// Maps VFP Image properties and methods to WinForms PictureBox
    ///
    /// Implements: IVFPObject, IVFPControl, IVFPImage
    /// Includes: VFPImage.xh, VFPObject.xh, ControlProperties.xh
    ///
    /// Base Class: System.Windows.Forms.PictureBox
    /// </summary>
    PARTIAL CLASS Image INHERIT System.Windows.Forms.PictureBox IMPLEMENTS IVFPObject, IVFPControl, IVFPImage

        // ============================================================================
        // Include VFP Image properties (Picture field, etc.)
        // ============================================================================
        #include "Headers/VFPImage.xh"

        // ============================================================================
        // Include VFPObject base implementation (IVFPObject, IVFPHelp)
        // ============================================================================
        #include "Headers/VFPObject.xh"

        // ============================================================================
        // Include common VFP control properties and event wiring
        // ============================================================================
        #include "Headers/ControlProperties.xh"

        // ============================================================================
        // PRIVATE FIELDS
        // ============================================================================

        PRIVATE _stretch AS LOGIC
        PRIVATE _alignment AS ContentAlignment
        PRIVATE _borderStyle AS BorderStyle
        PRIVATE _dragMode AS INT
        PRIVATE _dragIcon AS STRING
        PRIVATE _baseClass AS STRING
        PRIVATE _class AS STRING
        PRIVATE _classLibrary AS STRING
        PRIVATE _comment AS STRING
        PRIVATE _helpContextID AS LONG
        PRIVATE _whatsThisHelpID AS LONG

        // ============================================================================
        // PROPERTIES
        // ============================================================================

        /// <summary>Picture - Image file path</summary>
        PUBLIC PROPERTY Picture AS STRING
            GET
                RETURN SELF:_picture
            END GET
            SET
                SELF:_picture := VALUE
                SELF:LoadPicture()
            END SET
        END PROPERTY

        /// <summary>Stretch - Whether image stretches to fill control</summary>
        PUBLIC PROPERTY Stretch AS LOGIC
            GET
                RETURN SELF:_stretch
            END GET
            SET
                SELF:_stretch := VALUE
                IF VALUE
                    SELF:SizeMode := PictureBoxSizeMode.StretchImage
                ELSE
                    SELF:SizeMode := PictureBoxSizeMode.Normal
                ENDIF
            END SET
        END PROPERTY

        // ============================================================================
        // IVFPControl Implementation
        // ============================================================================

        [Category("VFP Behavior")];
        [Description("Drag icon path")];
        [DefaultValue("")];
        PROPERTY DragIcon AS STRING
            GET
                RETURN SELF:_dragIcon
            END GET
            SET
                SELF:_dragIcon := VALUE
            END SET
        END PROPERTY

        [Category("VFP Behavior")];
        [Description("Drag mode (0=manual, 1=automatic)")];
        [DefaultValue(0)];
        PROPERTY DragMode AS LONG
            GET
                RETURN SELF:_dragMode
            END GET
            SET
                SELF:_dragMode := VALUE
            END SET
        END PROPERTY

        PUBLIC METHOD Drag(nAction) AS USUAL CLIPPER
            RETURN NIL
        END METHOD

        PUBLIC METHOD SetFocus() AS VOID STRICT
            SELF:Focus()
        END METHOD

        // ============================================================================
        // METHODS
        // ============================================================================

        /// <summary>LoadPicture - Load image from _picture path</summary>
        PUBLIC METHOD LoadPicture() AS VOID
            IF !String.IsNullOrEmpty(SELF:_picture) .AND. File.Exists(SELF:_picture)
                TRY
                    SELF:Image := System.Drawing.Image.FromFile(SELF:_picture)
                CATCH ex AS System.Exception
                    // Silently fail - image not available
                    NOP
                END TRY
            ENDIF
        END METHOD

        /// <summary>LoadPicture - Load image from given path</summary>
        PUBLIC METHOD LoadPicture(cFilePath AS STRING) AS LOGIC
            IF File.Exists(cFilePath)
                TRY
                    SELF:Image := System.Drawing.Image.FromFile(cFilePath)
                    SELF:_picture := cFilePath
                    RETURN TRUE
                CATCH
                    RETURN FALSE
                END TRY
            ENDIF
            RETURN FALSE
        END METHOD

        /// <summary>ClearImage - Remove displayed image</summary>
        PUBLIC METHOD ClearImage() AS VOID
            SELF:Image := NULL
            SELF:_picture := ""
        END METHOD

        // ============================================================================
        // CONSTRUCTOR
        // ============================================================================

        CONSTRUCTOR() STRICT
            SUPER()
            SELF:_picture := ""
            SELF:_stretch := FALSE
            SELF:_alignment := ContentAlignment.MiddleCenter
            SELF:_borderStyle := BorderStyle.None
            SELF:_dragMode := 0
            SELF:_dragIcon := ""
            SELF:_baseClass := "Image"
            SELF:_class := "Image"
            SELF:_classLibrary := ""
            SELF:_comment := ""
            SELF:_helpContextID := 0
            SELF:_whatsThisHelpID := 0
            SELF:SizeMode := PictureBoxSizeMode.Normal
            SELF:BorderStyle := BorderStyle.None
            SELF:Size := System.Drawing.Size{100, 17}

    END CLASS

END NAMESPACE
