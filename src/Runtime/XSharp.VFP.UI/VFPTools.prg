// VFPTools.prg
// Created by    : fabri
// Creation Date : 7/6/2024 6:12:12 PM
// Created for   :
// WorkStation   : FABXPS


USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE XSharp.VFP.UI

/// <summary>
/// The VFPTools class.
/// </summary>
STATIC PUBLIC CLASS VFPTools

    /// <summary>
    ///Read an Image file, and create a System.Drawing.Image object.
    /// </summary>
    /// <returns></returns>
    STATIC METHOD ImageFromFile( filename AS STRING ) AS System.Drawing.Image
        // In Visual Studio ? So...In the Designer
        IF !System.IO.File.Exists( filename )
            IF VPFIsInDesignMode() .AND. !String.IsNullOrEmpty(filename)
                // To avoid a crash in Designer
                RETURN null // XSharp.VFP.UI.Properties.Resources.XSharp
            ELSE
                RETURN NULL
            ENDIF
        ENDIF
        // Do it
        var image := System.Drawing.Image.FromFile( filename )
        //
        RETURN image

    STATIC METHOD ImageStrechConvert( n AS INT ) AS System.Windows.Forms.PictureBoxSizeMode
        VAR convert := System.Windows.Forms.PictureBoxSizeMode.Normal
        SWITCH n
            CASE 1
                convert := System.Windows.Forms.PictureBoxSizeMode.AutoSize
            CASE 2
                convert := System.Windows.Forms.PictureBoxSizeMode.StretchImage
        END SWITCH
        RETURN convert


    STATIC METHOD TextAlignmentConvert( n AS INT ) AS System.Windows.Forms.HorizontalAlignment
        VAR convert := System.Windows.Forms.HorizontalAlignment.Left
        SWITCH n
            CASE 0
                convert := System.Windows.Forms.HorizontalAlignment.Left
            CASE 1
                convert := System.Windows.Forms.HorizontalAlignment.Right
            CASE 2
                convert := System.Windows.Forms.HorizontalAlignment.Center
            CASE 3
                convert := System.Windows.Forms.HorizontalAlignment.Left
        END SWITCH
        RETURN convert



    // Convert a VFP COLORREF long (0x00BBGGRR) to a System.Drawing.Color.
    // VFP stores color as R + G*256 + B*65536 (low byte = red).
    STATIC METHOD ColorFromVFP(nColor AS LONG) AS System.Drawing.Color
        IF nColor == 0
            RETURN System.Drawing.Color.Empty
        ENDIF
        VAR r := (INT)(nColor & 0xFF)
        VAR g := (INT)((nColor >> 8) & 0xFF)
        VAR b := (INT)((nColor >> 16) & 0xFF)
        RETURN System.Drawing.Color.FromArgb(r, g, b)

    STATIC METHOD ColorFromVFP(r AS LONG, g AS LONG, b AS LONG) AS System.Drawing.Color
        RETURN System.Drawing.Color.FromArgb(r, g, b)

    // Convert a System.Drawing.Color back to a VFP COLORREF long (R + G*256 + B*65536).
    STATIC METHOD ColorToVFP(c AS System.Drawing.Color) AS LONG
        IF c == System.Drawing.Color.Empty
            RETURN 0
        ENDIF
        RETURN (LONG)c:R + (LONG)c:G * 256 + (LONG)c:B * 65536

END CLASS
END NAMESPACE // XSharp.VFP.UI
