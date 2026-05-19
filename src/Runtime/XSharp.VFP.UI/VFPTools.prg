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
/// Static utility class with VFP-to-WinForms conversion helpers.<br/>
/// Provides image loading (<see cref="ImageFromFile"/>), VFP stretch/alignment code mapping
/// (<see cref="ImageStrechConvert"/>, <see cref="TextAlignmentConvert"/>), and bidirectional
/// VFP COLORREF conversion (<see cref="ColorFromVFP(System.Int64)"/>, <see cref="ColorToVFP"/>).
/// </summary>
STATIC PUBLIC CLASS VFPTools

    /// <summary>
    /// Loads an image from <paramref name="filename"/> and returns it as a <see cref="System.Drawing.Image"/>.<br/>
    /// Returns <c>NULL</c> if the file does not exist. In Designer mode, also returns <c>NULL</c> to prevent crashes.
    /// </summary>
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

    /// <summary>
    /// Converts a VFP Stretch code to <see cref="System.Windows.Forms.PictureBoxSizeMode"/>: 0=Normal, 1=AutoSize, 2=StretchImage.
    /// </summary>
    STATIC METHOD ImageStrechConvert( n AS INT ) AS System.Windows.Forms.PictureBoxSizeMode
        VAR convert := System.Windows.Forms.PictureBoxSizeMode.Normal
        SWITCH n
            CASE 1
                convert := System.Windows.Forms.PictureBoxSizeMode.AutoSize
            CASE 2
                convert := System.Windows.Forms.PictureBoxSizeMode.StretchImage
        END SWITCH
        RETURN convert


    /// <summary>
    /// Converts a VFP alignment code to <see cref="System.Windows.Forms.HorizontalAlignment"/>: 0=Left, 1=Right, 2=Center, 3=Left (same as 0).
    /// </summary>
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



    /// <summary>
    /// Converts a VFP COLORREF long (<c>R + G×256 + B×65536</c>, low byte = red) to a <see cref="System.Drawing.Color"/>. Returns <c>Color.Empty</c> for 0.
    /// </summary>
    STATIC METHOD ColorFromVFP(nColor AS LONG) AS System.Drawing.Color
        IF nColor == 0
            RETURN System.Drawing.Color.Empty
        ENDIF
        VAR r := (INT)(nColor & 0xFF)
        VAR g := (INT)((nColor >> 8) & 0xFF)
        VAR b := (INT)((nColor >> 16) & 0xFF)
        RETURN System.Drawing.Color.FromArgb(r, g, b)

    /// <summary>
    /// Creates a <see cref="System.Drawing.Color"/> directly from separate R, G, B component values.
    /// </summary>
    STATIC METHOD ColorFromVFP(r AS LONG, g AS LONG, b AS LONG) AS System.Drawing.Color
        RETURN System.Drawing.Color.FromArgb(r, g, b)

    /// <summary>
    /// Converts a <see cref="System.Drawing.Color"/> back to a VFP COLORREF long (<c>R + G×256 + B×65536</c>). Returns 0 for <c>Color.Empty</c>.
    /// </summary>
    STATIC METHOD ColorToVFP(c AS System.Drawing.Color) AS LONG
        IF c == System.Drawing.Color.Empty
            RETURN 0
        ENDIF
        RETURN (LONG)c:R + (LONG)c:G * 256 + (LONG)c:B * 65536

END CLASS
END NAMESPACE // XSharp.VFP.UI
