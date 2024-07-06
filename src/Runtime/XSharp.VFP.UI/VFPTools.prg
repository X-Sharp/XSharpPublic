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
            IF VPFIsInDesignMode()
                // To avoid a crash in Designer
                RETURN XSharp.VFP.UI.Properties.Resources.XSharp
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



END CLASS
END NAMESPACE // XSharp.VFP.UI
