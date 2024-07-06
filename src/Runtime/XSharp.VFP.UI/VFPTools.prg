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
END CLASS
END NAMESPACE // XSharp.VFP.UI
