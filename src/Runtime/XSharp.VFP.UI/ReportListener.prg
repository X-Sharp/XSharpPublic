// ReportListener.prg
// Created by    : fabri
// Creation Date : 11/19/2026 11:48:47 AM
// Created for   :
// WorkStation   : FABXPS16


USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text

BEGIN NAMESPACE XSharp.VFP.UI

/// <summary>
/// The ReportListener class.
/// </summary>
PARTIAL CLASS ReportListener

    // PlaceHolders for some WinForms properties to ease Export
    PROPERTY Name AS STRING AUTO
    PROPERTY Size AS System.Drawing.Size AUTO

CONSTRUCTOR()
    RETURN

END CLASS
END NAMESPACE // XSharp.VFP.UI
