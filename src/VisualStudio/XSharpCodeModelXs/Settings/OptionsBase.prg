// OptionsBase.prg
// Created by    : robert
// Creation Date : 6/8/2023 1:11:00 PM
// Created for   :
// WorkStation   : NYX


USING System
USING System.Collections.Generic
USING System.Text
USING System.IO

BEGIN NAMESPACE XSharp.Settings

	/// <summary>
    /// The OptionsBase class.
    /// </summary>
    ABSTRACT CLASS OptionsBase
        ABSTRACT METHOD WriteToSettings AS VOID
        /// <summary>
        /// Create the path in the Roaming Appdata folder
        /// </summary>
        PUBLIC STATIC METHOD CreatePath() as VOID
            var sPath := GetPath()
            IF ! Directory.Exists(sPath)
                Directory.CreateDirectory(sPath)
            ENDIF
        /// <summary>
        /// Return the path to the Roaming Appdata folder
        /// </summary>
        PUBLIC STATIC METHOD GetPath() as STRING
          return Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "XSharp")

	END CLASS
END NAMESPACE // XSharpModel.Settings
