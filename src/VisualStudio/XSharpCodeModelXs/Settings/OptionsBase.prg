//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.


USING System.Text
USING System.IO

NAMESPACE XSharp.Settings

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
