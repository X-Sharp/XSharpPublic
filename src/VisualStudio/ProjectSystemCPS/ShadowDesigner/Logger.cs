//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using XSharp.Settings;

namespace XSharp.ProjectSystem.ShadowDesigner
{
    /// <summary>
    /// Writes to the X# log (XSettings.Logger), like XSharp.Project.Logger in the X# project package.
    /// </summary>
    internal static class Logger
    {
        internal static void Exception(Exception e, string msg) => XSettings.Logger.Exception(e, msg);
        internal static void Information(string msg) => XSettings.Logger.Information(msg);
        internal static void Error(string msg) => XSettings.Logger.Error(msg);
    }
}
