//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.IO;
using XSharpModel;

namespace XSharp.LanguageService.ForeignSource
{
    /// <summary>Finds the C# or VB project of the solution whose output DLL an assembly was read from.</summary>
    internal static class ForeignProjects
    {
        /// <summary>The project file, or null when no referenced C#/VB project of an X# project produces this DLL.</summary>
        internal static string FindProjectFile(string assemblyFile)
        {
            if (string.IsNullOrEmpty(assemblyFile))
                return null;
            foreach (var project in XSolution.Projects)
            {
                var found = FindProjectFile(project.ProjectOutputs, assemblyFile);
                if (found != null)
                    return found;
            }
            return null;
        }

        internal static string FindProjectFile(IEnumerable<KeyValuePair<string, string>> outputs, string assemblyFile)
        {
            var wanted = Normalize(assemblyFile);
            if (wanted == null)
                return null;
            foreach (var output in outputs)
            {
                if (XProject.IsXSharpProject(output.Key))
                    continue;
                if (string.Equals(Normalize(output.Value), wanted, StringComparison.OrdinalIgnoreCase))
                    return output.Key;
            }
            return null;
        }

        private static string Normalize(string path)
        {
            if (string.IsNullOrEmpty(path))
                return null;
            try
            {
                return Path.GetFullPath(path);
            }
            catch (Exception)
            {
                return null;
            }
        }
    }
}
