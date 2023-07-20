//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using System;
using XSharpModel;

namespace XSharp.LanguageService
{
    public class LanguageServiceEvents
    {
        public static void Start()
        {
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                VS.Events.SolutionEvents.OnAfterCloseSolution += SolutionEvents_OnAfterCloseSolution;
                VS.Events.DocumentEvents.Closed += DocumentEvents_Closed;
            });

        }

        private static void SolutionEvents_OnAfterCloseSolution()
        {
            XSharpXMLDocTools.Close();
        }

        private static void DocumentEvents_Closed(string document)
        {
            // Remove document from OrphanedFilesProject
            // So it can be opened in normal project afterwards
            // when possible
            if (!IsXSharpProject(document))
            {
                Logger.Information("Languageservice.DocumentEvents_Closed " + document ?? "(none)");
                var xfile = XSolution.FindFile(document);
                if (xfile != null && xfile.Project.Name == OrphanedFilesProject.OrphanName)
                {
                    XSolution.OrphanedFilesProject.RemoveFile(document);
                }
            }
        }
        static bool IsXSharpProject(string fileName)
        {
            if (string.IsNullOrEmpty(fileName))
                return false;
            return string.Equals(System.IO.Path.GetExtension(fileName), ".xsproj", StringComparison.OrdinalIgnoreCase);
        }

    }
}


