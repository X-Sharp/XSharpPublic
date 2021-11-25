//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.Runtime.InteropServices;
using Microsoft.VisualStudio.Project.Automation;
using System;
using System.Collections;
using System.Collections.Generic;
using EnvDTE;
using Microsoft.VisualStudio.Project;
using VSLangProj;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
#if PACKAGEREFERENCE
using VSLangProj150;
using VSLangProj140;
using VSLangProj80;
#endif
namespace XSharp.Project
{
   

    public class OAVSProjectImports : VSLangProj.Imports
    {
        EnvDTE.Project project;
        List<string> imports;

        internal OAVSProjectImports(EnvDTE.Project prj)
        {
            project = prj;
            imports = new List<string>();
        }

        public EnvDTE.Project ContainingProject
        {
            get
            {
                return project;
            }
        }

        public int Count
        {
            get
            {
                return imports.Count;
            }
        }

        public DTE DTE
        {
            get
            {
                return ThreadHelper.JoinableTaskFactory.Run(async delegate
                    {
                        await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();

                        return project.DTE;
                    });
            }
        }

        public object Parent
        {
            get
            {
                return project;
            }
        }

        public void Add(string bstrImport)
        {
            imports.Add(bstrImport);
        }

        public IEnumerator GetEnumerator()
        {
            return imports.GetEnumerator();
        }

        public string Item(int lIndex)
        {
            if (lIndex >= 0 && lIndex < imports.Count)
                return imports[lIndex];
            return null;
        }

        public void Remove(object index)
        {
            if (index is Int32)
            {
                int iIndex = (Int32)index;
                if (iIndex > 0 && iIndex <= imports.Count)
                    imports.Remove(imports[iIndex - 1]);
            }
            else if (index is String)
            {
                string sIndex = index as String;
                if (imports.Contains(sIndex))
                {
                    imports.Remove(sIndex);
                }
            }
        }
    }
}
