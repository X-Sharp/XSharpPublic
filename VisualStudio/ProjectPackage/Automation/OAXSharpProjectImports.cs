//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Collections;
using System.Collections.Generic;
using EnvDTE;
using Microsoft.VisualStudio.Shell;

namespace XSharp.Project
{


    public class OAVSProjectImports : VSLangProj.Imports
    {
        readonly EnvDTE.Project project;
        readonly List<string> imports;

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
            if (index is int iIndex)
            {
                if (iIndex > 0 && iIndex <= imports.Count)
                    imports.Remove(imports[iIndex - 1]);
            }
            else if (index is string sIndex)
            {
                if (imports.Contains(sIndex))
                {
                    imports.Remove(sIndex);
                }
            }
        }
    }
}
