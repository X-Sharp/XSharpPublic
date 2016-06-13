//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System;
using System.ComponentModel;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio.Project;


namespace XSharp.Project
{
    [ComVisible(true), CLSCompliant(false), System.Runtime.InteropServices.ClassInterface(ClassInterfaceType.AutoDual)]
    [Guid("C252A673-C956-4307-9A18-14CEDC0C5194")]
    public class XSharpProjectNodeProperties : ProjectNodeProperties
    {
        #region ctors
        public XSharpProjectNodeProperties(ProjectNode node)
           : base(node)
        {
        }
        #endregion

        #region properties
        [Browsable(false)]
        public string OutputFileName
        {
            get
            {
                return ((XSharpProjectNode)(this.Node.ProjectMgr)).OutputFile;
            }
        }

        [Browsable(false)]
        public string AssemblyName
        {
            get
            {
                return this.Node.ProjectMgr.GetProjectProperty(ProjectFileConstants.AssemblyName);
            }
            set
            {
                this.Node.ProjectMgr.SetProjectProperty(ProjectFileConstants.AssemblyName, value);
            }
        }

        [Browsable(false)]
        public string DefaultNamespace
        {
            get
            {
                return this.Node.ProjectMgr.GetProjectProperty(ProjectFileConstants.RootNamespace);
            }
            set
            {
                this.Node.ProjectMgr.SetProjectProperty(ProjectFileConstants.RootNamespace, value);
            }
        }

        [Browsable(false)]
        public string RootNamespace
        {
            get
            {
                return this.Node.ProjectMgr.GetProjectProperty(ProjectFileConstants.RootNamespace);
            }
            set
            {
                this.Node.ProjectMgr.SetProjectProperty(ProjectFileConstants.RootNamespace, value);
            }
        }

        [Browsable(false)]
        public string OutputType
        {
            get
            {
                return this.Node.ProjectMgr.GetProjectProperty(ProjectFileConstants.OutputType);
            }
            set
            {
                this.Node.ProjectMgr.SetProjectProperty(ProjectFileConstants.OutputType, value);
            }
        }

        [Browsable(false)]
        public string TargetFrameworkMoniker
        {
            get
            {
                return this.Node.ProjectMgr.TargetFrameworkMoniker.FullName;
            }

        }

        #endregion
    }
}
