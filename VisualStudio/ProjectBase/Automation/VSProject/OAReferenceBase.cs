/* ****************************************************************************
 *
 * Copyright (c) Microsoft Corporation.
 *
 * This source code is subject to terms and conditions of the Apache License, Version 2.0. A
 * copy of the license can be found in the License.txt file at the root of this distribution. 
 * 
 * You must not remove this notice, or any other, from this software.
 *
 * ***************************************************************************/

using Microsoft.VisualStudio.Shell;
using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using System.Runtime.InteropServices;
using VSLangProj;
using VSLangProj110;
using VSLangProj150;
using VSLangProj2;
using VSLangProj80;


namespace Microsoft.VisualStudio.Project.Automation
{
    /// <summary>
    /// Represents the automation equivalent of ReferenceNode
    /// </summary>
    /// <typeparam name="RefType"></typeparam>
    [SuppressMessage("Microsoft.Naming", "CA1715:IdentifiersShouldHaveCorrectPrefix", MessageId = "T")]
    [ComVisible(true), CLSCompliant(false)]
    public abstract class OAReferenceBase<RT> : Reference3, IDisposable, Reference4, Reference2, Reference, Reference6, Reference5
        where RT : ReferenceNode
    {
        #region fields
        private RT referenceNode;
        #endregion

        #region ctors
        internal OAReferenceBase(RT referenceNode)
        {
            this.referenceNode = referenceNode;
        }
        #endregion

        #region properties
        public RT BaseReferenceNode
        {
            get { return referenceNode; }
        }
        #endregion

        #region Reference Members
        public virtual int BuildNumber
        {
            get { return 0; }
        }

        public virtual References Collection
        {
            get
            {
                return BaseReferenceNode.Parent.Object as References;
            }
        }

        public virtual EnvDTE.Project ContainingProject
        {
            get
            {
                ThreadHelper.ThrowIfNotOnUIThread();

                return BaseReferenceNode.ProjectMgr.GetAutomationObject() as EnvDTE.Project;
            }
        }

        public virtual bool CopyLocal
        {
            get
            {
                return false; // throw new NotImplementedException();
            }
            set
            {
                throw new NotImplementedException();
            }
        }

        public virtual string Culture
        {
            get { throw new NotImplementedException(); }
        }

        public virtual EnvDTE.DTE DTE
        {
            get
            {
                ThreadHelper.ThrowIfNotOnUIThread();

                return BaseReferenceNode.ProjectMgr.Site.GetService(typeof(EnvDTE.DTE)) as EnvDTE.DTE;
            }
        }

        public virtual string Description
        {
            get
            {
                return this.Name;
            }
        }

        public virtual string ExtenderCATID
        {
            get { throw new NotImplementedException(); }
        }

        public virtual object ExtenderNames
        {
            get { throw new NotImplementedException(); }
        }

        public virtual string Identity
        {
            get { throw new NotImplementedException(); }
        }

        public virtual int MajorVersion
        {
            get { return 0; }
        }

        public virtual int MinorVersion
        {
            get { return 0; }
        }

        public virtual string Name
        {
            get { throw new NotImplementedException(); }
        }

        public virtual string Path
        {
            get
            {
                return BaseReferenceNode.Url;
            }
        }

        public virtual string PublicKeyToken
        {
            get { throw new NotImplementedException(); }
        }

        public virtual void Remove()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            BaseReferenceNode.Remove(false);
        }

        public virtual int RevisionNumber
        {
            get { return 0; }
        }

        public virtual EnvDTE.Project SourceProject
        {
            get { return null; }
        }

        public virtual bool StrongName
        {
            get { return false; }
        }

        public virtual prjReferenceType Type
        {
            get { throw new NotImplementedException(); }
        }

        public virtual string Version
        {
            get { return new Version().ToString(); }
        }

        public virtual object get_Extender(string ExtenderName)
        {
            throw new NotImplementedException();
        }

        public void Dispose()
        {
            this.referenceNode = null;
        }
        #endregion

        public virtual string Aliases { get; set; }

        public virtual bool AutoReferenced {
            get { return false; }
        }

        public virtual bool Isolated { get; set; }

        public virtual uint RefType {
            get {
                // Default to native reference to help prevent callers from
                // making incorrect assumptions
                return (uint)__PROJECTREFERENCETYPE.PROJREFTYPE_NATIVE;
            }
        }

        public bool Resolved  =>referenceNode.Resolved;

        public virtual string RuntimeVersion => "";

        public virtual bool SpecificVersion
        {
            get
            {
                return false;
            }
            set
            {
                throw new NotImplementedException();
            }
        }

        public string SubType
        {
            get
            {
                throw new NotImplementedException();
            }
            set
            {
                throw new NotImplementedException();
            }
        }

        public bool EmbedInteropTypes { get; set; }

        public void GetMetadata(Array parrbstrDesiredMetadata, out Array pparrbstrMetadataElements, out Array pparrbstrMetadataValues)
        {
            Dictionary<string, string> outData = new Dictionary<string, string>();
            foreach (string meta in parrbstrDesiredMetadata)
            {
                outData.Add(meta, referenceNode.ItemNode.GetEvaluatedMetadata(meta));
            }
            pparrbstrMetadataElements = outData.Keys.ToArray();
            pparrbstrMetadataValues = outData.Values.ToArray();

        }

        public void AddOrUpdateMetadata(Array parrbstrMetadataElements, Array parrbstrMetadataValues)
        {
            Microsoft.VisualStudio.Shell.ThreadHelper.ThrowIfNotOnUIThread();
            for (int i = 0; i < parrbstrMetadataElements.Length; i++)
            {
                referenceNode.ItemNode.SetMetadata(parrbstrMetadataElements.GetValue(i) as string, parrbstrMetadataValues.GetValue(i) as string);
            }

        }

        public Array ExpandedSdkReferences =>  new object[0];

        public Reference Group => null;
    }
}
