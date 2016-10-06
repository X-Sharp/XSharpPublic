//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Diagnostics.CodeAnalysis;
using System.IO;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Project;

namespace XSharp.Project
{

    /// <summary>
    /// Represents a Folder node in a XSharp project.
    /// </summary>
    public class XSharpFolderNode : XFolderNode
    {
        // =========================================================================================
        // Constructors
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XSharpFolderNode"/> class.
        /// </summary>
        /// <param name="root">The root <see cref="XSharpProjectNode"/> that contains this node.</param>
        /// <param name="directoryPath">Root of the hierarchy.</param>
        /// <param name="element">The element that contains MSBuild properties.</param>
        public XSharpFolderNode(XSharpProjectNode root, string directoryPath, ProjectElement element)
           : this(root, directoryPath, element, false)
        {
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="XSharpFolderNode"/> class.
        /// </summary>
        /// <param name="root">The root <see cref="XSharpProjectNode"/> that contains this node.</param>
        /// <param name="directoryPath">Root of the hierarchy</param>
        /// <param name="element">The element that contains MSBuild properties.</param>
        /// <param name="isNonMemberItem">Indicates if this node is not a member of the project.</param>
        [SuppressMessage("Microsoft.Naming", "CA1702:CompoundWordsShouldBeCasedCorrectly", MessageId = "NonMember")]
        public XSharpFolderNode(XSharpProjectNode root, string directoryPath, ProjectElement element, bool isNonMemberItem)
           : base(root, directoryPath, element, isNonMemberItem)
        {
            root.AddURL(this.Url, this);
        }


        protected internal override void DeleteFromStorage(string path)
        {
            if (File.Exists(path))
            {
                File.SetAttributes(path, FileAttributes.Normal); // make sure it's not readonly.
                OurNativeMethods.ShellDelete(path, OurNativeMethods.RecycleOption.SendToRecycleBin,
                   OurNativeMethods.UICancelOption.DoNothing, OurNativeMethods.FileOrDirectory.Directory);

            }
        }

        public override void OnItemDeleted()
        {
            if (this.ProjectMgr is XSharpProjectNode)
            {
                var projectNode = this.ProjectMgr as XSharpProjectNode;
                if (projectNode != null)
                    projectNode.RemoveURL(this);

                base.OnItemDeleted();
            }
        }


        public override int SetEditLabel(string label)
        {
            int iResult;
            String sOldUrl = this.Url;
            iResult = base.SetEditLabel(label);
            if (iResult == VSConstants.S_OK && String.Compare(this.Url, sOldUrl, true) != 0)
            {
                XSharpProjectNode project = this.ProjectMgr as XSharpProjectNode;
                if (project != null)
                {
                    project.RemoveURL(sOldUrl);
                    project.AddURL(this.Url, this);
                }
            }
            return iResult;
        }

        protected override void Dispose(bool disposing)
        {
            if (this.ProjectMgr is XSharpProjectNode)
            {
                XSharpProjectNode projectNode = (XSharpProjectNode)this.ProjectMgr;
                if (projectNode != null)
                    projectNode.RemoveURL(this);
            }
            base.Dispose(disposing);
        }

    }
}