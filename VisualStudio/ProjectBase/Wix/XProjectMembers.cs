//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
namespace Microsoft.VisualStudio.Project
{

	using System;
	using System.Collections.Generic;
	using System.Diagnostics.CodeAnalysis;
	using System.IO;
	using MSBuild = Microsoft.Build.Evaluation;
	using Microsoft.VisualStudio.Project;
	using Microsoft.VisualStudio.Shell.Interop;
	using System.Runtime.InteropServices;
    using Microsoft.VisualStudio.Shell;
    using System.Runtime.CompilerServices;


    /// <summary>
    /// Contains helper methods for including/excluding items in a VulcanProjectNode.
    /// </summary>
    [Guid("285F8F28-375F-4A73-B81F-0A5525471B86")]
    internal static class XProjectMembers
    {
        /// <summary>
        /// Adds non member items to the hierarchy.
        /// </summary>
        /// <param name="project">The project to modify.</param>
        [SuppressMessage("Microsoft.Naming", "CA1702:CompoundWordsShouldBeCasedCorrectly", MessageId = "NonMember")]
        internal static void AddNonMemberItems(XProjectNode project)
        {
            IList<string> files = new List<string>();
            IList<string> folders = new List<string>();

            // obtain the list of files and folders under the project folder.
            XProjectMembers.GetRelativeFileSystemEntries(project.ProjectFolder, null, files, folders);

            // exclude the items which are the part of the build.
            XProjectMembers.ExcludeProjectBuildItems(project, files, folders);
            ThreadHelper.ThrowIfNotOnUIThread();

            XProjectMembers.AddNonMemberFolderItems(project, folders);
            XProjectMembers.AddNonMemberFileItems(project, files);
        }

        /// <summary>
        /// Removes non member item nodes from hierarchy.
        /// </summary>
        /// <param name="project">The project to modify.</param>
        [SuppressMessage("Microsoft.Naming", "CA1702:CompoundWordsShouldBeCasedCorrectly", MessageId = "NonMember")]
        internal static void RemoveNonMemberItems(XProjectNode project)
        {
            IList<HierarchyNode> nodeList = new List<HierarchyNode>();
            XHelperMethods.FindNodes(nodeList, project, XProjectMembers.IsNodeNonMemberItem, null);
            ThreadHelper.ThrowIfNotOnUIThread();
            for (int index = nodeList.Count - 1; index >= 0; index--)
            {
                HierarchyNode node = nodeList[index];
                HierarchyNode parent = node.Parent;
                project.RemoveURL(node.Url);
                node.OnItemDeleted();
                parent.RemoveChild(node);
            }
        }

        /// <summary>
        /// This is the filter for non member items.
        /// </summary>
        /// <param name="node">Node to be filtered.</param>
        /// <param name="criteria">Filter criteria.</param>
        /// <returns>Returns if the node is a non member item node or not.</returns>
        [SuppressMessage("Microsoft.Usage", "CA1806:DoNotIgnoreMethodResults", MessageId = "System.Boolean.TryParse(System.String,System.Boolean@)")]
        private static bool IsNodeNonMemberItem(HierarchyNode node, object criteria)
        {
            bool isNonMemberItem = false;
            ThreadHelper.ThrowIfNotOnUIThread();

            if (node != null)
            {
                object propObj = node.GetProperty((int)__VSHPROPID.VSHPROPID_IsNonMemberItem);
                if (propObj != null)
                {
                    Boolean.TryParse(propObj.ToString(), out isNonMemberItem);
                }
            }

            return isNonMemberItem;
        }

        /// <summary>
        /// Gets the file system entries of a folder and its all sub-folders with relative path.
        /// </summary>
        /// <param name="baseFolder">Base folder.</param>
        /// <param name="filter">Filter to be used. default is "*"</param>
        /// <param name="fileList">Files list containing the relative file paths.</param>
        /// <param name="folderList">Folders list containing the relative folder paths.</param>
        private static void GetRelativeFileSystemEntries(string baseFolder, string filter, IList<string> fileList, IList<string> folderList)
        {
            if (baseFolder == null)
            {
                throw new ArgumentNullException("baseFolder");
            }

            if (String.IsNullOrEmpty(filter))
            {
                filter = "*";  // include all files and folders
            }

            if (fileList != null)
            {
                string[] fileEntries = Directory.GetFiles(baseFolder, filter, SearchOption.AllDirectories);
                foreach (string file in fileEntries)
                {
                    FileInfo fileInfo = new FileInfo(file);
                    if ((fileInfo.Attributes & FileAttributes.Hidden) == FileAttributes.Hidden)
                    {
                        continue;
                    }

                    string fileRelativePath = XHelperMethods.GetRelativePath(baseFolder, file);
                    if (!String.IsNullOrEmpty(fileRelativePath))
                    {
                        fileList.Add(fileRelativePath);
                    }
                }
            }

            if (folderList != null)
            {
                string[] folderEntries = Directory.GetDirectories(baseFolder, filter, SearchOption.AllDirectories);
                foreach (string folder in folderEntries)
                {
                    DirectoryInfo folderInfo = new DirectoryInfo(folder);
                    if ((folderInfo.Attributes & FileAttributes.Hidden) == FileAttributes.Hidden)
                    {
                        continue;
                    }

                    string folderRelativePath = XHelperMethods.GetRelativePath(baseFolder, folder);
                    if (!String.IsNullOrEmpty(folderRelativePath))
                    {
                        folderList.Add(folderRelativePath);
                    }
                }
            }
        }

        /// <summary>
        /// Excludes the file and folder items from their corresponding maps if they are part of the build.
        /// </summary>
        /// <param name="project">The project to modify.</param>
        /// <param name="fileList">List containing relative files paths.</param>
        /// <param name="folderList">List containing relative folder paths.</param>
        private static void ExcludeProjectBuildItems(XProjectNode project, IList<string> fileList, IList<string> folderList)
        {

            var projectItems = project.BuildProject.Items;

            if (projectItems == null)
            {
                return; // do nothing, just ignore it.
            }
            else if (fileList == null && folderList == null)
            {
                throw new ArgumentNullException("folderList");
            }

            // we need these maps because we need to have both lowercase and actual case path information.
            // we use lower case paths for case-insesitive search of file entries and actual paths for
            // creating hierarchy node. if we don't do that, we will end up with duplicate nodes when the
            // case of path in .vnproj file doesn't match with the actual file path on the disk.
            IDictionary<string, string> folderMap = null;
            IDictionary<string, string> fileMap = null;

            if (folderList != null)
            {
                folderMap = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
                foreach (string folder in folderList)
                {
                    folderMap.Add(folder, folder);
                }
            }

            if (fileList != null)
            {
                fileMap = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
                foreach (string file in fileList)
                {
                    fileMap.Add(file, file);
                }
            }
            foreach (var buildItem in projectItems)
            {
                if (folderMap != null &&
                    folderMap.Count > 0 &&
                    String.Equals(buildItem.ItemType, ProjectFileConstants.Folder, StringComparison.OrdinalIgnoreCase))
                {
                    string relativePath = buildItem.EvaluatedInclude;
                    if (Path.IsPathRooted(relativePath)) // if not the relative path, make it relative
                    {
                        relativePath = XHelperMethods.GetRelativePath(project.ProjectFolder, relativePath);
                    }


                    if (folderMap.ContainsKey(relativePath))
                    {
                        folderList.Remove(folderMap[relativePath]); // remove it from the actual list.
                        folderMap.Remove(relativePath);
                    }
                }
                else if (fileMap != null &&
                    fileMap.Count > 0 &&
                    XSharpFileType.IsProjectItemType(buildItem))
                {
                    string relativePath = buildItem.EvaluatedInclude;
                    if (Path.IsPathRooted(relativePath)) // if not the relative path, make it relative
                    {
                        relativePath = XHelperMethods.GetRelativePath(project.ProjectFolder, relativePath);
                    }

                    if (fileMap.ContainsKey(relativePath))
                    {
                        fileList.Remove(fileMap[relativePath]); // remove it from the actual list.
                        fileMap.Remove(relativePath);
                    }
                }
            }
        }

        /// <summary>
        /// Adds non member folder items to the hierarchy.
        /// </summary>
        /// <param name="project">The project to modify.</param>
        /// <param name="folderList">Folders list containing the folder names.</param>
        [SuppressMessage("Microsoft.Naming", "CA1702:CompoundWordsShouldBeCasedCorrectly", MessageId = "NonMember")]
        private static void AddNonMemberFolderItems(XProjectNode project, IList<string> folderList)
        {
            if (folderList == null)
            {
                throw new ArgumentNullException("folderList");
            }

            foreach (string folderKey in folderList)
            {
                HierarchyNode parentNode = project;
                string[] folders = folderKey.Split(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar);
                XFolderNode topFolderNode = null;
                foreach (string folder in folders)
                {
                    string childNodeId = Path.Combine(parentNode.VirtualNodeName, folder);
                    FileInfo folderInfo = new FileInfo(Path.Combine(project.ProjectFolder, childNodeId));
                    if ((folderInfo.Attributes & FileAttributes.Hidden) == FileAttributes.Hidden)
                    {
                        break;
                    }

                    HierarchyNode childNode = parentNode.FindChild( childNodeId );
                    if (childNode == null)
                    {
                        if (topFolderNode == null)
                        {
                            topFolderNode = parentNode as XFolderNode;
                            if (topFolderNode != null && (!topFolderNode.IsNonMemberItem) && topFolderNode.IsExpanded)
                            {
                                topFolderNode = null;
                            }
                        }

                        ProjectElement element = new ProjectElement(project, null, true);
                        childNode = project.CreateFolderNode(childNodeId, element);
                        parentNode.AddChild(childNode);
                    }

                    parentNode = childNode;
                }
                ThreadHelper.ThrowIfNotOnUIThread();

                if (topFolderNode != null)
                {
                    topFolderNode.CollapseFolder();
                }
            }
        }

        /// <summary>
        /// Adds non member file items to the hierarchy.
        /// </summary>
        /// <param name="project">The project to modify.</param>
        /// <param name="fileList">Files containing the information about the non member file items.</param>
        [SuppressMessage("Microsoft.Naming", "CA1702:CompoundWordsShouldBeCasedCorrectly", MessageId = "NonMember")]
        private static void AddNonMemberFileItems(XProjectNode project, IList<string> fileList)
        {
            Utilities.ArgumentNotNull("fileList", fileList);

            foreach (string fileKey in fileList)
            {
                HierarchyNode parentNode = project;
                string[] pathItems = fileKey.Split(new char[] { Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar });

                if (String.Equals(fileKey, project.ProjectFile, StringComparison.OrdinalIgnoreCase))
                {
                    continue;
                }

                XFolderNode topFolderNode = null;
                foreach (string fileOrDir in pathItems)
                {
                    string childNodeId = Path.Combine(parentNode.VirtualNodeName, fileOrDir);
                    FileInfo fileOrDirInfo = new FileInfo(Path.Combine(project.ProjectFolder, childNodeId));
                    if ((fileOrDirInfo.Attributes & FileAttributes.Hidden) == FileAttributes.Hidden)
                    {
                        break;
                    }

                    //HierarchyNode childNode = project.FindURL(fileOrDirInfo.FullName);
                    HierarchyNode childNode = parentNode.FindChild( childNodeId );
                    if (childNode == null)
                    {
                        if (topFolderNode == null)
                        {
                            topFolderNode = parentNode as XFolderNode;
                            if (topFolderNode != null && (!topFolderNode.IsNonMemberItem) && topFolderNode.IsExpanded)
                            {
                                topFolderNode = null;
                            }
                        }

                        ProjectElement element = new ProjectElement(project, null, true);
                        element.Rename(childNodeId);
                        element.SetMetadata(ProjectFileConstants.Name, childNodeId);
                        childNode = project.CreateFileNode(element);
                        parentNode.AddChild(childNode);
                        break;
                    }

                    parentNode = childNode;
                }
                ThreadHelper.ThrowIfNotOnUIThread();

                if (topFolderNode != null)
                {
                    topFolderNode.CollapseFolder();
                }
            }
        }

    }
}
