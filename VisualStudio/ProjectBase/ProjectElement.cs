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

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio;
using MSBuild = Microsoft.Build.Evaluation;
using Microsoft.Build.Evaluation;
using MSBuildExecution = Microsoft.Build.Execution;
using Microsoft.VisualStudio.Shell;

namespace Microsoft.VisualStudio.Project
{
    // Abstracts over the most common differences between the Dev9 and Dev10 MSBuild item OM
    public static class MSBuildItem
    {
        public static string GetItemType(MSBuild.ProjectItem item)
        {
            return item.ItemType;
        }
        public static string GetMetadataValue(MSBuild.ProjectItem item, string name)
        {
            return item.GetMetadataValue(name);
        }
        public static string GetMetadataValue(MSBuildExecution.ProjectItemInstance item, string name)
        {
            return item.GetMetadataValue(name);
        }
        public static void SetMetadataValue(MSBuild.ProjectItem item, string name, string value)
        {
            item.SetMetadataValue(name, value);
        }
        public static string GetEvaluatedInclude(MSBuild.ProjectItem item)
        {
            return item.EvaluatedInclude;
        }
        public static string GetEvaluatedInclude(MSBuildExecution.ProjectItemInstance item)
        {
            return item.EvaluatedInclude;
        }
    }

    /// <summary>
    /// This class represent a project item (usualy a file) and allow getting and
    /// setting attribute on it.
    /// This class allow us to keep the internal details of our items hidden from
    /// our derived classes.
    /// While the class itself is public so it can be manipulated by derived classes,
    /// its internal constructors make sure it can only be created from within the assembly.
    /// </summary>
    public sealed class ProjectElement
    {
        #region fields
        private MSBuild.ProjectItem item;
        private ProjectNode itemProject;
        private bool deleted = false;
        private bool isVirtual = false;
        private Dictionary<string, string> virtualProperties;
        #endregion

        #region properties
        public string ItemName
        {
            get
            {
                if(this.HasItemBeenDeleted())
                {
                    return String.Empty;
                }
                else
                {
                    return this.item.ItemType;
                }
            }
            set
            {
                ThreadHelper.ThrowIfNotOnUIThread();
                if (!this.HasItemBeenDeleted())
                {
                    // Check out the project file.
                    if(!this.itemProject.QueryEditProjectFile(false))
                    {
                        throw Marshal.GetExceptionForHR(VSConstants.OLE_E_PROMPTSAVECANCELLED);
                    }

                    this.item.ItemType = value;
                    this.itemProject.SetProjectFileDirty(true);
                }
            }
        }

        public MSBuild.ProjectItem Item
        {
            get
            {
                return this.item;
            }
        }

        public bool IsVirtual
        {
            get
            {
                return this.isVirtual;
            }
        }
        #endregion

        #region ctors
        /// <summary>
        /// Constructor to create a new MSBuild.ProjectItem and add it to the project
        /// Only have internal constructors as the only one who should be creating
        /// such object is the project itself (see Project.CreateFileNode()).
        /// </summary>
        internal ProjectElement(ProjectNode project, string itemPath, string itemType)
        {
            Utilities.ArgumentNotNull("project", project);
            Utilities.ArgumentNotNull("itemPath", itemPath);
            Utilities.ArgumentNotNull("itemType", itemType);

            this.itemProject = project;

            // create and add the item to the project

            this.item = project.BuildProject.AddItem(itemType, Microsoft.Build.Evaluation.ProjectCollection.Escape(itemPath))[0];
            this.itemProject.SetProjectFileDirty(true);
            this.RefreshProperties();
        }

        /// <summary>
        /// Constructor to Wrap an existing MSBuild.ProjectItem
        /// Only have internal constructors as the only one who should be creating
        /// such object is the project itself (see Project.CreateFileNode()).
        /// </summary>
        /// <param name="project">Project that owns this item</param>
        /// <param name="existingItem">an MSBuild.ProjectItem; can be null if virtualFolder is true</param>
        /// <param name="virtualFolder">Is this item virtual (such as reference folder)</param>
        public ProjectElement(ProjectNode project, MSBuild.ProjectItem existingItem, bool virtualFolder)
        {
            Utilities.ArgumentNotNull("project", project);
            if(!virtualFolder && existingItem == null)
                throw new ArgumentNullException("existingItem");

            // Keep a reference to project and item
            this.itemProject = project;
            this.item = existingItem;
            this.isVirtual = virtualFolder;

            if(this.isVirtual)
                this.virtualProperties = new Dictionary<string, string>();
        }
        #endregion

        #region public methods
        /// <summary>
        /// Calling this method remove this item from the project file.
        /// Once the item is delete, you should not longer be using it.
        /// Note that the item should be removed from the hierarchy prior to this call.
        /// </summary>
        public void RemoveFromProjectFile()
        {
            if(!deleted && item != null)
            {
                deleted = true;
                itemProject.BuildProject.RemoveItem(item);
                this.itemProject.SetProjectFileDirty(true);
            }
            itemProject = null;
            item = null;
        }

        /// <summary>
        /// Set an attribute on the project element
        /// </summary>
        /// <param name="attributeName">Name of the attribute to set</param>
        /// <param name="attributeValue">Value to give to the attribute.  Use <c>null</c> to delete the metadata definition.</param>
        public void SetMetadata(string attributeName, string attributeValue)
        {
            Debug.Assert(String.Compare(attributeName, ProjectFileConstants.Include, StringComparison.OrdinalIgnoreCase) != 0, "Use rename as this won't work");

            if(this.IsVirtual)
            {
                // For virtual node, use our virtual property collection
                if(virtualProperties.ContainsKey(attributeName))
                    virtualProperties.Remove(attributeName);
                virtualProperties.Add(attributeName, attributeValue);
                return;
            }

            // Build Action is the type, not a property, so intercept
            if (String.Equals(attributeName, ProjectFileConstants.BuildAction, StringComparison.OrdinalIgnoreCase))
            {
                item.ItemType = attributeValue;
                return;
            }
            ThreadHelper.ThrowIfNotOnUIThread();

            String currentValue = item?.GetMetadataValue(attributeName);
            bool changed;
            if (String.IsNullOrEmpty(currentValue) && String.IsNullOrEmpty(attributeValue))
                changed = false;
            else
                changed = String.Compare(currentValue, attributeValue) != 0;
            if (changed)
            {
	            // Check out the project file.
	            if(!this.itemProject.QueryEditProjectFile(false))
	            {
	                throw Marshal.GetExceptionForHR(VSConstants.OLE_E_PROMPTSAVECANCELLED);
	            }

	            if(attributeValue == null)
	                item.RemoveMetadata(attributeName);
	            else
	                item.SetMetadataValue(attributeName, attributeValue);
	            itemProject.SetProjectFileDirty(true);
            }
        }

        public string GetEvaluatedMetadata(string attributeName)
        {
            if(this.IsVirtual)
            {
                // For virtual items, use our virtual property collection
                if(!virtualProperties.ContainsKey(attributeName))
                {
                    return String.Empty;
                }
                return virtualProperties[attributeName];
            }

            // cannot ask MSBuild for Include, so intercept it and return the corresponding property
            if (String.Compare(attributeName, ProjectFileConstants.Include, StringComparison.OrdinalIgnoreCase) == 0)
            {
                return item.EvaluatedInclude;
            }

            // Build Action is the type, not a property, so intercept this one as well
            if (String.Compare(attributeName, ProjectFileConstants.BuildAction, StringComparison.OrdinalIgnoreCase) == 0)
            {
                return item.ItemType;
            }

            return item.GetMetadataValue(attributeName);
        }

        /// <summary>
        /// Get the value of an attribute on a project element
        /// </summary>
        /// <param name="attributeName">Name of the attribute to get the value for</param>
        /// <returns>Value of the attribute</returns>
        public string GetMetadata(string attributeName)
        {
            if(this.IsVirtual)
            {
                // For virtual items, use our virtual property collection
                if(!virtualProperties.ContainsKey(attributeName))
                    return String.Empty;
                return virtualProperties[attributeName];
            }
            if (this.item != null)
            {
	            // cannot ask MSBuild for Include, so intercept it and return the corresponding property
	            if (String.Compare(attributeName, ProjectFileConstants.Include, StringComparison.OrdinalIgnoreCase) == 0)
	                return item.EvaluatedInclude;

	            // Build Action is the type, not a property, so intercept this one as well
	            if (String.Compare(attributeName, ProjectFileConstants.BuildAction, StringComparison.OrdinalIgnoreCase) == 0)
	                return item.ItemType;

	            return item.GetMetadataValue(attributeName);
            }
            return null;
        }

        /// <summary>
        /// Gets the attribute and throws the handed exception if the exception if the attribute is empty or null.
        /// </summary>
        /// <param name="attributeName">The name of the attribute to get.</param>
        /// <param name="exception">The exception to be thrown if not found or empty.</param>
        /// <returns>The attribute if found</returns>
        /// <remarks>The method will throw an Exception and neglect the passed in exception if the attribute is deleted</remarks>
        public string GetMetadataAndThrow(string attributeName, Exception exception)
        {
            Debug.Assert(!String.IsNullOrEmpty(attributeName), "Cannot retrieve an attribute for a null or empty attribute name");
            string attribute = GetMetadata(attributeName);

            if(String.IsNullOrEmpty(attributeName) && exception != null)
            {
                if(String.IsNullOrEmpty(exception.Message))
                {
                    Debug.Assert(!String.IsNullOrEmpty(this.itemProject.BaseURI.AbsoluteUrl), "Cannot retrieve an attribute for a project that does not have a name");
                    string message = String.Format(CultureInfo.CurrentCulture, SR.GetString(SR.AttributeLoad, CultureInfo.CurrentUICulture), attributeName, this.itemProject.BaseURI.AbsoluteUrl);
                    throw new Exception(message, exception);
                }
                throw exception;
            }

            return attribute;
        }


        public void Rename(string newPath)
        {
            string escapedPath = Microsoft.Build.Evaluation.ProjectCollection.Escape(newPath);
            if(this.IsVirtual)
            {
                virtualProperties[ProjectFileConstants.Include] = escapedPath;
                return;
            }

            item.Rename(escapedPath);
            this.RefreshProperties();
        }


        /// <summary>
        /// Reevaluate all properties for the current item
        /// This should be call if you believe the property for this item
        /// may have changed since it was created/refreshed, or global properties
        /// this items depends on have changed.
        /// Be aware that there is a perf cost in calling this function.
        /// </summary>
        public void RefreshProperties()
        {
            if(this.IsVirtual)
                return;

            itemProject.BuildProject.ReevaluateIfNecessary();

            IEnumerable<ProjectItem> items = itemProject.BuildProject.GetItems(item.ItemType);
            foreach (ProjectItem projectItem in items)
            {
                if(projectItem!= null && projectItem.UnevaluatedInclude.Equals(item.UnevaluatedInclude))
                {
                    item = projectItem;
                    return;
                }
            }
        }

        /// <summary>
        /// Return an absolute path for the passed in element.
        /// If the element is already an absolute path, it is returned.
        /// Otherwise, it is unrelativized using the project directory
        /// as the base.
        /// Note that any ".." in the paths will be resolved.
        ///
        /// For non-file system based project, it may make sense to override.
        /// </summary>
        /// <returns>FullPath</returns>
        public string GetFullPathForElement()
        {
            string path = this.GetMetadata(ProjectFileConstants.Include);
            if(!Path.IsPathRooted(path))
                path = Path.Combine(this.itemProject.ProjectFolder, path);

            // If any part of the path used relative paths, resolve this now
            path = Path.GetFullPath(path);
            return path;
        }

        #endregion

        #region helper methods
        /// <summary>
        /// Has the item been deleted
        /// </summary>
        private bool HasItemBeenDeleted()
        {
            return (this.deleted || this.item == null);
        }
        #endregion

        #region overridden from System.Object
        public static bool operator ==(ProjectElement element1, ProjectElement element2)
        {
            // Do they reference the same element?
            if(Object.ReferenceEquals(element1, element2))
                return true;

            // Verify that they are not null (cast to object first to avoid stack overflow)
            if(element1 as object == null || element2 as object == null)
            {
                return false;
            }

            Debug.Assert(!element1.IsVirtual || !element2.IsVirtual, "Cannot compare virtual nodes");

            // Cannot compare vitual items.
            if(element1.IsVirtual || element2.IsVirtual)
            {
                return false;
            }

            // Do they reference the same project?
            if(!element1.itemProject.Equals(element2.itemProject))
                return false;

            // Do they have the same include?
            string include1 = element1.GetMetadata(ProjectFileConstants.Include);
            string include2 = element2.GetMetadata(ProjectFileConstants.Include);

            // Unfortunately the checking for nulls have to be done again, since neither String.Equals nor String.Compare can handle nulls.
            // Virtual folders should not be handled here.
            if(include1 == null || include2 == null)
            {
                return false;
            }

            return String.Equals(include1, include2, StringComparison.CurrentCultureIgnoreCase);
        }


        public static bool operator !=(ProjectElement element1, ProjectElement element2)
        {
            return !(element1 == element2);
        }


        public override bool Equals(object obj)
        {
            ProjectElement element2 = obj as ProjectElement;
            if(element2 == null)
                return false;

            return this == element2;
        }


        public override int GetHashCode()
        {
            return base.GetHashCode();
        }
        #endregion



    }
}
