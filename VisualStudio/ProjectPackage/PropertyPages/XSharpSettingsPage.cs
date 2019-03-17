//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Runtime.Versioning;
using System.Windows.Forms;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Project;
using EnvDTE;
using EnvDTE80;
using System.ComponentModel;
using System.Drawing;
using Microsoft.VisualStudio.OLE.Interop;
using System.Reflection;

namespace XSharp.Project
{
    [CLSCompliant(false), ComVisible(true)]
    public abstract class XSharpSettingsPage : SettingsPage, IInternalExtenderProvider
    {

        #region IInternalExtenderProvider Members
        public object GetExtenderNames(string ExtenderCATID, object ExtendeeObject)
        {
            IVsHierarchy outerHierarchy = HierarchyNode.GetOuterHierarchy(ProjectMgr);

            if (outerHierarchy is IInternalExtenderProvider)
                return ((IInternalExtenderProvider)outerHierarchy).GetExtenderNames(ExtenderCATID, ExtendeeObject);

            return null;
        }

        public object GetExtender(string ExtenderCATID, string ExtenderName, object ExtendeeObject, IExtenderSite ExtenderSite, int Cookie)
        {
            IVsHierarchy outerHierarchy = HierarchyNode.GetOuterHierarchy(ProjectMgr);

            if (outerHierarchy is IInternalExtenderProvider)
                return ((IInternalExtenderProvider)outerHierarchy).GetExtender(
                    ExtenderCATID, ExtenderName, ExtendeeObject, ExtenderSite, Cookie);

            return null;
        }

        public bool CanExtend(string ExtenderCATID, string ExtenderName, object ExtendeeObject)
        {
            //
            IVsHierarchy outerHierarchy = HierarchyNode.GetOuterHierarchy(ProjectMgr);

            if (outerHierarchy is IInternalExtenderProvider)
                return ((IInternalExtenderProvider)outerHierarchy).CanExtend(
                    ExtenderCATID, ExtenderName, ExtendeeObject);

            return false;
        }
        #endregion

        #region ExtenderSupport

        [Browsable(false)]
        [AutomationBrowsable(false)]
        public virtual string ExtenderCATID
        {
            get
            {
                Guid catid = ProjectMgr.ProjectMgr.GetCATIDForType(GetType());
                if (Guid.Empty.CompareTo(catid) == 0)
                    throw new NotImplementedException();
                return catid.ToString("B");
            }
        }

        [Browsable(false)]
        [AutomationBrowsable(false)]
        public object ExtenderNames
        {
            get
            {
                ObjectExtenders extenderService = (ObjectExtenders)ProjectMgr.GetService(typeof(ObjectExtenders));
                return extenderService.GetExtenderNames(ExtenderCATID, this);
            }
        }

        public object get_Extender(string extenderName)
        {
            ObjectExtenders extenderService = (ObjectExtenders)ProjectMgr.GetService(typeof(ObjectExtenders));
            return extenderService.GetExtender(ExtenderCATID, extenderName, this);
        }

        #endregion

        // Expand the inner-panel to the full visible size
        public override void Move(Microsoft.VisualStudio.OLE.Interop.RECT[] arrRect)
        {
            base.Move(arrRect);
            // Get the Inner panel
            Control innerPanel = Control.FromHandle(new IntPtr(Grid.Handle));
            // Get the Size of the visible surface
            Size s = ThePanel.Size;
            s.Height -= 6;
            s.Width -= 6;
            // Move to the Top/Left
            innerPanel.Location = new Point(3, 3);
            // Expand the inner panel
            innerPanel.Size = s;
            //
        }
        internal bool getPrjLogic(String Name,  bool defaultValue = false)
        {
            bool property;
            string value = this.ProjectMgr.GetProjectProperty(Name, true);
            if (value != null)
                property = (value.ToLower() == "true");
            else
                property = defaultValue;
            return property;
        }

        internal bool  getCfgLogic(String Name, bool defaultValue = false)
        {
            bool property;
            string value = this.GetConfigProperty(Name);
            if (value != null)
                property = (value.ToLower() == "true");
            else
                property = defaultValue;
            return property;
        }


        internal string getPrjString(String Name, string defaultValue = "", bool unevaluated = false)
        {
            string property;
            string value = this.ProjectMgr.GetProjectProperty(Name, true, unevaluated);
            if (!String.IsNullOrEmpty(value))
                property = value;
            else
                property = defaultValue;
            return property;
        }
        internal string getCfgString(String Name,  string defaultValue = "")
        {
            string property;
            string value = this.GetUnevaluatedConfigProperty(Name);
            if (!String.IsNullOrEmpty(value))
                property = value;
            else
                property = defaultValue;
            return property;
        }

        internal int getCfgInteger(String Name, int defaultValue)
        {
            int property;
            string value = this.GetConfigProperty(Name);
            bool ok = false;
            int result;
            ok = int.TryParse(value, out result);
            if (!ok)
                property = defaultValue;
            else
                property = result;
            return property;
        }
        internal int getPrjInteger(String Name, int defaultValue)
        {
            int property;
            string value = this.ProjectMgr.GetProjectProperty(Name,true);
            bool ok = false;
            int result;
            ok = int.TryParse(value, out result);
            if (!ok)
                property = defaultValue;
            else
                property = result;
            return property;
        }

        internal void RemovePrjProperty(String Name)
        {
            var prop = this.ProjectMgr.BuildProject.GetProperty(Name);
            if (prop != null)
                this.ProjectMgr.BuildProject.RemoveProperty(prop);
        }

        internal static string AddSlash(string folderName)
        {
            if (String.IsNullOrEmpty(folderName))
                folderName = "";
            folderName = folderName.TrimEnd();
            if (folderName != "" && folderName[folderName.Length - 1] != Path.DirectorySeparatorChar)
            {
                folderName += Path.DirectorySeparatorChar;
            }
            return folderName;

        }

        internal void SetFieldReadOnly(string fieldName, bool readOnly)
        {
            PropertyDescriptor descriptor = TypeDescriptor.GetProperties(this.GetType())[fieldName];
            if (descriptor != null)
            {
                ReadOnlyAttribute attribute = (ReadOnlyAttribute)
                                              descriptor.Attributes[typeof(ReadOnlyAttribute)];
                if (attribute != null)
                {
                    FieldInfo fieldToChange = attribute.GetType().GetField("isReadOnly",
                                                     System.Reflection.BindingFlags.NonPublic |
                                                     System.Reflection.BindingFlags.Instance);
                    if (fieldToChange != null)
                        fieldToChange.SetValue(attribute, readOnly);
                }
            }

        }

    }
}

