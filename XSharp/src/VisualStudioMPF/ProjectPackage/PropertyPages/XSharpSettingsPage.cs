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

namespace XSharp.Project
{
    // abstract, so we don't need to implement BindProperties and ApplyChanges
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
            // Get the Innet panel
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
    }
}

