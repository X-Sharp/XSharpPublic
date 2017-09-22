using Microsoft.VisualStudio.Shell.Interop;
using System;
using System.Runtime.InteropServices;

namespace XSharp.Project
{
    static internal class EditorHelpers
    {
        internal static bool IsOurFile(string fileName)
        {
            var serviceProvider = XSharpEditorFactory.GetServiceProvider();
            // Find the document in the Running Document Table and Get Its ID
            IVsRunningDocumentTable rdt = serviceProvider.GetService(typeof(IVsRunningDocumentTable)) as IVsRunningDocumentTable;
            uint itemID;
            IVsHierarchy hierarchy;
            IntPtr unkDocData;
            uint cookie;
            rdt.FindAndLockDocument((uint)(_VSRDTFLAGS.RDT_NoLock), fileName,out hierarchy, out itemID, out unkDocData, out cookie);
            if (unkDocData != IntPtr.Zero)
            {
                Marshal.Release(unkDocData);
            }
            object result;
            // Ask for the Language. We return the product name
            hierarchy.GetProperty(itemID, (int)__VSHPROPID8.VSHPROPID_DiagHubLanguage, out result);
            return (result is string && (string)result == Constants.Product);
        }
    }
}
