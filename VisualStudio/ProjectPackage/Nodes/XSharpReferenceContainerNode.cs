//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Project;
using System.Diagnostics;
using System.IO;

namespace XSharp.Project
{
    /// <summary>
    /// Reference container node
    /// </summary>
    public class XSharpReferenceContainerNode : ReferenceContainerNode
   {
      public XSharpReferenceContainerNode( ProjectNode project ) : base( project )
      {
      }

      protected override ProjectReferenceNode CreateProjectReferenceNode( ProjectElement element )
      {
         ProjectReferenceNode node = new XSharpProjectReferenceNode( this.ProjectMgr, element );
         ReferenceNode existing = null;
         if (isDuplicateNode(node, ref existing))
         {
             ProjectReferenceNode existingNode = existing as ProjectReferenceNode;
             return existingNode;
         }
         return node;
      }
      public override ReferenceNode AddReferenceFromSelectorData(VSCOMPONENTSELECTORDATA selectorData, string wrapperTool )
      {
         if (String.IsNullOrEmpty(wrapperTool ))
            wrapperTool = WrapperToolAttributeValue.TlbImp.ToString().ToLowerInvariant();
         foreach (ReferenceNode child in this.EnumReferences())
         {
                XSharpComReferenceNode comnode = child as XSharpComReferenceNode;

            if (comnode != null && comnode.Matches(selectorData, wrapperTool))
               return comnode;
          
         }
         return base.AddReferenceFromSelectorData(selectorData, wrapperTool);
      }
      protected override ProjectReferenceNode CreateProjectReferenceNode( VSCOMPONENTSELECTORDATA selectorData )
      {
         ProjectReferenceNode node = new XSharpProjectReferenceNode(this.ProjectMgr, selectorData.bstrTitle, selectorData.bstrFile, selectorData.bstrProjRef);
         ReferenceNode existing = null;
         if (isDuplicateNode(node, ref existing))
         {
             ProjectReferenceNode existingNode = existing as ProjectReferenceNode;
             return existingNode;

         }
         return node;

      }
      protected override AssemblyReferenceNode CreateAssemblyReferenceNode(ProjectElement element)
      {
          AssemblyReferenceNode node = null;
          try
          {
              node = new XSharpAssemblyReferenceNode(this.ProjectMgr, element);
          }
          catch (ArgumentNullException e)
          {
              Trace.WriteLine("Exception : " + e.Message);
          }
          catch (FileNotFoundException e)
          {
              Trace.WriteLine("Exception : " + e.Message);
          }
          catch (BadImageFormatException e)
          {
              Trace.WriteLine("Exception : " + e.Message);
          }
          catch (FileLoadException e)
          {
              Trace.WriteLine("Exception : " + e.Message);
          }
          catch (System.Security.SecurityException e)
          {
              Trace.WriteLine("Exception : " + e.Message);
          }
          ReferenceNode existing = null;
          if (isDuplicateNode(node, ref existing))
          {
              AssemblyReferenceNode existingNode = existing as AssemblyReferenceNode;
          }
          return node;
      }

       // How to handle multiple references.
      public static int multiRefAutoCorrection = 0;

      protected override ComReferenceNode CreateComReferenceNode(ProjectElement reference)
      {
          return new XSharpComReferenceNode(this.ProjectMgr, reference);
      }

      protected override ComReferenceNode CreateComReferenceNode(Microsoft.VisualStudio.Shell.Interop.VSCOMPONENTSELECTORDATA selectorData, string wrapperTool )
      {
          ComReferenceNode node = new XSharpComReferenceNode(this.ProjectMgr, selectorData, wrapperTool);
          return node;
      }
    public override int ImageIndex
        {
            get
            {
                if (this.CanShowDefaultIcon())
                    return XSharpImageListIndex.Reference + XSharpProjectNode.imageOffset;
                else
                    return XSharpImageListIndex.DanglingReference + XSharpProjectNode.imageOffset;
            }
        }

        private bool isDuplicateNode( string nodeCaption, ref ReferenceNode ExistingNode)
      {
          if (nodeCaption != null)
          {
              foreach (ReferenceNode child in this.EnumReferences())
              {
                  // check for duplicate nodes
                  if (child.Caption == nodeCaption)
                  {
                      ExistingNode = child;
                      return true;
                  }
              }
          }
          ExistingNode = null;
          return false;

      }

      private bool isDuplicateNode(ReferenceNode node, ref ReferenceNode ExistingNode )
      {
         if (node != null)
         {
            foreach (ReferenceNode child in this.EnumReferences())
            {
               // check for duplicate nodes
               if (child.Caption == node.Caption )
               {
                   ExistingNode = child;
                   return true;
               }
            }
         }
         ExistingNode = null;
         return false;

      }
      protected override ReferenceNode CreateFileComponent(VSCOMPONENTSELECTORDATA selectorData, string _wrapperTool = null)
      {
          ReferenceNode node = null;
          ReferenceNode existing = null;
          // To avoid the add of the Reference in the reference list
          // we will first check if it is already in there
          if (selectorData.bstrFile == null)
          {
              throw new ArgumentNullException("selectorData");
          }
          //
          if (selectorData.bstrFile[0] == '*')
          {
              selectorData.bstrFile = selectorData.bstrFile.Substring(1);
          }
          // We have a path to a file, it could be anything
          // First see if it is a managed assembly
          if (File.Exists(selectorData.bstrFile))
          {
              string assemblyPath = selectorData.bstrFile;
              System.Reflection.AssemblyName assemblyName = System.Reflection.AssemblyName.GetAssemblyName(assemblyPath);
              string caption = assemblyName.Name;
              if (isDuplicateNode(caption, ref existing))
              {
                    //
                    string existingUrl = existing.Url;
                    if (File.Exists(existingUrl))
                        return existing;
                    // file does not exist so this new node is better
                    existing.Remove(false);
              }
          }
          //
          // Ok, try to create and add the reference
          node = base.CreateFileComponent(selectorData, _wrapperTool);
          if (isDuplicateNode(node, ref existing))
          {
              // The CreateFileComponent create and Add the project element
              // but as it is duplicated..Remove it !
              node.Remove(false);
              return existing;
          }
          return node;
      }
      /// <summary>
      /// Creates an assembly reference node from a file path.
      /// </summary>
      protected override AssemblyReferenceNode CreateAssemblyReferenceNode(string fileName)
      {
          AssemblyReferenceNode node = null;
          try
          {
              // Ok when file name is a full path or when it doesn't have a DLL extension
              if (!File.Exists(fileName))
              {
                  if (fileName.EndsWith(".dll",StringComparison.OrdinalIgnoreCase))
                  {
                      fileName = Path.GetFileNameWithoutExtension(fileName);
                  }
              }
              node = new XSharpAssemblyReferenceNode(this.ProjectMgr, fileName);
          }
          catch (ArgumentNullException e)
          {
              Trace.WriteLine("Exception : " + e.Message);
          }
          catch (FileNotFoundException e)
          {
              Trace.WriteLine("Exception : " + e.Message);
          }
          catch (BadImageFormatException e)
          {
              Trace.WriteLine("Exception : " + e.Message);
          }
          catch (FileLoadException e)
          {
              Trace.WriteLine("Exception : " + e.Message);
          }
          catch (System.Security.SecurityException e)
          {
              Trace.WriteLine("Exception : " + e.Message);
          }

          return node;
      }

   }
}
