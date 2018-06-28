//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.VisualStudio.Project;

namespace XSharp.Project
{
   class XSharpAssemblyReferenceNode : AssemblyReferenceNode
   {
      internal XSharpAssemblyReferenceNode(ProjectNode root, ProjectElement element)
         : base(root, element)
      {
         //Binding reference data at startup will cause a 'project has changed' method
         //BindReferenceData();

      }
        internal XSharpAssemblyReferenceNode(ProjectNode root, string assemblyPath)
           : base(root, assemblyPath)
        {
            //Binding reference data at startup will cause a 'project has changed' method
            BindReferenceData();
            ResolveAssemblyReference();

        }
        protected override NodeProperties CreatePropertiesObject()
        {
            return new XSharpAssemblyReferenceNodeProperties(this);
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

    }
}
