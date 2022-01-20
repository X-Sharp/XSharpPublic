//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.Runtime.InteropServices;
using Microsoft.VisualStudio.Project.Automation;
using System;
using Microsoft.VisualStudio.Project;
using VSLangProj;

using VSLangProj150;
using VSLangProj140;
using VSLangProj80;

namespace XSharp.Project
{

    /// <summary>
    /// Represents an automation friendly version of a language-specific project.
    /// </summary>
    [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Interoperability", "CA1409:ComVisibleTypesShouldBeCreatable")]
    [ComVisible(true), CLSCompliant(false)]
    public class OAXSharpVSProject : OAVSProject, VSProject4
    {
        private OAVSProjectImports imports;
        private VSProjectEvents events;
        internal OAXSharpVSProject(ProjectNode project) : base(project)
        {
            this.imports = new OAVSProjectImports(this.Project);
            this.events = new OAVSProjectEvents(this);
        }
        public override Imports Imports
        {
            get
            {
                return imports;
            }
        }
        
        public override VSProjectEvents Events
        {
            get
            {
                return events;
            }
        }

        public object PublishManager => throw new NotImplementedException();

        public VSProjectEvents2 Events2 => throw new NotImplementedException();

        public AnalyzerReferences AnalyzerReferences => throw new NotImplementedException();

        public VSLangProj150.PackageReferences PackageReferences
        {
            get
            {
                if (this.Project is OAXSharpProject oaxp)
                {
                    if (oaxp.Project is XSharpProjectNode xpn)
                    {
                        XSharpPackageReferenceContainerNode referenceContainer = xpn?.PackageReferenceContainerNode ?? null;
                        if (referenceContainer == null)
                        {
                            return null;
                        }
                        return referenceContainer.Object as PackageReferences;
                    }
                }
                return null;
            }
        }


    }

}
