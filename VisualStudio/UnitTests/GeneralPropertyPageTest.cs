/********************************************************************************************

Copyright (c) Microsoft Corporation 
All rights reserved. 

Microsoft Public License: 

This license governs use of the accompanying software. If you use the software, you 
accept this license. If you do not accept the license, do not use the software. 

1. Definitions 
The terms "reproduce," "reproduction," "derivative works," and "distribution" have the 
same meaning here as under U.S. copyright law. 
A "contribution" is the original software, or any additions or changes to the software. 
A "contributor" is any person that distributes its contribution under this license. 
"Licensed patents" are a contributor's patent claims that read directly on its contribution. 

2. Grant of Rights 
(A) Copyright Grant- Subject to the terms of this license, including the license conditions 
and limitations in section 3, each contributor grants you a non-exclusive, worldwide, 
royalty-free copyright license to reproduce its contribution, prepare derivative works of 
its contribution, and distribute its contribution or any derivative works that you create. 
(B) Patent Grant- Subject to the terms of this license, including the license conditions 
and limitations in section 3, each contributor grants you a non-exclusive, worldwide, 
royalty-free license under its licensed patents to make, have made, use, sell, offer for 
sale, import, and/or otherwise dispose of its contribution in the software or derivative 
works of the contribution in the software. 

3. Conditions and Limitations 
(A) No Trademark License- This license does not grant you rights to use any contributors' 
name, logo, or trademarks. 
(B) If you bring a patent claim against any contributor over patents that you claim are 
infringed by the software, your patent license from such contributor to the software ends 
automatically. 
(C) If you distribute any portion of the software, you must retain all copyright, patent, 
trademark, and attribution notices that are present in the software. 
(D) If you distribute any portion of the software in source code form, you may do so only 
under this license by including a complete copy of this license with your distribution. 
If you distribute any portion of the software in compiled or object code form, you may only 
do so under a license that complies with this license. 
(E) The software is licensed "as-is." You bear the risk of using it. The contributors give 
no express warranties, guarantees or conditions. You may have additional consumer rights 
under your local laws which this license cannot change. To the extent permitted under your 
local laws, the contributors exclude the implied warranties of merchantability, fitness for 
a particular purpose and non-infringement.

********************************************************************************************/

using System;
using System.IO;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Project;
using XSharp.Project;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Runtime.Versioning;

namespace XSharp.Project.UnitTests
{
    [TestClass]
    public class GeneralPropertyPageTest : BaseTest
    {
        private XSharpProjectPackage customProjectPackage;
        private XSharpGeneralPropertyPage generalPropertyPage;
        private XSharpProjectFactory customProjectFactory;
        private XSharpProjectNode projectNode;

        private const string expected = "test";

        [ClassInitialize]
        public static void TestClassInitialize(TestContext context)
        {
            projectFile = Path.Combine(context.TestDeploymentDir, projectFile);
        }

        [TestInitialize()]
        public override void Initialize()
        {
            base.Initialize();

            generalPropertyPage = new XSharpGeneralPropertyPage();

            customProjectPackage = new XSharpProjectPackage();
            ((IVsPackage)customProjectPackage).SetSite(serviceProvider);

            customProjectFactory = new XSharpProjectFactory(customProjectPackage);

            base.SetMsbuildEngine(customProjectFactory);

            int canCreate;
            if(VSConstants.S_OK == ((IVsProjectFactory)customProjectFactory).CanCreateProject(projectFile, 2, out canCreate))
            {
                PrivateType type = new PrivateType(typeof(XSharpProjectFactory));
                PrivateObject obj = new PrivateObject(customProjectFactory, type);
                projectNode = (XSharpProjectNode)obj.Invoke("PreCreateForOuter", new object[] { IntPtr.Zero });

                Guid iidProject = new Guid();
                int pfCanceled;
                projectNode.Load(projectFile, "", "", 2, ref iidProject, out pfCanceled);
            }
        }

        [TestMethod()]
        public void ConstructorTest()
        {
            XSharpGeneralPropertyPage page = generalPropertyPage;
            page.Name = expected;

            string actual = page.Name;
            Assert.AreEqual(expected, actual, "Name property value was not initialized by expected value in GeneralPropertyPage() constructor.");
        }

        [TestMethod()]
        public void ApplicationIconTest()
        {
            generalPropertyPage.ApplicationIcon = expected;

            string actual = generalPropertyPage.ApplicationIcon;

            Assert.AreEqual(expected, actual,
                "ApplicationIcon value was not initialized by expected value.");

            Assert.IsTrue((VSConstants.S_OK == generalPropertyPage.IsPageDirty()),
                "IsDirty status was unexpected after changing of the property of the tested object.");
        }

        [TestMethod()]
        public void AssemblyNameTest()
        {
            generalPropertyPage.AssemblyName = expected;

            string actual = generalPropertyPage.AssemblyName;

            Assert.AreEqual(expected, actual,
                "AssemblyName value was not initialized by expected value.");

            Assert.IsTrue((VSConstants.S_OK == generalPropertyPage.IsPageDirty()),
                "IsDirty status was unexpected after changing of the property of the tested object.");
        }

        [TestMethod()]
        public void DefaultNamespaceTest()
        {
            generalPropertyPage.RootNamespace = expected;

            string actual = generalPropertyPage.RootNamespace;

            Assert.AreEqual(expected, actual,
                "ApplicationIcon value was not initialized by expected value.");

            Assert.IsTrue((VSConstants.S_OK == generalPropertyPage.IsPageDirty()),
                "IsDirty status was unexpected after changing of the property of the tested object.");
        }

        [TestMethod()]
        public void OutputFileDllTest()
        {
            generalPropertyPage.OutputType = OutputType.Library;

            OutputType expected = OutputType.Library;
            OutputType actual = generalPropertyPage.OutputType;

            Assert.AreEqual(expected, actual,
                "OutputType value was not initialized by expected value.");

            Assert.IsTrue((VSConstants.S_OK == generalPropertyPage.IsPageDirty()),
                "IsDirty status was unexpected after changing of the property of the tested object.");
        }

        [TestMethod()]
        public void OutputFileExeTest()
        {
            generalPropertyPage.OutputType = OutputType.Exe;

            OutputType expected = OutputType.Exe;
            OutputType actual = generalPropertyPage.OutputType;

            Assert.AreEqual(expected, actual,
                "OutputType value was not initialized by expected value.");

            Assert.IsTrue((VSConstants.S_OK == generalPropertyPage.IsPageDirty()),
                "IsDirty status was unexpected after changing of the property of the tested object.");
        }

        [TestMethod()]
        public void OutputFileWinExeTest()
        {
            generalPropertyPage.OutputType = OutputType.WinExe;

            OutputType expected = OutputType.WinExe;
            OutputType actual = generalPropertyPage.OutputType;

            Assert.AreEqual(expected, actual,
                "OutputType value was not initialized by expected value.");

            Assert.IsTrue((VSConstants.S_OK == generalPropertyPage.IsPageDirty()),
                "IsDirty status was unexpected after changing of the property of the tested object.");
        }

        [TestMethod()]
        public void ProjectFileTest()
        {
            SetProjectConfig();

            string expected = Path.GetFileName(projectFile);
            string actual = generalPropertyPage.ProjectFile;

            Assert.AreEqual(expected, actual,
                "ProjectFile property value was initialized by unexpected path value.");
        }

        [TestMethod()]
        public void ProjectFolderTest()
        {
            SetProjectConfig();

            string expected = Path.GetDirectoryName(Path.GetDirectoryName(projectFile));
            string actual = generalPropertyPage.ProjectFolder;

            Assert.AreEqual(expected, actual,
                "ProjectFolder property value was initialized by unexpected path value.");
        }

        [TestMethod()]
        public void StartupObjectTest()
        {
            generalPropertyPage.StartupObject = expected;

            string actual = generalPropertyPage.StartupObject;

            Assert.AreEqual(expected, actual,
                "StartupObject value was not initialized by expected value.");

            Assert.IsTrue((VSConstants.S_OK == generalPropertyPage.IsPageDirty()),
                "IsDirty status was unexpected after changing of the property of the tested object.");
        }

        [TestMethod()]
        public void TargetFrameworkMonikerTest()
        {
            var fx4 = new FrameworkName(".NETFramework", new Version(4, 1));
            generalPropertyPage.TargetFrameworkMoniker = fx4;

            FrameworkName expected = fx4;
            FrameworkName actual = generalPropertyPage.TargetFrameworkMoniker;

            Assert.AreEqual(expected, actual,
                "TargetFrameworkMoniker value was not initialized to expected value.");

            Assert.IsTrue((VSConstants.S_OK == generalPropertyPage.IsPageDirty()),
                "IsDirty status was unexpected after changing of the property of the tested object.");
        }

        private void SetProjectConfig()
        {
            object[] ppUnk = new object[2];
            ProjectConfig config = new ProjectConfig(projectNode, "manualSetConfig");
            ppUnk[0] = config;
            generalPropertyPage.SetObjects(1, ppUnk);
        }
    }
}
