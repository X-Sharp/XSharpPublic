This source is copied from the DotNetProjectSystem source for C#, VB and F#. (https://github.com/dotnet/project-system.git)

The source is from version 17.12 of that projectsystem (commit a21769f7bcb7f58dab1b623fe2f3c78fe7b2c1b1)

We have made the following changes:

We have added new files for X# in the folders:
- src\Microsoft.VisualStudio.ProjectSystem.Managed\XSharp
- src\Microsoft.VisualStudio.ProjectSystem.Managed.VS\XSharp
- src\VsProjectSystem\src\Microsoft.VisualStudio.ProjectSystem.Managed\ProjectSystem\DesignTimeTargets
- setup\ProjectSystemSetup
- src\Microsoft.VisualStudio.ProjectSystem.Managed\ProjectSystem\Rules\Items

We have made changes to the following files to add guids and other settings for X#

eng\imports\LanguageSettings.props
eng\imports\StrongName.targets
src\Microsoft.VisualStudio.ProjectSystem.Managed\ProjectSystem\ProjectCapability.cs
src\Microsoft.VisualStudio.ProjectSystem.Managed\Microsoft.VisualStudio.ProjectSystem.Managed.csproj
src\Microsoft.VisualStudio.ProjectSystem.Managed.VS\Microsoft.VisualStudio.ProjectSystem.Managed.VS.csproj
src\Microsoft.VisualStudio.ProjectSystem.Managed.VS\Packaging\ProjectTypeCapabilities.cs
src\Microsoft.VisualStudio.ProjectSystem.Managed.VS\Packaging\ProjectTypeRegistration.cs
src\Microsoft.VisualStudio.ProjectSystem.Managed.VS\ProjectSystem\ProjectType.cs
Directory.Build.props

We have renamed:
ProjectSystem.Sln to XSProjectSstem.sln

We will make the following changes later:
- Change the PublicKey by setting the StrongNameKey property in the project files to XSharp
- Change the csc compiler by setting the CSCToolExe property in the project files
- Changed the assembly names (and folder names) from Microsoft.* to XSharp.*
  Note that then also references inside build scripts etc need to be changed
- We need to add a special attribute (IgnoresAccessChecksTo) to make sure that the violations to access internal types are also supported at runtime.

- Because some of the code depends on internals from other assemblies we have added a special C# compiler (in the csc folder)
  that does not validate the public keys and also allows an assembly "XSharp.VisualStudio.ProjectSystem.Managed.dll" to be used when the attribute speaks of "Microsoft.VisualStudio.ProjectSystem.Managed"
  this is dirty but it works
  
- We have seen that the CreatePkgDef compiler fails to run on an assembly when the publickey is not properly set 
  We will have to manually change the ppkgdef file that is created for the VS project system and adjust it to X#
  
TODO:
- Link the project system to our codemodel:
  - register projects  
  - register source files
  - register references
  - implement IXSharpProject, so the Codemodel can talk back to the project system.

- Project Property pages for Language and Dialect settings.
- Dialect selection option on Application Property page and other X# specific properties, such as VulcanCompatibleResources
  
  
  
To make this work we also need to install files in the following folder:
c:\Program Files\Microsoft Visual Studio\2022\Enterprise\MSBuild\Microsoft\VisualStudio\Managed
The following files are expected in that folder:
- NativeResource.BrowseObject.xaml 
- NativeResource.xaml 
- ProjectItemsSchema.XSharp.xaml 
- VOBinary.BrowseObject.xaml 
- VOBinary.xaml 
- XSharp.DesignTime.targets 



