# X# Native Resources

XSharp (X#) programs, especially the ones converted from VO, contain many unmanaged resources.
The Roslyn compiler only allows 1 unmanaged resource to be bound to the DLL or EXE.

When compiling the project the build system joins all native resources into one "virtual" resource file and creates a compiled native resource called NativeResources.res .
When your application contains a version resource then this version resource will be included in this file too.
When your application contains a manifest then this manifest will be included in the resource too.

When an application does not have any unmanaged resources then Roslyn will create a version resource from the Assembly level attributes such as 
- AssemblyVersion
- AssemblyCompany
- AssemblyDescription

We received a request to automatically add a version resource based on these attributes to the native resources produced by the resource compiler.
We also discovered that Roslyn takes the native resources "apart" and reconstructs them again and when doing so sometimes the native resources were corrupted.

So what we are doing now in the changed compiler code is:

- we are creating a version resource based on the Assembly Attributes
- we are creating a default manifest
- when the application already has a native resource then we suppress the version resource that the resource compile produced
- when the application does not have a manifest then we add a manifest to the resources that the resource compile produced
- after all of this we let Roslyn reconstruct the resources and we call a Win32 api function to "validate" the resources.   This fixes the problems that Roslyn created.
- when the assembly is digitally signed then we re-sign the assembly, since the validation of the resource may have invalidated the digital signature

You can suppress the addition of the generated version resource by using the `-nativeversion` compiler option


