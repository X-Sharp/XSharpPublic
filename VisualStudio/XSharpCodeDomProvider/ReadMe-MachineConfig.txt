// Installation :
// C:\Windows\Microsoft.NET\Framework\v4.0.30319\Config
//

To get the provider to work with MsBuild properly add the following to machine.config.
If a compilers node already exists in the config, then just add the <compiler line to the section
The publickeytoken must match the snk file
The version number must match the version number of the assembly
The assembly must be registered in the GAC

    
<system.codedom>
   <compilers>
      <compiler language="XSharp" extension=".prg"
          type="XSharp.CodeDom.XSharpCodeDomProvider,XSharpCodeDomProvider,
          Version=1.0.0.0, Culture=neutral, PublicKeyToken=ed555a0467764586,
          ProcessorArchitecture=MSIL"/>
    </compilers>
</system.codedom>

