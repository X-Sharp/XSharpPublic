For the code generation to work we need the following in the registry:
Other entries will probably follow.

AA6C8D78-22FF-423A-9C7C-5F2393824E04 is our project Factory





Windows Registry Editor Version 5.00

[HKEY_LOCAL_MACHINE\SOFTWARE\WOW6432Node\Microsoft\VisualStudio\14.0\Generators\{AA6C8D78-22FF-423A-9C7C-5F2393824E04}]
@=""


[HKEY_LOCAL_MACHINE\SOFTWARE\WOW6432Node\Microsoft\VisualStudio\14.0\Generators\{AA6C8D78-22FF-423A-9C7C-5F2393824E04}\.xaml]
@="MSBuild:Compile"



[HKEY_LOCAL_MACHINE\SOFTWARE\WOW6432Node\Microsoft\VisualStudio\14.0\Generators\{AA6C8D78-22FF-423A-9C7C-5F2393824E04}\PublicResXFileCodeGenerator]
"GeneratesDesignTimeSource"=dword:00000001
@="Microsoft ResX File Code Generator (public class)"
"CLSID"="{69b6a86d-ef43-4d9e-a758-8a18b38a7384}"

[HKEY_LOCAL_MACHINE\SOFTWARE\WOW6432Node\Microsoft\VisualStudio\14.0\Generators\{AA6C8D78-22FF-423A-9C7C-5F2393824E04}\PublicSettingsSingleFileGenerator]
"GeneratesSharedDesignTimeSource"=dword:00000001
"GeneratesDesignTimeSource"=dword:00000000
@="Generator for strongly typed settings class (public class)"
"CLSID"="{940f36b5-a42e-435e-8ef4-20b9d4801d22}"

[HKEY_LOCAL_MACHINE\SOFTWARE\WOW6432Node\Microsoft\VisualStudio\14.0\Generators\{AA6C8D78-22FF-423A-9C7C-5F2393824E04}\ResXFileCodeGenerator]
"GeneratesDesignTimeSource"=dword:00000001
@="Microsoft ResX File Code Generator"
"CLSID"="{c77981ec-ba19-3348-878f-6f88745a251e}"

[HKEY_LOCAL_MACHINE\SOFTWARE\WOW6432Node\Microsoft\VisualStudio\14.0\Generators\{AA6C8D78-22FF-423A-9C7C-5F2393824E04}\SettingsSingleFileGenerator]
"GeneratesSharedDesignTimeSource"=dword:00000001
"GeneratesDesignTimeSource"=dword:00000000
@="Generator for strongly typed settings class"
"CLSID"="{3B4C204A-88A2-3AF8-BCFD-CFCB16399541}"

