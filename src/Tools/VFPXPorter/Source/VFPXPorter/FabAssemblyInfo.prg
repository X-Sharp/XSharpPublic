USING System
USING System.Collections.Generic
USING System.Text
USING System.Reflection

BEGIN NAMESPACE VFPXPorter

	/// <summary>
	/// The AssemblyInfo class.
	/// </summary>
	PUBLIC CLASS FabAssemblyInfo
		PRIVATE asembly AS Assembly
		
		PUBLIC PROPERTY Company AS STRING GET GetAssemblyAttribute({a AS AssemblyCompanyAttribute => a:Company})
		PUBLIC PROPERTY Product AS STRING GET GetAssemblyAttribute({a AS AssemblyProductAttribute => a:Product})
		PUBLIC PROPERTY Copyright AS STRING GET GetAssemblyAttribute({a AS AssemblyCopyrightAttribute => a:Copyright})
		PUBLIC PROPERTY Trademark AS STRING GET GetAssemblyAttribute({a AS AssemblyTrademarkAttribute => a:Trademark})
		PUBLIC PROPERTY Title AS STRING GET GetAssemblyAttribute({a AS AssemblyTitleAttribute => a:Title})
		PUBLIC PROPERTY Description AS STRING GET GetAssemblyAttribute({a AS AssemblyDescriptionAttribute => a:Description})
		PUBLIC PROPERTY Configuration AS STRING GET GetAssemblyAttribute({a AS AssemblyDescriptionAttribute => a:Description})
		PUBLIC PROPERTY FileVersion AS STRING GET GetAssemblyAttribute({a AS AssemblyFileVersionAttribute => a:Version})
		
		//
		PUBLIC PROPERTY Version AS Version GET asembly:GetName():Version
		PUBLIC PROPERTY VersionFull AS STRING GET Version:ToString()
		PUBLIC PROPERTY VersionMajor AS STRING GET Version:Major:ToString()
		PUBLIC PROPERTY VersionMinor AS STRING GET Version:Minor:ToString()
		PUBLIC PROPERTY VersionBuild AS STRING GET Version:Build:ToString()
		PUBLIC PROPERTY VersionRevision AS STRING GET Version:Revision:ToString()

		//
		PUBLIC PROPERTY ReferencedAssemblies AS AssemblyName[] GET asembly:GetReferencedAssemblies()
		PUBLIC PROPERTY RuntimeVersion AS STRING GET asembly:ImageRuntimeVersion
		
		PUBLIC CONSTRUCTOR(pathToAssembly AS STRING )
			SELF:asembly := Assembly.LoadFrom(pathToAssembly)
			
			
		PRIVATE METHOD GetAssemblyAttribute<T>( getValue AS @@Func<T, STRING> ) AS STRING WHERE T IS Attribute 
			LOCAL attribute AS T
			//
			attribute := (T)Attribute.GetCustomAttribute(SELF:asembly, typeof(T))
			RETURN getValue(attribute)
			
			
	END CLASS
END NAMESPACE 
