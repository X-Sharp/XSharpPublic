// VSSolution.prg
// Created by    : fabri
// Creation Date : 11/20/2019 1:00:07 PM
// Created for   : 
// WorkStation   : FABPORTABLE


USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE VFPXPorterLib

	/// <summary>
	/// The VSSolution class.
	/// </summary>
	CLASS VSSolution
		
		PROPERTY Projects AS List<VSProject> AUTO
		
		PROPERTY GUID AS STRING AUTO
		
		PUBLIC CONSTRUCTOR( )
			SELF:Projects := List<VSProject>{}
		SELF:GUID := System.Guid.NewGuid().ToString("B"):ToUpper()
		
		PUBLIC METHOD Save( solutionPath AS STRING ) AS VOID
			VAR sb := StringBuilder{}
			//
			sb:AppendLine("Microsoft Visual Studio Solution File, Format Version 12.00")
			sb:AppendLine("# Visual Studio Version 15")
			sb:AppendLine("VisualStudioVersion = 15.0.28307.1525")
			sb:AppendLine("MinimumVisualStudioVersion = 10.0.40219.1")
			FOREACH xsProj AS VSProject IN SELF:Projects
				sb:Append(e"Project(\"{AA6C8D78-22FF-423A-9C7C-5F2393824E04}\") = ")
				sb:Append('"')
				sb:Append(xsProj:Name )
				sb:Append('"')
				sb:Append(", ")
				sb:Append('"')
				sb:Append(xsProj:Name+".xsproj" )
				sb:Append('"')
				sb:Append(", ")
				sb:Append('"')
				sb:Append(xsProj:GUID)
				sb:Append('"')
				sb:AppendLine()
				sb:AppendLine("EndProject")
			NEXT
			//
			sb:AppendLine("Global")
			sb:AppendLine("GlobalSection(SolutionConfigurationPlatforms) = preSolution")
			sb:AppendLine("Debug|Any CPU = Debug|Any CPU")
			sb:AppendLine("Release|Any CPU = Release|Any CPU")
			sb:AppendLine("EndGlobalSection")
			sb:AppendLine("GlobalSection(ProjectConfigurationPlatforms) = postSolution")
			FOREACH xsProj AS VSProject IN SELF:Projects
				sb:Append(xsProj:GUID)
				sb:AppendLine(".Debug|Any CPU.ActiveCfg = Debug|Any CPU")
				sb:Append(xsProj:GUID)
				sb:AppendLine(".Debug|Any CPU.Build.0 = Debug|Any CPU")
				sb:Append(xsProj:GUID)
				sb:AppendLine(".Release|Any CPU.ActiveCfg = Release|Any CPU")
				sb:Append(xsProj:GUID)
				sb:AppendLine(".Release|Any CPU.Build.0 = Release|Any CPU")
			NEXT
			sb:AppendLine("EndGlobalSection")
			sb:AppendLine("GlobalSection(SolutionProperties) = preSolution")
			sb:AppendLine("HideSolutionNode = FALSE")
			sb:AppendLine("EndGlobalSection")
			sb:AppendLine("GlobalSection(ExtensibilityGlobals) = postSolution")
			sb:Append("SolutionGuid = ")
			sb:AppendLine( SELF:GUID )
			sb:AppendLine("EndGlobalSection")
			sb:AppendLine("EndGlobal")
			//
		System.IO.File.WriteAllText( solutionPath, sb:ToString() )
		
	END CLASS
END NAMESPACE // global::FabVFPXPorter.VSSupport