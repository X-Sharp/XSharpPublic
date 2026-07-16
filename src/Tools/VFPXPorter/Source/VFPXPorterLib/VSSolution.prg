// VSSolution.prg
// Created by    : fabri
// Creation Date : 11/20/2019 1:00:07 PM
// Created for   :
// WorkStation   : FABPORTABLE


USING System
USING System.Collections.Generic
USING System.Text
USING System.Linq

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

			sb:AppendLine("Microsoft Visual Studio Solution File, Format Version 12.00")
			sb:AppendLine("# Visual Studio Version 15")
			sb:AppendLine("VisualStudioVersion = 15.0.28307.1525")
            sb:AppendLine("MinimumVisualStudioVersion = 10.0.40219.1")

			FOREACH xsProj AS VSProject IN SELF:Projects
				sb:Append(e"Project(\"{AA6C8D78-22FF-423A-9C7C-5F2393824E04}\") = ")
                sb:Append(i"""" + xsProj:Name + """, ")

                VAR cPath := IIF(String.IsNullOrEmpty(xsProj:RelativePath), xsProj:Name + ".xsproj", xsProj:RelativePath)
                sb:Append("""" + cPath + """, ")

				sb:Append("""" + xsProj:GUID + """")

                sb:AppendLine()
				sb:AppendLine("EndProject")
			NEXT
			//
            sb:AppendLine("Global")

                // Indentation with \t to VS doesn't mark the file as dirty
			    sb:AppendLine(e"\tGlobalSection(SolutionConfigurationPlatforms) = preSolution")
			        sb:AppendLine(e"\t\tDebug|Any CPU = Debug|Any CPU")
			        sb:AppendLine(e"\t\tRelease|Any CPU = Release|Any CPU")
                sb:AppendLine(e"\tEndGlobalSection")


                sb:AppendLine(e"\tGlobalSection(ProjectConfigurationPlatforms) = postSolution")
			    FOREACH xsProj AS VSProject IN SELF:Projects
				    sb:Append(e"\t\t" + xsProj:GUID)
				    sb:AppendLine(".Debug|Any CPU.ActiveCfg = Debug|Any CPU")
				    sb:Append(e"\t\t" + xsProj:GUID)
				    sb:AppendLine(".Debug|Any CPU.Build.0 = Debug|Any CPU")
				    sb:Append(e"\t\t" + xsProj:GUID)
				    sb:AppendLine(".Release|Any CPU.ActiveCfg = Release|Any CPU")
				    sb:Append(e"\t\t" + xsProj:GUID)
				    sb:AppendLine(".Release|Any CPU.Build.0 = Release|Any CPU")
                NEXT
                sb:AppendLine(e"\tEndGlobalSection")

			    sb:AppendLine(e"\tGlobalSection(SolutionProperties) = preSolution")
			        sb:AppendLine(e"\t\tHideSolutionNode = FALSE")
                sb:AppendLine(e"\tEndGlobalSection")

			    sb:AppendLine(e"\tGlobalSection(ExtensibilityGlobals) = postSolution")
			        sb:Append(e"\t\tSolutionGuid = ")
			        sb:AppendLine( SELF:GUID )
                sb:AppendLine(e"\tEndGlobalSection")

			sb:AppendLine("EndGlobal")
			//
            System.IO.File.WriteAllText( solutionPath, sb:ToString() )
            RETURN

        /// <summary>
        /// Try loading projects from an existing solution.
        /// </summary>
        PUBLIC METHOD Load( solutionPath AS STRING ) AS LOGIC
            IF !System.IO.File.Exists( solutionPath )
                RETURN FALSE
            ENDIF

            LOCAL lines := System.IO.File.ReadAllLines( solutionPath ) AS STRING[]

            FOREACH VAR line IN lines
                // We look for lines that define projects: Project("{GUID}") = "Name", "Path.xsproj", "{GUID}"
                IF line:Trim():StartsWith("Project(")
                    VAR parts := line:Split( <CHAR>{ ',', '=', '"', '(', ')', ' ' }, StringSplitOptions.RemoveEmptyEntries)

                    // Expected structure:
                    // [1] Project
                    // [2] {TypeGUID}
                    // [3] Name
                    // [4] Path
                    // [5] {ProjGUID}
                    IF parts:Length >= 5
                        VAR projName := parts[3]:Trim()
                        VAR projPath := parts[4]:Trim()
                        VAR projGuid := parts[5]:Trim()

                        // We avoid duplicates if the project is already on our list
                        IF !SELF:Projects:Any({ p => String.Compare(p:Name, projName, TRUE) == 0 })
                            // we add a project proxy (just name and GUID)
                            VAR p := VSProject{ projName }
                            p:GUID := projGuid // we keep the original GUID to VS does not get confused
                            p:RelativePath := projPath // to avoid losing the subfolder

                            SELF:Projects:Add( p )
                        ENDIF
                    ENDIF
                ENDIF
            NEXT
            RETURN TRUE

	END CLASS
END NAMESPACE // global::FabVFPXPorter.VSSupport
