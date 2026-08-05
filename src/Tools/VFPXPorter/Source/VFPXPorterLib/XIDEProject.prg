USING System.Collections.Generic
USING System.IO

BEGIN NAMESPACE VFPXPorterLib
	
	STATIC CLASS XideProject
		STATIC PRIVATE oWriter AS StreamWriter
		STATIC METHOD Save( oSolution AS VSSolution, cSolutionPath AS STRING ) AS LOGIC
			LOCAL lSuccess := FALSE AS LOGIC
			TRY
				BEGIN USING oWriter := StreamWriter{ Path.ChangeExtension( cSolutionPath, "xiproj" ) }
					Write( "PROJECT = {0}", Path.GetFileNameWithoutExtension( cSolutionPath ) )
					Write( "OutputFolder = {0}", "%ProjectPath%\Bin\" )
					Write( "GUID = {0}", TrimGUID( oSolution:GUID ) )
					Write( "SupportCF = 0" )
					Write( "NoAppFileCreate = 1" )
					
					Write( "[Applications]" )
					FOREACH oApp AS VSProject IN oSolution:Projects
						SaveApp( oApp, Path.GetDirectoryName( cSolutionPath ), oWriter )
					NEXT
					Write( "[EndApplications]" )
					Write( "" )
					Write( "[Configurations] )" )
					Write( "CONFIG=Debug,11111111-1111-1111-1111-111111111111,\Debug\" )
					Write( "CONFIG=Release,22222222-2222-2222-2222-222222222222,\Release\" )
					lSuccess := TRUE
				END USING
			CATCH
				lSuccess := FALSE
			END TRY
		RETURN lSuccess
		
		STATIC PRIVATE METHOD SaveApp( oApp AS VSProject, cSolutionFolder AS STRING, oWriter AS StreamWriter ) AS VOID
			LOCAL lContainsXSharpFolder := FALSE AS LOGIC
			
			LOCAL cAppFolder AS STRING
			cAppFolder := Path.GetDirectoryName( oApp:ProjectFileNameFullPath )
			
			IF .not. String.IsNullOrEmpty( cAppFolder ) .and. .not. String.IsNullOrEmpty( cSolutionFolder )
				IF cAppFolder:ToUpperInvariant( ):StartsWith( cSolutionFolder:ToUpperInvariant( ) )
					cAppFolder := "%ProjectPath%\" + cAppFolder:Substring( cSolutionFolder:Length )
				END IF
			END IF
			
			Write( "; ************** APPLICATION {0} *************", oApp:Name )
			Write( "Application = {0}", oApp:Name )
			
			Write( "Target = {0}", iif( oApp:ProjectType == ProjectType.ClassLibrary, "2", iif( oApp:ProjectType == ProjectType.Console , "0", "1" ) ) )
			Write( "Platform = AnyCPU" )
			Write( "Language = XSharp" )
			Write( "Runtime = CLR4" )                 
			Write( "Dialect = VFP" )
			Write( "Folder = {0}", cAppFolder )
			Write( "PrgSubFolder = " )
			Write( "ResourcesSubFolder = " )
			Write( "NameSpace = {0}", oApp:Name )
			Write( "Assembly = {0}", oApp:Name )
			Write( "GUID = {0}", TrimGUID( oApp:GUID ) )
			Write( "" )

			LOCAL oAppFolder AS FileFolder
			oAppFolder := FileFolder.Create( )
			FOREACH oFile AS AssetInfo IN oApp:FileList
				IF oFile:Action == FileAction.DoNotCopy
					LOCAL cFileName AS STRING
					cFileName := oFile:Path
					IF cFileName:StartsWith( ".." )
						LOOP
					END IF
					LOCAL nIndex := cFileName:LastIndexOf( "\" ) AS INT
					IF nIndex != - 1
						LOCAL cFolder AS STRING
						cFolder := cFileName:Substring(  0, nIndex )
						oAppFolder:AddSubFolder( cFolder )
					END IF
				ENDIF
			NEXT
			Write( "" )
			Write( "[FileGroups]" )
			LOCAL aFolders AS List< FileFolder >
			aFolders := List< FileFolder >{ }
			oAppFolder:GetAllFolders( aFolders )
			FOREACH oFolder AS FileFolder IN aFolders
				Write( "FileFolder = {0}", oFolder:Name )
				Write( "FileGroupGUID = {0}", TrimGUID( oFolder:Guid ) )
				IF oFolder:ParentFolder != NULL
					Write( "FileGroupGroup = {0}", TrimGUID( oFolder:ParentFolder:Guid ) )
				END IF
				IF oFolder:Name:ToUpperInvariant( ) == "XSHARP"
					lContainsXSharpFolder := TRUE
				END IF
			NEXT
			Write( "" )
			Write( "[Files]" )
			FOREACH oFile AS AssetInfo IN oApp:FileList
				IF oFile:Action == FileAction.DoNotCopy
					LOCAL cFileName AS STRING
					cFileName := oFile:Path
					IF cFileName:StartsWith( ".." )
						Write( "File = {0}", "%AppPath%\" + cFileName )
					ELSE
						Write( "File = {0}", "%AppPath%\" + cFileName )
						LOCAL nIndex := cFileName:LastIndexOf( "\" ) AS INT
						IF nIndex != - 1
							LOCAL cFolder AS STRING
							cFolder := cFileName:Substring(  0, nIndex )
							LOCAL oFolder AS FileFolder
							oFolder := oAppFolder:FindFolder( cFolder )
							Write( "FileFileGroup = {0}", oFolder:Guid )
						END IF
					END IF
					Write( "FileGUID = {0}", System.Guid.NewGuid( ):ToString( ):ToUpperInvariant( ) )
					LOCAL cExtension AS STRING
					cExtension := Path.GetExtension( cFileName ):ToLowerInvariant( )
					IF cExtension == ".prg"
						Write( "FileType = Code" )
					ELSEIF cExtension == ".xh"
						Write( "FileType = Text" )
					ELSE
						Write( "FileType = Binary" )
					END IF
					
				ENDIF
			NEXT
			Write( "" )
			Write( "[References]" )
			FOREACH oReference AS VFPXPorterLib.Reference IN oApp:ReferenceList
				DO CASE
				CASE oReference:IsXSharp
					Write( "ReferenceGAC = CLR4,{0}", oReference:Include )
				CASE oReference:IsLocal
					Write( "ReferenceBrowse = {0}", "%AppPath%\" + oReference:Include )
				OTHERWISE
					IF oReference:Include:StartsWith( "mscorlib" )
						LOOP
					END IF
					Write( "ReferenceGAC = CLR4,{0}", oReference:Include )
				END CASE
			NEXT
			FOREACH oLibrary AS VSProject IN oApp:ProjectReferenceList
				Write( "ReferenceProject = {0}", TrimGUID( oLibrary:GUID ) )
			NEXT
			Write( "" )
			Write( "[General Options]" )
			Write( "Switches=/define:VFP2XS" )
			Write( "IgnoreWarnings = 9073,9025,9092,9094,219,9108,1030" )
			Write( "ZeroArrays = 0" )
			Write( "CaseSensitive = 0" )
			Write( "ImplicitNamespace = 1" )
			Write( "VO1 = 0" )
			Write( "VO2 = 0" )
			Write( "VO3 = 0" )
			Write( "VO4 = 0" )
			Write( "VO5 = 0" )
			Write( "VO6 = 0" )
			Write( "VO7 = 0" )
			Write( "VO8 = 0" )
			Write( "VO9 = 1" )
			Write( "VO10 = 0" )
			Write( "VO11 = 0" )
			Write( "VO12 = 0" )
			Write( "VO13 = 0" )
			Write( "VO14 = 0" )
			Write( "VO15 = 1" )
			Write( "VO16 = 0" )
			Write( "VO17 = 0" )
			Write( "FOX1 = 0" )
			Write( "FOX2 = 0" )
			Write( "XPP1 = 0" )
			Write( "LateBound = 1" )
			Write( "Unsafe = 0" )
			Write( "Undeclared = 1" )
			Write( "EnforceSelf = 0" )
			Write( "EnforceOverride = 0" )
			Write( "UseNativeVersion = 0" )
			Write( "MemVar = 1" )
			Write( "IgnoreStdDefs = 0" )
			Write( "Ovf = 0" )
			Write( "FOvf = 0" )
			Write( "ModernSyntax = 0" )
			Write( "NamedArgs = 0" )
			Write( "InitLocals = 0" )
			Write( "AllowOldStyleAssignments = 1" )
			Write( "AllowDotOption = 1" )
			IF lContainsXSharpFolder
				Write( "StdDefsFile = {0}", "%AppPath%\XSharp\VFPXPorter.xh" )
			ENDIF
			Write( "" )
			Write( "[Configurations]" )
			Write( "AppConfig = Debug,11111111-1111-1111-1111-111111111111" )
			Write( "  Debug = 1" )
			Write( "  DebugInit = 1" )
			Write( "  DefineDebug = 1" )
			Write( "  DefineTrace = 0" )
			Write( "AppConfig = Release,22222222-2222-2222-2222-222222222222" )
			Write( "  Debug = 0" )
			Write( "  DebugInit = 0" )
			Write( "  DefineDebug = 0" )
			Write( "  DefineTrace = 1" )
			Write( "" )
			Write( "ENDApplication = {0}", oApp:Name )
			Write( "" )
		END METHOD
				
		STATIC PRIVATE METHOD TrimGUID( cGuid AS STRING ) AS STRING
		RETURN cGuid:Trim( <Char>{ c'{', c'}' } )

		STATIC PRIVATE METHOD Write( cLine AS STRING ) AS VOID
			Write( cLine, NULL )
		END METHOD
		STATIC PRIVATE METHOD Write( cLine AS STRING, oParam AS OBJECT ) AS VOID
			oWriter:WriteLine( String.Format( cLine, oParam ) )
		END METHOD
		
		PRIVATE CLASS FileFolder
			PRIVATE aSubFolders AS Dictionary< STRING, FileFolder >
			PRIVATE lRoot AS LOGIC
			PRIVATE cGuid AS STRING
	
			PROPERTY Name AS STRING AUTO GET PRIVATE SET
			PROPERTY FullName AS STRING AUTO GET PRIVATE SET
			PROPERTY Guid AS STRING GET SELF:cGuid
			PROPERTY SubFolders AS Dictionary< STRING, FileFolder > GET SELF:aSubFolders
			PROPERTY ParentFolder AS FileFolder AUTO GET PRIVATE SET
			
			PRIVATE CONSTRUCTOR( lRoot AS LOGIC )
				SELF:aSubFolders := Dictionary< STRING, FileFolder >{ }
				SELF:cGuid := System.Guid.NewGuid( ):ToString( ):ToUpperInvariant( )
				SELF:lRoot := lRoot
			END CONSTRUCTOR
			
			STATIC METHOD Create( ) AS FileFolder
				LOCAL oAppFolder AS FileFolder
				oAppFolder := FileFolder{ TRUE }
			RETURN oAppFolder
			
			
			METHOD FindFolder( cFullName AS STRING ) AS FileFolder
				cFullName := cFullName:ToUpperInvariant( )
				IF SELF:FullName == cFullName
					RETURN SELF
				END IF
				FOREACH oSubFolder AS FileFolder IN SELF:aSubFolders:Values
					IF oSubFolder:FindFolder( cFullName ) != NULL
						RETURN oSubFolder:FindFolder( cFullName )
					END IF
				NEXT
			RETURN NULL
			
			METHOD GetAllFolders( aFolders AS List< FileFolder > ) AS VOID
				FOREACH oSubFolder AS FileFolder IN SELF:aSubFolders:Values
					aFolders:Add( oSubFolder )
					oSubFolder:GetAllFolders( aFolders )
				NEXT
			END METHOD
			
			METHOD AddSubFolder( cFolder AS STRING ) AS VOID
				LOCAL nIndex := cFolder:IndexOf( "\" ) AS INT
				LOCAL cSubFolder := NULL AS STRING
				IF nIndex != -1
					cSubFolder := cFolder:Substring( nIndex + 1 )
					cFolder := cFolder:Substring( 0, nIndex )
				END IF
	
				LOCAL oFolder AS FileFolder
				IF SELF:aSubFolders:ContainsKey( cFolder:ToUpperInvariant( ) )
					oFolder := SELF:aSubFolders[ cFolder:ToUpperInvariant( ) ]
				ELSE
					oFolder := FileFolder{ FALSE }
					oFolder:Name := cFolder
					oFolder:FullName := iif( String.IsNullOrEmpty( SELF:Name ), "", SELF:FullName + "\" ) + cFolder:ToUpperInvariant( )
					IF .not. SELF:lRoot
						oFolder:ParentFolder := SELF
					END IF
					SELF:aSubFolders:Add( cFolder:ToUpperInvariant( ), oFolder )
				END IF
				IF cSubFolder != NULL
					oFolder:AddSubFolder( cSubFolder )
				END IF
			END METHOD
		END CLASS

	END CLASS

END NAMESPACE
