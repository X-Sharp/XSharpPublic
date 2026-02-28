
USING System
USING System.IO
USING System.Reflection
USING System.Collections.Generic
USING System.Linq
USING System.Text


BEGIN NAMESPACE VfpCompatMetrics
    // =======================================================
    // 1. Data Models
    // =======================================================
    CLASS FoxFunctionMetadata
        PROPERTY Name AS STRING AUTO
        PROPERTY Category AS STRING AUTO
        PROPERTY Engine AS STRING AUTO
        PROPERTY Status AS STRING AUTO
        PROPERTY Criticality AS STRING AUTO
        PROPERTY Notes AS STRING AUTO
        PROPERTY MethodName AS STRING AUTO
    END CLASS

    CLASS ValidationIssue
        PROPERTY Level AS STRING AUTO // "ERROR", "WARNING", "ALERT"
        PROPERTY FunctionName AS STRING AUTO
        PROPERTY Message AS STRING AUTO
    END CLASS

    // =======================================================
    // 2. Assembly Loader
    // =======================================================
    CLASS AssemblyLoader
        STATIC METHOD Load(assemblyPath AS STRING) AS Assembly
            IF !File.Exists(assemblyPath)
                THROW FileNotFoundException{"The assembly file was not found in the specified path", assemblyPath}
            ENDIF

            RETURN Assembly.LoadFrom(assemblyPath)
        END METHOD
    END CLASS

    // =======================================================
    // 3. Metadata extractor
    // =======================================================
    CLASS MetadataExtractor
        STATIC METHOD Extract(asm AS Assembly) AS List<FoxFunctionMetadata>
            VAR results := List<FoxFunctionMetadata>{}
            LOCAL types AS Type[]

            TRY
                types := asm:GetTypes()
            CATCH ex AS ReflectionTypeLoadException
                VAR validTypes := List<Type>{}
                FOREACH t AS Type IN ex:Types
                    IF t != NULL
                        validTypes:Add(t)
                    ENDIF
                NEXT
                types := validTypes:ToArray()
            END TRY

            FOREACH @@type AS Type IN types
                LOCAL methods AS MethodInfo[]
                TRY
                    methods := @@type:GetMethods(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static | BindingFlags.Instance)
                CATCH
                    LOOP
                END TRY

                FOREACH @@method AS MethodInfo IN methods
                    VAR attributes := @@method:GetCustomAttributes(FALSE)

                    FOREACH attr AS OBJECT IN attributes
                        VAR attrType := attr:GetType()

                        IF attrType:Name == "FoxProFunctionAttribute"
                            VAR meta := FoxFunctionMetadata{}
                            meta:MethodName := @@method:Name

                            meta:Name := GetPropString(attr, attrType, "Name")
                            meta:Category := GetPropString(attr, attrType, "Category")
                            meta:Engine := GetPropString(attr, attrType, "Engine")
                            meta:Status := GetPropString(attr, attrType, "Status")
                            meta:Criticality := GetPropString(attr, attrType, "Criticality")
                            meta:Notes := GetPropString(attr, attrType, "Notes")

                            results:Add(meta)
                            EXIT // just 1 attribute per function
                        ENDIF
                    NEXT
                NEXT
            NEXT
            RETURN results
        END METHOD

        PRIVATE STATIC METHOD GetPropString(@@instance AS OBJECT, @@type AS Type, propName AS STRING) AS STRING
            VAR prop := @@type:GetProperty(propName)
            IF prop != NULL
                VAR val := prop:GetValue(@@instance)
                IF val != NULL
                    RETURN val:ToString()
                ENDIF
            ENDIF

            RETURN ""
        END METHOD
    END CLASS

    // =======================================================
    // 4. Validation engine
    // =======================================================
    CLASS ValidationEngine
        STATIC METHOD Validate(data AS List<FoxFunctionMetadata>) AS List<ValidationIssue>
            VAR issues := List<ValidationIssue>{}
            VAR nameSet := HashSet<STRING>{}

            FOREACH item AS FoxFunctionMetadata IN data
                // 1. Empty name
                IF String.IsNullOrWhiteSpace(item:Name)
                    issues:Add(ValidationIssue{}{ Level := "ERROR", FunctionName := "<Unknown>", Message := "Unnamed attribute in the technical method: " + item:MethodName })
                ELSE
                    // 2. Duplicated names
                    VAR upperName := item:Name:ToUpper()
                    IF nameSet:Contains(upperName)
                        issues:Add(ValidationIssue{}{ Level := "ERROR", FunctionName := item:Name, Message := "Duplicated function name. More than 1 overload was decorated." })
                    ELSE
                        nameSet:Add(upperName)
                    ENDIF
                ENDIF

                // 3. Simple warning: marked as "Full" but with notes/limitations
                IF item:Status == "Full" && !String.IsNullOrWhiteSpace(item:Notes)
                    issues:Add(ValidationIssue{}{ Level := "WARNING", FunctionName := item:Name, Message := "State is 'Full' but contains limitation notes." })
                ENDIF

                // 4. Critical alert: High Criticality but is a Stub
                IF item:Criticality == "High" && item:Status == "Stub"
                    issues:Add(ValidationIssue{}{ Level := "ALERT", FunctionName := item:Name, Message := "CRITICAL TECHNICAL DEBT: Essential function marked only as Stub"})
                ENDIF
            NEXT

            RETURN issues
        END METHOD
    END CLASS

    // =========================================================================
    // 5. Metrics Engine
    // =========================================================================
    CLASS MetricsEngine
        PROPERTY TotalFunctions AS INT AUTO
        PROPERTY StatusCounts AS Dictionary<STRING, INT> AUTO
        PROPERTY EngineCounts AS Dictionary<STRING, INT> AUTO
        PROPERTY CategoryCounts AS Dictionary<STRING, INT> AUTO
        PROPERTY CompatibilityScore AS REAL8 AUTO

        CONSTRUCTOR(data AS List<FoxFunctionMetadata>)
            SELF:StatusCounts := Dictionary<STRING, INT>{}
            SELF:EngineCounts := Dictionary<STRING, INT>{}
            SELF:CategoryCounts := Dictionary<STRING, INT>{}
            SELF:TotalFunctions := data:Count

            LOCAL totalWeight := 0.0 AS REAL8
            LOCAL earnedWeight := 0.0 AS REAL8

            FOREACH item AS FoxFunctionMetadata IN data
                SELF:IncrementDict(SELF:StatusCounts, item:Status)
                SELF:IncrementDict(SELF:EngineCounts, item:Engine)
                SELF:IncrementDict(SELF:CategoryCounts, item:Category)

                VAR cWeight := SELF:GetCriticalityWeight(item:Criticality)
                VAR sWeight := SELF:GetStatusWeight(item:Status)

                totalWeight += (1.0 * cWeight)
                earnedWeight += (sWeight * cWeight)
            NEXT

            IF totalWeight > 0.0
                SELF:CompatibilityScore := earnedWeight / totalWeight
            ELSE
                SELF:CompatibilityScore := 0.0
            ENDIF
        END CONSTRUCTOR

        PRIVATE METHOD IncrementDict(dict AS Dictionary<STRING, INT>, key AS STRING) AS VOID
            VAR safeKey := IIF(String.IsNullOrWhiteSpace(key), "Unspecified", key)

            IF dict:ContainsKey(safeKey)
                dict[safeKey] := dict[safeKey] + 1
            ELSE
                dict:Add(safeKey, 1)
            ENDIF
        END METHOD

        PRIVATE METHOD GetStatusWeight(@@status AS STRING) AS REAL8
            DO CASE
                CASE @@status == "Full"; RETURN 1.0
                CASE @@status == "Partial"; RETURN 0.75
                CASE @@status == "Changed"; RETURN 0.60
                CASE @@status == "Stub"; RETURN 0.30
                CASE @@status == "Obsolete"; RETURN 0.20
                CASE @@status == "NotSupported"; RETURN 0.0
                OTHERWISE; RETURN 0.0
            END CASE
        END METHOD

        PRIVATE METHOD GetCriticalityWeight(crit AS STRING) AS REAL8
            DO CASE
                CASE crit == "High"; RETURN 3.0
                CASE crit == "Medium"; RETURN 2.0
                CASE crit == "Low"; RETURN 1.0
                OTHERWISE; RETURN 1.0 // base weight by default
            END CASE
        END METHOD
    END CLASS

    // =========================================================================
    // 6. Report printer
    // =========================================================================
    CLASS ReportPrinter
        STATIC METHOD Print(metrics AS MetricsEngine, issues AS List<ValidationIssue>) AS VOID
            Console.WriteLine("=============================================")
            Console.WriteLine("     VISUAL FOXPRO COMPATIBILITY REPORT      ")
            Console.WriteLine("=============================================")
            Console.WriteLine(String.Format("Total Instrumented Functions: {0}", metrics:TotalFunctions))

            Console.WriteLine(String.Format("Weighted Compatibility Score: {0:P2}", metrics:CompatibilityScore))
            Console.WriteLine("--------------------------------------------------")

            Console.WriteLine(">>> DISTRIBUTION BY STATUS:")
            FOREACH kvp AS KeyValuePair<STRING, INT> IN metrics:StatusCounts:OrderByDescending({ x => x:Value })
                Console.WriteLine(String.Format("  - {0, -15}: {1}", kvp:Key, kvp:Value))
            NEXT
            Console.WriteLine("--------------------------------------------------")

            Console.WriteLine(">>> DISTRIBUTION BY ENGINE:")
            FOREACH kvp AS KeyValuePair<STRING, INT> IN metrics:EngineCounts:OrderByDescending({ x => x:Value })
                Console.WriteLine(String.Format("  - {0, -15}: {1}", kvp:Key, kvp:Value))
            NEXT
            Console.WriteLine("--------------------------------------------------")

            IF issues:Count > 0
                Console.WriteLine(">>> CONSISTENCY PROBLEMS DETECTED (" + issues:Count:ToString() + "):")
                FOREACH issue AS ValidationIssue IN issues
                    LOCAL color := ConsoleColor.Yellow AS ConsoleColor
                    IF issue:Level == "ERROR"; color := ConsoleColor.Red; ENDIF
                    IF issue:Level == "ALERT"; color := ConsoleColor.Magenta; ENDIF
                    IF issue:Level == "WARNING"; color := ConsoleColor.Yellow; ENDIF

                    Console.ForegroundColor := color
                    Console.WriteLine(String.Format("  [{0}] {1}: {2}", issue:Level, issue:FunctionName, issue:Message))
                    Console.ResetColor()
                NEXT
            ELSE
                Console.ForegroundColor := ConsoleColor.Green
                Console.WriteLine(">>> No consistency problems were found")
                Console.ResetColor()
            ENDIF
            Console.WriteLine("=============================================")
        END METHOD
    END CLASS

    // =========================================================================
    // 7. CLI entry poitn
    // =========================================================================
    CLASS Program
        STATIC METHOD Start(args AS STRING[]) AS INT
	        IF args:Length == 0
                Console.WriteLine("Usage: vfpCompatMetrics <path_to_XSharp_VFP_dll>")
                RETURN 1
            ENDIF

            VAR asmPath := args[0]

            TRY
                Console.WriteLine("Loading assembly for analysis: " + asmPath)
                VAR asm := AssemblyLoader.Load(asmPath)

                Console.WriteLine("Extracting metadata based on attributes...")
                VAR metadataList := MetadataExtractor.Extract(asm)

                Console.WriteLine("Analyzing metrics and consistency...")
                VAR metrics := MetricsEngine{metadataList}
                VAR issues := ValidationEngine.Validate(metadataList)

                Console.Clear()
                ReportPrinter.Print(metrics, issues)

            CATCH ex AS Exception
                Console.ForegroundColor := ConsoleColor.Red
                Console.WriteLine("Fatal error during analysis:")
                Console.WriteLine(ex:Message)
                Console.ResetColor()
                RETURN -1
            END TRY


            RETURN 0
        END METHOD
    END CLASS
END NAMESPACE
