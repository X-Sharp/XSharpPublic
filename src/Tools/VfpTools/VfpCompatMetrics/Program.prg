USING System
USING System.IO
USING System.Reflection
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING System.Web.Script.Serialization


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

    // JSON models (Deserialization)
    class FoxUniverseRoot
        property Language as string auto
        property Version as string auto
        property TotalFunctions as int auto
        property Functions as List<UniverseFunction> auto
    end class

    class UniverseFunction
        property Name as string auto
        property Category as string auto
        property Engine as string auto
        property Criticality as string auto
        property IsExtension as logic auto
        property IsIntrinsic as logic auto
    end class

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
        STATIC METHOD Validate(data AS List<FoxFunctionMetadata>, universe as FoxUniverseRoot) AS List<ValidationIssue>
            VAR issues := List<ValidationIssue>{}
            VAR nameMap := Dictionary<string, FoxFunctionMetadata>{StringComparer.OrdinalIgnoreCase}

            // Detect orphans
            var universeSet := HashSet<string>{StringComparer.OrdinalIgnoreCase}
            foreach uFunc as UniverseFunction in universe:Functions
                if !String.IsNullOrWhiteSpace(uFunc:Name)
                    universeSet:Add(uFunc:Name)
                endif
            next

            FOREACH item AS FoxFunctionMetadata IN data
                IF String.IsNullOrWhiteSpace(item:Name)
                    issues:Add(ValidationIssue{}{ Level := "ERROR", FunctionName := "<Unknown>", Message := "Unnamed attribute in the technical method: " + item:MethodName })
                    loop
                ENDIF

                // 1. Detect Orphanhood (it's in code but not in JSON)
                if !universeSet:Contains(item:Name)
                    issues:Add(ValidationIssue{}{ Level := "ALERT", FunctionName := item:Name, Message := "ORPHAN: Decorated in code but missing from vfp_universe.json" })
                endif

                // 2. Duplicated
                IF nameMap:ContainsKey(item:Name)
                    var original := nameMap[item:Name]
                    issues:Add(ValidationIssue{}{ Level := "ERROR", FunctionName := item:Name, Message := "DUPLICATED: Already decorated in '" + original:MethodName + "'" })
                ELSE
                    nameMap:Add(item:Name, item)
                ENDIF


                // 3. Warnings and Debt
                IF item:Status == "Full" && !String.IsNullOrWhiteSpace(item:Notes)
                    issues:Add(ValidationIssue{}{ Level := "WARNING", FunctionName := item:Name, Message := "State is 'Full' but contains limitation notes." })
                ENDIF

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
        PROPERTY TotalUniverse AS INT AUTO
        property ImplementedCount as int auto
        property MissingCount as int auto
        property TotalExtensions as int auto
        property TotalIntrinsics as int auto              
        property MissingFunctions as List<string> auto    

        property StatusCounts as Dictionary<string, int> auto
        property EngineCoverage as Dictionary<string, string> auto

        property StructuralCoverage as real8 auto
        property TrueCompatibilityScore as real8 auto

        constructor(runtimeData as List<FoxFunctionMetadata>, universeRoot as FoxUniverseRoot)
            self:StatusCounts := Dictionary<string, int>{StringComparer.OrdinalIgnoreCase}
            self:EngineCoverage := Dictionary<string, string>{}
            self:MissingFunctions := List<string>{}

            var engImplCounts := Dictionary<string, int>{StringComparer.OrdinalIgnoreCase}
            var engUnivCounts := Dictionary<string, int>{StringComparer.OrdinalIgnoreCase}

            self:TotalUniverse := universeRoot:Functions:Count
            self:MissingCount := 0
            self:ImplementedCount := 0
            self:TotalExtensions := 0
            self:TotalIntrinsics := 0

            var implementedMap := Dictionary<string, FoxFunctionMetadata>{StringComparer.OrdinalIgnoreCase}
            foreach rItem as FoxFunctionMetadata in runtimeData
                if !String.IsNullOrWhiteSpace(rItem:Name) && !implementedMap:ContainsKey(rItem:Name)
                    implementedMap:Add(rItem:Name, rItem)
                endif
            next

            local totalWeight := 0.0 as real8
            local earnedWeight := 0.0 as real8

            foreach uItem as UniverseFunction in universeRoot:Functions
                if uItem:IsExtension
                    self:TotalUniverse--
                    if implementedMap:ContainsKey(uItem:Name)
                        self:TotalExtensions++
                        var rItem := implementedMap[uItem:Name]
                        self:IncrementDict(self:StatusCounts, "X# Extension")
                    endif
                    loop
                endif
                
                if uItem:IsIntrinsic
                    self:TotalIntrinsics++
                    self:ImplementedCount++ // Se considera 100% implementada por ser del compilador

                    self:IncrementDict(engUnivCounts, uItem:Engine)
                    self:IncrementDict(engImplCounts, uItem:Engine)
                    self:IncrementDict(self:StatusCounts, "Intrinsic")

                    var cWeight := self:GetCriticalityWeight(uItem:Criticality)
                    totalWeight += (1.0 * cWeight)
                    earnedWeight += (1.0 * cWeight) // Peso 1.0 = Full compatibilidad
                    loop
                endif

                if implementedMap:ContainsKey(uItem:Name)
                    var rItem := implementedMap[uItem:Name]
                    self:ImplementedCount++

                    self:IncrementDict(engUnivCounts, rItem:Engine)
                    self:IncrementDict(engImplCounts, rItem:Engine)
                    self:IncrementDict(self:StatusCounts, rItem:Status)

                    var cWeight := self:GetCriticalityWeight(rItem:Criticality)
                    var sWeight := self:GetStatusWeight(rItem:Status)

                    totalWeight += (1.0 * cWeight)
                    earnedWeight += (sWeight * cWeight)
                else
                    self:MissingCount++
                    self:MissingFunctions:Add(uItem:Name)
                    self:IncrementDict(self:StatusCounts, "Missing")

                    self:IncrementDict(engUnivCounts, uItem:Engine)

                    var cWeight := self:GetCriticalityWeight(uItem:Criticality)
                    totalWeight += (1.0 * cWeight)
                endif
            next

            if self:TotalUniverse > 0
                self:StructuralCoverage := ((Real8)self:ImplementedCount) / ((real8)self:TotalUniverse)
            endif

            if totalWeight > 0.0
                self:TrueCompatibilityScore := earnedWeight / totalWeight
            else
                self:TrueCompatibilityScore := 0.0
            endif

            foreach kvp as KeyValuePair<string, int> in engUnivCounts:OrderByDescending({x => x:Value})
                local engName := kvp:Key as string
                local uniCount := kvp:Value as int
                local impCount := 0 as int

                if engImplCounts:ContainsKey(engName)
                    impCount := engImplCounts[engName]
                endif

                local perc := 0.0 as real8
                if uniCount > 0; perc := ((real8)impCount / (real8)uniCount) * 100.0; endif

                self:EngineCoverage:Add(engName, String.Format("{0,3} / {1,-3} ({2,5:F1} %)", impCount, uniCount, perc))
            next
        end constructor

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
                CASE @@status == "Full" .OR. @@status == "Intrinsic"; RETURN 1.0
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
                OTHERWISE; RETURN 2.0 // medium by default
            END CASE
        END METHOD
    END CLASS

    // =========================================================================
    // 6. Report printer
    // =========================================================================
    CLASS ReportPrinter
        STATIC METHOD Print(metrics AS MetricsEngine, issues AS List<ValidationIssue>) AS VOID
            Console.WriteLine("=============================================")
            Console.WriteLine("    FORMAL VFP COMPATIBILITY AUDIT REPORT    ")
            Console.WriteLine("=============================================")
            Console.WriteLine(String.Format("Theoretical VFP9 Universe: {0} standard functions", metrics:TotalUniverse))
            Console.WriteLine(String.Format("VFP9 Functions Implemented: {0}", metrics:ImplementedCount))
            Console.WriteLine(String.Format("VFP9 Functions Missing    : {0}", metrics:MissingCount))

            if metrics:TotalIntrinsics > 0
                Console.ForegroundColor := ConsoleColor.DarkGreen
                Console.WriteLine(String.Format("X# Compiler Intrinsics   : {0} (Resolved internally)", metrics:TotalIntrinsics))
                Console.ResetColor()
            endif

            if metrics:TotalExtensions > 0
                Console.ForegroundColor := ConsoleColor.Cyan
                Console.WriteLine(String.Format("X# Custom Extensions   : {0} (Innovations)", metrics:TotalExtensions))
                Console.ResetColor()
            endif
            Console.WriteLine("--------------------------------------------------")

            Console.WriteLine(String.Format("Structural Coverage: {0:P2} (Raw Code vs JSON)", metrics:StructuralCoverage))
            Console.WriteLine(String.Format("TRUE WEIGHTED SCORE: {0:P2} (Implementation Quality)", metrics:TrueCompatibilityScore))
            Console.WriteLine("--------------------------------------------------")

            IF metrics:MissingCount > 0
                Console.ForegroundColor := ConsoleColor.Red
                Console.WriteLine(">>> MISSING FUNCTIONS (Not found in DLL & Not Intrinsic):")
                FOREACH missingName AS STRING IN metrics:MissingFunctions
                    Console.WriteLine("  - " + missingName)
                NEXT
                Console.ResetColor()
                Console.WriteLine("--------------------------------------------------")
            ENDIF

            Console.WriteLine(">>> DISTRIBUTION BY STATUS:")
            FOREACH kvp AS KeyValuePair<STRING, INT> IN metrics:StatusCounts:OrderByDescending({ x => x:Value })
                Console.WriteLine(String.Format("  - {0, -15}: {1}", kvp:Key, kvp:Value))
            NEXT
            Console.WriteLine("--------------------------------------------------")

            Console.WriteLine(">>> HEATMAP BY ENGINE (Implemented / Known Universe):")
            FOREACH kvp AS KeyValuePair<STRING, string> IN metrics:EngineCoverage
                Console.WriteLine(String.Format("  - {0, -15}: {1}", kvp:Key, kvp:Value))
            NEXT
            Console.WriteLine("--------------------------------------------------")

            IF issues:Count > 0
                Console.WriteLine(">>> AUDIT ISSUES DETECTED (" + issues:Count:ToString() + "):")
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

        static method PrintInventory(data as List<FoxFunctionMetadata>) as void
            Console.WriteLine("==================================================")
            Console.WriteLine("       VFP RUNTIME IMPLEMENTED INVENTORY          ")
            Console.WriteLine("==================================================")
            Console.WriteLine(String.Format("Total Functions Found: {0}", data:Count))
            Console.WriteLine("--------------------------------------------------")

            var grouped := Dictionary<string, List<FoxFunctionMetadata>>{StringComparer.OrdinalIgnoreCase}

            foreach item as FoxFunctionMetadata in data
                var eng := iif(String.IsNullOrWhiteSpace(item:Engine), "Unspecified", item:Engine)
                if !grouped:ContainsKey(eng)
                    grouped:Add(eng, List<FoxFunctionMetadata>{})
                endif
                grouped[eng]:Add(item)
            next

            foreach kvp as KeyValuePair<string, List<FoxFunctionMetadata>> in grouped:OrderBy({ x => x:Key })
                Console.ForegroundColor := ConsoleColor.Cyan
                Console.WriteLine(">> ENGINE: " + kvp:Key + " (" + kvp:Value:Count:ToString() + " functions)")
                Console.ResetColor()

                var sortedFuncs := kvp:Value:OrderBy({ f => f:Name })

                foreach func as FoxFunctionMetadata in sortedFuncs
                    local statusColor := ConsoleColor.Gray as ConsoleColor
                    if func:Status == "Full"; statusColor := ConsoleColor.Green; endif
                    if func:Status == "Stub" || func:Status == "NotSupported"; statusColor := ConsoleColor.Red; endif
                    if func:Status == "Partial"; statusColor := ConsoleColor.Yellow; endif

                    Console.Write("  - ")
                    Console.ForegroundColor := statusColor
                    Console.Write(String.Format("[{0, -12}] ", func:Status))
                    Console.ResetColor()

                    var crit := iif(String.IsNullOrWhiteSpace(func:Criticality), "Unspecified", func:Criticality)

                    Console.WriteLine(String.Format("{0, -25} (Category: {1, -20} | Criticality: {2})", func:Name, func:Category, crit))
                next
                Console.WriteLine()
            next

            Console.WriteLine("==================================================")
            Console.WriteLine("  End of Inventory")
            Console.WriteLine("==================================================")
        end method
    END CLASS

    // =========================================================================
    // 7. CLI entry point
    // =========================================================================
    CLASS Program
        STATIC METHOD Start(args AS STRING[]) AS INT

	        IF args:Length == 0
                Console.WriteLine("Usage for Audit: vfpCompatMetrics <path_to_XSharp_VFP_dll> <path_to_universe_json>")
                Console.WriteLine("Usage for Inventory: vfpCompatMetrics <path_to_XSharp_VFP_dll>")
                RETURN 1
            ENDIF

            VAR asmPath := args[0]
            var isInventoryMode := (args:Length == 1) // Si solo hay 1 argumento, es Inventario

            TRY
                Console.WriteLine("Loading assembly for analysis: " + asmPath)
                VAR asm := AssemblyLoader.Load(asmPath)

                Console.WriteLine("Extracting runtime metadata...")
                VAR metadataList := MetadataExtractor.Extract(asm)

                Console.Clear()

                if isInventoryMode
                    ReportPrinter.PrintInventory(metadataList)
                else
                    var jsonPath := args[1]
                    Console.WriteLine("Loading theoretical universe from JSON...")
                    var jsonString := File.ReadAllText(jsonPath)

                    var serializer := JavaScriptSerializer{}
                    serializer:MaxJsonLength := 2147483644
                    var universeRoot := serializer:Deserialize<FoxUniverseRoot>(jsonString)

                    Console.WriteLine("Analyzing metrics against the universe...")
                    VAR metrics := MetricsEngine{metadataList, universeRoot}
                    VAR issues := ValidationEngine.Validate(metadataList, universeRoot)

                    ReportPrinter.Print(metrics, issues)
                endif
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
