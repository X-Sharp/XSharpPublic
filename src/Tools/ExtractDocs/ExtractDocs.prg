#pragma options("az", on)
// ExtractDocs.prg
// Created by    : XSharp team
// Purpose       : Extract inline /// XML doc comment blocks from X# .prg files
//                 into an external XML file and replace them with
//                 /// <include file="..." path="doc/<EntityPath>/*" />
//
// Rules:
//   - Single-line /// <exclude .../> tags are left as-is in the source.
//   - Single-line /// <include .../> tags are left as-is (already external).
//   - Single-line /// <inheritdoc .../> tags are left as-is.
//   - Every other doc block (with actual content) is extracted.
//   - Entity names containing the X# @@ keyword-escape prefix are
//     normalised (e.g. @@Value -> Value) so the XML element name is valid.
//   - DELEGATE is treated as a member-level declaration (like METHOD), NOT a
//     scoped type, because it has no END DELEGATE and creates no new body scope.
//
// Usage:  ExtractDocs <sourceFolder> <xmlFileName> <xmlOutputPath>
//
// Example:
//   ExtractDocs ..\Runtime\XSharp.Core XSharp.Core.Docs.xml ..\..\..\Docs\XSharp.Core.Docs.xml

USING System
USING System.IO
USING System.Collections.Generic
USING System.Text
USING System.Text.RegularExpressions

// -- modifier / keyword sets ------------------------------------------------
STATIC CLASS XSharpKeywords
    STATIC PUBLIC Modifiers      AS HashSet<STRING>
    STATIC PUBLIC TypeKeywords   AS HashSet<STRING>
    STATIC PUBLIC MemberKeywords AS HashSet<STRING>
    STATIC PUBLIC GlobalKeywords AS HashSet<STRING>
    STATIC PUBLIC FieldVisibility AS HashSet<STRING>
    STATIC CONSTRUCTOR
        Modifiers := HashSet<STRING>{StringComparer.OrdinalIgnoreCase}
        FOREACH VAR kw IN <STRING>{"public","private","protected","internal","hidden","exported",;
                                    "static","virtual","override","abstract","sealed",;
                                    "partial","async","unsafe","new","extern","readonly"}
            Modifiers:Add(kw)
        NEXT

        // Scoped types: each one ends with END <keyword> and can contain members.
        // DELEGATE is intentionally excluded � it is a single-line declaration,
        // not a container, and is handled as a member keyword instead.
        TypeKeywords := HashSet<STRING>{StringComparer.OrdinalIgnoreCase}
        FOREACH VAR kw IN <STRING>{"class","interface","structure","struct","enum"}
            TypeKeywords:Add(kw)
        NEXT

        MemberKeywords := HashSet<STRING>{StringComparer.OrdinalIgnoreCase}
        FOREACH VAR kw IN <STRING>{"method","property","access","assign","operator","event","delegate"}
            MemberKeywords:Add(kw)
        NEXT

        GlobalKeywords := HashSet<STRING>{StringComparer.OrdinalIgnoreCase}
        FOREACH VAR kw IN <STRING>{"function","procedure","global","define"}
            GlobalKeywords:Add(kw)
        NEXT

        FieldVisibility := HashSet<STRING>{StringComparer.OrdinalIgnoreCase}
        FOREACH VAR kw IN <STRING>{"public","private","protected","internal","hidden","exported"}
            FieldVisibility:Add(kw)
        NEXT
    END CONSTRUCTOR
END CLASS

// -- entry point ------------------------------------------------------------
FUNCTION Start() AS VOID STRICT
    LOCAL cFolder  AS STRING
    LOCAL cXmlFile AS STRING
    LOCAL cXmlPath AS STRING
    VAR args := Environment.GetCommandLineArgs()
    // args[0] = exe, args[1..3] = our arguments
    IF args:Length < 4
        cFolder  := "e:\XSharp\Dev\src\Runtime\XSharp.RT"
        cXmlFile := "XSharp.RT.Docs.xml"
        cXmlPath := "e:\XSharp\Dev\src\Docs\"+cXmlFile
    ELSE
        cFolder  := args[1]
        cXmlFile := args[2]
        cXmlPath := args[3]
    ENDIF
    ExtractDocs(cFolder, cXmlFile, cXmlPath)
    Console.ReadLine()

FUNCTION ExtractDocs(cFolder AS STRING, cXmlFileName AS STRING, cXmlFilePath AS STRING) AS VOID
    // Ordered dictionary so the XML file preserves source order
    VAR xmlEntries := Dictionary<STRING, List<STRING>>{}
    VAR aFiles := Directory.GetFiles(cFolder, "*.prg", SearchOption.AllDirectories)
    VAR nModified := 0
    FOREACH VAR cFile IN aFiles
        // skip generated designer files
        IF Path.GetFileName(cFile):Contains(".Designer.")
            LOOP
        ENDIF
        IF ProcessFile(cFile, cXmlFileName, xmlEntries)
            ? "  modified: "+cFile
            nModified++
        ENDIF
    NEXT
    ? i"Modified {nModified} files, extracted {xmlEntries:Count} entries."
    IF xmlEntries:Count > 0
        WriteXmlFile(cXmlFilePath, xmlEntries)
        ? "Written: "+cXmlFilePath
    ENDIF

// -- process one .prg file -------------------------------------------------
FUNCTION ProcessFile(cFile AS STRING, cXmlFileName AS STRING, ;
                     xmlEntries AS Dictionary<STRING, List<STRING>>) AS LOGIC
    VAR aLines   := File.ReadAllLines(cFile, Encoding.UTF8)
    VAR result   := List<STRING>{}
    VAR docLines := List<STRING>{}
    // Type-context stack: each item is string[2] {name, kind}
    // kind = "type" for CLASS/INTERFACE/STRUCTURE; "enum" for ENUM
    VAR typeStack := Stack<STRING[]>{}
    VAR lModified := FALSE

    FOREACH VAR cLine IN aLines
        VAR line := cLine:TrimStart()

        // -- collect doc comment lines ------------------------------------
        IF line:StartsWith("///")
            docLines:Add(cLine)
            LOOP
        ENDIF

        // -- non-/// line -------------------------------------------------
        IF docLines:Count > 0
            IF String.IsNullOrWhiteSpace(cLine)
                // blank line breaks the association � flush doc block as-is
                result:Add(cLine)
                LOOP
            ENDIF

            VAR lower := line:ToLower()

            // end-of-type block (END CLASS / END INTERFACE / END STRUCTURE / END ENUM)
            IF IsEndType(lower)
                result:AddRange(docLines)
                docLines:Clear()
                IF typeStack:Count > 0
                    typeStack:Pop()
                ENDIF
                result:Add(cLine)
                LOOP
            ENDIF

            // BEGIN/END NAMESPACE � pass through without touching the type stack
            IF IsNamespaceLine(lower)
                result:AddRange(docLines)
                docLines:Clear()
                result:Add(cLine)
                LOOP
            ENDIF

            // detect the entity that follows the doc block
            LOCAL entityName AS STRING
            LOCAL entityKind AS STRING
            LOCAL inEnum := typeStack:Count > 0 .AND. typeStack:Peek()[1] == "enum" AS LOGIC
            LOCAL inType := typeStack:Count > 0 AS LOGIC
            GetEntity(line, inEnum, inType, OUT entityName, OUT entityKind)

            IF !String.IsNullOrEmpty(entityName) .AND. ShouldExtract(docLines)
                // build the XML path key
                LOCAL pathKey AS STRING
                IF (entityKind == "member" .OR. entityKind == "enum_member" .OR. entityKind == "field") ;
                   .AND. typeStack:Count > 0
                    pathKey := typeStack:Peek()[0] + "." + entityName
                ELSE
                    pathKey := entityName
                ENDIF
                pathKey := UniqueKey(pathKey, xmlEntries)

                // store the extracted content
                xmlEntries[pathKey] := ExtractContent(docLines)

                // preserve the indentation of the first doc line
                VAR indentLen := cLine:Length - cLine:TrimStart():Length
                VAR indent    := STRING{' ', indentLen}

                result:Add(indent+"/// <include file="""+cXmlFileName+""" path=""doc/"+pathKey+"/*"" />")
                docLines:Clear()
                lModified := TRUE
            ELSE
                // not extractable � keep the doc block in place
                result:AddRange(docLines)
                docLines:Clear()
            ENDIF

            // update the type-context stack for this non-doc line
            IF entityKind == "type" .OR. entityKind == "enum"
                IF !String.IsNullOrEmpty(entityName)
                    typeStack:Push(<STRING>{entityName, entityKind})
                ENDIF
            ELSEIF entityKind == "end_type" .AND. typeStack:Count > 0
                typeStack:Pop()
            ENDIF

        ELSE
            // no pending doc lines � still need to track type context
            VAR lower := line:ToLower()
            IF IsEndType(lower)
                IF typeStack:Count > 0
                    typeStack:Pop()
                ENDIF
            ELSEIF !IsNamespaceLine(lower)
                LOCAL entityName2 AS STRING
                LOCAL entityKind2 AS STRING
                LOCAL inEnum2 := typeStack:Count > 0 .AND. typeStack:Peek()[1] == "enum" AS LOGIC
                LOCAL inType2 := typeStack:Count > 0 AS LOGIC
                GetEntity(line, inEnum2, inType2, OUT entityName2, OUT entityKind2)
                IF (entityKind2 == "type" .OR. entityKind2 == "enum") .AND. !String.IsNullOrEmpty(entityName2)
                    typeStack:Push(<STRING>{entityName2, entityKind2})
                ENDIF
            ENDIF
        ENDIF

        result:Add(cLine)
    NEXT

    // flush any trailing doc lines
    result:AddRange(docLines)

    IF !lModified
        RETURN FALSE
    ENDIF
    File.WriteAllLines(cFile, result, Encoding.UTF8)
    RETURN TRUE

// -- entity detection ------------------------------------------------------
PROCEDURE GetEntity(cLine AS STRING, lInEnum AS LOGIC, lInType AS LOGIC, ;
                    entityName OUT STRING, entityKind OUT STRING)
    entityName := ""
    entityKind := ""
    VAR stripped := cLine:Trim()
    IF String.IsNullOrEmpty(stripped) .OR. stripped:StartsWith("//") .OR. stripped:StartsWith("#")
        RETURN
    ENDIF
    // Split on whitespace, '(', ',', ';', ':' and similar separators
    VAR raw := Regex.Split(stripped, "[\s\t(,;:]+")
    VAR orig := List<STRING>{}
    FOREACH VAR t IN raw
        IF !String.IsNullOrEmpty(t)
            orig:Add(t)
        ENDIF
    NEXT
    IF orig:Count == 0
        RETURN
    ENDIF
    // skip leading modifier keywords
    VAR i := 0
    DO WHILE i < orig:Count .AND. XSharpKeywords.Modifiers:Contains(orig[i])
        i++
    ENDDO
    IF i >= orig:Count
        RETURN
    ENDIF
    VAR kw   := orig[i]:ToLower()
    VAR rest := i + 1 < orig:Count

    // ---- scoped type declarations (CLASS, INTERFACE, STRUCTURE, ENUM) ----
    IF XSharpKeywords.TypeKeywords:Contains(kw)
        IF rest
            // fully-qualified names: take the last component only
            VAR raw2 := orig[i+1]
            VAR dot := raw2:LastIndexOf('.')
            raw2 := IIF(dot >= 0, raw2:Substring(dot+1), raw2)
            entityName := SanitizeName(raw2)
            entityKind := IIF(kw == "enum", "enum", "type")
        ENDIF
        RETURN
    ENDIF

    // ---- member declarations (METHOD, PROPERTY, DELEGATE, �) ------------
    IF XSharpKeywords.MemberKeywords:Contains(kw)
        IF rest
            entityName := SanitizeName(orig[i+1])
            entityKind := "member"
        ENDIF
        RETURN
    ENDIF

    // ---- global declarations (FUNCTION, PROCEDURE) -----------------------
    IF XSharpKeywords.GlobalKeywords:Contains(kw)
        IF rest
            entityName := SanitizeName(orig[i+1])
            entityKind := "global"
        ENDIF
        RETURN
    ENDIF

    // ---- CONSTRUCTOR / DESTRUCTOR ----------------------------------------
    IF kw == "constructor"
        entityName := "ctor"
        entityKind := "member"
        RETURN
    ENDIF
    IF kw == "destructor"
        entityName := "dtor"
        entityKind := "member"
        RETURN
    ENDIF

    // ---- MEMBER keyword inside an enum -----------------------------------
    IF kw == "member" .AND. lInEnum .AND. rest
        entityName := SanitizeName(orig[i+1])
        entityKind := "enum_member"
        RETURN
    ENDIF

    // ---- class field: visibility modifier + identifier + AS --------------
    IF lInType .AND. !XSharpKeywords.Modifiers:Contains(kw) .AND. orig:Count > 0
        IF XSharpKeywords.FieldVisibility:Contains(orig[0]:ToLower())
            IF i+1 < orig:Count .AND. orig[i+1]:ToLower() == "as"
                entityName := SanitizeName(orig[i])
                entityKind := "field"
            ENDIF
        ENDIF
    ENDIF

// -- helper predicates ----------------------------------------------------
FUNCTION IsEndType(lower AS STRING) AS LOGIC
    RETURN Regex.IsMatch(lower, "^end\s+(class|interface|structure|struct|enum)\b")

FUNCTION IsNamespaceLine(lower AS STRING) AS LOGIC
    RETURN Regex.IsMatch(lower, "^(begin|end)\s+namespace\b")

// -- should this doc block be extracted? ----------------------------------
FUNCTION ShouldExtract(docLines AS List<STRING>) AS LOGIC
    IF docLines:Count == 0
        RETURN FALSE
    ENDIF
    // Count # of lines that we do not process
      VAR nDoNotProcess := 0
      FOREACH VAR line IN docLines
        VAR content := line:Trim():TrimStart({'/'})
        content := content:Trim()
        IF content:StartsWith("<exclude") .OR. content:StartsWith("<include ") .OR. content:StartsWith("<inheritdoc")
            nDoNotProcess++
        ENDIF
      NEXT
      IF nDoNotProcess  == docLines:Count
         RETURN FALSE
      ENDIF
    // single-line: leave <exclude .../>, <include .../>, and <inheritdoc .../> in place
    IF docLines:Count == 1
        VAR content := docLines[0]:Trim():TrimStart({'/'})
        content := content:TrimStart({'/'})
        content := content:Trim()
        IF content:StartsWith("<exclude") .OR. content:StartsWith("<include ") .OR. content:StartsWith("<inheritdoc")
            RETURN FALSE
        ENDIF
    ENDIF
    // reject orphaned closing-tag fragments
    // (happens when a '//' line mid-block broke the doc collection)
    FOREACH VAR raw IN docLines
        VAR content := raw:Trim():TrimStart({'/'})
        content := content:TrimStart({'/'})
        content := content:Trim()
        IF content:Length > 0
            IF content:StartsWith("</")
                RETURN FALSE
            ENDIF
            EXIT
        ENDIF
    NEXT
    RETURN TRUE

// -- extract body text (strips the /// prefix from each line) -------------
FUNCTION ExtractContent(docLines AS List<STRING>) AS List<STRING>
    VAR result := List<STRING>{}
    FOREACH VAR cLine IN docLines
        VAR idx := cLine:IndexOf("///")
        IF idx >= 0
            VAR body := cLine:Substring(idx + 3)
            IF body:StartsWith(" ")
                body := body:Substring(1)
            ENDIF
            result:Add(body:TrimEnd())
        ELSE
            result:Add(cLine:TrimEnd())
        ENDIF
    NEXT
    RETURN result

// -- sanitize an entity name for use as an XML element name ---------------
FUNCTION SanitizeName(name AS STRING) AS STRING
    // X# uses @@ to escape keywords (e.g. @@Value means the 'Value' property)
    IF !String.IsNullOrEmpty(name) .AND. name:StartsWith("@@")
        name := name:Substring(2)
    ENDIF
    // Replace characters not valid in XML names with underscore
    name := Regex.Replace(name, "[^A-Za-z0-9_.\-]", "_")
    // XML names cannot start with a digit or hyphen
    IF !String.IsNullOrEmpty(name) .AND. (Char.IsDigit(name[0]) .OR. name[0] == '-')
        name := "_" + name
    ENDIF
    RETURN name

// -- return a key that is unique in xmlEntries -----------------------------
FUNCTION UniqueKey(baseKey AS STRING, xmlEntries AS Dictionary<STRING, List<STRING>>) AS STRING
    IF !xmlEntries:ContainsKey(baseKey)
        RETURN baseKey
    ENDIF
    VAR n := 2
    DO WHILE xmlEntries:ContainsKey(baseKey+"_"+n:ToString())
        n++
    ENDDO
    RETURN baseKey+"_"+n:ToString()

// -- write the collected entries to the XML file ---------------------------
FUNCTION WriteXmlFile(cPath AS STRING, xmlEntries AS Dictionary<STRING, List<STRING>>) AS VOID
    VAR sb := StringBuilder{}
    sb:AppendLine("<?xml version=""1.0"" encoding=""utf-8""?>")
    sb:AppendLine("<doc>")
    FOREACH VAR pair IN xmlEntries
        sb:AppendLine("    <"+pair:Key+">")
        FOREACH VAR cLine IN pair:Value
            sb:AppendLine("        "+cLine)
        NEXT
        sb:AppendLine("    </"+pair:Key+">")
    NEXT
    sb:AppendLine("</doc>")
    VAR dir := Path.GetDirectoryName(cPath)
    IF !String.IsNullOrEmpty(dir) .AND. !Directory.Exists(dir)
        Directory.CreateDirectory(dir)
    ENDIF
    File.WriteAllText(cPath, sb:ToString(), Encoding.UTF8)
