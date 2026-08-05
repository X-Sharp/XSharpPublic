// DbcFieldCache.prg
// Reads extended field properties (Caption, InputMask, Format) from VFP .dbc files
// and injects them into control PropertiesDict before ConvertProperties runs, so
// that controls bound to DBC-defined fields pick up masks/formats the SCX omits.

USING System
USING System.Collections.Generic
USING System.IO
USING XSharp.RDD

BEGIN NAMESPACE VFPXPorterLib

/// <summary>
/// Cache of per-field extended properties read from VFP Database Container files.
/// Keys are "alias.fieldname" (case-insensitive); values are property name → quoted value
/// in the same double-quoted format that the SCX PROPERTIES blob uses, so they are
/// compatible with ApplyPropertiesRules without any special handling.
/// </summary>
CLASS DbcFieldCache

    // "alias.fieldname" → { "Caption"/"InputMask"/"Format" → quoted string value }
    PRIVATE _cache AS Dictionary<STRING, Dictionary<STRING, STRING>>
    PRIVATE _sourcePath AS STRING

    /// <summary>
    /// Build the cache from the DataEnvironment item of a form being exported.
    /// sourcePath is the directory of the .scx file (used to resolve relative DBC paths).
    /// </summary>
    CONSTRUCTOR(sourcePath AS STRING, dataEnvItem AS SCXVCXItem)
        _cache      := Dictionary<STRING, Dictionary<STRING, STRING>>{StringComparer.OrdinalIgnoreCase}
        _sourcePath := sourcePath
        SELF:_LoadFromDataEnvironment(dataEnvItem)

    PROPERTY IsEmpty AS LOGIC GET _cache:Count == 0

    PRIVATE METHOD _LoadFromDataEnvironment(dataEnvItem AS SCXVCXItem) AS VOID
        IF dataEnvItem == NULL
            RETURN
        ENDIF
        FOREACH VAR child IN dataEnvItem:Childs
            VAR cursor := child ASTYPE SCXVCXItem
            IF cursor == NULL .OR. String.Compare(cursor:BaseClassName, "cursor", TRUE) != 0
                LOOP
            ENDIF
            SELF:_LoadCursor(cursor)
        NEXT

    PRIVATE METHOD _LoadCursor(cursor AS SCXVCXItem) AS VOID
        LOCAL dbPath       AS STRING
        LOCAL alias        AS STRING
        LOCAL cursorSource AS STRING
        // Properties in the SCX blob are stored as quoted strings (e.g. Database = "path\data.dbc")
        cursor:PropertiesDict:TryGetValue("Database",     OUT dbPath)
        cursor:PropertiesDict:TryGetValue("Alias",        OUT alias)
        cursor:PropertiesDict:TryGetValue("CursorSource", OUT cursorSource)
        IF String.IsNullOrEmpty(dbPath) .OR. String.IsNullOrEmpty(cursorSource)
            RETURN  // not a DBC-bound cursor
        ENDIF
        // Strip surrounding quotes added by the SCX PROPERTIES serialiser
        dbPath       := _Unquote(dbPath)
        alias        := _Unquote(alias)
        cursorSource := _Unquote(cursorSource)
        IF String.IsNullOrEmpty(dbPath)
            RETURN
        ENDIF
        // Resolve relative path against the SCX source folder
        IF !Path.IsPathRooted(dbPath)
            dbPath := Path.GetFullPath(Path.Combine(_sourcePath, dbPath))
        ENDIF
        IF !File.Exists(dbPath)
            RETURN
        ENDIF
        // Default alias to the table filename when not set explicitly
        IF String.IsNullOrEmpty(alias)
            alias := Path.GetFileNameWithoutExtension(cursorSource)
        ENDIF
        TRY
            // Open is a no-op if the DBC is already open; FindDatabase still works
            Dbc.Open(dbPath, TRUE, TRUE, FALSE)
            VAR oDb := Dbc.FindDatabase(dbPath)
            IF oDb == NULL
                RETURN
            ENDIF
            VAR tableName := Path.GetFileNameWithoutExtension(cursorSource)
            VAR oTable := oDb:FindTable(tableName)
            IF oTable == NULL
                RETURN
            ENDIF
            FOREACH VAR oField IN oTable:Fields
                VAR props := Dictionary<STRING, STRING>{StringComparer.OrdinalIgnoreCase}
                _TryAddQuoted(props, "Caption",   oField:Properties:GetValue<STRING>(DatabasePropertyType.Caption))
                _TryAddQuoted(props, "InputMask", oField:Properties:GetValue<STRING>(DatabasePropertyType.InputMask))
                _TryAddQuoted(props, "Format",    oField:Properties:GetValue<STRING>(DatabasePropertyType.Format))
                IF props:Count > 0
                    _cache[alias + "." + oField:ObjectName] := props
                ENDIF
            NEXT
        CATCH e AS Exception
            // Non-fatal: if the DBC can't be read, export continues without DBC properties
            XPorterLogger.Instance:Warning("DbcFieldCache: cannot read '" + dbPath + "': " + e:Message)
        END TRY

    /// <summary>
    /// Inject DBC-derived properties into a control's PropertiesDict, but only for
    /// properties not already defined in the SCX (SCX always wins).
    /// Call this before ConvertProperties so the injected values go through normal conversion.
    /// </summary>
    METHOD InjectMissingProperties(control AS SCXVCXItem) AS VOID
        IF SELF:IsEmpty
            RETURN
        ENDIF
        LOCAL controlSource AS STRING
        IF !control:PropertiesDict:TryGetValue("ControlSource", OUT controlSource)
            RETURN
        ENDIF
        // ControlSource is also quoted in the SCX blob
        controlSource := _Unquote(controlSource)
        IF String.IsNullOrEmpty(controlSource) .OR. !controlSource:Contains(".")
            RETURN  // no alias prefix — can't resolve field unambiguously
        ENDIF
        LOCAL props AS Dictionary<STRING, STRING>
        IF !_cache:TryGetValue(controlSource, OUT props)
            RETURN
        ENDIF
        FOREACH VAR kv IN props
            IF !control:PropertiesDict:ContainsKey(kv:Key)
                control:PropertiesDict:Add(kv:Key, kv:Value)
            ENDIF
        NEXT

    // Strip surrounding double-quotes from a value read out of the SCX PROPERTIES blob.
    PRIVATE STATIC METHOD _Unquote(value AS STRING) AS STRING
        IF String.IsNullOrEmpty(value)
            RETURN value
        ENDIF
        value := value:Trim()
        IF value:Length >= 2 .AND. value[0] == '"' .AND. value[value:Length - 1] == '"'
            RETURN value:Substring(1, value:Length - 2)
        ENDIF
        RETURN value

    // Wrap a non-empty raw DBC string value in double-quotes to match the SCX PROPERTIES format.
    // ApplyPropertiesRules emits values verbatim, so they must look like valid X# string literals.
    PRIVATE STATIC METHOD _TryAddQuoted(dict AS Dictionary<STRING,STRING>, propName AS STRING, value AS STRING) AS VOID
        IF !String.IsNullOrEmpty(value)
            dict[propName] := e"\"" + value + e"\""
        ENDIF

END CLASS
END NAMESPACE
