
using System.Text.Json


Class LowerCaseNamingPolicy INHERIT JsonNamingPolicy

    public override METHOD ConvertName( name AS STRING ) AS STRING
        return name:ToLowerInvariant()
    END METHOD

END CLASS
