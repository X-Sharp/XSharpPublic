// VFPXPorter.prg
// Created by    : fabri
// Creation Date : 11/16/2020 10:39:27 AM
// Created for   :
// WorkStation   : FABPORTABLE


USING System
USING System.Collections.Generic
USING System.Text
USING System.IO
USING System.Text.RegularExpressions
USING Newtonsoft.Json
USING System.Xml.Serialization
USING System.ComponentModel
USING Serilog
USING Serilog.Core

BEGIN NAMESPACE VFPXPorterLib

    /// <summary>
    /// The VFPXPorter class, base class for VFPXPorterVisualBase
    /// </summary>
    ABSTRACT CLASS XPorter IMPLEMENTS IXPorter

        PROPERTY Name AS STRING GET Path.GetFileNameWithoutExtension( SELF:Settings:ItemsPath )

        PROPERTY FileName AS STRING GET Path.GetFileName( SELF:Settings:ItemsPath )

        PROPERTY Settings AS XPorterSettings AUTO

        CONSTRUCTOR( )
            RETURN

        METHOD ClearResultText AS VOID STRICT
            BufferedSink.Instance:Clear()
            RETURN

        PROPERTY ResultText AS STRING
            GET
                VAR log := BufferedSink.Instance:Log
                BufferedSink.Instance:Clear()
                RETURN log
            END GET
        END PROPERTY

        PROPERTY ErrorText AS STRING
            GET
                VAR log := BufferedSink.Instance:Errors
                RETURN log
            END GET
        END PROPERTY

        VIRTUAL METHOD Initialize( filePath AS STRING, destPath AS STRING, settings AS XPorterSettings  ) AS VOID
            //
            SELF:Settings :=settings
            SELF:Settings:ItemsPath := filePath
            SELF:Settings:OutputPath := destPath
            //
            IF !Directory.Exists( SELF:Settings:OutputPath )
                // Let's try to create the destination Path
                Directory.CreateDirectory( SELF:Settings:OutputPath )
            ENDIF
            RETURN

        ABSTRACT METHOD Analyze( doBackup AS LOGIC ) AS LOGIC

        ABSTRACT METHOD Export( doBackup AS LOGIC ) AS LOGIC


#region Background Worker

            // The background task, if any, that will process the Export process
        PROTECTED _worker AS BackgroundWorker
        PROPERTY Worker AS BackgroundWorker
            SET
                SELF:_worker := VALUE
            END SET
        END PROPERTY
        // The total number of items to process
        PROTECTED _totalItems AS INT
        PROTECTED _progress AS INT

        PROPERTY Canceled AS LOGIC GET IIF( _worker != NULL, _worker:CancellationPending, FALSE )

        METHOD ResetProgress( total AS INT ) AS VOID
            SELF:_progress := 0
            SELF:_totalItems := total

        METHOD UpdateProgress() AS VOID
            IF ( SELF:_worker != NULL )
                SELF:_progress ++
                VAR percentComplete := (INT)Math.Round((REAL8)(100 * _progress) / _totalItems)
                IF percentComplete > 100
                    percentComplete := 100
                ENDIF
                SELF:_worker:ReportProgress(percentComplete)
            ENDIF


#endregion

        /// <summary>
        /// Extracts all placeholder names from a template string.
        /// Placeholders are expected to be in the format: <@placeholderName@>
        /// </summary>
        /// <param name="template">The template string to extract placeholders from</param>
        /// <returns>HashSet containing all unique placeholder names (without delimiters)</returns>
        PROTECTED METHOD ExtractPlaceholders(template AS STRING) AS HashSet<STRING>
            LOCAL placeholders := HashSet<STRING>{}
            IF String.IsNullOrEmpty(template)
                RETURN placeholders
            ENDIF
            LOCAL regex := Regex{"<@(\w+)@>"}
            FOREACH VAR match IN regex:Matches(template)
                IF match:Groups:Count > 1
                    placeholders:Add(match:Groups[1]:Value)
                ENDIF
            NEXT
            RETURN placeholders

        /// <summary>
        /// Validates that all required placeholders exist in a template.
        /// </summary>
        /// <param name="template">The template string to validate</param>
        /// <param name="templateName">Name of template for error messages</param>
        /// <param name="requiredPlaceholders">Collection of required placeholder names</param>
        /// <throws>Exception if any required placeholder is missing</throws>
        PROTECTED METHOD ValidateTemplate(template AS STRING, templateName AS STRING, ;
            requiredPlaceholders AS IEnumerable<STRING>) AS VOID
            
            IF String.IsNullOrEmpty(template)
                THROW Exception{"Template '" + templateName + "' is empty"}
            ENDIF
            
            LOCAL missing := List<STRING>{}
            FOREACH VAR placeholder IN requiredPlaceholders
                LOCAL marker := "<@" + placeholder + "@>"
                IF !template:Contains(marker)
                    missing:Add(placeholder)
                ENDIF
            NEXT
            
            IF missing:Count > 0
                THROW Exception{"Template '" + templateName + "' is missing required placeholders: " + ;
                    String.Join(", ", missing:Select(p => "<@" + p + "@>"):ToList())}
            ENDIF

        /// <summary>
        /// Replaces all placeholders in a template and validates that all placeholders were replaced.
        /// Throws an exception if the template contains unreplaced placeholders or has placeholders not in the replacements dictionary.
        /// </summary>
        /// <param name="template">The template string with placeholders</param>
        /// <param name="templateName">Name of template for error messages</param>
        /// <param name="replacements">Dictionary of placeholder->replacement pairs (without the <@ @> delimiters)</param>
        /// <returns>Template string with all placeholders replaced</returns>
        /// <throws>Exception if validation fails</throws>
        PROTECTED METHOD ReplaceAndValidate(template AS STRING, templateName AS STRING, ;
            replacements AS Dictionary<STRING, STRING>) AS STRING
            
            IF String.IsNullOrEmpty(template)
                RETURN template
            ENDIF
            
            // Extract all placeholders in the template
            LOCAL foundPlaceholders := SELF:ExtractPlaceholders(template)
            
            // Check for unrequested placeholders (likely typos in template or code)
            LOCAL unrequested := HashSet<STRING>{}
            FOREACH VAR placeholder IN foundPlaceholders
                IF !replacements:ContainsKey(placeholder)
                    unrequested:Add(placeholder)
                ENDIF
            NEXT
            
            IF unrequested:Count > 0
                THROW Exception{"Template '" + templateName + "' contains unexpected placeholders not provided: " + ;
                    String.Join(", ", unrequested:Select(p => "<@" + p + "@>"):ToList()) + ;
                    ". This may indicate a typo in the template or missing replacement data."}
            ENDIF
            
            // Perform replacements
            LOCAL result := template
            FOREACH VAR kvp IN replacements
                LOCAL placeholder := "<@" + kvp:Key + "@>"
                result := result:Replace(placeholder, kvp:Value)
            NEXT
            
            // Final check: ensure no unreplaced placeholders remain
            IF result:Contains("<@")
                LOCAL remaining := SELF:ExtractPlaceholders(result)
                THROW Exception{"Template '" + templateName + "' still contains unreplaced placeholders: " + ;
                    String.Join(", ", remaining:Select(p => "<@" + p + "@>"):ToList()) + ;
                    ". This indicates a replacement value was null or empty, which should not occur."}
            ENDIF
            
            RETURN result


    END CLASS
END NAMESPACE // VFPXPorterLib
