// TemplateHelper.prg
// Helper class for template placeholder validation and replacement
// Provides static methods for all classes to use template validation

USING System
USING System.Collections.Generic
USING System.Text
USING System.IO
USING System.Text.RegularExpressions
USING System.Linq
USING Serilog

BEGIN NAMESPACE VFPXPorterLib

    /// <summary>
    /// Static helper class for template placeholder validation and replacement.
    /// Provides reusable methods for any class that needs to process templates with placeholders.
    /// </summary>
    STATIC CLASS TemplateHelper

        /// <summary>
        /// Extracts all placeholder names from a template string.
        /// Placeholders are expected to be in the format: <@placeholderName@>
        /// </summary>
        /// <param name="template">The template string to extract placeholders from</param>
        /// <returns>HashSet containing all unique placeholder names (without delimiters)</returns>
        PUBLIC STATIC METHOD ExtractPlaceholders(template AS STRING) AS HashSet<STRING>
            VAR placeholders := HashSet<STRING>{}
            IF String.IsNullOrEmpty(template)
                XPorterLogger.Instance:Verbose("ExtractPlaceholders: Template is empty")
                RETURN placeholders
            ENDIF
            VAR regex := Regex{"<@(\w+)@>"}
            FOREACH match AS Match IN regex:Matches(template)
                IF match:Groups:Count > 1
                    placeholders:Add(match:Groups[1]:Value)
                ENDIF
            NEXT
            XPorterLogger.Instance:Verbose("ExtractPlaceholders: Found " + placeholders:Count:ToString() + " placeholders: " + ;
                String.Join(", ", placeholders:Select({p => "<@" + p + "@>"})))
            RETURN placeholders

        /// <summary>
        /// Validates that all required placeholders exist in a template.
        /// </summary>
        /// <param name="template">The template string to validate</param>
        /// <param name="templateName">Name of template for error messages</param>
        /// <param name="requiredPlaceholders">Collection of required placeholder names</param>
        /// <throws>Exception if any required placeholder is missing</throws>
        PUBLIC STATIC METHOD ValidateTemplate(template AS STRING, templateName AS STRING, ;
            requiredPlaceholders AS IEnumerable<STRING>) AS VOID

            IF String.IsNullOrEmpty(template)
                XPorterLogger.Instance:Error("ValidateTemplate '" + templateName + "': Template is empty")
                THROW Exception{"Template '" + templateName + "' is empty"}
            ENDIF

            XPorterLogger.Instance:Verbose("ValidateTemplate '" + templateName + "': Validating " + ;
                requiredPlaceholders:Count():ToString() + " required placeholders")

            VAR missing := List<STRING>{}
            FOREACH VAR placeholder IN requiredPlaceholders
                VAR marker := "<@" + placeholder + "@>"
                IF !template:Contains(marker)
                    missing:Add(placeholder)
                ENDIF
            NEXT

            IF missing:Count > 0
                VAR missingText := String.Join(", ", missing:Select( { p => "<@" + p + "@>" } ):ToList())
                XPorterLogger.Instance:Error("ValidateTemplate '" + templateName + "': Missing placeholders: " + missingText)
                THROW Exception{"Template '" + templateName + "' is missing required placeholders: " + missingText}
            ENDIF

            XPorterLogger.Instance:Verbose("ValidateTemplate '" + templateName + "': All required placeholders present")

        /// <summary>
        /// Replaces all placeholders in a template, in a single pass.
        /// - Placeholder names are matched case-insensitively against the replacement keys.
        /// - A placeholder with no replacement (typo, or a name unknown to the exporter) is logged
        ///   as a Warning and replaced by an empty string: the export goes on.
        /// - Replacement values are inserted verbatim and never re-scanned, so generated/user code
        ///   that happens to contain "&lt;@" cannot break the substitution.
        /// - Replacement keys not used by the template are ignored (a customised template may drop them).
        /// </summary>
        /// <param name="template">The template string with placeholders</param>
        /// <param name="templateName">Name of template for log messages</param>
        /// <param name="replacements">Dictionary of placeholder->replacement pairs (without the <@ @> delimiters)</param>
        /// <returns>Template string with all placeholders replaced</returns>
        PUBLIC STATIC METHOD ReplaceAndValidate(template AS STRING, templateName AS STRING, ;
            replacements AS Dictionary<STRING, STRING>) AS STRING

            IF String.IsNullOrEmpty(template)
                XPorterLogger.Instance:Verbose("ReplaceAndValidate '" + templateName + "': Template is empty, returning as-is")
                RETURN template
            ENDIF

            XPorterLogger.Instance:Information("ReplaceAndValidate '" + templateName + "': Starting replacement with " + ;
                replacements:Count:ToString() + " placeholders")

            // Case-insensitive view of the replacements
            VAR lookup := Dictionary<STRING, STRING>{ StringComparer.OrdinalIgnoreCase }
            FOREACH VAR kvp IN replacements
                lookup[kvp:Key] := kvp:Value
            NEXT

            VAR unknown := HashSet<STRING>{ StringComparer.OrdinalIgnoreCase }
            VAR result := StringBuilder{ template:Length }
            VAR lastPos := 0
            VAR regex := Regex{"<@(\w+)@>"}
            FOREACH match AS Match IN regex:Matches(template)
                // Copy the template text before this placeholder
                result:Append( template, lastPos, match:Index - lastPos )
                VAR name := match:Groups[1]:Value
                LOCAL value AS STRING
                IF lookup:TryGetValue( name, OUT value )
                    result:Append( IIF( value == NULL, "", value ) )
                ELSE
                    unknown:Add( name )
                ENDIF
                lastPos := match:Index + match:Length
            NEXT
            result:Append( template, lastPos, template:Length - lastPos )

            IF unknown:Count > 0
                VAR unknownText := String.Join(", ", unknown:Select({p => "<@" + p + "@>"}):ToList())
                XPorterLogger.Instance:Warning("ReplaceAndValidate '" + templateName + "': Unknown placeholders replaced by empty text: " + ;
                    unknownText + ". Check the template for a typo.")
            ENDIF

            XPorterLogger.Instance:Information("ReplaceAndValidate '" + templateName + "': Successfully completed")
            RETURN result:ToString()

    END CLASS

END NAMESPACE // VFPXPorterLib
