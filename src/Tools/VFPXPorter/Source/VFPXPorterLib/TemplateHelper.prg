// TemplateHelper.prg
// Helper class for template placeholder validation and replacement
// Provides static methods for all classes to use template validation

USING System
USING System.Collections.Generic
USING System.Text
USING System.IO
USING System.Text.RegularExpressions
USING System.Linq

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
                RETURN placeholders
            ENDIF
            VAR regex := Regex{"<@(\w+)@>"}
            FOREACH match AS Match IN regex:Matches(template)
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
        PUBLIC STATIC METHOD ValidateTemplate(template AS STRING, templateName AS STRING, ;
            requiredPlaceholders AS IEnumerable<STRING>) AS VOID
            
            IF String.IsNullOrEmpty(template)
                THROW Exception{"Template '" + templateName + "' is empty"}
            ENDIF
            
            VAR missing := List<STRING>{}
            FOREACH VAR placeholder IN requiredPlaceholders
                VAR marker := "<@" + placeholder + "@>"
                IF !template:Contains(marker)
                    missing:Add(placeholder)
                ENDIF
            NEXT
            
            IF missing:Count > 0
                THROW Exception{"Template '" + templateName + "' is missing required placeholders: " + ;
                    String.Join(", ", missing:Select( { p => "<@" + p + "@>" } ):ToList())}
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
        PUBLIC STATIC METHOD ReplaceAndValidate(template AS STRING, templateName AS STRING, ;
            replacements AS Dictionary<STRING, STRING>) AS STRING
            
            IF String.IsNullOrEmpty(template)
                RETURN template
            ENDIF
            
            // Extract all placeholders in the template
            VAR foundPlaceholders := TemplateHelper.ExtractPlaceholders(template)
            
            // Check for unrequested placeholders (likely typos in template or code)
            VAR unrequested := HashSet<STRING>{}
            FOREACH VAR placeholder IN foundPlaceholders
                IF !replacements:ContainsKey(placeholder)
                    unrequested:Add(placeholder)
                ENDIF
            NEXT
            
            IF unrequested:Count > 0
                THROW Exception{"Template '" + templateName + "' contains unexpected placeholders not provided: " + ;
                    String.Join(", ", unrequested:Select( {p => "<@" + p + "@>"}):ToList()) + ;
                    ". This may indicate a typo in the template or missing replacement data."}
            ENDIF
            
            // Perform replacements
            VAR result := template
            FOREACH VAR kvp IN replacements
                VAR placeholder := "<@" + kvp:Key + "@>"
                result := result:Replace(placeholder, kvp:Value)
            NEXT
            
            // Final check: ensure no unreplaced placeholders remain
            IF result:Contains("<@")
                VAR remaining := TemplateHelper.ExtractPlaceholders(result)
                THROW Exception{"Template '" + templateName + "' still contains unreplaced placeholders: " + ;
                    String.Join(", ", remaining:Select({p => "<@" + p + "@>"}):ToList()) + ;
                    ". This indicates a replacement value was null or empty, which should not occur."}
            ENDIF
            
            RETURN result

    END CLASS

END NAMESPACE // VFPXPorterLib
