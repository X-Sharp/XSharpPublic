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
                XPorterLogger.Instance:Verbose("ReplaceAndValidate '" + templateName + "': Template is empty, returning as-is")
                RETURN template
            ENDIF
            
            XPorterLogger.Instance:Information("ReplaceAndValidate '" + templateName + "': Starting replacement with " + ;
                replacements:Count:ToString() + " placeholders")
            
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
                VAR unrequestedText := String.Join(", ", unrequested:Select( {p => "<@" + p + "@>"}):ToList())
                XPorterLogger.Instance:Error("ReplaceAndValidate '" + templateName + "': Unexpected placeholders: " + unrequestedText)
                THROW Exception{"Template '" + templateName + "' contains unexpected placeholders not provided: " + ;
                    unrequestedText + ;
                    ". This may indicate a typo in the template or missing replacement data."}
            ENDIF
            
            // Perform replacements
            VAR result := template
            FOREACH VAR kvp IN replacements
                VAR placeholder := "<@" + kvp:Key + "@>"
                result := result:Replace(placeholder, kvp:Value)
                XPorterLogger.Instance:Verbose("ReplaceAndValidate '" + templateName + "': Replaced <@" + kvp:Key + "@>")
            NEXT
            
            // Final check: ensure no unreplaced placeholders remain
            IF result:Contains("<@")
                VAR remaining := TemplateHelper.ExtractPlaceholders(result)
                VAR remainingText := String.Join(", ", remaining:Select({p => "<@" + p + "@>"}):ToList())
                XPorterLogger.Instance:Error("ReplaceAndValidate '" + templateName + "': Unreplaced placeholders remain: " + remainingText)
                THROW Exception{"Template '" + templateName + "' still contains unreplaced placeholders: " + ;
                    remainingText + ;
                    ". This indicates a replacement value was null or empty, which should not occur."}
            ENDIF
            
            XPorterLogger.Instance:Information("ReplaceAndValidate '" + templateName + "': Successfully completed")
            RETURN result

    END CLASS

END NAMESPACE // VFPXPorterLib
