USING System
USING System.Text

BEGIN NAMESPACE XSharp.VFP.UI

    /// <summary>
    /// Formats values according to InputMask pattern
    /// Applies mask formatting, handles partial input, and extracts clean data
    /// </summary>
    PUBLIC CLASS InputMaskFormatter
        
        PRIVATE _validator AS InputMaskValidator
        
        /// <summary>
        /// Constructor
        /// </summary>
        PUBLIC CONSTRUCTOR()
            SELF:_validator := InputMaskValidator{}
        END CONSTRUCTOR
        
        /// <summary>
        /// Format a raw value with the mask.
        /// Iterates <c>pattern.Positions</c> using 0-based indices.
        /// Example: "5551234567" with mask "(999) 999-9999" → "(555) 123-4567"
        /// </summary>
        /// <param name="pattern">Parsed mask pattern.</param>
        /// <param name="inputValue">Raw data string (no mask characters).</param>
        PUBLIC METHOD FormatValue(pattern AS InputMaskPattern, inputValue AS STRING) AS STRING
            IF pattern == NIL .OR. !pattern:IsValid()
                RETURN inputValue
            ENDIF
            
            IF String.IsNullOrEmpty(inputValue)
                RETURN pattern:PlaceholderDisplay
            ENDIF
            
            VAR result := StringBuilder{}
            VAR inputIndex := 0
            
            // Process each position in the mask
            FOR VAR maskIndex := 0 TO pattern:Positions:Count - 1
                VAR maskPos := pattern:Positions[maskIndex]
                
                IF maskPos:Type == "literal"
                    // Insert literal character from mask
                    result:Append(maskPos:Literal)
                    
                ELSEIF maskPos:Type == "modifier"
                    // Skip modifiers - they don't display
                    CONTINUE
                    
                ELSE
                    // Data position - insert character from input if available
                    IF inputIndex < inputValue:Length
                        VAR ch := inputValue:Substring(inputIndex, 1)
                        
                        // Validate and transform character
                        IF SELF:_validator:IsValidCharacter(pattern, maskIndex, ch)
                            VAR transformed := SELF:_validator:TransformCharacter(pattern, maskIndex, ch)
                            result:Append(transformed)
                            inputIndex := inputIndex + 1
                        ELSE
                            // Invalid character - show placeholder instead
                            result:Append('_')
                        ENDIF
                    ELSE
                        // No more input - show placeholder
                        result:Append('_')
                    ENDIF
                ENDIF
            NEXT
            
            RETURN result:ToString()
        END METHOD
        
        /// <summary>
        /// Format with placeholders for empty positions
        /// Same as FormatValue but explicitly shows where data is needed
        /// </summary>
        PUBLIC METHOD FormatWithPlaceholders(pattern AS InputMaskPattern, inputValue AS STRING) AS STRING
            // FormatValue already shows placeholders
            RETURN SELF:FormatValue(pattern, inputValue)
        END METHOD
        
        /// <summary>
        /// Extract clean data without mask formatting.
        /// Iterates <c>maskedValue</c> using 0-based character indices aligned with
        /// <c>pattern.Positions</c>.
        /// Example: "(555) 123-4567" with mask "(999) 999-9999" → "5551234567"
        /// Removes all literals and placeholders, returns only data characters.
        /// </summary>
        PUBLIC METHOD ExtractDataValue(pattern AS InputMaskPattern, maskedValue AS STRING) AS STRING
            IF pattern == NIL .OR. String.IsNullOrEmpty(maskedValue)
                RETURN ""
            ENDIF
            
            VAR result := StringBuilder{}
            
            // Extract only data characters (skip literals, modifiers, placeholders)
            FOR VAR i := 0 TO maskedValue:Length - 1
                IF i >= pattern:Positions:Count
                    CONTINUE
                ENDIF
                
                VAR maskPos := pattern:Positions[i]
                
                // Only extract data position characters
                IF maskPos:Type != "literal" .AND. maskPos:Type != "modifier"
                    VAR ch := maskedValue[i]
                    
                    // Skip placeholder characters
                    IF ch != '_' .AND. ch != ' '
                        result:Append(ch)
                    ENDIF
                ENDIF
            NEXT
            
            RETURN result:ToString()
        END METHOD
        
        /// <summary>
        /// Get just the placeholder template for the mask
        /// Example: "(999) 999-9999" → "(___) ___-____"
        /// </summary>
        PUBLIC METHOD GetPlaceholder(pattern AS InputMaskPattern) AS STRING
            IF pattern == NIL
                RETURN ""
            ENDIF
            RETURN pattern:PlaceholderDisplay
        END METHOD
        
        /// <summary>
        /// Apply mask at a specific position.
        /// Used when inserting a single character.
        /// </summary>
        /// <param name="pattern">Parsed mask pattern.</param>
        /// <param name="position">0-based index into <c>pattern.Positions</c>.</param>
        /// <param name="inputChar">Single-character string to insert.</param>
        /// <returns>Transformed/validated character, or <c>""</c> if not valid at that position.</returns>
        PUBLIC METHOD ApplyMaskAtPosition(pattern AS InputMaskPattern, position AS INT, inputChar AS STRING) AS STRING
            IF pattern == NIL .OR. String.IsNullOrEmpty(inputChar)
                RETURN inputChar
            ENDIF
            
            IF position < 0 .OR. position >= pattern:Positions:Count
                RETURN inputChar
            ENDIF
            
            // Validate the character at this position
            IF SELF:_validator:IsValidCharacter(pattern, position, inputChar)
                RETURN SELF:_validator:TransformCharacter(pattern, position, inputChar)
            ENDIF
            
            RETURN ""
        END METHOD
        
        /// <summary>
        /// Rebuild entire formatted value from current state
        /// Called after edits like delete, backspace, etc.
        /// </summary>
        PUBLIC METHOD RebuildFormattedValue(pattern AS InputMaskPattern, currentText AS STRING) AS STRING
            IF pattern == NIL
                RETURN currentText
            ENDIF
            
            // Extract data and reformat
            VAR cleanData := SELF:ExtractDataValue(pattern, currentText)
            RETURN SELF:FormatValue(pattern, cleanData)
        END METHOD
        
        /// <summary>
        /// Fill in all available data from input
        /// Validates each character and applies mask
        /// </summary>
        PUBLIC METHOD FillAllData(pattern AS InputMaskPattern, inputData AS STRING) AS STRING
            IF pattern == NIL
                RETURN inputData
            ENDIF
            
            IF String.IsNullOrEmpty(inputData)
                RETURN pattern:PlaceholderDisplay
            ENDIF
            
            // Just format the input data with the mask
            RETURN SELF:FormatValue(pattern, inputData)
        END METHOD
        
        /// <summary>
        /// Calculate how many data characters can fit in the mask
        /// </summary>
        PUBLIC METHOD GetMaxDataLength(pattern AS InputMaskPattern) AS INT
            IF pattern == NIL
                RETURN 0
            ENDIF
            RETURN pattern:DataLength
        END METHOD
        
        /// <summary>
        /// Get total length of formatted output
        /// </summary>
        PUBLIC METHOD GetFormattedLength(pattern AS InputMaskPattern) AS INT
            IF pattern == NIL
                RETURN 0
            ENDIF
            RETURN pattern:TotalLength
        END METHOD
        
    END CLASS

END NAMESPACE
