USING System

BEGIN NAMESPACE XSharp.VFP.UI

    /// <summary>
    /// Validates characters against InputMask patterns
    /// Checks if input characters are valid for their mask positions
    /// and applies case transformations (uppercase, lowercase, etc.)
    /// </summary>
    PUBLIC CLASS InputMaskValidator
        
        /// <summary>
        /// Check if character is valid for the mask position
        /// Returns TRUE if character can be placed at this position
        /// </summary>
        PUBLIC METHOD IsValidCharacter(pattern AS InputMaskPattern, position AS INT, character AS STRING) AS LOGIC
            // Validate parameters
            IF pattern == NIL .OR. position < 0 .OR. position >= pattern:Positions:Count
                RETURN FALSE
            ENDIF
            
            IF String.IsNullOrEmpty(character)
                RETURN FALSE
            ENDIF
            
            VAR maskPos := pattern:Positions[position]
            VAR ch := character[1]
            
            SWITCH maskPos:Type
                // Digit required (0-9 only)
                CASE "digit"
                    RETURN Char.IsDigit(ch)
                    
                // Letter required (a-z, A-Z)
                CASE "letter"
                    RETURN Char.IsLetter(ch)
                    
                // Alphanumeric (0-9, a-z, A-Z)
                CASE "alphanumeric"
                    RETURN Char.IsLetterOrDigit(ch)
                    
                // Literals cannot be "typed" - they're auto-inserted
                CASE "literal"
                    RETURN FALSE
                    
                // Modifiers are not input positions
                CASE "modifier"
                    RETURN FALSE
                    
                OTHERWISE
                    RETURN TRUE
            END SWITCH
        END METHOD
        
        /// <summary>
        /// Transform character according to modifiers and pattern rules
        /// Applies case conversions, uppercasing, etc.
        /// </summary>
        PUBLIC METHOD TransformCharacter(pattern AS InputMaskPattern, position AS INT, character AS STRING) AS STRING
            // Validate parameters
            IF String.IsNullOrEmpty(character)
                RETURN ""
            ENDIF
            
            IF pattern == NIL .OR. position < 0 .OR. position >= pattern:Positions:Count
                RETURN character
            ENDIF
            
            VAR result := character
            VAR maskPos := pattern:Positions[position]
            
            // Check for uppercase modifier in current position
            IF maskPos:Modifier == "!"
                result := result:ToUpper()
            ELSE
                // Check if previous position had "!" modifier (applies to following chars)
                IF position > 0
                    VAR prevPos := pattern:Positions[position - 1]
                    IF prevPos:Type == "modifier" .AND. prevPos:Modifier == "!"
                        result := result:ToUpper()
                    ENDIF
                ENDIF
            ENDIF
            
            // For lowercase letter type ('a' in mask = lowercase optional letter), convert to lowercase
            IF maskPos:Type == "letter"
                IF maskPos:OriginalChar == Char.Parse("a")
                    IF Char.IsLetter(character[1])
                        result := result:ToLower()
                    ENDIF
                ENDIF
            ENDIF
            
            // For lowercase alphanumeric type ('x' in mask = lowercase optional alphanumeric), letters are lowercase
            IF maskPos:Type == "alphanumeric"
                IF maskPos:OriginalChar == Char.Parse("x")
                    IF Char.IsLetter(character[1])
                        result := result:ToLower()
                    ENDIF
                ENDIF
            ENDIF
            
            RETURN result
        END METHOD
        
        /// <summary>
        /// Check if character can be inserted at this position
        /// </summary>
        PUBLIC METHOD CanInsertAt(pattern AS InputMaskPattern, position AS INT) AS LOGIC
            IF position < 0 .OR. position >= pattern:Positions:Count
                RETURN FALSE
            ENDIF
            
            VAR maskPos := pattern:Positions[position]
            
            // Cannot insert at literal or modifier positions
            RETURN maskPos:Type != "literal" .AND. maskPos:Type != "modifier"
        END METHOD
        
        /// <summary>
        /// Get the next writable position after the given position
        /// Skips literals and modifiers automatically
        /// </summary>
        PUBLIC METHOD GetNextWritePosition(pattern AS InputMaskPattern, fromPos AS INT) AS INT
            IF pattern == NIL
                RETURN -1
            ENDIF
            
            VAR nextPos := pattern:GetNextDataPosition(fromPos)
            RETURN nextPos
        END METHOD
        
        /// <summary>
        /// Check if all required positions are filled
        /// </summary>
        PUBLIC METHOD IsComplete(pattern AS InputMaskPattern, inputValue AS STRING) AS LOGIC
            IF pattern == NIL .OR. String.IsNullOrEmpty(inputValue)
                RETURN FALSE
            ENDIF
            
            VAR inputIndex := 0
            VAR requiredCount := 0
            VAR filledCount := 0
            
            FOR VAR i := 0 TO pattern:Positions:Count - 1
                VAR maskPos := pattern:Positions[i]
                
                IF maskPos:Type != "literal" .AND. maskPos:Type != "modifier"
                    IF maskPos:Required
                        requiredCount := requiredCount + 1
                        IF inputIndex < inputValue:Length .AND. inputValue[inputIndex] != Char.Parse("_")
                            filledCount := filledCount + 1
                            inputIndex := inputIndex + 1
                        ENDIF
                    ENDIF
                ENDIF
            NEXT
            
            RETURN requiredCount > 0 .AND. requiredCount == filledCount
        END METHOD
        
        /// <summary>
        /// Validate entire masked value for completeness and correctness
        /// </summary>
        PUBLIC METHOD ValidateValue(pattern AS InputMaskPattern, maskedValue AS STRING) AS LOGIC
            IF pattern == NIL .OR. String.IsNullOrEmpty(maskedValue)
                RETURN FALSE
            ENDIF
            
            IF maskedValue:Length != pattern:TotalLength
                RETURN FALSE
            ENDIF
            
            // Check each position
            FOR VAR i := 0 TO maskedValue:Length - 1
                IF i >= pattern:Positions:Count
                    RETURN FALSE
                ENDIF
                
                VAR maskPos := pattern:Positions[i]
                VAR ch := maskedValue[i]
                
                IF maskPos:Type == "literal"
                    // Literal position must match
                    IF ch:ToString() != maskPos:Literal
                        RETURN FALSE
                    ENDIF
                ENDIF
            NEXT
            
            RETURN TRUE
        END METHOD
        
    END CLASS

END NAMESPACE
