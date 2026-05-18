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
            VAR ch := character[0]

            SWITCH maskPos:Type
                // Digit required (0-9 only) or optional (0-9, space, sign)
                CASE "digit"
                    IF maskPos:Required
                        RETURN Char.IsDigit(ch)
                    ELSE
                        RETURN Char.IsDigit(ch) .OR. ch == ' ' .OR. ch == '+' .OR. ch == '-'
                    ENDIF

                // Letter required (a-z, A-Z)
                CASE "letter"
                    RETURN Char.IsLetter(ch)

                // Alphanumeric (0-9, a-z, A-Z)
                CASE "alphanumeric"
                    RETURN Char.IsLetterOrDigit(ch)

                // Hexadecimal digit (0-9, A-F)
                CASE "hex"
                    RETURN Char.IsDigit(ch) .OR. (Char.ToUpper(ch) >= 'A' .AND. Char.ToUpper(ch) <= 'F')

                // Logical: Y picture accepts Y/N/J/T/F; L picture accepts T/F/J/Y/N
                CASE "logical"
                    IF maskPos:Modifier == "Y"
                        VAR up := Char.ToUpper(ch)
                        RETURN up == 'Y' .OR. up == 'N' .OR. up == 'J' .OR. up == 'T' .OR. up == 'F'
                    ELSE
                        VAR up2 := Char.ToUpper(ch)
                        RETURN up2 == 'T' .OR. up2 == 'F' .OR. up2 == 'J' .OR. up2 == 'Y' .OR. up2 == 'N'
                    ENDIF

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

            // Apply case modifier from the current position (!, U = uppercase; W = lowercase)
            IF maskPos:Modifier == "!" .OR. maskPos:Modifier == "U"
                result := result:ToUpper()
            ELSEIF maskPos:Modifier == "W"
                result := result:ToLower()
            ELSE
                // A preceding "!" modifier position forces the next data char to uppercase
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
                    IF Char.IsLetter(character[0])
                        result := result:ToLower()
                    ENDIF
                ENDIF
            ENDIF

            // For lowercase alphanumeric type ('x' in mask = lowercase optional alphanumeric), letters are lowercase
            IF maskPos:Type == "alphanumeric"
                IF maskPos:OriginalChar == Char.Parse("x")
                    IF Char.IsLetter(character[0])
                        result := result:ToLower()
                    ENDIF
                ENDIF
            ENDIF

            // Logical Y: canonicalise input — J/T/Y → "Y", everything else → "N"
            IF maskPos:Type == "logical" .AND. maskPos:Modifier == "Y"
                VAR upCh := Char.ToUpper(result[0])
                result := IIF(upCh == 'Y' .OR. upCh == 'J' .OR. upCh == 'T', "Y", "N")
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
