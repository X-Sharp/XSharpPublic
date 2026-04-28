USING System
USING System.Collections.Generic
USING System.Linq

BEGIN NAMESPACE XSharp.VFP.UI

    /// <summary>
    /// Parses and stores InputMask pattern information
    /// Breaks down a mask string like "(999) 999-9999" into structured position data
    /// </summary>
    PUBLIC CLASS InputMaskPattern
        
        /// <summary>
        /// The original mask string (e.g., "(999) 999-9999")
        /// </summary>
        PUBLIC PROPERTY MaskString AS STRING AUTO
        
        /// <summary>
        /// List of parsed positions in the mask
        /// </summary>
        PUBLIC PROPERTY Positions AS List<MaskPosition> AUTO
        
        /// <summary>
        /// Display placeholder string (e.g., "(___) ___-____")
        /// </summary>
        PUBLIC PROPERTY PlaceholderDisplay AS STRING AUTO
        
        /// <summary>
        /// Number of data positions (excluding literals)
        /// </summary>
        PUBLIC PROPERTY DataLength AS INT AUTO
        
        /// <summary>
        /// Total number of positions including literals
        /// </summary>
        PUBLIC PROPERTY TotalLength AS INT AUTO
        
        /// <summary>
        /// Constructor
        /// </summary>
        PUBLIC CONSTRUCTOR()
            SELF:Positions := List<MaskPosition>{}
        END CONSTRUCTOR
        
        /// <summary>
        /// Parse the mask string into structured pattern.
        /// All position indices stored in <see cref="Positions"/> are 0-based, matching
        /// the <c>TextBox.SelectionStart</c> / <c>SelectionLength</c> .NET convention.
        /// Example: "(999) 999-9999" →
        ///   Position 0: literal "("
        ///   Position 1-3: digit "9"
        ///   Position 4: literal ")"
        ///   Position 5: literal " "
        ///   Position 6-8: digit "9"
        ///   Position 9: literal "-"
        ///   Position 10-13: digit "9"
        /// </summary>
        /// <param name="maskStr">VFP-style mask string (e.g. "(999) 999-9999").</param>
        PUBLIC METHOD Parse(maskStr AS STRING) AS VOID
            SELF:MaskString := maskStr
            SELF:Positions:Clear()
            
            VAR sb := System.Text.StringBuilder{}
            VAR dataCount := 0
            
            FOR VAR i := 0 TO maskStr:Length - 1
                VAR ch := maskStr[i]
                VAR position := MaskPosition{}
                position:Index := i
                
                SWITCH ch
                    CASE '9'
                        // Digit required (0-9 only)
                        position:Type := "digit"
                        position:Required := TRUE
                        dataCount := dataCount + 1
                        sb:Append('_')
                        
                    CASE '#'
                        // Digit optional (0-9, space, +, -)
                        position:Type := "digit"
                        position:Required := FALSE
                        dataCount := dataCount + 1
                        sb:Append('_')
                        
                    CASE 'A'
                        // Letter required (a-z, A-Z)
                        position:Type := "letter"
                        position:Required := TRUE
                        dataCount := dataCount + 1
                        sb:Append('_')
                        
                    CASE 'a'
                        // Letter optional (a-z, A-Z, lowercase)
                        position:Type := "letter"
                        position:Required := FALSE
                        dataCount := dataCount + 1
                        sb:Append('_')
                        
                    CASE 'X'
                        // Alphanumeric required (0-9, a-z, A-Z)
                        position:Type := "alphanumeric"
                        position:Required := TRUE
                        dataCount := dataCount + 1
                        sb:Append('_')
                        
                    CASE 'x'
                        // Alphanumeric optional (0-9, a-z, A-Z, lowercase)
                        position:Type := "alphanumeric"
                        position:Required := FALSE
                        dataCount := dataCount + 1
                        sb:Append('_')
                        
                    CASE '!'
                        // Uppercase modifier - forces letters to uppercase
                        position:Type := "modifier"
                        position:Modifier := "!"
                        sb:Append('!')
                        
                    CASE '&'
                        // Space-fill modifier - fills spaces with blanks
                        position:Type := "modifier"
                        position:Modifier := "&"
                        sb:Append('&')
                        
                    OTHERWISE
                        // Any other character is a literal (parentheses, dashes, spaces, etc.)
                        position:Type := "literal"
                        position:Literal := ch:ToString()
                        sb:Append(ch)
                END SWITCH
                
                SELF:Positions:Add(position)
            NEXT
            
            SELF:PlaceholderDisplay := sb:ToString()
            SELF:DataLength := dataCount
            SELF:TotalLength := maskStr:Length
        END METHOD
        
        /// <summary>
        /// Get the type of character at position.
        /// Returns: "digit", "letter", "alphanumeric", "literal", "modifier", or "invalid"
        /// </summary>
        /// <param name="position">0-based index into <see cref="Positions"/> (mirrors <c>TextBox.SelectionStart</c>).</param>
        /// <returns>Type string, or "invalid" when <paramref name="position"/> is out of range.</returns>
        PUBLIC METHOD GetCharacterType(position AS INT) AS STRING
            IF position < 0 .OR. position >= SELF:Positions:Count
                RETURN "invalid"
            ENDIF
            RETURN SELF:Positions[position]:Type
        END METHOD
        
        /// <summary>
        /// Check if position is a literal character.
        /// </summary>
        /// <param name="position">0-based index into <see cref="Positions"/>.</param>
        /// <returns><c>TRUE</c> when the position holds a literal; <c>FALSE</c> when out of range.</returns>
        PUBLIC METHOD IsLiteralPosition(position AS INT) AS LOGIC
            IF position < 0 .OR. position >= SELF:Positions:Count
                RETURN FALSE
            ENDIF
            RETURN SELF:Positions[position]:Type == "literal"
        END METHOD
        
        /// <summary>
        /// Check if position is a modifier.
        /// </summary>
        /// <param name="position">0-based index into <see cref="Positions"/>.</param>
        /// <returns><c>TRUE</c> when the position is a modifier; <c>FALSE</c> when out of range.</returns>
        PUBLIC METHOD IsModifierPosition(position AS INT) AS LOGIC
            IF position < 0 .OR. position >= SELF:Positions:Count
                RETURN FALSE
            ENDIF
            RETURN SELF:Positions[position]:Type == "modifier"
        END METHOD
        
        /// <summary>
        /// Get next data position after given position.
        /// Skips literals and modifiers.
        /// </summary>
        /// <param name="fromPos">0-based starting index (exclusive); the search begins at <c>fromPos + 1</c>.</param>
        /// <returns>0-based index of the next data position, or <c>-1</c> if none exists.</returns>
        PUBLIC METHOD GetNextDataPosition(fromPos AS INT) AS INT
            FOR VAR i := fromPos + 1 TO SELF:Positions:Count - 1
                VAR maskType := SELF:Positions[i]:Type
                IF maskType != "literal" .AND. maskType != "modifier"
                    RETURN i
                ENDIF
            NEXT
            RETURN -1
        END METHOD
        
        /// <summary>
        /// Get previous data position before given position.
        /// Skips literals and modifiers.
        /// </summary>
        /// <param name="fromPos">0-based starting index (exclusive); the search begins at <c>fromPos - 1</c>.</param>
        /// <returns>0-based index of the previous data position, or <c>-1</c> if none exists.</returns>
        PUBLIC METHOD GetPreviousDataPosition(fromPos AS INT) AS INT
            FOR VAR i := fromPos - 1 DOWNTO 0
                VAR maskType := SELF:Positions[i]:Type
                IF maskType != "literal" .AND. maskType != "modifier"
                    RETURN i
                ENDIF
            NEXT
            RETURN -1
        END METHOD
        
        /// <summary>
        /// Get first data position in mask.
        /// </summary>
        /// <returns>0-based index of the first non-literal, non-modifier position, or <c>0</c> if the mask is empty.</returns>
        PUBLIC METHOD GetFirstDataPosition() AS INT
            FOR VAR i := 0 TO SELF:Positions:Count - 1
                VAR maskType := SELF:Positions[i]:Type
                IF maskType != "literal" .AND. maskType != "modifier"
                    RETURN i
                ENDIF
            NEXT
            RETURN 0
        END METHOD
        
        /// <summary>
        /// Get literal character at position (if it is a literal).
        /// </summary>
        /// <param name="position">0-based index into <see cref="Positions"/>.</param>
        /// <returns>The literal string at that position, or <c>""</c> if out of range or not a literal.</returns>
        PUBLIC METHOD GetLiteralAt(position AS INT) AS STRING
            IF position >= 0 .AND. position < SELF:Positions:Count
                VAR pos := SELF:Positions[position]
                IF pos:Type == "literal"
                    RETURN pos:Literal
                ENDIF
            ENDIF
            RETURN ""
        END METHOD
        
        /// <summary>
        /// Validate that pattern is well-formed
        /// </summary>
        PUBLIC METHOD IsValid() AS LOGIC
            RETURN !String.IsNullOrEmpty(SELF:MaskString) .AND. SELF:Positions:Count > 0 .AND. SELF:DataLength > 0
        END METHOD
        
    END CLASS

END NAMESPACE
