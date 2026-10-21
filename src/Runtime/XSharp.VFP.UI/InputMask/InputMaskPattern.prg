USING System
USING System.Collections.Generic
USING System.Linq


/*
VFP InputMask reference (from VFP Help File)
Control.InputMask = cMask

| Char | Type        | Description                                                       |
|------|-------------|-------------------------------------------------------------------|
|  9   | digit       | Digits and signs (0-9, +, -)                                      |
|  #   | digit       | Digits, spaces and signs (optional entry)                         |
|  A   | letter      | Alphabetic characters only (a-z, A-Z)                             |
|  N   | alphanumeric| Letters and digits only                                           |
|  X   | any         | Any character                                                     |
|  H   | hex         | Hexadecimal digits only (0-9, A-F); blocks non-hex input          |
|  L   | logical     | Logical data only (.T. / .F.)                                     |
|  Y   | logical     | Logical Y/N — Y or y = .T., N or n = .F.                         |
|  U   | modifier    | Alphabetic only, forced to uppercase (A-Z)                        |
|  W   | modifier    | Alphabetic only, forced to lowercase (a-z)                        |
|  !   | modifier    | Converts any letter to uppercase; no display placeholder          |
|  &   | modifier    | Space-fill — fills position with blanks; no display placeholder   |
|  .   | decimal     | Decimal point (per SET POINT); marks decimal position in mask     |
|  ,   | literal     | Digit-grouping separator (per regional settings)                  |
|  $   | literal     | Currency symbol in fixed position (per SET CURRENCY)              |
| $$   | literal     | Floating currency symbol — always adjacent to digits              |

Notes:
  - Chars not in the table are treated as literals (e.g. "(", ")", "-", " ").
  - "$$" is stored as two consecutive "$" literals; renderer decides fixed vs floating display.
  - "!" and "&" are modifiers: they affect input handling but produce no placeholder character.
*/
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
        /// Parse the mask string into structured pattern
        /// Example: "(999) 999-9999" →
        ///   Position 0: literal "("
        ///   Position 1-3: digit "9"
        ///   Position 4: literal ")"
        ///   Position 5: literal " "
        ///   Position 6-8: digit "9"
        ///   Position 9: literal "-"
        ///   Position 10-13: digit "9"
        /// </summary>
        PUBLIC METHOD Parse(maskStr AS STRING) AS VOID
            SELF:MaskString := maskStr
            SELF:Positions:Clear()

            VAR sb := System.Text.StringBuilder{}
            VAR dataCount := 0
            VAR charUnderscore := Char.Parse("_")

            FOR VAR i := 0 TO maskStr:Length - 1
                VAR ch := maskStr[i]
                VAR position := MaskPosition{}
                position:Index := i
                position:OriginalChar := ch

                SWITCH ch:ToString()
                    CASE "9"
                        // Digit required (0-9 only)
                        position:Type := "digit"
                        position:Required := TRUE
                        dataCount := dataCount + 1
                        sb:Append(charUnderscore)

                    CASE "#"
                        // Digit optional (0-9, space, +, -)
                        position:Type := "digit"
                        position:Required := FALSE
                        dataCount := dataCount + 1
                        sb:Append(charUnderscore)

                    CASE "A"
                        // Letter required (a-z, A-Z)
                        position:Type := "letter"
                        position:Required := TRUE
                        dataCount := dataCount + 1
                        sb:Append(charUnderscore)

                    CASE "a"
                        // Letter optional (a-z, A-Z, lowercase)
                        position:Type := "letter"
                        position:Required := FALSE
                        dataCount := dataCount + 1
                        sb:Append(charUnderscore)

                    CASE "X"
                        // Alphanumeric required (0-9, a-z, A-Z)
                        position:Type := "alphanumeric"
                        position:Required := TRUE
                        dataCount := dataCount + 1
                        sb:Append(charUnderscore)

                    CASE "x"
                        // Alphanumeric optional (0-9, a-z, A-Z, lowercase)
                        position:Type := "alphanumeric"
                        position:Required := FALSE
                        dataCount := dataCount + 1
                        sb:Append(charUnderscore)

                    CASE "!"
                        // Uppercase modifier - forces letters to uppercase; not a visible placeholder char
                        position:Type := "modifier"
                        position:Modifier := "!"
                        // Do NOT append to placeholder - modifier has no display character

                    CASE "&"
                        // Space-fill modifier — not a visible placeholder char
                        position:Type := "modifier"
                        position:Modifier := "&"

                    CASE "."
                        // Decimal point — marks decimal position; rendered as literal
                        position:Type := "decimal"
                        position:Literal := ch:ToString()
                        sb:Append(ch)

                    CASE ","
                        // Thousands separator — literal
                        position:Type := "literal"
                        position:Literal := ch:ToString()
                        sb:Append(ch)

                    CASE "$"
                        // Currency symbol — literal (fixed position; $$ floating = two $ chars)
                        position:Type := "literal"
                        position:Literal := ch:ToString()
                        sb:Append(ch)

                    CASE "H"
                        // Hexadecimal digit (0-9, A-F)
                        position:Type := "hex"
                        position:Required := TRUE
                        dataCount := dataCount + 1
                        sb:Append(charUnderscore)

                    CASE "L"
                        // Logical data (.T. / .F.)
                        position:Type := "logical"
                        position:Required := TRUE
                        dataCount := dataCount + 1
                        sb:Append(charUnderscore)

                    CASE "N"
                        // Letters and digits
                        position:Type := "alphanumeric"
                        position:Required := TRUE
                        dataCount := dataCount + 1
                        sb:Append(charUnderscore)

                    CASE "U"
                        // Alphabetic, forced uppercase
                        position:Type := "letter"
                        position:Modifier := "U"
                        position:Required := TRUE
                        dataCount := dataCount + 1
                        sb:Append(charUnderscore)

                    CASE "W"
                        // Alphabetic, forced lowercase
                        position:Type := "letter"
                        position:Modifier := "W"
                        position:Required := TRUE
                        dataCount := dataCount + 1
                        sb:Append(charUnderscore)

                    CASE "Y"
                        // Logical Y/N (Y = .T., N = .F.)
                        position:Type := "logical"
                        position:Modifier := "Y"
                        position:Required := TRUE
                        dataCount := dataCount + 1
                        sb:Append(charUnderscore)

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
        /// Get the type of character at position
        /// Returns: "digit", "letter", "alphanumeric", "literal", "modifier", or "invalid"
        /// </summary>
        PUBLIC METHOD GetCharacterType(position AS INT) AS STRING
            IF position < 0 .OR. position >= SELF:Positions:Count
                RETURN "invalid"
            ENDIF
            RETURN SELF:Positions[position]:Type
        END METHOD

        /// <summary>
        /// Check if position is a literal character
        /// </summary>
        PUBLIC METHOD IsLiteralPosition(position AS INT) AS LOGIC
            IF position < 0 .OR. position >= SELF:Positions:Count
                RETURN FALSE
            ENDIF
            RETURN SELF:Positions[position]:Type == "literal"
        END METHOD

        /// <summary>
        /// Check if position is a modifier
        /// </summary>
        PUBLIC METHOD IsModifierPosition(position AS INT) AS LOGIC
            IF position < 0 .OR. position >= SELF:Positions:Count
                RETURN FALSE
            ENDIF
            RETURN SELF:Positions[position]:Type == "modifier"
        END METHOD

        /// <summary>
        /// Get next data position after given position
        /// Skips literals and modifiers
        /// </summary>
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
        /// Get previous data position before given position
        /// Skips literals and modifiers
        /// </summary>
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
        /// Get first data position in mask
        /// </summary>
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
        /// Get literal character at position (if it is a literal)
        /// </summary>
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
