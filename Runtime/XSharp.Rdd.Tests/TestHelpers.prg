// TestHelpers.prg
// Created by    : fabri
// Creation Date : 6/8/2018 11:16:12 AM
// Created for   : 
// WorkStation   : FABPORTABLE


USING System
USING System.Collections.Generic
USING System.Text

STATIC GLOBAL rand AS Random


FUNCTION LoremIpsum( maxLength AS LONG  ) AS STRING
    RETURN LoremIpsum( maxLength, 1, Int32.MaxValue , 1 , Int32.MaxValue , Int32.MaxValue )
    
    
FUNCTION LoremIpsum( maxLength AS LONG , minWords AS LONG , maxWords AS LONG , minSentences AS LONG , maxSentences AS LONG , numParagraphs AS LONG ) AS STRING
    LOCAL words AS STRING[]
    LOCAL numSentences AS LONG
    LOCAL numWords AS LONG
    LOCAL result AS StringBuilder
    LOCAL p AS LONG
    LOCAL s AS LONG
    LOCAL w AS LONG
    LOCAL quit AS LOGIC
    LOCAL r AS LONG
    //
    IF ( maxLength == 0 )
        maxLength := Int32.MaxValue
    ENDIF
    quit := FALSE
    words := <STRING>;
    {"lorem", "ipsum", "dolor", "sit", "amet", "consectetuer", "adipiscing", "elit", "sed", "diam", "nonummy",;
    "nibh", "euismod", "tincidunt", "ut", "laoreet", "dolore", "magna", "aliquam", "erat" }
    //
    IF ( rand == NULL )
        rand := Random{ (LONG)DateTime.Now.Ticks }
    ENDIF
    numSentences := rand:@@NEXT(maxSentences - minSentences) + minSentences + 1
    numWords := rand:@@NEXT(maxWords - minWords) + minWords + 1
    result := StringBuilder{}
    //
    LOCAL nStart AS INT
    nStart := 1
    IF __ARRAYBASE__ == 0
        nStart -= 1
    ENDIF
    FOR p := nStart TO numParagraphs - ( 1 - nStart ) 
        FOR s := nStart TO numSentences - ( 1 - nStart )
            FOR w := nStart TO numWords - ( 1 - nStart )
                IF (w > nStart)
                    IF ( result:Length + 1 >= maxLength )
                        quit := TRUE
                        EXIT
                    ENDIF
                    result:Append(" ")
                ENDIF
                IF ( quit )
                    EXIT
                ENDIF
                r := rand:@@NEXT(words:Length)+1
                IF ( result:Length + words[r]:Length >= maxLength )
                    quit := TRUE
                    EXIT
                ENDIF
                result:Append(words[r])
            NEXT
            IF ( quit )
                EXIT
            ENDIF
            IF ( result:Length + 2 >= maxLength )
                quit := TRUE
                EXIT
            ENDIF
            result:Append(". ")
        NEXT
        IF ( quit )
            EXIT
        ENDIF
        IF ( result:Length + 2 >= maxLength )
            quit := TRUE
            EXIT
        ENDIF
        result:Append(Environment.NewLine)
    NEXT
    RETURN result:ToString()