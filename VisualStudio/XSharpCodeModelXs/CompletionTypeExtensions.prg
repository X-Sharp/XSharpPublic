
USING System
BEGIN NAMESPACE XSharpModel

    STATIC ;
    CLASS CompletionTypeExtensions
        // Methods
        STATIC METHOD IsEmpty( SELF cType AS CompletionType) AS Logic
            //
            RETURN ((cType == null) .OR. ! cType:IsInitialized)


    END CLASS

END NAMESPACE 

