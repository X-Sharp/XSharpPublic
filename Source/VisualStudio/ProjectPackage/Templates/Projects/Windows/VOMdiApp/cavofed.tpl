[CLASS] 
CLASS %classname% INHERIT %superclass%


[INIT]
METHOD Init() CLASS %classname%
    LOCAL   cPict                   AS STRING

    SUPER:Init( HyperLabel{#%hlname%, "%hlcaption%", "%hldescription%", "%hlhelpcontext%" },  "%type%", %len%, %dec% )
    cPict       := "%picture"
    IF SLen(cPict) > 0
        SELF:Picture := cPict
    ENDIF

    RETURN SELF



[DBSERVER]
Method GetFieldInfo%hlname%() CLASS %servername%
RETURN {#%hlname%, "%hlcaption%","%hldescription%","%hlhelpcontext%"}

[DBSERVER]
ACCESS %hlname%  AS %usualtype% PASCAL CLASS %servername%

    RETURN SELF:FieldGet(#%hlname%)


[DBSERVER]
ASSIGN  %hlname%(uValue AS %usualtype%)  AS %usualtype% PASCAL CLASS %servername%

    RETURN SELF:FieldPut(#%hlname%, uValue)

