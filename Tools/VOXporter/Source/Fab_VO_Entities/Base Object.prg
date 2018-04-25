
#include "GlobalDefines.vh"

BEGIN NAMESPACE Fab_VO_Entities

    CLASS FabIDEBaseObject
        // Base Class for All Fab IDE objects
        // At least, each object as a Name, and may raise some errors

	    PROTECT dwError	AS DWORD
	    PROTECT cError	AS STRING
	    PROTECT cName	AS STRING

	    CONSTRUCTOR()  
            SUPER()
        RETURN

        ACCESS ErrorCode AS DWORD  
            //r The last Error, as a numerical value, that the object produces.
        RETURN SELF:dwError


        ACCESS ErrorString AS STRING  
            //r The last Error, as a string value, that the object produces.
        RETURN SELF:cError


        VIRTUAL ACCESS Name AS STRING  
            //r A String with the Name of the Object
        RETURN SELF:cName


        VIRTUAL ACCESS Success AS LOGIC  
            //r A Logical Value indicating if the last operation with this object was error-free
        RETURN ( SELF:dwError == WIN32SUCCESS )


    END CLASS

END NAMESPACE