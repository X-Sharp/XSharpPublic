#include "GlobalDefines.vh"

BEGIN NAMESPACE Fab_VO_Entities

    CLASS FabApplicationAbstract	INHERIT	FabIDEBaseObject

        //
        //
        //
        //
        CONSTRUCTOR()  
        SUPER()
        RETURN

        VIRTUAL METHOD Close() AS VOID  

        RETURN

/*        VIRTUAL ACCESS CreateDate AS DATE 	
        RETURN NULL_DATE


        VIRTUAL ACCESS CreateTime AS STRING 	
        RETURN "00:00:00"*/


        VIRTUAL ACCESS Description AS STRING  
        RETURN ""


        VIRTUAL ACCESS ExeName AS STRING  
        RETURN ""


        VIRTUAL METHOD ExportModule( cModName AS STRING, cFileName AS STRING ) AS LOGIC  
        RETURN FALSE


        VIRTUAL ACCESS FullPath AS STRING  
        RETURN ""


        VIRTUAL  ASSIGN FullPath( cNew AS STRING )   
        RETURN 


        ACCESS IsApplication AS LOGIC  
        RETURN ( SELF:IsLibrary == FALSE ) .AND. ( SELF:IsDLL == FALSE )


        VIRTUAL ACCESS IsDLL AS LOGIC  
        RETURN FALSE


        VIRTUAL ACCESS IsLibrary AS LOGIC  
        RETURN FALSE

        VIRTUAL ACCESS	IsValid AS LOGIC 	
        RETURN FALSE


/*        VIRTUAL ACCESS LastBuildDate AS DATE 	
        RETURN NULL_DATE


        VIRTUAL ACCESS LastBuildTime AS STRING 	
        RETURN "00:00:00"*/


        VIRTUAL ACCESS LibraryNameList AS xARRAY  
        RETURN xARRAY{}


        VIRTUAL ACCESS	ModuleCount AS DWORD 	
        RETURN ALen( SELF:ModuleList )


/*        VIRTUAL METHOD ModuleFind( cModName AS STRING ) AS OBJECT 	
        RETURN NULL_OBJECT*/


        VIRTUAL ACCESS	ModuleList AS xARRAY 	
//        RETURN NULL_ARRAY
        RETURN NULL


/*        VIRTUAL ACCESS	ModuleNameList AS ARRAY 	
        RETURN NULL_ARRAY
*/
        PROTECT VIRTUAL METHOD Scan() AS VOID 	
        RETURN

        VIRTUAL METHOD	SortByName( ) AS VOID  
        RETURN

/*        VIRTUAL ACCESS UDCNameList AS ARRAY  
        RETURN {}*/

    END CLASS

END NAMESPACE

