// NtxLevel.prg
// Created by    : fabri
// Creation Date : 9/2/2018 6:15:26 PM
// Created for   : 
// WorkStation   : FABPORTABLE


USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE XSharp.RDD


    INTERNAL CLASS NtxLevel INHERIT NtxPage
        PRIVATE lExp AS LONG
        PRIVATE lBaseKeys AS LONG
        PRIVATE lKeys AS LONG
        PRIVATE liExtraKeys AS LONG
        PRIVATE liParents AS LONG

        PUBLIC PROPERTY Exp AS LONG
            GET
                RETURN lExp
                
            END GET
            SET
                lExp := VALUE
                
            END SET
        END PROPERTY
        
        PUBLIC PROPERTY BaseKeys AS LONG
            GET
                RETURN lBaseKeys
                
            END GET
            SET
                lBaseKeys := VALUE
                
            END SET
        END PROPERTY
        
        PUBLIC PROPERTY Keys AS LONG
            GET
                RETURN lKeys
                
            END GET
            SET
                lKeys := VALUE
                
            END SET
        END PROPERTY
        
        PUBLIC PROPERTY ExtraKeys AS LONG
            GET
                RETURN liExtraKeys
                
            END GET
            SET
                liExtraKeys := VALUE
                
            END SET
        END PROPERTY
        
        PUBLIC PROPERTY Parents AS LONG
            GET
                RETURN liParents
                
            END GET
            SET
                liParents := VALUE
                
            END SET
        END PROPERTY
        
        PUBLIC METHOD InitRefs(uiMaxEntry AS LONG , uiEntrySize AS LONG ) AS VOID
            LOCAL num AS SHORT
            LOCAL i AS LONG
            //
            SELF:Write( SELF:PageOffset)
            num := (SHORT)((uiMaxEntry + 2) * 2)
            //Init
            i := 0
            WHILE i <= uiMaxEntry
                SetRef(i, num)
                num := (SHORT)(num + (SHORT)uiEntrySize)
                //Iterators
                i++
            ENDDO
            SUPER:NodeCount := 0
            
            
        PUBLIC CONSTRUCTOR(order AS NtxOrder )
            SUPER(order, 0L)
            SELF:lExp := 0
            SELF:lBaseKeys := 0
            SELF:lKeys := 0
            SELF:liParents := 0
            SELF:liExtraKeys := 0
            
            
        PUBLIC METHOD Write( offset AS LONG ) AS LOGIC
            LOCAL result AS LOGIC
            //
            SELF:PageOffset := offset
            result := SELF:Write()
            SELF:PageOffset := 0
            RETURN result
            
            
    END CLASS
    
    
END NAMESPACE