/// <include file="Console.xml" path="doc/Console/*" />
CLASS Console
      
    /// <include file="Console.xml" path="doc/Console.ctor/*" />
    CONSTRUCTOR( ) STRICT
        RETURN 
      
    /// <include file="Console.xml" path="doc/Console.ctor/*" />
    CONSTRUCTOR( lNew AS LOGIC ) 
        SELF()
      
    /// <include file="Console.xml" path="doc/Console.Clear/*" />
    METHOD Clear( ) AS Console
        System.Console.Clear()
        System.Console.SetCursorPosition(0,0)
        RETURN SELF
      
    /// <include file="Console.xml" path="doc/Console.CursorPos/*" />
    PROPERTY CursorPos AS ConsoleCoord 
        GET
            RETURN ConsoleCoord{ System.Console.CursorLeft, System.Console.CursorTop }
        END GET
        SET 
            System.Console.CursorLeft := VALUE:X
            System.Console.CursorTop  := VALUE:Y
            RETURN
        END SET
    END PROPERTY
      
    /// <include file="Console.xml" path="doc/Console.CursorSize/*" />
    PROPERTY CursorSize AS INT ;
        GET System.Console.CursorSize ;
        SET System.Console.CursorSize := VALUE
      
    /// <include file="Console.xml" path="doc/Console.CursorVisible/*" />
    PROPERTY CursorVisible AS LOGIC ;
        GET System.Console.CursorVisible ;
        SET System.Console.CursorVisible := VALUE
        
    /// <include file="Console.xml" path="doc/Console.Destroy/*" />
    METHOD Destroy( ) AS USUAL CLIPPER
        RETURN SELF
            
    /// <include file="Console.xml" path="doc/Console.Read/*" />
    METHOD Read( ) AS STRING
        RETURN System.Console.ReadLine()
            
    /// <include file="Console.xml" path="doc/Console.Size/*" />
    PROPERTY Size AS ConsoleCoord
        GET 
            RETURN ConsoleCoord{ System.Console.BufferHeight, System.Console.BufferWidth}
        END GET
        SET 
            LOCAL coordW 			AS INT
            LOCAL coordH 			AS INT
            LOCAL bufferW           AS INT
            LOCAL bufferH           AS INT
            LOCAL oNewSize := VALUE as ConsoleCoord
            
            // define the new console window size and scroll position
            coordW := System.Console.LargestWindowWidth
            coordH := System.Console.LargestWindowHeight
            coordW := Min( oNewSize:X, coordW ) 
            coordH := Min( oNewSize:Y, coordH ) 
            
            bufferW := System.Console.BufferWidth
            bufferH := System.Console.BufferHeight
            
            // if the current buffer is larger than what we want, resize the
            // console window first, then the buffer
            IF ( coordW * coordH >  oNewSize:X * oNewSize:Y )
                System.Console.SetWindowSize(coordW, coordH)
                System.Console.SetBufferSize(bufferW,bufferH)
                
                
            ELSEIF ( coordW * coordH <  oNewSize:X * oNewSize:Y )
                // if the current buffer is smaller than what we want, resize the
                // buffer first, then the console window
                System.Console.SetBufferSize(bufferW,bufferH)
                System.Console.SetWindowSize(coordW, coordH)
            ENDIF
            
            RETURN
        END SET
    END PROPERTY
    
    /// <include file="Console.xml" path="doc/Console.TextAttribute/*" />
    PROPERTY TextAttribute AS WORD
    GET
        RETURN (WORD)System.Console.ForegroundColor | (WORD)(System.Console.BackgroundColor) << 4
    END GET
    SET
        System.Console.ForegroundColor := (ConsoleColor) (WORD)(VALUE & 0x000F)
        System.Console.BackgroundColor := (ConsoleColor) (WORD)((VALUE >> 4) & 0x000F)
    END SET
    END PROPERTY

    
    /// <include file="Console.xml" path="doc/Console.Title/*" />
    PROPERTY Title AS STRING ;
        GET System.Console.Title ;
        SET System.Console.Title := VALUE
    
    /// <include file="Console.xml" path="doc/Console.Wait/*" />
    METHOD Wait AS STRING
        local Info     as System.ConsoleKeyInfo
        LOCAL cResult   AS STRING
        info := System.Console.ReadKey(TRUE)
        cResult := Info:KeyChar:ToString()
        SELF:Write(cResult)
        return cResult
        
        
    /// <include file="Console.xml" path="doc/Console.Write/*" />
    METHOD Write( sMsg AS STRING ) AS VOID
        System.Console.Write(sMsg)
        RETURN
        
    /// <include file="Console.xml" path="doc/Console.WriteError/*" />
    METHOD WriteError( sMsg AS STRING) AS VOID
        System.Console.Error:Write(sMsg)
        RETURN 
        
    /// <include file="Console.xml" path="doc/Console.WriteLine/*" />
    METHOD WriteLine( sMsg AS STRING ) AS VOID
        System.Console.WriteLine(sMsg)
        RETURN 
        
END CLASS

