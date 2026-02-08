USING System
USING System.IO
USING System.Text
USING System.Threading.Tasks
using System.Text.Json
USING Microsoft.AspNetCore.Builder
USING Microsoft.AspNetCore.Routing
USING Microsoft.AspNetCore.Hosting
USING Microsoft.AspNetCore.Http
using Microsoft.AspNetCore.Mvc
using System.Runtime.CompilerServices

CLASS RouteUsers

    PRIVATE PROPERTY Owner AS RESTful AUTO

    CONSTRUCTOR( owner AS RESTful)
        // Constructor for RouteId class
        // This can be used to initialize any necessary properties or fields
        SELF:Owner := owner
    END CONSTRUCTOR 

    ASYNC METHOD ReplyGet( context AS HttpContext ) AS Task<IResult>
        TRY
        // Handle the request and return a response
            VAR data := JsonSerializer.Serialize( Owner:Data )

            context:Response:ContentType := "application/json"
            context:Response:WriteAsync( data )
            RETURN Results.Ok( )
        CATCH ex AS Exception
            // Handle any exceptions that occur during processing
            Console.WriteLine("Error: " + ex:Message)
            context:Response:StatusCode := 400 // Bad Request
            RETURN Results.BadRequest("Invalid request")
        END TRY
    END METHOD

    ASYNC METHOD ReplyPost( context AS HttpContext ) AS Task<IResult>
        TRY
            // Handle the request and return a response
            VAR urlPath := context:Request:Path:ToString()
            // Extract the ID from the URL path
            VAR id := urlPath:SubString(urlPath:LastIndexOf("/") + 1)
            VAR idInt := Convert.ToInt32(id)
            // Read the request body as a JSON string
            VAR bodyStream := context:Request:Body 
            VAR reader := StreamReader{ bodyStream, Encoding.UTF8 }
            VAR jsonString := await reader:ReadToEndAsync()
            VAR user := JsonSerializer.Deserialize<DataUser>(jsonString)
            // Search the data for the user with the specified ID
            IF Owner:Data:ContainsKey(idInt)
                // Update the existing user
                Owner:Data[idInt] := user
            ELSE
                // Add a new user
                Owner:Data:Add(idInt, user)
            ENDIF
            Console.WriteLine("jsonString: " + jsonString)

            context:Response:ContentType := "application/json"
            VAR data := JsonSerializer.Serialize( Owner:Data )
            //context:Response:WriteAsync( data )
            RETURN Results.Ok( data )
        CATCH ex AS Exception
            // Handle any exceptions that occur during processing
            Console.WriteLine("Error: " + ex:Message)
            RETURN Results.BadRequest("Invalid request")
        END TRY
    END METHOD
END CLASS