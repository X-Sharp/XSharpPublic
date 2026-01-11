USING System
USING System.IO
USING System.Text
USING System.Threading.Tasks
using System.Text.Json
USING Microsoft.AspNetCore.Builder
USING Microsoft.AspNetCore.Routing
USING Microsoft.AspNetCore.Hosting
USING Microsoft.AspNetCore.Http


CLASS RouteId

    PRIVATE PROPERTY Owner AS RESTful AUTO

    CONSTRUCTOR( owner AS RESTful)
        // Constructor for RouteId class
        // This can be used to initialize any necessary properties or fields
        SELF:Owner := owner
    END CONSTRUCTOR 

    ASYNC METHOD Reply( context AS HttpContext ) AS Task<IResult>
        // Create a response with the data ID
        // Access the request body and deserialize it
        // to a DataId object
        TRY
            // Read the request body as a JSON string
            VAR bodyStream := context:Request:Body 
            VAR reader := StreamReader{ bodyStream, Encoding.UTF8 }
            VAR jsonString := await reader:ReadToEndAsync()
            VAR data := JsonSerializer.Deserialize<DataId>(jsonString)
            // Write it on screen for debugging purpose
            Console.WriteLine("id: " + data:Id)
            // Return the data ID as a response
            context:Response:ContentType := "application/json"
            context:Response:WriteAsync( data:Id )
            RETURN Results.Ok(  )
        CATCH ex AS Exception
            // Handle any exceptions that occur during processing
            Console.WriteLine("Error: " + ex:Message)
            context:Response:StatusCode := 400 // Bad Request
            RETURN Results.BadRequest()
        END TRY
    END METHOD

END CLASS