USING System
USING System.IO
USING System.Text
USING System.Net
USING System.Net.Sockets
USING System.Linq
USING System.Runtime.CompilerServices
USING Microsoft.AspNetCore.Builder
USING Microsoft.AspNetCore.Routing
USING Microsoft.AspNetCore.Hosting
USING Microsoft.AspNetCore.Http
USING Microsoft.Extensions.DependencyInjection
USING Microsoft.Extensions.Hosting
USING System.Threading.Tasks
using System.Text.Json
USING System.Collections.Generic


CLASS RESTful

    PROPERTY Data AS Dictionary<int, DataUser> AUTO

    CONSTRUCTOR()
        // Constructor for RESTful class
        // This can be used to initialize any necessary properties or fields
        SELF:Data := Dictionary<int, DataUser>{}
        // Add some sample data
        FOR VAR i := 0 TO 4
            LOCAL o AS DataUser
            o := DataUser{}
            o:Id := i
            o:Name := "User " + i:ToString()
            SELF:Data:Add(i, o)
        NEXT
    END CONSTRUCTOR

    METHOD Run( args AS STRING[] ) AS VOID
        LOCAL Builder AS WebApplicationBuilder
        LOCAL app AS WebApplication
        //
        Builder := WebApplication.CreateBuilder(args)
        app := Builder:Build()
        // Build a Root with a Lambda function to handle requests
        app:MapGet("/", { context AS HttpContext =>
            // Handle the request and return a response
            context:Response:ContentType := "text/plain"
            context:Response:WriteAsync("Hello World!")
            RETURN Task.CompletedTask
        })
        // Build a Root with a Lambda function to handle requests
        app:MapGet("/users", { context AS HttpContext => RouteUsers{this}:ReplyGet(context) })
        // Build a Root with a Function
        app:MapPost("/id", { context AS HttpContext => RouteId{this}:Reply(context) })
        // Build a Root with a Function
        app:MapPost("/users/{id:int}", { context AS HttpContext => RouteUsers{this}:ReplyPost(context) })
        //
        app:Run()
    END METHOD
END CLASS