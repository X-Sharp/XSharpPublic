USING System


FUNCTION Start( args AS STRING[] ) AS VOID
    LOCAL app AS RESTful
    //
    System.Text.Encoding.RegisterProvider(System.Text.CodePagesEncodingProvider.Instance)
    //
    app := RESTful{}
    // Run the RESTful application
    app:Run(args)
    // Wait for the application to finish
END FUNCTION


