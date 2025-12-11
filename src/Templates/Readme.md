# Pre requisites

In order to generate the Templates you will need to install the .NET Template Engine :  

    dotnet new install Microsoft.TemplateEngine.Authoring.Templates

# Creation
Now, create a folder/project that will contains the templates. ( **working** currently )

    dotnet new templatepack -n "Templates.XSharp.NextVersion"
	
*Notice that we don't have XSharp.NextVersion.Templates : This will conflicts with the packageSourceMapping in Nuget.Config and block the dotnet pack*	
	

### Create the various templates : 
You can follow the [tutorial](https://learn.microsoft.com/fr-fr/dotnet/core/tutorials/cli-templates-create-project-template)

Let's say that you have 2 templates with the following structure :  
````  
working  
│   XSharp.NextVersion.Templates.csproj  
└───content  
    ├───anotherOne  
    │   └───.template.config  
    │           template.json  
    └───console  
        └───.template.config  
                template.json  
````
# Build
Go to the working folder, open a terminal and run `dotnet pack`.