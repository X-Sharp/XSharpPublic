# XSharp language *Next Version* Templates package

This package contains some projects templates for .NET 8 and greater for [XSharp language](https://www.xsharp.eu).

## Available Templates in this package  
- xsconsole         : A simple Console application, using the Core dialect.
- xsconsoleFox      : A simple Console application, using the VFP dialect.
- xsconsoleHarbour  : A simple Console application, using the Harbour dialect.
- xsconsoleVO       : A simple Console application, using the Visual Objects dialect.
- xsconsoleXPP      : A simple Console application, using the xBase++ dialect.
- xswebmap          : A RESTful application, using the Core dialect, using WebApplication MapGet/MapPost.
- xswebroute        : A RESTful application, using the Core dialect, using Attributes for routing.
- xswinforms        : A simple Windows Forms application, using the Core dialect.
- xswinformscctrli  : A CustomControl Windows Forms item, using the Core dialect.
- xswinformsctrli   : A UserControl Windows Forms item, using the Core dialect.
- xswinformslib     : A Windows Forms library, using the Core dialect.
- xswpfapp          : A WPF Application, using the Core dialect.
- xswpfcctrllib     : A WPF CustomControl library, using the Core dialect.
- xswpfctrl         : A WPF UserControl library, using the Core dialect.
- xswpflib          : A WPF Library, using the Core dialect.

## Installation 

- Download this package
- Open the download folder
- Run  
    **dotnet new install \<nupkg filename\>**

You can run `dotnet new list` to see the list of all available templates and check the installation.

## Un-Installation 
- Open a terminal
- Run  
    **dotnet new uninstall**
	
You will have a list of all installed packages and the corresponding uninstallation command.

## Usage example
- Create a folder, and name the folder with the name of the desired project  
- Open a terminal in that folder  
- Run `dotnet new xsconsole` to create a new console project in that folder.

In order to build the project, you can run
`dotnet build` in that folder

In order to run (and build if needed) the project, you can run
`dotnet run` in that folder

## Usage with VSCode

The `xsconsole` template comes with some basic setup to build and run the application.  
After running the `dotnet new xsconsole` command, you can type `code .`in order to run VSCode and open that folder.  

You can then modify the source, and run the program using the **Run/Run without debugging** menu.