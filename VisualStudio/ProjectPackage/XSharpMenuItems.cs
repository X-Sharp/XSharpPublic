//------------------------------------------------------------------------------
// <copyright file="XPorter.cs" company="Company">
//     Copyright (c) Company.  All rights reserved.
// </copyright>
//------------------------------------------------------------------------------

using System;
using System.ComponentModel.Design;
using System.Globalization;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;

namespace XSharp.Project
{
    /// <summary>
    /// Command handler
    /// </summary>
    internal sealed class XSharpMenuItems
    {
        /// <summary>
        /// Command ID.
        /// </summary>
        public const int IdXporter = 0x0100;
        public const int IdWebsite = 0x0101;
        public const int IdOnlineHelp = 0x0102;
        public const int IdVOXporter = 0x0103;

        /// <summary>
        /// Command menu group (command set GUID).
        /// </summary>
        public static readonly Guid CommandSet = new Guid("b8210244-d368-416c-8130-a669ef4297f6");

        /// <summary>
        /// VS Package that provides this command, not null.
        /// </summary>
        private readonly Package package;

        /// <summary>
        /// Initializes a new instance of the <see cref="XSharpMenuItems"/> class.
        /// Adds our command handlers for menu (commands must exist in the command table file)
        /// </summary>
        /// <param name="package">Owner package, not null.</param>
        private XSharpMenuItems(Package package)
        {
            if (package == null)
            {
                throw new ArgumentNullException("package");
            }

            this.package = package;

            OleMenuCommandService commandService = this.ServiceProvider.GetService(typeof(IMenuCommandService)) as OleMenuCommandService;
            if (commandService != null)
            {
                var menuCommandID = new CommandID(CommandSet, IdXporter);
                var menuItem = new MenuCommand(this.StartXPorter, menuCommandID);
                commandService.AddCommand(menuItem);
                menuCommandID = new CommandID(CommandSet, IdVOXporter);
                menuItem = new MenuCommand(this.StartXPorter, menuCommandID);
                commandService.AddCommand(menuItem);
                // repeat this with different named event handlers and Ids for other menu points
                menuCommandID = new CommandID(CommandSet, IdWebsite);
                menuItem = new MenuCommand(this.OpenWebsite, menuCommandID);
                commandService.AddCommand(menuItem);
                menuCommandID = new CommandID(CommandSet, IdOnlineHelp);
                menuItem = new MenuCommand(this.OpenWebsite, menuCommandID);
                commandService.AddCommand(menuItem);
            }
        }

        /// <summary>
        /// Gets the instance of the command.
        /// </summary>
        public static XSharpMenuItems Instance
        {
            get;
            private set;
        }

        /// <summary>
        /// Gets the service provider from the owner package.
        /// </summary>
        private IServiceProvider ServiceProvider
        {
            get
            {
                return this.package;
            }
        }

        /// <summary>
        /// Initializes the singleton instance of the command.
        /// </summary>
        /// <param name="package">Owner package, not null.</param>
        public static void Initialize(Package package)
        {
            Instance = new XSharpMenuItems(package);
        }

        /// <summary>
        /// This function is the callback used to execute the command when the menu item is clicked.
        /// See the constructor to see how the menu item is associated with this function using
        /// OleMenuCommandService service and MenuCommand class.
        /// </summary>
        /// <param name="sender">Event sender.</param>
        /// <param name="e">Event args.</param>
        private void StartXPorter(object sender, EventArgs e)
        {
            string REG_KEY = @"HKEY_LOCAL_MACHINE\" + XSharp.Constants.RegistryKey;
            string InstallPath = (string)Microsoft.Win32.Registry.GetValue(REG_KEY, XSharp.Constants.RegistryValue, "");
            MenuCommand cmd = sender as MenuCommand;
            string xporterPath = null;
            if (cmd.CommandID.ID == IdXporter)
            {
                xporterPath = System.IO.Path.Combine(InstallPath, @"Bin\Xporter.exe");
            }
            else if (cmd.CommandID.ID == IdVOXporter)
            {
                xporterPath = System.IO.Path.Combine(InstallPath, @"VOXPorter\VOXporter.exe");
            }
            if (!String.IsNullOrEmpty(xporterPath))
            {
                if (System.IO.File.Exists(xporterPath))
                {
                    var info = new System.Diagnostics.ProcessStartInfo();
                    info.FileName = xporterPath;
                    info.WorkingDirectory = System.IO.Path.GetDirectoryName(xporterPath);
                    System.Diagnostics.Process.Start(info);
                }
                else
                {
                    VsShellUtilities.ShowMessageBox(ServiceProvider, "Cannot find file \"" + xporterPath+"\"", "Can't start external program", OLEMSGICON.OLEMSGICON_CRITICAL, OLEMSGBUTTON.OLEMSGBUTTON_OK, OLEMSGDEFBUTTON.OLEMSGDEFBUTTON_FIRST);
                }
            }

        }
        /// <summary>
        /// This function is the callback used to execute the command when the menu item is clicked.
        /// See the constructor to see how the menu item is associated with this function using
        /// OleMenuCommandService service and MenuCommand class.
        /// </summary>
        /// <param name="sender">Event sender.</param>
        /// <param name="e">Event args.</param>
        private void OpenWebsite(object sender, EventArgs e)
        {
            MenuCommand cmd = sender as MenuCommand;


            switch (cmd.CommandID.ID)
            {
                case IdWebsite:
                    //System.Diagnostics.Process.Start("https://www.xsharp.info");
                    ((XSharpProjectPackage)package).OpenInBrowser("https://www.xsharp.info");
                    break;
                case IdOnlineHelp:
                    string REG_KEY = @"HKEY_LOCAL_MACHINE\" + XSharp.Constants.RegistryKey;
                    string InstallPath = (string)Microsoft.Win32.Registry.GetValue(REG_KEY, XSharp.Constants.RegistryValue, "");
                    string docPath = System.IO.Path.Combine(InstallPath, @"Help\XSharp.chm");
                    if (System.IO.File.Exists(docPath))
                    {
                        System.Diagnostics.Process.Start(docPath);
                    }
                    else
                    {
                        VsShellUtilities.ShowMessageBox(ServiceProvider, "Cannot find file \"" + docPath + "\"", "Can't show documentation", OLEMSGICON.OLEMSGICON_CRITICAL, OLEMSGBUTTON.OLEMSGBUTTON_OK, OLEMSGDEFBUTTON.OLEMSGDEFBUTTON_FIRST);
                    }

                    break;
            }

        }
    }
}
