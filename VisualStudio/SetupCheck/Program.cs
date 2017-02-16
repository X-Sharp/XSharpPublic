// <copyright file="Program.cs" company="Microsoft Corporation">
// Copyright (C) Microsoft Corporation. All rights reserved.
// Licensed under the MIT license. See LICENSE.txt in the project root for license information.
// </copyright>

using System;
using System.Linq;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio.Setup.Configuration;
using System.Text;

/// <summary>
/// The main program class.
/// </summary>
internal class Program
{
    private const int REGDB_E_CLASSNOTREG = unchecked((int)0x80040154);
    private const string FILENAME = "vs2017.txt";
    /// <summary>
    /// Program entry point.
    /// </summary>
    /// <param name="args">Command line arguments passed to the program.</param>
    /// <returns>The process exit code.</returns>

    internal static int Main(string[] args)
    {
        try
        {
            System.IO.File.Delete(FILENAME);
            var query = GetQuery();
            var query2 = (ISetupConfiguration2)query;
            var e = query2.EnumAllInstances();

            var helper = (ISetupHelper)query;

            int fetched;
            var instances = new ISetupInstance[1];
            do
            {
                e.Next(1, instances, out fetched);
                if (fetched > 0)
                {
                    PrintInstance(instances[0], helper);
                }
            }
            while (fetched > 0);

            return 0;
        }
        catch (Exception ex)
        {
            StringBuilder sb = new StringBuilder();
            sb.AppendLine($"Error 0x{ex.HResult:x8}: {ex.Message}");
            Console.Write(sb.ToString());
            System.IO.File.AppendAllText(FILENAME, sb.ToString());
            return ex.HResult;
        }
    }

    private static ISetupConfiguration GetQuery()
    {
        try
        {
            // Try to CoCreate the class object.
            return new SetupConfiguration();
        }
        catch (COMException ex) when (ex.HResult == REGDB_E_CLASSNOTREG)
        {
            // Try to get the class object using app-local call.
            ISetupConfiguration query;
            var result = GetSetupConfiguration(out query, IntPtr.Zero);

            if (result < 0)
            {
                throw new COMException("Failed to get query", result);
            }

            return query;
        }
    }

    private static void PrintInstance(ISetupInstance instance, ISetupHelper helper)
    {
        var instance2 = (ISetupInstance2)instance;
        var state = instance2.GetState();
        var sb = new StringBuilder();
                       

        sb.AppendLine($"InstanceId: {instance2.GetInstanceId()} ({(state == InstanceState.Complete ? "Complete" : "Incomplete")})");

        var installationVersion = instance.GetInstallationVersion();
        var version = helper.ParseVersion(installationVersion);

        sb.AppendLine($"InstallationVersion: {installationVersion} ({version})");

        if ((state & InstanceState.Local) == InstanceState.Local)
        {
            sb.AppendLine($"InstallationPath: {instance2.GetInstallationPath()}");
        }

        if ((state & InstanceState.Registered) == InstanceState.Registered)
        {
            sb.AppendLine($"Product: {instance2.GetProduct().GetId()}");
            sb.AppendLine("Workloads:");

            PrintWorkloads(sb, instance2.GetPackages());
        }

        sb.AppendLine();
        Console.Write(sb.ToString());
        System.IO.File.AppendAllText(FILENAME, sb.ToString());
    }

    private static void PrintWorkloads(StringBuilder sb, ISetupPackageReference[] packages)
    {
        var workloads = from package in packages
                        where string.Equals(package.GetType(), "Workload", StringComparison.OrdinalIgnoreCase)
                        orderby package.GetId()
                        select package;

        foreach (var workload in workloads)
        {
            sb.AppendLine($"    {workload.GetId()}");
        }
    }

    [DllImport("Microsoft.VisualStudio.Setup.Configuration.Native.dll", ExactSpelling = true, PreserveSig = true)]
    private static extern int GetSetupConfiguration(
        [MarshalAs(UnmanagedType.Interface), Out] out ISetupConfiguration configuration,
        IntPtr reserved);
}
