//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.Build.Utilities;
using Microsoft.Build.Framework;
using System.IO;

namespace XSharp.Build
{

    class Utilities
    {

        internal static bool CopyFileSafe(string source, string target)
        {
            try
            {
                if (System.IO.File.Exists(target))
                {
                    System.IO.File.SetAttributes(target, FileAttributes.Normal);
                    System.IO.File.Delete(target);
                }
                System.IO.File.Copy(source, target, true);
                return true;
            }
            catch (Exception e)
            {
                System.Diagnostics.Debug.WriteLine(e.Message);
                return false;
            }
        }
        internal static string AddSlash(string Path)
        {
            if (!String.IsNullOrEmpty(Path) && !Path.EndsWith("\\"))
                Path += "\\";
            return Path;

        }


        internal static bool TryConvertItemMetadataToBool(ITaskItem item, string itemMetadataName)
        {
            string metadataValue = item.GetMetadata(itemMetadataName);
            if (metadataValue == null || metadataValue.Length == 0)
            {
                return false;
            }

            try
            {
                return Utilities.ConvertStringToBool(metadataValue);
            }
            catch (System.ArgumentException)
            {
                throw new Exception("Invalid metadata attribute: " + itemMetadataName + " " + metadataValue);
            }
        }


        /// <summary>
        /// Converts a string to a bool.  We consider "true/false", "on/off", and
        /// "yes/no" to be valid boolean representations in the XML.
        /// </summary>
        /// <param name="parameterValue">The string to convert.</param>
        /// <returns>Boolean true or false, corresponding to the string.</returns>
        internal static bool ConvertStringToBool(string parameterValue)
        {
            if (ValidBooleanTrue(parameterValue))
            {
                return true;
            }
            else if (ValidBooleanFalse(parameterValue))
            {
                return false;
            }
            else
            {
                // Unsupported boolean representation.
                throw new Exception("Cannot convert string to bool: " + parameterValue);
            }
        }
        /// <summary>
        /// Returns true if the string represents a valid MSBuild boolean true value,
        /// such as "on", "!false", "yes"
        /// </summary>
        internal static bool ValidBooleanTrue(string parameterValue) =>
            String.Compare(parameterValue, "true", StringComparison.OrdinalIgnoreCase) == 0 ||
            String.Compare(parameterValue, "on", StringComparison.OrdinalIgnoreCase) == 0 ||
            String.Compare(parameterValue, "yes", StringComparison.OrdinalIgnoreCase) == 0 ||
            String.Compare(parameterValue, "!false", StringComparison.OrdinalIgnoreCase) == 0 ||
            String.Compare(parameterValue, "!off", StringComparison.OrdinalIgnoreCase) == 0 ||
            String.Compare(parameterValue, "!no", StringComparison.OrdinalIgnoreCase) == 0;

        /// <summary>
        /// Returns true if the string represents a valid MSBuild boolean false value,
        /// such as "!on" "off" "no" "!true"
        /// </summary>
        internal static bool ValidBooleanFalse(string parameterValue) =>
            String.Compare(parameterValue, "false", StringComparison.OrdinalIgnoreCase) == 0 ||
            String.Compare(parameterValue, "off", StringComparison.OrdinalIgnoreCase) == 0 ||
            String.Compare(parameterValue, "no", StringComparison.OrdinalIgnoreCase) == 0 ||
            String.Compare(parameterValue, "!true", StringComparison.OrdinalIgnoreCase) == 0 ||
            String.Compare(parameterValue, "!on", StringComparison.OrdinalIgnoreCase) == 0 ||
            String.Compare(parameterValue, "!yes", StringComparison.OrdinalIgnoreCase) == 0;


        /// <summary>
        /// Old VS projects had some pretty messed-up looking values for the
        /// "DefineConstants" property.  It worked fine in the IDE, because it
        /// effectively munged up the string so that it ended up being valid for
        /// the compiler.  We do the equivalent munging here now.
        ///
        /// Basically, we take the incoming string, and split it on comma/semicolon/space.
        /// Then we look at the resulting list of strings, and remove any that are
        /// illegal identifiers, and pass the remaining ones through to the compiler.
        ///
        /// Note that CSharp does support assigning a value to the constants ... in
        /// other words, a constant is either defined or not defined ... it can't have
        /// an actual value.
        /// </summary>
        internal static string GetDefineConstantsSwitch(string originalDefineConstants, TaskLoggingHelper log)
        {
            if (originalDefineConstants == null)
            {
                return null;
            }

            StringBuilder finalDefineConstants = new StringBuilder();

            // Split the incoming string on comma/semicolon/space.
            string[] allIdentifiers = originalDefineConstants.Split(new char[] { ',', ';', ' ' });

            // Loop through all the parts, and for the ones that are legal C# identifiers,
            // add them to the outgoing string.
            foreach (string singleIdentifier in allIdentifiers)
            {
                // Separate them with a semicolon if there's something already in
                // the outgoing string.
                if (finalDefineConstants.Length > 0)
                {
                    finalDefineConstants.Append(";");
                }

                finalDefineConstants.Append(singleIdentifier);
            }

            if (finalDefineConstants.Length > 0)
            {
                return finalDefineConstants.ToString();
            }
            else
            {
                // We wouldn't want to pass in an empty /define: switch on the csc.exe command-line.
                return null;
            }
        }
    }

}
