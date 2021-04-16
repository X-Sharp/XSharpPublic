//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

namespace Microsoft.VisualStudio.Project
{
    using System;
    using System.Collections.Generic;
    using System.Globalization;
    using System.IO;
    using System.Text;
    using System.Text.RegularExpressions;

    /// <summary>
    /// Class to handle validation of the different types of votive project properties
    /// </summary>
    public static class PropertyValidator
    {

        // =========================================================================================
        // Methods
        // =========================================================================================

        /// <summary>
        /// Entry point for all property validation in  projects.
        /// </summary>
        /// <param name="propertyName">Name of the property being validated. (The name in the project file, not the localized name.)</param>
        /// <param name="value">Property value to be validated.</param>
        public static void ValidateProperty(string propertyName, string value)
        {
            XHelperMethods.VerifyNonNullArgument(propertyName, "propertyName");
            XHelperMethods.VerifyNonNullArgument(value, "value");

            switch (propertyName)
            {
                case XProjectFileConstants.Cultures:
                    ValidateCultures(propertyName, value);
                    break;

                case XProjectFileConstants.OutputName:
                    ValidateFilename(propertyName, value);
                    break;

                case XProjectFileConstants.IntermediateOutputPath:
                case XProjectFileConstants.OutputPath:
                    ValidatePath(propertyName, value);
                    break;

                case XProjectFileConstants.IncludeSearchPaths:
                    ValidatePath(propertyName, value);
                    break;

                case XProjectFileConstants.ReferencePaths:
                    ValidatePath(propertyName, value);
                    break;
            }
        }

        /// <summary>
        /// Validates a string representing a list of cultures.
        /// </summary>
        /// <param name="propertyName">Localized name of the property being validated.</param>
        /// <param name="cultures">The string to be validated.</param>
        private static void ValidateCultures(string propertyName, string cultures)
        {
            if (cultures.Length != 0)
            {
                if (cultures[cultures.Length - 1] == ';')
                {
                    cultures = cultures.Substring(0, cultures.Length - 1);
                }

                string[] cultureGroupArray = cultures.Split(';');
                foreach (string cultureGroup in cultureGroupArray)
                {
                    string[] culturesArray = cultureGroup.Split(',');
                    foreach (string cultureName in culturesArray)
                    {
                        string trimCultureName = cultureName.Trim();
                        if (trimCultureName.Equals("neutral", StringComparison.OrdinalIgnoreCase))
                        {
                            trimCultureName = String.Empty;
                        }

                        try
                        {
                            CultureInfo.GetCultureInfo(trimCultureName);
                        }
                        catch (ArgumentException)
                        {
                            string errorMessage = String.Format(CultureInfo.CurrentUICulture, Microsoft.VisualStudio.Project.Wix.XStrings.InvalidCultureError, propertyName, trimCultureName);
                            throw new ProjectPropertyArgumentException(errorMessage);
                        }
                    }
                }
            }
        }

        /// <summary>
        /// Validates a file name.
        /// </summary>
        /// <param name="propertyName">Localized name of the property being validated.</param>
        /// <param name="fileName">Name of a file to be validated.</param>
        private static void ValidateFilename(string propertyName, string fileName)
        {
            if (fileName.Length == 0)
            {
                throw new ProjectPropertyArgumentException(String.Format(CultureInfo.CurrentUICulture, Wix.XStrings.InvalidFileNameEmpty, propertyName));
            }

            List<char> invalidChars = new List<char>(Path.GetInvalidFileNameChars());
            invalidChars.AddRange(new char[] { '*', '?' });

            ValidateChars(propertyName, fileName, invalidChars, Wix.XStrings.InvalidFileNameInvalidChar);
        }

        /// <summary>
        /// Validates a file or directory path.
        /// </summary>
        /// <param name="propertyName">Localized name of the property being validated.</param>
        /// <param name="filePath">Path to be validated.</param>
        private static void ValidatePath(string propertyName, string filePath)
        {
            List<char> invalidChars = new List<char>(Path.GetInvalidPathChars());
            invalidChars.AddRange(new char[] { '*', '?' });

            ValidateChars(propertyName, filePath, invalidChars, Wix.XStrings.InvalidPath);
        }

        /// <summary>
        /// Validates that a property value doesn't contain any characters from a list of invalid characters.
        /// </summary>
        /// <param name="propertyName">Localized name of the property being validated.</param>
        /// <param name="value">Property value being validated.</param>
        /// <param name="invalidChars">List of characters not allowed in the property value.</param>
        /// <param name="message">Error message format string to throw when validation fails.</param>
        private static void ValidateChars(string propertyName, string value, IList<char> invalidChars, string message)
        {
            foreach (char c in invalidChars)
            {
                string escapeCode = String.Format(CultureInfo.InvariantCulture, "%{0:x2}", (int)c);
                if (value.IndexOf(Convert.ToString(c, CultureInfo.InvariantCulture), StringComparison.Ordinal) >= 0 || value.IndexOf(escapeCode, StringComparison.Ordinal) >= 0)
                {
                    throw new ProjectPropertyArgumentException(String.Format(CultureInfo.CurrentUICulture, message, propertyName));
                }
            }
        }

        /// <summary>
        /// Validates that a string property value matches a regular expression.
        /// </summary>
        /// <param name="propertyName">Localized name of the property being validated.</param>
        /// <param name="value">Property value being validated.</param>
        /// <param name="regex">Validation regular expresion.</param>
        /// <param name="allowEmpty">Whether to allow an empty string as a valid value.</param>
        /// <param name="message">Error message format string to throw when validation fails.</param>
        private static void ValidateWithRegex(string propertyName, string value, Regex regex, bool allowEmpty, string message)
        {
            if (!((allowEmpty && value.Length == 0) || regex.IsMatch(value)))
            {
                throw new ProjectPropertyArgumentException(String.Format(CultureInfo.CurrentUICulture, message, propertyName));
            }
        }
    }
}
