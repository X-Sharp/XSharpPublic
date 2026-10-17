// Copyright (c) XSharp BV.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.

namespace XSharp.ProjectImages
{
    /// <summary>
    /// Identifiers of the images in XSharp.ProjectImages.imagemanifest.
    /// Together with <see cref="XSharpImageIds.ImageGuid"/> they form the ImageMoniker for an image.
    /// </summary>
    public static class XSharpImageIds
    {
        /// <summary>Guid of the image manifest. Must match the ImageGuid symbol in the manifest and in Menus.vsct</summary>
        public static readonly System.Guid ImageGuid = new System.Guid("34382b2d-f265-42d7-88f0-3423331e7210");

        // Ids of the images, extracted from the old project imagelist (16x16)
        public const int Image1 = 1;
        public const int Image2 = 2;
        public const int Image3 = 3;
        public const int Image4 = 4;
        public const int Image5 = 5;
        public const int Image6 = 6;
        public const int Image7 = 7;
        public const int Image8 = 8;
        public const int Image9 = 9;
        public const int Image10 = 10;
        public const int Image11 = 11;
        public const int Image12 = 12;
        public const int Image13 = 13;
        public const int Image14 = 14;
        public const int Image15 = 15;
        public const int Image16 = 16;
        public const int Image17 = 17;
        public const int Image18 = 18;
        public const int Image19 = 19;

        // Meaningful names for the images above
        public const int Project = Image1;
        public const int Source = Image2;
        public const int Form = Image3;
        public const int Server = Image4;
        public const int FieldSpec = Image5;
        public const int Menu = Image6;
        public const int VO = Image7;
        public const int SQLTable = Image8;
        public const int Test = Image9;
        public const int Report = Image10;
        public const int Reference = Image11;
        public const int DanglingReference = Image12;
        public const int TabOrder = Image13;
        public const int FoxPro = Image14;
        public const int NuGet = Image15;
        public const int Header = Image16;
        public const int IncludeFolder = Image17;
        public const int ReferenceGroup = Image18;
        public const int MissingFile = Image19;
    }
}
