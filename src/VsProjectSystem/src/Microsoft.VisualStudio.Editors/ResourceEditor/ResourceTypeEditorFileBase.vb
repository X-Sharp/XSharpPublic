﻿' Licensed to the .NET Foundation under one or more agreements. The .NET Foundation licenses this file to you under the MIT license. See the LICENSE.md file in the project root for more information.

Option Explicit On
Option Strict On
Option Compare Binary
Imports System.IO

Namespace Microsoft.VisualStudio.Editors.ResourceEditor

    Friend MustInherit Class ResourceTypeEditorFileBase
        Inherits ResourceTypeEditorInternalBase

        ''' <summary>
        ''' Checks the resource's value for any errors.  If any error is found it should be indicated by throwing
        '''    an exception.  These errors will be displayed automatically by the callee in the task list.
        ''' For most resource type editors, the default implementation will be sufficient, which simply calls
        '''    IResource.GetValue() and lets any exceptions bubble up.
        ''' </summary>
        ''' <param name="Resource">The IResource instance.  May not be Nothing.  The value of the resource.  Must be of the type handled by this ResourceTypeEditor.</param>
        ''' <remarks>
        ''' The default version of this function calls IResource.GetValue().
        ''' </remarks>
        Public Overrides Sub CheckValueForErrors(Resource As IResource)
            ValidateResourceValue(Resource, ResourceTypeEditorBinaryFile.BinaryFileValueType, ResourceTypeEditorTextFile.TextFileValueType)
            Debug.Assert(Resource.IsLink)

            'For binary and text files, there's no reason to ever actually try to load the file from disk in order
            '  to check for errors.  The only errors that can happen are memory or IO related, and these they'll
            '  get at compile time.  Thus, we'll save the perf and memory hit from doing this for large files in the
            '  editor.
            '
            'Therefore, the only check we need to make is for file not found.
            If Not File.Exists(Resource.LinkedFilePath) Then
                Throw New FileNotFoundException() 'No need to add a message - it will be changed by the callee anyway.
            End If
        End Sub

        ''' <summary>
        ''' Indicates whether the resources edited by this resource editor type are allowed to have their
        '''   Persistence property changed.
        ''' </summary>
        ''' <param name="ResourceContentFile">The resource file that contains the resource</param>
        Friend Overrides ReadOnly Property CanChangePersistenceProperty(ResourceContentFile As IResourceContentFile) As Boolean
            Get
                Return False
            End Get
        End Property

    End Class
End Namespace
