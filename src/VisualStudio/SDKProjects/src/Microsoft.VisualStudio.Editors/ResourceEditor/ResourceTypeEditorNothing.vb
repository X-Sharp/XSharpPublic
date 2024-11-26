﻿' Licensed to the .NET Foundation under one or more agreements. The .NET Foundation licenses this file to you under the MIT license. See the LICENSE.md file in the project root for more information.

Option Explicit On
Option Strict On
Option Compare Binary

Imports Microsoft.VisualStudio.Editors.Package

Namespace Microsoft.VisualStudio.Editors.ResourceEditor

    ''' <summary>
    ''' A resource type editor for resx file entries of the type ResXFileRef (which simply means that
    '''   the resource's value is Nothing (null).
    ''' </summary>
    ''' <remarks>We show "Nothing/null" in the string table, and do not allow the end user
    '''   to change the value.</remarks>
    Friend NotInheritable Class ResourceTypeEditorNothing
        Inherits ResourceTypeEditorStringBase

        '======================================================================
        '= PROPERTIES =                                                       =
        '======================================================================

        ''' <summary>
        ''' Returns whether this resource type should be displayed in a string table or not.
        ''' </summary>
        ''' <value>True if this resources handled by this ResourceTypeEditor should be displayed
        '''   in a string table, and False if they should be displayed in a listview.</value>
        Public Overrides ReadOnly Property StringValueCanBeEdited As Boolean
            Get
                'Can't change a Nothing/null value to anything else.
                Return False
            End Get
        End Property

        '======================================================================
        '= METHODS =                                                          =
        '======================================================================

        ''' <summary>
        ''' Given a resource, returns a string formatted to display the value of the resource.  This is the inverse
        '''   of StringParseFormattedCellValue().
        ''' </summary>
        ''' <param name="Resource">The Resource instance for which this conversion is being made.</param>
        ''' <param name="ResourceValue">The resource value.  May not be Nothing.  The value of the resource to save.  Must be of the type handled by this ResourceTypeEditor.</param>
        ''' <returns>The formatted string.</returns>
        ''' <remarks>
        '''   Caller is responsible for displaying exceptions thrown by this method.
        '''   This function only needs to be implemented if StringValueCanBeEdited returns True for the class.
        '''   This function is not exposed publicly, because the class it is defined on is not public.
        ''' </remarks>
        Public Overrides Function StringGetFormattedCellValue(Resource As Resource, ResourceValue As Object) As String
            'Don't validate the ResourceValue - it is allowed to be Nothing for this one resource type editor class.
            If Resource Is Nothing Then
                Debug.Fail("Resource shouldn't be nothing")
                Throw New InternalException
            End If

            'We simply display a value indicating to the user that this is a Nothing/null value.
            Return My.Resources.Microsoft_VisualStudio_Editors_Designer.RSE_NothingValue
        End Function

    End Class

End Namespace
