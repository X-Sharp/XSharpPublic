﻿' Licensed to the .NET Foundation under one or more agreements. The .NET Foundation licenses this file to you under the MIT license. See the LICENSE.md file in the project root for more information.

Imports System.ComponentModel.Design
Imports System.Drawing
Imports System.IO
Imports System.Reflection
Imports System.Runtime.InteropServices
Imports System.Runtime.Serialization
Imports System.Text
Imports System.Text.RegularExpressions
Imports System.Windows.Forms

Imports Microsoft.VisualStudio.Shell
Imports Microsoft.VisualStudio.Shell.Interop
Imports Microsoft.VisualStudio.Telemetry

Namespace Microsoft.VisualStudio.Editors.AppDesCommon

    Friend Module Utils

        'The transparent color used for all bitmaps in the resource editor is lime (R=0, G=255, B=0).
        '  Any pixels of this color will be converted to transparent if StandardTransparentColor
        '  is passed to GetManifestBitmap
        Public ReadOnly StandardTransparentColor As Color = Color.Lime

        Public VBPackageInstance As IVBPackage

        ' The maximal amount of files that can be added at one shot. (copied from other VS features)
        Private Const VSDPLMAXFILES As Integer = 200

        'Property page GUIDs.  These are used only for sorting the tabs in the project designer, and for providing a
        '  unique ID for SQM.  Both cases are optional (we handle getting property pages with GUIDs we don't recognize).
        'PERF: NOTE: Initializing GUIDs from numeric values as below is a lot faster than initializing from strings.
        Public NotInheritable Class KnownPropertyPageGuids
            Public Shared ReadOnly GuidApplicationPage_VB As Guid = New Guid(&H8998E48EUI, &HB89AUS, &H4034US, &HB6, &H6E, &H35, &H3D, &H8C, &H1F, &HDC, &H2E)
            Public Shared ReadOnly GuidApplicationPage_VB_WPF As Guid = New Guid(&HAA1F44UI, &H2BA3US, &H4EAAUS, &HB5, &H4A, &HCE, &H18, &H0, &HE, &H6C, &H5D)
            Public Shared ReadOnly GuidApplicationPage_CS As Guid = New Guid(&H5E9A8AC2UI, &H4F34US, &H4521US, CByte(&H85), CByte(&H8F), CByte(&H4C), CByte(&H24), CByte(&H8B), CByte(&HA3), CByte(&H15), CByte(&H32))
            Public Shared ReadOnly GuidApplicationPage_JS As Guid = GuidApplicationPage_CS
            Public Shared ReadOnly GuidSigningPage As Guid = New Guid(&HF8D6553FUI, &HF752US, &H4DBFUS, CByte(&HAC), CByte(&HB6), CByte(&HF2), CByte(&H91), CByte(&HB7), CByte(&H44), CByte(&HA7), CByte(&H92))
            Public Shared ReadOnly GuidReferencesPage_VB As Guid = New Guid(&H4E43F4ABUI, &H9F03US, &H4129US, CByte(&H95), CByte(&HBF), CByte(&HB8), CByte(&HFF), CByte(&H87), CByte(&HA), CByte(&HF6), CByte(&HAB))
            Public Shared ReadOnly GuidServicesPropPage As Guid = New Guid(&H43E38D2EUI, &H4EB8US, &H4204US, CByte(&H82), CByte(&H25), CByte(&H93), CByte(&H57), CByte(&H31), CByte(&H61), CByte(&H37), CByte(&HA4))
            Public Shared ReadOnly GuidSecurityPage As Guid = New Guid(&HDF8F7042UI, &HBB1US, &H47D1US, CByte(&H8E), CByte(&H6D), CByte(&HDE), CByte(&HB3), CByte(&HD0), CByte(&H76), CByte(&H98), CByte(&HBD))
            Public Shared ReadOnly GuidSecurityPage_WPF As Guid = New Guid(&HA2C8FEUI, &H3844US, &H41BEUS, CByte(&H96), CByte(&H37), CByte(&H16), CByte(&H74), CByte(&H54), CByte(&HA7), CByte(&HF1), CByte(&HA7))
            Public Shared ReadOnly GuidPublishPage As Guid = New Guid(&HCC4014F5UI, &HB18DUS, &H439CUS, CByte(&H93), CByte(&H52), CByte(&HF9), CByte(&H9D), CByte(&H98), CByte(&H4C), CByte(&HCA), CByte(&H85))
            Public Shared ReadOnly GuidDebugPage As Guid = New Guid(&H6185191FUI, &H1008US, &H4FB2US, CByte(&HA7), CByte(&H15), CByte(&H3A), CByte(&H4E), CByte(&H4F), CByte(&H27), CByte(&HE6), CByte(&H10))
            Public Shared ReadOnly GuidCompilePage_VB As Guid = New Guid(&HEDA661EAUI, &HDC61US, &H4750US, CByte(&HB3), CByte(&HA5), CByte(&HF6), CByte(&HE9), CByte(&HC7), CByte(&H40), CByte(&H60), CByte(&HF5))
            Public Shared ReadOnly GuidBuildPage_CS As Guid = New Guid(&HA54AD834UI, &H9219US, &H4AA6US, CByte(&HB5), CByte(&H89), CByte(&H60), CByte(&H7A), CByte(&HF2), CByte(&H1C), CByte(&H3E), CByte(&H26))
            Public Shared ReadOnly GuidBuildPage_JS As Guid = New Guid(&H8ADF8DB1UI, &HA8B8US, &H4E04US, CByte(&HA6), CByte(&H16), CByte(&H2E), CByte(&HFC), CByte(&H59), CByte(&H5F), CByte(&H27), CByte(&HF4))
            Public Shared ReadOnly GuidReferencePathsPage As Guid = New Guid(&H31911C8UI, &H6148US, &H4E25US, CByte(&HB1), CByte(&HB1), CByte(&H44), CByte(&HBC), CByte(&HA9), CByte(&HA0), CByte(&HC4), CByte(&H5C))
            Public Shared ReadOnly GuidBuildEventsPage As Guid = New Guid(&H1E78F8DBUI, &H6C07US, &H4D61US, CByte(&HA1), CByte(&H8F), CByte(&H75), CByte(&H14), CByte(&H1), CByte(&HA), CByte(&HBD), CByte(&H56))
            Public Shared ReadOnly GuidDatabasePage_SQL As Guid = New Guid(&H87F6ADCEUI, &H9161US, &H489FUS, CByte(&H90), CByte(&H7E), CByte(&H39), CByte(&H30), CByte(&HA6), CByte(&H42), CByte(&H96), CByte(&H9))
            Public Shared ReadOnly GuidFxCopPage As Guid = New Guid(&H984AE51AUI, &H4B21US, &H44E7US, CByte(&H82), CByte(&H2C), CByte(&HDD), CByte(&H5E), CByte(&H4), CByte(&H68), CByte(&H93), CByte(&HEF))
            Public Shared ReadOnly GuidDeployPage As Guid = New Guid(&H29AB1D1BUI, &H10E8US, &H4511US, CByte(&HA3), CByte(&H62), CByte(&HEF), CByte(&H15), CByte(&H71), CByte(&HB8), CByte(&H44), CByte(&H3C))
            Public Shared ReadOnly GuidDevicesPage_VSD As Guid = New Guid(&H7B74AADFUI, &HACA4US, &H410EUS, CByte(&H8D), CByte(&H4B), CByte(&HAF), CByte(&HE1), CByte(&H19), CByte(&H83), CByte(&H5B), CByte(&H99))
            Public Shared ReadOnly GuidDebugPage_VSD As Guid = New Guid(&HAC5FAEC7UI, &HD452US, &H4AC1US, CByte(&HBC), CByte(&H44), CByte(&H2D), CByte(&H7E), CByte(&HCE), CByte(&H6D), CByte(&HF0), CByte(&H6C))
            Public Shared ReadOnly GuidMyExtensionsPage As Guid = New Guid(&HF24459FCUI, &HE883US, &H4A8EUS, CByte(&H9D), CByte(&HA2), CByte(&HAE), CByte(&HF6), CByte(&H84), CByte(&HF0), CByte(&HE1), CByte(&HF4))
            Public Shared ReadOnly GuidOfficePublishPage As Guid = New Guid(&HCC7369A8UI, &HB9B0US, &H439CUS, CByte(&HB1), CByte(&H36), CByte(&HBA), CByte(&H95), CByte(&H58), CByte(&H19), CByte(&HF7), CByte(&HF8))
            Public Shared ReadOnly GuidServicesPage As Guid = New Guid(&H43E38D2EUI, &H43B8US, &H4204US, CByte(&H82), CByte(&H25), CByte(&H93), CByte(&H57), CByte(&H31), CByte(&H61), CByte(&H37), CByte(&HA4))
            Public Shared ReadOnly GuidWAPWebPage As Guid = New Guid(&H909D16B3UI, &HC8E8US, &H43D1US, CByte(&HA2), CByte(&HB8), CByte(&H26), CByte(&HEA), CByte(&HD), CByte(&H4B), CByte(&H6B), CByte(&H57))
        End Class

        ''' <summary>
        ''' Helper to convert ItemIds or other 32 bit ID values
        ''' where it is sometimes treated as an Int32 and sometimes UInt32
        ''' ItemId is sometimes marshaled as a VT_INT_PTR, and often declared 
        ''' UInt in the interop assemblies. Otherwise we get overflow exceptions converting 
        ''' negative numbers to UInt32.  We just want raw bit translation.
        ''' </summary>
        ''' <param name="obj"></param>
        Public Function NoOverflowCUInt(obj As Object) As UInteger
            Return NoOverflowCUInt(CLng(obj))
        End Function

        ''' <summary>
        ''' Masks the top 32 bits to get just the lower 32bit number
        ''' </summary>
        ''' <param name="LongValue"></param>
        Public Function NoOverflowCUInt(LongValue As Long) As UInteger
            Return CUInt(LongValue And UInteger.MaxValue)
        End Function

        ''' <summary>
        ''' Retrieves a given bitmap from the manifest resources (unmodified)
        ''' </summary>
        ''' <param name="BitmapID">Name of the bitmap resource (not including the assembly name, e.g. "Link.bmp")</param>
        ''' <param name="assembly">Name of the assembly containing the resource</param>
        ''' <returns>The retrieved bitmap</returns>
        ''' <remarks>Throws an internal exception if the bitmap cannot be found or loaded.</remarks>
        Public Function GetManifestBitmap(BitmapID As String, Optional ByRef assembly As Assembly = Nothing) As Bitmap
            Return DirectCast(GetManifestImage(BitmapID, assembly), Bitmap)
        End Function

        ''' <summary>
        ''' Retrieves a transparent copy of a given bitmap from the manifest resources.
        ''' </summary>
        ''' <param name="BitmapID">Name of the bitmap resource (not including the assembly name, e.g. "Link.bmp")</param>
        ''' <param name="TransparentColor">The color that represents transparent in the bitmap</param>
        ''' <param name="assembly">Name of the assembly containing the bitmap resource</param>
        ''' <returns>The retrieved transparent bitmap</returns>
        ''' <remarks>Throws an internal exception if the bitmap cannot be found or loaded.</remarks>
        Public Function GetManifestBitmapTransparent(BitmapID As String, ByRef TransparentColor As Color, Optional assembly As Assembly = Nothing) As Bitmap
            Dim Bitmap As Bitmap = GetManifestBitmap(BitmapID, assembly)
            If Bitmap IsNot Nothing Then
                Bitmap.MakeTransparent(TransparentColor)
                Return Bitmap
            Else
                Debug.Fail("Couldn't find internal resource")
                Throw New Package.InternalException(String.Format(My.Resources.Designer.RSE_Err_Unexpected_NoResource_1Arg, BitmapID))
            End If
        End Function

        ''' <summary>
        ''' Retrieves a transparent copy of a given bitmap from the manifest resources.
        ''' </summary>
        ''' <param name="BitmapID">Name of the bitmap resource (not including the assembly name, e.g. "Link.bmp")</param>
        ''' <param name="assembly">Name of assembly containing the manifest resource</param>
        ''' <returns>The retrieved transparent bitmap</returns>
        ''' <remarks>Throws an internal exception if the bitmap cannot be found or loaded.</remarks>
        Public Function GetManifestBitmapTransparent(BitmapID As String, Optional ByRef assembly As Assembly = Nothing) As Bitmap
            Return GetManifestBitmapTransparent(BitmapID, StandardTransparentColor, assembly)
        End Function

        ''' <summary>
        ''' Retrieves a given image from the manifest resources.
        ''' </summary>
        ''' <param name="ImageID">Name of the bitmap resource (not including the assembly name, e.g. "Link.bmp")</param>
        ''' <param name="assembly"></param>
        ''' <returns>The retrieved bitmap</returns>
        ''' <remarks>Throws an internal exception if the bitmap cannot be found or loaded.</remarks>
        Public Function GetManifestImage(ImageID As String, Optional ByRef assembly As Assembly = Nothing) As Image
            Dim BitmapStream As Stream = GetType(Utils).Assembly.GetManifestResourceStream(ImageID)
            If assembly IsNot Nothing Then
                BitmapStream = assembly.GetManifestResourceStream(ImageID)
            End If
            If BitmapStream IsNot Nothing Then
                Dim Image As Image = Image.FromStream(BitmapStream)
                If Image IsNot Nothing Then
                    Return Image
                End If
                Debug.Fail("Unable to find image resource from manifest: " & ImageID)
            Else
                Debug.Fail("Unable to find image resource from manifest: " & ImageID)
            End If
            Throw New Package.InternalException(String.Format(My.Resources.Designer.RSE_Err_Unexpected_NoResource_1Arg, ImageID))
        End Function

        ''' <summary>
        ''' Logical implies.  Often useful in Debug.Assert's.  Essentially, it is to be
        '''   read as "a being true implies that b is true".  Therefore, the function returns
        '''  False if a is true and b is false.  Otherwise it returns True (as there's no
        '''   evidence to suggest that the implication is incorrect).
        ''' </summary>
        Public Function Implies(a As Boolean, b As Boolean) As Boolean
            Return Not (a And Not b)
        End Function

        ''' <summary>
        ''' Retrieves the error message from an exception in a manner appropriate for the build.  For release, simply
        '''   retrieves ex.Message (just the message, no call stack).  For debug builds, appends the callstack and
        '''   also the inner exception, if any.
        ''' </summary>
        ''' <param name="ex"></param>
        Public Function DebugMessageFromException(ex As Exception) As String
#If DEBUG Then
            Dim ErrorMessage As String = ex.Message & vbCrLf & vbCrLf & vbCrLf & "[SHOWN IN DEBUG ONLY] STACK TRACE:" & vbCrLf & ex.StackTrace
            If ex.InnerException IsNot Nothing Then
                ErrorMessage &= vbCrLf & vbCrLf & "INNER EXCEPTION: " & vbCrLf & vbCrLf & ex.InnerException.ToString()
            End If

            Return ErrorMessage
#Else
            Return ex.Message
#End If
        End Function

        ''' <summary>
        ''' Attempts to create a string representation of an object, for debug purposes.  Under retail,
        '''   returns an empty string.
        ''' </summary>
        ''' <param name="Value">The value to turn into a displayable string.</param>
        Public Function DebugToString(Value As Object) As String
#If DEBUG Then
            Try
                If Value Is Nothing Then
                    Return "<Nothing>"
                ElseIf TypeOf Value Is String Then
                    Return """" & CStr(Value) & """"
                ElseIf TypeOf Value Is Control Then
                    Dim c As Control = DirectCast(Value, Control)
                    If c.Name <> "" Then
                        Return c.Name & " (Text=""" & c.Text & """)"
                    Else
                        Return "[" & c.GetType.Name & "] (Text=""" & c.Text & """)"
                    End If
                Else
                    Return Value.ToString()
                End If
            Catch ex As Exception
                Return "[" & ex.GetType.Name & "]"
            End Try
#Else
            Return ""
#End If
        End Function

        ''' <summary>
        ''' Logs the given exception and returns True so it can be used in an exception handler.
        ''' </summary>
        ''' <param name="ex">The exception to log.</param>
        ''' <param name="exceptionEventDescription">Additional description for the cause of the exception.</param>
        ''' <param name="throwingComponentName">Name of the component that threw that exception, generally the containing type name.</param>
        Public Function ReportWithoutCrash(ex As Exception,
                                           exceptionEventDescription As String,
                                           Optional throwingComponentName As String = "general") As Boolean
            Debug.Assert(ex IsNot Nothing)
            Debug.Assert(Not String.IsNullOrEmpty(throwingComponentName))
            Debug.Assert(Not String.IsNullOrEmpty(exceptionEventDescription))

            If IsCheckoutCanceledException(ex) Then Return True

            ' Follow naming convention for entity name: A string to identify the entity in the feature. E.g. open-project, build-project, fix-error.
            throwingComponentName = Regex.Replace(throwingComponentName, "([A-Z])", "-$1").TrimPrefix("-").ToLower() + "-fault"

            TelemetryService.DefaultSession.PostFault(
                eventName:="vs/ml/proppages/appdesigner/" & throwingComponentName.ToLower,
                description:=exceptionEventDescription,
                exceptionObject:=ex)

            Debug.Fail(exceptionEventDescription & vbCrLf & $"Exception: {ex}")
            Return True
        End Function

        ''' <summary>
        ''' Given an exception, returns True if it is a CheckOut exception.
        ''' </summary>
        ''' <param name="ex">The exception to check rethrow if it's caused by canceling checkout</param>
        Public Function IsCheckoutCanceledException(ex As Exception) As Boolean
            If (TypeOf ex Is CheckoutException AndAlso ex.Equals(CheckoutException.Canceled)) _
                OrElse
                (TypeOf ex Is COMException AndAlso DirectCast(ex, COMException).ErrorCode = AppDesInterop.Win32Constant.OLE_E_PROMPTSAVECANCELLED) _
            Then
                Return True
            End If

            If ex.InnerException IsNot Nothing Then
                Return IsCheckoutCanceledException(ex.InnerException)
            End If

            Return False
        End Function

        ''' <summary>
        ''' If the given string is Nothing, return "", else return the original string.
        ''' </summary>
        ''' <param name="Str"></param>
        Public Function NothingToEmptyString(Str As String) As String
            If Str Is Nothing Then
                Return String.Empty
            Else
                Return Str
            End If
        End Function

        ''' <summary>
        ''' If the given string is "", return Nothing, else return the original string.
        ''' </summary>
        ''' <param name="Str"></param>
        Public Function EmptyStringToNothing(Str As String) As String
            If Str Is Nothing OrElse Str.Length = 0 Then
                Return Nothing
            Else
                Return Str
            End If
        End Function

        ''' <summary>
        ''' A better IIf
        ''' </summary>
        ''' <param name="Condition">The condition to test.</param>
        ''' <param name="TrueExpression">What to return if the condition is True</param>
        ''' <param name="FalseExpression">What to return if the condition is False</param>
        Public Function IIf(Of T)(Condition As Boolean, TrueExpression As T, FalseExpression As T) As T
            If Condition Then
                Return TrueExpression
            Else
                Return FalseExpression
            End If
        End Function

        ''' <summary>
        ''' Set the drop-down width of a combobox wide enough to show the text of all entries in it
        ''' </summary>
        ''' <param name="ComboBox">The combobox to change the width for</param>
        Public Sub SetComboBoxDropdownWidth(ComboBox As ComboBox)
            If ComboBox IsNot Nothing Then
                ComboBox.DropDownWidth = Math.Max(MeasureMaxTextWidth(ComboBox, ComboBox.Items), ComboBox.Width)
            Else
                Debug.Fail("SetComboBoxDropdownWidth: No combobox specified")
            End If
        End Sub

        ''' <summary>
        ''' Set the drop-down width of a datagridviewcomboboxcolumn wide enough to show the text of all entries in it
        ''' </summary>
        ''' <param name="column">The column to change the width for</param>
        ''' <remarks>
        ''' This does not take the current cell style into account - it uses the font from the parent datagridview (if any)
        ''' It also makes room for the scrollbar even though it may not be visible...
        ''' </remarks>
        Public Sub SetComboBoxColumnDropdownWidth(column As DataGridViewComboBoxColumn)
            If column IsNot Nothing AndAlso column.DataGridView IsNot Nothing Then
                column.DropDownWidth = Math.Max(MeasureMaxTextWidth(column.DataGridView, column.Items) + SystemInformation.VerticalScrollBarWidth, column.Width)
            Else
                Debug.Fail("SetComboBoxColumnDropdownWidth: No combobox column specified, or the column didn't have a parent datagridview!")
            End If
        End Sub

        ''' <summary>
        ''' Check whether the screen reader is running
        ''' </summary>
        Public Function IsScreenReaderRunning() As Boolean
            Dim pvParam As IntPtr = Marshal.AllocCoTaskMem(4)
            Try
                If AppDesInterop.NativeMethods.SystemParametersInfo(AppDesInterop.Win32Constant.SPI_GETSCREENREADER, 0, pvParam, 0) <> 0 Then
                    Dim result As Integer = Marshal.ReadInt32(pvParam)
                    Return result <> 0
                End If
            Finally
                Marshal.FreeCoTaskMem(pvParam)
            End Try
            Return False
        End Function

        ''' <summary>
        ''' Sets error code and error message through IVsUIShell interface
        ''' </summary>
        ''' <param name="hr">error code</param>
        ''' <param name="errorMessage">error message</param>
        Public Sub SetErrorInfo(sp As ServiceProvider, hr As Integer, errorMessage As String)
            Dim vsUIShell As IVsUIShell = Nothing

            If sp IsNot Nothing Then
                vsUIShell = CType(sp.GetService(GetType(IVsUIShell)), IVsUIShell)
            End If

            If vsUIShell Is Nothing AndAlso Not VBPackageInstance IsNot Nothing Then
                vsUIShell = CType(VBPackageInstance.GetService(GetType(IVsUIShell)), IVsUIShell)
            End If

            If vsUIShell IsNot Nothing Then
                vsUIShell.SetErrorInfo(hr, errorMessage, 0, Nothing, Nothing)
            Else
                Debug.Fail("Could not get IVsUIShell from service provider. Can't set specific error message.")
            End If
        End Sub

        ''' <summary>
        ''' Sets focus to the first (or last) control inside of a parent HWND.
        ''' </summary>
        ''' <param name="HwndParent">The container HWND.</param>
        ''' <param name="First">If True, sets focus to the first control, otherwise the last.</param>
        Public Function FocusFirstOrLastTabItem(HwndParent As IntPtr, First As Boolean) As Boolean
            If HwndParent.Equals(IntPtr.Zero) Then
                Return False
            End If

            Dim c As Control = Control.FromChildHandle(HwndParent)
            If c IsNot Nothing Then
                'WinForms controls don't set WS_TABSTOP so GetNextDlgTabItem doesn't work well for them.

                Dim TabStopOnly As Boolean = True
                Dim Nested As Boolean = True
                Dim Wrap As Boolean = True
                If c.SelectNextControl(Nothing, First, TabStopOnly, Nested, Wrap) Then
                    Dim cc As ContainerControl = TryCast(c, ContainerControl)
                    If cc IsNot Nothing AndAlso cc.ActiveControl IsNot Nothing Then
                        cc.ActiveControl.Focus()
                    End If

                    Return True
                End If

                'Perhaps all the controls are disabled
                Return False
            End If

            'Use standard Win32 function for native dialog pages
            Dim FirstTabStop As IntPtr = AppDesInterop.NativeMethods.GetNextDlgTabItem(HwndParent, IntPtr.Zero, False)
            If FirstTabStop.Equals(IntPtr.Zero) Then
                Return False
            End If

            Dim NextTabStop As IntPtr
            If First Then
                NextTabStop = FirstTabStop
            Else
                NextTabStop = AppDesInterop.NativeMethods.GetNextDlgTabItem(HwndParent, FirstTabStop, True)
            End If

            If NextTabStop.Equals(IntPtr.Zero) Then
                Return False
            End If

            AppDesInterop.NativeMethods.SetFocus(NextTabStop)
            Return True
        End Function

        ''' <summary>
        ''' Returns a given path with a backslash at the end, if not already there.
        ''' </summary>
        ''' <param name="Path">The path to add a backslash to.</param>
        Public Function AppendBackslash(Path As String) As String
            If Path <> "" AndAlso Right(Path, 1) <> IO.Path.DirectorySeparatorChar AndAlso Right(Path, 1) <> IO.Path.AltDirectorySeparatorChar Then
                Return Path & IO.Path.DirectorySeparatorChar
            Else
                Return Path
            End If
        End Function

        ''' <summary>
        ''' Browses for a File.
        ''' </summary>
        ''' <param name="ServiceProvider">Service Provider</param>
        ''' <param name="ParentWindow">Window Handle of the parent window</param>
        ''' <param name="InitialDirectory">The initial directory for the dialog.  Can be Nothing or empty.</param>
        ''' <param name="DialogTitle">The title to use for the browse dialog.</param>
        ''' <param name="Filter">file type filter</param>
        ''' <param name="FilterIndex"></param>
        ''' <param name="MutiSelect">Whether we should support multi-selection</param>
        ''' <param name="NeedThrowError">Throw error when the dialog fails unexpectedly</param>
        ''' <returns>a collection of files</returns>
        Public Function GetFilesViaBrowse(ServiceProvider As IServiceProvider, ParentWindow As IntPtr,
                InitialDirectory As String, DialogTitle As String,
                Filter As String, FilterIndex As UInteger, MutiSelect As Boolean,
                Optional DefaultFileName As String = Nothing,
                Optional NeedThrowError As Boolean = False) As ArrayList

            Dim uishell As IVsUIShell =
                CType(ServiceProvider.GetService(GetType(IVsUIShell)), IVsUIShell)

            Dim fileNames As New ArrayList()

            InitialDirectory = NormalizeInitialDirectory(InitialDirectory)
            If InitialDirectory = "" Then
                InitialDirectory = Nothing
            End If

            Filter = GetNativeFilter(Filter)

            Dim MaxPathName As Integer = AppDesInterop.Win32Constant.MAX_PATH + 1
            If MutiSelect Then
                MaxPathName = (AppDesInterop.Win32Constant.MAX_PATH + 1) * VSDPLMAXFILES
            End If

            Dim vsOpenFileName As VSOPENFILENAMEW()

            Dim defaultName(MaxPathName) As Char
            If DefaultFileName IsNot Nothing Then
                DefaultFileName.CopyTo(0, defaultName, 0, DefaultFileName.Length)
            End If

            Dim stringMemPtr As IntPtr = Marshal.AllocHGlobal(MaxPathName * 2 + 2)
            Marshal.Copy(defaultName, 0, stringMemPtr, defaultName.Length)

            Try
                vsOpenFileName = New VSOPENFILENAMEW(0) {}
                vsOpenFileName(0).lStructSize = CUInt(Marshal.SizeOf(vsOpenFileName(0)))
                vsOpenFileName(0).hwndOwner = ParentWindow
                vsOpenFileName(0).pwzDlgTitle = DialogTitle
                vsOpenFileName(0).nMaxFileName = CUInt(MaxPathName)
                vsOpenFileName(0).pwzFileName = stringMemPtr
                vsOpenFileName(0).pwzInitialDir = InitialDirectory
                vsOpenFileName(0).pwzFilter = Filter
                vsOpenFileName(0).nFilterIndex = FilterIndex
                vsOpenFileName(0).nFileOffset = 0
                vsOpenFileName(0).nFileExtension = 0
                vsOpenFileName(0).dwHelpTopic = 0

                If MutiSelect Then
                    vsOpenFileName(0).dwFlags = &H200   'OFN_ALLOWMULTISELECT
                Else
                    vsOpenFileName(0).dwFlags = 0
                End If

                Dim hr As Integer = uishell.GetOpenFileNameViaDlg(vsOpenFileName)
                If VSErrorHandler.Succeeded(hr) Then
                    Dim buffer(MaxPathName) As Char
                    Marshal.Copy(stringMemPtr, buffer, 0, buffer.Length)
                    Dim path As String = Nothing
                    Dim i As Integer = 0
                    For j As Integer = 0 To buffer.Length - 1
                        If buffer(j) = Chr(0) Then
                            If i = j Then
                                Exit For
                            End If
                            If i = 0 Then
                                path = New String(buffer, 0, j)
                            Else
                                fileNames.Add(path & IO.Path.DirectorySeparatorChar & New String(buffer, i, j - i))
                            End If
                            i = j + 1
                        End If
                    Next

                    If fileNames.Count = 0 AndAlso path IsNot Nothing Then
                        fileNames.Add(path)
                    End If
                ElseIf NeedThrowError Then
                    If hr = AppDesInterop.Win32Constant.OLE_E_PROMPTSAVECANCELLED Then
                        'We shouldn't thrown error, if User cancelled out of dialog
                    Else
                        VSErrorHandler.ThrowOnFailure(hr)
                    End If
                End If
            Finally
                Marshal.FreeHGlobal(stringMemPtr)
            End Try

            Return fileNames
        End Function

        ''' <summary>
        ''' Change the Filter String to the format we can use in IVsUIShell function
        ''' </summary>
        ''' <param name="Filter">file type filter</param>
        ''' <returns>a native filter string</returns>
        Private Function GetNativeFilter(Filter As String) As String
            If Filter IsNot Nothing Then
                Dim length As Integer = Filter.Length
                Dim buf As Char() = New Char(length) {}

                Filter.CopyTo(0, buf, 0, length)

                For i As Integer = 0 To length - 1
                    If buf(i) = "|"c Then
                        buf(i) = Chr(0)
                    End If
                Next
                Filter = New String(buf)
            End If
            Return Filter
        End Function

        ''' <summary>
        ''' Change the InitialDirectory path to the format we can use in IVsUIShell function
        ''' </summary>
        ''' <param name="InitialDirectory">The initial directory for the dialog.  Can be Nothing or empty.</param>
        ''' <returns>a directory path</returns>
        Private Function NormalizeInitialDirectory(InitialDirectory As String) As String
            If InitialDirectory IsNot Nothing Then
                InitialDirectory = Trim(InitialDirectory)
                If InitialDirectory = "" Then
                    InitialDirectory = String.Empty
                Else
                    Try
                        'Path needs a backslash at the end, or it will be interpreted as a directory + filename
                        InitialDirectory = Path.GetFullPath(AppendBackslash(InitialDirectory))
                    Catch ex As Exception
                        InitialDirectory = String.Empty
                    End Try
                End If
            Else
                InitialDirectory = String.Empty
            End If
            Return InitialDirectory
        End Function

        ''' <summary>
        ''' Helper method to measure the maximum width of a collection of strings given a particular font...
        ''' </summary>
        ''' <param name="ctrl"></param>
        ''' <param name="items"></param>
        Public Function MeasureMaxTextWidth(ctrl As Control, items As IEnumerable) As Integer
            Dim MaxEntryWidth As Integer = 0
            Using g As Graphics = ctrl.CreateGraphics()
                For Each Entry As Object In items
                    Dim EntryText As String = ""
                    If Entry Is Nothing Then
                        EntryText = ""
                    ElseIf TypeOf Entry Is String Then
                        EntryText = DirectCast(Entry, String)
                    Else
                        'CONSIDER: should try type converter first
                        EntryText = Entry.ToString()
                    End If

                    Dim Width As Integer = CInt(g.MeasureString(EntryText, ctrl.Font).Width)
                    MaxEntryWidth = Math.Max(MaxEntryWidth, Width)
                Next
            End Using
            Return MaxEntryWidth
        End Function

#Region "Telemetry"
        Public NotInheritable Class TelemetryLogger

            'A list of known editor guids
            ' Each property page will be reported back to telemetry with the 1-based index in which it is present 
            ' in this list. All unknown entries will be reported as &hFF
            '
            ' Add more entries to the end of this list. Do *not* put any new entries in the middle of the list!
            Private Shared ReadOnly s_sqmOrder() As Guid = {
                KnownPropertyPageGuids.GuidApplicationPage_VB,
                KnownPropertyPageGuids.GuidApplicationPage_CS,
                KnownPropertyPageGuids.GuidApplicationPage_JS,
                KnownPropertyPageGuids.GuidCompilePage_VB,
                KnownPropertyPageGuids.GuidBuildPage_CS,
                KnownPropertyPageGuids.GuidBuildPage_JS,
                KnownPropertyPageGuids.GuidBuildEventsPage,
                KnownPropertyPageGuids.GuidDebugPage,
                KnownPropertyPageGuids.GuidReferencesPage_VB,
                KnownPropertyPageGuids.GuidReferencePathsPage,
                KnownPropertyPageGuids.GuidSigningPage,
                KnownPropertyPageGuids.GuidSecurityPage,
                KnownPropertyPageGuids.GuidPublishPage,
                KnownPropertyPageGuids.GuidDatabasePage_SQL,
                KnownPropertyPageGuids.GuidFxCopPage,
                KnownPropertyPageGuids.GuidDeployPage,
                KnownPropertyPageGuids.GuidDevicesPage_VSD,
                KnownPropertyPageGuids.GuidDebugPage_VSD,
                KnownPropertyPageGuids.GuidApplicationPage_VB_WPF,
                KnownPropertyPageGuids.GuidSecurityPage_WPF,
                KnownPropertyPageGuids.GuidMyExtensionsPage,
                KnownPropertyPageGuids.GuidOfficePublishPage,
                KnownPropertyPageGuids.GuidServicesPage,
                KnownPropertyPageGuids.GuidWAPWebPage
            }

            Private Const UNKNOWN_PAGE As Byte = &HFF
            Private Const DEFAULT_PAGE As Byte = 0

            Private Const ProjectSystemEventNamePrefix As String = "vs/projectsystem/"
            Private Const AppDesignerEventNamePrefix As String = ProjectSystemEventNamePrefix + "appdesigner/"

            Private Const ProjectSystemPropertyNamePrefix As String = "vs.projectsystem."
            Private Const AppDesignerPropertyNamePrefix As String = ProjectSystemPropertyNamePrefix + "appdesigner."

            ''' <summary>
            ''' Map a known property page or designer id to telemetry display name to log.
            ''' </summary>
            ''' <param name="guid"></param>
            Private Shared Function PageGuidToId(guid As Guid) As Byte
                For i As Integer = 0 To s_sqmOrder.Length - 1
                    If s_sqmOrder(i).Equals(guid) Then
                        Return CByte(i + 1)
                    End If
                Next
                Return UNKNOWN_PAGE
            End Function

            Public Shared Sub LogAppDesignerDefaultPageOpened()
                LogAppDesignerPageOpened(DEFAULT_PAGE)
            End Sub

            Public Shared Sub LogAppDesignerPageOpened(pageGuid As Guid, Optional tabTitle As String = Nothing, Optional alreadyOpened As Boolean = False)
                Dim pageId = PageGuidToId(pageGuid)
                LogAppDesignerPageOpened(pageId, pageGuid, tabTitle, alreadyOpened)
            End Sub

            Private Const PageOpenedEventName As String = AppDesignerEventNamePrefix + "page-opened"
            Private Const PageOpenedPropertyName As String = AppDesignerPropertyNamePrefix + "page-opened"
            Private Const PageOpenedPropertyNamePrefix As String = PageOpenedPropertyName + "."

            Private Shared Sub LogAppDesignerPageOpened(pageId As Byte, Optional pageGuid As Guid? = Nothing, Optional tabTitle As String = Nothing, Optional alreadyOpened As Boolean = False)
                Dim userTask = New UserTaskEvent(PageOpenedEventName, TelemetryResult.Success)
                userTask.Properties(PageOpenedPropertyName) = pageId

                If pageGuid IsNot Nothing Then
                    userTask.Properties(PageOpenedPropertyNamePrefix + "pageguid") = pageGuid.Value.ToString()
                End If

                If tabTitle IsNot Nothing Then
                    userTask.Properties(PageOpenedPropertyNamePrefix + "tabtitle") = tabTitle
                End If

                userTask.Properties(PageOpenedPropertyNamePrefix + "alreadyopened") = alreadyOpened

                TelemetryService.DefaultSession.PostEvent(userTask)
            End Sub

            Private Const EditorCreationEventName As String = ProjectSystemEventNamePrefix + "propertiespages/createeditor"
            Private Const EditorCreationPropertyNamePrefix As String = ProjectSystemPropertyNamePrefix + "propertiespages.createeditor."

            Public Shared Sub LogEditorCreation(useNewEditor As Boolean, fileName As String, physicalView As String)
                Dim telemetryEvent As TelemetryEvent = New TelemetryEvent(EditorCreationEventName)
                telemetryEvent.Properties(EditorCreationPropertyNamePrefix + "UseNewEditor") = useNewEditor
                telemetryEvent.Properties(EditorCreationPropertyNamePrefix + "FileName") = New TelemetryPiiProperty(fileName)
                telemetryEvent.Properties(EditorCreationPropertyNamePrefix + "PhysicalView") = physicalView
                TelemetryService.DefaultSession.PostEvent(telemetryEvent)
            End Sub

        End Class
#End Region

        Public NotInheritable Class ObjectSerializer

            ' KnownType information is used by DataContractSerializer for serialization of types that it may not know of currently.
            ' Size is used in Bitmap and has issues being recognized in DataContractSerializer for the unit tests of this class.
            Private Shared ReadOnly s_knownTypes As Type() = {GetType(Size)}

            Public Shared Sub Serialize(stream As Stream, value As Object)
                Requires.NotNull(stream)
                Requires.NotNull(value)
                Using writer As New BinaryWriter(stream, Encoding.UTF8, leaveOpen:=True)
                    Dim valueType = value.GetType()
                    writer.Write(valueType.AssemblyQualifiedName)
                    writer.Flush()
                    Call New DataContractSerializer(valueType, s_knownTypes).WriteObject(stream, value)
                End Using
            End Sub

            Public Shared Function Deserialize(stream As Stream) As Object
                Requires.NotNull(stream)
                If stream.Length = 0 Then
                    Throw New SerializationException("The stream contains no content.")
                End If
                Using reader As New BinaryReader(stream, Encoding.UTF8, leaveOpen:=True)
                    Dim valueType = Type.GetType(reader.ReadString())
                    Return New DataContractSerializer(valueType, s_knownTypes).ReadObject(stream)
                End Using
            End Function

        End Class

    End Module
End Namespace
