' Licensed to the .NET Foundation under one or more agreements. The .NET Foundation licenses this file to you under the MIT license. See the LICENSE.md file in the project root for more information.

Imports System.Runtime.InteropServices

Imports Microsoft.VisualStudio.OLE.Interop

Namespace Microsoft.VisualStudio.Editors.AppDesInterop

    Public NotInheritable Class CAUUIDMarshaler

        Public Shared Function GetData(cauuid As CAUUID) As Guid()
            Const GUID_BYTE_COUNT As Integer = 16
            Dim guids As Guid()
            Dim CurrentPtr As IntPtr
            Dim bytes As Byte() = New Byte(GUID_BYTE_COUNT - 1) {}

            guids = New Guid(CInt(cauuid.cElems) - 1) {}
            CurrentPtr = cauuid.pElems
            For Index As Integer = 0 To CInt(cauuid.cElems) - 1
                Marshal.Copy(CurrentPtr, bytes, 0, GUID_BYTE_COUNT)
                guids(Index) = New Guid(bytes)
                CurrentPtr = New IntPtr(CurrentPtr.ToInt64() + GUID_BYTE_COUNT) 'Increment pointer
            Next
            Return guids
        End Function

    End Class

End Namespace
