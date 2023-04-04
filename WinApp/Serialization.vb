Imports System.Runtime.Serialization
Imports System.Runtime.Serialization.Formatters
Imports System.Security.Permissions

Public Class Serialization

    Public Shared Function ToBytes(objRootObj As Object,  strContextName As String) As Byte()

        Dim objMemStream As New IO.MemoryStream
        Try
            Dim objFormatter As IFormatter = New Binary.BinaryFormatter
            objFormatter.Context = New StreamingContext(StreamingContextStates.File)
            objFormatter.Serialize(objMemStream, objRootObj)
            Return objMemStream.ToArray
        Catch ex As Exception
            ex = ex.GetBaseException
            MessageBox.Show(My.Application.MainWindow, String.Format("Failed to save {1}.{0}{0}{2}", vbCrLf, strContextName, ex.Message), My.Application.Name, MessageBoxButton.OK, MessageBoxImage.Exclamation)
        Finally
            If objMemStream IsNot Nothing Then objMemStream.Close()
        End Try

    End Function

    Public Shared Function FromBytes(bytaBytes As Byte(),  strContextName As String) As Object

        If bytaBytes Is Nothing Then Return Nothing

        Dim objMemStream As New IO.MemoryStream(bytaBytes)
        Try
            Dim objFormatter As IFormatter = New Binary.BinaryFormatter
            objFormatter.Context = New StreamingContext(StreamingContextStates.File)
            objFormatter.Binder = New VersionDeserializationBinder
            Return objFormatter.Deserialize(objMemStream)
        Catch ex As Exception
            ex = ex.GetBaseException
            MessageBox.Show(My.Application.MainWindow, String.Format("Failed to load {1}.{0}{0}{2}", vbCrLf, strContextName, ex.Message), My.Application.Name, MessageBoxButton.OK, MessageBoxImage.Exclamation)
        Finally
            If objMemStream IsNot Nothing Then objMemStream.Close()
        End Try

    End Function

    Private NotInheritable Class VersionDeserializationBinder
        Inherits SerializationBinder

        Public Overrides Function BindToType(assemblyName As String,  typeName As String) As Type
            'for backwards compatibility
            Select Case True
                Case typeName.Contains("RRAuto.") AndAlso Type.GetType(typeName) Is Nothing  'if type was declared in this application but no longer exists
                    '"typeName.Contains()" is used because the namespace could appear in the middle of the string for generics
                    Throw New ApplicationException(String.Format("Type name '{0}' has been depricated and no substitute type was provided for deserialization.", typeName))

                Case Else
                    'return the new version type of the serialized type with the same name
                    Return Type.GetType(typeName)

            End Select
        End Function
    End Class

End Class
