''' <summary>Non-generic version of TaskCompletionSource not provided by Microsoft.</summary>
Friend Class TaskCompletionSource

    Private ReadOnly Tcs As New TaskCompletionSource(Of Object)

    Public Function TrySetResult() As Boolean
        Return Me.Tcs.TrySetResult(Nothing)
    End Function

    Public Function TrySetCanceled() As Boolean
        Return Me.Tcs.TrySetCanceled()
    End Function

    Public ReadOnly Property Task() As Task
        Get
            Return Me.Tcs.Task
        End Get
    End Property

    Public Sub SetResult()
        Me.Tcs.SetResult(Nothing)
    End Sub

End Class

''' <summary>An extended queue class that supports removing objects.</summary>
Friend NotInheritable Class QueueX(Of T)
    Inherits Queue(Of T)

    Public Sub Remove(obj As T)
        If Me.Contains(obj) Then
            Dim intCnt As Integer = Me.Count
            While intCnt > 0
                Dim objItem As T = Me.Dequeue()
                If Not objItem.Equals(obj) Then 'note: with generic T the .Equals(obj as Object) overload is observed
                    Me.Enqueue(objItem)
                End If
                intCnt -= 1
            End While
        End If
    End Sub

End Class

''' <summary>Provides a boxed boolean value that can be passed by reference.</summary>
Friend NotInheritable Class [Boolean]

    Public Value As Boolean = False

    Public Sub New()
    End Sub

    Public Sub New(blnValue As Boolean)
        Me.Value = blnValue
    End Sub

End Class
