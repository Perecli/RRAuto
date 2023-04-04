Imports RRAutoLib.CTC
Imports RRAutoLib.Scripting

Namespace Windows

    Public Class ScriptErrors

        Public Sub New(objErrors As List(Of CompilerService.ScriptError))
            ' This call is required by the designer.
            InitializeComponent()

            Me.ListViewErrors.ItemsSource = objErrors
        End Sub

        Private Sub cmdOK_Click(sender As Object,  e As RoutedEventArgs) Handles cmdOK.Click
            Me.Close()
        End Sub

    End Class

End Namespace