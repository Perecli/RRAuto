Imports RRAutoLib.CTC
Imports RRAutoLib.Scripting
Imports RRAuto.CustomControls

Namespace Windows

    Partial Public Class GeneratedAssembly

        Private WithEvents _objScriptTextBox As New CodeEditor With {.ReadOnly = True}

        Friend Sub New()

            ' This call is required by the designer.
            InitializeComponent()

            Me.FormsHost.Child = _objScriptTextBox
            _objScriptTextBox.Text = CompilerService.GenerateCode()
            _objScriptTextBox.MoveToBeginning()            

        End Sub

        Private Sub GeneratedAssembly_Loaded(sender As Object, e As RoutedEventArgs) Handles Me.Loaded
            _objScriptTextBox.Focus()
        End Sub

        Private Sub cmdOK_Click(sender As Object, e As RoutedEventArgs) Handles cmdOK.Click
            Me.Close()
        End Sub

    End Class

End Namespace