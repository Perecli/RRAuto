Namespace Windows

    Partial Public Class SplashScreen

        Protected Overrides Sub OnInitialized(e As System.EventArgs)
            MyBase.OnInitialized(e)

            Me.AppNameLabel.Text = My.Application.Name
        End Sub

        Public Sub SetStatus(strStatusText As String)
            Me.StatusLabel.Text = strStatusText
            Me.StatusBar.Value += 1
            Application.DoEvents()
        End Sub

    End Class

End Namespace