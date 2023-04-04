Imports RRAutoLib.CTC

Namespace CustomControls

    Public Class ContCtcEvents

        Friend Sub New()

            ' This call is required by the designer.
            InitializeComponent()

            InitContextMenus()

            AddHandler CtcService.RxMessage, AddressOf RxMessage
            AddHandler CtcService.BeforeModeChange, AddressOf BeforeModeChange

        End Sub

#Region "Context Menu"

        Private Sub InitContextMenus()

            Me.ContextMenu = New ContextMenu()

            AddMenuItem("Clear", False, Nothing, New RoutedEventHandler(AddressOf Clear_Click))
            AddMenuItem("Save Log", False, "IconSave", New RoutedEventHandler(AddressOf Save_Click))
            Me.ContextMenu.Items.Add(New Separator())
            AddMenuItem("Show Native Posts", My.Settings.CtcEventsShowNative, Nothing, New RoutedEventHandler(AddressOf ShowNative_Click))
            AddMenuItem("Show User Posts", My.Settings.CtcEventsShowUser, Nothing, New RoutedEventHandler(AddressOf ShowUser_Click))

        End Sub

        Private Sub AddMenuItem(strText As String, blnChecked As Boolean, objImageResKey As String, delHandler As RoutedEventHandler)

            Dim objMenuItem As New MenuItem()
            With objMenuItem
                .Header = strText
                .IsChecked = blnChecked
                If objImageResKey <> Nothing Then
                    Dim objImage As New Image
                    objImage.Width = 16
                    objImage.Height = 16
                    objImage.Source = TryFindResource(objImageResKey)
                    .Icon = objImage
                End If
                If delHandler IsNot Nothing Then AddHandler .Click, delHandler
            End With
            Me.ContextMenu.Items.Add(objMenuItem)

        End Sub

        Private Sub Clear_Click(sender As Object, e As RoutedEventArgs)
            Me.Items.Clear()
        End Sub

        Private Sub Save_Click(sender As Object, e As RoutedEventArgs)

            Dim saveFileDialog As New Microsoft.Win32.SaveFileDialog
            With saveFileDialog
                .FileName = "CtcEvents"
                .DefaultExt = "txt"
                .Filter = "Text Files (*.txt)|*.txt|All Files (*.*)|*.*"
                .InitialDirectory = Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments)
                If .ShowDialog() Then
                    Dim objStreamWriter As IO.StreamWriter = New IO.StreamWriter(.FileName, False)
                    With objStreamWriter
                        For Each objItem As Object In Me.Items
                            .Write(objItem.Desc)
                            .WriteLine()
                        Next
                        .Close()
                    End With
                End If
            End With

        End Sub

        Private Sub ShowNative_Click(sender As Object, e As RoutedEventArgs)

            Dim objMenuItem As MenuItem = sender
            objMenuItem.IsChecked = Not objMenuItem.IsChecked
            My.Settings.CtcEventsShowNative = objMenuItem.IsChecked

        End Sub

        Private Sub ShowUser_Click(sender As Object, e As RoutedEventArgs)

            Dim objMenuItem As MenuItem = sender
            objMenuItem.IsChecked = Not objMenuItem.IsChecked
            My.Settings.CtcEventsShowUser = objMenuItem.IsChecked

        End Sub

#End Region

        Private Sub RxMessage(enuCategory As CtcService.MessageCat, strMessage As String)

            Select Case True
                Case enuCategory = CtcService.MessageCat.Native AndAlso Not My.Settings.CtcEventsShowNative
                Case enuCategory = CtcService.MessageCat.User AndAlso Not My.Settings.CtcEventsShowUser
                Case Else
                    Dim objIcon As ImageSource
                    Select Case enuCategory
                        Case CtcService.MessageCat.Native
                            objIcon = My.Application.TryFindResource("IconInfo")
                        Case CtcService.MessageCat.User
                            objIcon = My.Application.TryFindResource("IconUser")
                        Case CtcService.MessageCat.Error
                            objIcon = My.Application.TryFindResource("IconError")
                    End Select

                    Dim objItem As Object = New With {.Icon = objIcon, .Desc = strMessage}

                    Me.Items.Add(objItem)
                    If Me.Items.Count > My.Settings.ListMaxLines Then
                        For intCountOver As Integer = 1 To Me.Items.Count - My.Settings.ListMaxLines
                            Me.Items.RemoveAt(0)
                        Next
                    End If

                    Me.ScrollIntoView(objItem)
            End Select

        End Sub

        Private Sub BeforeModeChange()
            If Not CtcService.IsStarted AndAlso My.Settings.ClearCtcEvents Then
                Me.Items.Clear()
            End If
        End Sub

    End Class

End Namespace
