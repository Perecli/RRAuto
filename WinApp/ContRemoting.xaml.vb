Imports RRAutoLib.Remoting

Namespace CustomControls

    Public Class ContRemoting

        Friend Sub New()

            ' This call is required by the designer.
            InitializeComponent()

            InitContextMenus()

            'populate the list of connections 
            For Each objConn As RemService.Connection In RemService.Connections
                Me.Items.Add(objConn)
            Next

            AddHandler RemService.ConnectionAction, AddressOf ConnectionAction


        End Sub

#Region "Context Menu"

        Private Sub InitContextMenus()
            Me.ContextMenu = New ContextMenu()

            AddMenuItem("Select All", Nothing, New RoutedEventHandler(AddressOf SelectAll_Click))
            AddMenuItem("Kill selected", Nothing, New RoutedEventHandler(AddressOf KillSel_Click))
        End Sub

        Private Sub AddMenuItem(strText As String, objImageResKey As String, delHandler As RoutedEventHandler)

            Dim objMenuItem As New MenuItem()
            With objMenuItem
                .Header = strText
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

        Private Sub SelectAll_Click(sender As Object, e As RoutedEventArgs)
            Me.SelectAll()
        End Sub

        Private Sub KillSel_Click(sender As Object, e As RoutedEventArgs)
            If Me.SelectedItems.Count = 0 Then
                MessageBox.Show(My.Application.MainWindow, "No rows have been selected.", My.Application.Name, MessageBoxButton.OK, MessageBoxImage.Exclamation)
            Else
                'kill connection to be implemented later when SignalR adds this functionality; it was promised in future releases
                MessageBox.Show(My.Application.MainWindow, "Kill connection not supported yet.", My.Application.Name, MessageBoxButton.OK, MessageBoxImage.Information)

                'this was the old WCF implementation 
                'can't do For Each here because ConnectionKilled() event, which alters the collection, ends up getting called by Kill() 
                'While Me.SelectedItems.Count > 0
                '    Me.SelectedItems(0).Kill()
                'End While
            End If
        End Sub

#End Region

        Private Sub ConnectionAction(enuAction As RemService.Connection.Action, objConnection As RemService.Connection)
            Select Case enuAction
                Case RemService.Connection.Action.Created
                    Me.Items.Add(objConnection)
                Case RemService.Connection.Action.Updated
                    Me.Items.Remove(objConnection)
                    Me.Items.Add(objConnection)
                Case RemService.Connection.Action.Deleted
                    Me.Items.Remove(objConnection)
                Case RemService.Connection.Action.AllClear
                    Me.Items.Clear()
            End Select
        End Sub

    End Class

    <ValueConversion(GetType(String), GetType(String))>
    Friend Class UpperCaseConverter
        Implements IValueConverter

        Public Function Convert(value As Object, targetType As Type, parameter As Object, culture As Globalization.CultureInfo) As Object Implements IValueConverter.Convert
            Return DirectCast(value, String).ToUpper
        End Function

        Public Function ConvertBack(value As Object, targetType As Type, parameter As Object, culture As Globalization.CultureInfo) As Object Implements IValueConverter.ConvertBack
            'no implementation
        End Function

    End Class

    'left for reference on how to do a string field to image converter
    '<ValueConversion(GetType(String), GetType(DrawingImage))>
    'Friend Class ServiceHostConverter
    '    Implements IValueConverter

    '    Public Function Convert(value As Object, targetType As Type, parameter As Object, culture As Globalization.CultureInfo) As Object Implements IValueConverter.Convert
    '        Select Case value
    '            Case "Trottle"
    '                Return My.Application.TryFindResource("IconThrottle")
    '            Case "LoconetSrv"
    '                Return My.Application.TryFindResource("IconComStation")
    '            Case Else
    '                Return My.Application.TryFindResource("IconPlaceHolder")
    '        End Select
    '    End Function

    '    Public Function ConvertBack(value As Object, targetType As Type, parameter As Object, culture As Globalization.CultureInfo) As Object Implements IValueConverter.ConvertBack
    '        'no implementation
    '    End Function

    'End Class

End Namespace
