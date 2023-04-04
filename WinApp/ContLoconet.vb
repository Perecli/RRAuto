Imports RRAutoLib.CTC
Imports RRAutoLib.Loconet

Namespace CustomControls

    Partial Public Class ContLoconet
        Inherits ListView

        'exists to get GridViewColumn objects by name; GridView does not support this
        Private _objColumns As New Dictionary(Of String, GridViewColumn)

        Friend Sub New()
            InitContextMenus()

            'set up column structure
            Me.BeginInit()
            Me.Foreground = Brushes.Black
            Me.View = New GridView()
            AddColumn(True, "", "Image", 30)
            ResolveColumn("TimeStamp", "MnuShowTimeStamp", My.Settings.LoconetShowStamp)
            ResolveColumn("Source", "MnuShowSource", My.Settings.LoconetShowSource)
            ResolveColumn("Description", "MnuShowDesc", My.Settings.LoconetShowDesc)
            ResolveColumn("Parameters", "MnuShowParam", My.Settings.LoconetShowParm)
            ResolveColumn("OpCode", "MnuShowOpCode", My.Settings.LoconetShowOpCode)
            ResolveColumn("RawHex", "MnuShowRawHex", My.Settings.LoconetShowHex)
            ResolveColumn("PacketID", "MnuShowID", My.Settings.LoconetShowID)
            Me.EndInit()

            AddHandler CtcService.LoconetService.RxPacketOnSyncThread, AddressOf RxPacket
        End Sub

#Region "Context Menu"

        'exists to get MenuItem objects by name; Me.ContextMenu.FindName() does not work with code behind
        Private _objMenuItems As New Dictionary(Of String, MenuItem)

        Private Sub InitContextMenus()
            Me.ContextMenu = New ContextMenu()

            AddMenuItem("MnuClear", "Clear", Nothing, New RoutedEventHandler(AddressOf Clear_Click))
            AddMenuItem("MnuSave", "Save Log", "IconSave", New RoutedEventHandler(AddressOf Save_Click))
            Me.ContextMenu.Items.Add(New Separator())
            AddMenuItem("MnuShowTimeStamp", "Show Time Stamp", Nothing, New RoutedEventHandler(AddressOf ShowStamp_Click))
            AddMenuItem("MnuShowSource", "Show Source", Nothing, New RoutedEventHandler(AddressOf ShowSource_Click))
            AddMenuItem("MnuShowDesc", "Show Description", Nothing, New RoutedEventHandler(AddressOf ShowDesc_Click))
            AddMenuItem("MnuShowParam", "Show Parameters", Nothing, New RoutedEventHandler(AddressOf ShowParm_Click))
            AddMenuItem("MnuShowOpCode", "Show Op Code", Nothing, New RoutedEventHandler(AddressOf ShowOpCode_Click))
            AddMenuItem("MnuShowRawHex", "Show Raw Hex", Nothing, New RoutedEventHandler(AddressOf ShowHex_Click))
            AddMenuItem("MnuShowID", "Show Packet ID", Nothing, New RoutedEventHandler(AddressOf ShowID_Click))
        End Sub

        Private Sub AddMenuItem(strName As String, strText As String, objImageResKey As String, delHandler As RoutedEventHandler)
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
            _objMenuItems.Add(strName, objMenuItem)
        End Sub

        Private Sub Clear_Click(sender As Object, e As RoutedEventArgs)
            Me.Items.Clear()
        End Sub

        Private Sub Save_Click(sender As Object, e As RoutedEventArgs)
            Dim saveFileDialog As New Microsoft.Win32.SaveFileDialog
            With saveFileDialog
                .FileName = "LoconetLog"
                .DefaultExt = "txt"
                .Filter = "Text Files (*.txt)|*.txt|All Files (*.*)|*.*"
                .InitialDirectory = Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments)
                If .ShowDialog() Then
                    Dim objStreamWriter As IO.StreamWriter = New IO.StreamWriter(.FileName, False)
                    With objStreamWriter
                        For Each objItem As Object In Me.Items
                            .Write(DirectCast(objItem.TimeStamp, String).PadRight(12))
                            .Write(" | ")
                            .Write(DirectCast(objItem.Source, String).PadRight(8))
                            .Write(" | ")
                            .Write(DirectCast(objItem.Description, String).PadRight(40))
                            .Write(" | ")
                            .Write(DirectCast(objItem.Parameters, String).PadRight(45))
                            .Write(" | ")
                            .Write(DirectCast(objItem.OpCode, String).PadRight(16))
                            .Write(" | ")
                            .Write(DirectCast(objItem.RawHex, String).PadRight(45))
                            .Write(" | ")
                            .Write(DirectCast(objItem.PacketID, String).PadRight(38))
                            .WriteLine()
                        Next
                        .Close()
                    End With
                End If
            End With
        End Sub

        Private Sub ShowStamp_Click(sender As Object, e As RoutedEventArgs)
            My.Settings.LoconetShowStamp = Not My.Settings.LoconetShowStamp
            ResolveColumn("TimeStamp", "MnuShowTimeStamp", My.Settings.LoconetShowStamp)
        End Sub

        Private Sub ShowSource_Click(sender As Object, e As RoutedEventArgs)
            My.Settings.LoconetShowSource = Not My.Settings.LoconetShowSource
            ResolveColumn("Source", "MnuShowSource", My.Settings.LoconetShowSource)
        End Sub

        Private Sub ShowDesc_Click(sender As Object, e As RoutedEventArgs)
            My.Settings.LoconetShowDesc = Not My.Settings.LoconetShowDesc
            ResolveColumn("Description", "MnuShowDesc", My.Settings.LoconetShowDesc)
        End Sub

        Private Sub ShowParm_Click(sender As Object, e As RoutedEventArgs)
            My.Settings.LoconetShowParm = Not My.Settings.LoconetShowParm
            ResolveColumn("Parameters", "MnuShowParam", My.Settings.LoconetShowParm)
        End Sub

        Private Sub ShowOpCode_Click(sender As Object, e As RoutedEventArgs)
            My.Settings.LoconetShowOpCode = Not My.Settings.LoconetShowOpCode
            ResolveColumn("OpCode", "MnuShowOpCode", My.Settings.LoconetShowOpCode)
        End Sub

        Private Sub ShowHex_Click(sender As Object, e As RoutedEventArgs)
            My.Settings.LoconetShowHex = Not My.Settings.LoconetShowHex
            ResolveColumn("RawHex", "MnuShowRawHex", My.Settings.LoconetShowHex)
        End Sub

        Private Sub ShowID_Click(sender As Object, e As RoutedEventArgs)
            My.Settings.LoconetShowID = Not My.Settings.LoconetShowID
            ResolveColumn("PacketID", "MnuShowID", My.Settings.LoconetShowID)
        End Sub

#End Region

        Private Sub ResolveColumn(strColumnName As String, strMenuItemName As String, blnShow As Boolean)

            Select Case True
                Case blnShow And Not _objColumns.ContainsKey(strColumnName)
                    Select Case strColumnName
                        Case "TimeStamp"
                            AddColumn(False, "Time Stamp", strColumnName, 75)
                        Case "Source"
                            AddColumn(False, "Source", strColumnName, 55)
                        Case "Description"
                            AddColumn(False, "Description", strColumnName, 150)
                        Case "Parameters"
                            AddColumn(False, "Parameters", strColumnName, 360)
                        Case "OpCode"
                            AddColumn(False, "Op Code", strColumnName, 100)
                        Case "RawHex"
                            AddColumn(False, "Raw Hex", strColumnName, 270)
                        Case "PacketID"
                            AddColumn(False, "Packet ID", strColumnName, 260)
                    End Select
                    _objMenuItems(strMenuItemName).IsChecked = True

                Case Not blnShow And _objColumns.ContainsKey(strColumnName)
                    RemoveColumn(strColumnName)
                    _objMenuItems(strMenuItemName).IsChecked = False

            End Select

        End Sub

        Private Sub AddColumn(blnIsImage As Boolean, strColText As String, strColumnName As String, intWidth As Integer)
            Dim objStyle As New Style()
            objStyle.Setters.Add(New Setter(ListView.HorizontalContentAlignmentProperty, HorizontalAlignment.Left))
            objStyle.Setters.Add(New Setter(ListView.PaddingProperty, New Thickness(5, 0, 5, 0)))

            Dim objCol As New GridViewColumn()
            With objCol
                .Header = strColText
                .Width = intWidth
                .HeaderContainerStyle = objStyle

                If blnIsImage Then
                    Dim objFrameworkElementFactory As New FrameworkElementFactory(GetType(Image))
                    objFrameworkElementFactory.SetBinding(Image.SourceProperty, New Binding(strColumnName))
                    objFrameworkElementFactory.SetValue(Image.WidthProperty, 16.0)
                    objFrameworkElementFactory.SetValue(Image.HeightProperty, 16.0)
                    Dim objTemplate As New DataTemplate()
                    objTemplate.VisualTree = objFrameworkElementFactory
                    .CellTemplate = objTemplate
                Else
                    .DisplayMemberBinding = New Binding(strColumnName)
                End If
            End With
            DirectCast(Me.View, GridView).Columns.Add(objCol)
            _objColumns.Add(strColumnName, objCol)
        End Sub

        Private Sub RemoveColumn(strColumnName As String)
            DirectCast(Me.View, GridView).Columns.Remove(_objColumns(strColumnName))
            _objColumns.Remove(strColumnName)
        End Sub


        Private Sub RxPacket(objPacket As Packet)

            Dim strImageKey As String
            Select Case True
                Case TypeOf objPacket Is PkSetPowerOn
                    strImageKey = "IconPowerOn"
                Case TypeOf objPacket Is PkSetPowerOff
                    strImageKey = "IconPowerOff"
                Case TypeOf objPacket Is PkSetLocoAdr OrElse
                     TypeOf objPacket Is PkSetLocoAdrExp
                    strImageKey = "IconSlotAssign"
                Case TypeOf objPacket Is PkSetSwitch OrElse
                     TypeOf objPacket Is PkGetSwitchState
                    strImageKey = "IconSwitch"
                Case TypeOf objPacket Is PkReqSlotData
                    strImageKey = "IconSlotRequest"
                Case TypeOf objPacket Is PkSlotMove
                    strImageKey = "IconSlotMove"
                Case TypeOf objPacket Is PkSlotStatus
                    strImageKey = "IconSlotStatus"
                Case TypeOf objPacket Is PkInput OrElse
                     TypeOf objPacket Is PkSwitchInput OrElse
                     TypeOf objPacket Is PkMultiSense OrElse
                     TypeOf objPacket Is PkLissy
                    strImageKey = "IconSensor"
                Case TypeOf objPacket Is PkSetSlotSpeed OrElse
                     TypeOf objPacket Is PkSetSlotDirFunc OrElse
                     TypeOf objPacket Is PkSetSlotFunc5to8 OrElse
                     TypeOf objPacket Is PkLocoCommand
                    strImageKey = "IconThrottle"
                Case objPacket.OpCode = OpCodes.SL_RD_DATA OrElse
                     objPacket.OpCode = OpCodes.SL_RD_DATA_EXP
                    strImageKey = "IconSlotRead"
                Case objPacket.OpCode = OpCodes.WR_SL_DATA OrElse
                     objPacket.OpCode = OpCodes.WR_SL_DATA_EXP
                    strImageKey = "IconSlotWrite"
                Case TypeOf objPacket Is PkPeerXfer
                    strImageKey = "Device"
                Case TypeOf objPacket Is PkFind
                    strImageKey = "Find"
                Case TypeOf objPacket Is PkSecurityElem
                    strImageKey = "Security"
                Case TypeOf objPacket Is PkImmediate
                    strImageKey = "IconDecoder"
                Case TypeOf objPacket Is PkUnknown
                    If objPacket.Description.StartsWith("Corrupt") Then
                        strImageKey = "IconPkCorrupt"
                    Else
                        strImageKey = "IconPkUnknown"
                    End If
                Case Else
                    strImageKey = "IconComStation"
            End Select

            Dim objItem As Object = New With {
                .Image = TryFindResource(strImageKey),
                .TimeStamp = String.Format("{0:000000.000}", objPacket.TimeStamp),
                .Source = If(objPacket.IsEchoe, "Local", "Remote"),
                .Description = objPacket.Description,
                .Parameters = objPacket.ParmsDesc,
                .OpCode = objPacket.OpCode.ToString,
                .RawHex = objPacket.BytesToString(),
                .PacketID = objPacket.ID.ToString.ToUpper}

            Me.Items.Add(objItem)
            If Me.Items.Count > My.Settings.ListMaxLines Then
                For intCountOver As Integer = 1 To Me.Items.Count - My.Settings.ListMaxLines
                    Me.Items.RemoveAt(0)
                Next
            End If

            Me.ScrollIntoView(objItem)
        End Sub

    End Class

End Namespace
