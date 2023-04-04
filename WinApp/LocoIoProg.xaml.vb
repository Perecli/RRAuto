Imports System.ComponentModel
Imports System.Collections.Specialized
Imports System.Text.RegularExpressions
Imports System.Collections.ObjectModel
Imports System.Windows.Threading
Imports RRAuto.PropertyGridHelpers
Imports RRAutoLib.CTC
Imports RRAutoLib.Loconet

Namespace Windows

    Partial Public Class LocoIoProg

        Private _objLvPortConfigs As New ObservableCollection(Of LvPortConfig)()

        ''' <summary>Used for binding to ListView in XAML</summary>
        Public ReadOnly Property LvPortConfigs() As ObservableCollection(Of LvPortConfig)
            Get
                Return _objLvPortConfigs
            End Get
        End Property

        Public Sub New()
            ' This call is required by the Windows Form Designer.
            InitializeComponent()

            For bytPort As Byte = 1 To 16
                _objLvPortConfigs.Add(New LvPortConfig(bytPort))
            Next
        End Sub

#Region "Validation"

        Private Function DeviceAdrIsValid() As Boolean
            Dim strValue As String = Me.DeviceAdr.Text.Trim
            If strValue.Length > 0 Then
                If Regex.IsMatch(strValue, "^\d{1,3}$") Then
                    Dim intValue As Integer = strValue
                    If (intValue >= 1 And intValue <= 127) Then
                        If intValue <> 80 Then
                            Return True
                        Else
                            MessageBox.Show(Me, "LocoIO device address 80 is reserved for the PC.", My.Application.Name, MessageBoxButton.OK, MessageBoxImage.Exclamation)
                        End If
                    Else
                        MessageBox.Show(Me, "The LocoIO device address is out of range. Valid values are 1-127 excluding 80 which is reserved for the PC.", My.Application.Name, MessageBoxButton.OK, MessageBoxImage.Exclamation)
                    End If
                Else
                    MessageBox.Show(Me, "The LocoIO device address must be an integer 1-127 excluding 80 which is reserved for the PC.", My.Application.Name, MessageBoxButton.OK, MessageBoxImage.Exclamation)
                End If
            Else
                MessageBox.Show(Me, "The LocoIO device address is needed for this operation.", My.Application.Name, MessageBoxButton.OK, MessageBoxImage.Exclamation)
            End If

            Return False
        End Function

        Private Function DeviceSubAdrIsValid() As Boolean
            Dim strValue As String = Me.DeviceSubAdr.Text.Trim
            If strValue.Length > 0 Then
                If Regex.IsMatch(strValue, "^\d{1,3}$") Then
                    Dim intValue As Integer = strValue
                    If (intValue >= 1 And intValue <= 126) Then
                        Return True
                    Else
                        MessageBox.Show(Me, "The LocoIO device sub-address is out of range. Valid values are 1-126.", My.Application.Name, MessageBoxButton.OK, MessageBoxImage.Exclamation)
                    End If
                Else
                    MessageBox.Show(Me, "The LocoIO device sub-address must be an integer 1-126.", My.Application.Name, MessageBoxButton.OK, MessageBoxImage.Exclamation)
                End If
            Else
                MessageBox.Show(Me, "The LocoIO device sub-address is needed for this operation.", My.Application.Name, MessageBoxButton.OK, MessageBoxImage.Exclamation)
            End If

            Return False
        End Function

        Private Function BlinkingRateIsValid() As Boolean
            Dim strValue As String = Me.BlinkingRate.Text.Trim
            If strValue.Length > 0 Then
                If Regex.IsMatch(strValue, "^\d{1,2}$") Then
                    Dim intValue As Integer = strValue
                    If (intValue >= 0 And intValue <= 15) Then
                        Return True
                    Else
                        MessageBox.Show(Me, "The blinking rate is out of range. Valid values are 0-15.", My.Application.Name, MessageBoxButton.OK, MessageBoxImage.Exclamation)
                    End If
                Else
                    MessageBox.Show(Me, "The blinking rate must be an integer 0-15.", My.Application.Name, MessageBoxButton.OK, MessageBoxImage.Exclamation)
                End If
            Else
                MessageBox.Show(Me, "The blinking rate is needed for this operation.", My.Application.Name, MessageBoxButton.OK, MessageBoxImage.Exclamation)
            End If

            Return False
        End Function

#End Region

#Region "Port Config Tab"

        Private Async Sub CmdRequestConfig_Click(sender As System.Object, e As RoutedEventArgs) Handles CmdRequestConfig.Click
            If DeviceAdrIsValid() And DeviceSubAdrIsValid() Then
                For bytPort As Byte = 1 To 16
                    Dim objPkLocoIO As PkLocoIO = Await PkLocoIO.RequestConfig(CtcService.LoconetService, CType(Me.DeviceAdr.Text, Byte), CType(Me.DeviceSubAdr.Text, Byte), bytPort)
                    If objPkLocoIO IsNot Nothing Then
                        Dim objPortConfig As PkLocoIO.PortConfig = objPkLocoIO.Read
                        Dim objLvPortConfig As LvPortConfig = _objLvPortConfigs(objPortConfig.Port - 1)
                        objLvPortConfig.PortConfig = objPortConfig
                        objLvPortConfig.NotifyConfigChanged()
                        RebuildPropertyGridSelectedObjects()
                    End If
                Next
            End If
        End Sub

        Private Async Sub CmdWriteConfig_Click(sender As System.Object, e As RoutedEventArgs) Handles CmdWriteConfig.Click
            If DeviceAdrIsValid() And DeviceSubAdrIsValid() Then
                For Each objLvPortConfig As LvPortConfig In _objLvPortConfigs
                    PkLocoIO.WriteConfig(CtcService.LoconetService, CType(Me.DeviceAdr.Text, Byte), CType(Me.DeviceSubAdr.Text, Byte), objLvPortConfig.PortConfig)
                Next
                RefreshPorts(Await PkLocoIO.RequestSetup(CtcService.LoconetService, CType(Me.DeviceAdr.Text, Byte), CType(Me.DeviceSubAdr.Text, Byte)))
            End If
        End Sub


        Private Sub ListViewPortConfigs_SelectionChanged(sender As Object, e As System.Windows.Controls.SelectionChangedEventArgs) Handles ListViewPortConfigs.SelectionChanged
            RebuildPropertyGridSelectedObjects()
        End Sub

        Private Sub RebuildPropertyGridSelectedObjects()
            If Me.ListViewPortConfigs.SelectedItems.Count > 0 Then
                'repackage PortConfig objects from LvPortConfig to PgPortConfig
                Dim objaSelected(Me.ListViewPortConfigs.SelectedItems.Count - 1) As PgPortConfig
                For intIdx As Integer = 0 To Me.ListViewPortConfigs.SelectedItems.Count - 1
                    objaSelected(intIdx) = New PgPortConfig(DirectCast(Me.ListViewPortConfigs.SelectedItems(intIdx), LvPortConfig).PortConfig)
                Next

                Me.PropertyGrid.SelectedObjects = objaSelected
            Else
                Me.PropertyGrid.SelectedObjects = New Object() {}
            End If

        End Sub

        Private Sub PropertyGrid_PropertyValueChanged(s As Object, e As System.Windows.Forms.PropertyValueChangedEventArgs) Handles PropertyGrid.PropertyValueChanged
            'refresh the selected ListView items when the configuration enumeration is changed in the PropertyGrid
            If e.ChangedItem.Label = "Config" Then
                For Each objSelItem As LvPortConfig In Me.ListViewPortConfigs.SelectedItems
                    objSelItem.NotifyConfigChanged()
                Next
            End If
        End Sub

#End Region

#Region "Multiport Tab"

        Private Async Sub CmdConfigMultiport_Click(sender As System.Object, e As RoutedEventArgs) Handles CmdConfigMultiport.Click
            If DeviceAdrIsValid() And DeviceSubAdrIsValid() Then
                For bytPort As Byte = 1 To 16
                    Dim objPortConfig As New PkLocoIO.PortConfig
                    objPortConfig.Config = PkLocoIO.ConfigType.OutInpRepNorm
                    objPortConfig.Port = bytPort
                    PkLocoIO.WriteConfig(CtcService.LoconetService, CType(Me.DeviceAdr.Text, Byte), CType(Me.DeviceSubAdr.Text, Byte), objPortConfig)
                Next
                RefreshPorts(Await PkLocoIO.RequestSetup(CtcService.LoconetService, CType(Me.DeviceAdr.Text, Byte), CType(Me.DeviceSubAdr.Text, Byte)))
            End If
        End Sub

        Private Sub CmdClearAll_Click(sender As System.Object, e As RoutedEventArgs) Handles CmdClearAll.Click
            Me.ChkPortState1.IsChecked = False
            Me.ChkPortState2.IsChecked = False
            Me.ChkPortState3.IsChecked = False
            Me.ChkPortState4.IsChecked = False
            Me.ChkPortState5.IsChecked = False
            Me.ChkPortState6.IsChecked = False
            Me.ChkPortState7.IsChecked = False
            Me.ChkPortState8.IsChecked = False
            Me.ChkPortState9.IsChecked = False
            Me.ChkPortState10.IsChecked = False
            Me.ChkPortState11.IsChecked = False
            Me.ChkPortState12.IsChecked = False
            Me.ChkPortState13.IsChecked = False
            Me.ChkPortState14.IsChecked = False
            Me.ChkPortState15.IsChecked = False
            Me.ChkPortState16.IsChecked = False
        End Sub

        Private Sub CmdSelectAll_Click(sender As System.Object, e As RoutedEventArgs) Handles CmdSelectAll.Click
            Me.ChkPortState1.IsChecked = True
            Me.ChkPortState2.IsChecked = True
            Me.ChkPortState3.IsChecked = True
            Me.ChkPortState4.IsChecked = True
            Me.ChkPortState5.IsChecked = True
            Me.ChkPortState6.IsChecked = True
            Me.ChkPortState7.IsChecked = True
            Me.ChkPortState8.IsChecked = True
            Me.ChkPortState9.IsChecked = True
            Me.ChkPortState10.IsChecked = True
            Me.ChkPortState11.IsChecked = True
            Me.ChkPortState12.IsChecked = True
            Me.ChkPortState13.IsChecked = True
            Me.ChkPortState14.IsChecked = True
            Me.ChkPortState15.IsChecked = True
            Me.ChkPortState16.IsChecked = True
        End Sub

        Private Async Sub CmdRequestStates_Click(sender As System.Object, e As RoutedEventArgs) Handles CmdRequestStates.Click
            If Me.DeviceAdrIsValid() Then
                Dim objPkLocoIO As PkLocoIO = Await PkLocoIO.RequestStates(CtcService.LoconetService, CType(Me.DeviceAdr.Text, Byte))
                If objPkLocoIO IsNot Nothing Then
                    Dim objStates As Boolean() = objPkLocoIO.Read
                    Me.ChkPortState1.IsChecked = objStates(0)
                    Me.ChkPortState2.IsChecked = objStates(1)
                    Me.ChkPortState3.IsChecked = objStates(2)
                    Me.ChkPortState4.IsChecked = objStates(3)
                    Me.ChkPortState5.IsChecked = objStates(4)
                    Me.ChkPortState6.IsChecked = objStates(5)
                    Me.ChkPortState7.IsChecked = objStates(6)
                    Me.ChkPortState8.IsChecked = objStates(7)
                    Me.ChkPortState9.IsChecked = objStates(8)
                    Me.ChkPortState10.IsChecked = objStates(9)
                    Me.ChkPortState11.IsChecked = objStates(10)
                    Me.ChkPortState12.IsChecked = objStates(11)
                    Me.ChkPortState13.IsChecked = objStates(12)
                    Me.ChkPortState14.IsChecked = objStates(13)
                    Me.ChkPortState15.IsChecked = objStates(14)
                    Me.ChkPortState16.IsChecked = objStates(15)
                End If
            End If
        End Sub

        Private Sub CmdWriteStates_Click(sender As System.Object, e As RoutedEventArgs) Handles CmdWriteStates.Click
            If DeviceAdrIsValid() Then
                PkLocoIO.WriteStates(CtcService.LoconetService, CType(Me.DeviceAdr.Text, Byte), _
                                    New Boolean() {True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True},
                                    New Boolean() {Me.ChkPortState1.IsChecked, Me.ChkPortState2.IsChecked, Me.ChkPortState3.IsChecked, Me.ChkPortState4.IsChecked,
                                                   Me.ChkPortState5.IsChecked, Me.ChkPortState6.IsChecked, Me.ChkPortState7.IsChecked, Me.ChkPortState8.IsChecked,
                                                   Me.ChkPortState9.IsChecked, Me.ChkPortState10.IsChecked, Me.ChkPortState11.IsChecked, Me.ChkPortState12.IsChecked,
                                                   Me.ChkPortState13.IsChecked, Me.ChkPortState14.IsChecked, Me.ChkPortState15.IsChecked, Me.ChkPortState16.IsChecked})
            End If
        End Sub

#End Region

#Region "Setup Tab"

        Private Sub CmdBroadcast_Click(sender As System.Object, e As RoutedEventArgs) Handles CmdBroadcast.Click
            If DeviceAdrIsValid() And DeviceSubAdrIsValid() Then
                If MessageBox.Show(Me, "Perform the broadcast?", My.Application.Name, MessageBoxButton.OKCancel, MessageBoxImage.Question) = MessageBoxResult.OK Then
                    PkLocoIO.BroadcastAddress(CtcService.LoconetService, CType(Me.DeviceAdr.Text, Byte), CType(Me.DeviceSubAdr.Text, Byte))
                End If
            End If
        End Sub

        Private Async Sub CmdRequestSetup_Click(sender As System.Object, e As RoutedEventArgs) Handles CmdRequestSetup.Click
            If DeviceAdrIsValid() And DeviceSubAdrIsValid() Then
                Dim objPkLocoIO As PkLocoIO = Await PkLocoIO.RequestSetup(CtcService.LoconetService, CType(Me.DeviceAdr.Text, Byte), CType(Me.DeviceSubAdr.Text, Byte))
                If objPkLocoIO IsNot Nothing Then
                    Dim objSetup As PkLocoIO.Setup = objPkLocoIO.Read
                    Me.Version.Text = "Firmware version: " & objSetup.Version
                    Me.ChkAltPushButtons.IsChecked = objSetup.AlternatePushButtons
                    Me.ChkServoMotorOutputs.IsChecked = objSetup.ServoMotorOutputs
                    Me.BlinkingRate.Text = objSetup.BlinkingRate
                End If
            End If
        End Sub

        Private Sub CmdWriteSetup_Click(sender As System.Object, e As RoutedEventArgs) Handles CmdWriteSetup.Click
            If DeviceAdrIsValid() And DeviceSubAdrIsValid() And BlinkingRateIsValid() Then
                Dim objSetup As New PkLocoIO.Setup
                objSetup.AlternatePushButtons = Me.ChkAltPushButtons.IsChecked
                objSetup.ServoMotorOutputs = Me.ChkServoMotorOutputs.IsChecked
                objSetup.BlinkingRate = CType(Me.BlinkingRate.Text, Byte)
                PkLocoIO.WriteSetup(CtcService.LoconetService, CType(Me.DeviceAdr.Text, Byte), CType(Me.DeviceSubAdr.Text, Byte), objSetup)
            End If
        End Sub

#End Region

        Private Sub RefreshPorts(objPkLocoIO As PkLocoIO)
            If objPkLocoIO IsNot Nothing Then
                Dim objSetup As PkLocoIO.Setup = objPkLocoIO.Read
                objSetup.PortRefresh = True
                PkLocoIO.WriteSetup(CtcService.LoconetService, CType(Me.DeviceAdr.Text, Byte), CType(Me.DeviceSubAdr.Text, Byte), objSetup)
            End If
        End Sub

#Region "Wrapper classes"

        ''' <summary>PkLocoIO.PortConfig wrapper for the listview.</summary>
        Public NotInheritable Class LvPortConfig
            Implements INotifyPropertyChanged  'adds ability for the ObservableCollection containing this object to update the UI on Update not just the intrinsic Add/Remove

            Private _objPortConfig As PkLocoIO.PortConfig

            Public Sub New(bytPort As Byte)
                _objPortConfig = New PkLocoIO.PortConfig
                _objPortConfig.Port = bytPort
            End Sub

            Public Property PortConfig() As PkLocoIO.PortConfig
                Get
                    Return _objPortConfig
                End Get
                Set(Value As PkLocoIO.PortConfig)
                    _objPortConfig = Value
                End Set
            End Property

            ''' <summary>For binding to ListView.</summary>
            Public ReadOnly Property Port() As String
                Get
                    Return String.Format("{0:00}", _objPortConfig.Port)
                End Get
            End Property

            ''' <summary>For binding to ListView.</summary>
            Public ReadOnly Property Config() As String
                Get
                    Return _objPortConfig.Config.ToString
                End Get
            End Property

            Public Sub NotifyConfigChanged()
                RaiseEvent PropertyChanged(Me, New PropertyChangedEventArgs("Config"))
            End Sub

            Public Event PropertyChanged(sender As Object,  e As PropertyChangedEventArgs) Implements INotifyPropertyChanged.PropertyChanged
        End Class

        ''' <summary>PkLocoIO.PortConfig wrapper for the property grid.</summary>
        <TypeConverter(GetType(DynamicPropGridConverter))> Public NotInheritable Class PgPortConfig
            Implements IDynamicPropertyGrid
            Private _objPortConfig As PkLocoIO.PortConfig

            Public Sub New()
                _objPortConfig = New PkLocoIO.PortConfig
            End Sub

            Public Sub New(objPortConfig As PkLocoIO.PortConfig)
                _objPortConfig = objPortConfig
            End Sub

            <Browsable(False)> Public ReadOnly Property PortConfig() As PkLocoIO.PortConfig
                Get
                    Return _objPortConfig
                End Get
            End Property

            Public Property Config() As PkLocoIO.ConfigType
                Get
                    Return _objPortConfig.Config
                End Get
                Set(Value As PkLocoIO.ConfigType)
                    _objPortConfig.Config = Value
                End Set
            End Property

            Public ReadOnly Property OpCode() As OpCodes
                Get
                    Return _objPortConfig.Packet.OpCode
                End Get
            End Property


            Public Property Address() As Short
                Get
                    Select Case True
                        Case TypeOf _objPortConfig.Packet Is PkInput
                            Return DirectCast(_objPortConfig.Packet, PkInput).Address
                        Case TypeOf _objPortConfig.Packet Is PkSwitchInput
                            Return DirectCast(_objPortConfig.Packet, PkSwitchInput).Address
                    End Select
                End Get
                Set(Value As Short)
                    Select Case True
                        Case TypeOf _objPortConfig.Packet Is PkInput
                            DirectCast(_objPortConfig.Packet, PkInput).Address = Value
                        Case TypeOf _objPortConfig.Packet Is PkSwitchInput
                            DirectCast(_objPortConfig.Packet, PkSwitchInput).Address = Value
                    End Select
                End Set
            End Property

            Public Property BDL16Adr() As Short
                Get
                    Return DirectCast(_objPortConfig.Packet, PkInput).BDL16Address
                End Get
                Set(Value As Short)
                    DirectCast(_objPortConfig.Packet, PkInput).BDL16Address = Value
                End Set
            End Property

            Public Property BDL16Port() As Byte
                Get
                    Return DirectCast(_objPortConfig.Packet, PkInput).BDL16Port
                End Get
                Set(Value As Byte)
                    DirectCast(_objPortConfig.Packet, PkInput).BDL16Port = Value
                End Set
            End Property

            Public Property OnOffState() As OnOff
                Get
                    Select Case True
                        Case TypeOf _objPortConfig.Packet Is PkInput
                            Return DirectCast(_objPortConfig.Packet, PkInput).State
                        Case TypeOf _objPortConfig.Packet Is PkSwitchInput
                            Return DirectCast(_objPortConfig.Packet, PkSwitchInput).State
                    End Select
                End Get
                Set(Value As OnOff)
                    Select Case True
                        Case TypeOf _objPortConfig.Packet Is PkInput
                            DirectCast(_objPortConfig.Packet, PkInput).State = Value
                        Case TypeOf _objPortConfig.Packet Is PkSwitchInput
                            DirectCast(_objPortConfig.Packet, PkSwitchInput).State = Value
                    End Select
                End Set
            End Property


            Public Property Switch() As Short
                Get
                    Return DirectCast(_objPortConfig.Packet, PkSetSwitch).Switch
                End Get
                Set(Value As Short)
                    DirectCast(_objPortConfig.Packet, PkSetSwitch).Switch = Value
                End Set
            End Property

            Public Property SwitchState() As SwitchState
                Get
                    Return DirectCast(_objPortConfig.Packet, PkSetSwitch).State
                End Get
                Set(Value As SwitchState)
                    DirectCast(_objPortConfig.Packet, PkSetSwitch).State = Value
                End Set
            End Property

            Public Property Key() As OnOff
                Get
                    Return DirectCast(_objPortConfig.Packet, PkSetSwitch).Key
                End Get
                Set(Value As OnOff)
                    DirectCast(_objPortConfig.Packet, PkSetSwitch).Key = Value
                End Set
            End Property


            Public ReadOnly Property ConfigByte() As String
                Get
                    Return ByteToString(_objPortConfig.Config >> 4)
                End Get
            End Property

            Public ReadOnly Property Value1Byte() As String
                Get
                    If _objPortConfig.Packet Is Nothing Then
                        Return ByteToString(0)
                    Else
                        Return ByteToString(_objPortConfig.Packet.Bytes(1))
                    End If
                End Get
            End Property

            Public ReadOnly Property Value2Byte() As String
                Get
                    If _objPortConfig.Packet Is Nothing Then
                        Return ByteToString(0)
                    Else
                        Return ByteToString(_objPortConfig.Packet.Bytes(2))
                    End If
                End Get
            End Property

            Private Function ByteToString(bytByte As Byte) As String
                Dim strResult As String
                For bytShift As SByte = 7 To 0 Step -1
                    strResult &= (bytByte And (1 << bytShift)) >> bytShift
                Next
                strResult &= String.Format(" - {0}", bytByte)
                Return strResult
            End Function


            Public Function PropGridSort() As String() Implements IDynamicPropertyGrid.PropGridSort
                Return New String() {"Config", "OpCode", "Address", "BDL16Adr", "BDL16Port", "OnOffState", "Switch", "SwitchState", "Key", "ConfigByte", "Value1Byte", "Value2Byte"}
            End Function

            Public Function PropGridDynamicAttrib() As ListDictionary Implements IDynamicPropertyGrid.PropGridDynamicAttrib
                Dim objList As New ListDictionary

                Dim objaConfig As Attribute() = {New DescriptionAttribute("Determines how the LocoIO port will be configured."), New RefreshPropertiesAttribute(RefreshProperties.All)}
                Dim objaOpCode As Attribute() = {New DescriptionAttribute("Packet type name as defined by the Loconet specification.")}

                Dim objaAddress As Attribute() = {New RefreshPropertiesAttribute(RefreshProperties.Repaint)}
                Dim objaBDL16Adr As Attribute() = {New RefreshPropertiesAttribute(RefreshProperties.Repaint)}
                Dim objaBDL16Port As Attribute() = {New RefreshPropertiesAttribute(RefreshProperties.Repaint)}
                Dim objaOnOffState As Attribute() = {}

                Dim objaSwitch As Attribute() = {New RefreshPropertiesAttribute(RefreshProperties.Repaint)}
                Dim objaSwState As Attribute() = {}
                Dim objaKey As Attribute() = {}

                Dim objaCfgByte As Attribute() = {New DescriptionAttribute("Encoded bits in the port's config byte.")}
                Dim objaVal1Byte As Attribute() = {New DescriptionAttribute("Encoded bits in the port's first value byte.")}
                Dim objaVal2Byte As Attribute() = {New DescriptionAttribute("Encoded bits in the port's second value byte.")}

                'determine property description and visibility
                Select Case True
                    Case _objPortConfig.Packet Is Nothing
                        DynAttrib.Hide(objaOpCode)

                        DynAttrib.Hide(objaAddress)
                        DynAttrib.Hide(objaBDL16Adr)
                        DynAttrib.Hide(objaBDL16Port)
                        DynAttrib.Hide(objaOnOffState)

                        DynAttrib.Hide(objaSwitch)
                        DynAttrib.Hide(objaSwState)
                        DynAttrib.Hide(objaKey)

                    Case TypeOf _objPortConfig.Packet Is PkInput
                        DynAttrib.Description(objaAddress, "Value 0-4095 identifying the device independent input address.")
                        DynAttrib.Description(objaBDL16Adr, "Value 1-256 identifying the BDL16 device address.")
                        DynAttrib.Description(objaBDL16Port, "Value 1-16 identifying the BDL16 port number.")
                        DynAttrib.Description(objaOnOffState, "Sensor state. (Off = free or low; On = occupied or high)")

                        DynAttrib.Hide(objaSwitch)
                        DynAttrib.Hide(objaSwState)
                        DynAttrib.Hide(objaKey)

                    Case TypeOf _objPortConfig.Packet Is PkSwitchInput
                        DynAttrib.Description(objaAddress, "Value 0-2047 identifying the device input address.")
                        DynAttrib.Hide(objaBDL16Adr)
                        DynAttrib.Hide(objaBDL16Port)
                        DynAttrib.Description(objaOnOffState, "Sensor state. (Off = free or low; On = occupied or high)")

                        DynAttrib.Hide(objaSwitch)
                        DynAttrib.Hide(objaSwState)
                        DynAttrib.Hide(objaKey)

                    Case TypeOf _objPortConfig.Packet Is PkSetSwitch
                        DynAttrib.Hide(objaAddress)
                        DynAttrib.Hide(objaBDL16Adr)
                        DynAttrib.Hide(objaBDL16Port)
                        DynAttrib.Hide(objaOnOffState)

                        DynAttrib.Description(objaSwitch, "Value 1-2048 identifying the switch number.")
                        DynAttrib.Description(objaSwState, "The state of the switch.")
                        DynAttrib.Description(objaKey, "A key down event 'On' or key up event 'Off'.")
                End Select

                objList.Add("Config", New DynamicDescriptor(objaConfig))
                objList.Add("OpCode", New DynamicDescriptor("Op Code", objaOpCode))
                objList.Add("Address", New DynamicDescriptor("Device Adr", objaAddress))
                objList.Add("BDL16Adr", New DynamicDescriptor("BDL16x Adr", objaBDL16Adr))
                objList.Add("BDL16Port", New DynamicDescriptor("BDL16x Port", objaBDL16Port))
                objList.Add("OnOffState", New DynamicDescriptor("State", objaOnOffState))
                objList.Add("Switch", New DynamicDescriptor(objaSwitch))
                objList.Add("SwitchState", New DynamicDescriptor("State", objaSwState))
                objList.Add("Key", New DynamicDescriptor(objaKey))
                objList.Add("ConfigByte", New DynamicDescriptor("Config Byte", objaCfgByte))
                objList.Add("Value1Byte", New DynamicDescriptor("Value1 Byte", objaVal1Byte))
                objList.Add("Value2Byte", New DynamicDescriptor("Value2 Byte", objaVal2Byte))
                Return objList
            End Function

        End Class

#End Region

    End Class

End Namespace
