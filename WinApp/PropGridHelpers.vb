Imports System.Collections.Specialized
Imports System.ComponentModel
Imports System.Windows.Forms.Design
Imports System.Drawing.Design
Imports RRAutoLib.CTC
Imports RRAutoLib.Loconet
Imports RRAuto.DrawHelper


Namespace PropertyGridHelpers

#Region "Dynamic Attributes Helpers"

    'used to handle how an object's properties are displayed in property grid
    Public NotInheritable Class DynamicPropGridConverter
        Inherits TypeConverter

        Public Overloads Overrides Function GetPropertiesSupported(context As System.ComponentModel.ITypeDescriptorContext) As Boolean
            Return True     'needed so GetProperties() function gets called
        End Function

        Public Overloads Overrides Function GetProperties(context As System.ComponentModel.ITypeDescriptorContext,  value As Object,  attributes() As System.Attribute) As System.ComponentModel.PropertyDescriptorCollection
            'get the dynamic configuration of the property grid
            Dim objDynamicAttrib As ListDictionary = DirectCast(value, IDynamicPropertyGrid).PropGridDynamicAttrib

            'get the original property descriptor collection
            Dim objPropDescColSrc As PropertyDescriptorCollection = TypeDescriptor.GetProperties(value, False)

            'this is the converted property descriptor collection
            Dim objPropDescColTgt As PropertyDescriptorCollection

            If objDynamicAttrib Is Nothing Then
                'if no dynamic configuration has been defined then keep the original propety descriptor collection
                objPropDescColTgt = objPropDescColSrc
            Else
                objPropDescColTgt = New PropertyDescriptorCollection(New PropertyDescriptor() {})

                For Each objPropDescSrc As PropertyDescriptor In objPropDescColSrc
                    Dim objPropDescTgt As PropertyDescriptor

                    Dim sctDynamicDescriptor As DynamicDescriptor = objDynamicAttrib.Item(objPropDescSrc.Name)
                    If sctDynamicDescriptor Is Nothing Then
                        'if no dynamic configuration was given for this particular property then keep the original descriptor
                        objPropDescTgt = objPropDescSrc
                    Else
                        'assign dynamic configuration
                        objPropDescTgt = New PropertyDescExt(objPropDescSrc, sctDynamicDescriptor.DisplayName, sctDynamicDescriptor.Attributes)
                    End If
                    'add new property descriptor to the new colleaction only if it is browsable
                    If objPropDescTgt.IsBrowsable Then objPropDescColTgt.Add(objPropDescTgt)
                Next
            End If

            'return the new custom sorted property descriptor collection
            Return objPropDescColTgt.Sort(DirectCast(value, IDynamicPropertyGrid).PropGridSort)
        End Function
    End Class

    'used to chance the attributes of a PropertyDescriptor
    Public NotInheritable Class PropertyDescExt
        Inherits PropertyDescriptor
        Private _objPropDescSource As PropertyDescriptor
        Private _strDisplayName As String

        Public Sub New(objPropDescSource As PropertyDescriptor,  strDisplayName As String,  objaAttrs() As Attribute)
            MyBase.New(objPropDescSource, objaAttrs)
            _objPropDescSource = objPropDescSource
            _strDisplayName = strDisplayName
        End Sub

        Public Overrides ReadOnly Property DisplayName() As String
            Get
                If _strDisplayName Is Nothing Then
                    Return MyBase.DisplayName
                Else
                    Return _strDisplayName
                End If
            End Get
        End Property

        Public Overrides Function GetValue(component As Object) As Object
            Return _objPropDescSource.GetValue(component)
        End Function

        Public Overrides Sub SetValue(component As Object,  value As Object)
            _objPropDescSource.SetValue(component, value)
        End Sub

        Public Overrides ReadOnly Property IsReadOnly() As Boolean
            Get
                For Each objAttribute As Attribute In AttributeArray
                    If TypeOf objAttribute Is ReadOnlyAttribute Then Return (DirectCast(objAttribute, ReadOnlyAttribute).IsReadOnly)
                Next
                Return False
            End Get
        End Property

        Public Overrides Function CanResetValue(component As Object) As Boolean
            Return _objPropDescSource.CanResetValue(component)
        End Function

        Public Overrides Sub ResetValue(component As Object)
            _objPropDescSource.ResetValue(component)
        End Sub

        Public Overrides ReadOnly Property ComponentType() As System.Type
            Get
                Return _objPropDescSource.ComponentType
            End Get
        End Property

        Public Overrides Function ShouldSerializeValue(component As Object) As Boolean
            Return _objPropDescSource.ShouldSerializeValue(component)
        End Function

        Public Overrides ReadOnly Property PropertyType() As System.Type
            Get
                Return _objPropDescSource.PropertyType
            End Get
        End Property
    End Class

    Public NotInheritable Class DynamicDescriptor
        Public DisplayName As String
        Public Attributes As Attribute() = {}

        Public Sub New(objaAttributes As Attribute())
            Attributes = objaAttributes
        End Sub

        Public Sub New(strDisplayName As String)
            DisplayName = strDisplayName
        End Sub

        Public Sub New(strDisplayName As String,  objaAttributes As Attribute())
            DisplayName = strDisplayName
            Attributes = objaAttributes
        End Sub
    End Class

    Public Class DynAttrib

        Public Shared Sub Join(ByRef objAttribs1 As Attribute(),  objAttribs2 As Attribute())
            ReDim Preserve objAttribs1(objAttribs1.Length - 1 + objAttribs2.Length)
            objAttribs2.CopyTo(objAttribs1, objAttribs1.Length - objAttribs2.Length)
        End Sub

        Public Shared Sub Hide(ByRef objAttribs As Attribute())
            Join(objAttribs, New Attribute() {New BrowsableAttribute(False)})
        End Sub

        Public Shared Sub Disable(ByRef objAttribs As Attribute())
            Join(objAttribs, New Attribute() {New ReadOnlyAttribute(True)})
        End Sub

        Public Shared Sub Description(ByRef objAttribs As Attribute(),  strDesc As String)
            Join(objAttribs, New Attribute() {New DescriptionAttribute(strDesc)})
        End Sub

    End Class

    Public Interface IDynamicPropertyGrid
        Function PropGridSort() As String()
        Function PropGridDynamicAttrib() As ListDictionary
    End Interface

    Public Interface IWrapperObject
        ReadOnly Property WrappedObject() As Object
    End Interface

#End Region


#Region "RRAuto Options Converters"

    Public NotInheritable Class ComPortConverter
        Inherits TypeConverter

        Public Overloads Overrides Function GetStandardValuesSupported(context As ITypeDescriptorContext) As Boolean
            Return True
        End Function

        Public Overloads Overrides Function GetStandardValuesExclusive(context As ITypeDescriptorContext) As Boolean
            Return True
        End Function

        Public Overloads Overrides Function GetStandardValues(context As ITypeDescriptorContext) As StandardValuesCollection
            Dim objList As New List(Of String)
            For Each strPortName As String In My.Computer.Ports.SerialPortNames
                objList.Add(strPortName)
            Next
            objList.Sort()
            Return New StandardValuesCollection(objList)
        End Function

    End Class

#End Region

#Region "Ctc Objects Converters"

    Public NotInheritable Class CtcObjectBaseConverter
        Inherits TypeConverter

        Private Class CtcObjectBaseComparer
            Implements IComparer(Of CtcObjectBase)

            Public Function Compare(x As CtcObjectBase,  y As CtcObjectBase) As Integer Implements IComparer(Of CtcObjectBase).Compare
                Select Case True
                    Case x Is Nothing And y Is Nothing
                        Return 0
                    Case y Is Nothing
                        Return 1
                    Case x Is Nothing
                        Return -1
                    Case x.Name = y.Name
                        Return 0
                    Case x.Name > y.Name
                        Return 1
                    Case x.Name < y.Name
                        Return -1
                End Select
            End Function
        End Class

        Public Overloads Overrides Function GetStandardValuesSupported(context As ITypeDescriptorContext) As Boolean
            Return True
        End Function

        Public Overloads Overrides Function GetStandardValues(context As ITypeDescriptorContext) As StandardValuesCollection
            Dim objList As New List(Of CtcObjectBase)
            objList.Add(Nothing)
            Select Case context.PropertyDescriptor.PropertyType.Name
                Case "Block"
                    For Each objBlock As Block In CtcService.Blocks
                        objList.Add(objBlock)
                    Next
                Case "Sensor"
                    For Each objSensor As Sensor In CtcService.Sensors
                        objList.Add(objSensor)
                    Next
            End Select
            objList.Sort(New CtcObjectBaseComparer)
            Return New StandardValuesCollection(objList)
        End Function

        Public Overloads Overrides Function GetStandardValuesExclusive(context As ITypeDescriptorContext) As Boolean
            Return True
        End Function

        Public Overloads Overrides Function CanConvertTo(context As ITypeDescriptorContext,  destinationType As System.Type) As Boolean
            If destinationType Is GetType(String) Then
                Return True
            End If
            Return MyBase.CanConvertTo(context, destinationType)
        End Function

        Public Overloads Overrides Function ConvertTo(context As ITypeDescriptorContext,  culture As System.Globalization.CultureInfo,  value As Object,  destinationType As System.Type) As Object
            Select Case True
                Case value Is Nothing
                    Return "None"
                Case TypeOf value Is CtcObjectBase
                    Return DirectCast(value, CtcObjectBase).Name
                Case Else
                    Return MyBase.ConvertTo(context, culture, value, destinationType)
            End Select
        End Function

        Public Overloads Overrides Function CanConvertFrom(context As ITypeDescriptorContext,  sourceType As System.Type) As Boolean
            If sourceType Is GetType(String) Then
                Return True
            End If
            Return MyBase.CanConvertFrom(context, sourceType)
        End Function

        Public Overloads Overrides Function ConvertFrom(context As ITypeDescriptorContext,  culture As System.Globalization.CultureInfo,  value As Object) As Object
            Select Case value
                Case "None"
                    Return Nothing
                Case Else
                    Select Case context.PropertyDescriptor.PropertyType.Name
                        Case "Block"
                            Return CtcService.Blocks(value)
                        Case "Sensor"
                            Return CtcService.Sensors(value)
                    End Select
            End Select
        End Function

    End Class

    Public NotInheritable Class LocationConverter
        Inherits ExpandableObjectConverter

        Public Overloads Overrides Function CanConvertTo(context As ITypeDescriptorContext,  destinationType As Type) As Boolean
            If (destinationType Is GetType(Location)) Then
                Return True
            End If
            Return MyBase.CanConvertFrom(context, destinationType)
        End Function

        Public Overloads Overrides Function ConvertTo(context As ITypeDescriptorContext,  culture As System.Globalization.CultureInfo,  value As Object,  destinationType As System.Type) As Object
            If (destinationType Is GetType(System.String) AndAlso TypeOf value Is Location) Then
                Dim sctLocation As Location = DirectCast(value, Location)
                Return sctLocation.Col & ", " & sctLocation.Row
            End If
            Return MyBase.ConvertTo(context, culture, value, destinationType)
        End Function

        Public Overloads Overrides Function CanConvertFrom(context As ITypeDescriptorContext,  sourceType As System.Type) As Boolean
            If sourceType Is GetType(String) Then
                Return True
            End If
            Return MyBase.CanConvertFrom(context, sourceType)
        End Function

        Public Overloads Overrides Function ConvertFrom(context As ITypeDescriptorContext,  culture As System.Globalization.CultureInfo,  value As Object) As Object
            If TypeOf value Is String Then
                Try
                    Dim s As String = CStr(value)
                    Dim comma As Integer = s.IndexOf(",")

                    If (comma <> -1) Then
                        Dim checkCol As String = s.Substring(0, (comma))
                        Dim checkRow As String = s.Substring(comma + 1)

                        Return New Location(Integer.Parse(checkCol), Integer.Parse(checkRow))
                    End If
                Catch
                    Throw New ArgumentException("Can not convert '" & CStr(value) & "' to Location type.")
                End Try
            End If
            Return MyBase.ConvertFrom(context, culture, value)
        End Function
    End Class

    Public NotInheritable Class SensorDeviceConverter
        Inherits TypeConverter

        Public Overloads Overrides Function GetStandardValuesSupported(context As ITypeDescriptorContext) As Boolean
            Return True     'tells the propertygrid to display a combobox
        End Function

        Public Overloads Overrides Function GetStandardValuesExclusive(context As ITypeDescriptorContext) As Boolean
            Return True     'makes the combobox select only; false would allow free text entry
        End Function

        Public Overloads Overrides Function GetStandardValues(context As ITypeDescriptorContext) As StandardValuesCollection
            Select Case DirectCast(context.Instance, PgSensor).ObservePacket
                Case Sensor.ObservePacketType.InputRep
                    Return New StandardValuesCollection(New Sensor.DeviceType() {Sensor.DeviceType.BDL16x, Sensor.DeviceType.DS54, Sensor.DeviceType.Other})
                Case Sensor.ObservePacketType.SwitchRep
                    Return New StandardValuesCollection(New Sensor.DeviceType() {Sensor.DeviceType.DS54, Sensor.DeviceType.Other})
                Case Sensor.ObservePacketType.MultiSense
                    Return New StandardValuesCollection(New Sensor.DeviceType() {Sensor.DeviceType.BDL16x, Sensor.DeviceType.Other})
                Case Sensor.ObservePacketType.SecurityElem
                    Return New StandardValuesCollection(New Sensor.DeviceType() {Sensor.DeviceType.Other})
                Case Sensor.ObservePacketType.Lissy
                    Return New StandardValuesCollection(New Sensor.DeviceType() {Sensor.DeviceType.Other})
            End Select
        End Function

        Public Overloads Overrides Function CanConvertFrom(context As System.ComponentModel.ITypeDescriptorContext,  sourceType As System.Type) As Boolean
            Return True     'since free text is not allowed we can always convert from the string items in the list
        End Function

        Public Overloads Overrides Function ConvertFrom(context As ITypeDescriptorContext,  culture As System.Globalization.CultureInfo,  value As Object) As Object
            Return [Enum].Parse(GetType(Sensor.DeviceType), value)
        End Function

    End Class

    Public NotInheritable Class SignalLookConverter
        Inherits StringConverter

        Public Overloads Overrides Function GetStandardValuesSupported(context As ITypeDescriptorContext) As Boolean
            Return True     'tells the propertygrid to display a combobox
        End Function

        Public Overloads Overrides Function GetStandardValuesExclusive(context As ITypeDescriptorContext) As Boolean
            Return True     'makes the combobox select only; false would allow free text entry
        End Function

        Public Overloads Overrides Function GetStandardValues(context As ITypeDescriptorContext) As StandardValuesCollection
            Dim objList As New List(Of String)
            For Each enuLookKey As SignalLookKey In [Enum].GetValues(GetType(SignalLookKey))
                objList.Add(GetSignalLook(enuLookKey).Name)
            Next
            Return New StandardValuesCollection(objList)
        End Function

    End Class

    Public NotInheritable Class AspectConverter
        Inherits TypeConverter

        Public Overloads Overrides Function GetStandardValuesSupported(context As ITypeDescriptorContext) As Boolean
            Return True
        End Function

        Public Overloads Overrides Function GetStandardValuesExclusive(context As ITypeDescriptorContext) As Boolean
            Return True
        End Function

        Public Overloads Overrides Function GetStandardValues(context As ITypeDescriptorContext) As StandardValuesCollection
            Dim objList As New List(Of Signal.SignalAspect)
            For Each objState As PkStatesList.State In DirectCast(context.Instance, PgSignal).States
                objList.Add(DirectCast(objState.Value, Signal.SignalAspect))
            Next
            Return New StandardValuesCollection(objList)
        End Function

        Public Overloads Overrides Function CanConvertFrom(context As System.ComponentModel.ITypeDescriptorContext,  sourceType As System.Type) As Boolean
            If sourceType Is GetType(String) Then
                Return True
            End If
            Return MyBase.CanConvertFrom(context, sourceType)
        End Function

        Public Overloads Overrides Function ConvertFrom(context As ITypeDescriptorContext,  culture As System.Globalization.CultureInfo,  value As Object) As Object
            If (TypeOf value Is String) Then
                Try
                    Return System.Enum.Parse(GetType(Signal.SignalAspect), value)
                Catch
                    Throw New ArgumentException("Can not convert '" & CStr(value) & "' to SignalAspect enum type.")
                End Try
            End If
            Return MyBase.ConvertFrom(context, culture, value)
        End Function

    End Class

    Public NotInheritable Class TrackStateConverter
        Inherits TypeConverter

        Public Overloads Overrides Function GetStandardValuesSupported(context As ITypeDescriptorContext) As Boolean
            Return True
        End Function

        Public Overloads Overrides Function GetStandardValues(context As ITypeDescriptorContext) As StandardValuesCollection
            Dim objList As New List(Of Track.TrackState)
            For Each objState As PkStatesList.State In DirectCast(context.Instance, PgTrack).States
                objList.Add(DirectCast(objState.Value, Track.TrackState))
            Next
            Return New StandardValuesCollection(objList)
        End Function

        Public Overloads Overrides Function CanConvertFrom(context As System.ComponentModel.ITypeDescriptorContext,  sourceType As System.Type) As Boolean
            If sourceType Is GetType(String) Then
                Return True
            End If
            Return MyBase.CanConvertFrom(context, sourceType)
        End Function

        Public Overloads Overrides Function ConvertFrom(context As ITypeDescriptorContext,  culture As System.Globalization.CultureInfo,  value As Object) As Object
            If (TypeOf value Is String) Then
                Try
                    Return System.Enum.Parse(GetType(Track.TrackState), value)
                Catch
                    Throw New ArgumentException("Can not convert '" & CStr(value) & "' to TrackState enum type.")
                End Try
            End If
            Return MyBase.ConvertFrom(context, culture, value)
        End Function

        Public Overloads Overrides Function GetStandardValuesExclusive(context As ITypeDescriptorContext) As Boolean
            Return True
        End Function
    End Class

    Public NotInheritable Class AccessoryStateConverter
        Inherits TypeConverter

        Public Overloads Overrides Function GetStandardValuesSupported(context As ITypeDescriptorContext) As Boolean
            Return True
        End Function

        Public Overloads Overrides Function GetStandardValues(context As ITypeDescriptorContext) As StandardValuesCollection
            Dim objList As New List(Of String)
            For Each objState As PkStatesList.State In DirectCast(context.Instance, PgAccessory).States
                objList.Add(objState.Name)
            Next
            Return New StandardValuesCollection(objList)
        End Function

        Public Overloads Overrides Function CanConvertFrom(context As System.ComponentModel.ITypeDescriptorContext,  sourceType As System.Type) As Boolean
            If sourceType Is GetType(String) Then Return True
            Return MyBase.CanConvertFrom(context, sourceType)
        End Function

        Public Overloads Overrides Function ConvertFrom(context As ITypeDescriptorContext,  culture As System.Globalization.CultureInfo,  value As Object) As Object
            If (TypeOf value Is String) Then Return value
            Return MyBase.ConvertFrom(context, culture, value)
        End Function

        Public Overloads Overrides Function GetStandardValuesExclusive(context As ITypeDescriptorContext) As Boolean
            Return True
        End Function
    End Class

#End Region

#Region "Packet Converters"

    Public NotInheritable Class DccInstrConverter
        Inherits TypeConverter

        Public Overloads Overrides Function GetStandardValuesExclusive(context As ITypeDescriptorContext) As Boolean
            Return True
        End Function

        Public Overloads Overrides Function GetStandardValuesSupported(context As ITypeDescriptorContext) As Boolean
            Return True
        End Function

        Public Overloads Overrides Function GetStandardValues(context As ITypeDescriptorContext) As StandardValuesCollection
            'only a subset of all the instruction types are supported
            Return New StandardValuesCollection(New PkImmediate.DccInstrType() {
                PkImmediate.DccInstrType.Func0to4, PkImmediate.DccInstrType.Func5to8, PkImmediate.DccInstrType.Func9to12,
                PkImmediate.DccInstrType.Func13To20, PkImmediate.DccInstrType.Func21To28})
        End Function

        Public Overloads Overrides Function CanConvertFrom(context As System.ComponentModel.ITypeDescriptorContext,  sourceType As System.Type) As Boolean
            'since the entry is limited to the list we are guaranteed that the entry can be converted
            Return True
        End Function

        Public Overloads Overrides Function ConvertFrom(context As ITypeDescriptorContext,  culture As System.Globalization.CultureInfo,  value As Object) As Object
            'since the entry is limited to the list we are guaranteed that the entry can be converted
            Return System.Enum.Parse(GetType(PkImmediate.DccInstrType), value)
        End Function

    End Class

#End Region


#Region "UI Editors"

    Public NotInheritable Class MultiPortEditor
        Inherits UITypeEditor

        Public Overloads Overrides Function EditValue(context As ITypeDescriptorContext,  provider As IServiceProvider,  value As Object) As Object
            If context IsNot Nothing And context.Instance IsNot Nothing And provider IsNot Nothing Then
                Dim objService As IWindowsFormsEditorService = provider.GetService(GetType(IWindowsFormsEditorService))
                If objService IsNot Nothing Then
                    Dim objControl As New UIEditors.MultiPortEditorControl
                    objControl.Configuration = value
                    objService.DropDownControl(objControl)
                    value = objControl.Configuration
                    objControl.Dispose()
                End If
            End If
            Return value
        End Function

        Public Overloads Overrides Function GetEditStyle(context As ITypeDescriptorContext) As UITypeEditorEditStyle
            If context IsNot Nothing AndAlso context.Instance IsNot Nothing Then
                Return UITypeEditorEditStyle.DropDown
            End If
            Return MyBase.GetEditStyle(context)
        End Function

    End Class

    Public NotInheritable Class DccFunctionsEditor
        Inherits UITypeEditor

        Public Overloads Overrides Function EditValue(context As ITypeDescriptorContext,  provider As IServiceProvider,  value As Object) As Object
            If context IsNot Nothing And context.Instance IsNot Nothing And provider IsNot Nothing Then
                Dim objService As IWindowsFormsEditorService = provider.GetService(GetType(IWindowsFormsEditorService))
                If objService IsNot Nothing Then
                    Dim objControl As New UIEditors.DccFunctionsEditorControl(value)
                    objService.DropDownControl(objControl)
                    value = objControl.Configuration
                    objControl.Dispose()
                End If
            End If
            Return value
        End Function

        Public Overloads Overrides Function GetEditStyle(context As ITypeDescriptorContext) As UITypeEditorEditStyle
            If context IsNot Nothing AndAlso context.Instance IsNot Nothing Then
                Return UITypeEditorEditStyle.DropDown
            End If
            Return MyBase.GetEditStyle(context)
        End Function

    End Class

    'this used to be a UIEditor that poped up a form; kept it as a reference on how to do this
    'Public NotInheritable Class EventScriptEditor
    '    Inherits UITypeEditor

    '    Public Overloads Overrides Function EditValue(context As ITypeDescriptorContext,  provider As IServiceProvider,  value As Object) As Object
    '        If context IsNot Nothing And context.Instance IsNot Nothing And provider IsNot Nothing Then
    '            Dim objService As IWindowsFormsEditorService = provider.GetService(GetType(IWindowsFormsEditorService))
    '            If objService IsNot Nothing Then
    '                Dim objForm As New ScriptEditorForm
    '                objForm.Scripts = value
    '                objService.ShowDialog(objForm)
    '                value = objForm.Scripts
    '                objForm.Dispose()
    '            End If
    '        End If
    '        Return value
    '    End Function

    '    Public Overloads Overrides Function GetEditStyle(context As ITypeDescriptorContext) As UITypeEditorEditStyle
    '        If context IsNot Nothing AndAlso context.Instance IsNot Nothing Then
    '            Return UITypeEditorEditStyle.Modal
    '        End If
    '        Return MyBase.GetEditStyle(context)
    '    End Function

    'End Class

#End Region


#Region "Wrapper for RRAuto Options"

    <TypeConverter(GetType(DynamicPropGridConverter))> Public NotInheritable Class PgOptions
        Implements IDynamicPropertyGrid

        'application
        Private _blnLoadLastFile As Boolean = My.Settings.LoadLastFile
        Private _srtListMaxLines As UShort = My.Settings.ListMaxLines

        'loconet
        Private _strComPort As String = "COM" & My.Settings.ComPort
        Private _enuBaudRate As BaudRate = My.Settings.BaudRate
        Private _blnFlowControl As Boolean = My.Settings.FlowControl
        Private _blnSimulationMode As Boolean = My.Settings.SimulationMode

        'operation
        Private _blnBroadcastStates As Boolean = My.Settings.BroadcastStates
        Private _blnQuerySensors As Boolean = My.Settings.QuerySensors
        Private _blnClearCtcEvents As Boolean = My.Settings.ClearCtcEvents
        Private _blnForceSetState As Boolean = My.Settings.ForceSetState

        'remoting
        Private _srtRemServPort As UShort = My.Settings.RemServPort

        'switchboard
        Private _dblDefaultZoomLevel As Double = My.Settings.DefaultZoomLevel
        Private _intZoomIncrement As Byte = My.Settings.ZoomIncrement
        Friend _sctBackColor As Color = My.Settings.BackColor
        Friend _sctGridColor As Color = My.Settings.GridColor
        Friend _sctTurnoutSetColor As Color = My.Settings.TurnoutSetColor
        Friend _sctTurnoutPendingColor As Color = My.Settings.TurnoutPendingColor
        Friend _sctTurnoutLockedColor As Color = My.Settings.TurnoutLockedColor
        Friend _sctTrackNoBlockColor As Color = My.Settings.TrackNoBlockColor
        Friend _sctFreeBlockColor As Color = My.Settings.FreeBlockColor
        Friend _sctResBlockColor As Color = My.Settings.ResBlockColor
        Friend _sctResOccBlockColor As Color = My.Settings.ResOccBlockColor
        Friend _sctOccBlockColor As Color = My.Settings.OccBlockColor
        Friend _sctSelectionColor As Color = My.Settings.SelectionColor


        <Category("Application"), Description("Open the last used layout on next application launch.")>
        Public Property LoadLastFile() As Boolean
            Get
                Return _blnLoadLastFile
            End Get
            Set(Value As Boolean)
                _blnLoadLastFile = Value
            End Set
        End Property

        <Category("Application"), Description("Number of lines buffered in running lists. (i.e. Loconet Log and CTC Message windows)")>
        Public Property ListMaxLines() As UShort
            Get
                Return _srtListMaxLines
            End Get
            Set(Value As UShort)
                _srtListMaxLines = Value
            End Set
        End Property


        <Category("Loconet"), Description("Physical communication port. You must reconnect to use the new value."), TypeConverter(GetType(ComPortConverter))>
        Public Property ComPort() As String
            Get
                Return _strComPort
            End Get
            Set(Value As String)
                _strComPort = Value
            End Set
        End Property

        <Category("Loconet"), Description("Serial port speed. You must reconnect to use the new value.")>
        Public Property BaudRate() As BaudRate
            Get
                Return _enuBaudRate
            End Get
            Set(Value As BaudRate)
                _enuBaudRate = Value
            End Set
        End Property

        <Category("Loconet"), Description("Enable RTS/CTS serial port flow control. You must reconnect to use the new value.")>
        Public Property FlowControl() As Boolean
            Get
                Return _blnFlowControl
            End Get
            Set(Value As Boolean)
                _blnFlowControl = Value
            End Set
        End Property

        <Category("Loconet"), Description("Suppress Loconet packet exchanges to allow operations while disconnected.")>
        Public Property SimulationMode() As Boolean
            Get
                Return _blnSimulationMode
            End Get
            Set(Value As Boolean)
                _blnSimulationMode = Value
            End Set
        End Property


        <Category("Operation"), Description("Transmit state information, on every operation start, to synchronize the physical objects to their virtual counterparts.")>
        Public Property BroadcastStates() As Boolean
            Get
                Return _blnBroadcastStates
            End Get
            Set(Value As Boolean)
                _blnBroadcastStates = Value
            End Set
        End Property

        <Category("Operation"), Description("Interogate sensors, on every operation start, to synchronize the virtual objects to their physical counterparts.")>
        Public Property QuerySensors() As Boolean
            Get
                Return _blnQuerySensors
            End Get
            Set(Value As Boolean)
                _blnQuerySensors = Value
            End Set
        End Property

        <Category("Operation"), Description("Clear the 'CTC Events' list on every operation start.")>
        Public Property ClearCtcEvents() As Boolean
            Get
                Return _blnClearCtcEvents
            End Get
            Set(Value As Boolean)
                _blnClearCtcEvents = Value
            End Set
        End Property

        <Category("Operation"), Description("Transmit 'Set State' commands even if the target state is already held.")>
        Public Property ForceSetState() As Boolean
            Get
                Return _blnForceSetState
            End Get
            Set(Value As Boolean)
                _blnForceSetState = Value
            End Set
        End Property


        <Category("Remoting"), Description("Listening port number for the HTTP remoting service. You must restart the service to use the new value.")>
        Public Property RemServPort() As UShort
            Get
                Return _srtRemServPort
            End Get
            Set(Value As UShort)
                _srtRemServPort = Value
            End Set
        End Property


        <Category("Switchboard"), Description("Default zoom level of switchboard viewports.")>
        Public Property DefaultZoomLevel() As Double
            Get
                Return _dblDefaultZoomLevel
            End Get
            Set(Value As Double)
                If Value < 0.2 Or Value > 5 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid default zoom level. Valid values are 0.2-5.")
                _dblDefaultZoomLevel = Value
            End Set
        End Property

        <Category("Switchboard"), Description("The amount of zoom scaling applied for each zoom step.")>
        Public Property ZoomIncrement() As Byte
            Get
                Return _intZoomIncrement
            End Get
            Set(Value As Byte)
                If Value < 2 Or Value > 80 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid zoom increment. Valid values are 2-80.")
                _intZoomIncrement = Value
            End Set
        End Property

        <Category("Switchboard"), Description("Color of the switchboard's back ground.")>
        Public Property BackColor() As System.Drawing.Color
            Get
                Return _sctBackColor.Convert
            End Get
            Set(Value As System.Drawing.Color)
                _sctBackColor = Value.Convert(255)
            End Set
        End Property

        <Category("Switchboard"), Description("Color of the switchboard's grid when in edit mode.")>
        Public Property GridColor() As System.Drawing.Color
            Get
                Return _sctGridColor.Convert
            End Get
            Set(Value As System.Drawing.Color)
                _sctGridColor = Value.Convert(255)
            End Set
        End Property

        <Category("Switchboard"), Description("Color used to indicate that a turnout is set but not locked.")>
        Public Property TurnoutSetColor() As System.Drawing.Color
            Get
                Return _sctTurnoutSetColor.Convert
            End Get
            Set(Value As System.Drawing.Color)
                _sctTurnoutSetColor = Value.Convert(255)
            End Set
        End Property

        <Category("Switchboard"), Description("Color used to indicate that a turnout is in a pending change state.")>
        Public Property TurnoutPendingColor() As System.Drawing.Color
            Get
                Return _sctTurnoutPendingColor.Convert
            End Get
            Set(Value As System.Drawing.Color)
                _sctTurnoutPendingColor = Value.Convert(255)
            End Set
        End Property

        <Category("Switchboard"), Description("Color used to indicate that a turnout is locked by a route.")>
        Public Property TurnoutLockedColor() As System.Drawing.Color
            Get
                Return _sctTurnoutLockedColor.Convert
            End Get
            Set(Value As System.Drawing.Color)
                _sctTurnoutLockedColor = Value.Convert(255)
            End Set
        End Property

        <Category("Switchboard"), Description("Color of track that has not been assigned to a block.")>
        Public Property TrackNoBlockColor() As System.Drawing.Color
            Get
                Return _sctTrackNoBlockColor.Convert
            End Get
            Set(Value As System.Drawing.Color)
                _sctTrackNoBlockColor = Value.Convert(255)
            End Set
        End Property

        <Category("Switchboard"), Description("Color of block that is neither reserved nor occupied.")>
        Public Property FreeBlockColor() As System.Drawing.Color
            Get
                Return _sctFreeBlockColor.Convert
            End Get
            Set(Value As System.Drawing.Color)
                _sctFreeBlockColor = Value.Convert(255)
            End Set
        End Property

        <Category("Switchboard"), Description("Color of block that is reserved but not occupied.")>
        Public Property ResBlockColor() As System.Drawing.Color
            Get
                Return _sctResBlockColor.Convert
            End Get
            Set(Value As System.Drawing.Color)
                _sctResBlockColor = Value.Convert(255)
            End Set
        End Property

        <Category("Switchboard"), Description("Color of block that is reserved and occupied.")>
        Public Property ResOccBlockColor() As System.Drawing.Color
            Get
                Return _sctResOccBlockColor.Convert
            End Get
            Set(Value As System.Drawing.Color)
                _sctResOccBlockColor = Value.Convert(255)
            End Set
        End Property

        <Category("Switchboard"), Description("Color of block that is occupied without a reservation.")>
        Public Property OccBlockColor() As System.Drawing.Color
            Get
                Return _sctOccBlockColor.Convert
            End Get
            Set(Value As System.Drawing.Color)
                _sctOccBlockColor = Value.Convert(255)
            End Set
        End Property

        <Category("Switchboard"), Description("Color of a selected items while in edit mode.")>
        Public Property SelectionColor() As System.Drawing.Color
            Get
                Return _sctSelectionColor.Convert
            End Get
            Set(Value As System.Drawing.Color)
                _sctSelectionColor = Value.Convert(255)
            End Set
        End Property

        Public Function PropGridSort() As String() Implements IDynamicPropertyGrid.PropGridSort
            Return New String() {"LoadLastFile", "ListMaxLines",
                                 "ComPort", "BaudRate", "FlowControl", "SimulationMode",
                                 "BroadcastStates", "QuerySensors", "ClearCtcEvents", "ForceSetState",
                                 "RemServPort",
                                 "DefaultZoomLevel", "ZoomIncrement",
                                 "BackColor", "GridColor", "TurnoutSetColor", "TurnoutPendingColor", "TurnoutLockedColor", "TrackNoBlockColor",
                                 "FreeBlockColor", "ResBlockColor", "ResOccBlockColor", "OccBlockColor", "SelectionColor"}
        End Function

        Public Function PropGridDynamicAttrib() As ListDictionary Implements IDynamicPropertyGrid.PropGridDynamicAttrib
            Dim objList As New ListDictionary
            objList.Add("LoadLastFile", New DynamicDescriptor("Load Last File"))
            objList.Add("ListMaxLines", New DynamicDescriptor("List Max Lines"))

            objList.Add("ComPort", New DynamicDescriptor("Com Port"))
            objList.Add("BaudRate", New DynamicDescriptor("Baud Rate"))
            objList.Add("FlowControl", New DynamicDescriptor("Flow Control"))
            objList.Add("SimulationMode", New DynamicDescriptor("Simulation Mode"))

            objList.Add("BroadcastStates", New DynamicDescriptor("Broadcast States"))
            objList.Add("QuerySensors", New DynamicDescriptor("Query Sensors"))
            objList.Add("ClearCtcEvents", New DynamicDescriptor("Clear CTC Events"))
            objList.Add("ForceSetState", New DynamicDescriptor("Force Set State"))

            objList.Add("RemServPort", New DynamicDescriptor("HTTP Listen Port"))

            objList.Add("DefaultZoomLevel", New DynamicDescriptor("Default Zoom Level"))
            objList.Add("ZoomIncrement", New DynamicDescriptor("Zoom Increment"))
            objList.Add("BackColor", New DynamicDescriptor("Background Color"))
            objList.Add("GridColor", New DynamicDescriptor("Grid Color"))
            objList.Add("TurnoutSetColor", New DynamicDescriptor("Turnout Set Color"))
            objList.Add("TurnoutPendingColor", New DynamicDescriptor("Turnout Pending Color"))
            objList.Add("TurnoutLockedColor", New DynamicDescriptor("Turnout Locked Color"))
            objList.Add("TrackNoBlockColor", New DynamicDescriptor("Track Without Block"))
            objList.Add("FreeBlockColor", New DynamicDescriptor("Block Free Color"))
            objList.Add("ResBlockColor", New DynamicDescriptor("Block Reserved Color"))
            objList.Add("ResOccBlockColor", New DynamicDescriptor("Block Res & Occ Color"))
            objList.Add("OccBlockColor", New DynamicDescriptor("Block Occupied Color"))
            objList.Add("SelectionColor", New DynamicDescriptor("Selection Color"))

            Return objList
        End Function

    End Class

#End Region

#Region "Wrappers for Ctc Objects"

    Public NotInheritable Class PgCtcObjectListBase
        Implements IWrapperObject
        Protected _objWrappedObject As ICtcObjectListBase

        <Browsable(False)> Public ReadOnly Property WrappedObject() As Object Implements IWrapperObject.WrappedObject
            Get
                Return _objWrappedObject
            End Get
        End Property

        Public Sub New(objCtcObjectListBase As ICtcObjectListBase)
            _objWrappedObject = objCtcObjectListBase
        End Sub

        <Description("Number or objects in the collection.")> Public ReadOnly Property Count() As Integer
            Get
                Return _objWrappedObject.Count
            End Get
        End Property

    End Class

    <TypeConverter(GetType(DynamicPropGridConverter))> Public MustInherit Class PgCtcObjectBase(Of T As CtcObjectBase)
        Implements IDynamicPropertyGrid, IWrapperObject
        Protected _objWrappedObject As T

        <Browsable(False)> Public ReadOnly Property WrappedObject() As Object Implements IWrapperObject.WrappedObject
            Get
                Return _objWrappedObject
            End Get
        End Property

        Public Property Name() As String
            Get
                Return _objWrappedObject.Name
            End Get
            Set(strValue As String)
                _objWrappedObject.Name = strValue
            End Set
        End Property

        Protected Function ShouldSerializeName() As Boolean
            '"ShouldSerialize" & ProperyName - makes default names not be bold; similar to DefaultValueAttribute but more customizable
            Return _objWrappedObject.Name <> _objWrappedObject.DefaultName
        End Function

        Public Overridable Function PropGridSort() As String() Implements IDynamicPropertyGrid.PropGridSort
        End Function

        Public Overridable Function PropGridDynamicAttrib() As ListDictionary Implements IDynamicPropertyGrid.PropGridDynamicAttrib
            Dim objList As New ListDictionary
            Dim objaName As Attribute() = {New DescriptionAttribute("User defined identifier for this object.")}

            If CtcService.IsStarted Then
                DynAttrib.Disable(objaName)
            End If

            objList.Add("Name", New DynamicDescriptor(objaName))
            Return objList
        End Function

    End Class


    Public NotInheritable Class PgTrack
        Inherits PgCtcObjectBase(Of Track)

        Public Sub New(objTrack As Track)
            _objWrappedObject = objTrack
        End Sub

        Public Property Location() As Location
            Get
                Return _objWrappedObject.Location
            End Get
            Set(Value As Location)
                _objWrappedObject.Location = Value
            End Set
        End Property

        Public Property Type() As Track.TrackType
            Get
                Return _objWrappedObject.Type
            End Get
            Set(Value As Track.TrackType)
                _objWrappedObject.Type = Value
            End Set
        End Property

        <Browsable(False)> Public ReadOnly Property States() As PkStatesList
            Get
                Return _objWrappedObject.States
            End Get
        End Property

        Public Property Orientation() As Byte
            Get
                Return _objWrappedObject.Orientation
            End Get
            Set(Value As Byte)
                _objWrappedObject.Orientation = Value
            End Set
        End Property

        Public Property Block() As Block
            Get
                Return _objWrappedObject.Block
            End Get
            Set(Value As Block)
                _objWrappedObject.Block = Value
            End Set
        End Property

        Public Property SwitchTime() As UShort
            Get
                Return _objWrappedObject.States.SwitchTime
            End Get
            Set(Value As UShort)
                _objWrappedObject.States.SwitchTime = Value
            End Set
        End Property

        Public Property DefaultState() As Track.TrackState
            Get
                Return _objWrappedObject.States.Default.Value
            End Get
            Set(Value As Track.TrackState)
                _objWrappedObject.States.Default = _objWrappedObject.States(Value)
            End Set
        End Property

        Public ReadOnly Property ActiveState() As Track.TrackState
            Get
                Return _objWrappedObject.State.Value
            End Get
        End Property

        Public ReadOnly Property Disposition() As PkStatesList.StateDisposition
            Get
                Return _objWrappedObject.States.Disposition
            End Get
        End Property

        Public Overrides Function PropGridSort() As String()
            Return New String() {"Name", "Location", "Type", "Orientation", "Block", "SwitchTime", "DefaultState", "ActiveState", "Disposition"}
        End Function

        Public Overrides Function PropGridDynamicAttrib() As ListDictionary
            Dim objList As ListDictionary = MyBase.PropGridDynamicAttrib()

            Dim blnShowForTurnouts As Boolean = _objWrappedObject.IsTurnout

            Dim objaLoc As Attribute() = {New DescriptionAttribute("Location of the track element on the switchboard grid."), New TypeConverterAttribute(GetType(LocationConverter))}
            Dim objaType As Attribute() = {New DescriptionAttribute("The configuration type of this track element."), New RefreshPropertiesAttribute(RefreshProperties.All)} 'RefreshProperties.All re-queries the PropertyGridConverter; needed for changes between track and turnouts  
            Dim objaOrient As Attribute() = {New DescriptionAttribute("The rotated position of the track element placed on the switchboard. Valid values are 1-8.")}
            Dim objaBlock As Attribute() = {New DescriptionAttribute("The block that this track is a part of."), New TypeConverterAttribute(GetType(CtcObjectBaseConverter))}
            Dim objaSwTime As Attribute() = {New DescriptionAttribute("Time in milliseconds that it takes for this turnout to switch from one state to another."), New BrowsableAttribute(blnShowForTurnouts)}
            Dim objaDefSta As Attribute() = {New DescriptionAttribute("Turnout state assigned when the railroad is reset."), New BrowsableAttribute(blnShowForTurnouts), New TypeConverterAttribute(GetType(TrackStateConverter))}
            Dim objaActSta As Attribute() = {New DescriptionAttribute("Currently active turnout state."), New TypeConverterAttribute(GetType(TrackStateConverter)), New BrowsableAttribute(blnShowForTurnouts)}
            Dim objaDisp As Attribute() = {New DescriptionAttribute("The three disposition values are: Set=(points are set), Pending=(points are being switched), Locked=(points are set and locked)."), New BrowsableAttribute(blnShowForTurnouts)}

            If CtcService.IsStarted Then
                DynAttrib.Disable(objaLoc)
                DynAttrib.Disable(objaType)
                DynAttrib.Disable(objaOrient)
                DynAttrib.Disable(objaBlock)
                DynAttrib.Disable(objaSwTime)
                DynAttrib.Disable(objaDefSta)
            End If

            objList.Add("Location", New DynamicDescriptor(objaLoc))
            objList.Add("Type", New DynamicDescriptor(objaType))
            objList.Add("Orientation", New DynamicDescriptor(objaOrient))
            objList.Add("Block", New DynamicDescriptor(objaBlock))
            objList.Add("SwitchTime", New DynamicDescriptor("Switch Time", objaSwTime))
            objList.Add("DefaultState", New DynamicDescriptor("Default State", objaDefSta))
            objList.Add("ActiveState", New DynamicDescriptor("Active State", objaActSta))
            objList.Add("Disposition", New DynamicDescriptor(objaDisp))
            Return objList
        End Function

    End Class

    Public NotInheritable Class PgBlock
        Inherits PgCtcObjectBase(Of Block)

        Public Sub New(objBlock As Block)
            _objWrappedObject = objBlock
        End Sub

        Public Property Length() As UShort
            Get
                Return _objWrappedObject.Length
            End Get
            Set(Value As UShort)
                _objWrappedObject.Length = Value
            End Set
        End Property

        Public Property HasCatenary() As Boolean
            Get
                Return _objWrappedObject.HasCatenary
            End Get
            Set(Value As Boolean)
                _objWrappedObject.HasCatenary = Value
            End Set
        End Property

        Public Property Sensor() As Sensor
            Get
                Return _objWrappedObject.Sensor
            End Get
            Set(Value As Sensor)
                _objWrappedObject.Sensor = Value
            End Set
        End Property

        Public ReadOnly Property Occupied() As Boolean
            Get
                Return _objWrappedObject.Occupied
            End Get
        End Property

        Public ReadOnly Property Reserved() As Boolean
            Get
                Return _objWrappedObject.Reserved
            End Get
        End Property

        Public Overrides Function PropGridSort() As String()
            Return New String() {"Name", "Length", "HasCatenary", "Sensor", "Occupied", "Reserved"}
        End Function

        Public Overrides Function PropGridDynamicAttrib() As ListDictionary
            Dim objList As ListDictionary = MyBase.PropGridDynamicAttrib()

            Dim objaLen As Attribute() = {New DescriptionAttribute("Length of block in centimeters. (Not currently used)")}
            Dim objaCat As Attribute() = {New DescriptionAttribute("Indicates if this block has over head catenary for engines that require it.")}
            Dim objaSensor As Attribute() = {New DescriptionAttribute("Sensor that triggers occupancy for this block."), New TypeConverterAttribute(GetType(CtcObjectBaseConverter))}
            Dim objaOccup As Attribute() = {New DescriptionAttribute("Indicates if this block is occupied.")}
            Dim objaRes As Attribute() = {New DescriptionAttribute("Indicates if this block is reserved.")}

            If CtcService.IsStarted Then
                DynAttrib.Disable(objaLen)
                DynAttrib.Disable(objaCat)
                DynAttrib.Disable(objaSensor)
            End If

            objList.Add("Length", New DynamicDescriptor(objaLen))
            objList.Add("HasCatenary", New DynamicDescriptor("Has Catenary", objaCat))
            objList.Add("Sensor", New DynamicDescriptor(objaSensor))
            objList.Add("Occupied", New DynamicDescriptor(objaOccup))
            objList.Add("Reserved", New DynamicDescriptor(objaRes))
            Return objList
        End Function

    End Class

    Public NotInheritable Class PgRoute
        Inherits PgCtcObjectBase(Of Route)

        Public Sub New(objRoute As Route)
            _objWrappedObject = objRoute
        End Sub

        Public Property SerializeSwitching() As Boolean
            Get
                Return _objWrappedObject.SerializeSwitching
            End Get
            Set(Value As Boolean)
                _objWrappedObject.SerializeSwitching = Value
            End Set
        End Property

        Public Property Length() As UShort
            Get
                Return _objWrappedObject.Length
            End Get
            Set(Value As UShort)
                _objWrappedObject.Length = Value
            End Set
        End Property

        Public ReadOnly Property State() As Route.RouteState
            Get
                Return _objWrappedObject.State
            End Get
        End Property

        Public Overrides Function PropGridSort() As String()
            Return New String() {"Name", "SerializeSwitching", "Length", "State"}
        End Function

        Public Overrides Function PropGridDynamicAttrib() As ListDictionary
            Dim objList As ListDictionary = MyBase.PropGridDynamicAttrib()

            Dim objaSerSw As Attribute() = {New DescriptionAttribute("If True, the route will serialize the switching of its turnouts.")}
            Dim objaLen As Attribute() = {New DescriptionAttribute("Length of route in centimeters. (Not currently used)")}
            Dim objaState As Attribute() = {New DescriptionAttribute("Activation state of this route.")}

            If CtcService.IsStarted Then
                DynAttrib.Disable(objaSerSw)
                DynAttrib.Disable(objaLen)
            End If

            objList.Add("SerializeSwitching", New DynamicDescriptor("Serialize Sw.", objaSerSw))
            objList.Add("Length", New DynamicDescriptor(objaLen))
            objList.Add("State", New DynamicDescriptor(objaState))
            Return objList
        End Function

    End Class

    Public NotInheritable Class PgSensor
        Inherits PgCtcObjectBase(Of Sensor)

        Public Sub New(objSensor As Sensor)
            _objWrappedObject = objSensor
        End Sub

        Public Property ObservePacket() As Sensor.ObservePacketType
            Get
                Return _objWrappedObject.ObservePacket
            End Get
            Set(Value As Sensor.ObservePacketType)
                _objWrappedObject.ObservePacket = Value
            End Set
        End Property

        Public Property Address() As UShort
            Get
                Return _objWrappedObject.Address
            End Get
            Set(Value As UShort)
                _objWrappedObject.Address = Value
            End Set
        End Property

        Public Property InputDevice() As Sensor.DeviceType
            Get
                Return _objWrappedObject.InputDevice
            End Get
            Set(Value As Sensor.DeviceType)
                _objWrappedObject.InputDevice = Value
            End Set
        End Property

#Region "Helper functions for address conversion"

        Private _objPkInput As New PkInput

        Private Sub LoadPkInput()
            _objPkInput.Address = _objWrappedObject.Address
        End Sub

        Private Sub SavePkInput()
            _objWrappedObject.Address = _objPkInput.Address
        End Sub

        Private _objPkSwitchInput As New PkSwitchInput

        Private Sub LoadPkSwitchInput()
            _objPkSwitchInput.Address = _objWrappedObject.Address
        End Sub

        Private Sub SavePkSwitchInput()
            _objWrappedObject.Address = _objPkSwitchInput.Address
        End Sub

        Private _objPkMultiSense As New PkMultiSense

        Private Sub LoadPkMultiSense()
            _objPkMultiSense.DeviceAddress = _objWrappedObject.Address
        End Sub

        Private Sub SavePkMultiSense()
            _objWrappedObject.Address = _objPkMultiSense.DeviceAddress
        End Sub

#End Region

#Region "PkInput BDL16"

        Public Property IrBDL16Adr() As UShort
            Get
                LoadPkInput()
                Return _objPkInput.BDL16Address
            End Get
            Set(Value As UShort)
                LoadPkInput()
                _objPkInput.BDL16Address = Value
                SavePkInput()
            End Set
        End Property

        Public Property IrBDL16Port() As Byte
            Get
                LoadPkInput()
                Return _objPkInput.BDL16Port
            End Get
            Set(Value As Byte)
                LoadPkInput()
                _objPkInput.BDL16Port = Value
                SavePkInput()
            End Set
        End Property

#End Region

#Region "PkInput DS54"

        Public Property IrDS54Adr() As UShort
            Get
                LoadPkInput()
                Return _objPkInput.DS54Address
            End Get
            Set(Value As UShort)
                LoadPkInput()
                _objPkInput.DS54Address = Value
                SavePkInput()
            End Set
        End Property

        Public Property IrDS54Pair() As Byte
            Get
                LoadPkInput()
                Return _objPkInput.DS54InputPair
            End Get
            Set(Value As Byte)
                LoadPkInput()
                _objPkInput.DS54InputPair = Value
                SavePkInput()
            End Set
        End Property

        Public Property IrDS54Type() As DS54InputType
            Get
                LoadPkInput()
                Return _objPkInput.DS54InputType
            End Get
            Set(Value As DS54InputType)
                LoadPkInput()
                _objPkInput.DS54InputType = Value
                SavePkInput()
            End Set
        End Property

#End Region

#Region "PkSwitchInput DS54"

        Public Property SrDS54Adr() As UShort
            Get
                LoadPkSwitchInput()
                Return _objPkSwitchInput.DS54Address
            End Get
            Set(Value As UShort)
                LoadPkSwitchInput()
                _objPkSwitchInput.DS54Address = Value
                SavePkSwitchInput()
            End Set
        End Property

        Public Property SrDS54Pair() As Byte
            Get
                LoadPkSwitchInput()
                Return _objPkSwitchInput.DS54InputPair
            End Get
            Set(Value As Byte)
                LoadPkSwitchInput()
                _objPkSwitchInput.DS54InputPair = Value
                SavePkSwitchInput()
            End Set
        End Property

        Public Property SrDS54Type() As DS54InputType
            Get
                LoadPkSwitchInput()
                Return _objPkSwitchInput.DS54InputType
            End Get
            Set(Value As DS54InputType)
                LoadPkSwitchInput()
                _objPkSwitchInput.DS54InputType = Value
                SavePkSwitchInput()
            End Set
        End Property

#End Region

#Region "PkMultiSense BDL16"

        Public Property MsBDL16Adr() As UShort
            Get
                LoadPkMultiSense()
                Return _objPkMultiSense.BDL16Address
            End Get
            Set(Value As UShort)
                LoadPkMultiSense()
                _objPkMultiSense.BDL16Address = Value
                SavePkMultiSense()
            End Set
        End Property

        Public Property MsBDL16Zone() As Byte
            Get
                LoadPkMultiSense()
                Return _objPkMultiSense.BDL16Zone
            End Get
            Set(Value As Byte)
                LoadPkMultiSense()
                _objPkMultiSense.BDL16Zone = Value
                SavePkMultiSense()
            End Set
        End Property

#End Region

        Public ReadOnly Property State() As OnOff
            Get
                Return _objWrappedObject.State
            End Get
        End Property

        Public Overrides Function PropGridSort() As String()
            Return New String() {"Name", "ObservePacket", "Address", "InputDevice",
                                 "IrBDL16Adr", "IrBDL16Port",
                                 "IrDS54Adr", "IrDS54Pair", "IrDS54Type",
                                 "SrDS54Adr", "SrDS54Pair", "SrDS54Type",
                                 "MsBDL16Adr", "MsBDL16Zone",
                                 "State"}
        End Function

        Public Overrides Function PropGridDynamicAttrib() As ListDictionary
            Dim objList As ListDictionary = MyBase.PropGridDynamicAttrib()

            Dim objaObservePk As Attribute() = {New DescriptionAttribute("Loconet packet type to observe for sensor events."), New RefreshPropertiesAttribute(RefreshProperties.All)}
            Dim objaAddress As Attribute() = {New DescriptionAttribute("Device independent address."), New RefreshPropertiesAttribute(RefreshProperties.Repaint)}
            Dim objaDevice As Attribute() = {New DescriptionAttribute("Device type which determines how the address space bits are allocated."), New TypeConverterAttribute(GetType(SensorDeviceConverter)), New RefreshPropertiesAttribute(RefreshProperties.All)}

            Dim objaIrBDL16Adr As Attribute() = {New DescriptionAttribute("Value 1-256 identifying the BDL16x address."), New RefreshPropertiesAttribute(RefreshProperties.Repaint)}
            Dim objaIrBDL16Port As Attribute() = {New DescriptionAttribute("Value 1-16 identifying the BDL16x port number."), New RefreshPropertiesAttribute(RefreshProperties.Repaint)}

            Dim objaIrDS54Adr As Attribute() = {New DescriptionAttribute("Value 1-512 identifying the DS54 address."), New RefreshPropertiesAttribute(RefreshProperties.Repaint)}
            Dim objaIrDS54Pair As Attribute() = {New DescriptionAttribute("Value 1-4 identifying the DS54 input pair."), New RefreshPropertiesAttribute(RefreshProperties.Repaint)}
            Dim objaIrDS54Type As Attribute() = {New DescriptionAttribute("The input type of a DS54 input pair."), New RefreshPropertiesAttribute(RefreshProperties.Repaint)}

            Dim objaSrDS54Adr As Attribute() = {New DescriptionAttribute("Value 1-512 identifying the DS54 address."), New RefreshPropertiesAttribute(RefreshProperties.Repaint)}
            Dim objaSrDS54Pair As Attribute() = {New DescriptionAttribute("Value 1-4 identifying the DS54 input pair."), New RefreshPropertiesAttribute(RefreshProperties.Repaint)}
            Dim objaSrDS54Type As Attribute() = {New DescriptionAttribute("The input type of a DS54 input pair."), New RefreshPropertiesAttribute(RefreshProperties.Repaint)}

            Dim objaMsBDL16Adr As Attribute() = {New DescriptionAttribute("Value 1-256 identifying the BDL16x address."), New RefreshPropertiesAttribute(RefreshProperties.Repaint)}
            Dim objaMsBDL16Zone As Attribute() = {New DescriptionAttribute("Value 1-8 identifying the BDL16x zone number."), New RefreshPropertiesAttribute(RefreshProperties.Repaint)}

            Dim objaState As Attribute() = {New DescriptionAttribute("The current state of the sensor.")}

            If Not (Me.ObservePacket = Sensor.ObservePacketType.InputRep AndAlso Me.InputDevice = Sensor.DeviceType.BDL16x) Then
                DynAttrib.Hide(objaIrBDL16Adr)
                DynAttrib.Hide(objaIrBDL16Port)
            End If

            If Not (Me.ObservePacket = Sensor.ObservePacketType.InputRep AndAlso Me.InputDevice = Sensor.DeviceType.DS54) Then
                DynAttrib.Hide(objaIrDS54Adr)
                DynAttrib.Hide(objaIrDS54Pair)
                DynAttrib.Hide(objaIrDS54Type)
            End If

            If Not (Me.ObservePacket = Sensor.ObservePacketType.SwitchRep AndAlso Me.InputDevice = Sensor.DeviceType.DS54) Then
                DynAttrib.Hide(objaSrDS54Adr)
                DynAttrib.Hide(objaSrDS54Pair)
                DynAttrib.Hide(objaSrDS54Type)
            End If

            If Not (Me.ObservePacket = Sensor.ObservePacketType.MultiSense AndAlso Me.InputDevice = Sensor.DeviceType.BDL16x) Then
                DynAttrib.Hide(objaMsBDL16Adr)
                DynAttrib.Hide(objaMsBDL16Zone)
            End If

            If CtcService.IsStarted Then
                DynAttrib.Disable(objaObservePk)
                DynAttrib.Disable(objaAddress)
                DynAttrib.Disable(objaIrBDL16Adr)
                DynAttrib.Disable(objaIrBDL16Port)
                DynAttrib.Disable(objaIrDS54Adr)
                DynAttrib.Disable(objaIrDS54Pair)
                DynAttrib.Disable(objaIrDS54Type)
                DynAttrib.Disable(objaSrDS54Adr)
                DynAttrib.Disable(objaSrDS54Pair)
                DynAttrib.Disable(objaSrDS54Type)
                DynAttrib.Disable(objaMsBDL16Adr)
                DynAttrib.Disable(objaMsBDL16Zone)
            End If

            objList.Add("ObservePacket", New DynamicDescriptor("Observe", objaObservePk))
            objList.Add("Address", New DynamicDescriptor("Device Adr", objaAddress))
            objList.Add("InputDevice", New DynamicDescriptor("Device Type", objaDevice))

            objList.Add("IrBDL16Adr", New DynamicDescriptor("BDL16x Adr", objaIrBDL16Adr))
            objList.Add("IrBDL16Port", New DynamicDescriptor("BDL16x Port", objaIrBDL16Port))

            objList.Add("IrDS54Adr", New DynamicDescriptor("DS54 Adr", objaIrDS54Adr))
            objList.Add("IrDS54Pair", New DynamicDescriptor("DS54 Inp Pair", objaIrDS54Pair))
            objList.Add("IrDS54Type", New DynamicDescriptor("DS54 Inp Type", objaIrDS54Type))

            objList.Add("SrDS54Adr", New DynamicDescriptor("DS54 Adr", objaSrDS54Adr))
            objList.Add("SrDS54Pair", New DynamicDescriptor("DS54 Inp Pair", objaSrDS54Pair))
            objList.Add("SrDS54Type", New DynamicDescriptor("DS54 Inp Type", objaSrDS54Type))

            objList.Add("MsBDL16Adr", New DynamicDescriptor("BDL16x Adr", objaMsBDL16Adr))
            objList.Add("MsBDL16Zone", New DynamicDescriptor("BDL16x Zone", objaMsBDL16Zone))

            objList.Add("State", New DynamicDescriptor(objaState))
            Return objList
        End Function

    End Class

    Public NotInheritable Class PgSignal
        Inherits PgCtcObjectBase(Of Signal)

        Public Sub New(objSignal As Signal)
            _objWrappedObject = objSignal
        End Sub

        Public Property Location() As Location
            Get
                Return _objWrappedObject.Location
            End Get
            Set(Value As Location)
                _objWrappedObject.Location = Value
            End Set
        End Property

        Public Property Type() As String
            Get
                Dim strLookKey As String = _objWrappedObject.LookKey
                If strLookKey = Nothing Then
                    Return "Undefined"
                Else
                    Dim strLookName As String = GetSignalLook(strLookKey).Name
                    If strLookName = Nothing Then
                        Return "Unknown"
                    Else
                        Return strLookName
                    End If
                End If
            End Get
            Set(Value As String)
                Static objLookup As Dictionary(Of String, String)   'populate once; use multiple times
                If objLookup Is Nothing Then
                    objLookup = New Dictionary(Of String, String)
                    For Each enuLookKey As SignalLookKey In [Enum].GetValues(GetType(SignalLookKey))
                        objLookup.Add(GetSignalLook(enuLookKey).Name, enuLookKey.ToString)
                    Next
                End If
                Dim strLookKey As String = objLookup(Value)
                With _objWrappedObject
                    .Config = GetSignalLook(strLookKey).Config
                    .LookKey = strLookKey
                End With
            End Set
        End Property

        Public Property DrawScale() As Single
            Get
                Return _objWrappedObject.DrawScale
            End Get
            Set(Value As Single)
                _objWrappedObject.DrawScale = Value
            End Set
        End Property

        Public Property CellAlign() As Alignment
            Get
                Return _objWrappedObject.CellAlign
            End Get
            Set(Value As Alignment)
                _objWrappedObject.CellAlign = Value
                _objWrappedObject.AlignOffsetX = 0
                _objWrappedObject.AlignOffsetY = 0
            End Set
        End Property

        Public Property AlignOffsetX() As Single
            Get
                Return _objWrappedObject.AlignOffsetX
            End Get
            Set(Value As Single)
                _objWrappedObject.AlignOffsetX = Math.Max(Math.Min(Value, SbConst.CellSize.Width / 2), -SbConst.CellSize.Width / 2)
            End Set
        End Property

        Public Property AlignOffsetY() As Single
            Get
                Return _objWrappedObject.AlignOffsetY
            End Get
            Set(Value As Single)
                _objWrappedObject.AlignOffsetY = Math.Max(Math.Min(Value, SbConst.CellSize.Height / 2), -SbConst.CellSize.Height / 2)
            End Set
        End Property

        <Browsable(False)> Public ReadOnly Property States() As PkStatesList
            Get
                Return _objWrappedObject.Aspects
            End Get
        End Property

        Public Property SwitchTime() As UShort
            Get
                Return _objWrappedObject.Aspects.SwitchTime
            End Get
            Set(Value As UShort)
                _objWrappedObject.Aspects.SwitchTime = Value
            End Set
        End Property

        Public Property DefaultAspect() As Signal.SignalAspect
            Get
                Return _objWrappedObject.Aspects.Default.Value
            End Get
            Set(Value As Signal.SignalAspect)
                _objWrappedObject.Aspects.Default = _objWrappedObject.Aspects(Value)
            End Set
        End Property

        Public ReadOnly Property ActiveAspect() As Signal.SignalAspect
            Get
                Return _objWrappedObject.Aspect.Value
            End Get
        End Property

        Public ReadOnly Property Disposition() As PkStatesList.StateDisposition
            Get
                Return _objWrappedObject.Aspects.Disposition
            End Get
        End Property

        Public Overrides Function PropGridSort() As String()
            Return New String() {"Name", "Location", "Type", "DrawScale", "CellAlign", "AlignOffsetX", "AlignOffsetY", "SwitchTime", "DefaultAspect", "ActiveAspect", "Disposition"}
        End Function

        Public Overrides Function PropGridDynamicAttrib() As ListDictionary
            Dim objList As ListDictionary = MyBase.PropGridDynamicAttrib()

            Dim objaLoc As Attribute() = {New DescriptionAttribute("Location of the signal element on the switchboard grid."), New TypeConverterAttribute(GetType(LocationConverter))}
            Dim objaType As Attribute() = {New DescriptionAttribute("Signal configuration representing the allowed aspect states and look."), New TypeConverterAttribute(GetType(SignalLookConverter))}
            Dim objaScale As Attribute() = {New DescriptionAttribute("The signal's drawing surface scale. Valid values are decimals 0.1-10. Example: 0.5 is half default size; 2 is double default size.")}
            Dim objaAlign As Attribute() = {New DescriptionAttribute("The signal's drawing surface alignment in relation to its switchboard cell location.")}
            Dim objaOffsetX As Attribute() = {New DescriptionAttribute("The horizontal decimal offset of the signal's drawing surface from the position set by 'Cell Align'.")}
            Dim objaOffsetY As Attribute() = {New DescriptionAttribute("The vertical decimal offset of the signal's drawing surface from the position set by 'Cell Align'.")}
            Dim objaSwTime As Attribute() = {New DescriptionAttribute("Time in millisecons that it takes for this signal to switch from one aspect to another.")}
            Dim objaDefAsp As Attribute() = {New DescriptionAttribute("Aspect state assigned to the signal when the railroad is reset."), New TypeConverterAttribute(GetType(AspectConverter))}
            Dim objaActAsp As Attribute() = {New DescriptionAttribute("Currently active signal aspect."), New TypeConverterAttribute(GetType(AspectConverter))}
            Dim objaDisp As Attribute() = {New DescriptionAttribute("The three disposition values are: Set=(aspect is set), Pending=(aspect is being switched), Locked=(aspect is set and locked).")}

            If CtcService.IsStarted Then
                DynAttrib.Disable(objaLoc)
                DynAttrib.Disable(objaType)
                DynAttrib.Disable(objaScale)
                DynAttrib.Disable(objaAlign)
                DynAttrib.Disable(objaOffsetX)
                DynAttrib.Disable(objaOffsetY)
                DynAttrib.Disable(objaSwTime)
                DynAttrib.Disable(objaDefAsp)
            End If

            objList.Add("Location", New DynamicDescriptor(objaLoc))
            objList.Add("Type", New DynamicDescriptor(objaType))
            objList.Add("DrawScale", New DynamicDescriptor("Draw Scale", objaScale))
            objList.Add("CellAlign", New DynamicDescriptor("Cell Align.", objaAlign))
            objList.Add("AlignOffsetX", New DynamicDescriptor("Horz. Offset", objaOffsetX))
            objList.Add("AlignOffsetY", New DynamicDescriptor("Vert. Offset", objaOffsetY))
            objList.Add("SwitchTime", New DynamicDescriptor("Switch Time", objaSwTime))
            objList.Add("DefaultAspect", New DynamicDescriptor("Default Aspect", objaDefAsp))
            objList.Add("ActiveAspect", New DynamicDescriptor("Active Aspect", objaActAsp))
            objList.Add("Disposition", New DynamicDescriptor(objaDisp))
            Return objList
        End Function

    End Class

    Public NotInheritable Class PgAccessory
        Inherits PgCtcObjectBase(Of Accessory)

        Public Sub New(objAccessory As Accessory)
            _objWrappedObject = objAccessory
        End Sub

        <Browsable(False)> Public ReadOnly Property States() As PkStatesList
            Get
                Return _objWrappedObject.States
            End Get
        End Property

        Public Property SwitchTime() As UShort
            Get
                Return _objWrappedObject.States.SwitchTime
            End Get
            Set(Value As UShort)
                _objWrappedObject.States.SwitchTime = Value
            End Set
        End Property


        Public Property DefaultState() As String
            Get
                Return GetStateName(_objWrappedObject.States.Default)
            End Get
            Set(Value As String)
                Dim objState As PkStatesList.State = _objWrappedObject.States(Value)
                If objState IsNot Nothing Then
                    _objWrappedObject.States.Default = objState
                End If
            End Set
        End Property

        Public ReadOnly Property ActiveState() As String
            Get
                Return GetStateName(_objWrappedObject.State)
            End Get
        End Property

        Private Function GetStateName(objState As PkStatesList.State) As String
            Return If(objState Is Nothing, "n/a", objState.Name)
        End Function


        Public ReadOnly Property Disposition() As PkStatesList.StateDisposition
            Get
                Return _objWrappedObject.States.Disposition
            End Get
        End Property

        Public Overrides Function PropGridSort() As String()
            Return New String() {"Name", "SwitchTime", "DefaultState", "ActiveState", "Disposition"}
        End Function

        Public Overrides Function PropGridDynamicAttrib() As ListDictionary
            Dim objList As ListDictionary = MyBase.PropGridDynamicAttrib()

            Dim objaSwTime As Attribute() = {New DescriptionAttribute("Time in millisecons that it takes for this accessory to switch from one state to another.")}
            Dim objaDefSta As Attribute() = {New DescriptionAttribute("State assigned to the accessory when the railroad is reset."), New TypeConverterAttribute(GetType(AccessoryStateConverter))}
            Dim objaActSta As Attribute() = {New DescriptionAttribute("Currently active accessory state."), New TypeConverterAttribute(GetType(AccessoryStateConverter))}
            Dim objaDisp As Attribute() = {New DescriptionAttribute("The three disposition values are: Set=(state is set), Pending=(state is being set), Locked=(state is set and locked).")}

            If CtcService.IsStarted Then
                DynAttrib.Disable(objaSwTime)
                DynAttrib.Disable(objaDefSta)
            End If

            objList.Add("SwitchTime", New DynamicDescriptor("Switch Time", objaSwTime))
            objList.Add("DefaultState", New DynamicDescriptor("Default State", objaDefSta))
            objList.Add("ActiveState", New DynamicDescriptor("Active State", objaActSta))
            objList.Add("Disposition", New DynamicDescriptor(objaDisp))
            Return objList
        End Function

    End Class

    Public NotInheritable Class PgLabel
        Inherits PgCtcObjectBase(Of Label)

        Public Sub New(objLabel As Label)
            _objWrappedObject = objLabel
        End Sub

        Public Property Location() As Location
            Get
                Return _objWrappedObject.Location
            End Get
            Set(Value As Location)
                _objWrappedObject.Location = Value
            End Set
        End Property

        Public Property FontFamily() As String
            Get
                Return _objWrappedObject.FontFamily
            End Get
            Set(Value As String)
                _objWrappedObject.FontFamily = Value
            End Set
        End Property

        Public Property TextPadding() As Single
            Get
                Return _objWrappedObject.TextPadding
            End Get
            Set(Value As Single)
                _objWrappedObject.TextPadding = Value
            End Set
        End Property

        Public Property TextColor() As System.Drawing.Color
            Get
                Return _objWrappedObject.TextColor.ToColor.Convert
            End Get
            Set(Value As System.Drawing.Color)
                _objWrappedObject.TextColor = Value.Convert(255).ToString
            End Set
        End Property

        Public Property BackColor() As System.Drawing.Color
            Get
                Return _objWrappedObject.BackColor.ToColor.Convert
            End Get
            Set(Value As System.Drawing.Color)
                _objWrappedObject.BackColor = Value.Convert(Me.BackOpacity).ToString
            End Set
        End Property

        Public Property BackOpacity() As Byte
            Get
                Return _objWrappedObject.BackColor.ToColor.A
            End Get
            Set(Value As Byte)
                _objWrappedObject.BackColor = _objWrappedObject.BackColor.ToColor.SetAlpha(Value).ToString
            End Set
        End Property

        Public Property BackCornerRadius() As Single
            Get
                Return _objWrappedObject.BackCornerRadius
            End Get
            Set(Value As Single)
                _objWrappedObject.BackCornerRadius = Value
            End Set
        End Property

        Public Property DrawScale() As Single
            Get
                Return _objWrappedObject.DrawScale
            End Get
            Set(Value As Single)
                _objWrappedObject.DrawScale = Value
            End Set
        End Property

        Public Property CellAlign() As Alignment
            Get
                Return _objWrappedObject.CellAlign
            End Get
            Set(Value As Alignment)
                _objWrappedObject.CellAlign = Value
                _objWrappedObject.AlignOffsetX = 0
                _objWrappedObject.AlignOffsetY = 0
            End Set
        End Property

        Public Property AlignOffsetX() As Single
            Get
                Return _objWrappedObject.AlignOffsetX
            End Get
            Set(Value As Single)
                _objWrappedObject.AlignOffsetX = Math.Max(Math.Min(Value, SbConst.CellSize.Width / 2), -SbConst.CellSize.Width / 2)
            End Set
        End Property

        Public Property AlignOffsetY() As Single
            Get
                Return _objWrappedObject.AlignOffsetY
            End Get
            Set(Value As Single)
                _objWrappedObject.AlignOffsetY = Math.Max(Math.Min(Value, SbConst.CellSize.Height / 2), -SbConst.CellSize.Height / 2)
            End Set
        End Property

        Public Property StaticText() As String
            Get
                If _objWrappedObject.StaticText Is Nothing Then
                    Return Nothing
                Else
                    Return _objWrappedObject.StaticText.Replace(vbCrLf, "#NL#")
                End If
            End Get
            Set(Value As String)
                If Value Is Nothing Then
                    _objWrappedObject.StaticText = Nothing
                Else
                    _objWrappedObject.StaticText = Value.Replace("#NL#", vbCrLf)
                End If
            End Set
        End Property

        Public ReadOnly Property DynamicText() As String
            Get
                Return _objWrappedObject.DynamicText
            End Get
        End Property

        Public Overrides Function PropGridSort() As String()
            Return New String() {"Name", "Location", "FontFamily", "TextPadding", "TextColor", "BackColor", "BackOpacity", "BackCornerRadius", "DrawScale", "CellAlign", "AlignOffsetX", "AlignOffsetY", "StaticText", "DynamicText"}
        End Function

        Public Overrides Function PropGridDynamicAttrib() As ListDictionary
            Dim objList As ListDictionary = MyBase.PropGridDynamicAttrib()

            Dim objaLoc As Attribute() = {New DescriptionAttribute("Location of the label element on the switchboard grid."), New TypeConverterAttribute(GetType(LocationConverter))}
            Dim objaFontFam As Attribute() = {New DescriptionAttribute("The font name of the label's text.")}
            Dim objaTextPad As Attribute() = {New DescriptionAttribute("The padding size around the label's text. Valid values are decimals 0-200.")}
            Dim objaTextColor As Attribute() = {New DescriptionAttribute("The color of the label's text.")}
            Dim objaBackColor As Attribute() = {New DescriptionAttribute("The color of the label's text background.")}
            Dim objaBackOpac As Attribute() = {New DescriptionAttribute("The level of background opacity. The range is 0-255, where 0 is fully transparent and 255 is fully opaque.")}
            Dim objaBackCorRad As Attribute() = {New DescriptionAttribute("The text's background corner radius size. Valid values are decimals 0-800. 0 for no rounded corners.")}
            Dim objaScale As Attribute() = {New DescriptionAttribute("The label's drawing surface scale. Valid values are decimals 0.1-10. Example: 0.5 is half default size; 2 is double default size.")}
            Dim objaAlign As Attribute() = {New DescriptionAttribute("The label's drawing surface alignment in relation to its switchboard cell location.")}
            Dim objaOffsetX As Attribute() = {New DescriptionAttribute("The horizontal decimal offset of the label's drawing surface from the position set by 'Cell Align'.")}
            Dim objaOffsetY As Attribute() = {New DescriptionAttribute("The vertical decimal offset of the label's drawing surface from the position set by 'Cell Align'.")}
            Dim objaStatText As Attribute() = {New DescriptionAttribute("The label's static text is shown only if the dynamic text is blank. Use '#NL#' to indicate a new line.")}
            Dim objaDynText As Attribute() = {New DescriptionAttribute("The label's dynamic text overrides the static text and can be scripted.")}

            If CtcService.IsStarted Then
                DynAttrib.Disable(objaLoc)
                DynAttrib.Disable(objaFontFam)
                DynAttrib.Disable(objaTextPad)
                DynAttrib.Disable(objaTextColor)
                DynAttrib.Disable(objaBackColor)
                DynAttrib.Disable(objaBackOpac)
                DynAttrib.Disable(objaBackCorRad)
                DynAttrib.Disable(objaScale)
                DynAttrib.Disable(objaAlign)
                DynAttrib.Disable(objaOffsetX)
                DynAttrib.Disable(objaOffsetY)
                DynAttrib.Disable(objaStatText)
            End If

            objList.Add("Location", New DynamicDescriptor(objaLoc))
            objList.Add("FontFamily", New DynamicDescriptor("Font Family", objaFontFam))
            objList.Add("TextPadding", New DynamicDescriptor("Text Padding", objaTextPad))
            objList.Add("TextColor", New DynamicDescriptor("Text Color", objaTextColor))
            objList.Add("BackColor", New DynamicDescriptor("Back Color", objaBackColor))
            objList.Add("BackOpacity", New DynamicDescriptor("Back Opacity", objaBackOpac))
            objList.Add("BackCornerRadius", New DynamicDescriptor("Back Cor. Rad.", objaBackCorRad))
            objList.Add("DrawScale", New DynamicDescriptor("Draw Scale", objaScale))
            objList.Add("CellAlign", New DynamicDescriptor("Cell Align.", objaAlign))
            objList.Add("AlignOffsetX", New DynamicDescriptor("Horz. Offset", objaOffsetX))
            objList.Add("AlignOffsetY", New DynamicDescriptor("Vert. Offset", objaOffsetY))
            objList.Add("StaticText", New DynamicDescriptor("Static Text", objaStatText))
            objList.Add("DynamicText", New DynamicDescriptor("Dynamic Text", objaDynText))
            Return objList
        End Function

    End Class

    Public NotInheritable Class PgButton
        Inherits PgCtcObjectBase(Of Button)

        Public Sub New(objButton As Button)
            _objWrappedObject = objButton
        End Sub

        Public Property Location() As Location
            Get
                Return _objWrappedObject.Location
            End Get
            Set(Value As Location)
                _objWrappedObject.Location = Value
            End Set
        End Property

        Public Property Behavior() As Button.ButtonBehavior
            Get
                Return _objWrappedObject.Behavior
            End Get
            Set(Value As Button.ButtonBehavior)
                _objWrappedObject.Behavior = Value
            End Set
        End Property

        Public Property Look() As Button.ButtonLook
            Get
                Return _objWrappedObject.Look
            End Get
            Set(Value As Button.ButtonLook)
                _objWrappedObject.Look = Value
            End Set
        End Property

        Public Property DrawScale() As Single
            Get
                Return _objWrappedObject.DrawScale
            End Get
            Set(Value As Single)
                _objWrappedObject.DrawScale = Value
            End Set
        End Property

        Public Property CellAlign() As Alignment
            Get
                Return _objWrappedObject.CellAlign
            End Get
            Set(Value As Alignment)
                _objWrappedObject.CellAlign = Value
                _objWrappedObject.AlignOffsetX = 0
                _objWrappedObject.AlignOffsetY = 0
            End Set
        End Property

        Public Property AlignOffsetX() As Single
            Get
                Return _objWrappedObject.AlignOffsetX
            End Get
            Set(Value As Single)
                _objWrappedObject.AlignOffsetX = Math.Max(Math.Min(Value, SbConst.CellSize.Width / 2), -SbConst.CellSize.Width / 2)
            End Set
        End Property

        Public Property AlignOffsetY() As Single
            Get
                Return _objWrappedObject.AlignOffsetY
            End Get
            Set(Value As Single)
                _objWrappedObject.AlignOffsetY = Math.Max(Math.Min(Value, SbConst.CellSize.Height / 2), -SbConst.CellSize.Height / 2)
            End Set
        End Property

        Public ReadOnly Property State() As OnOff
            Get
                Return _objWrappedObject.State
            End Get
        End Property

        Public Overrides Function PropGridSort() As String()
            Return New String() {"Name", "Location", "Behavior", "Look", "DrawScale", "CellAlign", "AlignOffsetX", "AlignOffsetY", "State"}
        End Function

        Public Overrides Function PropGridDynamicAttrib() As ListDictionary
            Dim objList As ListDictionary = MyBase.PropGridDynamicAttrib()

            Dim objaLoc As Attribute() = {New DescriptionAttribute("Location of the button element on the switchboard grid."), New TypeConverterAttribute(GetType(LocationConverter))}
            Dim objaBehav As Attribute() = {New DescriptionAttribute("The state behavior of this button element.")}
            Dim objaLook As Attribute() = {New DescriptionAttribute("The switchboard look of this button element.")}
            Dim objaScale As Attribute() = {New DescriptionAttribute("The button's drawing surface scale. Valid values are decimals 0.1-10. Example: 0.5 is half default size; 2 is double default size.")}
            Dim objaAlign As Attribute() = {New DescriptionAttribute("The button's drawing surface alignment in relation to its switchboard cell location.")}
            Dim objaOffsetX As Attribute() = {New DescriptionAttribute("The horizontal decimal offset of the button's drawing surface from the position set by 'Cell Align'.")}
            Dim objaOffsetY As Attribute() = {New DescriptionAttribute("The vertical decimal offset of the button's drawing surface from the position set by 'Cell Align'.")}
            Dim objaState As Attribute() = {New DescriptionAttribute("The current state of the button.")}
            
            If CtcService.IsStarted Then
                DynAttrib.Disable(objaLoc)
                DynAttrib.Disable(objaBehav)
                DynAttrib.Disable(objaLook)
                DynAttrib.Disable(objaScale)
                DynAttrib.Disable(objaAlign)
                DynAttrib.Disable(objaOffsetX)
                DynAttrib.Disable(objaOffsetY)
            End If

            objList.Add("Location", New DynamicDescriptor(objaLoc))
            objList.Add("Behavior", New DynamicDescriptor(objaBehav))
            objList.Add("Look", New DynamicDescriptor(objaLook))
            objList.Add("DrawScale", New DynamicDescriptor("Draw Scale", objaScale))
            objList.Add("CellAlign", New DynamicDescriptor("Cell Align.", objaAlign))
            objList.Add("AlignOffsetX", New DynamicDescriptor("Horz. Offset", objaOffsetX))
            objList.Add("AlignOffsetY", New DynamicDescriptor("Vert. Offset", objaOffsetY))
            objList.Add("State", New DynamicDescriptor(objaState))
            Return objList
        End Function

    End Class

    Public NotInheritable Class PgEngine
        Inherits PgCtcObjectBase(Of Engine)

        Public Sub New(objEngine As Engine)
            _objWrappedObject = objEngine
        End Sub

        Public Property Length() As UShort
            Get
                Return _objWrappedObject.Length
            End Get
            Set(Value As UShort)
                _objWrappedObject.Length = Value
            End Set
        End Property

        Public Property ReqCatenary() As Boolean
            Get
                Return _objWrappedObject.ReqCatenary
            End Get
            Set(Value As Boolean)
                _objWrappedObject.ReqCatenary = Value
            End Set
        End Property

        Public Property AutoBindInOpr() As Boolean
            Get
                Return _objWrappedObject.AutoBindInOpr
            End Get
            Set(Value As Boolean)
                _objWrappedObject.AutoBindInOpr = Value
            End Set
        End Property

        Public Property Address() As UShort
            Get
                Return _objWrappedObject.Address
            End Get
            Set(Value As UShort)
                _objWrappedObject.Address = Value
            End Set
        End Property

        Public Property SpeedSteps() As SpeedSteps
            Get
                Return _objWrappedObject.SpeedSteps
            End Get
            Set(Value As SpeedSteps)
                _objWrappedObject.SpeedSteps = Value
            End Set
        End Property

        Public ReadOnly Property Slot() As String
            Get
                Select Case True
                    Case Not _objWrappedObject.IsBound
                        Return "Unbound"
                    Case _objWrappedObject.Slot > 0
                        Return _objWrappedObject.Slot
                    Case Else
                        Return "Simulated"
                End Select
            End Get
        End Property

        Public ReadOnly Property Speed() As Byte
            Get
                Return _objWrappedObject.Speed
            End Get
        End Property

        Public ReadOnly Property Direction() As LocoDirection
            Get
                Return _objWrappedObject.Direction
            End Get
        End Property

        Public ReadOnly Property Functions() As String
            Get
                With _objWrappedObject
                    Return String.Format("{0}, {1}, {2}, {3}, {4}, {5}, {6}, {7}, {8}",
                        .Functions(0), .Functions(1), .Functions(2), .Functions(3), .Functions(4), .Functions(5), .Functions(6), .Functions(7), .Functions(8))
                End With
            End Get
        End Property

        Public Overrides Function PropGridSort() As String()
            Return New String() {"Name", "Length", "ReqCatenary", "AutoBindInOpr", "Address", "SpeedSteps", "Slot", "Speed", "Direction", "Functions"}
        End Function

        Public Overrides Function PropGridDynamicAttrib() As ListDictionary
            Dim objList As ListDictionary = MyBase.PropGridDynamicAttrib()

            Dim objaLen As Attribute() = {New DescriptionAttribute("Length of engine over buffers in millimeters. (Not currently used)")}
            Dim objaCat As Attribute() = {New DescriptionAttribute("Engine is electric and requires overhead catenary. (Not currently used)")}
            Dim objaBind As Attribute() = {New DescriptionAttribute("Auto bind the engine to the command station when in operation mode.")}
            Dim objaAddr As Attribute() = {New DescriptionAttribute("DCC address assigned to the engine's decoder. Valid values are 0-16383.")}
            Dim objaStep As Attribute() = {New DescriptionAttribute("Speed step type to be configured when binding to a slot.")}
            Dim objaSlot As Attribute() = {New DescriptionAttribute("Command station's slot number that this engine is bound to.")}
            Dim objaSpeed As Attribute() = {New DescriptionAttribute("The speed of the engine.")}
            Dim objaDir As Attribute() = {New DescriptionAttribute("The engine's direction of travel.")}
            Dim objaFunc As Attribute() = {New DescriptionAttribute("Function states 0 through 8.")}

            If CtcService.IsStarted Then
                DynAttrib.Disable(objaLen)
                DynAttrib.Disable(objaCat)
            End If

            If _objWrappedObject.IsBound Then
                DynAttrib.Disable(objaAddr)
                DynAttrib.Disable(objaStep)
            End If

            objList.Add("Length", New DynamicDescriptor(objaLen))
            objList.Add("ReqCatenary", New DynamicDescriptor("Req Catenary", objaCat))
            objList.Add("AutoBindInOpr", New DynamicDescriptor("Opr Auto Bind", objaBind))
            objList.Add("Address", New DynamicDescriptor(objaAddr))
            objList.Add("SpeedSteps", New DynamicDescriptor("Speed Steps", objaStep))
            objList.Add("Slot", New DynamicDescriptor(objaSlot))
            objList.Add("Speed", New DynamicDescriptor(objaSpeed))
            objList.Add("Direction", New DynamicDescriptor(objaDir))
            objList.Add("Functions", New DynamicDescriptor(objaFunc))
            Return objList
        End Function

    End Class

    Public NotInheritable Class PgSequence
        Inherits PgCtcObjectBase(Of Sequence)

        Public Sub New(objSequence As Sequence)
            _objWrappedObject = objSequence
        End Sub

        Public ReadOnly Property Status() As Sequence.SeqStatus
            Get
                Return _objWrappedObject.Status
            End Get
        End Property

        Public Overrides Function PropGridSort() As String()
            Return New String() {"Name", "Status"}
        End Function

        Public Overrides Function PropGridDynamicAttrib() As ListDictionary
            Dim objList As ListDictionary = MyBase.PropGridDynamicAttrib()

            Dim objaStatus As Attribute() = {New DescriptionAttribute("Operational status: Idle, Recording, or Playing.")}
            objList.Add("Status", New DynamicDescriptor(objaStatus))
            Return objList
        End Function

    End Class

    Public NotInheritable Class PgEventScript
        Inherits PgCtcObjectBase(Of EventScript)

        Public Sub New(objScript As EventScript)
            _objWrappedObject = objScript
        End Sub

        Public Property Enabled() As Boolean
            Get
                Return _objWrappedObject.Enabled
            End Get
            Set(Value As Boolean)
                _objWrappedObject.Enabled = Value
            End Set
        End Property

        Public Overrides Function PropGridSort() As String()
            Return New String() {"Name", "Enabled"}
        End Function

        Public Overrides Function PropGridDynamicAttrib() As ListDictionary
            Dim objList As ListDictionary = MyBase.PropGridDynamicAttrib()

            Dim objaEnabled As Attribute() = {New DescriptionAttribute("If False, the script will be excluded from operation.")}

            If CtcService.IsStarted Then
                DynAttrib.Disable(objaEnabled)
            End If

            objList.Add("Enabled", New DynamicDescriptor(objaEnabled))

            Return objList
        End Function

    End Class

    Public NotInheritable Class PgStepScript
        Inherits PgCtcObjectBase(Of StepScript)

        Public Sub New(objStepScript As StepScript)
            _objWrappedObject = objStepScript
        End Sub

        Public Property Enabled() As Boolean
            Get
                Return _objWrappedObject.Enabled
            End Get
            Set(Value As Boolean)
                _objWrappedObject.Enabled = Value
            End Set
        End Property

        Public Property [Loop]() As Boolean
            Get
                Return _objWrappedObject.Loop
            End Get
            Set(Value As Boolean)
                _objWrappedObject.Loop = Value
            End Set
        End Property

        Public ReadOnly Property Running() As Boolean
            Get
                Return _objWrappedObject.Running
            End Get
        End Property

        Public ReadOnly Property StepPos() As String
            Get
                Return _objWrappedObject.StepPosName
            End Get
        End Property

        Public Overrides Function PropGridSort() As String()
            Return New String() {"Name", "Enabled", "Loop", "Running", "StepPos"}
        End Function

        Public Overrides Function PropGridDynamicAttrib() As ListDictionary
            Dim objList As ListDictionary = MyBase.PropGridDynamicAttrib()

            Dim objaEnabled As Attribute() = {New DescriptionAttribute("If False, the script will be excluded from operation.")}
            Dim objaLoop As Attribute() = {New DescriptionAttribute("If True, script will loop back to the start when it reaches the end.")}
            Dim objaRunning As Attribute() = {New DescriptionAttribute("If True, the step script is currently running.")}
            Dim objaStepPos As Attribute() = {New DescriptionAttribute("The step name of the position at which execution is or will occur next.")}

            If CtcService.IsStarted Then
                DynAttrib.Disable(objaEnabled)
            End If

            objList.Add("Enabled", New DynamicDescriptor(objaEnabled))
            objList.Add("Loop", New DynamicDescriptor(objaLoop))
            objList.Add("Running", New DynamicDescriptor(objaRunning))
            objList.Add("StepPos", New DynamicDescriptor("Step Position", objaStepPos))

            Return objList
        End Function

    End Class

    Public NotInheritable Class PgGlobalScript
        Inherits PgCtcObjectBase(Of GlobalScript)

        Public Sub New(objScript As GlobalScript)
            _objWrappedObject = objScript
        End Sub

        Public Property Enabled() As Boolean
            Get
                Return _objWrappedObject.Enabled
            End Get
            Set(Value As Boolean)
                _objWrappedObject.Enabled = Value
            End Set
        End Property

        Public Overrides Function PropGridSort() As String()
            Return New String() {"Name", "Enabled"}
        End Function

        Public Overrides Function PropGridDynamicAttrib() As ListDictionary
            Dim objList As ListDictionary = MyBase.PropGridDynamicAttrib()

            Dim objaEnabled As Attribute() = {New DescriptionAttribute("If False, the script will be excluded from operation.")}

            If CtcService.IsStarted Then
                DynAttrib.Disable(objaEnabled)
            End If

            objList.Add("Enabled", New DynamicDescriptor(objaEnabled))

            Return objList
        End Function

    End Class

    <TypeConverter(GetType(DynamicPropGridConverter))> Public NotInheritable Class PgState
        Implements IDynamicPropertyGrid, IWrapperObject
        Protected _objWrappedObject As PkStatesList.State

        <Browsable(False)> Public ReadOnly Property WrappedObject() As Object Implements IWrapperObject.WrappedObject
            Get
                Return _objWrappedObject
            End Get
        End Property

        Public Sub New(objState As PkStatesList.State)
            _objWrappedObject = objState
        End Sub

        Public Property Name() As String
            Get
                Return _objWrappedObject.Name
            End Get
            Set(strValue As String)
                _objWrappedObject.Name = strValue
            End Set
        End Property

        Protected Function ShouldSerializeName() As Boolean
            '"ShouldSerialize" & ProperyName - makes default names not be bold; similar to DefaultValueAttribute but more customizable
            Return _objWrappedObject.Name <> _objWrappedObject.DefaultName
        End Function

        Public Function PropGridSort() As String() Implements IDynamicPropertyGrid.PropGridSort
            Return New String() {"Name"}
        End Function

        Public Function PropGridDynamicAttrib() As ListDictionary Implements IDynamicPropertyGrid.PropGridDynamicAttrib
            Dim objList As New ListDictionary
            Dim objaName As Attribute() = {New DescriptionAttribute("User defined identifier for this state.")}

            If CtcService.IsStarted Then
                DynAttrib.Disable(objaName)
            End If

            objList.Add("Name", New DynamicDescriptor(objaName))
            Return objList
        End Function

    End Class

#End Region

#Region "Wrappers for Packet Objects"

    <TypeConverter(GetType(DynamicPropGridConverter))> Public MustInherit Class PgPacket(Of T As Packet)
        Implements IDynamicPropertyGrid, IWrapperObject
        Protected _objWrappedObject As T

        <Browsable(False)> Public ReadOnly Property WrappedObject() As Object Implements IWrapperObject.WrappedObject
            Get
                Return _objWrappedObject
            End Get
        End Property

        Public ReadOnly Property OpCode() As OpCodes
            Get
                Return _objWrappedObject.OpCode
            End Get
        End Property

        Public Overridable Function PropGridSort() As String() Implements IDynamicPropertyGrid.PropGridSort
        End Function

        Public Overridable Function PropGridDynamicAttrib() As ListDictionary Implements IDynamicPropertyGrid.PropGridDynamicAttrib
            Dim objList As New ListDictionary
            Dim objaOpCode As Attribute() = {New DescriptionAttribute("Packet type name as defined by the Loconet specification.")}

            objList.Add("OpCode", New DynamicDescriptor("Op Code", objaOpCode))
            Return objList
        End Function

    End Class

    Public NotInheritable Class PgPkSetSwitch
        Inherits PgPacket(Of PkSetSwitch)

        Public Sub New(objPacket As PkSetSwitch)
            _objWrappedObject = objPacket
        End Sub

        Public Property Switch() As UShort
            Get
                Return _objWrappedObject.Switch
            End Get
            Set(Value As UShort)
                _objWrappedObject.Switch = Value
            End Set
        End Property

        Public Property State() As SwitchState
            Get
                Return _objWrappedObject.State
            End Get
            Set(Value As SwitchState)
                _objWrappedObject.State = Value
            End Set
        End Property

        Public Property Key() As OnOff
            Get
                Return _objWrappedObject.Key
            End Get
            Set(Value As OnOff)
                _objWrappedObject.Key = Value
            End Set
        End Property

        Public Overrides Function PropGridSort() As String()
            Return New String() {"OpCode", "Switch", "State", "Key"}
        End Function

        Public Overrides Function PropGridDynamicAttrib() As ListDictionary
            Dim objList As ListDictionary = MyBase.PropGridDynamicAttrib()

            Dim objaSwitch As Attribute() = {New DescriptionAttribute("Value 1-2048 identifying the switch number.")}
            Dim objaState As Attribute() = {New DescriptionAttribute("State of switch command being sent.")}
            Dim objaKey As Attribute() = {New DescriptionAttribute("Packet can emulate a throttle key down event 'On' or key up event 'Off'. Most devices respond to ON events.")}

            If CtcService.IsStarted Then
                DynAttrib.Disable(objaSwitch)
                DynAttrib.Disable(objaState)
                DynAttrib.Disable(objaKey)
            End If

            objList.Add("Switch", New DynamicDescriptor(objaSwitch))
            objList.Add("State", New DynamicDescriptor(objaState))
            objList.Add("Key", New DynamicDescriptor(objaKey))
            Return objList
        End Function

    End Class

    Public NotInheritable Class PgPkInput
        Inherits PgPacket(Of PkInput)

        Public Sub New(objPacket As PkInput)
            _objWrappedObject = objPacket
        End Sub

        Public Property Address() As UShort
            Get
                Return _objWrappedObject.Address
            End Get
            Set(Value As UShort)
                _objWrappedObject.Address = Value
            End Set
        End Property

        Public Property BDL16Adr() As UShort
            Get
                Return _objWrappedObject.BDL16Address
            End Get
            Set(Value As UShort)
                _objWrappedObject.BDL16Address = Value
            End Set
        End Property

        Public Property BDL16Port() As Byte
            Get
                Return _objWrappedObject.BDL16Port
            End Get
            Set(Value As Byte)
                _objWrappedObject.BDL16Port = Value
            End Set
        End Property

        Public Property State() As OnOff
            Get
                Return _objWrappedObject.State
            End Get
            Set(Value As OnOff)
                _objWrappedObject.State = Value
            End Set
        End Property

        Public Overrides Function PropGridSort() As String()
            Return New String() {"OpCode", "Address", "BDL16Adr", "BDL16Port", "State"}
        End Function

        Public Overrides Function PropGridDynamicAttrib() As ListDictionary
            Dim objList As ListDictionary = MyBase.PropGridDynamicAttrib()

            Dim objaAddress As Attribute() = {New DescriptionAttribute("Value 0-4095 identifying the device independent input address."), New RefreshPropertiesAttribute(RefreshProperties.Repaint)}
            Dim objaBDL16Adr As Attribute() = {New DescriptionAttribute("Value 1-256 identifying the BDL16 device address."), New RefreshPropertiesAttribute(RefreshProperties.Repaint)}
            Dim objaBDL16Port As Attribute() = {New DescriptionAttribute("Value 1-16 identifying the BDL16 port number."), New RefreshPropertiesAttribute(RefreshProperties.Repaint)}
            Dim objaState As Attribute() = {New DescriptionAttribute("The state of the sensor.")}

            If CtcService.IsStarted Then
                DynAttrib.Disable(objaAddress)
                DynAttrib.Disable(objaBDL16Adr)
                DynAttrib.Disable(objaBDL16Port)
                DynAttrib.Disable(objaState)
            End If

            objList.Add("Address", New DynamicDescriptor("Device Adr", objaAddress))
            objList.Add("BDL16Adr", New DynamicDescriptor("BDL16x Adr", objaBDL16Adr))
            objList.Add("BDL16Port", New DynamicDescriptor("BDL16x Port", objaBDL16Port))
            objList.Add("State", New DynamicDescriptor(objaState))
            Return objList
        End Function

    End Class

    Public NotInheritable Class PgPkImmediate
        Inherits PgPacket(Of PkImmediate)

        Public Sub New(objPacket As PkImmediate)
            _objWrappedObject = objPacket
        End Sub

        Public Property DccAdr() As UShort
            Get
                Return _objWrappedObject.DccAddress
            End Get
            Set(Value As UShort)
                'get values because we have to set them again in the right order
                Dim enuInstruction As PkImmediate.DccInstrType = _objWrappedObject.DccInstruction
                Dim enuaFunctions As OnOff() = _objWrappedObject.DccFunctions

                'set values in the right order
                _objWrappedObject.DccAddress = Value
                _objWrappedObject.DccInstruction = enuInstruction
                _objWrappedObject.DccFunctions = enuaFunctions
            End Set
        End Property

        Public Property Instruction() As PkImmediate.DccInstrType
            Get
                Return _objWrappedObject.DccInstruction
            End Get
            Set(Value As PkImmediate.DccInstrType)
                'get this value because we have to set it again in the right order
                Dim enuaFunctions As OnOff() = _objWrappedObject.DccFunctions

                'set values in the right order
                _objWrappedObject.DccInstruction = Value
                Select Case Value
                    Case PkImmediate.DccInstrType.Func0to4
                        ReDim Preserve enuaFunctions(4)
                    Case PkImmediate.DccInstrType.Func5to8, PkImmediate.DccInstrType.Func9to12
                        ReDim Preserve enuaFunctions(3)
                    Case PkImmediate.DccInstrType.Func13To20, PkImmediate.DccInstrType.Func21To28
                        ReDim Preserve enuaFunctions(7)
                End Select
                _objWrappedObject.DccFunctions = enuaFunctions
            End Set
        End Property

        Public Structure DataPayload
            Public Instruction As PkImmediate.DccInstrType  'used to determine how the DccFunctionsEditor UIEditor should be configured 
            Public FuncStates As OnOff()

            Public Overrides Function ToString() As String
                Return "(Functions)"
            End Function
        End Structure

        Public Property Configuration() As DataPayload
            Get
                Dim sctDataPayload As DataPayload
                sctDataPayload.Instruction = Me.Instruction
                sctDataPayload.FuncStates = _objWrappedObject.DccFunctions
                Return sctDataPayload
            End Get
            Set(Value As DataPayload)
                _objWrappedObject.DccFunctions = Value.FuncStates
            End Set
        End Property

        Public Overrides Function PropGridSort() As String()
            Return New String() {"OpCode", "DccAdr", "Instruction", "Configuration"}
        End Function

        Public Overrides Function PropGridDynamicAttrib() As ListDictionary
            Dim objList As ListDictionary = MyBase.PropGridDynamicAttrib()

            Dim objaDccAdr As Attribute() = {New DescriptionAttribute("DCC device address to which the packet will be sent. Valid values are 1-10239.")}
            Dim objaInstr As Attribute() = {New DescriptionAttribute("Instruction targeted at the DCC decoder."), New TypeConverterAttribute(GetType(DccInstrConverter))}
            Dim objaConfig As Attribute() = {New DescriptionAttribute("Configuration of the function states to be set."), New EditorAttribute(GetType(DccFunctionsEditor), GetType(UITypeEditor))}

            If CtcService.IsStarted Then
                DynAttrib.Disable(objaDccAdr)
                DynAttrib.Disable(objaInstr)
                DynAttrib.Disable(objaConfig)
            End If

            objList.Add("DccAdr", New DynamicDescriptor("DCC Address", objaDccAdr))
            objList.Add("Instruction", New DynamicDescriptor(objaInstr))
            objList.Add("Configuration", New DynamicDescriptor(objaConfig))
            Return objList
        End Function

    End Class

    Public NotInheritable Class PgLocoIO
        Inherits PgPacket(Of PkLocoIO)

        Public Sub New(objPacket As PkLocoIO)
            _objWrappedObject = objPacket
        End Sub

        Public Property Address() As Byte
            Get
                Return _objWrappedObject.Address
            End Get
            Set(Value As Byte)
                _objWrappedObject.Address = Value
            End Set
        End Property

        Public Structure MultiPortConfig
            Public PortMask() As Boolean
            Public PortState() As Boolean

            Public Overrides Function ToString() As String
                Return "(Ports)"
            End Function
        End Structure

        Public Property Configuration() As MultiPortConfig
            Get
                Dim sctMultiPortConfig As MultiPortConfig
                sctMultiPortConfig.PortMask = _objWrappedObject.MultiMask
                sctMultiPortConfig.PortState = _objWrappedObject.MultiState
                Return sctMultiPortConfig
            End Get
            Set(Value As MultiPortConfig)
                _objWrappedObject.MultiMask = Value.PortMask
                _objWrappedObject.MultiState = Value.PortState
            End Set
        End Property

        Public Overrides Function PropGridSort() As String()
            Return New String() {"OpCode", "Address", "Configuration"}
        End Function

        Public Overrides Function PropGridDynamicAttrib() As ListDictionary
            Dim objList As ListDictionary = MyBase.PropGridDynamicAttrib()

            Dim objaAddress As Attribute() = {New DescriptionAttribute("LocoIO device address to which the packet will be sent. Valid values are integers 1-79 and 81-127.")}
            Dim objaConfig As Attribute() = {New DescriptionAttribute("Configuration of the LocoIO port states to be set."), New EditorAttribute(GetType(MultiPortEditor), GetType(UITypeEditor))}

            If CtcService.IsStarted Then
                DynAttrib.Disable(objaAddress)
                DynAttrib.Disable(objaConfig)
            End If

            objList.Add("Address", New DynamicDescriptor("Device Adr", objaAddress))
            objList.Add("Configuration", New DynamicDescriptor(objaConfig))
            Return objList
        End Function

    End Class

#End Region

End Namespace
