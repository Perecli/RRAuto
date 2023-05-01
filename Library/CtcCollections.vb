Imports System.Runtime.Serialization
Imports System.Security.Permissions
Imports RRAutoLib.Loconet
Imports RRAutoLib.Scripting

Namespace CTC

    ''' <summary>Generic interface for lists of CTC objects.</summary>
    Public Interface ICtcObjectListBase
        Inherits IEnumerable
        ReadOnly Property Count() As Integer
        Sub MoveFirst(objCtcObject As CtcObjectBase)
        Sub MoveLast(objCtcObject As CtcObjectBase)
    End Interface

    ''' <summary>Abstract base class for lists of CTC and switchboard objects.</summary>
    <Serializable()> Public MustInherit Class CtcObjectListBase(Of T As {New, CtcObjectBase})
        Implements ICtcObjectListBase, ISerializable, IDeserializationComplete, IEnumerable(Of T)
        Protected _objList As List(Of T)
        Protected _intNamingCounter As Integer

        'needed because the deserialization constructor hides the default constructor
        Friend Sub New()
            _objList = New List(Of T)
        End Sub

        'Serialization -----------------------------------------------------------------

        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            'only called for definitions
            _objList = info.GetValue("CollectionList", GetType(Object))
            _intNamingCounter = info.GetInt32("NamingCounter")
        End Sub

        Friend Overridable Sub PostDeserialize() Implements IDeserializationComplete.PostDeserialize
            Select Case CtcService.SerializationContent
                Case SerializationContent.Definition
                    'placeholder for future compatibility conversions

                Case SerializationContent.State
                    'this object not serialized for states
            End Select

            For Each objItem As T In _objList
                objItem.PostDeserialize()
            Next
        End Sub

        Friend Overridable Sub Initialize() Implements IDeserializationComplete.Initialize
            Select Case CtcService.SerializationContent
                Case SerializationContent.Definition
                    For Each objItem As T In _objList
                        objItem.Parent = Me    'parent relations are not serialized so they are being rebuilt here
                    Next

                Case SerializationContent.State
                    'this object not serialized for states
            End Select

            For Each objItem As T In _objList
                objItem.Initialize()
            Next
        End Sub

        <SecurityPermission(SecurityAction.LinkDemand, Flags:=SecurityPermissionFlag.SerializationFormatter)> _
        Protected Overridable Sub GetObjectData(info As SerializationInfo, context As StreamingContext) Implements ISerializable.GetObjectData
            'only called for definitions
            info.AddValue("CollectionList", _objList)
            info.AddValue("NamingCounter", _intNamingCounter)
        End Sub

        '-------------------------------------------------------------------------------

        Protected MustOverride Sub AssignDefaultName(objCtcObject As T)

        ''' <summary>Creates and adds a new object to the CTC list.</summary>
        ''' <returns>The created object that was added.</returns>
        Public Function Add() As T
            CtcIsStartedException.Check()
            Dim objCtcObject As New T()
            AssignDefaultName(objCtcObject)
            objCtcObject.Parent = Me
            _objList.Add(objCtcObject)
            Return objCtcObject
        End Function

        ''' <summary>Adds a given object to the CTC list.</summary>
        ''' <param name="objCtcObject">The object to be added.</param>
        Public Sub Add(objCtcObject As T)
            CtcIsStartedException.Check()
            AssignDefaultName(objCtcObject)
            objCtcObject.Parent = Me
            _objList.Add(objCtcObject)
        End Sub

        ''' <summary>Removes an object from the CTC list.</summary>
        ''' <param name="objCtcObject">The object to be removed.</param>
        Public Sub Remove(objCtcObject As T)
            CtcIsStartedException.Check()
            objCtcObject.Parent = Nothing
            _objList.Remove(objCtcObject)
        End Sub

        ''' <summary>Gets the number of elements in the list.</summary>
        Public ReadOnly Property Count As Integer Implements ICtcObjectListBase.Count
            Get
                Return _objList.Count
            End Get
        End Property

        Public Function Contains(objCtcObject As T) As Boolean
            Return _objList.Contains(objCtcObject)
        End Function

        Public Overloads Function GetEnumeratorOfT() As IEnumerator(Of T) Implements IEnumerable(Of T).GetEnumerator
            Return _objList.GetEnumerator()
        End Function

        Public Overloads Function GetEnumerator() As IEnumerator Implements IEnumerable.GetEnumerator
            Return _objList.GetEnumerator()
        End Function

        ''' <summary>Gets a list item by index.</summary>
        Default Public ReadOnly Property Item(intIndex As Integer) As T
            Get
                Return _objList(intIndex)
            End Get
        End Property

        ''' <summary>Gets a list item by <see cref="CtcObjectBase.ID" />.</summary>
        Default Public ReadOnly Property Item(sctID As Guid) As T
            Get
                For Each objCtcObjectBase As T In _objList
                    If objCtcObjectBase.ID.Equals(sctID) Then Return objCtcObjectBase
                Next
            End Get
        End Property

        ''' <summary>Gets a list item by <see cref="CtcObjectBase.Name" />.</summary>
        Default Public ReadOnly Property Item(strName As String) As T
            Get
                For Each objCtcObjectBase As T In _objList
                    If objCtcObjectBase.Name = strName Then Return objCtcObjectBase
                Next
            End Get
        End Property

        ''' <summary>Repositions list item to be first in the list.</summary>
        ''' <param name="objCtcObject">Object being moved.</param>
        Public Sub MoveFirst(objCtcObject As CtcObjectBase) Implements ICtcObjectListBase.MoveFirst
            CtcIsStartedException.Check()
            If _objList.Remove(objCtcObject) Then
                _objList.Insert(0, objCtcObject)
            End If
        End Sub

        ''' <summary>Repositions list item to be last in the list.</summary>
        ''' <param name="objCtcObject">Object being moved.</param>
        Public Sub MoveLast(objCtcObject As CtcObjectBase) Implements ICtcObjectListBase.MoveLast
            CtcIsStartedException.Check()
            If _objList.Remove(objCtcObject) Then
                _objList.Add(objCtcObject)
            End If
        End Sub

    End Class


    ''' <summary>A list of <see cref="Track" /> objects.</summary>
    <Serializable()> Public NotInheritable Class TracksList
        Inherits CtcObjectListBase(Of Track)

        Friend Sub New()
            MyBase.New()
        End Sub

        'inherit serialization
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        Protected Overrides Sub AssignDefaultName(objCtcObject As Track)
            _intNamingCounter += 1
            objCtcObject.DefaultName = "Track" & _intNamingCounter
        End Sub

        ''' <summary>Removes references to the given <see cref="Block" /> potentially held by any <see cref="Track" /> object.</summary>
        Public Sub RemoveBlockAssoc(objBlock As Block)
            CtcIsStartedException.Check()
            For Each objTrack As Track In _objList
                If objTrack.Block Is objBlock Then
                    objTrack.Block = Nothing
                    objTrack.NotifyObjectChanged(Me)
                End If
            Next
        End Sub

        Default Public Overloads ReadOnly Property Item(sctLocation As Location) As List(Of Track)
            Get
                Dim objTracks As New List(Of Track)
                For Each objTrack As Track In _objList
                    If objTrack.Location.Equals(sctLocation) Then objTracks.Add(objTrack)
                Next
                Return objTracks
            End Get
        End Property

    End Class

    ''' <summary>A list of <see cref="Block" /> objects.</summary>
    <Serializable()> Public NotInheritable Class BlocksList
        Inherits CtcObjectListBase(Of Block)

        Friend Sub New()
            MyBase.New()
        End Sub

        'inherit serialization
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        Protected Overrides Sub AssignDefaultName(objCtcObject As Block)
            _intNamingCounter += 1
            objCtcObject.DefaultName = "Block" & _intNamingCounter
        End Sub

        ''' <summary>Removes references to the given <see cref="Sensor" /> potentially held by any <see cref="Block" /> object.</summary>
        Friend Sub RemoveSensorAssoc(objSensor As Sensor)
            For Each objBlock As Block In _objList
                If objBlock.Sensor Is objSensor Then
                    objBlock.Sensor = Nothing
                    objBlock.NotifyObjectChanged(Me)
                End If
            Next
        End Sub

    End Class

    ''' <summary>A list of <see cref="Route" /> objects.</summary>
    <Serializable()> Public NotInheritable Class RoutesList
        Inherits CtcObjectListBase(Of Route)

        Friend Sub New()
            MyBase.New()
        End Sub

        'inherit serialization
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        Protected Overrides Sub AssignDefaultName(objCtcObject As Route)
            _intNamingCounter += 1
            objCtcObject.DefaultName = "Route" & _intNamingCounter
        End Sub

        Default Public Overloads ReadOnly Property Item(objTrack As Track) As List(Of Route)
            Get
                Dim objRoutes As New List(Of Route)
                For Each objRoute As Route In _objList
                    If objRoute.RouteElements.Contains(objTrack) Then
                        objRoutes.Add(objRoute)
                    End If
                Next
                Return objRoutes
            End Get
        End Property

        ''' <summary>Removes references to the given track potentially held by a route object.</summary>
        Friend Sub RemoveTrackAssoc(objTrack As Track)
            For Each objRoute As Route In _objList
                If objRoute.RouteElements.Contains(objTrack) Then
                    objRoute.RouteElements.Remove(objTrack)
                End If
            Next
        End Sub

        ''' <summary>Changes the state associated with a track that my no longer be allowed for a changed track type.</summary>
        Friend Sub UpdateTrackState(objTrack As Track)
            For Each objRoute As Route In _objList
                If objRoute.RouteElements.Contains(objTrack) Then
                    Dim objRouteElement As Route.RouteElement = objRoute.RouteElements(objTrack)
                    If objTrack.States Is Nothing Then
                        objRouteElement.State = Track.TrackState.NoState
                    Else
                        If Not objTrack.States.Contains(objRouteElement.State) Then objRouteElement.State = objTrack.States._bytDefaultState
                    End If
                End If
            Next
        End Sub

    End Class

    ''' <summary>A list of <see cref="Sensor" /> objects.</summary>
    <Serializable()> Public NotInheritable Class SensorsList
        Inherits CtcObjectListBase(Of Sensor)

        Friend Sub New()
            MyBase.New()
        End Sub

        'inherit serialization
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        Protected Overrides Sub AssignDefaultName(objCtcObject As Sensor)
            _intNamingCounter += 1
            objCtcObject.DefaultName = "Sensor" & _intNamingCounter
        End Sub

    End Class

    ''' <summary>A list of <see cref="Signal" /> objects.</summary>
    <Serializable()> Public NotInheritable Class SignalsList
        Inherits CtcObjectListBase(Of Signal)

        Friend Sub New()
            MyBase.New()
        End Sub

        'inherit serialization
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        Protected Overrides Sub AssignDefaultName(objCtcObject As Signal)
            _intNamingCounter += 1
            objCtcObject.DefaultName = "Signal" & _intNamingCounter
        End Sub

        Default Public Overloads ReadOnly Property Item(sctLocation As Location) As List(Of Signal)
            Get
                Dim objSignals As New List(Of Signal)
                For Each objSignal As Signal In _objList
                    If objSignal.Location.Equals(sctLocation) Then objSignals.Add(objSignal)
                Next
                Return objSignals
            End Get
        End Property

    End Class

    ''' <summary>A list of <see cref="Accessory" /> objects.</summary>
    <Serializable()> Public NotInheritable Class AccessoriesList
        Inherits CtcObjectListBase(Of Accessory)

        Friend Sub New()
            MyBase.New()
        End Sub

        'inherit serialization
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        Protected Overrides Sub AssignDefaultName(objCtcObject As Accessory)
            _intNamingCounter += 1
            objCtcObject.DefaultName = "Accessory" & _intNamingCounter
        End Sub

    End Class

    ''' <summary>A list of <see cref="Label" /> objects.</summary>
    <Serializable()> Public NotInheritable Class LabelsList
        Inherits CtcObjectListBase(Of Label)

        Friend Sub New()
            MyBase.New()
        End Sub

        'inherit serialization
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        Protected Overrides Sub AssignDefaultName(objCtcObject As Label)
            _intNamingCounter += 1
            objCtcObject.DefaultName = "Label" & _intNamingCounter
        End Sub

        Default Public Overloads ReadOnly Property Item(sctLocation As Location) As List(Of Label)
            Get
                Dim objLabels As New List(Of Label)
                For Each objLabel As Label In _objList
                    If objLabel.Location.Equals(sctLocation) Then objLabels.Add(objLabel)
                Next
                Return objLabels
            End Get
        End Property

    End Class

    ''' <summary>A list of <see cref="Button" /> objects.</summary>
    <Serializable()> Public NotInheritable Class ButtonsList
        Inherits CtcObjectListBase(Of Button)

        Friend Sub New()
            MyBase.New()
        End Sub

        'inherit serialization
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        Protected Overrides Sub AssignDefaultName(objCtcObject As Button)
            _intNamingCounter += 1
            objCtcObject.DefaultName = "Button" & _intNamingCounter
        End Sub

        Default Public Overloads ReadOnly Property Item(sctLocation As Location) As List(Of Button)
            Get
                Dim objButtons As New List(Of Button)
                For Each objButton As Button In _objList
                    If objButton.Location.Equals(sctLocation) Then objButtons.Add(objButton)
                Next
                Return objButtons
            End Get
        End Property

    End Class

    ''' <summary>A list of <see cref="Engine" /> objects.</summary>
    <Serializable()> Public NotInheritable Class EnginesList
        Inherits CtcObjectListBase(Of Engine)

        Friend Sub New()
            MyBase.New()
        End Sub

        'inherit serialization
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        Protected Overrides Sub AssignDefaultName(objCtcObject As Engine)
            _intNamingCounter += 1
            objCtcObject.DefaultName = "Engine" & _intNamingCounter
        End Sub

    End Class

    ''' <summary>A list of <see cref="Sequence" /> objects.</summary>
    <Serializable()> Public NotInheritable Class SequencesList
        Inherits CtcObjectListBase(Of Sequence)

        Friend Sub New()
            MyBase.New()
        End Sub

        'inherit serialization
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        Protected Overrides Sub AssignDefaultName(objCtcObject As Sequence)
            _intNamingCounter += 1
            objCtcObject.DefaultName = "Sequence" & _intNamingCounter
        End Sub

    End Class

    ''' <summary>A list of <see cref="EventScript" /> objects.</summary>
    <Serializable()> Public NotInheritable Class EventScriptsList
        Inherits CtcObjectListBase(Of EventScript)

        Friend Sub New()
            MyBase.New()
        End Sub

        'inherit serialization
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        Protected Overrides Sub AssignDefaultName(objCtcObject As EventScript)
            _intNamingCounter += 1
            objCtcObject.DefaultName = "Script" & _intNamingCounter
        End Sub

        ''' <summary>Removes references to the given boundable object potentially held by any <see cref="EventScript" /> event.</summary>        
        Friend Sub RemoveEventsAssoc(objObject As ISupportsScriptEvents)
            For Each objEventScript As EventScript In _objList
                Dim blnScriptChanged As Boolean = False
                Dim intIdx As Integer = 0
                Do While intIdx < objEventScript.EventBindings.Count  'can't use For Each here because For Each collection can not be tampered with while iterating
                    If objEventScript.EventBindings(intIdx).Owner Is objObject Then
                        objEventScript.EventBindings.RemoveAt(intIdx)
                        blnScriptChanged = True
                    Else
                        intIdx += 1
                    End If
                Loop
                If blnScriptChanged Then objEventScript.NotifyObjectChanged(Me)
            Next
        End Sub

    End Class

    ''' <summary>A list of <see cref="StepScript" /> objects.</summary>
    <Serializable()> Public NotInheritable Class StepScriptsList
        Inherits CtcObjectListBase(Of StepScript)

        Friend Sub New()
            MyBase.New()
        End Sub

        'inherit serialization
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        Protected Overrides Sub AssignDefaultName(objCtcObject As StepScript)
            _intNamingCounter += 1
            objCtcObject.DefaultName = "Script" & _intNamingCounter
        End Sub

    End Class

    ''' <summary>A list of <see cref="GlobalScript" /> objects.</summary>
    <Serializable()> Public NotInheritable Class GlobalScriptsList
        Inherits CtcObjectListBase(Of GlobalScript)

        Friend Sub New()
            MyBase.New()
        End Sub

        'inherit serialization
        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
        End Sub

        Protected Overrides Sub AssignDefaultName(objCtcObject As GlobalScript)
            _intNamingCounter += 1
            objCtcObject.DefaultName = "Script" & _intNamingCounter
        End Sub

    End Class


    ''' <summary>Provides means for a CTC object to have packet sending states.</summary>
    <Serializable()> Public NotInheritable Class PkStatesList
        Implements IEnumerable, ISerializable, IDeserializationComplete
        Friend _objParent As CtcObjectBase
        Friend _objStatesList As List(Of PkStatesList.State)
        Friend _srtPostPkWait As UShort
        Friend _bytDefaultState As Byte
        Friend _bytActiveState As Byte
        Friend _enuDisposition As StateDisposition

        Friend Sub New(objParent As CtcObjectBase)
            _objParent = objParent
            Me.PublishEvents()  'must be after _objParent is set
            _objStatesList = New List(Of PkStatesList.State)
        End Sub

#Region "Serialization"

        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            'this occurs only for definitions
            _objStatesList = info.GetValue("StatesList", GetType(Object))
            _srtPostPkWait = info.GetInt16("SwitchTime")  'renamed
            _bytDefaultState = info.GetByte("DefaultState")
            _bytActiveState = _bytDefaultState  'init this in case states file is not available otherwise it will get overriden anyway
        End Sub

        Friend Sub PostDeserialize() Implements IDeserializationComplete.PostDeserialize
            For Each objItem As PkStatesList.State In _objStatesList
                objItem.PostDeserialize()
            Next
        End Sub

        Friend Sub Initialize() Implements IDeserializationComplete.Initialize
            Me.PublishEvents()    'must be after _objParent is set
            For Each objItem As PkStatesList.State In _objStatesList
                objItem.Parent = _objParent   'parent relations are not serialized so they are being rebuilt here
                objItem.Initialize()
            Next
        End Sub

        <SecurityPermission(SecurityAction.LinkDemand, Flags:=SecurityPermissionFlag.SerializationFormatter)> _
        Protected Sub GetObjectData(info As SerializationInfo, context As StreamingContext) Implements ISerializable.GetObjectData
            'this occurs only for definitions
            info.AddValue("StatesList", _objStatesList)
            info.AddValue("SwitchTime", _srtPostPkWait)    'renamed
            info.AddValue("DefaultState", _bytDefaultState)
        End Sub

#End Region

#Region "Enums/Classes"

        ''' <summary>Enumerates the disposition options of a state.</summary>
        ''' <remarks>
        ''' A <i>Locked</i> disposition has an additional safety benefit over a <i>Set</i> one in that 
        ''' it must be implicitly unlocked before it can be set to another state by another process.
        ''' </remarks>
        Public Enum StateDisposition As Byte
            ''' <summary>State is set but not locked.</summary>
            [Set]
            ''' <summary>State is in the proccess of being set or locked.</summary>
            Pending
            ''' <summary>State is set and locked.</summary>
            Locked
        End Enum

        ''' <summary>Provides the configuration of a single state.</summary>
        <Serializable()> Public NotInheritable Class State
            Implements IEnumerable, ISerializable, IDeserializationComplete
            Private _objParent As CtcObjectBase
            Private _strName As String
            Private _bytValue As Byte
            Private _objPacketsList As List(Of Packet)

            Friend Sub New(bytValue As Byte, objParent As CtcObjectBase)
                _bytValue = bytValue
                _objParent = objParent
                _objPacketsList = New List(Of Packet)
            End Sub

#Region "Serialization"

            Protected Sub New(info As SerializationInfo, context As StreamingContext)
                'this occurs only for definitions
                _strName = info.GetString("Name")
                _bytValue = info.GetByte("Value")
                _objPacketsList = info.GetValue("PacketList", GetType(Object))
            End Sub

            Friend Sub PostDeserialize() Implements IDeserializationComplete.PostDeserialize
            End Sub

            Friend Sub Initialize() Implements IDeserializationComplete.Initialize
                For Each objPacket As Packet In _objPacketsList
                    objPacket.Tag = Me   '.Tag used as parent reference
                Next
            End Sub

            <SecurityPermission(SecurityAction.LinkDemand, Flags:=SecurityPermissionFlag.SerializationFormatter)> _
            Protected Sub GetObjectData(info As SerializationInfo, context As StreamingContext) Implements ISerializable.GetObjectData
                'this occurs only for definitions
                info.AddValue("Name", _strName)
                info.AddValue("Value", _bytValue)
                info.AddValue("PacketList", _objPacketsList)
            End Sub

#End Region

            Public Property Parent() As CtcObjectBase
                Get
                    Return _objParent
                End Get
                Friend Set(Value As CtcObjectBase)
                    _objParent = Value
                End Set
            End Property

            ''' <summary>Gets the internally assigned unique name identifying this state.</summary>
            Public ReadOnly Property DefaultName() As String
                Get
                    Select Case True
                        Case TypeOf _objParent Is Track
                            Return DirectCast(_bytValue, Track.TrackState).ToString
                        Case TypeOf _objParent Is Signal
                            Return DirectCast(_bytValue, Signal.SignalAspect).ToString
                        Case TypeOf _objParent Is Accessory
                            Return "State" & _bytValue
                    End Select
                End Get
            End Property

            ''' <summary>Gets the internally assigned unique value identifying this state.</summary>
            Public ReadOnly Property Value() As Byte
                Get
                    Return _bytValue
                End Get
            End Property

            ''' <summary>Gets or sets a user given name identifying this state.</summary>
            ''' <remarks>If a name is not given the <see cref="DefaultName" /> is returned.</remarks>
            Public Property Name() As String
                Get
                    If _strName = "" Then
                        Return Me.DefaultName
                    Else
                        Return _strName
                    End If
                End Get
                Set(strValue As String)
                    CtcIsStartedException.Check()
                    _strName = strValue
                End Set
            End Property

            ''' <summary>Provides <i>For...Each</i> ability to enumerate the <see cref="Packet" /> list.</summary>
            ''' <remarks>Packets associated to this state are transmitted to the Loconet network when this state is active.</remarks>
            Public Function GetEnumerator() As IEnumerator Implements IEnumerable.GetEnumerator
                'allows For...Each usage
                Return _objPacketsList.GetEnumerator()
            End Function

            ''' <summary>Adds a packet to this state's internal <see cref="Packet" /> list.</summary>
            ''' <remarks>Packets associated to this state are transmitted to the Loconet network when this state is active.</remarks>
            Public Sub AddPacket(objPacket As Packet)
                CtcIsStartedException.Check()
                objPacket.Tag = Me  'add parent reference
                _objPacketsList.Add(objPacket)
            End Sub

            ''' <summary>Removes a given packet associated to this state's internal <see cref="Packet" /> list.</summary>
            ''' <remarks>Packets associated to this state are transmitted to the Loconet network when this state is active.</remarks>
            ''' <seealso cref="GetEnumerator" />
            Public Sub RemovePacket(objPacket As Packet)
                CtcIsStartedException.Check()
                _objPacketsList.Remove(objPacket)
            End Sub

            ''' <summary>Removes all packets associated to this state's internal <see cref="Packet" /> list.</summary>
            ''' <remarks>Packets associated to this state are transmitted to the Loconet network when this state is active.</remarks>
            Public Sub ClearPackets()
                CtcIsStartedException.Check()
                _objPacketsList.Clear()
            End Sub

        End Class

#End Region

#Region "Configuration"

        ''' <summary>Gets or sets the time in milliseconds to wait, after sending all state packets, before considering the state settled.</summary>
        ''' <remarks>For slow motion switching machines this ensures the virtual state is not set before the physical state has a chance to settle.</remarks>
        ''' <seealso cref="Packet.PostTxWait"/>
        Public Property PostPkWait() As UShort
            Get
                Return _srtPostPkWait
            End Get
            Set(Value As UShort)
                CtcIsStartedException.Check()
                _srtPostPkWait = Value
            End Set
        End Property

        ''' <summary>Gets or sets the state applied to new or reset state objects.</summary>
        Public Property [Default]() As PkStatesList.State
            Get
                Return Me(_bytDefaultState)
            End Get
            Set(objState As PkStatesList.State)
                CtcIsStartedException.Check()
                _bytDefaultState = objState.Value
            End Set
        End Property

        ''' <summary>Provides <i>For...Each</i> ability to enumerate the states in the list.</summary>
        Public Function GetEnumerator() As IEnumerator Implements IEnumerable.GetEnumerator
            Return _objStatesList.GetEnumerator()
        End Function

        ''' <summary>Gets the number of states in the list.</summary>
        Public ReadOnly Property Count() As Integer
            Get
                Return _objStatesList.Count
            End Get
        End Property

        ''' <summary>Gets the previous state in the list relative to the <see cref="Active" /> state.</summary>
        ''' <remarks>First state in the list is wrapped around to the last.</remarks>
        ''' <returns>Returns <i>Nothing</i> if less than two states exist.</returns>
        Public Function Prev() As State
            Return Me.Prev(_bytActiveState)
        End Function

        ''' <summary>Gets the previous state in the list relative to a given state.</summary>
        ''' <remarks>First state in the list is wrapped around to the last.</remarks>
        ''' <returns>Returns <i>Nothing</i> if less than two states exist or the given state is invalid.</returns>
        Public Function Prev(bytValue As Byte) As State
            If _objStatesList.Count < 2 Then Return Nothing

            Dim objState As State = Me.Item(bytValue)
            If objState Is Nothing Then Return Nothing

            If _objStatesList.IndexOf(objState) = 0 Then
                Return _objStatesList(_objStatesList.Count - 1)
            Else
                Return _objStatesList(_objStatesList.IndexOf(objState) - 1)
            End If

        End Function

        ''' <summary>Gets the next state in the list relative to the <see cref="Active" /> state.</summary>
        ''' <remarks>Last item in the list is wrapped around to the first.</remarks>
        ''' <returns>Returns <i>Nothing</i> if less than two states exist.</returns>
        Public Function [Next]() As State
            Return Me.Next(_bytActiveState)
        End Function

        ''' <summary>Gets the next state in the list relative to a given state.</summary>
        ''' <remarks>Last item in the list is wrapped around to the first.</remarks>
        ''' <returns>Returns <i>Nothing</i> if less than two states exist or the given state is invalid.</returns>
        Public Function [Next](bytValue As Byte) As State
            If _objStatesList.Count < 2 Then Return Nothing

            Dim objState As State = Me.Item(bytValue)
            If objState Is Nothing Then Return Nothing

            If _objStatesList.IndexOf(objState) = _objStatesList.Count - 1 Then
                Return _objStatesList(0)
            Else
                Return _objStatesList(_objStatesList.IndexOf(objState) + 1)
            End If

        End Function

        ''' <summary>Gets a state object from the list by its index.</summary>
        Default Public Overloads ReadOnly Property Item(intIndex As Integer) As State
            Get
                Return _objStatesList(intIndex)
            End Get
        End Property

        ''' <summary>Gets a state object from the list by its value.</summary>
        Default Public Overloads ReadOnly Property Item(bytValue As Byte) As State
            Get
                For Each objState As State In _objStatesList
                    If objState.Value = bytValue Then Return objState
                Next
            End Get
        End Property

        ''' <summary>Gets a state object from the list by its name.</summary>
        Default Public Overloads ReadOnly Property Item(strName As String) As State
            Get
                For Each objState As State In _objStatesList
                    If objState.Name = strName Then Return objState
                Next
            End Get
        End Property

        ''' <summary>Determines whether a specific state is present in the list.</summary>
        Public Function Contains(bytValue As Byte) As Boolean
            Return Me(bytValue) IsNot Nothing
        End Function

        ''' <summary>Resets the <see cref="Active" /> state to <see cref="[Default]" /> state and <see cref="Disposition" /> to <see cref="StateDisposition.Set" />.</summary>
        ''' <remarks>This property must be set while the CtcService is stopped otherwise a <see cref="CtcIsStartedException" /> will be thrown.</remarks>
        Public Sub Reset()
            CtcIsStartedException.Check()
            _bytActiveState = _bytDefaultState
            _enuDisposition = StateDisposition.Set
        End Sub

        Friend Sub Update(objStates As List(Of Byte))
            'add missing states for the new CtcObjectBase type
            For Each bytState As Byte In objStates
                If Not Me.Contains(bytState) Then
                    _objStatesList.Add(New PkStatesList.State(bytState, _objParent))
                End If
            Next

            'remove states that no longer pertain to the new CtcObjectBase type
            Dim intIdx As Integer = 0
            Do While intIdx < _objStatesList.Count  'can't use For Each here because For Each collection can not be tampered with while iterating
                If Not objStates.Contains(_objStatesList(intIdx).Value) Then
                    _objStatesList.Remove(_objStatesList(intIdx))
                Else
                    intIdx += 1
                End If
            Loop

            _bytDefaultState = objStates(0)
            _bytActiveState = objStates(0)
        End Sub

#End Region

#Region "Operation"

        ''' <summary>Gets the disposition of the <see cref="Active" /> state.</summary>
        Public ReadOnly Property Disposition() As StateDisposition
            Get
                Return _enuDisposition
            End Get
        End Property

        ''' <summary>Gets the currently active state object.</summary>    
        Public ReadOnly Property Active() As PkStatesList.State
            Get
                Return Me(_bytActiveState)
            End Get
        End Property


        Friend Async Function SetStateValidate(objState As State, strGivenKey As String, sctCancelToken As CancellationToken) As Task(Of Boolean)

            Select Case _enuDisposition
                Case StateDisposition.Pending
                    CtcService.PostMessage(CtcService.MessageCat.Error, String.Format("[{0}] could not be set because it is pending a state change.", _objParent.ToString))
                    Return False

                Case StateDisposition.Locked
                    CtcService.PostMessage(CtcService.MessageCat.Error, String.Format("[{0}] could not be set because it is locked.", _objParent.ToString))
                    Return False

                Case StateDisposition.Set
                    Select Case True
                        Case objState Is Nothing
                            CtcService.PostMessage(CtcService.MessageCat.Error, String.Format("[{0}] could not be set to [{1}]. Given state is invalid.", _objParent.ToString, strGivenKey))
                            Return False

                        Case _bytActiveState <> objState.Value
                            Dim objScriptEventArgs As New ScriptEventArgs(_objParent, "BeforeStateChange", objState, sctCancelToken)
                            Await DirectCast(_objParent, ISupportsScriptEvents).ScriptEventBinder.InvokeHandlersOnCtc(objScriptEventArgs)
                            If objScriptEventArgs.ReqCancel.Value Then
                                CtcService.PostMessage(CtcService.MessageCat.Native, String.Format("[{0}] was not set. Canceled by user script.", _objParent.ToString))
                                Return False
                            End If

                    End Select
            End Select

            Return True

        End Function

        Friend Async Function SetStatePerform(objState As State, blnByRoute As Boolean, sctCancelToken As CancellationToken) As Task(Of Boolean)

            If CtcService.ForceSetState OrElse _bytActiveState <> objState.Value Then

                If Not blnByRoute Then
                    Await SetDispositionAndInvoke(StateDisposition.Pending, sctCancelToken)
                    _objParent.NotifyObjectChanged(Me)
                End If

                If Not CtcService.SimulationMode Then
                    For Each objPacket As Packet In objState
                        Await CtcService.LoconetService.TxPacket(objPacket.Clone())
                        Await Task.Delay(objPacket.PostTxWait)
                    Next
                End If

                Await Task.Delay(_srtPostPkWait)

                Await SetActiveStateAndInvoke(objState.Value, sctCancelToken)
                CtcService.PostMessage(CtcService.MessageCat.Native, String.Format("[{0}] has been set to [{1}].", _objParent.ToString, objState.Name))

                If Not blnByRoute Then
                    Await SetDispositionAndInvoke(StateDisposition.Set, sctCancelToken)
                    _objParent.NotifyObjectChanged(Me)
                End If

            End If

            Return True

        End Function

        Friend Function SetDispositionAndInvoke(enuDisposition As StateDisposition, sctCancelToken As CancellationToken) As Task

            If _enuDisposition <> enuDisposition Then
                _enuDisposition = enuDisposition
                Return DirectCast(_objParent, ISupportsScriptEvents).ScriptEventBinder.InvokeHandlersOnCtc(New ScriptEventArgs(_objParent, "DispositionChanged", Nothing, sctCancelToken))
            End If
            Return Task.FromResult(0)

        End Function

        Private Function SetActiveStateAndInvoke(bytActiveState As Byte, sctCancelToken As CancellationToken) As Task

            If _bytActiveState <> bytActiveState Then
                _bytActiveState = bytActiveState
                Return DirectCast(_objParent, ISupportsScriptEvents).ScriptEventBinder.InvokeHandlersOnCtc(New ScriptEventArgs(_objParent, "StateChanged", Nothing, sctCancelToken))
            End If
            Return Task.FromResult(0)

        End Function

#End Region

#Region "Scripting Events"

        Private _straScriptingEvents As String() = New String() {
            "BeforeStateChange",
            "DispositionChanged",
            "StateChanged"
        }

        Private Sub PublishEvents()
            DirectCast(_objParent, ISupportsScriptEvents).ScriptEventBinder.PublishEvents(_straScriptingEvents)
        End Sub

        Friend Sub Dispose()
            DirectCast(_objParent, ISupportsScriptEvents).ScriptEventBinder.UnPublishEvents(_straScriptingEvents)
        End Sub

#End Region

        Friend Function ShallowClone() As PkStatesList
            Return Me.MemberwiseClone()
        End Function

    End Class

End Namespace


