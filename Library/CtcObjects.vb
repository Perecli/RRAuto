Imports System.Runtime.Serialization
Imports System.Security.Permissions
Imports System.ComponentModel
Imports RRAutoLib.Loconet
Imports RRAutoLib.Scripting

Namespace CTC

    ''' <summary>Object alignment as it relates to its parent.</summary>
    Public Enum Alignment As Byte
        TopLeft
        TopCenter
        TopRight
        MiddleLeft
        MiddleCenter
        MiddleRight
        BottomLeft
        BottomCenter
        BottomRight
    End Enum


    ''' <summary>Interface implemented by classes derived from <see cref="CtcObjectBase" /> that have a global unique identifier.</summary>
    Public Interface ICtcObjectBase
        ReadOnly Property ID() As Guid
        Property Name() As String
        Sub NotifyObjectChanged(objSender As Object)
    End Interface

    ''' <summary>Interface implemented by classes derived from <see cref="CtcObjectBase" /> that have a switchboard location.</summary>
    Public Interface ILocation
        Inherits ICtcObjectBase

        Property Location() As Location
    End Interface

    ''' <summary>Interface implemented by classes derived from <see cref="CtcObjectBase" /> that float obove the tracks.</summary>
    Public Interface ITopLayer
        Inherits ICtcObjectBase
        Inherits ILocation

        Property DrawScale() As Single
        Property CellAlign() As Alignment
        Property AlignOffsetX() As Single
        Property AlignOffsetY() As Single
    End Interface

    ''' <summary>Interface implemented by classes derived from <see cref="CtcObjectBase" /> that can be operated from the switchboard.</summary>
    Public Interface ISwBrdOperable
        Inherits ICtcObjectBase

        Function Press(Optional sctCancelToken As CancellationToken = Nothing) As Task(Of Boolean)
        Function Release(Optional sctCancelToken As CancellationToken = Nothing) As Task(Of Boolean)
    End Interface

    ''' <summary>Interface implemented by classes derived from <see cref="CtcObjectBase" /> that have packet sending states.</summary>
    Public Interface IPkStates
        Inherits ICtcObjectBase

        ReadOnly Property States() As PkStatesList
        ReadOnly Property State As PkStatesList.State
        Function SetState(bytStateValue As Byte, Optional sctCancelToken As CancellationToken = Nothing) As Task(Of Boolean)
        Function SetState(strStateName As String, Optional sctCancelToken As CancellationToken = Nothing) As Task(Of Boolean)
        Function SetState(objState As PkStatesList.State, Optional sctCancelToken As CancellationToken = Nothing) As Task(Of Boolean)

    End Interface

    ''' <summary>Interface implemented by classes derived from <see cref="CtcObjectBase" /> that have editable script.</summary>
    Public Interface IScript
        Inherits ICtcObjectBase

        Property Enabled() As Boolean
        Property Script() As String
    End Interface


    ''' <summary>The grid location of a switchboard object.</summary>
    <Serializable()> Public Structure Location
        Private _srtCol As Short
        Private _srtRow As Short
        <NonSerialized()> Public Shared ReadOnly Empty As Location   'I think shared variables are not serialized anyway

        Public Sub New(intCol As Short, intRow As Short)
            _srtCol = intCol
            _srtRow = intRow
        End Sub

        <Description("Column of grid element.")> Public Property Col() As Short
            Get
                Return _srtCol
            End Get
            Set(srtValue As Short)
                'CtcIsStartedException.Check()   'had to remove this because this is called internally while CTC is running 
                _srtCol = srtValue
            End Set
        End Property

        <Description("Row of grid element.")> Public Property Row() As Short
            Get
                Return _srtRow
            End Get
            Set(srtValue As Short)
                'CtcIsStartedException.Check()   'had to remove this because this is called internally while CTC is running 
                _srtRow = srtValue
            End Set
        End Property

        Public Function Offset(srtOffsetCol As Short, srtOffsetRow As Short) As Location
            Return New Location(_srtCol + srtOffsetCol, _srtRow + srtOffsetRow)
        End Function

    End Structure


    ''' <summary>Abstract base class for CTC and switchboard objects.</summary>
    <Serializable()> Public MustInherit Class CtcObjectBase
        Implements ISerializable, IDeserializationComplete, ICloneable, ICtcObjectBase

        'serialized
        Protected _sctId As Guid = Guid.NewGuid
        Protected _strDefaultName As String = "Unnamed"
        Protected _strName As String
        Protected _objPersistedTag As Byte()

        Protected _blnDeleted As Boolean
        Protected _objParent As ICtcObjectListBase
        Protected _objStoredState As CtcObjectBase

        Protected Sub New()
        End Sub

#Region "Serialization"

        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            _sctId = info.GetValue("Id", GetType(Object))
            If CtcService.SerializationContent = SerializationContent.Definition Then
                _strDefaultName = info.GetString("DefaultName")
                _strName = info.GetString("Name")
                _objPersistedTag = info.GetValue("Tag", GetType(Object))
            End If
        End Sub

        Friend Overridable Sub PostDeserialize() Implements IDeserializationComplete.PostDeserialize
        End Sub

        Friend Overridable Sub Initialize() Implements IDeserializationComplete.Initialize
        End Sub

        <SecurityPermission(SecurityAction.LinkDemand, Flags:=SecurityPermissionFlag.SerializationFormatter)> _
        Protected Overridable Sub GetObjectData(info As SerializationInfo, context As StreamingContext) Implements ISerializable.GetObjectData
            info.AddValue("Id", _sctId)
            If CtcService.SerializationContent = SerializationContent.Definition Then
                info.AddValue("DefaultName", _strDefaultName)
                info.AddValue("Name", _strName)
                info.AddValue("Tag", _objPersistedTag)
            End If
        End Sub

#End Region

        ''' <summary>Gets the CTC object's globaly unique static identifier.</summary>
        ''' <remarks>This is mainly used internally for definition/state serialization matching but can be used for other things that require an immutable identifier.</remarks>
        Public ReadOnly Property ID() As Guid Implements ICtcObjectBase.ID
            Get
                Return _sctId
            End Get
        End Property

        ''' <summary>Gets the default identifiable name assigned to this CTC object.</summary>
        ''' <remarks>
        ''' This value is generated by the <see cref="CtcObjectListBase" /> when this <see cref="CtcObjectBase" /> is added to it.
        ''' If not tracked by any of the <see cref="CtcService" /> lists it returns "Unnamed".
        ''' In general, the <see cref="CtcObjectBase.Name" /> property should be used for identification. 
        ''' This default is used as a guaranteed fallback name if no <see cref="CtcObjectBase.Name" /> has been given.
        ''' </remarks>
        Public Property DefaultName() As String
            Get
                Return _strDefaultName
            End Get
            Friend Set(value As String)
                _strDefaultName = value
            End Set
        End Property

        ''' <summary>Gets or sets the identifiable name assigned to this CTC object.</summary>
        ''' <returns>If no value has been previously given to this property the <see cref="CtcObjectBase.DefaultName" /> will be returned instead.</returns>
        Public Property Name() As String Implements ICtcObjectBase.Name
            Get
                If _strName = "" Then
                    Return _strDefaultName
                Else
                    Return _strName
                End If
            End Get
            Set(strValue As String)
                If Me.Parent IsNot Nothing Then CtcIsStartedException.Check()
                _strName = strValue
            End Set
        End Property

        ''' <summary>Gets or sets the binary representation of an arbitrary object that provides additional data to be stored with the layout data.</summary>
        ''' <remarks>Best practice is to store the result byte stream of your serialized object.</remarks>
        Public Property PersistedTag() As Byte()
            Get
                Return _objPersistedTag
            End Get
            Set(Value As Byte())
                _objPersistedTag = Value
            End Set
        End Property

        ''' <summary>Gets or sets an arbitrary object that provides additional data but is disposed between sessions.</summary>
        Public Property Tag() As Object

        ''' <summary>Gets the parent list that contains this CTC object.</summary>
        Public Property Parent() As ICtcObjectListBase
            Get
                Return _objParent
            End Get
            Friend Set(value As ICtcObjectListBase)
                _objParent = value
            End Set
        End Property

        ''' <summary>Deletes this CTC object.</summary>
        ''' <remarks>Properly disposes the object and removes all references to it.</remarks>
        Public Overridable Sub DeleteSelf()
            If Me.Parent IsNot Nothing Then CtcIsStartedException.Check()
            _blnDeleted = True
        End Sub

        ''' <summary>Gets an indication whether this CTC object has been flaged for deletion by <see cref="DeleteSelf" />.</summary>
        Public ReadOnly Property IsDeleted() As Boolean
            Get
                Return _blnDeleted
            End Get
        End Property

        ''' <summary>Raises the <see cref="CtcService.ObjectChanged" /> event for this object.</summary>
        ''' <param name="objSender">The caller's object reference which will be passed to the event.</param>
        ''' <remarks>This method should be called after changing a CTC object's definition.</remarks>
        Public Sub NotifyObjectChanged(objSender As Object) Implements ICtcObjectBase.NotifyObjectChanged
            'todo: when converting to this approach, "obj Is obj" comparisons will have to be revisited 
            'Dim objActiveState As CtcObjectBase = Me.Clone
            'CTC.NotifyObjectChanged(objSender, objActiveState, _objStoredState)
            '_objStoredState = objActiveState

            CtcService.RaiseObjectChangedEvent(objSender, Me, _objStoredState)
            _objStoredState = Me.Clone
        End Sub

        ''' <summary>Used internally during deserialization.</summary>
        Protected Sub InitStoredState()
            _objStoredState = Me.Clone
        End Sub

        ''' <summary>Used internally as an initializer before the CTC service is started.</summary>
        Friend Overridable Sub BeforeCtcServiceStart()
        End Sub

        ''' <summary>Used internally as an initializer after the CTC service is started.</summary>
        Friend Overridable Function AfterCtcServiceStart() As Task
        End Function

        ''' <summary>Used internally as a disposer before the CTC service is stopped.</summary>
        Friend Overridable Sub BeforeCtcServiceStop()
        End Sub

        ''' <summary>Used internally as a disposer after the CTC service is stopped.</summary>
        Friend Overridable Sub AfterCtcServiceStop()
        End Sub

        ''' <summary>Used internally.</summary>
        Friend Overridable Function Clone() As Object Implements ICloneable.Clone
            Dim objNewSelf As CtcObjectBase = Me.MemberwiseClone()

            'don't want cloned objects to copy previous clone references
            'if this is not set to nothing an endless string of references would occur
            objNewSelf._objStoredState = Nothing

            Return objNewSelf
        End Function

    End Class


    ''' <summary>A switchboard track object.</summary>
    <Serializable()> Public NotInheritable Class Track
        Inherits CtcObjectBase
        Implements IPkStates, ILocation, ISupportsScriptEvents, ISwBrdOperable

        'serialized
        Private _sctLocation As Location
        Private _enuType As TrackType = TrackType.TrackStraight
        Private _bytOrientation As Byte = 3
        Private _objBlock As Block
        Private _objStates As PkStatesList

        Public Sub New()
            _sctLocation = New Location(1, 1)
        End Sub

        Public Sub New(enuType As TrackType, bytOrientation As Byte)
            Me.New()
            Me.Type = enuType
            Me.Orientation = bytOrientation
        End Sub

#Region "Serialization"

        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
            Select Case CtcService.SerializationContent
                Case SerializationContent.Definition
                    _sctLocation = info.GetValue("Location", GetType(Object))
                    _enuType = info.GetValue("Type", GetType(Object))
                    _bytOrientation = info.GetByte("Orientation")
                    _objBlock = info.GetValue("Block", GetType(Object))
                    _objStates = info.GetValue("States", GetType(Object))

                Case SerializationContent.State
                    Dim objTrack As Track = CtcService.Tracks(Me.ID)
                    If objTrack IsNot Nothing AndAlso objTrack.IsTurnout Then
                        objTrack.States._bytActiveState = info.GetByte("CurrentState")
                        objTrack.States._enuDisposition = info.GetValue("Disposition", GetType(Object))
                    End If

            End Select
        End Sub

        Friend Overrides Sub PostDeserialize()
            MyBase.PostDeserialize()
            Select Case CtcService.SerializationContent
                Case SerializationContent.Definition
                    If Me.IsTurnout Then _objStates.PostDeserialize()

                Case SerializationContent.State

            End Select
        End Sub

        Friend Overrides Sub Initialize()
            MyBase.Initialize()
            Select Case CtcService.SerializationContent
                Case SerializationContent.Definition
                    If Me.IsTurnout Then
                        _objStates._objParent = Me 'parent relations are not serialized so they are being rebuilt here
                        _objStates.Initialize()
                    End If
                    Me.InitStoredState()

                Case SerializationContent.State
                    If Me.IsTurnout Then Me.InitStoredState()

            End Select
        End Sub

        <SecurityPermission(SecurityAction.LinkDemand, Flags:=SecurityPermissionFlag.SerializationFormatter)> _
        Protected Overrides Sub GetObjectData(info As SerializationInfo, context As StreamingContext)
            MyBase.GetObjectData(info, context)
            Select Case CtcService.SerializationContent
                Case SerializationContent.Definition
                    info.AddValue("Location", _sctLocation)
                    info.AddValue("Type", _enuType)
                    info.AddValue("Orientation", _bytOrientation)
                    info.AddValue("Block", _objBlock)
                    info.AddValue("States", _objStates)
                Case SerializationContent.State
                    If Me.IsTurnout Then
                        info.AddValue("CurrentState", _objStates._bytActiveState)
                        info.AddValue("Disposition", If(_objStates._enuDisposition = PkStatesList.StateDisposition.Pending, PkStatesList.StateDisposition.Set, _objStates._enuDisposition))
                    End If
            End Select
        End Sub

#End Region

#Region "Enumerations"

        ''' <summary>Enumerates the available track types.</summary>
        Public Enum TrackType As Byte
            TrackStraight
            ''' <summary>Mildly curved track.</summary>
            TrackCurved1
            ''' <summary>Sharply curved track.</summary>
            TrackCurved2
            ''' <summary>Right angle crossing.</summary>
            TrackCrossing1
            ''' <summary>Diagonal crossing.</summary>
            TrackCrossing2
            TrackTerminator
            TurnoutLeft
            TurnoutRight
            TurnoutSingleSlip
            TurnoutDoubleSlip
            TurnoutThreeWay
        End Enum

        ''' <summary>Enumerates the available track states.</summary>
        Public Enum TrackState As Byte
            NoState   'passed, on behalf of tracks with no state, to functions that handle tracks and turnours
            Straight
            Diverging
            Cross1
            Cross2
            Diverging1
            Diverging2
        End Enum

#End Region

#Region "Overrides"

        Public Overrides Sub DeleteSelf()
            MyBase.DeleteSelf()
            'remove all references to this track
            CtcService.Routes.RemoveTrackAssoc(Me)
            CtcService.EventScripts.RemoveEventsAssoc(Me)
            CtcService.Tracks.Remove(Me)
        End Sub

        Public Overrides Function ToString() As String
            If Me.IsTurnout() Then
                Return "Turnout." & Me.Name
            Else
                Return "Track." & Me.Name
            End If
        End Function

#End Region

#Region "Configuration"

        ''' <summary>Gets or sets the locations of this track on the switchboard.</summary>
        Public Property Location() As Location Implements ILocation.Location
            Get
                Return _sctLocation
            End Get
            Set(sctValue As Location)
                If CtcService.Tracks.Contains(Me) Then CtcIsStartedException.Check()
                _sctLocation = sctValue
            End Set
        End Property

        ''' <summary>Gets or sets type of this track.</summary>
        Public Property Type() As TrackType
            Get
                Return _enuType
            End Get
            Set(Value As TrackType)
                If CtcService.Tracks.Contains(Me) Then CtcIsStartedException.Check()

                If Value.ToString.StartsWith("Turnout") Then
                    If _objStates Is Nothing Then
                        _objStates = New PkStatesList(Me)
                        _objStates._srtPostPkWait = 1000
                    End If
                    Select Case Value
                        Case TrackType.TurnoutLeft, TrackType.TurnoutRight
                            _objStates.Update(New List(Of Byte)(New TrackState() {TrackState.Straight, TrackState.Diverging}))
                        Case TrackType.TurnoutSingleSlip
                            _objStates.Update(New List(Of Byte)(New TrackState() {TrackState.Cross1, TrackState.Cross2, TrackState.Diverging}))
                        Case TrackType.TurnoutDoubleSlip
                            _objStates.Update(New List(Of Byte)(New TrackState() {TrackState.Cross1, TrackState.Cross2, TrackState.Diverging1, TrackState.Diverging2}))
                        Case TrackType.TurnoutThreeWay
                            _objStates.Update(New List(Of Byte)(New TrackState() {TrackState.Straight, TrackState.Diverging1, TrackState.Diverging2}))
                    End Select
                Else
                    If _objStates IsNot Nothing Then
                        CtcService.EventScripts.RemoveEventsAssoc(Me)
                        _objStates.Dispose()
                        _objStates = Nothing
                    End If
                End If

                'if this track is contained in a route, it ensures that the state for it contained in the route is a valid one since it could have become obsolete
                CtcService.Routes.UpdateTrackState(Me)

                _enuType = Value
            End Set
        End Property

        ''' <summary>Gets or sets the orientation of the track on the switchboard.</summary>
        ''' <value>Valid values are 1-8.</value>
        ''' <remarks>This property is used for rotating the track on the switchboard.</remarks>
        Public Property Orientation() As Byte
            Get
                Return _bytOrientation
            End Get
            Set(Value As Byte)
                If CtcService.Tracks.Contains(Me) Then CtcIsStartedException.Check()
                If Value < 1 Or Value > 8 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid orientation. Valid values are 1-8.")
                _bytOrientation = Value
            End Set
        End Property

        ''' <summary>Gets or sets the <see cref="Block" /> that this track is assigned to.</summary>
        ''' <value>A <see cref="Block" /> instance. <i>Nothing</i> (default) if this track is not assigned to any <see cref="Block" />.</value>
        Public Property Block() As Block
            Get
                Return _objBlock
            End Get
            Set(Value As Block)
                If CtcService.Tracks.Contains(Me) Then CtcIsStartedException.Check()
                _objBlock = Value
            End Set
        End Property

        ''' <summary>Gets the states list configured by the <see cref="Type" /> property.</summary>
        Public ReadOnly Property States() As PkStatesList Implements IPkStates.States
            Get
                Return _objStates
            End Get
        End Property

        ''' <summary>Gets a value indicating whether this track is a turnout.</summary>
        ''' <value><i>True</i> if this track is a turnout; otherwise, <i>False</i>.</value>
        ''' <remarks>If this track is not a turnout <see cref="States"/> will return <i>Nothing</i>.</remarks>
        Public ReadOnly Property IsTurnout() As Boolean
            Get
                Return _objStates IsNot Nothing
            End Get
        End Property

#End Region

#Region "Operation"

        ''' <summary>Action to be performd when a switchboard track represenation is pressed by mouse or touch-point.</summary>
        ''' <param name="sctCancelToken">Token provided by a controlling source to notify this method that it should cancel its execution.</param>
        ''' <returns>An awaitable <see cref="Tasks.Task" /> returning <i>True</i> on success, otherwise <i>False</i>.</returns>
        ''' <remarks>The CtcService must be started otherwise a <see cref="CtcIsStoppedException" /> will be thrown.</remarks>
        ''' <seealso cref="OperateRelease" /> 
        Public Function OperatePress(Optional sctCancelToken As CancellationToken = Nothing) As Task(Of Boolean) Implements ISwBrdOperable.Press

            If CtcService.IsCurrentThread Then
                Return OperatePressOnCtc(sctCancelToken)
            Else
                CtcIsStoppedException.Check()

                Dim objTcs As New TaskCompletionSource(Of Boolean)
                CtcService.Execute(Async Sub() objTcs.TrySetResult(Await OperatePressOnCtc(sctCancelToken)))
                Return objTcs.Task
            End If

        End Function

        Private Async Function OperatePressOnCtc(sctCancelToken As CancellationToken) As Task(Of Boolean)

            Dim objScriptEventArgs As New ScriptEventArgs(Me, "BeforeOperatePress", Nothing, sctCancelToken)
            Await Me.ScriptEventBinder.InvokeHandlersOnCtc(objScriptEventArgs)
            'since there is no native implementation for a track press operation, we don't have to check for Canceled operation

            Return True

        End Function

        ''' <summary>Action to be performd when a switchboard track represenation is released by mouse or touch-point.</summary>
        ''' <param name="sctCancelToken">Token provided by a controlling source to notify this method that it should cancel its execution.</param>
        ''' <returns>An awaitable <see cref="Tasks.Task" /> returning <i>True</i> on success, otherwise <i>False</i>.</returns>
        ''' <remarks>The CtcService must be started otherwise a <see cref="CtcIsStoppedException" /> will be thrown.</remarks>
        ''' <seealso cref="OperatePress" /> 
        Public Function OperateRelease(Optional sctCancelToken As CancellationToken = Nothing) As Task(Of Boolean) Implements ISwBrdOperable.Release

            If CtcService.IsCurrentThread Then
                Return OperateReleaseOnCtc(sctCancelToken)
            Else
                CtcIsStoppedException.Check()

                Dim objTcs As New TaskCompletionSource(Of Boolean)
                CtcService.Execute(Async Sub() objTcs.TrySetResult(Await OperateReleaseOnCtc(sctCancelToken)))
                Return objTcs.Task
            End If

        End Function

        Private Async Function OperateReleaseOnCtc(sctCancelToken As CancellationToken) As Task(Of Boolean)

            Dim objScriptEventArgs As New ScriptEventArgs(Me, "BeforeOperateRelease", Nothing, sctCancelToken)
            Await Me.ScriptEventBinder.InvokeHandlersOnCtc(objScriptEventArgs)
            If objScriptEventArgs.ReqCancel.Value Then
                CtcService.PostMessage(CtcService.MessageCat.Native, String.Format("Native operation for [{0}] was canceled by user script.", Me.ToString))
            Else
                If Me.IsTurnout Then
                    Dim objState As PkStatesList.State = Me.States.Next()
                    If objState IsNot Nothing Then
                        Return Await Me.SetStateOnCtc(objState, objState.Name, sctCancelToken)
                    End If
                End If
            End If

            Return True

        End Function


        ''' <summary>Gets the track's currently active state object.</summary>
        ''' <remarks>This is a short hand version of <i>Track.States.</i><see cref="PkStatesList.Active">Active</see>.</remarks>
        Public ReadOnly Property State As PkStatesList.State Implements IPkStates.State
            Get
                Return Me.States.Active
            End Get
        End Property


        ''' <summary>Sets the track's active state by value.</summary>
        ''' <param name="bytStateValue">A <see cref="TrackState" /> member that is valid for this track type.</param>
        ''' <param name="sctCancelToken">Token provided by a controlling source to notify this method that it should cancel its execution.</param>
        ''' <returns>An awaitable <see cref="Tasks.Task" /> returning <i>True</i> on success, otherwise <i>False</i>.</returns>
        ''' <remarks>The CtcService must be started otherwise a <see cref="CtcIsStoppedException" /> will be thrown.</remarks>
        ''' <seealso cref="Track.States" />  
        Public Function SetState(bytStateValue As Byte, Optional sctCancelToken As CancellationToken = Nothing) As Task(Of Boolean) Implements IPkStates.SetState
            Return SetState(_objStates(bytStateValue), DirectCast(bytStateValue, TrackState).ToString, sctCancelToken)
        End Function

        ''' <summary>Sets the track's active state by name.</summary>
        ''' <param name="strStateName">The state's <see cref="PkStatesList.State.Name" />.</param>    
        ''' <param name="sctCancelToken">Token provided by a controlling source to notify this method that it should cancel its execution.</param> 
        ''' <returns>An awaitable <see cref="Tasks.Task" /> returning <i>True</i> on success, otherwise <i>False</i>.</returns>   
        ''' <remarks>The CtcService must be started otherwise a <see cref="CtcIsStoppedException" /> will be thrown.</remarks> 
        ''' <seealso cref="Track.States" /> 
        Public Function SetState(strStateName As String, Optional sctCancelToken As CancellationToken = Nothing) As Task(Of Boolean) Implements IPkStates.SetState
            Return SetState(_objStates(strStateName), strStateName, sctCancelToken)
        End Function

        ''' <summary>Sets the track's active state by object.</summary>
        ''' <param name="sctCancelToken">Token provided by a controlling source to notify this method that it should cancel its execution.</param> 
        ''' <returns>An awaitable <see cref="Tasks.Task" /> returning <i>True</i> on success, otherwise <i>False</i>.</returns>   
        ''' <remarks>The CtcService must be started otherwise a <see cref="CtcIsStoppedException" /> will be thrown.</remarks> 
        ''' <seealso cref="Track.States" /> 
        Public Function SetState(objState As PkStatesList.State, Optional sctCancelToken As CancellationToken = Nothing) As Task(Of Boolean) Implements IPkStates.SetState
            Return SetState(objState, objState.Name, sctCancelToken)
        End Function


        Private Function SetState(objState As PkStatesList.State, strGivenKey As String, sctCancelToken As CancellationToken) As Task(Of Boolean)

            If Not Me.IsTurnout Then Exit Function

            If CtcService.IsCurrentThread Then
                Return SetStateOnCtc(objState, strGivenKey, sctCancelToken)
            Else
                CtcIsStoppedException.Check()

                Dim objTcs As New TaskCompletionSource(Of Boolean)
                CtcService.Execute(Async Sub() objTcs.TrySetResult(Await SetStateOnCtc(objState, strGivenKey, sctCancelToken)))
                Return objTcs.Task
            End If

        End Function

        Private Async Function SetStateOnCtc(objState As PkStatesList.State, strGivenKey As String, sctCancelToken As CancellationToken) As Task(Of Boolean)
            If Await _objStates.SetStateValidate(objState, strGivenKey, sctCancelToken) Then
                Return Await _objStates.SetStatePerform(objState, False, sctCancelToken)
            Else
                Return False
            End If
        End Function

#End Region

#Region "Scripting Events"

        Private _objScriptEventBinder As New ScriptEventBinder(Me, {
            "BeforeOperatePress",
            "BeforeOperateRelease"
        })

        Public ReadOnly Property ScriptEventBinder As ScriptEventBinder Implements ISupportsScriptEvents.ScriptEventBinder
            Get
                Return _objScriptEventBinder
            End Get
        End Property

#End Region

        Friend Overrides Function Clone() As Object
            'do a shallow copy clone
            Dim objNewSelf As Track = MyBase.Clone()

            'do a shallow copy clone on states; state items and packets do not get cloned
            If Me.States IsNot Nothing Then objNewSelf._objStates = Me.States.ShallowClone()

            Return objNewSelf
        End Function

    End Class

    ''' <summary>A set of <see cref="Track" /> objects treated as one unit.</summary>
    ''' <remarks>A block is usually associated with a sensor to monitor the occupancy of a contiguous set of tracks. A track can be associated to one block only.</remarks>
    <Serializable()> Public NotInheritable Class Block
        Inherits CtcObjectBase

        'serialized
        Private _srtLength As UShort
        Private _blnHasCatenary As Boolean
        Private _objSensor As Sensor
        Private _blnOccupied As Boolean
        Private _blnReserved As Boolean

        Public Sub New()
        End Sub

#Region "Serialization"

        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
            Select Case CtcService.SerializationContent
                Case SerializationContent.Definition
                    _srtLength = info.GetInt16("Length")
                    _blnHasCatenary = info.GetBoolean("HasCatenary")
                    _objSensor = info.GetValue("Sensor", GetType(Object))
                Case SerializationContent.State
                    Dim objBlock As Block = CtcService.Blocks(Me.ID)
                    If objBlock IsNot Nothing Then
                        Try
                            _blnOccupied = info.GetBoolean("Occupied")
                            _blnReserved = info.GetBoolean("Reserved")
                        Catch
                            'compatibility added 11/20/13
                            'we don't set the CompatibilityMode because this is the .rrs states file
                        End Try
                    End If
            End Select
        End Sub

        Friend Overrides Sub Initialize()
            MyBase.Initialize()

            Me.InitStoredState()    'for both serialization contents
        End Sub

        <SecurityPermission(SecurityAction.LinkDemand, Flags:=SecurityPermissionFlag.SerializationFormatter)> _
        Protected Overrides Sub GetObjectData(info As SerializationInfo, context As StreamingContext)
            MyBase.GetObjectData(info, context)
            Select Case CtcService.SerializationContent
                Case SerializationContent.Definition
                    info.AddValue("Length", _srtLength)
                    info.AddValue("HasCatenary", _blnHasCatenary)
                    info.AddValue("Sensor", _objSensor)
                Case SerializationContent.State
                    info.AddValue("Occupied", _blnOccupied)
                    info.AddValue("Reserved", _blnReserved)
            End Select
        End Sub

#End Region

#Region "Overrides"

        Public Overrides Sub DeleteSelf()
            MyBase.DeleteSelf()
            'remove all references to this block
            CtcService.Tracks.RemoveBlockAssoc(Me)
            CtcService.Blocks.Remove(Me)
        End Sub

        Public Overrides Function ToString() As String
            Return "Block." & Me.Name
        End Function

        Friend Overrides Function AfterCtcServiceStart() As Task
            If _objSensor IsNot Nothing Then _
                AddHandler _objSensor.StateChangedOnCtcThread, AddressOf SensorStateChanged
            Return Task.FromResult(0)
        End Function

        Friend Overrides Sub BeforeCtcServiceStop()
            If _objSensor IsNot Nothing Then _
                RemoveHandler _objSensor.StateChangedOnCtcThread, AddressOf SensorStateChanged
        End Sub

#End Region

#Region "Configuration"

        ''' <summary>Gets or sets the physical length of the block.</summary>
        ''' <value>Number in centimeters.</value>
        ''' <remarks>Not currently used. Reserverd for future planned features.</remarks>
        Public Property Length() As UShort
            Get
                Return _srtLength
            End Get
            Set(Value As UShort)
                If CtcService.Blocks.Contains(Me) Then CtcIsStartedException.Check()
                _srtLength = Value
            End Set
        End Property

        ''' <summary>Gets or sets a value that indicates whether the block has over head catenary.</summary>
        ''' <value>If <i>True</i> engines that require catenary should be allowed on the block.</value>
        Public Property HasCatenary() As Boolean
            Get
                Return _blnHasCatenary
            End Get
            Set(blnValue As Boolean)
                If CtcService.Blocks.Contains(Me) Then CtcIsStartedException.Check()
                _blnHasCatenary = blnValue
            End Set
        End Property

        ''' <summary>Gets or sets the <see cref="Sensor" /> object associated with the block.</summary>
        Public Property Sensor() As Sensor
            Get
                Return _objSensor
            End Get
            Set(Value As Sensor)
                If CtcService.Blocks.Contains(Me) Then CtcIsStartedException.Check()
                _objSensor = Value
            End Set
        End Property

#End Region

#Region "Operation"

        ''' <summary>Gets a value that indicates whether the block has been occupied.</summary>
        ''' <returns>Returns <i>True</i> if the associated <see cref="Sensor" /> reported an <i>On</i> state; otherwise <i>False</i>.</returns>
        Public Property Occupied() As Boolean
            Get
                Return _blnOccupied
            End Get
            Friend Set(value As Boolean)
                _blnOccupied = value
            End Set
        End Property

        ''' <summary>Gets or sets a value that indicates whether the block has been reserved.</summary>
        ''' <remarks>Can be used to reserve the block to prevent contention of the same resource.</remarks>
        Public Property Reserved() As Boolean
            Get
                Return _blnReserved
            End Get
            Set(value As Boolean)
                If _blnReserved <> value Then
                    _blnReserved = value
                    Me.NotifyObjectChanged(Me)
                    CtcService.PostMessage(CtcService.MessageCat.Native, String.Format("[{0}] reservation has been set to [{1}].", Me.ToString, _blnReserved.ToString))
                End If
            End Set
        End Property

#End Region

#Region "Event Handling"

        ''' <summary>Runs on the CTC thread.</summary>
        Private Sub SensorStateChanged(objPacket As Packet)
            'objPacket not currently used; reserved for future enhancements

            Me.Occupied = _objSensor.State = OnOff.On
            Me.NotifyObjectChanged(Me)
            CtcService.PostMessage(CtcService.MessageCat.Native, String.Format("[{0}] occupation has been set to [{1}].", Me.ToString, _blnOccupied.ToString))
        End Sub

#End Region

    End Class

    ''' <summary>A set of <see cref="Track" /> objects with a particular turnout state configuration.</summary>
    <Serializable()> Public NotInheritable Class Route
        Inherits CtcObjectBase
        Implements ISupportsScriptEvents

        'serialized
        Private _blnSerializeSwitching As Boolean
        Private _srtLength As UShort
        Private _objRouteElements As RouteElementList
        Private _enuState As RouteState = RouteState.Unlocked

        Public Sub New()
            _objRouteElements = New RouteElementList
        End Sub

#Region "Serialization"

        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
            Select Case CtcService.SerializationContent
                Case SerializationContent.Definition
                    Try
                        'added new field that broke compatibility
                        _blnSerializeSwitching = info.GetBoolean("SerializeSwitching")
                    Catch
                        'compatibility added 10/27/12
                        CtcService.CompatibilityMode = True
                    End Try
                    _srtLength = info.GetInt16("Length")
                    _objRouteElements = info.GetValue("RouteElements", GetType(Object))
                Case SerializationContent.State
                    Dim objRoute As Route = CtcService.Routes(Me.ID)
                    If objRoute IsNot Nothing Then
                        objRoute._enuState = info.GetValue("State", GetType(Object))
                    End If
            End Select
        End Sub

        Friend Overrides Sub Initialize()
            MyBase.Initialize()

            Me.InitStoredState()    'for both serialization contents
        End Sub

        <SecurityPermission(SecurityAction.LinkDemand, Flags:=SecurityPermissionFlag.SerializationFormatter)> _
        Protected Overrides Sub GetObjectData(info As SerializationInfo, context As StreamingContext)
            MyBase.GetObjectData(info, context)
            Select Case CtcService.SerializationContent
                Case SerializationContent.Definition
                    info.AddValue("SerializeSwitching", _blnSerializeSwitching)
                    info.AddValue("Length", _srtLength)
                    info.AddValue("RouteElements", _objRouteElements)
                Case SerializationContent.State
                    info.AddValue("State", _enuState)
            End Select
        End Sub

#End Region

#Region "Enumerations"

        Public Enum RouteState As Byte
            ''' <summary>Members of this route can be used by others.</summary>
            Unlocked
            ''' <summary>Route is in the process of being set or locked.</summary>
            Pending
            ''' <summary>No member of this route can be used by another.</summary>
            Locked
        End Enum

        Friend Enum RouteAction As Byte
            [Set]
            Lock
            Unlock
        End Enum

#End Region

#Region "Classes"

        ''' <summary>A list of <see cref="RouteElement" /> objects.</summary>
        <Serializable()> Public NotInheritable Class RouteElementList
            Implements IEnumerable, ISerializable
            Private _objElementsList As List(Of RouteElement)

            Public Sub New()
                MyBase.New()
                _objElementsList = New List(Of RouteElement)
            End Sub

            'Serialization -------------------------------------------------------------

            Protected Sub New(info As SerializationInfo, context As StreamingContext)
                'only called for definitions
                _objElementsList = info.GetValue("ElementsList", GetType(Object))
            End Sub

            <SecurityPermission(SecurityAction.LinkDemand, Flags:=SecurityPermissionFlag.SerializationFormatter)> _
            Protected Sub GetObjectData(info As SerializationInfo, context As StreamingContext) Implements ISerializable.GetObjectData
                'only called for definitions
                info.AddValue("ElementsList", _objElementsList)
            End Sub

            '---------------------------------------------------------------------------

            Public Function GetEnumerator() As IEnumerator Implements IEnumerable.GetEnumerator
                'allows For...Each usage
                Return _objElementsList.GetEnumerator()
            End Function

            Public Function Add() As RouteElement
                CtcIsStartedException.Check()
                Dim objRouteElement As New RouteElement
                _objElementsList.Add(objRouteElement)
                Return objRouteElement
            End Function

            Public Sub Add(objRouteElement As RouteElement)
                'CtcIsStartedException.Check()   'had to remove this because this is called internally while CTC is running 
                _objElementsList.Add(objRouteElement)
            End Sub

            Public Sub Remove(objRouteElement As RouteElement)
                CtcIsStartedException.Check()
                _objElementsList.Remove(objRouteElement)
            End Sub

            Public Sub Remove(objTrack As Track)
                CtcIsStartedException.Check()
                Dim intIdx As Integer = 0
                Do While intIdx < _objElementsList.Count  'can't use For Each here because For Each collection can not be tampered with while iterating
                    If DirectCast(_objElementsList(intIdx), RouteElement).Track Is objTrack Then
                        _objElementsList.RemoveAt(intIdx)
                    Else
                        intIdx += 1
                    End If
                Loop
            End Sub

            Public Sub Clear()
                CtcIsStartedException.Check()
                _objElementsList.Clear()
            End Sub

            Public Function Contains(objRouteElement As RouteElement) As Boolean
                Return _objElementsList.Contains(objRouteElement)
            End Function

            Public Function Contains(objTrack As Track) As Boolean
                Return Me.Item(objTrack) IsNot Nothing
            End Function

            Default Public ReadOnly Property Item(objTrack As Track) As RouteElement
                Get
                    For Each objRouteElement As RouteElement In _objElementsList
                        If objRouteElement.Track Is objTrack Then Return objRouteElement
                    Next
                End Get
            End Property

            Default Public ReadOnly Property Item(intIndex As Integer) As RouteElement
                Get
                    Return _objElementsList(intIndex)
                End Get
            End Property

            Public ReadOnly Property Count() As Integer
                Get
                    Return _objElementsList.Count
                End Get
            End Property

        End Class

        ''' <summary>A switchboard track object with a configured turnout state.</summary>
        <Serializable()> Public NotInheritable Class RouteElement
            Implements ICloneable, ISerializable
            Private _objTrack As Track
            Private _enuState As Track.TrackState

            Public Sub New()
            End Sub

            Public Sub New(objTrack As Track, enuState As Track.TrackState)
                Me.Track = objTrack
                Me.State = enuState
            End Sub

            'Serialization -------------------------------------------------------------

            Protected Sub New(info As SerializationInfo, context As StreamingContext)
                'only called for definitions
                _objTrack = info.GetValue("Track", GetType(Object))
                _enuState = info.GetValue("State", GetType(Object))
            End Sub

            <SecurityPermission(SecurityAction.LinkDemand, Flags:=SecurityPermissionFlag.SerializationFormatter)> _
            Protected Sub GetObjectData(info As SerializationInfo, context As StreamingContext) Implements ISerializable.GetObjectData
                'only called for definitions
                info.AddValue("Track", _objTrack)
                info.AddValue("State", _enuState)
            End Sub

            '---------------------------------------------------------------------------

            Public Property Track() As Track
                Get
                    Return _objTrack
                End Get
                Set(Value As Track)
                    CtcIsStartedException.Check()
                    _objTrack = Value
                End Set
            End Property

            Public Property State() As Track.TrackState
                Get
                    Return _enuState
                End Get
                Set(Value As Track.TrackState)
                    CtcIsStartedException.Check()
                    _enuState = Value
                End Set
            End Property

            Public Function Clone() As Object Implements ICloneable.Clone
                Return Me.MemberwiseClone
            End Function

        End Class

#End Region

#Region "Overrides"

        Public Overrides Sub DeleteSelf()
            MyBase.DeleteSelf()
            'remove all references to this route
            CtcService.EventScripts.RemoveEventsAssoc(Me)
            CtcService.Routes.Remove(Me)
        End Sub

        Public Overrides Function ToString() As String
            Return "Route." & Me.Name
        End Function

#End Region

#Region "Configuration"

        ''' <summary>Indicates whether the route should serialize the switching of its turnouts.</summary>
        ''' <remarks>
        ''' Some switch solenoids operate such that concurrent turnout switching can be problematic. 
        ''' This option switches a route's turnouts one at a time waiting for the state of the previous one to settle before setting the next. 
        ''' The settling of the states can be further tuned through <see cref="PkStatesList.PostPkWait" /> or <see cref="Packet.PostTxWait" />.
        ''' </remarks>
        Public Property SerializeSwitching() As Boolean
            Get
                Return _blnSerializeSwitching
            End Get
            Set(Value As Boolean)
                If CtcService.Routes.Contains(Me) Then CtcIsStartedException.Check()
                _blnSerializeSwitching = Value
            End Set
        End Property

        ''' <summary>Gets or sets the physical length of the route.</summary>
        ''' <value>Number in centimeters.</value>
        ''' <remarks>Not used internally. Can be used for calculations in custom script.</remarks>
        Public Property Length() As UShort
            Get
                Return _srtLength
            End Get
            Set(Value As UShort)
                If CtcService.Routes.Contains(Me) Then CtcIsStartedException.Check()
                _srtLength = Value
            End Set
        End Property

        Public ReadOnly Property RouteElements() As RouteElementList
            Get
                Return _objRouteElements
            End Get
        End Property

        ''' <summary>Indicates whether the route shares one or more tracks with another given route.</summary>        
        Public Function Intersects(objRoute As Route) As Boolean
            For Each objRouteElement As RouteElement In _objRouteElements
                If objRoute.RouteElements.Contains(objRouteElement.Track) Then
                    Return True
                End If
            Next
        End Function

        ''' <summary>Resets the route to the default <see cref="RouteState.Unlocked" /> state.</summary>
        ''' <remarks>This property must be set while the CtcService is stopped otherwise a <see cref="CtcIsStartedException" /> will be thrown.</remarks>
        Public Sub Reset()
            If CtcService.Routes.Contains(Me) Then CtcIsStartedException.Check()
            _enuState = RouteState.Unlocked
        End Sub

#End Region

#Region "Operation"

        ''' <summary>Gets the route's current <see cref="Route.RouteState" /> value.</summary>
        Public ReadOnly Property State() As RouteState
            Get
                Return _enuState
            End Get
        End Property

        ''' <summary>Sets the turnout states of the route, if no locking conflict exists, but does not lock the route.</summary>
        ''' <param name="sctCancelToken">Token provided by a controlling source to notify this method that it should cancel its execution.</param>
        ''' <returns>An awaitable <see cref="Tasks.Task" /> returning <i>True</i> on success, otherwise <i>False</i>.</returns>
        ''' <remarks>The CtcService must be started otherwise a <see cref="CtcIsStoppedException" /> will be thrown.</remarks> 
        Public Function [Set](Optional sctCancelToken As CancellationToken = Nothing) As Task(Of Boolean)
            Return SetRoute(RouteAction.Set, sctCancelToken)
        End Function

        ''' <summary>Sets the turnout states of the route, if no locking conflict exists, and locks the route.</summary>
        ''' <param name="sctCancelToken">Token provided by a controlling source to notify this method that it should cancel its execution.</param>
        ''' <returns>An awaitable <see cref="Tasks.Task" /> returning <i>True</i> on success, otherwise <i>False</i>.</returns>
        ''' <remarks>The CtcService must be started otherwise a <see cref="CtcIsStoppedException" /> will be thrown.</remarks> 
        Public Function Lock(Optional sctCancelToken As CancellationToken = Nothing) As Task(Of Boolean)
            Return SetRoute(RouteAction.Lock, sctCancelToken)
        End Function

        ''' <summary>Unlocks the route, if route has been previously locked.</summary>
        ''' <param name="sctCancelToken">Token provided by a controlling source to notify this method that it should cancel its execution.</param>
        ''' <returns>An awaitable <see cref="Tasks.Task" /> returning <i>True</i> on success, otherwise <i>False</i>.</returns>
        ''' <remarks>The CtcService must be started otherwise a <see cref="CtcIsStoppedException" /> will be thrown.</remarks> 
        Public Function Unlock(Optional sctCancelToken As CancellationToken = Nothing) As Task(Of Boolean)
            Return SetRoute(RouteAction.Unlock, sctCancelToken)
        End Function


        Private Function SetRoute(enuAction As RouteAction, sctCancelToken As CancellationToken) As Task(Of Boolean)

            If CtcService.IsCurrentThread Then
                Return SetRouteOnCtc(enuAction, sctCancelToken)  'waitable only from CTC thread
            Else
                CtcIsStoppedException.Check()

                Dim objTcs As New TaskCompletionSource(Of Boolean)
                CtcService.Execute(Async Sub() objTcs.TrySetResult(Await SetRouteOnCtc(enuAction, sctCancelToken)))
                Return objTcs.Task
            End If

        End Function

        Private Async Function SetRouteOnCtc(enuAction As RouteAction, sctCancelToken As CancellationToken) As Task(Of Boolean)

            If _enuState = RouteState.Pending Then
                CtcService.PostMessage(CtcService.MessageCat.Error, String.Format("[{0}] is in a pending state. Action aborted.", Me.ToString))
                Return False
            End If

            If _enuState = RouteState.Unlocked AndAlso enuAction = RouteAction.Unlock Then
                CtcService.PostMessage(CtcService.MessageCat.Native, String.Format("[{0}] is already unlocked. Action aborted.", Me.ToString))
                Return False
            End If

            If _enuState = RouteState.Locked AndAlso (enuAction = RouteAction.Set OrElse enuAction = RouteAction.Lock) Then
                CtcService.PostMessage(CtcService.MessageCat.Native, String.Format("[{0}] is in a locked state. Action aborted.", Me.ToString))
                Return False
            End If

            Select Case enuAction
                Case RouteAction.Set, RouteAction.Lock
                    For Each objRouteElement As RouteElement In _objRouteElements
                        If objRouteElement.Track.IsTurnout Then
                            If Not Await objRouteElement.Track.States.SetStateValidate(objRouteElement.Track.States(objRouteElement.State), objRouteElement.State.ToString, sctCancelToken) Then
                                CtcService.PostMessage(CtcService.MessageCat.Native, String.Format("[{0}] contains turnouts that could not be set. Action aborted.", Me.ToString))
                                Return False
                            End If
                        Else
                            For Each objRoute As Route In CtcService.Routes
                                If objRoute.State = RouteState.Unlocked Then Continue For
                                If objRoute.RouteElements.Contains(objRouteElement.Track) Then
                                    CtcService.PostMessage(CtcService.MessageCat.Native, String.Format("[{0}] contains tracks in conflict with other routes. Action aborted.", Me.ToString))
                                    Return False
                                End If
                            Next
                        End If
                    Next
            End Select

            Dim objScriptEventArgs As New ScriptEventArgs(Me, "Before" & enuAction.ToString, Nothing, sctCancelToken)
            Await Me.ScriptEventBinder.InvokeHandlersOnCtc(objScriptEventArgs)
            If objScriptEventArgs.ReqCancel.Value Then
                CtcService.PostMessage(CtcService.MessageCat.Native, String.Format("[{0}] action was canceled by user script.", Me.ToString))
                Return False
            End If

            'validation is complete, now do the work ---------------------------------------------

            Select Case enuAction
                Case RouteAction.Set, RouteAction.Lock
                    Await SetStateAndInvoke(RouteState.Pending, sctCancelToken)
                    Me.NotifyObjectChanged(Me)

                    For Each objRouteElement As RouteElement In Me.RouteElements
                        If objRouteElement.Track.IsTurnout Then
                            Await objRouteElement.Track.States.SetDispositionAndInvoke(PkStatesList.StateDisposition.Pending, sctCancelToken)
                            objRouteElement.Track.NotifyObjectChanged(Me)
                        End If
                    Next

                    If _blnSerializeSwitching Then
                        For Each objRouteElement As RouteElement In Me.RouteElements
                            If objRouteElement.Track.IsTurnout Then
                                Await SetRouteElem(objRouteElement, enuAction, sctCancelToken)
                            End If
                        Next
                    Else
                        Dim objTasks As New List(Of Task)
                        For Each objRouteElement As RouteElement In Me.RouteElements
                            If objRouteElement.Track.IsTurnout Then
                                Dim objTask As Task = SetRouteElem(objRouteElement, enuAction, sctCancelToken)
                                objTasks.Add(objTask)
                            End If
                        Next
                        Await Task.WhenAll(objTasks)
                    End If

                    Select Case enuAction
                        Case RouteAction.Set
                            Await SetStateAndInvoke(RouteState.Unlocked, sctCancelToken)
                            Me.NotifyObjectChanged(Me)
                            CtcService.PostMessage(CtcService.MessageCat.Native, String.Format("[{0}] has been set.", Me.ToString))

                        Case RouteAction.Lock
                            Await SetStateAndInvoke(RouteState.Locked, sctCancelToken)
                            Me.NotifyObjectChanged(Me)
                            CtcService.PostMessage(CtcService.MessageCat.Native, String.Format("[{0}] has been locked.", Me.ToString))
                    End Select

                Case RouteAction.Unlock
                    For Each objRouteElement As RouteElement In _objRouteElements
                        If objRouteElement.Track.IsTurnout Then
                            Await objRouteElement.Track.States.SetDispositionAndInvoke(PkStatesList.StateDisposition.Set, sctCancelToken)
                            objRouteElement.Track.NotifyObjectChanged(Me)
                        End If
                    Next

                    Await SetStateAndInvoke(RouteState.Unlocked, sctCancelToken)
                    Me.NotifyObjectChanged(Me)
                    CtcService.PostMessage(CtcService.MessageCat.Native, String.Format("[{0}] has been unlocked.", Me.ToString))

            End Select

            Return True

        End Function

        Private Async Function SetRouteElem(objRouteElement As RouteElement, enuAction As RouteAction, sctCancelToken As CancellationToken) As Task

            Await objRouteElement.Track.States.SetStatePerform(objRouteElement.Track.States(objRouteElement.State), True, sctCancelToken)

            Select Case enuAction
                Case RouteAction.Set
                    Await objRouteElement.Track.States.SetDispositionAndInvoke(PkStatesList.StateDisposition.Set, sctCancelToken)
                    objRouteElement.Track.NotifyObjectChanged(Me)

                Case RouteAction.Lock
                    Await objRouteElement.Track.States.SetDispositionAndInvoke(PkStatesList.StateDisposition.Locked, sctCancelToken)
                    objRouteElement.Track.NotifyObjectChanged(Me)
                    CtcService.PostMessage(CtcService.MessageCat.Native, String.Format("[{0}] has been locked by [{1}].", objRouteElement.Track.ToString, Me.ToString))
            End Select

        End Function

        Private Function SetStateAndInvoke(enuState As RouteState, sctCancelToken As CancellationToken) As Task

            If _enuState <> enuState Then
                _enuState = enuState
                Return Me.ScriptEventBinder.InvokeHandlersOnCtc(New ScriptEventArgs(Me, "StateChanged", Nothing, sctCancelToken))
            End If
            Return Task.FromResult(0)

        End Function

#End Region

#Region "Scripting Events"

        Private _objScriptEventBinder As New ScriptEventBinder(Me, {
            "BeforeSet",
            "BeforeLock",
            "BeforeUnlock",
            "StateChanged"
        })

        Public ReadOnly Property ScriptEventBinder As ScriptEventBinder Implements ISupportsScriptEvents.ScriptEventBinder
            Get
                Return _objScriptEventBinder
            End Get
        End Property

#End Region

        Friend Overrides Function Clone() As Object
            Dim objNewSelf As Route = MyBase.Clone()
            objNewSelf._objRouteElements = New RouteElementList
            For Each objRouteElement As RouteElement In _objRouteElements
                objNewSelf._objRouteElements.Add(objRouteElement.Clone)
            Next
            Return objNewSelf
        End Function

    End Class

    ''' <summary>A sensor or input object.</summary>
    <Serializable()> Public NotInheritable Class Sensor
        Inherits CtcObjectBase
        Implements ISupportsScriptEvents

        'serialized
        Private _enuObservePacket As ObservePacketType = ObservePacketType.InputRep
        Private _srtAddress As UShort
        Private _enuInputDevice As DeviceType = DeviceType.BDL16x
        Private _enuState As OnOff

        Public Sub New()
        End Sub

#Region "Serialization"

        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
            Select Case CtcService.SerializationContent
                Case SerializationContent.Definition
                    Try
                        _enuObservePacket = info.GetValue("ObservePacket", GetType(Object))
                        _srtAddress = info.GetValue("Address", GetType(Object))
                        _enuInputDevice = info.GetValue("InputDevice", GetType(Object))
                    Catch
                        Try
                            'convert obsolete values to new variables
                            _enuObservePacket = info.GetValue("ObservePacket", GetType(Object))
                            _srtAddress = info.GetValue("Address", GetType(Object))
                            _enuInputDevice = info.GetValue("InputRepConfig", GetType(Object))

                            'compatibility added 11/19/11
                            CtcService.CompatibilityMode = True
                        Catch
                            'convert obsolete values to new variables
                            _enuObservePacket = ObservePacketType.InputRep
                            _enuInputDevice = info.GetValue("InputDevice", GetType(Object))
                            Dim objTempPk As New PkInput
                            objTempPk.Bytes(1) = info.GetByte("Byte1")
                            objTempPk.Bytes(2) = info.GetByte("Byte2")
                            _srtAddress = objTempPk.Address

                            'compatibility added 10/27/11
                            CtcService.CompatibilityMode = True
                        End Try
                    End Try

                Case SerializationContent.State
                    Dim objSensor As Sensor = CtcService.Sensors(Me.ID)
                    If objSensor IsNot Nothing Then
                        _enuState = info.GetValue("State", GetType(Object))
                    End If

            End Select
        End Sub

        Friend Overrides Sub Initialize()
            MyBase.Initialize()

            Me.InitStoredState()    'for both serialization contents
        End Sub

        <SecurityPermission(SecurityAction.LinkDemand, Flags:=SecurityPermissionFlag.SerializationFormatter)> _
        Protected Overrides Sub GetObjectData(info As SerializationInfo, context As StreamingContext)
            MyBase.GetObjectData(info, context)
            Select Case CtcService.SerializationContent
                Case SerializationContent.Definition
                    info.AddValue("ObservePacket", _enuObservePacket)
                    info.AddValue("Address", _srtAddress)
                    info.AddValue("InputDevice", _enuInputDevice)
                Case SerializationContent.State
                    info.AddValue("State", _enuState)
            End Select
        End Sub

#End Region

#Region "Enumerations"

        ''' <summary>The Loconet packet type to observe for sensor events.</summary>
        Public Enum ObservePacketType As Byte
            ''' <summary>Observes the <see cref="PkInput" /> packet.</summary>
            InputRep
            ''' <summary>Observes the <see cref="PkSwitchInput" /> packet.</summary>
            SwitchRep
            ''' <summary>Observes the <see cref="PkMultiSense" /> packet.</summary>
            MultiSense
            ''' <summary>Observes the <see cref="PkSecurityElem" /> packet.</summary>
            SecurityElem
            ''' <summary>Observes the <see cref="PkLissy" /> packet.</summary>
            Lissy
        End Enum

        ''' <summary>The device type generating the sensor event packets.</summary>
        ''' <remarks>Determines how the packet's 12 bit address space should be decoded.</remarks>
        Public Enum DeviceType As Byte
            ''' <summary>8 bits used for device address and 4 used for ports.</summary>
            BDL16x
            ''' <summary>9 bits used for device address, 2 bits for input pair, and 1 bit for input type.</summary>
            DS54
            ''' <summary>All 12 bits used for device address.</summary>
            Other
        End Enum

#End Region

#Region "Overrides"

        Public Overrides Sub DeleteSelf()
            MyBase.DeleteSelf()
            'remove all references to this sensor
            CtcService.Blocks.RemoveSensorAssoc(Me)
            CtcService.EventScripts.RemoveEventsAssoc(Me)
            CtcService.Sensors.Remove(Me)
        End Sub

        Public Overrides Function ToString() As String
            Return "Sensor." & Me.Name
        End Function

        Friend Overrides Function AfterCtcServiceStart() As Task
            AddHandler CtcService.LoconetService.RxPacketOnWorkerThread, AddressOf RxPacket
            Return Task.FromResult(0)
        End Function

        Friend Overrides Sub BeforeCtcServiceStop()
            RemoveHandler CtcService.LoconetService.RxPacketOnWorkerThread, AddressOf RxPacket
        End Sub

#End Region

#Region "Configuration"

        ''' <summary>Gets or sets the packet type this sensor should observe for events.</summary>  
        Public Property ObservePacket() As ObservePacketType
            Get
                Return _enuObservePacket
            End Get
            Set(Value As ObservePacketType)
                If CtcService.Sensors.Contains(Me) Then CtcIsStartedException.Check()
                _enuObservePacket = Value

                Dim objSupDevType As List(Of Sensor.DeviceType)
                Dim srtMaxAddress As UShort
                Select Case Value
                    Case Sensor.ObservePacketType.InputRep
                        objSupDevType = New List(Of Sensor.DeviceType) From {Sensor.DeviceType.Other, Sensor.DeviceType.BDL16x, Sensor.DeviceType.DS54}
                        srtMaxAddress = 4095
                    Case Sensor.ObservePacketType.SwitchRep
                        objSupDevType = New List(Of Sensor.DeviceType) From {Sensor.DeviceType.Other, Sensor.DeviceType.DS54}
                        srtMaxAddress = 2047
                    Case Sensor.ObservePacketType.MultiSense
                        objSupDevType = New List(Of Sensor.DeviceType) From {Sensor.DeviceType.Other, Sensor.DeviceType.BDL16x}
                        srtMaxAddress = 4095
                    Case Sensor.ObservePacketType.SecurityElem
                        objSupDevType = New List(Of Sensor.DeviceType) From {Sensor.DeviceType.Other}
                        srtMaxAddress = 16383
                    Case Sensor.ObservePacketType.Lissy
                        objSupDevType = New List(Of Sensor.DeviceType) From {Sensor.DeviceType.Other}
                        srtMaxAddress = 4095
                End Select

                'if current input device is not supported by the given observed packet type, default the device to first supported
                If Not objSupDevType.Contains(_enuInputDevice) Then _enuInputDevice = objSupDevType(0)

                'if current address is larger than the observed packet type allows, cap it
                _srtAddress = Math.Min(_srtAddress, srtMaxAddress)
            End Set
        End Property

        ''' <summary>Gets or sets the device address this sensor will respond to.</summary>        
        Public Property Address() As UShort
            Get
                Return _srtAddress
            End Get
            Set(Value As UShort)
                If CtcService.Sensors.Contains(Me) Then CtcIsStartedException.Check()

                Dim srtMaxAddress As UShort
                Select Case _enuObservePacket
                    Case Sensor.ObservePacketType.SwitchRep
                        srtMaxAddress = 2047
                    Case Sensor.ObservePacketType.InputRep, Sensor.ObservePacketType.MultiSense, Sensor.ObservePacketType.Lissy
                        srtMaxAddress = 4095
                    Case Sensor.ObservePacketType.SecurityElem
                        srtMaxAddress = 16383
                End Select
                If Value > srtMaxAddress Then Throw New ArgumentOutOfRangeException(Nothing, String.Format("Invalid address. For observing '{0}' packets, valid values are 0-{1}.", _enuObservePacket, srtMaxAddress))

                _srtAddress = Value
            End Set
        End Property

        ''' <summary>Gets or sets the input device type which determines how the packet's 12 bit address space should be decoded.</summary>        
        Public Property InputDevice() As DeviceType
            Get
                Return _enuInputDevice
            End Get
            Set(Value As DeviceType)
                'no need to check if CTC is started because this is only used for configuring the UI display and not for operations on the CTC thread
                'no need to validate either because only the client needs to validate this 
                _enuInputDevice = Value
            End Set
        End Property

#End Region

#Region "Operation"

        ''' <summary>Gets the sensor's current <see cref="Loconet.OnOff" /> value.</summary>
        Public ReadOnly Property State() As OnOff
            Get
                Return _enuState
            End Get
        End Property

        ''' <summary>Sets the sensor's <see cref="Loconet.OnOff" /> value.</summary>
        ''' <param name="enuState">The new state to be assigned.</param>
        ''' <param name="sctCancelToken">Token provided by a controlling source to notify this method that it should cancel its execution.</param>
        ''' <remarks>The CtcService must be started otherwise a <see cref="CtcIsStoppedException" /> will be thrown.</remarks>
        Public Function SetState(enuState As OnOff, Optional sctCancelToken As CancellationToken = Nothing) As task

            Return SetState(enuState, Nothing, sctCancelToken)

        End Function


        Private Function SetState(enuState As OnOff, objPacket As Packet, sctCancelToken As CancellationToken) As task

            If CtcService.IsCurrentThread Then
                Return SetStateOnCtc(enuState, objPacket, sctCancelToken)
            Else
                CtcIsStoppedException.Check()

                Dim objTcs As New TaskCompletionSource
                CtcService.Execute(Async Sub()
                                       Await SetStateOnCtc(enuState, objPacket, sctCancelToken)
                                       objTcs.TrySetResult()
                                   End Sub)
                Return objTcs.Task
            End If

        End Function

        Private Async Function SetStateOnCtc(enuState As OnOff, objPacket As Packet, sctCancelToken As CancellationToken) As Task

            If enuState <> _enuState Then
                _enuState = enuState
                Me.NotifyObjectChanged(Me)

                CtcService.PostMessage(CtcService.MessageCat.Native, String.Format("[{0}] has been set to [{1}].", Me.ToString, _enuState.ToString))

                Await Me.ScriptEventBinder.InvokeHandlersOnCtc(New ScriptEventArgs(Me, "StateChanged", objPacket, sctCancelToken))
                Me.RaiseStateChangedEvent(objPacket)
            Else
                CtcService.PostMessage(CtcService.MessageCat.Native, String.Format("[{0}] already set to [{1}].", Me.ToString, _enuState.ToString))
            End If

            Await Me.ScriptEventBinder.InvokeHandlersOnCtc(New ScriptEventArgs(Me, "StateReported", objPacket, sctCancelToken))
            Me.RaiseStateReportedEvent(objPacket)

        End Function

#End Region

#Region "Event Handling"

        ''' <summary>Runs on worker thread created by the Loconet service.</summary>
        Private Sub RxPacket(objPacket As Packet)
            'packets types we should examine
            Select Case True
                Case TypeOf objPacket Is PkInput AndAlso _enuObservePacket = ObservePacketType.InputRep
                    Dim objPkInput As PkInput = objPacket
                    If objPkInput.Address = _srtAddress Then
                        Me.SetState(objPkInput.State, objPkInput, Nothing)
                    End If

                Case TypeOf objPacket Is PkSwitchInput AndAlso _enuObservePacket = ObservePacketType.SwitchRep
                    Dim objPkSwitchInput As PkSwitchInput = objPacket
                    If objPkSwitchInput.Address = _srtAddress Then
                        Me.SetState(objPkSwitchInput.State, objPkSwitchInput, Nothing)
                    End If

                Case TypeOf objPacket Is PkMultiSense AndAlso _enuObservePacket = ObservePacketType.MultiSense
                    Dim objPkMultiSense As PkMultiSense = objPacket
                    If objPkMultiSense.DeviceAddress = _srtAddress Then
                        Select Case objPkMultiSense.Type
                            Case MultiSenseType.TransponderDetect
                                Me.SetState(OnOff.On, objPkMultiSense, Nothing)

                            Case MultiSenseType.TransponderRelease
                                Me.SetState(OnOff.Off, objPkMultiSense, Nothing)
                        End Select
                    End If

                Case TypeOf objPacket Is PkSecurityElem AndAlso _enuObservePacket = ObservePacketType.SecurityElem
                    Dim objPkSecurityElem As PkSecurityElem = objPacket
                    If objPkSecurityElem.Address = _srtAddress Then
                        Me.SetState(OnOff.Off, objPkSecurityElem, Nothing)
                    End If

                Case TypeOf objPacket Is PkLissy AndAlso _enuObservePacket = ObservePacketType.Lissy
                    Dim objPkLissy As PkLissy = objPacket
                    If objPkLissy.DevAddr = _srtAddress Then
                        Me.SetState(OnOff.Off, objPkLissy, Nothing)
                    End If

            End Select
        End Sub

#End Region

#Region "Standard Events"

        ''' <summary>Occurs after a phisical sensor has reported its state different than the current state of our logical sensor.</summary>
        Public Event StateChanged(objSender As Sensor, objPacket As Packet)

        ''' <summary>For internal event propagation to blocks.</summary>
        Friend Event StateChangedOnCtcThread(objPacket As Packet)

        ''' <summary>Runs on the CTC thread.</summary>
        Friend Sub RaiseStateChangedEvent(objPacket As Packet)

            RaiseEvent StateChangedOnCtcThread(objPacket)

            Select Case True
                Case CtcService.SyncContext Is Nothing
                    'raise event on a new worker thread
                    Dim objDelegate As New Action(Sub() RaiseEvent StateChanged(Me, objPacket))
                    objDelegate.BeginInvoke(Nothing, Nothing)

                Case CtcService.SyncContext.CheckAccess  'this should never happen because the CTC thread is unlikely to be the user provided thread
                    'raise event on the current thread
                    RaiseEvent StateChanged(Me, objPacket)

                Case Else
                    'raise event on the sync context thread
                    CtcService.SyncContext.InvokeAsync(Sub() RaiseEvent StateChanged(Me, objPacket))
            End Select

        End Sub

        '------

        ''' <summary>Occurs after a phisical sensor has reported its state, regardless of the current state of our logical sensor.</summary>
        Public Event StateReported(objSender As Sensor, objPacket As Packet)

        ''' <summary>Runs on the CTC thread.</summary>
        Friend Sub RaiseStateReportedEvent(objPacket As Packet)

            Select Case True
                Case CtcService.SyncContext Is Nothing
                    'raise event on a new worker thread
                    Dim objDelegate As New Action(Sub() RaiseEvent StateReported(Me, objPacket))
                    objDelegate.BeginInvoke(Nothing, Nothing)

                Case CtcService.SyncContext.CheckAccess  'this should never happen because this sub runs on the CTC thread
                    'raise event on the current thread
                    RaiseEvent StateReported(Me, objPacket)

                Case Else
                    'raise event on the sync context thread
                    CtcService.SyncContext.InvokeAsync(Sub() RaiseEvent StateReported(Me, objPacket))
            End Select

        End Sub

#End Region

#Region "Scripting Events"

        Private _objScriptEventBinder As New ScriptEventBinder(Me, {
            "StateChanged",
            "StateReported"
        })

        Public ReadOnly Property ScriptEventBinder As ScriptEventBinder Implements ISupportsScriptEvents.ScriptEventBinder
            Get
                Return _objScriptEventBinder
            End Get
        End Property

#End Region

    End Class

    ''' <summary>A switchboard signal object.</summary>
    <Serializable()> Public NotInheritable Class Signal
        Inherits CtcObjectBase
        Implements IPkStates, ITopLayer, ISupportsScriptEvents, ISwBrdOperable

        'serialized
        Private _sctLocation As Location
        Private _enuConfig As SignalConfig  'default this in the constructor so the Config property can be called
        Private _strLookKey As String
        Private _sinDrawScale As Single = 1
        Private _enuCellAlign As Alignment = Alignment.MiddleCenter
        Private _sinAlignOffsetX As Single = 0
        Private _sinAlignOffsetY As Single = 0
        Private _objStates As PkStatesList

        Public Sub New()
            Me.New(SignalConfig.StopClearCautionShunt, Nothing)
        End Sub

        Public Sub New(enuConfig As SignalConfig, strLookKey As String)
            _sctLocation = New Location(1, 1)
            _objStates = New PkStatesList(Me)
            _objStates._srtPostPkWait = 100
            Me.Config = enuConfig
            _strLookKey = strLookKey
        End Sub

#Region "Serialization"

        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
            Select Case CtcService.SerializationContent
                Case SerializationContent.Definition
                    _sctLocation = info.GetValue("Location", GetType(Object))
                    _enuConfig = info.GetValue("Config", GetType(Object))
                    _strLookKey = info.GetString("LookKey")
                    Try
                        'added new fields that broke compatibility
                        _sinDrawScale = info.GetValue("DrawScale", GetType(Object))
                        _enuCellAlign = info.GetValue("CellAlign", GetType(Object))
                        _sinAlignOffsetX = info.GetValue("AlignOffsetX", GetType(Object))
                        _sinAlignOffsetY = info.GetValue("AlignOffsetY", GetType(Object))
                    Catch
                        'compatibility added 07/09/12
                        CtcService.CompatibilityMode = True
                    End Try
                    _objStates = info.GetValue("States", GetType(Object))

                Case SerializationContent.State
                    Dim objSignal As Signal = CtcService.Signals(Me.ID)
                    If objSignal IsNot Nothing Then
                        objSignal.Aspects._bytActiveState = info.GetByte("CurrentState")
                        objSignal.Aspects._enuDisposition = info.GetValue("Disposition", GetType(Object))
                    End If
            End Select
        End Sub

        Friend Overrides Sub PostDeserialize()
            MyBase.PostDeserialize()
            Select Case CtcService.SerializationContent
                Case SerializationContent.Definition
                    _objStates.PostDeserialize()

                Case SerializationContent.State

            End Select
        End Sub

        Friend Overrides Sub Initialize()
            MyBase.Initialize()
            Select Case CtcService.SerializationContent
                Case SerializationContent.Definition
                    _objStates._objParent = Me          'parent relations are not serialized so they are being rebuilt here
                    _objStates.Initialize()

                Case SerializationContent.State

            End Select
            Me.InitStoredState()
        End Sub

        <SecurityPermission(SecurityAction.LinkDemand, Flags:=SecurityPermissionFlag.SerializationFormatter)> _
        Protected Overrides Sub GetObjectData(info As SerializationInfo, context As StreamingContext)
            MyBase.GetObjectData(info, context)
            Select Case CtcService.SerializationContent
                Case SerializationContent.Definition
                    info.AddValue("Location", _sctLocation)
                    info.AddValue("Config", _enuConfig)
                    info.AddValue("LookKey", _strLookKey)
                    info.AddValue("DrawScale", _sinDrawScale)
                    info.AddValue("CellAlign", _enuCellAlign)
                    info.AddValue("AlignOffsetX", _sinAlignOffsetX)
                    info.AddValue("AlignOffsetY", _sinAlignOffsetY)
                    info.AddValue("States", _objStates)
                Case SerializationContent.State
                    info.AddValue("CurrentState", _objStates._bytActiveState)
                    info.AddValue("Disposition", If(_objStates._enuDisposition = PkStatesList.StateDisposition.Pending, PkStatesList.StateDisposition.Set, _objStates._enuDisposition))
            End Select
        End Sub

#End Region

#Region "Enumerations"

        ''' <summary>Enumerates the available signal aspect configurations.</summary>
        Public Enum SignalConfig As Byte
            ''' <summary>Two aspect signal consisting of: <see cref="SignalAspect.Stop" /> and <see cref="SignalAspect.Clear" />.</summary>
            StopClear
            ''' <summary>Three aspect signal consisting of: <see cref="SignalAspect.Stop" />, <see cref="SignalAspect.Clear" />, and <see cref="SignalAspect.Caution" />.</summary>
            StopClearCaution
            ''' <summary>Four aspect signal consisting of: <see cref="SignalAspect.Stop" />, <see cref="SignalAspect.Clear" />, <see cref="SignalAspect.Caution" />, and <see cref="SignalAspect.ShuntOnly" />.</summary>
            StopClearCautionShunt
            ''' <summary>Four aspect signal consisting of: <see cref="SignalAspect.Dark" />, <see cref="SignalAspect.Stop" />, <see cref="SignalAspect.Clear" />, and <see cref="SignalAspect.Caution" />.</summary>
            DarkStopClearCaution
            ''' <summary>Five aspect signal consisting of: <see cref="SignalAspect.Dark" />, <see cref="SignalAspect.Stop" />, <see cref="SignalAspect.Clear" />, <see cref="SignalAspect.Caution" />, and <see cref="SignalAspect.ShuntOnly" />.</summary>
            DarkStopClearCautionShunt
            ''' <summary>Four aspect signal consisting of: <see cref="SignalAspect.Stop" />, <see cref="SignalAspect.Clear" />, <see cref="SignalAspect.Caution" />, and <see cref="SignalAspect.Caution2" />.</summary>
            StopClearCautionCaution2
        End Enum


        ''' <summary>Enumerates the available signal aspects.</summary>        
        Public Enum SignalAspect As Byte
            ''' <summary>Not applicable in current mast configuration.</summary>            
            ''' <remarks>All signal lights are off.</remarks>
            Dark
            ''' <summary>Do not proceed.</summary>
            ''' <remarks>Signal light is usually red.</remarks>
            [Stop]
            ''' <summary>Clear to proceed.</summary>
            ''' <remarks>Signal light is usually green.</remarks>
            Clear
            ''' <summary>Proceed at reduced speed.</summary>
            ''' <remarks>Signal light is usually yellow.</remarks>
            Caution
            ''' <summary>Do not proceed except for shunting movements.</summary>
            ''' <remarks>Signal light is usually white.</remarks>
            ShuntOnly
            ''' <summary>Secondary caution indicator. Proceed at reduced speed.</summary>
            ''' <remarks>Signal light is usually yellow.</remarks>
            Caution2
        End Enum

#End Region

#Region "Overrides"

        Public Overrides Sub DeleteSelf()
            MyBase.DeleteSelf()
            CtcService.EventScripts.RemoveEventsAssoc(Me)
            CtcService.Signals.Remove(Me)
        End Sub

        Public Overrides Function ToString() As String
            Return "Signal." & Me.Name
        End Function

#End Region

#Region "Configuration"

        ''' <summary>Gets or sets the location of the signal on the switchboard.</summary>
        ''' <remarks>The CtcService must be stopped otherwise a <see cref="CtcIsStartedException" /> will be thrown.</remarks>
        Public Property Location() As Location Implements ITopLayer.Location
            Get
                Return _sctLocation
            End Get
            Set(sctValue As Location)
                If CtcService.Signals.Contains(Me) Then CtcIsStartedException.Check()
                _sctLocation = sctValue
            End Set
        End Property

        ''' <summary>Gets or sets the aspect configuration of this signal.</summary>
        ''' <remarks>The CtcService must be stopped otherwise a <see cref="CtcIsStartedException" /> will be thrown.</remarks>
        Public Property Config() As SignalConfig
            Get
                Return _enuConfig
            End Get
            Set(Value As SignalConfig)
                If CtcService.Signals.Contains(Me) Then CtcIsStartedException.Check()

                Select Case Value
                    Case SignalConfig.StopClearCautionCaution2
                        _objStates.Update(New List(Of Byte)(New SignalAspect() {SignalAspect.Stop, SignalAspect.Clear, SignalAspect.Caution, SignalAspect.Caution2}))
                    Case SignalConfig.DarkStopClearCautionShunt
                        _objStates.Update(New List(Of Byte)(New SignalAspect() {SignalAspect.Dark, SignalAspect.Stop, SignalAspect.Clear, SignalAspect.Caution, SignalAspect.ShuntOnly}))
                    Case SignalConfig.StopClearCautionShunt
                        _objStates.Update(New List(Of Byte)(New SignalAspect() {SignalAspect.Stop, SignalAspect.Clear, SignalAspect.Caution, SignalAspect.ShuntOnly}))
                    Case SignalConfig.DarkStopClearCaution
                        _objStates.Update(New List(Of Byte)(New SignalAspect() {SignalAspect.Dark, SignalAspect.Stop, SignalAspect.Clear, SignalAspect.Caution}))
                    Case SignalConfig.StopClearCaution
                        _objStates.Update(New List(Of Byte)(New SignalAspect() {SignalAspect.Stop, SignalAspect.Clear, SignalAspect.Caution}))
                    Case SignalConfig.StopClear
                        _objStates.Update(New List(Of Byte)(New SignalAspect() {SignalAspect.Stop, SignalAspect.Clear}))
                End Select

                _enuConfig = Value
            End Set
        End Property

        ''' <summary>Arbitrary client key describing the visual look of this signal.</summary>
        ''' <remarks>
        ''' Internally this property is not used for anything but clients can store a value here describing the visual look of the
        ''' signal, assuming there is more than one. For example, signals with the same <see cref="Config" /> but a different localized look.
        ''' </remarks>
        Public Property LookKey() As String
            Get
                Return _strLookKey
            End Get
            Set(value As String)
                _strLookKey = value
            End Set
        End Property

        ''' <summary>Gets or sets the signal's drawing surface scale.</summary>
        ''' <value>Values are capped within: 0.1 to 10. The default scale is 1. For example 0.5 would be half normal size and 2 would be twice normal size.</value>
        ''' <remarks>This property must be set while the CtcService is stopped otherwise a <see cref="CtcIsStartedException" /> will be thrown.</remarks>
        Public Property DrawScale() As Single Implements ITopLayer.DrawScale
            Get
                Return _sinDrawScale
            End Get
            Set(Value As Single)
                If CtcService.Signals.Contains(Me) Then CtcIsStartedException.Check()
                _sinDrawScale = Math.Round(Math.Min(Math.Max(Value, 0.1), 10), 6)
            End Set
        End Property

        ''' <summary>Gets or sets the signal's drawing surface alignment in relation to its switchboard cell location.</summary>
        ''' <remarks>This property must be set while the CtcService is stopped otherwise a <see cref="CtcIsStartedException" /> will be thrown.</remarks>
        Public Property CellAlign() As Alignment Implements ITopLayer.CellAlign
            Get
                Return _enuCellAlign
            End Get
            Set(Value As Alignment)
                If CtcService.Signals.Contains(Me) Then CtcIsStartedException.Check()
                _enuCellAlign = Value
            End Set
        End Property

        ''' <summary>Gets or sets the horizontal offset of the signal's drawing surface from the position set by <see cref="CellAlign" />.</summary>
        ''' <remarks>This property must be set while the CtcService is stopped otherwise a <see cref="CtcIsStartedException" /> will be thrown.</remarks>
        Public Property AlignOffsetX() As Single Implements ITopLayer.AlignOffsetX
            Get
                Return _sinAlignOffsetX
            End Get
            Set(Value As Single)
                If CtcService.Signals.Contains(Me) Then CtcIsStartedException.Check()
                _sinAlignOffsetX = Value
            End Set
        End Property

        ''' <summary>Gets or sets the vertical offset of the signal's drawing surface from the position set by <see cref="CellAlign" />.</summary>
        ''' <remarks>This property must be set while the CtcService is stopped otherwise a <see cref="CtcIsStartedException" /> will be thrown.</remarks>
        Public Property AlignOffsetY() As Single Implements ITopLayer.AlignOffsetY
            Get
                Return _sinAlignOffsetY
            End Get
            Set(Value As Single)
                If CtcService.Signals.Contains(Me) Then CtcIsStartedException.Check()
                _sinAlignOffsetY = Value
            End Set
        End Property

        ''' <summary>Gets the aspects list configured by the <see cref="Config" /> property.</summary>
        Public ReadOnly Property Aspects() As PkStatesList Implements IPkStates.States
            Get
                Return _objStates
            End Get
        End Property

#End Region

#Region "Operation"

        ''' <summary>Action to be performd when a switchboard signal represenation is pressed by mouse or touch-point.</summary>
        ''' <param name="sctCancelToken">Token provided by a controlling source to notify this method that it should cancel its execution.</param>
        ''' <returns>An awaitable <see cref="Tasks.Task" /> returning <i>True</i> on success, otherwise <i>False</i>.</returns>
        ''' <remarks>The CtcService must be started otherwise a <see cref="CtcIsStoppedException" /> will be thrown.</remarks> 
        ''' <seealso cref="OperateRelease" /> 
        Public Function OperatePress(Optional sctCancelToken As CancellationToken = Nothing) As Task(Of Boolean) Implements ISwBrdOperable.Press

            If CtcService.IsCurrentThread Then
                Return OperatePressOnCtc(sctCancelToken)
            Else
                CtcIsStoppedException.Check()

                Dim objTcs As New TaskCompletionSource(Of Boolean)
                CtcService.Execute(Async Sub() objTcs.TrySetResult(Await OperatePressOnCtc(sctCancelToken)))
                Return objTcs.Task
            End If

        End Function

        Private Async Function OperatePressOnCtc(sctCancelToken As CancellationToken) As Task(Of Boolean)

            Dim objScriptEventArgs As New ScriptEventArgs(Me, "BeforeOperatePress", Nothing, sctCancelToken)
            Await Me.ScriptEventBinder.InvokeHandlersOnCtc(objScriptEventArgs)
            'since there is no native implementation for a signal press operation, we don't have to check for Canceled operation

            Return True

        End Function

        ''' <summary>Action to be performd when a switchboard signal represenation is released by mouse or touch-point.</summary>
        ''' <param name="sctCancelToken">Token provided by a controlling source to notify this method that it should cancel its execution.</param>
        ''' <returns>An awaitable <see cref="Tasks.Task" /> returning <i>True</i> on success, otherwise <i>False</i>.</returns>
        ''' <remarks>The CtcService must be started otherwise a <see cref="CtcIsStoppedException" /> will be thrown.</remarks>
        ''' <seealso cref="OperatePress" /> 
        Public Function OperateRelease(Optional sctCancelToken As CancellationToken = Nothing) As Task(Of Boolean) Implements ISwBrdOperable.Release

            If CtcService.IsCurrentThread Then
                Return OperateReleaseOnCtc(sctCancelToken)
            Else
                CtcIsStoppedException.Check()

                Dim objTcs As New TaskCompletionSource(Of Boolean)
                CtcService.Execute(Async Sub() objTcs.TrySetResult(Await OperateReleaseOnCtc(sctCancelToken)))
                Return objTcs.Task
            End If

        End Function

        Private Async Function OperateReleaseOnCtc(sctCancelToken As CancellationToken) As Task(Of Boolean)

            Dim objScriptEventArgs As New ScriptEventArgs(Me, "BeforeOperateRelease", Nothing, sctCancelToken)
            Await Me.ScriptEventBinder.InvokeHandlersOnCtc(objScriptEventArgs)
            If objScriptEventArgs.ReqCancel.Value Then
                CtcService.PostMessage(CtcService.MessageCat.Native, String.Format("Native operation for [{0}] was canceled by user script.", Me.ToString))
            Else
                Dim objState As PkStatesList.State = Me.Aspects.Next()
                If objState IsNot Nothing Then
                    Return Await Me.SetAspectOnCtc(objState, objState.Name, sctCancelToken)
                End If
            End If

            Return True

        End Function


        ''' <summary>Gets the signal's currently active aspect object.</summary>
        ''' <remarks>This is a short hand version of <i>Signal.Aspects.</i><see cref="PkStatesList.Active">Active</see>.</remarks>
        Public ReadOnly Property Aspect As PkStatesList.State Implements IPkStates.State
            Get
                Return Me.Aspects.Active
            End Get
        End Property


        ''' <summary>Sets the signal's active aspect by value.</summary>
        ''' <param name="bytAspectValue">A <see cref="SignalAspect" /> member that is valid for this signal configuration.</param>
        ''' <param name="sctCancelToken">Token provided by a controlling source to notify this method that it should cancel its execution.</param>
        ''' <returns>An awaitable <see cref="Tasks.Task" /> returning <i>True</i> on success, otherwise <i>False</i>.</returns>
        ''' <remarks>The CtcService must be started otherwise a <see cref="CtcIsStoppedException" /> will be thrown.</remarks>
        ''' <seealso cref="Signal.Aspects" /> 
        Public Function SetAspect(bytAspectValue As Byte, Optional sctCancelToken As CancellationToken = Nothing) As Task(Of Boolean) Implements IPkStates.SetState
            Return SetAspect(_objStates(bytAspectValue), DirectCast(bytAspectValue, SignalAspect).ToString, sctCancelToken)
        End Function

        ''' <summary>Sets the signal's active aspect by name.</summary>
        ''' <param name="strAspectName">The aspect's <see cref="PkStatesList.State.Name" />.</param>        
        ''' <param name="sctCancelToken">Token provided by a controlling source to notify this method that it should cancel its execution.</param>
        ''' <returns>An awaitable <see cref="Tasks.Task" /> returning <i>True</i> on success, otherwise <i>False</i>.</returns>
        ''' <remarks>The CtcService must be started otherwise a <see cref="CtcIsStoppedException" /> will be thrown.</remarks>
        ''' <seealso cref="Signal.Aspects" /> 
        Public Function SetAspect(strAspectName As String, Optional sctCancelToken As CancellationToken = Nothing) As Task(Of Boolean) Implements IPkStates.SetState
            Return SetAspect(_objStates(strAspectName), strAspectName, sctCancelToken)
        End Function

        ''' <summary>Sets the signal's active aspect by object.</summary>           
        ''' <param name="sctCancelToken">Token provided by a controlling source to notify this method that it should cancel its execution.</param>
        ''' <returns>An awaitable <see cref="Tasks.Task" /> returning <i>True</i> on success, otherwise <i>False</i>.</returns>
        ''' <remarks>The CtcService must be started otherwise a <see cref="CtcIsStoppedException" /> will be thrown.</remarks>
        ''' <seealso cref="Signal.Aspects" /> 
        Public Function SetAspect(objAspect As PkStatesList.State, Optional sctCancelToken As CancellationToken = Nothing) As Task(Of Boolean) Implements IPkStates.SetState
            Return SetAspect(objAspect, objAspect.Name, sctCancelToken)
        End Function


        Private Function SetAspect(objState As PkStatesList.State, strGivenKey As String, sctCancelToken As CancellationToken) As Task(Of Boolean)

            If CtcService.IsCurrentThread Then
                Return SetAspectOnCtc(objState, strGivenKey, sctCancelToken)
            Else
                CtcIsStoppedException.Check()

                Dim objTcs As New TaskCompletionSource(Of Boolean)
                CtcService.Execute(Async Sub() objTcs.TrySetResult(Await SetAspectOnCtc(objState, strGivenKey, sctCancelToken)))
                Return objTcs.Task
            End If

        End Function

        Private Async Function SetAspectOnCtc(objState As PkStatesList.State, strGivenKey As String, sctCancelToken As CancellationToken) As Task(Of Boolean)
            If Await _objStates.SetStateValidate(objState, strGivenKey, sctCancelToken) Then
                Return Await _objStates.SetStatePerform(objState, False, sctCancelToken)
            Else
                Return False
            End If
        End Function

#End Region

#Region "Scripting Events"

        Private _objScriptEventBinder As New ScriptEventBinder(Me, {
            "BeforeOperatePress",
            "BeforeOperateRelease"
        })

        Public ReadOnly Property ScriptEventBinder As ScriptEventBinder Implements ISupportsScriptEvents.ScriptEventBinder
            Get
                Return _objScriptEventBinder
            End Get
        End Property

#End Region

    End Class

    ''' <summary>An accessory object to be controled.</summary>
    <Serializable()> Public NotInheritable Class Accessory
        Inherits CtcObjectBase
        Implements IPkStates, ISupportsScriptEvents

        'serialized
        Private _objStates As PkStatesList
        Private _intStatesNamingCounter As Integer

        Public Sub New()
            _objStates = New PkStatesList(Me)            
        End Sub

#Region "Serialization"

        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
            Select Case CtcService.SerializationContent
                Case SerializationContent.Definition
                    _objStates = info.GetValue("States", GetType(Object))
                    _intStatesNamingCounter = info.GetInt32("StatesNamingCounter")

                Case SerializationContent.State
                    Dim objAccessory As Accessory = CtcService.Accessories(Me.ID)
                    If objAccessory IsNot Nothing Then
                        objAccessory.States._bytActiveState = info.GetByte("CurrentState")
                        objAccessory.States._enuDisposition = info.GetValue("Disposition", GetType(Object))
                    End If

            End Select
        End Sub

        Friend Overrides Sub PostDeserialize()
            MyBase.PostDeserialize()
            Select Case CtcService.SerializationContent
                Case SerializationContent.Definition
                    _objStates.PostDeserialize()

                Case SerializationContent.State

            End Select
        End Sub

        Friend Overrides Sub Initialize()
            MyBase.Initialize()
            Select Case CtcService.SerializationContent
                Case SerializationContent.Definition
                    _objStates._objParent = Me          'parent relations are not serialized so they are being rebuilt here
                    _objStates.Initialize()

                Case SerializationContent.State

            End Select
            Me.InitStoredState()
        End Sub

        <SecurityPermission(SecurityAction.LinkDemand, Flags:=SecurityPermissionFlag.SerializationFormatter)> _
        Protected Overrides Sub GetObjectData(info As SerializationInfo, context As StreamingContext)
            MyBase.GetObjectData(info, context)
            Select Case CtcService.SerializationContent
                Case SerializationContent.Definition
                    info.AddValue("States", _objStates)
                    info.AddValue("StatesNamingCounter", _intStatesNamingCounter)
                Case SerializationContent.State
                    info.AddValue("CurrentState", _objStates._bytActiveState)
                    info.AddValue("Disposition", If(_objStates._enuDisposition = PkStatesList.StateDisposition.Pending, PkStatesList.StateDisposition.Set, _objStates._enuDisposition))
            End Select
        End Sub

#End Region

#Region "Overrides"

        Public Overrides Sub DeleteSelf()
            MyBase.DeleteSelf()
            CtcService.EventScripts.RemoveEventsAssoc(Me)
            CtcService.Accessories.Remove(Me)
        End Sub

        Public Overrides Function ToString() As String
            Return "Accessory." & Me.Name
        End Function

#End Region

#Region "Configuration"

        ''' <summary>Gets the states list.</summary>
        Public ReadOnly Property States() As PkStatesList Implements IPkStates.States
            Get
                Return _objStates
            End Get
        End Property

        Public Sub AddState()
            If CtcService.Accessories.Contains(Me) Then CtcIsStartedException.Check()
            _intStatesNamingCounter += 1
            Dim objState As New PkStatesList.State(_intStatesNamingCounter, Me)
            With _objStates
                ._objStatesList.Add(objState)
                ._srtPostPkWait = 100
                If ._bytDefaultState = 0 Then ._bytDefaultState = objState.Value
                If ._bytActiveState = 0 Then ._bytActiveState = ._bytDefaultState
            End With
        End Sub

        Public Sub RemoveState(objState As PkStatesList.State)
            If CtcService.Accessories.Contains(Me) Then CtcIsStartedException.Check()
            _objStates._objStatesList.Remove(objState)
            If objState.Value = _objStates._bytDefaultState Then
                If _objStates.Count > 0 Then
                    _objStates._bytDefaultState = _objStates(0).Value
                Else
                    _objStates._bytDefaultState = 0
                End If
            End If
            If objState.Value = _objStates._bytActiveState Then
                If _objStates.Count > 0 Then
                    _objStates._bytActiveState = _objStates(0).Value
                Else
                    _objStates._bytActiveState = 0
                End If
            End If
        End Sub

        Public Sub ClearStates()
            If CtcService.Accessories.Contains(Me) Then CtcIsStartedException.Check()
            With _objStates
                ._objStatesList.Clear()
                ._bytDefaultState = 0
                ._bytActiveState = 0
            End With
        End Sub

#End Region

#Region "Operation"

        ''' <summary>Gets the accessory's currently active state object.</summary>
        ''' <remarks>This is a short hand version of <i>Accessory.States.</i><see cref="PkStatesList.Active">Active</see>.</remarks>
        Public ReadOnly Property State As PkStatesList.State Implements IPkStates.State
            Get
                Return Me.States.Active
            End Get
        End Property


        ''' <summary>Sets the accessory's active state by value.</summary>
        ''' <param name="bytStateValue">The state's unique internal counter <see cref="PkStatesList.State.Value" />.</param>
        ''' <param name="sctCancelToken">Token provided by a controlling source to notify this method that it should cancel its execution.</param>
        ''' <returns>An awaitable <see cref="Tasks.Task" /> returning <i>True</i> on success, otherwise <i>False</i>.</returns>    
        ''' <remarks>The CtcService must be started otherwise a <see cref="CtcIsStoppedException" /> will be thrown.</remarks>
        ''' <seealso cref="Accessory.States" /> 
        Public Function SetState(bytStateValue As Byte, Optional sctCancelToken As CancellationToken = Nothing) As Task(Of Boolean) Implements IPkStates.SetState
            Return SetState(_objStates(bytStateValue), bytStateValue, sctCancelToken)
        End Function

        ''' <summary>Sets the accessory's active state by name.</summary>
        ''' <param name="strStateName">The state's <see cref="PkStatesList.State.Name" />.</param>        
        ''' <param name="sctCancelToken">Token provided by a controlling source to notify this method that it should cancel its execution.</param>
        ''' <returns>An awaitable <see cref="Tasks.Task" /> returning <i>True</i> on success, otherwise <i>False</i>.</returns>   
        ''' <remarks>The CtcService must be started otherwise a <see cref="CtcIsStoppedException" /> will be thrown.</remarks>
        ''' <seealso cref="Accessory.States" /> 
        Public Function SetState(strStateName As String, Optional sctCancelToken As CancellationToken = Nothing) As Task(Of Boolean) Implements IPkStates.SetState
            Return SetState(_objStates(strStateName), strStateName, sctCancelToken)
        End Function

        ''' <summary>Sets the accessory's active state by object.</summary>           
        ''' <param name="sctCancelToken">Token provided by a controlling source to notify this method that it should cancel its execution.</param>
        ''' <returns>An awaitable <see cref="Tasks.Task" /> returning <i>True</i> on success, otherwise <i>False</i>.</returns>   
        ''' <remarks>The CtcService must be started otherwise a <see cref="CtcIsStoppedException" /> will be thrown.</remarks>
        ''' <seealso cref="Accessory.States" /> 
        Public Function SetState(objState As PkStatesList.State, Optional sctCancelToken As CancellationToken = Nothing) As Task(Of Boolean) Implements IPkStates.SetState
            Return SetState(objState, objState.Name, sctCancelToken)
        End Function


        Private Function SetState(objState As PkStatesList.State, strGivenKey As String, sctCancelToken As CancellationToken) As Task(Of Boolean)

            If CtcService.IsCurrentThread Then
                Return SetStateOnCtc(objState, strGivenKey, sctCancelToken)
            Else
                CtcIsStoppedException.Check()

                Dim objTcs As New TaskCompletionSource(Of Boolean)
                CtcService.Execute(Async Sub() objTcs.TrySetResult(Await SetStateOnCtc(objState, strGivenKey, sctCancelToken)))
                Return objTcs.Task
            End If

        End Function

        Private Async Function SetStateOnCtc(objState As PkStatesList.State, strGivenKey As String, sctCancelToken As CancellationToken) As Task(Of Boolean)
            If Await _objStates.SetStateValidate(objState, strGivenKey, sctCancelToken) Then
                Return Await _objStates.SetStatePerform(objState, False, sctCancelToken)
            Else
                Return False
            End If
        End Function

#End Region

#Region "Scripting Events"

        Private _objScriptEventBinder As New ScriptEventBinder(Me, {})

        Public ReadOnly Property ScriptEventBinder As ScriptEventBinder Implements ISupportsScriptEvents.ScriptEventBinder
            Get
                Return _objScriptEventBinder
            End Get
        End Property

#End Region

    End Class

    ''' <summary>An switchboard text label.</summary>
    <Serializable()> Public NotInheritable Class Label
        Inherits CtcObjectBase
        Implements ITopLayer, ISupportsScriptEvents, ISwBrdOperable
        
        'serialized
        Private _sctLocation As Location
        Private _strFontFamily As String = "Arial"
        Private _sinTextPadding As Single = 3
        Private _strTextColor As String = "#FFFFFFFF"   'white; used a non structure specific color representation 
        Private _strBackColor As String = "#FF000000"   'black; used a non structure specific color representation 
        Private _sinBackCornerRadius As Single = 5
        Private _sinDrawScale As Single = 1
        Private _enuCellAlign As Alignment = Alignment.MiddleCenter
        Private _sinAlignOffsetX As Single = 0
        Private _sinAlignOffsetY As Single = 0
        Private _strStaticText As String
        Private _strDynamicText As String

        Public Sub New()
            _sctLocation = New Location(1, 1)
        End Sub

#Region "Serialization"

        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
            Select Case CtcService.SerializationContent
                Case SerializationContent.Definition
                    _sctLocation = info.GetValue("Location", GetType(Object))
                    _strFontFamily = info.GetValue("FontFamily", GetType(Object))
                    _sinTextPadding = info.GetValue("TextPadding", GetType(Object))
                    _strTextColor = info.GetValue("TextColor", GetType(Object))
                    _strBackColor = info.GetValue("BackColor", GetType(Object))
                    _sinBackCornerRadius = info.GetValue("BackCornerRadius", GetType(Object))
                    Try
                        _sinDrawScale = info.GetValue("DrawScale", GetType(Object))
                    Catch ex As Exception
                        'compatibility added 09/07/12
                        CtcService.CompatibilityMode = True
                    End Try
                    Try
                        'added the new offset fields and renamed TextAlign to CellAlign
                        _enuCellAlign = info.GetValue("CellAlign", GetType(Object))
                        _sinAlignOffsetX = info.GetValue("AlignOffsetX", GetType(Object))
                        _sinAlignOffsetY = info.GetValue("AlignOffsetY", GetType(Object))
                    Catch ex As Exception
                        _enuCellAlign = info.GetValue("TextAlign", GetType(Object))

                        'compatibility added 07/09/12
                        CtcService.CompatibilityMode = True
                    End Try
                    _strStaticText = info.GetValue("StaticText", GetType(Object))

                Case SerializationContent.State
                    Dim objLabel As Label = CtcService.Labels(Me.ID)
                    If objLabel IsNot Nothing Then
                        objLabel._strDynamicText = info.GetValue("DynamicText", GetType(Object))
                    End If
            End Select
        End Sub

        Friend Overrides Sub Initialize()
            MyBase.Initialize()

            Me.InitStoredState()    'for both serialization contents
        End Sub

        <SecurityPermission(SecurityAction.LinkDemand, Flags:=SecurityPermissionFlag.SerializationFormatter)>
        Protected Overrides Sub GetObjectData(info As SerializationInfo, context As StreamingContext)
            MyBase.GetObjectData(info, context)
            Select Case CtcService.SerializationContent
                Case SerializationContent.Definition
                    info.AddValue("Location", _sctLocation)
                    info.AddValue("FontFamily", _strFontFamily)
                    info.AddValue("TextPadding", _sinTextPadding)
                    info.AddValue("TextColor", _strTextColor)
                    info.AddValue("BackColor", _strBackColor)
                    info.AddValue("BackCornerRadius", _sinBackCornerRadius)
                    info.AddValue("DrawScale", _sinDrawScale)
                    info.AddValue("CellAlign", _enuCellAlign)
                    info.AddValue("AlignOffsetX", _sinAlignOffsetX)
                    info.AddValue("AlignOffsetY", _sinAlignOffsetY)
                    info.AddValue("StaticText", _strStaticText)
                Case SerializationContent.State
                    info.AddValue("DynamicText", _strDynamicText)
            End Select
        End Sub

#End Region

#Region "Overrides"

        Public Overrides Sub DeleteSelf()
            MyBase.DeleteSelf()
            CtcService.Labels.Remove(Me)
        End Sub

        Public Overrides Function ToString() As String
            Return "Label." & Me.Name
        End Function

#End Region

#Region "Configuration"

        ''' <summary>Gets or sets the location of the label on the switchboard.</summary>
        ''' <remarks>This property must be set while the CtcService is stopped otherwise a <see cref="CtcIsStartedException" /> will be thrown.</remarks>
        Public Property Location() As Location Implements ITopLayer.Location
            Get
                Return _sctLocation
            End Get
            Set(Value As Location)
                If CtcService.Labels.Contains(Me) Then CtcIsStartedException.Check()
                _sctLocation = Value
            End Set
        End Property

        ''' <summary>Gets or sets the font name used to render the label's text.</summary>
        ''' <remarks>This property must be set while the CtcService is stopped otherwise a <see cref="CtcIsStartedException" /> will be thrown.</remarks>
        Public Property FontFamily() As String
            Get
                Return _strFontFamily
            End Get
            Set(Value As String)
                If CtcService.Labels.Contains(Me) Then CtcIsStartedException.Check()
                _strFontFamily = Value
            End Set
        End Property

        ''' <summary>Gets or sets the padding size around the label's text.</summary>
        ''' <remarks>This property must be set while the CtcService is stopped otherwise a <see cref="CtcIsStartedException" /> will be thrown.</remarks>
        Public Property TextPadding() As Single
            Get
                Return _sinTextPadding
            End Get
            Set(Value As Single)
                If CtcService.Labels.Contains(Me) Then CtcIsStartedException.Check()
                If Value < 0 Or Value > 200 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid text padding. Valid values are decimals 0-200.")
                _sinTextPadding = Value
            End Set
        End Property

        ''' <summary>Gets or sets the 32-bit ARGB color used to render the label's text.</summary>
        ''' <remarks>This property must be set while the CtcService is stopped otherwise a <see cref="CtcIsStartedException" /> will be thrown.</remarks>
        Public Property TextColor() As String
            Get
                Return _strTextColor
            End Get
            Set(Value As String)
                If CtcService.Labels.Contains(Me) Then CtcIsStartedException.Check()
                _strTextColor = Value
            End Set
        End Property

        ''' <summary>Gets or sets the 32-bit ARGB color used to render the text's background.</summary>
        ''' <remarks>This property must be set while the CtcService is stopped otherwise a <see cref="CtcIsStartedException" /> will be thrown.</remarks>
        Public Property BackColor() As String
            Get
                Return _strBackColor
            End Get
            Set(Value As String)
                If CtcService.Labels.Contains(Me) Then CtcIsStartedException.Check()
                _strBackColor = Value
            End Set
        End Property

        ''' <summary>Gets or sets the text's background corner radius size.</summary>
        ''' <value>Use 0 for no rounded corners.</value>
        ''' <remarks>This property must be set while the CtcService is stopped otherwise a <see cref="CtcIsStartedException" /> will be thrown.</remarks>
        Public Property BackCornerRadius() As Single
            Get
                Return _sinBackCornerRadius
            End Get
            Set(Value As Single)
                If CtcService.Labels.Contains(Me) Then CtcIsStartedException.Check()
                If Value < 0 Or Value > 800 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid corner radius. Valid values are decimals 0-800.")
                _sinBackCornerRadius = Value
            End Set
        End Property

        ''' <summary>Gets or sets the label's drawing surface scale.</summary>
        ''' <value>Values are capped within: 0.1 to 10. The default scale is 1. For example 0.5 would be half normal size and 2 would be twice normal size.</value>
        ''' <remarks>This property must be set while the CtcService is stopped otherwise a <see cref="CtcIsStartedException" /> will be thrown.</remarks>
        Public Property DrawScale() As Single Implements ITopLayer.DrawScale
            Get
                Return _sinDrawScale
            End Get
            Set(Value As Single)
                If CtcService.Labels.Contains(Me) Then CtcIsStartedException.Check()
                _sinDrawScale = Math.Round(Math.Min(Math.Max(Value, 0.1), 10), 6)
            End Set
        End Property

        ''' <summary>Gets or sets the label's drawing surface alignment in relation to its switchboard cell location.</summary>
        ''' <remarks>This property must be set while the CtcService is stopped otherwise a <see cref="CtcIsStartedException" /> will be thrown.</remarks>
        Public Property CellAlign() As Alignment Implements ITopLayer.CellAlign
            Get
                Return _enuCellAlign
            End Get
            Set(Value As Alignment)
                If CtcService.Labels.Contains(Me) Then CtcIsStartedException.Check()
                _enuCellAlign = Value
            End Set
        End Property

        ''' <summary>Gets or sets the horizontal offset of the label's drawing surface from the position set by <see cref="CellAlign" />.</summary>
        ''' <remarks>This property must be set while the CtcService is stopped otherwise a <see cref="CtcIsStartedException" /> will be thrown.</remarks>
        Public Property AlignOffsetX() As Single Implements ITopLayer.AlignOffsetX
            Get
                Return _sinAlignOffsetX
            End Get
            Set(Value As Single)
                If CtcService.Labels.Contains(Me) Then CtcIsStartedException.Check()
                _sinAlignOffsetX = Value
            End Set
        End Property

        ''' <summary>Gets or sets the vertical offset of the label's drawing surface from the position set by <see cref="CellAlign" />.</summary>
        ''' <remarks>This property must be set while the CtcService is stopped otherwise a <see cref="CtcIsStartedException" /> will be thrown.</remarks>
        Public Property AlignOffsetY() As Single Implements ITopLayer.AlignOffsetY
            Get
                Return _sinAlignOffsetY
            End Get
            Set(Value As Single)
                If CtcService.Labels.Contains(Me) Then CtcIsStartedException.Check()
                _sinAlignOffsetY = Value
            End Set
        End Property

        ''' <summary>Gets or sets the label's text to be shown if the <see cref="DynamicText" /> is blank.</summary>
        ''' <remarks>This property must be set while the CtcService is stopped otherwise a <see cref="CtcIsStartedException" /> will be thrown.</remarks>
        Public Property StaticText() As String
            Get
                Return _strStaticText
            End Get
            Set(Value As String)
                If CtcService.Labels.Contains(Me) Then CtcIsStartedException.Check()
                _strStaticText = Value
            End Set
        End Property

        ''' <summary>Resets the label to its default static text.</summary>
        ''' <remarks>This property must be set while the CtcService is stopped otherwise a <see cref="CtcIsStartedException" /> will be thrown.</remarks>
        Public Sub Reset()
            If CtcService.Labels.Contains(Me) Then CtcIsStartedException.Check()
            _strDynamicText = Nothing
        End Sub

        ''' <summary>Gets the effective text shown by the label.</summary>
        ''' <value>Returns the <see cref="DynamicText" />if not blank, otherwise returns the <see cref="StaticText" />.</value>
        Public ReadOnly Property Text() As String
            Get
                Return If(_strDynamicText = Nothing, _strStaticText, _strDynamicText)
            End Get
        End Property

#End Region

#Region "Operation"

        ''' <summary>Action to be performd when a switchboard label represenation is pressed by mouse or touch-point.</summary>
        ''' <param name="sctCancelToken">Token provided by a controlling source to notify this method that it should cancel its execution.</param>
        ''' <returns>An awaitable <see cref="Tasks.Task" /> returning <i>True</i> on success, otherwise <i>False</i>.</returns>
        ''' <remarks>The CtcService must be started otherwise a <see cref="CtcIsStoppedException" /> will be thrown.</remarks>
        ''' <seealso cref="OperateRelease" /> 
        Public Function OperatePress(Optional sctCancelToken As CancellationToken = Nothing) As Task(Of Boolean) Implements ISwBrdOperable.Press

            If CtcService.IsCurrentThread Then
                Return OperatePressOnCtc(sctCancelToken)
            Else
                CtcIsStoppedException.Check()

                Dim objTcs As New TaskCompletionSource(Of Boolean)
                CtcService.Execute(Async Sub() objTcs.TrySetResult(Await OperatePressOnCtc(sctCancelToken)))
                Return objTcs.Task
            End If

        End Function

        Private Async Function OperatePressOnCtc(sctCancelToken As CancellationToken) As Task(Of Boolean)

            Dim objScriptEventArgs As New ScriptEventArgs(Me, "BeforeOperatePress", Nothing, sctCancelToken)
            Await Me.ScriptEventBinder.InvokeHandlersOnCtc(objScriptEventArgs)
            'since there is no native implementation for a label press operation, we don't have to check for Canceled operation

            Return True

        End Function

        ''' <summary>Action to be performd when a switchboard label represenation is released by mouse or touch-point.</summary>
        ''' <param name="sctCancelToken">Token provided by a controlling source to notify this method that it should cancel its execution.</param>
        ''' <returns>An awaitable <see cref="Tasks.Task" /> returning <i>True</i> on success, otherwise <i>False</i>.</returns>
        ''' <remarks>The CtcService must be started otherwise a <see cref="CtcIsStoppedException" /> will be thrown.</remarks> 
        ''' <seealso cref="OperatePress" /> 
        Public Function OperateRelease(Optional sctCancelToken As CancellationToken = Nothing) As Task(Of Boolean) Implements ISwBrdOperable.Release

            If CtcService.IsCurrentThread Then
                Return OperateReleaseOnCtc(sctCancelToken)
            Else
                CtcIsStoppedException.Check()

                Dim objTcs As New TaskCompletionSource(Of Boolean)
                CtcService.Execute(Async Sub() objTcs.TrySetResult(Await OperateReleaseOnCtc(sctCancelToken)))
                Return objTcs.Task
            End If

        End Function

        Private Async Function OperateReleaseOnCtc(sctCancelToken As CancellationToken) As Task(Of Boolean)

            Dim objScriptEventArgs As New ScriptEventArgs(Me, "BeforeOperateRelease", Nothing, sctCancelToken)
            Await Me.ScriptEventBinder.InvokeHandlersOnCtc(objScriptEventArgs)
            'since there is no native implementation for a label release operation, we don't have to check for Canceled operation

            Return True

        End Function


        ''' <summary>Gets or sets the label's text that should override the <see cref="StaticText" />.</summary>
        ''' <remarks>This property must be set while the CtcService is running otherwise a <see cref="CtcIsStoppedException" /> will be thrown.</remarks>
        Public Property DynamicText() As String
            Get
                Return _strDynamicText
            End Get
            Set(Value As String)
                If CtcService.IsCurrentThread Then
                    _strDynamicText = Value
                    Me.NotifyObjectChanged(Me)
                Else
                    CtcIsStoppedException.Check()

                    CtcService.Execute(Sub()
                                           _strDynamicText = Value
                                           Me.NotifyObjectChanged(Me)
                                       End Sub)
                End If
            End Set
        End Property

#End Region

#Region "Scripting Events"

        Private _objScriptEventBinder As New ScriptEventBinder(Me, {
            "BeforeOperatePress",
            "BeforeOperateRelease"
        })

        Public ReadOnly Property ScriptEventBinder As ScriptEventBinder Implements ISupportsScriptEvents.ScriptEventBinder
            Get
                Return _objScriptEventBinder
            End Get
        End Property

#End Region

    End Class

    ''' <summary>An switchboard button.</summary>
    <Serializable()> Public NotInheritable Class Button
        Inherits CtcObjectBase
        Implements ITopLayer, ISupportsScriptEvents, ISwBrdOperable

        'serialized
        Private _sctLocation As Location
        Private _enuBehavior As ButtonBehavior = ButtonBehavior.ManualState
        Private _enuLook As ButtonLook = ButtonLook.PushGreen
        Private _sinDrawScale As Single = 1
        Private _enuCellAlign As Alignment = Alignment.MiddleCenter
        Private _sinAlignOffsetX As Single = 0
        Private _sinAlignOffsetY As Single = 0
        Private _enuState As OnOff = OnOff.Off

        Public Sub New()
            _sctLocation = New Location(1, 1)
        End Sub

#Region "Serialization"

        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
            Select Case CtcService.SerializationContent
                Case SerializationContent.Definition
                    _sctLocation = info.GetValue("Location", GetType(Object))

                    Try
                        _enuBehavior = info.GetValue("Behavior", GetType(Object))
                    Catch
                        _enuBehavior = info.GetValue("Type", GetType(Object))

                        'compatibility added 2/19/13
                        CtcService.CompatibilityMode = True
                    End Try

                    Try
                        _enuLook = info.GetValue("Look", GetType(Object))
                    Catch
                        'compatibility added 2/19/13
                        CtcService.CompatibilityMode = True
                    End Try

                    _sinDrawScale = info.GetValue("DrawScale", GetType(Object))
                    _enuCellAlign = info.GetValue("CellAlign", GetType(Object))
                    _sinAlignOffsetX = info.GetValue("AlignOffsetX", GetType(Object))
                    _sinAlignOffsetY = info.GetValue("AlignOffsetY", GetType(Object))

                Case SerializationContent.State
                    Dim objButton As Button = CtcService.Buttons(Me.ID)
                    If objButton IsNot Nothing Then
                        objButton._enuState = info.GetValue("State", GetType(Object))
                    End If
            End Select
        End Sub

        Friend Overrides Sub Initialize()
            MyBase.Initialize()

            Me.InitStoredState()    'for both serialization contents
        End Sub

        <SecurityPermission(SecurityAction.LinkDemand, Flags:=SecurityPermissionFlag.SerializationFormatter)> _
        Protected Overrides Sub GetObjectData(info As SerializationInfo, context As StreamingContext)
            MyBase.GetObjectData(info, context)
            Select Case CtcService.SerializationContent
                Case SerializationContent.Definition
                    info.AddValue("Location", _sctLocation)
                    info.AddValue("Behavior", _enuBehavior)
                    info.AddValue("Look", _enuLook)
                    info.AddValue("DrawScale", _sinDrawScale)
                    info.AddValue("CellAlign", _enuCellAlign)
                    info.AddValue("AlignOffsetX", _sinAlignOffsetX)
                    info.AddValue("AlignOffsetY", _sinAlignOffsetY)
                Case SerializationContent.State
                    info.AddValue("State", _enuState)
            End Select
        End Sub

#End Region

#Region "Enumerations"

        ''' <summary>Operation button behavior.</summary>
        Public Enum ButtonBehavior As Byte
            ''' <summary>State is not set by the button. Optionally the state can be set through script.</summary>
            ManualState
            ''' <summary>State is automatically set <i>On</i> when the button is pushed and <i>Off</i> when released.</summary>
            PushState
            ''' <summary>State is automatically toggled when the button is released.</summary>
            ToggleState
        End Enum

        ''' <summary>Switchboard button look.</summary>
        Public Enum ButtonLook As Byte
            PushGreen = 0
            PushBlue
            PushRed
            PushYellow
            'ToggleGreen = 20  'left room for adding other Push* colors since the byte not the string of the enum is serialized
            'ToggleBlue
            'ToggleRed
            'ToggleYellow
        End Enum

#End Region

#Region "Overrides"

        Public Overrides Sub DeleteSelf()
            MyBase.DeleteSelf()
            'remove all references to this sensor
            CtcService.EventScripts.RemoveEventsAssoc(Me)
            CtcService.Buttons.Remove(Me)
        End Sub

        Public Overrides Function ToString() As String
            Return "Button." & Me.Name
        End Function

#End Region

#Region "Configuration"

        ''' <summary>Gets or sets the location of the button on the switchboard.</summary>
        ''' <remarks>This property must be set while the CtcService is stopped otherwise a <see cref="CtcIsStartedException" /> will be thrown.</remarks>
        Public Property Location() As Location Implements ITopLayer.Location
            Get
                Return _sctLocation
            End Get
            Set(Value As Location)
                If CtcService.Buttons.Contains(Me) Then CtcIsStartedException.Check()
                _sctLocation = Value
            End Set
        End Property

        ''' <summary>Gets or sets the button's state behavior.</summary>
        ''' <remarks>This property must be set while the CtcService is stopped otherwise a <see cref="CtcIsStartedException" /> will be thrown.</remarks>
        Public Property Behavior() As ButtonBehavior
            Get
                Return _enuBehavior
            End Get
            Set(Value As ButtonBehavior)
                If CtcService.Buttons.Contains(Me) Then CtcIsStartedException.Check()
                _enuBehavior = Value
            End Set
        End Property

        ''' <summary>Gets or sets the button's switchboard look.</summary>
        ''' <remarks>This property must be set while the CtcService is stopped otherwise a <see cref="CtcIsStartedException" /> will be thrown.</remarks>
        Public Property Look() As ButtonLook
            Get
                Return _enuLook
            End Get
            Set(Value As ButtonLook)
                If CtcService.Buttons.Contains(Me) Then CtcIsStartedException.Check()
                _enuLook = Value
            End Set
        End Property

        ''' <summary>Gets or sets the button's drawing surface scale.</summary>
        ''' <value>Values are capped within: 0.1 to 10. The default scale is 1. For example 0.5 would be half normal size and 2 would be twice normal size.</value>
        ''' <remarks>This property must be set while the CtcService is stopped otherwise a <see cref="CtcIsStartedException" /> will be thrown.</remarks>
        Public Property DrawScale() As Single Implements ITopLayer.DrawScale
            Get
                Return _sinDrawScale
            End Get
            Set(Value As Single)
                If CtcService.Buttons.Contains(Me) Then CtcIsStartedException.Check()
                _sinDrawScale = Math.Round(Math.Min(Math.Max(Value, 0.1), 10), 6)
            End Set
        End Property

        ''' <summary>Gets or sets the button's drawing surface alignment in relation to its switchboard cell location.</summary>
        ''' <remarks>This property must be set while the CtcService is stopped otherwise a <see cref="CtcIsStartedException" /> will be thrown.</remarks>
        Public Property CellAlign() As Alignment Implements ITopLayer.CellAlign
            Get
                Return _enuCellAlign
            End Get
            Set(Value As Alignment)
                If CtcService.Buttons.Contains(Me) Then CtcIsStartedException.Check()
                _enuCellAlign = Value
            End Set
        End Property

        ''' <summary>Gets or sets the horizontal offset of the button's drawing surface from the position set by <see cref="CellAlign" />.</summary>
        ''' <remarks>This property must be set while the CtcService is stopped otherwise a <see cref="CtcIsStartedException" /> will be thrown.</remarks>
        Public Property AlignOffsetX() As Single Implements ITopLayer.AlignOffsetX
            Get
                Return _sinAlignOffsetX
            End Get
            Set(Value As Single)
                If CtcService.Buttons.Contains(Me) Then CtcIsStartedException.Check()
                _sinAlignOffsetX = Value
            End Set
        End Property

        ''' <summary>Gets or sets the vertical offset of the button's drawing surface from the position set by <see cref="CellAlign" />.</summary>
        ''' <remarks>This property must be set while the CtcService is stopped otherwise a <see cref="CtcIsStartedException" /> will be thrown.</remarks>
        Public Property AlignOffsetY() As Single Implements ITopLayer.AlignOffsetY
            Get
                Return _sinAlignOffsetY
            End Get
            Set(Value As Single)
                If CtcService.Buttons.Contains(Me) Then CtcIsStartedException.Check()
                _sinAlignOffsetY = Value
            End Set
        End Property

        ''' <summary>Resets the button to the default <see cref="OnOff.Off" /> state.</summary>
        ''' <remarks>This property must be set while the CtcService is stopped otherwise a <see cref="CtcIsStartedException" /> will be thrown.</remarks>
        Public Sub Reset()
            If CtcService.Buttons.Contains(Me) Then CtcIsStartedException.Check()
            _enuState = OnOff.Off
        End Sub

#End Region

#Region "Operation"

        ''' <summary>Action to be performd when a switchboard button represenation is pressed by mouse or touch-point.</summary>
        ''' <param name="sctCancelToken">Token provided by a controlling source to notify this method that it should cancel its execution.</param>
        ''' <returns>An awaitable <see cref="Tasks.Task" /> returning <i>True</i> on success, otherwise <i>False</i>.</returns>
        ''' <remarks>The CtcService must be started otherwise a <see cref="CtcIsStoppedException" /> will be thrown.</remarks>
        ''' <seealso cref="OperateRelease" /> 
        Public Function OperatePress(Optional sctCancelToken As CancellationToken = Nothing) As Task(Of Boolean) Implements ISwBrdOperable.Press

            If CtcService.IsCurrentThread Then
                Return OperatePressOnCtc(sctCancelToken)
            Else
                CtcIsStoppedException.Check()

                Dim objTcs As New TaskCompletionSource(Of Boolean)
                CtcService.Execute(Async Sub() objTcs.TrySetResult(Await OperatePressOnCtc(sctCancelToken)))
                Return objTcs.Task
            End If

        End Function

        Private Async Function OperatePressOnCtc(sctCancelToken As CancellationToken) As Task(Of Boolean)

            Dim objScriptEventArgs As New ScriptEventArgs(Me, "BeforeOperatePress", Nothing, sctCancelToken)
            Await Me.ScriptEventBinder.InvokeHandlersOnCtc(objScriptEventArgs)
            If objScriptEventArgs.ReqCancel.Value Then
                CtcService.PostMessage(CtcService.MessageCat.Native, String.Format("Native operation for [{0}] was canceled by user script.", Me.ToString))
            Else
                Await Me.OperateOnCtc(True, sctCancelToken)
            End If

            Return True

        End Function

        ''' <summary>Action to be performd when a switchboard button represenation is released by mouse or touch-point.</summary>
        ''' <param name="sctCancelToken">Token provided by a controlling source to notify this method that it should cancel its execution.</param>
        ''' <returns>An awaitable <see cref="Tasks.Task" /> returning <i>True</i> on success, otherwise <i>False</i>.</returns>
        ''' <remarks>The CtcService must be started otherwise a <see cref="CtcIsStoppedException" /> will be thrown.</remarks>
        ''' <seealso cref="OperatePress" /> 
        Public Function OperateRelease(Optional sctCancelToken As CancellationToken = Nothing) As Task(Of Boolean) Implements ISwBrdOperable.Release

            If CtcService.IsCurrentThread Then
                Return OperateReleaseOnCtc(sctCancelToken)
            Else
                CtcIsStoppedException.Check()

                Dim objTcs As New TaskCompletionSource(Of Boolean)
                CtcService.Execute(Async Sub() objTcs.TrySetResult(Await OperateReleaseOnCtc(sctCancelToken)))
                Return objTcs.Task
            End If

        End Function

        Private Async Function OperateReleaseOnCtc(sctCancelToken As CancellationToken) As Task(Of Boolean)

            Dim objScriptEventArgs As New ScriptEventArgs(Me, "BeforeOperateRelease", Nothing, sctCancelToken)
            Await Me.ScriptEventBinder.InvokeHandlersOnCtc(objScriptEventArgs)
            If objScriptEventArgs.ReqCancel.Value Then
                CtcService.PostMessage(CtcService.MessageCat.Native, String.Format("Native operation for [{0}] was canceled by user script.", Me.ToString))
            Else
                Await Me.OperateOnCtc(False, sctCancelToken)
            End If

            Return True

        End Function


        Private Function OperateOnCtc(blnPress As Boolean, sctCancelToken As CancellationToken) As Task

            Select Case _enuBehavior
                Case ButtonBehavior.ManualState
                    'do nothing regarding state

                Case ButtonBehavior.PushState
                    Return SetStateOnCtc(If(blnPress, OnOff.On, OnOff.Off), sctCancelToken)

                Case ButtonBehavior.ToggleState
                    If Not blnPress Then   'on release
                        Return SetStateOnCtc(If(_enuState = OnOff.On, OnOff.Off, OnOff.On), sctCancelToken)
                    End If

            End Select

            Return Task.FromResult(0)

        End Function


        ''' <summary>Gets the button's current <see cref="Loconet.OnOff" /> value.</summary>
        Public ReadOnly Property State() As OnOff
            Get
                Return _enuState
            End Get
        End Property

        ''' <summary>Sets the button's <see cref="Loconet.OnOff" /> value.</summary>
        ''' <param name="enuState">The new state to be assigned.</param>
        ''' <param name="sctCancelToken">Token provided by a controlling source to notify this method that it should cancel its execution.</param>        
        ''' <returns>An awaitable <see cref="Tasks.Task" /> returning <i>True</i> on success, otherwise <i>False</i>.</returns>
        ''' <remarks>The CtcService must be started otherwise a <see cref="CtcIsStoppedException" /> will be thrown.</remarks>
        Public Function SetState(enuState As OnOff, Optional sctCancelToken As CancellationToken = Nothing) As Task

            If CtcService.IsCurrentThread Then
                Return SetStateOnCtc(enuState, sctCancelToken)
            Else
                CtcIsStoppedException.Check()

                Dim objTcs As New TaskCompletionSource
                CtcService.Execute(Async Sub()
                                       Await SetStateOnCtc(enuState, sctCancelToken)
                                       objTcs.TrySetResult()
                                   End Sub)
                Return objTcs.Task
            End If

        End Function


        Private Async Function SetStateOnCtc(enuState As OnOff, sctCancelToken As CancellationToken) As Task

            If enuState <> _enuState Then
                Dim objScriptEventArgs As New ScriptEventArgs(Me, "BeforeStateChange", enuState, sctCancelToken)
                Await Me.ScriptEventBinder.InvokeHandlersOnCtc(objScriptEventArgs)
                If objScriptEventArgs.ReqCancel.Value Then
                    CtcService.PostMessage(CtcService.MessageCat.Native, String.Format("[{0}] state change was canceled by user script.", Me.ToString))
                Else
                    _enuState = enuState
                    Me.NotifyObjectChanged(Me)
                    CtcService.PostMessage(CtcService.MessageCat.Native, String.Format("[{0}] has been set to [{1}].", Me.ToString, _enuState.ToString))
                    Await Me.ScriptEventBinder.InvokeHandlersOnCtc(New ScriptEventArgs(Me, "StateChanged", Nothing, sctCancelToken))
                End If
            Else
                CtcService.PostMessage(CtcService.MessageCat.Native, String.Format("[{0}] already set to [{1}].", Me.ToString, _enuState.ToString))
            End If

        End Function

#End Region

#Region "Scripting Events"

        Private _objScriptEventBinder As New ScriptEventBinder(Me, {
            "BeforeOperatePress",
            "BeforeOperateRelease",
            "BeforeStateChange",
            "StateChanged"
        })

        Public ReadOnly Property ScriptEventBinder As ScriptEventBinder Implements ISupportsScriptEvents.ScriptEventBinder
            Get
                Return _objScriptEventBinder
            End Get
        End Property

#End Region

    End Class

    ''' <summary>A DCC locomotive to be controled.</summary>
    <Serializable()> Public NotInheritable Class Engine
        Inherits CtcObjectBase
        Implements ISupportsScriptEvents

        'serialized
        Private _blnAutoBindInOpr As Boolean = True
        Private _srtLength As UShort
        Private _blnReqCatenary As Boolean
        Private _srtAddress As UShort = 3
        Private _enuSpeedSteps As SpeedSteps = SpeedSteps.DCC_128_SS

        Private _blnIsSimBound As Boolean = False   'in simulation mode this flag pretends the Engine is bound
        Private _bytSlot As Byte                    '0 = slot not assigned
        Private _bytSpeed As Byte                   'valid values are 0 to Me.SpeedStepMax
        Private _enuDirection As LocoDirection = LocoDirection.Forward
        Private _enuFunctions As OnOff() = New OnOff() {
            OnOff.Off, OnOff.Off, OnOff.Off, OnOff.Off, OnOff.Off,                                      '0-4
            OnOff.Off, OnOff.Off, OnOff.Off, OnOff.Off,                                                 '5-8
            OnOff.Off, OnOff.Off, OnOff.Off, OnOff.Off,                                                 '9-12
            OnOff.Off, OnOff.Off, OnOff.Off, OnOff.Off, OnOff.Off, OnOff.Off, OnOff.Off, OnOff.Off,     '13-20
            OnOff.Off, OnOff.Off, OnOff.Off, OnOff.Off, OnOff.Off, OnOff.Off, OnOff.Off, OnOff.Off      '21-28
        }

        'Since _enuDirection and _enuFunctions get set on RxPacket events, and current values are bundled together with changing values in a packet,
        'there are scenarios where the wrong direction/function values can be sent if the RxPacket from a previous update is not received before another value change order is given. 
        'For example:
        '- user sets the direction 
        '- before the RxPacket has a chance to update the direction, the user sets a function (e.g. through scripting)
        '- since updating the function requires sending the direction as well, the old not yet updated direction will be sent negating the previous direction change
        'For this reason, these two asserted versions hold the values that have been last sent but not confirmed as chanaged. The reason we need to leave the actual values
        'unchanged is so we can compare them with the confirmed values upon Rx packet receipt and see if they've changed since Engine events should only get raised on change.
        'UI indicators should be bound to the real values while packet transmission should use their asserted versions. 
        Private _enuAssertedDir As LocoDirection = LocoDirection.Forward
        Private _enuAssertedFunc As OnOff() = New OnOff() {
            OnOff.Off, OnOff.Off, OnOff.Off, OnOff.Off, OnOff.Off,                                      '0-4
            OnOff.Off, OnOff.Off, OnOff.Off, OnOff.Off,                                                 '5-8
            OnOff.Off, OnOff.Off, OnOff.Off, OnOff.Off,                                                 '9-12
            OnOff.Off, OnOff.Off, OnOff.Off, OnOff.Off, OnOff.Off, OnOff.Off, OnOff.Off, OnOff.Off,     '13-20
            OnOff.Off, OnOff.Off, OnOff.Off, OnOff.Off, OnOff.Off, OnOff.Off, OnOff.Off, OnOff.Off      '21-28
        }

        Private _objSpeedRampCts As CancellationTokenSource  'used to cancel a speed ramp in progress

        Public Sub New()
        End Sub

#Region "Serialization"

        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
            Select Case CtcService.SerializationContent
                Case SerializationContent.Definition
                    _blnAutoBindInOpr = info.GetBoolean("AutoBindInOpr")
                    _srtLength = info.GetInt16("Length")
                    _blnReqCatenary = info.GetBoolean("ReqCatenary")
                    _srtAddress = info.GetInt16("Address")
                    _enuSpeedSteps = info.GetValue("SpeedSteps", GetType(Object))
                Case SerializationContent.State

            End Select
        End Sub

        Friend Overrides Sub Initialize()
            MyBase.Initialize()
            Select Case CtcService.SerializationContent
                Case SerializationContent.Definition
                    Me.InitStoredState()
                Case SerializationContent.State
                    'engines have no saved states
            End Select
        End Sub

        <SecurityPermission(SecurityAction.LinkDemand, Flags:=SecurityPermissionFlag.SerializationFormatter)> _
        Protected Overrides Sub GetObjectData(info As SerializationInfo, context As StreamingContext)
            MyBase.GetObjectData(info, context)
            Select Case CtcService.SerializationContent
                Case SerializationContent.Definition
                    info.AddValue("AutoBindInOpr", _blnAutoBindInOpr)
                    info.AddValue("Length", _srtLength)
                    info.AddValue("ReqCatenary", _blnReqCatenary)
                    info.AddValue("Address", _srtAddress)
                    info.AddValue("SpeedSteps", _enuSpeedSteps)
                Case SerializationContent.State

            End Select
        End Sub

#End Region

#Region "Overrides"

        Public Overrides Sub DeleteSelf()
            MyBase.DeleteSelf()
            CtcService.Engines.Remove(Me)
            Me.UnbindSlot(False)
        End Sub

        Public Overrides Function ToString() As String
            Return "Engine." & Me.Name
        End Function

        Friend Overrides Async Function AfterCtcServiceStart() As Task
            'serialises engine bindings on the CTC thread
            If Not Me.IsBound And _blnAutoBindInOpr Then
                Await Me.BindSlot(True)
            End If
        End Function

        Friend Overrides Sub BeforeCtcServiceStop()
            Me.EmergencyStop()
        End Sub

#End Region

#Region "Configuration"

        ''' <summary>Gets or sets whether the engine should automatically bind to a slot when starting the CTC service.</summary>
        Public Property AutoBindInOpr As Boolean
            Get
                Return _blnAutoBindInOpr
            End Get
            Set(value As Boolean)
                _blnAutoBindInOpr = value
            End Set
        End Property

        ''' <summary>Gets or sets the DCC address of the engine.</summary>
        ''' <remarks>This property can only be set when the engine is not bound to a slot.</remarks>
        ''' <seealso cref="BindSlot(Boolean)" />
        ''' <seealso cref="UnbindSlot(Boolean)" />
        Public Property Address() As UShort
            Get
                Return _srtAddress
            End Get
            Set(Value As UShort)
                If Value > 16383 Then Throw New ArgumentOutOfRangeException(Nothing, "Invalid address. Valid values are 0-16383.")
                If Me.IsBound Then Throw New ApplicationException("Can not change Engine.Address while bound to a slot.")
                _srtAddress = Value
            End Set
        End Property

        ''' <summary>Gets or sets the speed step type of the engine.</summary>
        ''' <remarks>This property can only be set when the engine is not bound to a slot.</remarks>
        ''' <seealso cref="BindSlot(Boolean)" />
        ''' <seealso cref="UnbindSlot(Boolean)" />
        Public Property SpeedSteps() As SpeedSteps
            Get
                Return _enuSpeedSteps
            End Get
            Set(enuValue As SpeedSteps)
                If Me.IsBound Then Throw New ApplicationException("Can not change Engine.SpeedSteps while bound to a slot.")
                _enuSpeedSteps = enuValue
            End Set
        End Property

        ''' <summary>Gets the maximum speed value that can be assigned to the engine.</summary>
        ''' <remarks>This value is derived from the <see cref="SpeedSteps"/> property.</remarks>
        ''' <seealso cref="SpeedSteps" />
        ''' <seealso cref="Speed" />        
        Public ReadOnly Property SpeedStepMax() As Byte
            Get
                Select Case _enuSpeedSteps
                    Case SpeedSteps.Trinary_Old_14, SpeedSteps.DCC_14_SS
                        Return 14 - 2
                    Case SpeedSteps.DCC_28_SS, SpeedSteps.DCC_28_SS_Adv
                        Return 28 - 2
                    Case SpeedSteps.DCC_128_SS, SpeedSteps.DCC_128_SS_Adv
                        Return 128 - 2
                    Case Else
                        Return 0
                End Select
            End Get
        End Property

        ''' <summary>Gets or sets the engine's length over buffers.</summary>
        ''' <value>Number in millimeters.</value>
        ''' <remarks>Not currently used. Reserverd for future planned features.</remarks>
        Public Property Length() As UShort
            Get
                Return _srtLength
            End Get
            Set(Value As UShort)
                If CtcService.Engines.Contains(Me) Then CtcIsStartedException.Check()
                _srtLength = Value
            End Set
        End Property

        ''' <summary>Gets or sets whether the engine is electric and requires overhead catenary.</summary>
        ''' <remarks>Not currently used. Reserverd for future planned features.</remarks>
        Public Property ReqCatenary() As Boolean
            Get
                Return _blnReqCatenary
            End Get
            Set(blnValue As Boolean)
                If CtcService.Engines.Contains(Me) Then CtcIsStartedException.Check()
                _blnReqCatenary = blnValue
            End Set
        End Property

#End Region

#Region "Operation"

        '-------- bind slot -----------

        ''' <summary>Results returned by the <see cref="BindSlotResponse"/> event after calling <see cref="BindSlot(Boolean)"/>.</summary>
        Public Enum BindSlotResult As Byte
            ''' <summary>Engine has been bound to a slot and can now be controlled.</summary>
            Success = 0
            ''' <summary>Same as <see cref="Success"/> but the slot was in use and was stolen in the process.</summary>
            SuccessSlotStolen
            ''' <summary>Failed to bind because the slot associated with the engine's address is already in use.</summary>
            FailedInUse
            ''' <summary>Failed to bind because the slot associated with the engine's address is consisted upwards.</summary>
            ''' <remarks>Up consisted engines must be unlinked before usage.</remarks>
            FailedUpConsisted
            ''' <summary>Failed to bind because there are no free slots available.</summary>
            FailedNoFreeSlot
            ''' <summary>Command station acknowledged but could not bind for an unknown reason.</summary>
            FailedUnknown
            ''' <summary>Failed to bind because the command station did not respond, possibly due to a Loconet connection problem.</summary>
            FailedNoResponse
        End Enum

        ''' <summary>Binds the engine to the command station so that it can be controlled.</summary>
        ''' <param name="blnSteal">
        ''' In cases when a normal bind would be rejected with a <see cref="BindSlotResult.FailedInUse"/> result,
        ''' setting this parameter to <i>True</i> will force the bind by stealing the slot in use. 
        ''' </param>
        ''' <returns>An awaitable <see cref="Task" /> returning a <see cref="BindSlotResult"/>.</returns>
        ''' <remarks>
        ''' The <see cref="Slot"/> property will be set to a value greater than 0 on success.
        ''' The <see cref="BindSlotResponse"/> event will be raised on completion returning a <see cref="BindSlotResult"/>.
        ''' </remarks>
        ''' <seealso cref="UnbindSlot(Boolean)" />
        Public Async Function BindSlot(blnSteal As Boolean) As Task(Of BindSlotResult)

            'if a speed ramp is in progress, cancel it first
            StopSpeedRamp()

            If CtcService.SimulationMode Then
                'update the engine with data from a simulated slot
                _blnIsSimBound = True
                _bytSpeed = 0
                _enuDirection = LocoDirection.Forward
                _enuAssertedDir = _enuDirection
                For bytIdx As Byte = 0 To 28
                    _enuFunctions(bytIdx) = OnOff.Off
                    _enuAssertedFunc(bytIdx) = OnOff.Off
                Next
                Return BindSlot(BindSlotResult.Success)
            Else
                Dim objPkSetLocoAdr As New PkSetLocoAdr(_srtAddress)
                Await CtcService.LoconetService.TxPacket(objPkSetLocoAdr)
                Select Case True
                    Case objPkSetLocoAdr.RxPacket Is Nothing
                        Return BindSlot(BindSlotResult.FailedNoResponse)

                    Case TypeOf objPkSetLocoAdr.RxPacket Is PkLongAck
                        Dim objPkLonkAck As PkLongAck = objPkSetLocoAdr.RxPacket
                        If objPkLonkAck.Type = LongAckType.NoFreeSlot Then
                            Return BindSlot(BindSlotResult.FailedNoFreeSlot)
                        Else
                            Return BindSlot(BindSlotResult.FailedUnknown)
                        End If

                    Case TypeOf objPkSetLocoAdr.RxPacket Is PkLocoSlot
                        Dim objPkReadLocoSlot As PkLocoSlot = objPkSetLocoAdr.RxPacket
                        If objPkReadLocoSlot.ConsistType = ConsistType.MidConsist Or objPkReadLocoSlot.ConsistType = ConsistType.SubMember Then
                            Return BindSlot(BindSlotResult.FailedUpConsisted)
                        Else
                            'null slot move used to guarantee proper slot usage interlocking in a multi-user asynchronous environment
                            'also puts SlotActivity to InUse
                            Dim objPkSlotMove As New PkSlotMove(objPkReadLocoSlot.Slot, objPkReadLocoSlot.Slot)
                            Await CtcService.LoconetService.TxPacket(objPkSlotMove)

                            Dim blnSlotStolen As Boolean
                            Select Case True
                                Case objPkSlotMove.RxPacket Is Nothing
                                    Return BindSlot(BindSlotResult.FailedNoResponse)

                                Case TypeOf objPkSlotMove.RxPacket Is PkLocoSlot
                                    objPkReadLocoSlot = objPkSlotMove.RxPacket
                                    blnSlotStolen = False

                                Case TypeOf objPkSlotMove.RxPacket Is PkLongAck
                                    Dim objPkLonkAck As PkLongAck = objPkSlotMove.RxPacket
                                    If objPkLonkAck.Type = LongAckType.IllegalMove Then
                                        If blnSteal Then
                                            blnSlotStolen = True
                                        Else
                                            Return BindSlot(BindSlotResult.FailedInUse)
                                        End If
                                    Else
                                        Return BindSlot(BindSlotResult.FailedUnknown)
                                    End If

                                Case Else
                                    Return BindSlot(BindSlotResult.FailedUnknown)

                            End Select

                            'send a slot write packet
                            Dim objPkWriteLocoSlot As PkLocoSlot = objPkReadLocoSlot.Clone()
                            objPkWriteLocoSlot.SpeedSteps = _enuSpeedSteps
                            Await CtcService.LoconetService.TxPacket(objPkWriteLocoSlot)

                            Select Case True
                                Case objPkWriteLocoSlot.RxPacket Is Nothing
                                    Return BindSlot(BindSlotResult.FailedNoResponse)

                                Case TypeOf objPkWriteLocoSlot.RxPacket Is PkLongAck
                                    Dim objPkLonkAck As PkLongAck = objPkWriteLocoSlot.RxPacket
                                    If objPkLonkAck.Type = LongAckType.ProgNoReply Then
                                        'update the engine with slot data
                                        _bytSlot = objPkReadLocoSlot.Slot
                                        _bytSpeed = If(objPkReadLocoSlot.Speed = 0, 0, objPkReadLocoSlot.Speed - 1)
                                        _enuDirection = objPkReadLocoSlot.Direction
                                        _enuAssertedDir = _enuDirection
                                        Dim enuRxFuncs As OnOff() = objPkReadLocoSlot.Functions0to4
                                        For bytIdx As Byte = 0 To 4
                                            _enuFunctions(bytIdx) = enuRxFuncs(bytIdx)
                                            _enuAssertedFunc(bytIdx) = enuRxFuncs(bytIdx)
                                        Next
                                        enuRxFuncs = objPkReadLocoSlot.Functions5to8
                                        For bytIdx As Byte = 0 To 3
                                            _enuFunctions(bytIdx + 5) = enuRxFuncs(bytIdx)
                                            _enuAssertedFunc(bytIdx + 5) = enuRxFuncs(bytIdx)
                                        Next
                                        For bytIdx As Byte = 9 To 28  'these function states are not stored in slots so current state can not be retrieved
                                            _enuFunctions(bytIdx) = OnOff.Off
                                            _enuAssertedFunc(bytIdx) = OnOff.Off
                                        Next

                                        'start listening to slot data now that the engine is bound
                                        AddHandler CtcService.LoconetService.RxPacketOnWorkerThread, AddressOf RxSlotData

                                        If blnSlotStolen Then
                                            Return BindSlot(BindSlotResult.SuccessSlotStolen)
                                        Else
                                            Return BindSlot(BindSlotResult.Success)
                                        End If
                                    Else
                                        Return BindSlot(BindSlotResult.FailedUnknown)
                                    End If

                                Case Else
                                    Return BindSlot(BindSlotResult.FailedUnknown)

                            End Select
                        End If

                    Case Else
                        Return BindSlot(BindSlotResult.FailedUnknown)

                End Select
            End If

        End Function

        Private Function BindSlot(enuResult As BindSlotResult) As BindSlotResult

            Select Case enuResult
                Case BindSlotResult.Success
                    If _bytSlot = 0 Then
                        CtcService.PostMessage(CtcService.MessageCat.Native, String.Format("[{0}] has been bound to a simulated slot.", Me.ToString))
                    Else
                        CtcService.PostMessage(CtcService.MessageCat.Native, String.Format("[{0}] has been bound to slot [{1}].", Me.ToString, _bytSlot))
                    End If
                    Me.NotifyObjectChanged(Me)

                Case BindSlotResult.SuccessSlotStolen
                    CtcService.PostMessage(CtcService.MessageCat.Native, String.Format("[{0}] has been bound to stolen slot [{1}].", Me.ToString, _bytSlot))
                    Me.NotifyObjectChanged(Me)

                Case BindSlotResult.FailedInUse
                    CtcService.PostMessage(CtcService.MessageCat.Error, String.Format("Failed to bind [{0}] to slot. Slot is in use.", Me.ToString))

                Case BindSlotResult.FailedUpConsisted
                    CtcService.PostMessage(CtcService.MessageCat.Error, String.Format("Failed to bind [{0}] to slot. Engine is up consisted.", Me.ToString))

                Case BindSlotResult.FailedNoFreeSlot
                    CtcService.PostMessage(CtcService.MessageCat.Error, String.Format("Failed to bind [{0}] to slot. No free slot available.", Me.ToString))

                Case BindSlotResult.FailedUnknown
                    CtcService.PostMessage(CtcService.MessageCat.Error, String.Format("Failed to bind [{0}] to slot.", Me.ToString))

                Case BindSlotResult.FailedNoResponse
                    CtcService.PostMessage(CtcService.MessageCat.Error, String.Format("Failed to bind [{0}] to slot. Command station did not respond.", Me.ToString))

            End Select

            Me.RaiseBindSlotResponseEvent(enuResult)

            Return enuResult

        End Function

        '-------- unbind slot ---------

        ''' <summary>Results returned by the <see cref="UnbindSlotResponse"/> event after calling <see cref="UnbindSlot(Boolean)"/>.</summary>
        Public Enum UnbindSlotResult As Byte
            ''' <summary>Engine has been unbound from its slot.</summary>
            Success = 0
            ''' <summary>Engine has been unbound from its slot but the network did not confirm it.</summary>
            SuccessNoCfm
            ''' <summary>Command station acknowledged but could not unbind for an unknown reason.</summary>
            FailedUnknown
            ''' <summary>Failed to unbind because the command station did not respond, possibly due to a Loconet connection problem.</summary>
            FailedNoResponse
        End Enum

        ''' <summary>Unbinds the engine from the command station and frees the slot.</summary>
        ''' <param name="blnNetMustCfm">
        ''' Indicates if the Loconet network must confirm the slot release commands before the Engine can be marked unbound.
        ''' When <i>False</i> the Engine will be marked unbound even if disconnected from the network. 
        ''' </param>
        ''' <returns>An awaitable <see cref="Tasks.Task" /> returning a <see cref="UnbindSlotResult"/>.</returns>
        ''' <remarks>
        ''' The <see cref="Slot"/> property will be set to 0 on success. Call is ignored if the engine is not bound to a slot.
        ''' The <see cref="UnbindSlotResponse"/> event will be raised on completion returning an <see cref="UnbindSlotResult"/>.
        ''' </remarks>
        ''' <seealso cref="BindSlot(Boolean)" />
        Public Async Function UnbindSlot(blnNetMustCfm As Boolean) As Task(Of UnbindSlotResult)

            If Not Me.IsBound Then Exit Function

            'if a speed ramp is in progress, cancel it first
            StopSpeedRamp()

            If CtcService.SimulationMode Then
                Return UnbindSlot(UnbindSlotResult.Success)
            Else
                If Await CtcService.LoconetService.TxPacket(New PkSetSlotSpeed(_bytSlot, 0)) Then
                    If Await CtcService.LoconetService.TxPacket(New PkSlotStatus(_bytSlot, SlotActivity.Idle, ConsistType.NoConsist, Loconet.SpeedSteps.DCC_128_SS)) Then
                        Dim objPkSlotMove As New PkSlotMove(_bytSlot, 0)
                        Await CtcService.LoconetService.TxPacket(objPkSlotMove)
                        Select Case True
                            Case objPkSlotMove.RxPacket Is Nothing
                                If blnNetMustCfm Then
                                    Return UnbindSlot(UnbindSlotResult.FailedNoResponse)
                                Else
                                    Return UnbindSlot(UnbindSlotResult.SuccessNoCfm)
                                End If

                            Case TypeOf objPkSlotMove.RxPacket Is PkRdWrSlotData  'slot move was successful
                                Return UnbindSlot(UnbindSlotResult.Success)

                            Case TypeOf objPkSlotMove.RxPacket Is PkLongAck
                                'theoretically a PkLongAck response is possible because of an illegal slot move but should not occur since we are moving to slot 0
                                If blnNetMustCfm Then
                                    Return UnbindSlot(UnbindSlotResult.FailedUnknown)
                                Else
                                    Return UnbindSlot(UnbindSlotResult.SuccessNoCfm)
                                End If

                            Case Else
                                If blnNetMustCfm Then
                                    Return UnbindSlot(UnbindSlotResult.FailedUnknown)
                                Else
                                    Return UnbindSlot(UnbindSlotResult.SuccessNoCfm)
                                End If

                        End Select
                    Else
                        If blnNetMustCfm Then
                            Return UnbindSlot(UnbindSlotResult.FailedNoResponse)
                        Else
                            Return UnbindSlot(UnbindSlotResult.SuccessNoCfm)
                        End If
                    End If
                Else
                    If blnNetMustCfm Then
                        Return UnbindSlot(UnbindSlotResult.FailedNoResponse)
                    Else
                        Return UnbindSlot(UnbindSlotResult.SuccessNoCfm)
                    End If
                End If
            End If

        End Function

        Private Function UnbindSlot(enuResult As UnbindSlotResult) As UnbindSlotResult

            Select Case enuResult
                Case UnbindSlotResult.Success, UnbindSlotResult.SuccessNoCfm
                    If _bytSlot = 0 Then
                        CtcService.PostMessage(CtcService.MessageCat.Native, String.Format("[{0}] has been unbound from its simulated slot.", Me.ToString))
                        _blnIsSimBound = False
                    Else
                        'stop listening to slot data 
                        RemoveHandler CtcService.LoconetService.RxPacketOnWorkerThread, AddressOf RxSlotData

                        CtcService.PostMessage(CtcService.MessageCat.Native, String.Format("[{0}] has been unbound from slot [{1}].", Me.ToString, _bytSlot))
                        _bytSlot = 0     'this is here because it has to be set to zero after the above message post so that the old slot is shown
                    End If
                    Me.NotifyObjectChanged(Me)

                Case UnbindSlotResult.FailedUnknown
                    CtcService.PostMessage(CtcService.MessageCat.Error, String.Format("Failed to unbind [{0}] from slot [{1}].", Me.ToString, _bytSlot))

                Case UnbindSlotResult.FailedNoResponse
                    CtcService.PostMessage(CtcService.MessageCat.Error, String.Format("Failed to unbind [{0}] from slot [{1}]. Command station did not respond.", Me.ToString, _bytSlot))

            End Select

            Me.RaiseUnbindSlotResponseEvent(enuResult)

            Return enuResult

        End Function

        '------------------------------

        ''' <summary>Gets a value indicating whether the engine has been bound to a command station slot.</summary>
        ''' <remarks>If the <i>Engine</i> is bound, in simulation mode, this method will return <i>True</i> even if not actually physically bound.</remarks>
        ''' <seealso cref="BindSlot(Boolean)" />
        ''' <seealso cref="UnbindSlot(Boolean)" />
        Public ReadOnly Property IsBound() As Boolean
            Get
                Return If(CtcService.SimulationMode, _blnIsSimBound, _bytSlot > 0)
            End Get
        End Property

        ''' <summary>Gets the slot number assigned to the <i>Engine</i> by the command station.</summary>
        ''' <value>Zero if engine is not bound to a slot, otherwise returns the slot number the engine is bound to.</value>
        ''' <seealso cref="BindSlot(Boolean)" />
        ''' <seealso cref="UnbindSlot(Boolean)" />
        Public ReadOnly Property Slot() As Byte
            Get
                Return _bytSlot
            End Get
        End Property

        ''' <summary>Stops the engine immediately ignoring deceleration curves that might be programmed with the engine's decoder.</summary>        
        ''' <remarks>The engine must be bound to a command station slot prior to calling this method.</remarks>
        ''' <seealso cref="BindSlot(Boolean)" />
        ''' <seealso cref="UnbindSlot(Boolean)" />
        Public Sub EmergencyStop()
            If Not Me.IsBound Then Exit Sub

            'if a speed ramp is in progress, cancel it first
            StopSpeedRamp()

            If CtcService.SimulationMode Then
                TxPacketSim("Speed", 1, Nothing)
            Else
                CtcService.LoconetService.TxPriorityPacket(New PkSetSlotSpeed(_bytSlot, 1))
            End If
        End Sub

        ''' <summary>Gets or sets the speed of the engine.</summary>
        ''' <value>Valid values are 0 to <see cref="SpeedStepMax"/>.</value>
        ''' <remarks>The engine must be bound to a command station slot prior to setting this property.</remarks>
        ''' <seealso cref="BindSlot(Boolean)" />
        ''' <seealso cref="UnbindSlot(Boolean)" />
        ''' <seealso cref="SpeedStepMax" />
        Public Property Speed() As Byte
            Get
                Return _bytSpeed
            End Get
            Set(Value As Byte)
                SetSpeed(Value, Nothing)
            End Set
        End Property

        ''' <summary>Sets the speed of the engine.</summary>
        ''' <param name="bytSpeed">Valid values are 0 to <see cref="SpeedStepMax"/>.</param>
        ''' <param name="sctClientToken">A token that indentifies the client requesting the speed change.</param>
        ''' <remarks>
        ''' This method is similar to the <see cref="Speed"/> property except you can also specify a client token.
        ''' The token is returned through the <see cref="SpeedChanged"/> event. Through comparison, one can determine if the event was created
        ''' as a direct action of our client or was created by an external unknown one.<br /><br />
        ''' The engine must be bound to a command station slot prior to calling this method. 
        ''' </remarks>
        ''' <seealso cref="BindSlot(Boolean)" />
        ''' <seealso cref="UnbindSlot(Boolean)" />
        ''' <seealso cref="SpeedStepMax" />
        ''' <seealso cref="Speed" />
        Public Sub SetSpeed(bytSpeed As Byte, sctClientToken As Guid)
            If Not Me.IsBound Then Exit Sub

            'if a speed ramp is in progress, cancel it first
            StopSpeedRamp()

            bytSpeed = If(bytSpeed = 0, 0, Math.Min(bytSpeed, Me.SpeedStepMax) + 1)  'convert Engine speed to packet speed

            If CtcService.SimulationMode Then
                TxPacketSim("Speed", bytSpeed, sctClientToken)
            Else
                CtcService.LoconetService.TxPrioritySpeed(_bytSlot, bytSpeed, sctClientToken)
            End If

        End Sub

        ''' <summary>Creates a task that performs a gradual engine speed increase/decrease.</summary>
        ''' <param name="bytSpeedTarget">The target speed that will end the ramp. Valid values are 0 to <see cref="SpeedStepMax"/>.</param>
        ''' <param name="srtInterval">Number of milliseconds to wait between each speed step. Valid values are 100-3000.</param>
        ''' <returns>
        ''' When the task is awaited, it returns <i>True</i>, if the speed ramp ran to it completion, 
        ''' or <i>False</i>, if it was interrupted prematurely by another process.
        ''' </returns>
        ''' <remarks>
        ''' Starting a speed ramp will cancel any other running speed ramp for this engine object. The ramp will start from the 
        ''' current speed and end with the target speed.<br /><br />
        ''' The engine must be bound to a command station slot prior to calling this method.
        ''' </remarks>
        ''' <seealso cref="BindSlot(Boolean)" />
        ''' <seealso cref="UnbindSlot(Boolean)" />
        ''' <seealso cref="StopSpeedRamp" />
        ''' <seealso cref="SpeedStepMax" />
        Public Function StartSpeedRamp(bytSpeedTarget As Byte, srtInterval As UShort) As Task(Of Boolean)
            If Not Me.IsBound Then Exit Function

            'if another speed ramp is in progress, cancel it first
            StopSpeedRamp()

            Dim objTcs As New TaskCompletionSource(Of Boolean)
            _objSpeedRampCts = New CancellationTokenSource()
            Dim objCancelToken As CancellationToken = _objSpeedRampCts.Token

            Task.Run(Sub()

                         Dim bytSpeedRampTarget As Byte = Math.Min(bytSpeedTarget, Me.SpeedStepMax)
                         While bytSpeedRampTarget <> _bytSpeed AndAlso Not objCancelToken.IsCancellationRequested
                             Dim bytSpeed As Byte
                             Select Case True
                                 Case bytSpeedRampTarget < _bytSpeed
                                     bytSpeed = _bytSpeed - 1
                                 Case bytSpeedRampTarget > _bytSpeed
                                     bytSpeed = _bytSpeed + 1
                                 Case Else
                                     'in case _bytSpeed is changed by another thread after the While condition was checked
                                     Continue While
                             End Select

                             bytSpeed = If(bytSpeed = 0, 0, bytSpeed + 1)

                             If CtcService.SimulationMode Then
                                 TxPacketSim("Speed", bytSpeed, Nothing)
                             Else
                                 CtcService.LoconetService.TxPrioritySpeed(_bytSlot, bytSpeed, Nothing)
                             End If

                             'wait until delay time has passed or cancel token is signaled as canceled;
                             'allowed delay value between speed steps is 100-3000 ms
                             objCancelToken.WaitHandle.WaitOne(Math.Min(Math.Max(srtInterval, 100), 3000))

                             'these two are equivalent to the line above except they throw errors when the token is canceled so I opted for the above
                             'Task.Delay([ms], objCancelToken).Wait()
                             'Task.Delay([ms]).Wait(objCancelToken)
                         End While

                         objTcs.TrySetResult(Not objCancelToken.IsCancellationRequested)

                     End Sub, objCancelToken)

            Return objTcs.Task

        End Function

        ''' <summary>Stops a previously started speed ramp.</summary>
        ''' <seealso cref="StartSpeedRamp" />
        Public Sub StopSpeedRamp()
            If _objSpeedRampCts IsNot Nothing Then _objSpeedRampCts.Cancel()
        End Sub

        ''' <summary>Gets or sets the engine's direction of travel.</summary>
        ''' <remarks>The engine must be bound to a command station slot prior to setting this property.</remarks>
        ''' <seealso cref="BindSlot(Boolean)" />
        ''' <seealso cref="UnbindSlot(Boolean)" />
        Public Property Direction() As LocoDirection
            Get
                Return _enuDirection
            End Get
            Set(Value As LocoDirection)
                If Not Me.IsBound Then Exit Property

                _enuAssertedDir = Value
                Dim enuTxFuncs As OnOff()
                enuTxFuncs = New OnOff() {_enuAssertedFunc(0), _enuAssertedFunc(1), _enuAssertedFunc(2), _enuAssertedFunc(3), _enuAssertedFunc(4)}

                If CtcService.SimulationMode Then
                    TxPacketSim("Direction", _enuAssertedDir)
                Else
                    CtcService.LoconetService.TxPriorityPacket(New PkSetSlotDirFunc(_bytSlot, _enuAssertedDir, enuTxFuncs))
                End If
            End Set
        End Property

        ''' <summary>Gets or sets the engine's function states.</summary>
        ''' <param name="bytIdx">Valid values are 0-28.</param>
        ''' <remarks>
        ''' These are the functions supported by your DCC decoder.<br /><br />
        ''' The engine must be bound to a command station slot prior to setting this property.
        ''' </remarks>
        ''' <seealso cref="BindSlot(Boolean)" />
        ''' <seealso cref="UnbindSlot(Boolean)" />
        Public Property Functions(bytIdx As Byte) As OnOff
            Get
                Return _enuFunctions(bytIdx)
            End Get
            Set(Value As OnOff)
                If Not Me.IsBound Then Exit Property

                _enuAssertedFunc(bytIdx) = Value
                Select Case bytIdx
                    Case 0 To 4
                        Dim enuTxFuncs As OnOff() = New OnOff() {_enuAssertedFunc(0), _enuAssertedFunc(1), _enuAssertedFunc(2), _enuAssertedFunc(3), _enuAssertedFunc(4)}
                        If CtcService.SimulationMode Then
                            TxPacketSim("Function0to4", enuTxFuncs)
                        Else
                            CtcService.LoconetService.TxPriorityPacket(New PkSetSlotDirFunc(_bytSlot, _enuAssertedDir, enuTxFuncs))
                        End If

                    Case 5 To 8
                        Dim enuTxFuncs As OnOff() = New OnOff() {_enuAssertedFunc(5), _enuAssertedFunc(6), _enuAssertedFunc(7), _enuAssertedFunc(8)}
                        If CtcService.SimulationMode Then
                            TxPacketSim("Function5to8", enuTxFuncs)
                        Else
                            CtcService.LoconetService.TxPriorityPacket(New PkSetSlotFunc5to8(_bytSlot, enuTxFuncs))
                        End If

                    Case 9 To 12
                        Dim enuTxFuncs As OnOff() = New OnOff() {_enuAssertedFunc(9), _enuAssertedFunc(10), _enuAssertedFunc(11), _enuAssertedFunc(12)}
                        If CtcService.SimulationMode Then
                            TxPacketSim("Function9to12", enuTxFuncs)
                        Else
                            CtcService.LoconetService.TxPriorityPacket(New PkImmediate() With {
                                .DccAddress = _srtAddress,
                                .DccInstruction = PkImmediate.DccInstrType.Func9to12,
                                .DccFunctions = enuTxFuncs
                            })
                        End If

                    Case 13 To 20
                        Dim enuTxFuncs As OnOff() = New OnOff() {_enuAssertedFunc(13), _enuAssertedFunc(14), _enuAssertedFunc(15), _enuAssertedFunc(16), _enuAssertedFunc(17), _enuAssertedFunc(18), _enuAssertedFunc(19), _enuAssertedFunc(20)}
                        If CtcService.SimulationMode Then
                            TxPacketSim("Function13to20", enuTxFuncs)
                        Else
                            CtcService.LoconetService.TxPriorityPacket(New PkImmediate() With {
                                .DccAddress = _srtAddress,
                                .DccInstruction = PkImmediate.DccInstrType.Func13To20,
                                .DccFunctions = enuTxFuncs
                            })
                        End If

                    Case 21 To 28
                        Dim enuTxFuncs As OnOff() = New OnOff() {_enuAssertedFunc(21), _enuAssertedFunc(22), _enuAssertedFunc(23), _enuAssertedFunc(24), _enuAssertedFunc(25), _enuAssertedFunc(26), _enuAssertedFunc(27), _enuAssertedFunc(28)}
                        If CtcService.SimulationMode Then
                            TxPacketSim("Function21to28", enuTxFuncs)
                        Else
                            CtcService.LoconetService.TxPriorityPacket(New PkImmediate() With {
                                .DccAddress = _srtAddress,
                                .DccInstruction = PkImmediate.DccInstrType.Func21To28,
                                .DccFunctions = enuTxFuncs
                            })
                        End If
                End Select
            End Set
        End Property

        ''' <summary>Gets the engine's function states.</summary>
        ''' <returns>A clone of the underlying array. Changes made to this array will not set the engine's functions. Use <see cref="Functions(Byte)"/> to set them.</returns>
        ''' <remarks>These are the functions supported by your DCC decoder.</remarks>
        Public ReadOnly Property Functions As OnOff()
            Get
                Return _enuFunctions.Clone
            End Get
        End Property

#End Region

#Region "Event Handling"

        ''' <summary>Executed on the Loconet API's Rx worker thread.</summary>
        Private Sub RxSlotData(objPacket As Packet)

            Dim blnChanged As Boolean = False    'did the Engine object change in any way

            Select Case True
                Case TypeOf objPacket Is PkLocoSlot
                    Dim objPkLocoSlot As PkLocoSlot = objPacket
                    If objPkLocoSlot.Slot = _bytSlot Then
                        RxSpeed(objPkLocoSlot.Speed, Nothing, blnChanged)
                        RxDirection(objPkLocoSlot.Direction, blnChanged)
                        RxFunction(0, objPkLocoSlot.Functions0to4, blnChanged)
                        RxFunction(5, objPkLocoSlot.Functions5to8, blnChanged)
                    End If

                Case TypeOf objPacket Is PkSetSlotSpeed
                    Dim objPkSetSlotSpeed As PkSetSlotSpeed = objPacket
                    If objPkSetSlotSpeed.Slot = _bytSlot Then
                        RxSpeed(objPkSetSlotSpeed.Speed, objPkSetSlotSpeed.Tag, blnChanged)
                    End If

                Case TypeOf objPacket Is PkSetSlotDirFunc
                    Dim objPkSetSlotDirFunc As PkSetSlotDirFunc = objPacket
                    If objPkSetSlotDirFunc.Slot = _bytSlot Then
                        RxDirection(objPkSetSlotDirFunc.Direction, blnChanged)
                        RxFunction(0, objPkSetSlotDirFunc.Functions, blnChanged)
                    End If

                Case TypeOf objPacket Is PkSetSlotFunc5to8
                    Dim objPkSetSlotFunc5to8 As PkSetSlotFunc5to8 = objPacket
                    If objPkSetSlotFunc5to8.Slot = _bytSlot Then
                        RxFunction(5, objPkSetSlotFunc5to8.Functions, blnChanged)
                    End If

                Case TypeOf objPacket Is PkSetSlotFunc9to12
                    'RRA never sends this packet beacuse it incorrectly overwrites slot function 5-8 states (Digitrax bug),
                    'but observed here in case this packet is placed on the Loconet network by some entity external to RRA
                    Dim objPkSetSlotFunc9to12 As PkSetSlotFunc9to12 = objPacket
                    If objPkSetSlotFunc9to12.Slot = _bytSlot Then
                        RxFunction(9, objPkSetSlotFunc9to12.Functions, blnChanged)
                    End If

                Case TypeOf objPacket Is PkImmediate
                    Dim objPkImmediate As PkImmediate = objPacket
                    If objPkImmediate.DccAddress = _srtAddress Then
                        Select Case objPkImmediate.DccInstruction
                            Case PkImmediate.DccInstrType.Func0to4
                                RxFunction(0, objPkImmediate.DccFunctions, blnChanged) '* see below
                            Case PkImmediate.DccInstrType.Func5to8
                                RxFunction(5, objPkImmediate.DccFunctions, blnChanged) '* see below
                            Case PkImmediate.DccInstrType.Func9to12
                                RxFunction(9, objPkImmediate.DccFunctions, blnChanged)
                            Case PkImmediate.DccInstrType.Func13To20
                                RxFunction(13, objPkImmediate.DccFunctions, blnChanged)
                            Case PkImmediate.DccInstrType.Func21To28
                                RxFunction(21, objPkImmediate.DccFunctions, blnChanged)
                        End Select
                        '* this will cause the Engine and command station slot function states to become desynchronized since PkImmediate does not update the
                        'slot function states; this is why RRA will not use PkImmediate to set functions 0-8; however, since hardware throttles will do this,
                        'we update the Engine's function states to the best known value even though the slot states could become stale; 
                        'another sloppy design decision by Digitrax
                    End If

            End Select

            'notify object changed
            If blnChanged Then Me.NotifyObjectChanged(Me)

        End Sub

        ''' <summary>Simulates a packet transmit and returns immediately. It spawns a new thread to simulate a Loconet API Rx thread.</summary>
        Private Sub TxPacketSim(strType As String, ParamArray objArg() As Object)

            Dim delRxSlotDataSim As New Action(
                Sub()
                    'this sub mimics what RxSlotData() does 

                    Dim blnChanged As Boolean = False    'did the Engine object change in any way

                    Select Case strType
                        Case "Speed"
                            RxSpeed(objArg(0), objArg(1), blnChanged)
                        Case "Direction"
                            RxDirection(objArg(0), blnChanged)
                        Case "Function0to4"
                            RxFunction(0, objArg(0), blnChanged)
                        Case "Function5to8"
                            RxFunction(5, objArg(0), blnChanged)
                        Case "Function9to12"
                            RxFunction(9, objArg(0), blnChanged)
                        Case "Function13to20"
                            RxFunction(13, objArg(0), blnChanged)
                        Case "Function21to28"
                            RxFunction(21, objArg(0), blnChanged)
                    End Select

                    'notify object changed
                    If blnChanged Then Me.NotifyObjectChanged(Me)

                End Sub)

            'execute all code defined above on a new worker thread to simulate a Loconet worker thread
            delRxSlotDataSim.BeginInvoke(Nothing, Nothing)

        End Sub


        Private Sub RxSpeed(bytSpeed As Byte, sctClientToken As Guid, ByRef blnChanged As Boolean)

            bytSpeed = If(bytSpeed = 0, 0, bytSpeed - 1)  'convert packet speed to Engine speed

            If _bytSpeed <> bytSpeed Then

                _bytSpeed = bytSpeed
                blnChanged = True

                'raise script events
                Me.ScriptEventBinder.InvokeHandlersOnAny(New ScriptEventArgs(Me, "SpeedChanged", Nothing, Nothing))

                'raise standard events
                Me.RaiseSpeedChangedEvent(sctClientToken)
            End If

        End Sub

        Private Sub RxDirection(enuDirection As LocoDirection, ByRef blnChanged As Boolean)

            If _enuDirection <> enuDirection Then

                _enuDirection = enuDirection
                _enuAssertedDir = enuDirection  'the asserted is also updated in case another engine bound to the same slot as this one generated this RxPacket meaning we did not assert these values
                blnChanged = True

                'raise script events
                Me.ScriptEventBinder.InvokeHandlersOnAny(New ScriptEventArgs(Me, "DirectionChanged", Nothing, Nothing))

                'raise standard events
                Me.RaiseDirectionChangedEvent()
            End If

        End Sub

        Private Sub RxFunction(bytFuncStart As Byte, enuFunctions() As OnOff, ByRef blnChanged As Boolean)

            For bytIdx As Byte = 0 To enuFunctions.Count - 1
                If _enuFunctions(bytIdx + bytFuncStart) <> enuFunctions(bytIdx) Then
                    blnChanged = True
                    Exit For
                End If
            Next

            If blnChanged Then
                For bytIdx As Byte = 0 To enuFunctions.Count - 1
                    _enuFunctions(bytIdx + bytFuncStart) = enuFunctions(bytIdx)
                    _enuAssertedFunc(bytIdx + bytFuncStart) = enuFunctions(bytIdx)  'the asserted is also updated in case another engine bound to the same slot as this one generated this RxPacket meaning we did not assert these values
                Next

                'raise script events
                Me.ScriptEventBinder.InvokeHandlersOnAny(New ScriptEventArgs(Me, "FunctionChanged", Nothing, Nothing))

                'raise standard events
                Me.RaiseFunctionChangedEvent()
            End If

        End Sub

#End Region

#Region "Standard Events"

        ''' <summary>Occurs after an attempt to bind an engine to a slot.</summary>
        Public Event BindSlotResponse(enuResult As BindSlotResult)

        Private Sub RaiseBindSlotResponseEvent(enuResult As BindSlotResult)

            If CtcService.SyncContext Is Nothing OrElse CtcService.SyncContext.CheckAccess Then
                'raise event on the current thread
                RaiseEvent BindSlotResponse(enuResult)
            Else
                'raise event on the sync context thread
                CtcService.SyncContext.InvokeAsync(Sub() RaiseEvent BindSlotResponse(enuResult))
            End If

        End Sub

        '------

        ''' <summary>Occurs after an attempt to unbind an engine from a slot.</summary>
        Public Event UnbindSlotResponse(enuResult As UnbindSlotResult)

        Private Sub RaiseUnbindSlotResponseEvent(enuResult As UnbindSlotResult)

            If CtcService.SyncContext Is Nothing OrElse CtcService.SyncContext.CheckAccess Then
                'raise event on the current thread
                RaiseEvent UnbindSlotResponse(enuResult)
            Else
                'raise event on the sync context thread
                CtcService.SyncContext.InvokeAsync(Sub() RaiseEvent UnbindSlotResponse(enuResult))
            End If

        End Sub

        '------

        ''' <summary>Occurs when the speed of the engine's bound slot has changed.</summary>
        Public Event SpeedChanged(sctClientToken As Guid)

        Private Sub RaiseSpeedChangedEvent(sctClientToken As Guid)

            If CtcService.SyncContext Is Nothing OrElse CtcService.SyncContext.CheckAccess Then
                'raise event on the current thread
                RaiseEvent SpeedChanged(sctClientToken)
            Else
                'raise event on the sync context thread
                CtcService.SyncContext.InvokeAsync(Sub() RaiseEvent SpeedChanged(sctClientToken))
            End If

        End Sub

        '------

        ''' <summary>Occurs when the direction of the engine's bound slot has changed.</summary>
        Public Event DirectionChanged()

        Private Sub RaiseDirectionChangedEvent()

            If CtcService.SyncContext Is Nothing OrElse CtcService.SyncContext.CheckAccess Then
                'raise event on the current thread
                RaiseEvent DirectionChanged()
            Else
                'raise event on the sync context thread
                CtcService.SyncContext.InvokeAsync(Sub() RaiseEvent DirectionChanged())
            End If

        End Sub

        '------

        ''' <summary>Occurs when any function of the engine's bound slot has changed.</summary>
        Public Event FunctionChanged()

        Private Sub RaiseFunctionChangedEvent()

            If CtcService.SyncContext Is Nothing OrElse CtcService.SyncContext.CheckAccess Then
                'raise event on the current thread
                RaiseEvent FunctionChanged()
            Else
                'raise event on the sync context thread
                CtcService.SyncContext.InvokeAsync(Sub() RaiseEvent FunctionChanged())
            End If

        End Sub

#End Region

#Region "Scripting Events"

        Private _objScriptEventBinder As New ScriptEventBinder(Me, {
            "SpeedChanged",
            "DirectionChanged",
            "FunctionChanged"
        })

        Public ReadOnly Property ScriptEventBinder As ScriptEventBinder Implements ISupportsScriptEvents.ScriptEventBinder
            Get
                Return _objScriptEventBinder
            End Get
        End Property

#End Region

    End Class

    ''' <summary>A series of Loconet packets that can be played back onto the network.</summary>
    ''' <remarks>
    '''     A sequence contains a series of ordered Loconet packets, each with event timing information, so that when played, can perform a scripted action.  
    '''     Since sequences record raw packet traffic, playing them could cause the state integrity of CTC managed objects to be poluted. 
    '''     For example if the CTC had the state of a turnout locked through a route, it would not be prudent to run a sequence changing the state of that turnout. 
    '''     Examples of appropriate sequence use might be: accessory automated behaviors (i.e. cranes, turntables, street lights), special engine beaviors (i.e. yard movements, decoupling, etc.)
    ''' </remarks>
    <Serializable()> Public NotInheritable Class Sequence
        Inherits CtcObjectBase

        'serialized
        Private _objSeqItems As List(Of SeqItem)

        Private _enuStatus As SeqStatus = SeqStatus.Idle
        Private _objPlayThread As Thread
        Private _objStatusLock As New Object   'used only for SyncLocking _enuStatus

        Public Sub New()
            _objSeqItems = New List(Of SeqItem)
        End Sub

#Region "Serialization"

        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
            Select Case CtcService.SerializationContent
                Case SerializationContent.Definition
                    _objSeqItems = info.GetValue("SeqItems", GetType(Object))
                Case SerializationContent.State
                    'sequences have no saved states
            End Select
        End Sub

        Friend Overrides Sub Initialize()
            MyBase.Initialize()
            Select Case CtcService.SerializationContent
                Case SerializationContent.Definition
                    Me.InitStoredState()
                Case SerializationContent.State
                    'sequences have no saved states
            End Select
        End Sub

        <SecurityPermission(SecurityAction.LinkDemand, Flags:=SecurityPermissionFlag.SerializationFormatter)> _
        Protected Overrides Sub GetObjectData(info As SerializationInfo, context As StreamingContext)
            MyBase.GetObjectData(info, context)
            Select Case CtcService.SerializationContent
                Case SerializationContent.Definition
                    info.AddValue("SeqItems", _objSeqItems)
                Case SerializationContent.State
                    'sequences have no saved states
            End Select
        End Sub

#End Region

#Region "Enumerations"

        Public Enum SeqStatus As Byte
            ''' <summary>The sequence is not currently recording nor playing.</summary>
            Idle
            ''' <summary>The sequence is currently recording.</summary>
            Recording
            ''' <summary>The sequence is currently playing.</summary>
            Playing
        End Enum

#End Region

#Region "Classes"

        ''' <summary>A sequence item which represents a packet to be played (sent to the Locoent network) at a given time.</summary>
        <Serializable()> Public NotInheritable Class SeqItem
            Implements ISerializable, IComparable(Of SeqItem)

            Private _dblTime As Double  'time in seconds
            Private _objPacket As Packet

            Public Sub New(dblTime As Double, objPacket As Packet)
                _dblTime = dblTime
                _objPacket = objPacket
            End Sub

            'Serialization -------------------------------------------------------------

            Protected Sub New(info As SerializationInfo, context As StreamingContext)
                'this occurs only for definitions
                _dblTime = info.GetDouble("Time")
                _objPacket = info.GetValue("Packet", GetType(Object))
            End Sub

            <SecurityPermission(SecurityAction.LinkDemand, Flags:=SecurityPermissionFlag.SerializationFormatter)> _
            Protected Sub GetObjectData(info As SerializationInfo, context As StreamingContext) Implements ISerializable.GetObjectData
                'this occurs only for definitions
                info.AddValue("Time", _dblTime)
                info.AddValue("Packet", _objPacket)
            End Sub

            '---------------------------------------------------------------------------

            ''' <summary>The time at which the associated Loconet <see cref="Packet" /> will be played.</summary>
            ''' <value>Time in seconds representing the event on the time line. For example a value of 4.045 means this event item will occur 4 seconds and 45 milliseconds after the first item in the time line, assuming the first item has a time value of 0.</value>
            Public Property Time() As Double
                Get
                    Return _dblTime
                End Get
                Set(Value As Double)
                    _dblTime = Value
                End Set
            End Property

            ''' <summary>The Loconet packet to be played at the associated <see cref="Time" />.</summary>
            Public Property Packet() As Packet
                Get
                    Return _objPacket
                End Get
                Set(Value As Packet)
                    _objPacket = Value
                End Set
            End Property

            ''' <summary>Used for sorting collections of <see cref="SeqItem" /> by time stamp.</summary>
            Private Function CompareTo(other As SeqItem) As Integer Implements System.IComparable(Of SeqItem).CompareTo
                Select Case Me.Time - other.Time
                    Case Is < 0
                        Return -1
                    Case Is > 0
                        Return 1
                    Case Else
                        Return 0
                End Select
            End Function

        End Class

#End Region

#Region "Overrides"

        Public Overrides Sub DeleteSelf()
            Me.Stop()
            MyBase.DeleteSelf()
            CtcService.Sequences.Remove(Me)
        End Sub

        Public Overrides Function ToString() As String
            Return "Sequence." & Me.Name
        End Function

#End Region

#Region "Configuration"

        ''' <summary>Gets the current status of the sequence. (Idle, Recording, or Playing)</summary>
        Public ReadOnly Property Status() As SeqStatus
            Get
                SyncLock _objStatusLock
                    Return _enuStatus
                End SyncLock
            End Get
        End Property

        ''' <summary>Gets a list of <see cref="SeqItem" /> objects associated with the sequence.</summary>
        ''' <remarks>
        ''' The returned list is a copy of the internal list which must remain immutable for thread safety.
        ''' For this reason any change in the returned list will not directly affect the sequence.
        ''' You must use <see cref="SetItems" /> in order to apply your changes back to the sequence.
        ''' </remarks>
        ''' <seealso cref="SetItems" />
        Public Function GetItems() As List(Of SeqItem)
            'returns a clone collection of the internal _objSeqItems so the user can't change it outside of our synclock control
            Dim objSeqItems As New List(Of SeqItem)
            SyncLock DirectCast(_objSeqItems, ICollection).SyncRoot
                For Each objItem As SeqItem In _objSeqItems
                    objSeqItems.Add(New SeqItem(objItem.Time, objItem.Packet.Clone))
                Next
            End SyncLock
            Return objSeqItems
        End Function

        ''' <summary>Sets the list of <see cref="SeqItem" /> objects associated with the sequence.</summary>
        ''' <returns>Returns <i>True</i> if items were successfully set; <i>False</i> if items could not be set because the sequence is currently in a recording state.</returns>
        ''' <remarks>During the set process, items will be ordered by their <see cref="SeqItem.Time" /> value and the whole time line may be shifted so that the first item will have a time of 0.</remarks>
        ''' <seealso cref="GetItems" />
        Public Function SetItems(objItems As List(Of SeqItem)) As Boolean
            'clone the given collection to the internal _objSeqItems
            'since the user can hold a reference to the given collection and posibly change it, cloning is needed for thread safety to ensure _objSeqItems is not changed outside of our synclock control
            SyncLock _objStatusLock
                Select Case _enuStatus
                    Case SeqStatus.Idle, SeqStatus.Playing
                        'we sort the items by time stamp because the PlayInvoke() method expects them to be in order 
                        objItems.Sort()

                        'make sure the sort did not introduce empty space in the front of the sequence
                        'also shift the time line if there are negative time entries
                        'the time of the first SeqItem should always be 0; if not shift time line over so first one is 0
                        If objItems.Count > 0 AndAlso objItems(0).Time <> 0 Then
                            Dim dblOffset As Double = objItems(0).Time
                            For Each objItem As SeqItem In objItems
                                objItem.Time -= dblOffset
                            Next
                        End If

                        'update internal _objSeqItems
                        SyncLock DirectCast(_objSeqItems, ICollection).SyncRoot
                            _objSeqItems.Clear()
                            For Each objItem As SeqItem In objItems
                                _objSeqItems.Add(New SeqItem(objItem.Time, objItem.Packet.Clone))
                            Next
                        End SyncLock

                        Return True

                    Case Else
                        Return False

                End Select
            End SyncLock
        End Function

        ''' <summary>Substitutes slot numbers, of packets, that target command station slots.</summary>
        ''' <returns>Returns <i>True</i> if slot numbers were successfully substituted; <i>False</i> if slot numbers could not be substituted because the sequence is currently in a recording state.</returns>
        ''' <remarks>If the command station's slot number, associated with the engine, targeted by your sequence, has been changed, this method can be used to update the obsolete slot numbers.</remarks>
        Public Function SubstituteSlot(bytOldSlot As Byte, bytNewSlot As Byte) As Boolean
            'todo: might be a good idea to validate the passed slot parameters
            SyncLock _objStatusLock
                Select Case _enuStatus
                    Case SeqStatus.Idle, SeqStatus.Playing
                        SyncLock DirectCast(_objSeqItems, ICollection).SyncRoot
                            For Each objItem As SeqItem In _objSeqItems
                                Select Case objItem.Packet.OpCode
                                    Case OpCodes.LOCO_SPD, OpCodes.LOCO_DIRF, OpCodes.LOCO_SND, OpCodes.LOCO_F912
                                        Dim objPacket As Object = objItem.Packet
                                        If objPacket.Slot = bytOldSlot Then objPacket.Slot = bytNewSlot
                                End Select
                            Next
                        End SyncLock

                        Return True

                    Case Else
                        Return False

                End Select
            End SyncLock
        End Function

#End Region

#Region "Operation"

        ''' <summary>Starts recording the sequence.</summary>
        ''' <returns>Returns <i>True</i> if record was successfully started; <i>False</i> if record could not be started because the sequence is currently in a recording or playing state.</returns>
        ''' <remarks>
        ''' A sequence recording will record Loconet packet traffic encountered whether created by the application or external devices. 
        ''' Any empty space (void of packet traffic) at the start and end of the recording will be automatically trimmed out.
        ''' </remarks>        
        ''' <seealso cref="Play" />
        ''' <seealso cref="[Stop]" />
        Public Function Record() As Boolean
            SyncLock _objStatusLock
                If _enuStatus = SeqStatus.Idle Then
                    _enuStatus = SeqStatus.Recording
                    SyncLock DirectCast(_objSeqItems, ICollection).SyncRoot
                        _objSeqItems.Clear()
                    End SyncLock
                    AddHandler CtcService.LoconetService.RxPacketOnWorkerThread, AddressOf RecordPacket
                Else
                    Return False
                End If
            End SyncLock

            Me.NotifyObjectChanged(Me)

            Return True
        End Function

        Private Sub RecordPacket(objPacket As Packet)
            Static dblStartTime As Double

            'only records certain type of packets
            Select Case objPacket.OpCode
                Case OpCodes.SW_REQ, OpCodes.LOCO_SPD, OpCodes.LOCO_DIRF, OpCodes.LOCO_SND, OpCodes.LOCO_F912
                    SyncLock DirectCast(_objSeqItems, ICollection).SyncRoot
                        If _objSeqItems.Count = 0 Then dblStartTime = objPacket.TimeStamp
                        Dim objRecPacket As Packet = objPacket.Clone   'clone the received packet so we keep the original received packet pristine for other RxPacket events 
                        objRecPacket.Tag = Nothing 'since for speed packets, the client throttle ID is held here to identify the packet source, we clear it to signify a different output source on playback 
                        _objSeqItems.Add(New SeqItem(objPacket.TimeStamp - dblStartTime, objRecPacket))
                    End SyncLock
            End Select
        End Sub

        '---------

        ''' <summary>Plays the sequence.</summary>
        ''' <returns>Returns <i>True</i> if play was successfully started; <i>False</i> if play could not be started because the sequence is currently in a playing or recording state.</returns>
        ''' <seealso cref="Record" />
        ''' <seealso cref="[Stop]" />
        Public Function Play() As Boolean
            SyncLock _objStatusLock
                If _enuStatus = SeqStatus.Idle Then
                    _enuStatus = SeqStatus.Playing
                    _objPlayThread = New Thread(AddressOf PlayInvoke)
                    With _objPlayThread
                        .IsBackground = True
                        .Name = "CtcSequencePlay"
                        .Priority = ThreadPriority.Normal
                        .Start(Me.GetItems())    'pass a clone of _objSeqItems so that it doesn't have to be locked for thread safety during the whole play sequence
                    End With
                Else
                    Return False
                End If
            End SyncLock

            Me.NotifyObjectChanged(Me)

            Return True
        End Function

        Private Sub PlayInvoke(objSeqItems As List(Of SeqItem))
            Dim objStopWatch As Stopwatch = Stopwatch.StartNew()
            For Each objItem As SeqItem In objSeqItems
                While objItem.Time > objStopWatch.Elapsed.TotalSeconds
                End While
                Select Case objItem.Packet.OpCode
                    Case OpCodes.LOCO_SPD, OpCodes.LOCO_DIRF, OpCodes.LOCO_SND, OpCodes.LOCO_F912
                        CtcService.LoconetService.TxPriorityPacket(objItem.Packet)
                    Case Else
                        CtcService.LoconetService.TxPacket(objItem.Packet)
                End Select
            Next
            SyncLock _objStatusLock
                _enuStatus = SeqStatus.Idle
            End SyncLock

            Me.NotifyObjectChanged(Me)
        End Sub

        '---------

        ''' <summary>Stops recording or playing the sequence.</summary>
        ''' <seealso cref="Record" />
        ''' <seealso cref="Play" />
        Public Sub [Stop]()
            SyncLock _objStatusLock
                Select Case _enuStatus
                    Case SeqStatus.Recording
                        RemoveHandler CtcService.LoconetService.RxPacketOnWorkerThread, AddressOf RecordPacket

                    Case SeqStatus.Playing
                        If _objPlayThread Is Nothing Then Exit Sub
                        With _objPlayThread
                            .Abort()
                            .Join()   'block until play thread ends
                        End With
                        _objPlayThread = Nothing

                    Case Else
                        Exit Sub

                End Select
                _enuStatus = SeqStatus.Idle
            End SyncLock

            Me.NotifyObjectChanged(Me)
        End Sub

#End Region

    End Class

    ''' <summary>A script that can be triggered by one or more events.</summary>
    ''' <remarks>Use this type of script object to enforce rules triggered by railroad events. Examples: a signaling system, enforcing security rules, etc.</remarks>
    <Serializable()> Public NotInheritable Class EventScript
        Inherits CtcObjectBase
        Implements IScript, IInvocationTarget

        'serialized
        Private _blnEnabled As Boolean = True
        Private _objEventBindings As EventsList
        Private _strScript As String = ""

        Public Sub New()
            _objEventBindings = New EventsList
        End Sub

#Region "Serialization"

        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
            If CtcService.SerializationContent = SerializationContent.Definition Then
                _blnEnabled = info.GetBoolean("Enabled")
                _objEventBindings = info.GetValue("BoundEvents", GetType(Object))
                _strScript = info.GetString("Script")
            End If
        End Sub

        Friend Overrides Sub PostDeserialize()
            MyBase.PostDeserialize()
            Select Case CtcService.SerializationContent
                Case SerializationContent.Definition
                    _objEventBindings.PostDeserialize()

                Case SerializationContent.State
                    'scripts have no saved states
            End Select
        End Sub

        Friend Overrides Sub Initialize()
            MyBase.Initialize()
            Select Case CtcService.SerializationContent
                Case SerializationContent.Definition
                    Me.InitStoredState()
                    _objEventBindings.Initialize()

                Case SerializationContent.State
                    'scripts have no saved states
            End Select
        End Sub

        <SecurityPermission(SecurityAction.LinkDemand, Flags:=SecurityPermissionFlag.SerializationFormatter)> _
        Protected Overrides Sub GetObjectData(info As SerializationInfo, context As StreamingContext)
            MyBase.GetObjectData(info, context)
            If CtcService.SerializationContent = SerializationContent.Definition Then
                info.AddValue("Enabled", _blnEnabled)
                info.AddValue("BoundEvents", _objEventBindings)
                info.AddValue("Script", _strScript)
            End If
        End Sub

#End Region

#Region "Classes"

        ''' <summary>A list of <see cref="EventScript.Event" /> objects.</summary>
        <Serializable()> Public NotInheritable Class EventsList
            Implements IEnumerable, ISerializable, IDeserializationComplete

            Private _objEventsList As List(Of [Event])

            'Constructors --------------------------------------------------------------

            Public Sub New()
                _objEventsList = New List(Of [Event])
            End Sub

            'Serialization -------------------------------------------------------------

            Protected Sub New(info As SerializationInfo, context As StreamingContext)
                If CtcService.SerializationContent = SerializationContent.Definition Then
                    _objEventsList = info.GetValue("_objEventsList", GetType(Object))
                End If
            End Sub

            Public Sub PostDeserialize() Implements IDeserializationComplete.PostDeserialize
                For Each objItem As [Event] In _objEventsList
                    objItem.PostDeserialize()
                Next
            End Sub

            Public Sub Initialize() Implements IDeserializationComplete.Initialize
                For Each objItem As [Event] In _objEventsList
                    objItem.Initialize()
                Next
            End Sub

            <SecurityPermission(SecurityAction.LinkDemand, Flags:=SecurityPermissionFlag.SerializationFormatter)> _
            Protected Sub GetObjectData(info As SerializationInfo, context As StreamingContext) Implements ISerializable.GetObjectData
                If CtcService.SerializationContent = SerializationContent.Definition Then
                    info.AddValue("_objEventsList", _objEventsList)
                End If
            End Sub

            '---------------------------------------------------------------------------

            Public ReadOnly Property Count() As Integer
                Get
                    Return _objEventsList.Count
                End Get
            End Property

            Public Function GetEnumerator() As IEnumerator Implements IEnumerable.GetEnumerator
                'allows For...Each usage
                Return _objEventsList.GetEnumerator()
            End Function

            Default Public ReadOnly Property Item(intIndex As Integer) As [Event]
                Get
                    Return _objEventsList(intIndex)
                End Get
            End Property

            Public Sub Clear()
                CtcIsStartedException.Check()
                _objEventsList.Clear()
            End Sub

            Public Sub Add(objEvent As [Event])
                CtcIsStartedException.Check()
                _objEventsList.Add(objEvent)
            End Sub

            Public Sub Remove(objEvent As [Event])
                CtcIsStartedException.Check()
                _objEventsList.Remove(objEvent)
            End Sub

            Public Sub RemoveAt(intIndex As Integer)
                CtcIsStartedException.Check()
                _objEventsList.RemoveAt(intIndex)
            End Sub

        End Class

        <Serializable()> Public NotInheritable Class [Event]
            Implements ISerializable, IDeserializationComplete
            Private _objOwner As ISupportsScriptEvents
            Private _strName As String

            Public Sub New(objOwner As ISupportsScriptEvents, strName As String)
                _objOwner = objOwner
                _strName = strName
            End Sub

            'Serialization -------------------------------------------------------------

            'deserialize into an Object type for compatibility
            Private _objTempOwner As Object

            Protected Sub New(info As SerializationInfo, context As StreamingContext)
                If CtcService.SerializationContent = SerializationContent.Definition Then
                    _objTempOwner = info.GetValue("_objComponent", GetType(Object))   'local variable name changed but kept the serialized name for compatibility
                    _strName = info.GetString("_strName")
                End If
            End Sub

            Public Sub PostDeserialize() Implements IDeserializationComplete.PostDeserialize
                'compatibility added 1/2/13
                'PkStatesList no longer implements ISupportsScriptEvents and should no longer be an event owner 
                If Not TypeOf _objTempOwner Is PkStatesList Then
                    'this is if layout is current; for compatibility look in Me.Initialize()
                    _objOwner = _objTempOwner
                End If
            End Sub

            Public Sub Initialize() Implements IDeserializationComplete.Initialize
                'compatibility added 1/2/13
                'PkStatesList no longer implements ISupportsScriptEvents and should no longer be an event owner  
                If TypeOf _objTempOwner Is PkStatesList Then
                    'this is set here because PkStatesList.Parent is not populated until the Initialize phase of CtcObjectBase
                    _objOwner = DirectCast(_objTempOwner, PkStatesList)._objParent
                    CtcService.CompatibilityMode = True
                End If
            End Sub

            <SecurityPermission(SecurityAction.LinkDemand, Flags:=SecurityPermissionFlag.SerializationFormatter)> _
            Protected Sub GetObjectData(info As SerializationInfo, context As StreamingContext) Implements ISerializable.GetObjectData
                If CtcService.SerializationContent = SerializationContent.Definition Then
                    info.AddValue("_objComponent", _objOwner)           'local variable name changed but kept the serialized name for compatibility
                    info.AddValue("_strName", _strName)
                End If
            End Sub

            '---------------------------------------------------------------------------

            Public ReadOnly Property Owner() As ISupportsScriptEvents
                Get
                    Return _objOwner
                End Get
            End Property

            Public ReadOnly Property Name() As String
                Get
                    Return _strName
                End Get
            End Property

            Public Overrides Function Equals(obj As Object) As Boolean
                If TypeOf obj Is [Event] Then
                    Dim objEvent As [Event] = obj
                    If objEvent.Owner Is _objOwner And objEvent.Name = _strName Then
                        Return True
                    Else
                        Return False
                    End If
                Else
                    Return False
                End If
            End Function

        End Class

#End Region

#Region "Overrides"

        Public Overrides Sub DeleteSelf()
            MyBase.DeleteSelf()
            CtcService.EventScripts.Remove(Me)
        End Sub

        Public Overrides Function ToString() As String
            Return "EventScript." & Me.Name
        End Function

#End Region

#Region "Configuration"

        ''' <summary>Gets or sets whether the script will be compiled and be made available for operation.</summary>
        ''' <remarks>This property must be set while the CtcService is stopped otherwise a <see cref="CtcIsStartedException" /> will be thrown.</remarks>
        Public Property Enabled() As Boolean Implements IScript.Enabled
            Get
                Return _blnEnabled
            End Get
            Set(Value As Boolean)
                If CtcService.EventScripts.Contains(Me) Then CtcIsStartedException.Check()
                _blnEnabled = Value
            End Set
        End Property

        ''' <summary>Gets the list of events that will trigger the execution of this script.</summary>
        Public ReadOnly Property EventBindings() As EventsList
            Get
                Return _objEventBindings
            End Get
        End Property

        ''' <summary>Gets or sets the user script to be executed.</summary>
        ''' <remarks>
        ''' Script code should use VB.NET syntax. The code is dynamically compiled when the CTC service is started.
        ''' This property must be set while the CtcService is stopped otherwise a <see cref="CtcIsStartedException" /> will be thrown. 
        ''' </remarks>
        Public Property Script() As String Implements IScript.Script
            Get
                Return _strScript
            End Get
            Set(Value As String)
                If CtcService.EventScripts.Contains(Me) Then CtcIsStartedException.Check()
                _strScript = Value
            End Set
        End Property

#End Region

#Region "Operation"

        Friend Property InvocTargetHandler() As Func(Of ScriptEventArgs, Task) Implements IInvocationTarget.Handler

        Friend Property InvocTargetName As String Implements IInvocationTarget.Name
            Get
                Return Me.ToString
            End Get
            Set(value As String)
                'do nothing because this invocation target has a static name while in operation
            End Set
        End Property

        ''' <summary>Executes the user script irrespective of the bound events.</summary>
        ''' <param name="objScriptEventArgs">The event arguments to be passed to the script function.</param>
        ''' <returns>An awaitable <see cref="Tasks.Task" />.</returns>
        ''' <remarks>The CtcService must be started otherwise a <see cref="CtcIsStoppedException" /> will be thrown.</remarks> 
        Public Function Execute(objScriptEventArgs As ScriptEventArgs) As Task

            If CompilerService.IsScriptBound Then
                If CtcService.IsCurrentThread Then
                    Return ScriptEventBinder.InvokeAwaitableHandler(Me, objScriptEventArgs)
                Else
                    CtcIsStoppedException.Check()

                    CtcService.Execute(Sub()
                                           ScriptEventBinder.InvokeHandler(Me, objScriptEventArgs)
                                       End Sub)
                End If
            Else
                CtcService.PostMessage(CtcService.MessageCat.Error, "Could not execute script. No script has been bound for this session.")
            End If

        End Function

#End Region

    End Class

    ''' <summary>A script that can be started, stopped and resumed from defined step points.</summary>
    ''' <remarks>Use this type of script object to sequentially execute a list of steps. Examples: running a locomotive, scripting actions of a craine, etc.</remarks>
    <Serializable()> Public NotInheritable Class StepScript
        Inherits CtcObjectBase
        Implements IScript, ISupportsScriptEvents

        'serialized
        Private _blnEnabled As Boolean = True
        Private _blnLoop As Boolean = False
        Private _strScript As String = ""

        Private _objSteps As New List(Of StepItem)          'didn't use Dictionary because we need to guarantee element order 
        Private _intStepPos As UInteger = 0
        Private _blnRunning As Boolean = False
        Private _blnStopReq As Boolean = False              'a stop script request was submitted
        Private _objCTS As CancellationTokenSource

        Public Sub New()
        End Sub

#Region "Serialization"

        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
            If CtcService.SerializationContent = SerializationContent.Definition Then
                _blnEnabled = info.GetBoolean("Enabled")
                _blnLoop = info.GetBoolean("Loop")
                _strScript = info.GetString("Script")
            End If
        End Sub

        Friend Overrides Sub Initialize()
            MyBase.Initialize()
            Select Case CtcService.SerializationContent
                Case SerializationContent.Definition
                    Me.InitStoredState()

                Case SerializationContent.State
                    'StepScript have no saved states
            End Select
        End Sub

        <SecurityPermission(SecurityAction.LinkDemand, Flags:=SecurityPermissionFlag.SerializationFormatter)> _
        Protected Overrides Sub GetObjectData(info As SerializationInfo, context As StreamingContext)
            MyBase.GetObjectData(info, context)
            If CtcService.SerializationContent = SerializationContent.Definition Then
                info.AddValue("Enabled", _blnEnabled)
                info.AddValue("Loop", _blnLoop)
                info.AddValue("Script", _strScript)
            End If
        End Sub

#End Region

        Private Structure StepItem
            Friend Property Name As String
            Friend Property Lambda As [Delegate]
        End Structure

#Region "Overrides"

        Public Overrides Sub DeleteSelf()
            MyBase.DeleteSelf()
            CtcService.StepScripts.Remove(Me)
        End Sub

        Public Overrides Function ToString() As String
            Return "StepScript." & Me.Name
        End Function

        Friend Overrides Sub BeforeCtcServiceStop()
            _blnStopReq = True
            Try
                _objCTS.Cancel()
            Catch
                '_objCTS could be Nothing or set to Nothing concurently by the CTC thread
            End Try
        End Sub

        Friend Overrides Sub AfterCtcServiceStop()
            If _objCTS IsNot Nothing Then
                _objCTS.Dispose()
                _objCTS = Nothing
            End If
            _blnRunning = False
            _objSteps.Clear()
            _intStepPos = 0
            Me.NotifyObjectChanged(Me)
        End Sub

#End Region

#Region "Configuration"

        ''' <summary>Gets or sets whether the script will be compiled and be made available for operation.</summary>
        ''' <remarks>This property must be set while the CtcService is stopped otherwise a <see cref="CtcIsStartedException" /> will be thrown.</remarks>
        Public Property Enabled() As Boolean Implements IScript.Enabled
            Get
                Return _blnEnabled
            End Get
            Set(Value As Boolean)
                If CtcService.StepScripts.Contains(Me) Then CtcIsStartedException.Check()
                _blnEnabled = Value
            End Set
        End Property

        ''' <summary>Gets or sets whether the script should restart from the beginning when it reaches the end.</summary>
        Public Property [Loop]() As Boolean
            Get
                Return _blnLoop
            End Get
            Set(Value As Boolean)
                'this is safe to be set in both edit/operation mode and cross thread 
                _blnLoop = Value
            End Set
        End Property

        ''' <summary>Gets or sets the user script used to initialize the execution steps.</summary>
        ''' <remarks>
        ''' Script code should use VB.NET syntax. The code is dynamically compiled when the CTC service is started.
        ''' This property must be set while the CtcService is stopped otherwise a <see cref="CtcIsStartedException" /> will be thrown. 
        ''' </remarks>
        Public Property Script() As String Implements IScript.Script
            Get
                Return _strScript
            End Get
            Set(Value As String)
                If CtcService.StepScripts.Contains(Me) Then CtcIsStartedException.Check()
                _strScript = Value
            End Set
        End Property

        ''' <summary>Adds a step definition to the step script list.</summary>
        ''' <param name="strName">A user friendly name to uniquely identify the step.</param>
        ''' <param name="delLambda">The lambda function that constitutes the step's actions.</param>
        ''' <remarks>This method should only be used to define script steps. Never call this function from within the step's lambda or from other scripts.</remarks>
        Public Sub [Step](strName As String, delLambda As [Delegate])

            If CtcService.IsStarted Then
                'don't allow the step definitions to be altered after the CTC has started (prevents users from adding steps through other scripts)
                CtcService.PostMessage(CtcService.MessageCat.Native, String.Format("Attempt to re-define [{0}] is not allowed.", Me.ToString))
            Else
                If strName Is Nothing OrElse strName.Trim = Nothing Then strName = "Step" & _objSteps.Count + 1
                _objSteps.Add(New StepItem() With {.Name = strName, .Lambda = delLambda})
            End If

        End Sub

        ''' <summary>Gets a list of previously defined step names.</summary>
        Public ReadOnly Property StepNames() As List(Of String)
            Get
                'readonly should be safe from any thread 
                Dim objNames As New List(Of String)
                For Each sctStep As StepItem In _objSteps
                    objNames.Add(sctStep.Name)
                Next
                Return objNames
            End Get
        End Property

#End Region

#Region "Operation"

        ''' <summary>Gets a value indicating whether the step script is currently running.</summary>
        Public ReadOnly Property Running() As Boolean
            Get
                Return _blnRunning
            End Get
        End Property

        ''' <summary>Gets the step position at which execution is or will occur next.</summary>
        ''' <value>The zero-based index of the step.</value>
        Public ReadOnly Property StepPos() As UInteger
            Get
                Return _intStepPos
            End Get
        End Property

        ''' <summary>Gets the step name associated with the <see cref="StepPos" /> index.</summary>
        ''' <value>The user given name of the step.</value>
        Public ReadOnly Property StepPosName() As String
            Get
                Return If(_objSteps.Count = 0, "(Undefined)", _objSteps(_intStepPos).Name)
            End Get
        End Property

        ''' <summary>Gets the cancellation token of the running script that can be used to cancel individial script methods.</summary>
        ''' <value>The token is only valid when the script is running.</value>
        ''' <remarks>This token is placed in a calceled state when the script is stopped. A new one is auto-generated every time the script is started.</remarks>
        Public ReadOnly Property CancelToken() As CancellationToken
            Get
                Try
                    Return _objCTS.Token
                Catch                    
                    'could error if _objCTS is either Nothing or disposed
                    'on error we return an empty non-cancelable token
                End Try
            End Get
        End Property

        ''' <summary>Starts script execution at a given step position.</summary>
        ''' <param name="intIdx">The zero-based index of the step to start from.</param>
        ''' <returns>An awaitable <see cref="Tasks.Task" />.</returns>
        ''' <remarks>The CtcService must be started otherwise a <see cref="CtcIsStoppedException" /> will be thrown.</remarks> 
        Public Function StartAt(intIdx As UInteger) As Task

            If CtcService.IsCurrentThread Then
                Return StartAtOnCtc(intIdx)
            Else
                CtcIsStoppedException.Check()

                Dim objTcs As New TaskCompletionSource()
                CtcService.Execute(Async Sub()
                                       Await StartAtOnCtc(intIdx)
                                       objTcs.TrySetResult()
                                   End Sub)
                Return objTcs.Task
            End If

        End Function

        Private Async Function StartAtOnCtc(intIdx As UInteger) As Task

            If Not _blnEnabled Then
                CtcService.PostMessage(CtcService.MessageCat.Native, String.Format("[{0}] can't be started. Script is disabled.", Me.ToString))
                Exit Function
            End If

            If intIdx > _objSteps.Count - 1 Then
                CtcService.PostMessage(CtcService.MessageCat.Native, String.Format("[{0}] can't be started. Step index [{1}] is invalid.", Me.ToString, intIdx))
                Exit Function
            End If

            Dim objScriptEventArgs As New ScriptEventArgs(Me, "BeforeStart", _objSteps(intIdx).Name, Nothing)
            Await Me.ScriptEventBinder.InvokeHandlersOnCtc(objScriptEventArgs)
            If objScriptEventArgs.ReqCancel.Value Then
                CtcService.PostMessage(CtcService.MessageCat.Native, String.Format("[{0}] was not started. Canceled by user script.", Me.ToString))
                Exit Function
            End If

            'this check not placed before the "BeforeStart" invocation because the invocation could invalidate this validation
            If _blnRunning Then
                CtcService.PostMessage(CtcService.MessageCat.Native, String.Format("[{0}] is already running.", Me.ToString))
                Exit Function
            End If

            _blnRunning = True
            _blnStopReq = False
            _objCTS = New CancellationTokenSource
            _intStepPos = intIdx
            Me.NotifyObjectChanged(Me)
            CtcService.PostMessage(CtcService.MessageCat.Native, String.Format("[{0}] was started at step [{1}].", Me.ToString, _objSteps(intIdx).Name))
            Await Me.ScriptEventBinder.InvokeHandlersOnCtc(New ScriptEventArgs(Me, "Started", Nothing, Nothing))
            Await Me.Execute()

        End Function

        Private Async Function Execute() As Task

            'execute step at current position
            Dim objTask As Task
            Try
                objTask = _objSteps(_intStepPos).Lambda.DynamicInvoke()
                If objTask IsNot Nothing Then Await objTask
            Catch ex As TaskCanceledException
                'do nothing since this is a normal condition
            Catch ex As Exception
                CtcService.PostMessage(CtcService.MessageCat.Error,
                    String.Format("Runtime error occurred in [{0}] at step [{1}]: {2}", Me, _objSteps(_intStepPos).Name, If(ex.InnerException Is Nothing, ex.Message, ex.InnerException.Message)))
            End Try

            'see if a stop request was issued while the Awaits of the just executed step handed off the CTC thread to other queued items
            If _blnStopReq Then
                'end further execution because of stop order
                _objCTS.Dispose()
                _objCTS = Nothing
                _blnRunning = False
                Me.NotifyObjectChanged(Me)
                CtcService.PostMessage(CtcService.MessageCat.Native, String.Format("[{0}] was stopped in step [{1}].", Me.ToString, _objSteps(_intStepPos).Name))
                Await Me.ScriptEventBinder.InvokeHandlersOnCtc(New ScriptEventArgs(Me, "Stopped", Nothing, Nothing))
            Else
                'process the next step in list
                If _intStepPos >= _objSteps.Count - 1 Then
                    'we've just executed the last step in the list so reset the position to the beginning
                    _intStepPos = 0
                    Me.NotifyObjectChanged(Me)

                    If Me.Loop Then
                        'execute from first step
                        Await Me.Execute()
                    Else
                        'end further execution because of completion
                        _objCTS.Cancel()
                        _objCTS.Dispose()
                        _objCTS = Nothing
                        _blnRunning = False
                        Me.NotifyObjectChanged(Me)
                        CtcService.PostMessage(CtcService.MessageCat.Native, String.Format("[{0}] completed normally.", Me.ToString))
                        Await Me.ScriptEventBinder.InvokeHandlersOnCtc(New ScriptEventArgs(Me, "Completed", Nothing, Nothing))
                    End If
                Else
                    'move execution position forward
                    _intStepPos += 1
                    Me.NotifyObjectChanged(Me)

                    'execute the next step
                    Await Me.Execute()
                End If
            End If

        End Function

        ''' <summary>Stops script execution.</summary>
        Public Sub [Stop]()
            If CtcService.IsCurrentThread Then
                StopOnCtc()
            Else
                CtcIsStoppedException.Check()
                CtcService.Execute(AddressOf StopOnCtc)
            End If
        End Sub

        Private Sub StopOnCtc()
            'has to run on the CTC thread because the state changes made here can only be safely injected during the Awaits of the step script's user code
            'where the CTC thread hands off thread time to jobs put on its queue, like this one

            If _blnRunning Then
                If _blnStopReq Then
                    CtcService.PostMessage(CtcService.MessageCat.Native, String.Format("[{0}] is already being stopped.", Me.ToString))
                Else
                    _blnStopReq = True
                    _objCTS.Cancel()
                End If
            Else
                CtcService.PostMessage(CtcService.MessageCat.Native, String.Format("[{0}] is already stopped.", Me.ToString))
            End If
        End Sub

#End Region

#Region "Scripting Events"

        Private _objScriptEventBinder As New ScriptEventBinder(Me, {
            "BeforeStart",
            "Started",
            "Stopped",
            "Completed"
        })

        Public ReadOnly Property ScriptEventBinder As ScriptEventBinder Implements ISupportsScriptEvents.ScriptEventBinder
            Get
                Return _objScriptEventBinder
            End Get
        End Property

#End Region

    End Class

    ''' <summary>A script containing shared classes, methods, and variable declarations.</summary>
    ''' <remarks>Use this type of script object for common code accessible from all other scripts. Examples: behavioral class definitions, shared functions, shared variables, etc.</remarks>
    <Serializable()> Public NotInheritable Class GlobalScript
        Inherits CtcObjectBase
        Implements IScript

        'serialized
        Private _blnEnabled As Boolean = True
        Private _strScript As String = ""

        Public Sub New()
        End Sub

#Region "Serialization"

        Protected Sub New(info As SerializationInfo, context As StreamingContext)
            MyBase.New(info, context)
            If CtcService.SerializationContent = SerializationContent.Definition Then
                _blnEnabled = info.GetBoolean("Enabled")
                _strScript = info.GetString("Script")
            End If
        End Sub

        Friend Overrides Sub Initialize()
            MyBase.Initialize()
            Select Case CtcService.SerializationContent
                Case SerializationContent.Definition
                    Me.InitStoredState()

                Case SerializationContent.State
                    'StepScript have no saved states
            End Select
        End Sub

        <SecurityPermission(SecurityAction.LinkDemand, Flags:=SecurityPermissionFlag.SerializationFormatter)> _
        Protected Overrides Sub GetObjectData(info As SerializationInfo, context As StreamingContext)
            MyBase.GetObjectData(info, context)
            If CtcService.SerializationContent = SerializationContent.Definition Then
                info.AddValue("Enabled", _blnEnabled)
                info.AddValue("Script", _strScript)
            End If
        End Sub

#End Region

#Region "Overrides"

        Public Overrides Sub DeleteSelf()
            MyBase.DeleteSelf()
            CtcService.GlobalScripts.Remove(Me)
        End Sub

        Public Overrides Function ToString() As String
            Return "GlobalScript." & Me.Name
        End Function

#End Region

#Region "Configuration"

        ''' <summary>Gets or sets whether the script will be compiled and be made available for operation.</summary>
        ''' <remarks>This property must be set while the CtcService is stopped otherwise a <see cref="CtcIsStartedException" /> will be thrown.</remarks>
        Public Property Enabled() As Boolean Implements IScript.Enabled
            Get
                Return _blnEnabled
            End Get
            Set(Value As Boolean)
                If CtcService.GlobalScripts.Contains(Me) Then CtcIsStartedException.Check()
                _blnEnabled = Value
            End Set
        End Property

        ''' <summary>Gets or sets the user script global declarations.</summary>
        ''' <remarks>
        ''' Script code should use VB.NET syntax. The code is dynamically compiled when the CTC service is started.
        ''' This property must be set while the CtcService is stopped otherwise a <see cref="CtcIsStartedException" /> will be thrown. 
        ''' </remarks>
        Public Property Script() As String Implements IScript.Script
            Get
                Return _strScript
            End Get
            Set(Value As String)
                If CtcService.GlobalScripts.Contains(Me) Then CtcIsStartedException.Check()
                _strScript = Value
            End Set
        End Property

#End Region

    End Class

End Namespace

