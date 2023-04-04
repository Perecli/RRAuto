Imports System.Collections.ObjectModel
Imports System.ComponentModel
Imports RRAutoLib.CTC
Imports RRAutoLib.Loconet
Imports RRAuto.DrawHelper

Namespace CustomControls

    ''' <summary>ThreeView with some added functionality</summary>    
    Public Class TreeViewX
        Inherits TreeView

        Public Event NodeDoubleClick(objNode As TreeViewItem)

        Protected Overrides Sub OnMouseDoubleClick(e As MouseButtonEventArgs)
            MyBase.OnMouseDoubleClick(e)

            Dim objNode As TreeViewItem = ParseForNode(e)
            If objNode IsNot Nothing Then OnNodeDoubleClick(objNode)
        End Sub

        Protected Overridable Sub OnNodeDoubleClick(objNode As TreeViewItem)
            RaiseEvent NodeDoubleClick(objNode)
        End Sub

        Public Shared Function ParseForNode(e As RoutedEventArgs) As TreeViewItem

            Dim objDep As DependencyObject = e.OriginalSource

            'walk up the visual tree until a node is found or no node is found
            While objDep IsNot Nothing AndAlso Not TypeOf objDep Is TreeViewItem
                objDep = VisualTreeHelper.GetParent(objDep)
            End While

            Return objDep

        End Function

    End Class

    ''' <summary>ListView with some added functionality</summary>    
    Public Class ListViewX
        Inherits ListView

        Public Event ItemDoubleClick(objDataItem As Object)

        Protected Overrides Sub OnMouseDoubleClick(e As MouseButtonEventArgs)
            MyBase.OnMouseDoubleClick(e)

            Dim objItem As ListViewItem = ParseForItem(e)
            If objItem IsNot Nothing Then OnItemDoubleClick(objItem.Content)
        End Sub

        Protected Overridable Sub OnItemDoubleClick(objDataItem As Object)
            RaiseEvent ItemDoubleClick(objDataItem)
        End Sub

        Public Shared Function ParseForItem(e As RoutedEventArgs) As ListViewItem

            Dim objDep As DependencyObject = e.OriginalSource

            'walk up the visual tree until a node is found or no node is found
            While objDep IsNot Nothing AndAlso Not TypeOf objDep Is ListViewItem
                objDep = VisualTreeHelper.GetParent(objDep)
            End While

            Return objDep

        End Function

    End Class

#Region "TreeView - direct node manipulation"

    ''' <summary>Basic treeview node with just text.</summary>
    Friend Class TreeViewNode1
        Inherits TreeViewItem

        Protected _objTextBlock As New TextBlock

        Public Sub New()
            _objTextBlock.VerticalAlignment = VerticalAlignment.Center
            Me.Header = _objTextBlock
        End Sub

        Public Sub New(strText As String, Optional objTag As Object = Nothing)
            Me.New()
            Me.Text = strText
            Me.Tag = objTag
        End Sub

        Public Property Text() As String
            Get
                Return _objTextBlock.Text
            End Get
            Set(value As String)
                _objTextBlock.Text = value
            End Set
        End Property

        ''' <remarks>
        ''' The base class' FontWeight property is crap as its value get inherited by all child nodes. 
        ''' So I added this property which behaves as expected.
        ''' </remarks>
        Public Property TextWeight() As FontWeight
            Get
                Return _objTextBlock.FontWeight
            End Get
            Set(value As FontWeight)
                _objTextBlock.FontWeight = value
            End Set
        End Property

        Public ReadOnly Property ContextMenuPlacementTarget() As UIElement
            Get
                Return _objTextBlock
            End Get
        End Property

        Public Sub Sort()
            'doing a SortDescriptions.Add() triggers the sort however that sort is static in that node inserts and updates do not retrigger the sort
            'but if we call SortDescriptions.Add() everytime we need sorting something seems to compound internally that make the sort slower and slower with each call
            'to solve this I call SortDescriptions.Clear() before the SortDescriptions.Add() call
            With Me.Items.SortDescriptions
                .Clear()
                .Add(New SortDescription("Text", ListSortDirection.Ascending)) 'warning: this is super slow on large sets so call it as little as possible
            End With
        End Sub

    End Class

    ''' <summary>This treeview node has an image and text.</summary>
    Friend Class TreeViewNode2
        Inherits TreeViewNode1

        Private _objImage As New Image

        Public Sub New()
            Dim objStack As New StackPanel
            objStack.Orientation = Orientation.Horizontal
            Me.Header = objStack

            'add icon to stack
            _objImage.Width = 16
            _objImage.Height = 16
            _objImage.VerticalAlignment = VerticalAlignment.Center
            _objImage.Margin = New Thickness(0, 0, 4, 0)
            objStack.Children.Add(_objImage)

            'add text to stack
            _objTextBlock.VerticalAlignment = VerticalAlignment.Center
            objStack.Children.Add(_objTextBlock)
        End Sub

        Public Sub New(objImageSource As ImageSource, strText As String, Optional objTag As Object = Nothing)
            Me.New()
            Me.ImageSource = objImageSource
            Me.Text = strText
            Me.Tag = objTag
        End Sub

        Public Property ImageSource() As ImageSource
            Get
                Return _objImage.Source
            End Get
            Set(value As ImageSource)
                _objImage.Source = value
            End Set
        End Property

    End Class

#End Region

#Region "TreeView - Model-View-ViewModel binding (not currently used)"

    ''' <summary>Creates a sorted CollectionView from the children of a TreeViewItemViewModel.</summary>
    <ValueConversion(GetType(IList), GetType(IEnumerable))>
    Public Class TreeViewItemSortConverter
        Implements IValueConverter

        Public Function Convert(value As Object, targetType As Type, parameter As Object, culture As Globalization.CultureInfo) As Object Implements IValueConverter.Convert
            Dim objColView As New ListCollectionView(value)
            objColView.SortDescriptions.Add(New SortDescription(parameter.ToString, ListSortDirection.Ascending))
            Return objColView
        End Function

        Public Function ConvertBack(value As Object, targetType As Type, parameter As Object, culture As Globalization.CultureInfo) As Object Implements IValueConverter.ConvertBack
            Throw New NotImplementedException()
        End Function

    End Class

    ''' <summary>ViewModel base class for binding data to TreeViewItems.</summary>
    Public Class TreeViewItemViewModel
        Implements INotifyPropertyChanged

        Protected _objParent As TreeViewItemViewModel
        Protected _objChildren As New ObservableCollection(Of TreeViewItemViewModel)
        Private _blnExpanded As Boolean
        Private _blnSelected As Boolean

        Public ReadOnly Property Parent() As TreeViewItemViewModel
            Get
                Return _objParent
            End Get
        End Property

        Public ReadOnly Property Children() As ObservableCollection(Of TreeViewItemViewModel)
            Get
                Return _objChildren
            End Get
        End Property


        Public Property IsExpanded() As Boolean
            Get
                Return _blnExpanded
            End Get
            Set(value As Boolean)
                If value <> _blnExpanded Then
                    _blnExpanded = value
                    Me.OnPropertyChanged("IsExpanded")
                End If

                'expand all the way up to the root
                If _blnExpanded AndAlso _objParent IsNot Nothing Then
                    _objParent.IsExpanded = True
                End If
            End Set
        End Property

        Public Property IsSelected() As Boolean
            Get
                Return _blnSelected
            End Get
            Set(value As Boolean)
                If value <> _blnSelected Then
                    _blnSelected = value
                    Me.OnPropertyChanged("IsSelected")
                End If

                'expand parent of selected node in case it is hidden
                If _blnSelected AndAlso _objParent IsNot Nothing Then
                    _objParent.IsExpanded = True
                End If
            End Set
        End Property


        Public Overridable ReadOnly Property Name() As String
            Get
                Return "Unnamed"
            End Get
        End Property

        Public Overridable ReadOnly Property Icon() As ImageSource
            Get
                Return My.Application.TryFindResource("IconPlaceHolder")
            End Get
        End Property

        Public Overridable ReadOnly Property DataItem As Object
            Get
            End Get
        End Property


        Public Sub Refresh()
            Me.OnPropertyChanged("Name")
            Me.OnPropertyChanged("Icon")

            'refresh all downstream
            For Each objItem As TreeViewItemViewModel In _objChildren
                objItem.Refresh()
            Next
        End Sub

        Public Sub Delete()
            If _objParent IsNot Nothing Then _objParent._objChildren.Remove(Me)
        End Sub

        '-------------------------

        Public Event PropertyChanged(sender As Object, e As PropertyChangedEventArgs) Implements INotifyPropertyChanged.PropertyChanged

        Protected Overridable Sub OnPropertyChanged(propertyName As String)
            RaiseEvent PropertyChanged(Me, New PropertyChangedEventArgs(propertyName))
        End Sub

    End Class


    ''' <summary>Root ViewModel for CtcService collection types.</summary>
    Public Class CtcRootViewModel

        Private _objCtcLists As List(Of CtcListViewModel)

        Public Sub New()
            _objCtcLists = New List(Of CtcListViewModel) From {
                New CtcListViewModel(CtcService.Tracks, "Tracks", "IconTrack"),
                New CtcListViewModel(CtcService.Blocks, "Blocks", "IconBlock"),
                New CtcListViewModel(CtcService.Routes, "Routes", "IconRoute"),
                New CtcListViewModel(CtcService.Sensors, "Sensors", "IconSensor"),
                New CtcListViewModel(CtcService.Signals, "Signals", "IconSignal"),
                New CtcListViewModel(CtcService.Accessories, "Accessories", "IconAccessory"),
                New CtcListViewModel(CtcService.Engines, "Engines", "IconEngine"),
                New CtcListViewModel(CtcService.Sequences, "Sequences", "IconCronometer"),
                New CtcListViewModel(CtcService.EventScripts, "Event Scripts", "IconDocEvent")
            }
        End Sub

        Public ReadOnly Property Root() As List(Of CtcListViewModel)
            Get
                Return _objCtcLists
            End Get
        End Property


        Public Function FindChild(objObject As ICtcObjectListBase) As CtcListViewModel
            For Each objViewModel As CtcListViewModel In _objCtcLists
                If objViewModel.DataItem Is objObject Then Return objViewModel
            Next
        End Function

        Public Function FindChild(objObject As CtcObjectBase) As CtcListViewModel
            For Each objViewModel As CtcListViewModel In _objCtcLists
                If objViewModel.DataItem Is objObject.Parent Then Return objViewModel
            Next
        End Function

    End Class

    ''' <summary>TreeViewItem ViewModel for ICtcObjectListBase data types.</summary>
    Public Class CtcListViewModel
        Inherits TreeViewItemViewModel

        Private _objCtcList As ICtcObjectListBase
        Private _strName As String
        Private _strIconResource As String

        Public Sub New(objCtcList As ICtcObjectListBase, strName As String, strIconResource As String)
            _objCtcList = objCtcList
            _strName = strName
            _strIconResource = strIconResource

            'load children
            For Each objCtcObject As CtcObjectBase In _objCtcList
                _objChildren.Add(New CtcObjViewModel(Me, objCtcObject))
            Next
        End Sub


        Public Overrides ReadOnly Property Name() As String
            Get
                Return _strName
            End Get
        End Property

        Public Overrides ReadOnly Property Icon As ImageSource
            Get
                Return My.Application.TryFindResource(_strIconResource)
            End Get
        End Property

        Public Overrides ReadOnly Property DataItem As Object
            Get
                Return _objCtcList
            End Get
        End Property


        Public Function FindChild(objObject As CtcObjectBase) As CtcObjViewModel
            For Each objViewModel As CtcObjViewModel In _objChildren
                If objViewModel.DataItem Is objObject Then Return objViewModel
            Next
        End Function

        Public Sub AddChild(objCtcObject As CtcObjectBase)
            _objChildren.Add(New CtcObjViewModel(Me, objCtcObject))
        End Sub

    End Class

    ''' <summary>TreeViewItem ViewModel for CtcObjectBase data types.</summary>
    Public Class CtcObjViewModel
        Inherits TreeViewItemViewModel

        Private _objCtcObject As CtcObjectBase

        Public Sub New(objParent As TreeViewItemViewModel, objCtcObject As CtcObjectBase)
            _objParent = objParent
            _objCtcObject = objCtcObject

            'load children
            If TypeOf _objCtcObject Is IPkStates Then
                If (TypeOf _objCtcObject Is Track AndAlso DirectCast(_objCtcObject, Track).IsTurnout) Or Not TypeOf _objCtcObject Is Track Then
                    For Each objState As PkStatesList.State In CType(_objCtcObject, IPkStates).States
                        _objChildren.Add(New StateViewModel(Me, objState))
                    Next
                End If
            End If
        End Sub


        Public Overrides ReadOnly Property Name() As String
            Get
                Return _objCtcObject.Name
            End Get
        End Property

        Public Overrides ReadOnly Property Icon As ImageSource
            Get
                Select Case True
                    Case TypeOf _objCtcObject Is Track
                        Return GetTrackDrawingImage(_objCtcObject)

                    Case TypeOf _objCtcObject Is Block
                        Return My.Application.TryFindResource("IconBlock")

                    Case TypeOf _objCtcObject Is Route
                        Select Case DirectCast(_objCtcObject, Route).State
                            Case Route.RouteState.Unlocked
                                Return My.Application.TryFindResource("IconRoute")
                            Case Route.RouteState.Locked
                                Return My.Application.TryFindResource("IconRouteLocked")
                            Case Route.RouteState.Pending
                                Return My.Application.TryFindResource("IconRoutePending")
                        End Select

                    Case TypeOf _objCtcObject Is Sensor
                        If DirectCast(_objCtcObject, Sensor).State = OnOff.On Then
                            Return My.Application.TryFindResource("IconSensor")
                        Else
                            Return My.Application.TryFindResource("IconSensorOff")
                        End If

                    Case TypeOf _objCtcObject Is Signal
                        Return GetSignalDrawingImage(_objCtcObject)

                    Case TypeOf _objCtcObject Is Accessory
                        Return My.Application.TryFindResource("IconAccessory")

                    Case TypeOf _objCtcObject Is Engine
                        Return My.Application.TryFindResource("IconEngine")

                    Case TypeOf _objCtcObject Is Sequence
                        Select Case DirectCast(_objCtcObject, Sequence).Status
                            Case Sequence.SeqStatus.Idle
                                Return My.Application.TryFindResource("IconCronometer")
                            Case Sequence.SeqStatus.Recording
                                Return My.Application.TryFindResource("IconCronRecord")
                            Case Sequence.SeqStatus.Playing
                                Return My.Application.TryFindResource("IconCronPlay")
                        End Select

                    Case TypeOf _objCtcObject Is EventScript
                        Return My.Application.TryFindResource("IconDocEvent")

                    Case Else
                        Return MyBase.Icon
                End Select
            End Get
        End Property

        Public Overrides ReadOnly Property DataItem As Object
            Get
                Return _objCtcObject
            End Get
        End Property

    End Class

    ''' <summary>TreeViewItem ViewModel for PkStatesList.State data types.</summary>
    Public Class StateViewModel
        Inherits TreeViewItemViewModel

        Private _objState As PkStatesList.State

        Public Sub New(objParent As TreeViewItemViewModel, objState As PkStatesList.State)
            _objParent = objParent
            _objState = objState

            'load children
            For Each objPacket As Packet In _objState
                _objChildren.Add(New PacketViewModel(Me, objPacket))
            Next
        End Sub


        Public Overrides ReadOnly Property Name() As String
            Get
                Return _objState.Name
            End Get
        End Property

        Public Overrides ReadOnly Property Icon As ImageSource
            Get
                Return My.Application.TryFindResource("State")
            End Get
        End Property

        Public Overrides ReadOnly Property DataItem As Object
            Get
                Return _objState
            End Get
        End Property

    End Class

    ''' <summary>TreeViewItem ViewModel for Packet data types.</summary>
    Public Class PacketViewModel
        Inherits TreeViewItemViewModel

        Private _objPacket As Packet

        Public Sub New(objParent As TreeViewItemViewModel, objPacket As Packet)
            _objParent = objParent
            _objPacket = objPacket
        End Sub


        Public Overrides ReadOnly Property Name() As String
            Get
                Return _objPacket.Description
            End Get
        End Property

        Public Overrides ReadOnly Property Icon As ImageSource
            Get
                Select Case True
                    Case TypeOf _objPacket Is PkSetSwitch
                        Return My.Application.TryFindResource("IconSwitch")
                    Case TypeOf _objPacket Is PkInput
                        Return My.Application.TryFindResource("IconSensor")
                    Case TypeOf _objPacket Is PkImmediate
                        Return My.Application.TryFindResource("IconDecoder")
                    Case TypeOf _objPacket Is PkLocoIO
                        Return My.Application.TryFindResource("Device")
                End Select
            End Get
        End Property

        Public Overrides ReadOnly Property DataItem As Object
            Get
                Return _objPacket
            End Get
        End Property

    End Class

#End Region

End Namespace
