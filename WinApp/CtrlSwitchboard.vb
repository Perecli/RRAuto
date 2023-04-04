Imports System.Runtime.CompilerServices
Imports System.Windows.Media
Imports System.Windows
Imports System.Windows.Controls
Imports RRAutoLib.Loconet
Imports RRAutoLib.CTC
Imports RRAuto.DrawHelper

Namespace CustomControls

    Public Class Switchboard
        Inherits FrameworkElement

        Private _objConfig As LayoutConfig.SwitchboardConfig    'current zoom scale and current panning position

        Private _objScaleTransform As New ScaleTransform
        Private _objPanTransform As New TranslateTransform
        Private _blnIsPanning As Boolean                        'true while mouse panning is occuring
        Private _sctMouseDownPoint As Point = Nothing           'mouse down point used for mouse drag panning
        Private _sctMouseDownPanPos As New Point()              'panning position at mouse down used for mouse drag panning

        Private _objVisuals As New Visuals(Me)                  'drawing visual tree manager
        Private _sctCurrentCell As Cell                         'represents the cell that the mouse is hovering over
        Private _sctCurrentPoint As Point                       'represents the point that the mouse is hovering over

        Friend Event CellLocChanged(sctLocation As Location)

#Region "Classes/Structures/Enums"

        Friend Structure Cell
            Private _sctLocation As Location

            Friend ReadOnly Property Location() As Location
                Get
                    Return _sctLocation
                End Get
            End Property

            Friend Function SetLocation(sctPos As Point) As Boolean
                Dim srtCol, srtRow As Short
                'note: an error could occur here if the sctPos translates to a srtCol/srtRow values outside the short data type range
                srtCol = Math.Floor(sctPos.X / SbConst.CellSize.Width + 1)
                srtRow = Math.Floor(sctPos.Y / SbConst.CellSize.Height + 1)

                'return true if location changed
                If srtCol <> _sctLocation.Col Or srtRow <> _sctLocation.Row Then
                    _sctLocation.Col = srtCol
                    _sctLocation.Row = srtRow
                    Return True
                End If
            End Function

            ''' <summary>Gets the coordinate rectangle of this cell's location.</summary>
            Friend Function Rect() As Rect
                'this method is not currently used but may be in the future
                Return Rect(_sctLocation)
            End Function

            ''' <summary>Gets the coordinate rectangle of another cell's location.</summary>
            Friend Shared Function Rect(sctLocation As Location) As Rect
                Return Rect(sctLocation.Col, sctLocation.Row)
            End Function

            ''' <summary>Gets the coordinate rectangle of another cell's location.</summary>
            Friend Shared Function Rect(srtCol As Short, srtRow As Short) As Rect
                Dim sctPoint As Point
                sctPoint.X = (srtCol - 1) * SbConst.CellSize.Width
                sctPoint.Y = (srtRow - 1) * SbConst.CellSize.Height
                Return New Rect(sctPoint, SbConst.CellSize)
            End Function

        End Structure

        Friend Class Visuals
            'visual tree hierarchy
            Private _objVisRoot As VisualCollection
            Private _objVisGrid As New DrawingVisual
            Private _objVisTracks As New DrawingVisual
            Private _objVisTrackConns As New DrawingVisual
            Private _objVisTopLayer As New DrawingVisual
            Private _objVisSelection As New DrawingVisual

            'lookups between visuals and CTC objects
            Private _objVisualLookup As New Dictionary(Of CtcObjectBase, DrawingVisual)
            Private _objCtcObjLookup As New Dictionary(Of DrawingVisual, CtcObjectBase)
            Private _objTrkConLookup As New List(Of Connection)

            Friend Class Connection
                Friend Property Track1 As Track
                Friend Property Track2 As Track
                Friend Property Visual As DrawingVisual
            End Class


            Friend Sub New(objVisual As Visual)
                _objVisRoot = New VisualCollection(objVisual)
            End Sub

            Friend Sub Init()
                With _objVisRoot
                    .Add(_objVisGrid)
                    .Add(_objVisTracks)
                    .Add(_objVisTrackConns)
                    .Add(_objVisTopLayer)
                    .Add(_objVisSelection)
                End With
                'RenderOptions.SetEdgeMode(_objVisTracks, EdgeMode.Aliased)
            End Sub

            Friend Sub Clear()
                _objVisTracks.Children.Clear()
                _objVisTrackConns.Children.Clear()
                _objVisTopLayer.Children.Clear()

                _objVisualLookup.Clear()
                _objCtcObjLookup.Clear()
                _objTrkConLookup.Clear()
            End Sub


            Friend ReadOnly Property Root() As VisualCollection
                Get
                    Return _objVisRoot
                End Get
            End Property

            Friend ReadOnly Property Grid() As DrawingVisual
                Get
                    Return _objVisGrid
                End Get
            End Property

            Friend ReadOnly Property TopLayer() As DrawingVisual
                Get
                    Return _objVisTopLayer
                End Get
            End Property

            Friend ReadOnly Property Selection() As DrawingVisual
                Get
                    Return _objVisSelection
                End Get
            End Property


            Friend Function FromCtcObj(objCtcObject As CtcObjectBase) As DrawingVisual

                Dim objVisual As DrawingVisual
                _objVisualLookup.TryGetValue(objCtcObject, objVisual)

                Select Case True
                    Case objVisual Is Nothing And objCtcObject.IsDeleted
                        'do nothing; but why the hell is this condition being called?

                    Case objVisual Is Nothing
                        'for insert visual

                        objVisual = New DrawingVisual()

                        'add visual to render tree
                        Select Case True
                            Case TypeOf objCtcObject Is Track
                                _objVisTracks.Children.Add(objVisual)

                            Case TypeOf objCtcObject Is Signal OrElse TypeOf objCtcObject Is RRAutoLib.CTC.Label OrElse TypeOf objCtcObject Is RRAutoLib.CTC.Button
                                _objVisTopLayer.Children.Add(objVisual)

                        End Select

                        'add lookups between visuals and CTC objects
                        _objVisualLookup.Add(objCtcObject, objVisual)
                        _objCtcObjLookup.Add(objVisual, objCtcObject)

                        Return objVisual

                    Case objCtcObject.IsDeleted
                        'for delete visual

                        'remove visual from render tree
                        DirectCast(objVisual.Parent, DrawingVisual).Children.Remove(objVisual)

                        'remove lookups between visuals and CTC objects
                        _objVisualLookup.Remove(objCtcObject)
                        _objCtcObjLookup.Remove(objVisual)

                        'visual no longer exists
                        Return Nothing

                    Case Else
                        'for update visual

                        Return objVisual

                End Select

            End Function

            Friend Function ToCtcObj(objVisual As DrawingVisual) As CtcObjectBase
                Dim objCtcObject As CtcObjectBase
                _objCtcObjLookup.TryGetValue(objVisual, objCtcObject)
                Return objCtcObject
            End Function


            Friend Sub TrkConAdd(objTrack1 As Track, objTrack2 As Track, objVisual As DrawingVisual)
                'add track connection visual
                _objVisTrackConns.Children.Add(objVisual)

                'add track connection lookup
                Dim objConnection As New Connection
                With objConnection
                    .Track1 = objTrack1
                    .Track2 = objTrack2
                    .Visual = objVisual
                End With
                _objTrkConLookup.Add(objConnection)
            End Sub

            Friend Sub TrkConRemove(objTrack As Track)
                Dim intIdx As Integer = 0
                Do While intIdx < _objTrkConLookup.Count  'can't use For Each here because For Each collection can not be tampered with while iterating
                    If _objTrkConLookup(intIdx).Track1 Is objTrack OrElse _objTrkConLookup(intIdx).Track2 Is objTrack Then
                        'remove track connection visuals
                        _objVisTrackConns.Children.Remove(_objTrkConLookup(intIdx).Visual)

                        'remove track connection lookups
                        _objTrkConLookup.RemoveAt(intIdx)
                    Else
                        intIdx += 1
                    End If
                Loop
            End Sub

            Friend Function TrkConContains(objTrack1 As Track, objTrack2 As Track) As Boolean
                For Each objConnection As Connection In _objTrkConLookup
                    If (objConnection.Track1 Is objTrack1 AndAlso objConnection.Track2 Is objTrack2) OrElse
                       (objConnection.Track2 Is objTrack1 AndAlso objConnection.Track1 Is objTrack2) Then
                        Return True
                    End If
                Next
            End Function


            Friend Sub BringToFront(objCtcObject As CtcObjectBase)

                'note that both the objCtcObject and it's coresponding visual are being synchronized in the same ordinal position of their respective parent collections
                'this is so when saving the layout or generating new visuals from the CTC objects their z-order is maintained

                objCtcObject.Parent.MoveLast(objCtcObject)
                'My.Application.SetLayoutDirty()   'decided not to dirty the layout even though the layout z-order is changing because this gets called eveytime the context object is changed

                Dim objVisual As DrawingVisual
                If _objVisualLookup.TryGetValue(objCtcObject, objVisual) Then
                    With DirectCast(objVisual.Parent, DrawingVisual)
                        .Children.Remove(objVisual)
                        .Children.Add(objVisual)
                    End With
                End If

            End Sub

            Friend Sub SendToBack(objCtcObject As CtcObjectBase)

                'note that both the objCtcObject and it's coresponding visual are being synchronized in the same ordinal position of their respective parent collections
                'this is so when we save the layout or generate new visuals from the CTC objects their z-order is maintained

                objCtcObject.Parent.MoveFirst(objCtcObject)
                My.Application.SetLayoutDirty()

                Dim objVisual As DrawingVisual
                If _objVisualLookup.TryGetValue(objCtcObject, objVisual) Then
                    With DirectCast(objVisual.Parent, DrawingVisual)
                        .Children.Remove(objVisual)
                        .Children.Insert(0, objVisual)
                    End With
                End If

            End Sub

        End Class

#End Region

        Friend Sub Initialize(objConfig As LayoutConfig.SwitchboardConfig)
            _objConfig = objConfig

            Me.Focusable = True
            Me.LayoutTransform = _objScaleTransform
            Me.RenderTransform = _objPanTransform

            If _objConfig.Deserialized Then  'if configuration is from a previous saved session
                'restore zoom/pan
                DoScaleTransform()
                DoPanTransform()
            Else
                InitZoomPan()
            End If

            _objVisuals.Init()

            DrawAll()

            'add event handlers
            AddHandler CtcService.ObjectChanged, AddressOf ObjectChanged
            AddHandler CtcService.ModeChanged, AddressOf ModeChanged
            AddHandler My.Application.SbColorChanged, AddressOf SbColorChanged
            AddHandler My.Application.ContextObjChanged, AddressOf ContextObjChanged


            InitContextMenus()
        End Sub

#Region "Zoom/Pan"

        Private Sub InitZoomPan()
            'init zoom
            _objConfig.ScaleFactor = My.Settings.DefaultZoomLevel
            DoScaleTransform()

            'init panning
            'the container's VerticalContentAlignment="Center" HorizontalContentAlignment="Center" seems to place this control's left/top position in its center
            'so we have to move it to the real center based on its dimentions and zoom factor
            Dim intSwitchboardWidth As Integer = SbConst.CellSize.Width * SbConst.GridSize.Width
            Dim intSwitchboardHeight As Integer = SbConst.CellSize.Height * SbConst.GridSize.Height
            _objConfig.PanPos.X = intSwitchboardWidth / 2 * -1 * _objConfig.ScaleFactor
            _objConfig.PanPos.Y = intSwitchboardHeight / 2 * -1 * _objConfig.ScaleFactor
            DoPanTransform()
        End Sub

        Private Sub DoScaleTransform()
            _objScaleTransform.ScaleX = _objConfig.ScaleFactor
            _objScaleTransform.ScaleY = _objConfig.ScaleFactor
        End Sub

        Private Sub DoPanTransform()
            _objPanTransform.X = _objConfig.PanPos.X
            _objPanTransform.Y = _objConfig.PanPos.Y
        End Sub

        Private Sub ZoomInOut(Optional srtInOut As Short = 0, Optional dblManipDelta As Double = 1)

            Dim dblOldScaleFactor As Double = _objConfig.ScaleFactor

            'calculate zoom scale factor
            Select Case True
                Case dblManipDelta <> 1
                    _objConfig.ScaleFactor += (dblManipDelta - 1) * _objConfig.ScaleFactor
                Case srtInOut <> 0
                    _objConfig.ScaleFactor += srtInOut * _objConfig.ScaleFactor / My.Settings.ZoomIncrement
                Case Else
                    Exit Sub  'do nothing because scale was not changed
            End Select
            _objConfig.ScaleFactor = Math.Max(_objConfig.ScaleFactor, SbConst.MinScaleFactor)
            _objConfig.ScaleFactor = Math.Min(_objConfig.ScaleFactor, SbConst.MaxScaleFactor)
            DoScaleTransform()

            'recalculate panning based on new zoom scale factor
            _objConfig.PanPos.X = _objConfig.ScaleFactor * _objConfig.PanPos.X / dblOldScaleFactor
            _objConfig.PanPos.Y = _objConfig.ScaleFactor * _objConfig.PanPos.Y / dblOldScaleFactor
            DoPanTransform()

        End Sub

#End Region

#Region "Rendering"

        Protected Overloads Overrides ReadOnly Property VisualChildrenCount() As Integer
            Get
                Return _objVisuals.Root.Count
            End Get
        End Property

        Protected Overloads Overrides Function GetVisualChild(index As Integer) As Visual

            If index < 0 OrElse index >= _objVisuals.Root.Count Then Throw New ArgumentOutOfRangeException("index")

            Return _objVisuals.Root(index)

        End Function


        Private Sub DrawAll()
            DrawGrid()
            DrawSelAdorner(Nothing)

            'clear CTC object visuals
            _objVisuals.Clear()

            'draw all objects
            For Each objTrack As Track In CtcService.Tracks
                DrawTrack(objTrack, True)
            Next
            For Each objSignal As Signal In CtcService.Signals
                DrawSignal(objSignal)
            Next
            For Each objLabel As RRAutoLib.CTC.Label In CtcService.Labels
                DrawLabel(objLabel)
            Next
            For Each objButton As RRAutoLib.CTC.Button In CtcService.Buttons
                DrawButton(objButton)
            Next
        End Sub

        Private Sub DrawGrid()
            Dim objDrawingContext As DrawingContext = _objVisuals.Grid.RenderOpen()

            If My.Application.AppMode = AppMode.Edit Then

                Dim objBrush As New SolidColorBrush(My.Settings.GridColor)
                objBrush.Freeze()

                'draw little cell corner squares
                For intCol As Integer = 1 To SbConst.GridSize.Width + 1
                    For intRow As Integer = 1 To SbConst.GridSize.Height + 1
                        Dim sctCellRect As Rect = Cell.Rect(intCol, intRow)
                        sctCellRect.Offset(-1, -1)
                        objDrawingContext.DrawRectangle(objBrush, DirectCast(Nothing, Pen), New Rect(sctCellRect.TopLeft, New Size(2, 2)))
                    Next
                Next

                Dim objPen As New Pen
                With objPen
                    objBrush = New SolidColorBrush(My.Settings.GridColor.SetAlpha(25))
                    objBrush.Freeze()
                    .Brush = objBrush
                    .Thickness = 1
                    .Freeze()
                End With

                'draw squares surounding the whole cell
                For intCol As Integer = 1 To SbConst.GridSize.Width
                    For intRow As Integer = 1 To SbConst.GridSize.Height
                        Dim sctCellRect As Rect = Cell.Rect(intCol, intRow)
                        sctCellRect.Offset(0.5, 0.5)
                        objDrawingContext.DrawRectangle(Nothing, objPen, New Rect(sctCellRect.TopLeft, New Size(SbConst.CellSize.Width - 1, SbConst.CellSize.Height - 1)))
                    Next
                Next

            End If

            objDrawingContext.Close()
        End Sub

        Private Sub DrawTrack(objTrack As Track, blnBatchedConnections As Boolean)
            If objTrack Is Nothing Then Exit Sub

            Dim objVisual As DrawingVisual = _objVisuals.FromCtcObj(objTrack)
            Dim sctTrackDerivedColors As TrackDerivedColors

            '----------------------------------------------- Draw Track --------------------------------------------------------------------------------

            If objVisual IsNot Nothing Then

                sctTrackDerivedColors = GetTrackDerivedColors(objTrack, True, My.Settings.BackColor)
                Dim objDrawing As DrawingGroup = GetTrackDrawing(objTrack, sctTrackDerivedColors)
                If objDrawing IsNot Nothing Then

                    Dim sctCellRect As Rect = Cell.Rect(objTrack.Location)
                    objDrawing.ApplyTransform(New TranslateTransform(sctCellRect.Left - SbConst.TrackBrushCellOverlap, sctCellRect.Top - SbConst.TrackBrushCellOverlap))  'position
                    objDrawing.Freeze()

                    With objVisual.RenderOpen()
                        .DrawDrawing(objDrawing)
                        .Close()
                    End With

                End If

            End If

            '----------------------------------------------- Draw Track Connections --------------------------------------------------------------------

            '"blnBatchedConnections = true" is used when drawing all the tracks from scratch and is a little faster because all connections are inserts
            'we assume _objVisTrackConns.Children and _objTrackConnLookup are cleared 
            'in this situation we don't need to delete any invalidated connection visuals; 
            'for each track we check if the connections we need are present (i.e. added by an adjacent track); if present, we do nothing; if not we add them

            '"blnBatchedConnections = false" is used when partialy updating the switchboard with tracks;
            'in this situation we delete all connection visuals for the track in question and then add the new ones; 
            'this takes care of inserted, updated, and deleted connections; this is a little slower because some connections could be drawn twice

            If Not blnBatchedConnections Then
                'delete all connections visuals for this track
                _objVisuals.TrkConRemove(objTrack)
            End If

            If objTrack.IsDeleted Then Exit Sub

            Dim objConnections As New TrackConnDefinitions(objTrack, True)
            For Each objConnDef As TrackConnDefinitions.Connection In objConnections

                If blnBatchedConnections Then
                    If _objVisuals.TrkConContains(objTrack, objConnDef.OtherTrack) Then
                        'skip the drawing of this connection; an adjacent track must have already rendered it
                        Continue For
                    End If
                End If

                objVisual = New DrawingVisual()

                _objVisuals.TrkConAdd(objTrack, objConnDef.OtherTrack, objVisual)

                Dim sctOtherTrackDerivedColors As TrackDerivedColors = GetTrackDerivedColors(objConnDef.OtherTrack, True, My.Settings.BackColor)
                Dim objDrawing As DrawingGroup = GetTrackConnDrawing(objConnDef, sctTrackDerivedColors, sctOtherTrackDerivedColors)
                If objDrawing IsNot Nothing Then

                    Dim sctOrigin As Point = Cell.Rect(objTrack.Location).TopLeft

                    'find center point of placement
                    Select Case objConnDef.Direction
                        Case TrackConnDefinitions.Direction.N
                            sctOrigin.Offset(SbConst.CellSize.Width / 2, 0)
                        Case TrackConnDefinitions.Direction.NE
                            sctOrigin.Offset(SbConst.CellSize.Width, 0)
                        Case TrackConnDefinitions.Direction.E
                            sctOrigin.Offset(SbConst.CellSize.Width, SbConst.CellSize.Height / 2)
                        Case TrackConnDefinitions.Direction.SE
                            sctOrigin.Offset(SbConst.CellSize.Width, SbConst.CellSize.Height)
                        Case TrackConnDefinitions.Direction.S
                            sctOrigin.Offset(SbConst.CellSize.Width / 2, SbConst.CellSize.Height)
                        Case TrackConnDefinitions.Direction.SW
                            sctOrigin.Offset(0, SbConst.CellSize.Height)
                        Case TrackConnDefinitions.Direction.W
                            sctOrigin.Offset(0, SbConst.CellSize.Height / 2)
                    End Select

                    'find top left point of placement
                    sctOrigin.Offset(-objDrawing.Bounds.Width / 2, -objDrawing.Bounds.Height / 2)

                    objDrawing.ApplyTransform(New TranslateTransform(sctOrigin.X, sctOrigin.Y))  'position
                    objDrawing.Freeze()

                    With objVisual.RenderOpen()
                        .DrawDrawing(objDrawing)
                        .Close()
                    End With

                End If
            Next

        End Sub

        Private Sub DrawSignal(objSignal As Signal, Optional blnSelOnly As Boolean = False)
            If objSignal Is Nothing Then Exit Sub

            Dim objVisual As DrawingVisual = _objVisuals.FromCtcObj(objSignal)
            If objVisual IsNot Nothing Then

                Dim objDrawing As DrawingGroup = GetSignalDrawing(objSignal)
                If objDrawing IsNot Nothing Then

                    Dim sctDrawRect As New Rect(New Size(objDrawing.Bounds.Width * objSignal.DrawScale, objDrawing.Bounds.Height * objSignal.DrawScale))
                    AlignToCell(sctDrawRect, Cell.Rect(objSignal.Location), objSignal.CellAlign)
                    sctDrawRect.Offset(objSignal.AlignOffsetX, objSignal.AlignOffsetY)

                    If Not blnSelOnly Then
                        objDrawing.ApplyTransform(New ScaleTransform(objSignal.DrawScale, objSignal.DrawScale))   'scale
                        objDrawing.ApplyTransform(New TranslateTransform(sctDrawRect.X, sctDrawRect.Y))  'position
                        objDrawing.Freeze()

                        With objVisual.RenderOpen()
                            .DrawDrawing(objDrawing)
                            .Close()
                        End With
                    End If

                    'draw selection adorner if signal is selected
                    If My.Application.AppMode = AppMode.Edit AndAlso My.Application.SbContextObj Is objSignal Then
                        DrawSelAdorner(sctDrawRect)
                    End If

                End If

            End If

        End Sub

        Private Sub DrawLabel(objLabel As RRAutoLib.CTC.Label, Optional blnSelOnly As Boolean = False)
            If objLabel Is Nothing Then Exit Sub

            Dim objVisual As DrawingVisual = _objVisuals.FromCtcObj(objLabel)
            If objVisual IsNot Nothing Then

                If objLabel.Text = Nothing AndAlso My.Application.AppMode = AppMode.Operation Then
                    'hide label with no text in operation mode; in edit mode we show objLabel.Name on no text
                    objVisual.RenderOpen().Close()

                Else
                    Dim objDrawing As DrawingGroup = GetLabelDrawing(objLabel)

                    Dim sctDrawRect As New Rect(New Size(objDrawing.Bounds.Width * objLabel.DrawScale, objDrawing.Bounds.Height * objLabel.DrawScale))
                    AlignToCell(sctDrawRect, Cell.Rect(objLabel.Location), objLabel.CellAlign)
                    sctDrawRect.Offset(objLabel.AlignOffsetX, objLabel.AlignOffsetY)

                    If Not blnSelOnly Then
                        objDrawing.ApplyTransform(New ScaleTransform(objLabel.DrawScale, objLabel.DrawScale))   'scale
                        objDrawing.ApplyTransform(New TranslateTransform(sctDrawRect.X, sctDrawRect.Y))  'position
                        objDrawing.Freeze()

                        With objVisual.RenderOpen()
                            .DrawDrawing(objDrawing)
                            .Close()
                        End With
                    End If

                    'draw selection adorner if label is selected
                    If My.Application.AppMode = AppMode.Edit AndAlso My.Application.SbContextObj Is objLabel Then
                        DrawSelAdorner(sctDrawRect)
                    End If

                End If

            End If

        End Sub

        Private Sub DrawButton(objButton As RRAutoLib.CTC.Button, Optional blnSelOnly As Boolean = False)
            If objButton Is Nothing Then Exit Sub

            Dim objVisual As DrawingVisual = _objVisuals.FromCtcObj(objButton)

            If objVisual IsNot Nothing Then

                Dim objDrawing As DrawingGroup = GetButtonDrawing(objButton)
                If objDrawing IsNot Nothing Then

                    Dim sctDrawRect As New Rect(New Size(objDrawing.Bounds.Width * objButton.DrawScale, objDrawing.Bounds.Height * objButton.DrawScale))
                    AlignToCell(sctDrawRect, Cell.Rect(objButton.Location), objButton.CellAlign)
                    sctDrawRect.Offset(objButton.AlignOffsetX, objButton.AlignOffsetY)

                    If Not blnSelOnly Then
                        objDrawing.ApplyTransform(New ScaleTransform(objButton.DrawScale, objButton.DrawScale))   'scale
                        objDrawing.ApplyTransform(New TranslateTransform(sctDrawRect.X, sctDrawRect.Y))  'position
                        objDrawing.Freeze()

                        With objVisual.RenderOpen()
                            .DrawDrawing(objDrawing)
                            .Close()
                        End With
                    End If

                    'draw selection adorner if signal is selected
                    If My.Application.AppMode = AppMode.Edit AndAlso My.Application.SbContextObj Is objButton Then
                        DrawSelAdorner(sctDrawRect)
                    End If

                End If

            End If

        End Sub

        Private Sub DrawSelAdorner(sctRect As Rect)

            Dim objDrawingContext As DrawingContext = _objVisuals.Selection.RenderOpen()

            If sctRect <> Nothing Then
                Dim objPenBlack As New Pen(New SolidColorBrush(Colors.Black), 2)
                objPenBlack.Freeze()

                Dim objPenWhite As New Pen(New SolidColorBrush(My.Settings.SelectionColor), 2)
                objPenWhite.DashStyle = New DashStyle(New Double() {1, 3}, 0)
                objPenWhite.Freeze()

                Dim sctSelectionRect As New Rect(sctRect.X - 3, sctRect.Y - 3, sctRect.Width + 6, sctRect.Height + 6)
                objDrawingContext.DrawRectangle(Nothing, objPenBlack, sctSelectionRect)
                objDrawingContext.DrawRectangle(Nothing, objPenWhite, sctSelectionRect)
            End If

            objDrawingContext.Close()

        End Sub


        'aligns the visual rectangle being drawn on a cell rectangle based on the given alignment type
        Private Sub AlignToCell(ByRef sctVisRect As Rect, sctCellRect As Rect, enuAlign As Alignment)
            Select Case enuAlign
                Case Alignment.TopLeft, Alignment.MiddleLeft, Alignment.BottomLeft
                    sctVisRect.X = sctCellRect.Left

                Case Alignment.TopCenter, Alignment.MiddleCenter, Alignment.BottomCenter
                    sctVisRect.X = sctCellRect.Center - sctVisRect.Center

                Case Alignment.TopRight, Alignment.MiddleRight, Alignment.BottomRight
                    sctVisRect.X = sctCellRect.Right - sctVisRect.Right
            End Select
            Select Case enuAlign
                Case Alignment.TopLeft, Alignment.TopCenter, Alignment.TopRight
                    sctVisRect.Y = sctCellRect.Top

                Case Alignment.MiddleLeft, Alignment.MiddleCenter, Alignment.MiddleRight
                    sctVisRect.Y = sctCellRect.Middle - sctVisRect.Middle

                Case Alignment.BottomLeft, Alignment.BottomCenter, Alignment.BottomRight
                    sctVisRect.Y = sctCellRect.Bottom - sctVisRect.Bottom
            End Select
        End Sub

        'keeps track of drawn objects and draws one only if it hasn't been drawn before
        Private Sub DrawObjectOnce(objCtcObject As CtcObjectBase, objDrawnObjects As List(Of CtcObjectBase))
            Select Case True
                Case TypeOf objCtcObject Is Track
                    If Not objDrawnObjects.Contains(objCtcObject) Then
                        DrawTrack(objCtcObject, False)
                        objDrawnObjects.Add(objCtcObject)
                    End If

                Case TypeOf objCtcObject Is Block
                    For Each objTrack As Track In CtcService.Tracks
                        If objTrack.Block Is objCtcObject Then
                            DrawObjectOnce(objTrack, objDrawnObjects)
                        End If
                    Next

                Case TypeOf objCtcObject Is Route
                    For Each objRouteElement As Route.RouteElement In DirectCast(objCtcObject, Route).RouteElements
                        DrawObjectOnce(objRouteElement.Track, objDrawnObjects)
                    Next

            End Select
        End Sub

#End Region

#Region "Input Handling"

        'forwarded events from parent ---------

        Friend Sub _OnMouseUp(e As MouseButtonEventArgs)

            Select Case e.ChangedButton
                Case MouseButton.Left
                    If _blnIsPanning Then
                        _blnIsPanning = False
                    Else
                        LeftClickUp()
                    End If
                Case MouseButton.Right
                    ShowContextMenu()
                Case MouseButton.Middle
                    InitZoomPan()
            End Select

        End Sub

        Friend Sub _OnMouseDown(e As MouseButtonEventArgs)

            If e.LeftButton = MouseButtonState.Pressed Then
                If Keyboard.IsKeyDown(Key.Space) Then   'capture Space-LClick key combo for panning 
                    'can not get position from Me control because it moves on MouseMove which throws off the mouse delta calculation
                    'since the parent is stationary it is a perfect candidate
                    _blnIsPanning = True
                    _sctMouseDownPoint = e.GetPosition(Me.Parent)
                    _sctMouseDownPanPos = _objConfig.PanPos
                    'Me.Cursor = Cursors.ScrollAll
                Else
                    LeftClickDown()
                End If
            End If

        End Sub

        Friend Sub _OnMouseMove(e As MouseEventArgs)
            'get current cell position under mouse and report it out if changed
            If _sctCurrentCell.SetLocation(e.GetPosition(Me)) Then RaiseEvent CellLocChanged(_sctCurrentCell.Location)

            'get current point position under mouse (used for visual hit tests)
            _sctCurrentPoint = e.GetPosition(Me)

            If _blnIsPanning Then
                'during panning if the left click is released off of this control the OnMouseUp event is not fired so we must turn off the panning flag here
                'it would be nicer if we could disable panning when the switchboard is losing focus but I haven't been able to do this; revisit again later
                If Mouse.LeftButton = MouseButtonState.Released Then
                    _blnIsPanning = False
                Else
                    'can not get position from Me control because it moves on MouseMove which throws off the mouse delta calculation
                    'since the parent is stationary it is a perfect candidate
                    Dim sctMouseHoverPoint As Point = e.GetPosition(Me.Parent)
                    _objConfig.PanPos.X = _sctMouseDownPanPos.X + (sctMouseHoverPoint.X - _sctMouseDownPoint.X)
                    _objConfig.PanPos.Y = _sctMouseDownPanPos.Y + (sctMouseHoverPoint.Y - _sctMouseDownPoint.Y)
                    DoPanTransform()
                End If
            End If

        End Sub

        Friend Sub _OnMouseWheel(e As MouseWheelEventArgs)
            ZoomInOut(srtInOut:=If(e.Delta < 0, -1, 1))
        End Sub

        Friend Sub _OnManipulationDelta(e As ManipulationDeltaEventArgs)

            'do swipe panning
            _objConfig.PanPos.X += e.DeltaManipulation.Translation.X
            _objConfig.PanPos.Y += e.DeltaManipulation.Translation.Y
            DoPanTransform()

            'do pinch zoom
            ZoomInOut(dblManipDelta:=e.DeltaManipulation.Scale.X)

        End Sub


        'keyboard shortcuts
        Protected Overrides Sub OnKeyDown(e As KeyEventArgs)
            MyBase.OnKeyDown(e)

            Dim objSbContextObj As CtcObjectBase = My.Application.SbContextObj

            Select Case True
                Case Keyboard.IsKeyDown(Key.Space)
                    'track selection modes
                    If My.Application.AppMode = AppMode.Edit Then
                        Select Case e.Key
                            Case Key.D1
                                My.Application.SbTrackMode = SbTrackMode.Tracks
                            Case Key.D2
                                My.Application.SbTrackMode = SbTrackMode.Blocks
                            Case Key.D3
                                My.Application.SbTrackMode = SbTrackMode.Routes
                        End Select
                    End If

                    'panning
                    Select Case e.Key
                        Case Key.Down
                            _objConfig.PanPos.Y -= SbConst.PanIncrement
                            DoPanTransform()
                        Case Key.Up
                            _objConfig.PanPos.Y += SbConst.PanIncrement
                            DoPanTransform()
                        Case Key.Left
                            _objConfig.PanPos.X += SbConst.PanIncrement
                            DoPanTransform()
                        Case Key.Right
                            _objConfig.PanPos.X -= SbConst.PanIncrement
                            DoPanTransform()
                    End Select

                Case Keyboard.Modifiers = ModifierKeys.Shift
                    If My.Application.AppMode = AppMode.Edit Then
                        Select Case e.Key
                            Case Key.Up
                                If TypeOf objSbContextObj Is ITopLayer Then ShiftAlignOffset(0, -1)
                            Case Key.Down
                                If TypeOf objSbContextObj Is ITopLayer Then ShiftAlignOffset(0, 1)
                            Case Key.Left
                                If TypeOf objSbContextObj Is ITopLayer Then ShiftAlignOffset(-1, 0)
                            Case Key.Right
                                If TypeOf objSbContextObj Is ITopLayer Then ShiftAlignOffset(1, 0)
                        End Select
                    End If

                Case Keyboard.Modifiers = ModifierKeys.Control
                    If My.Application.AppMode = AppMode.Edit Then
                        Select Case e.Key
                            Case Key.C
                                Select Case True
                                    Case TypeOf objSbContextObj Is Signal
                                        CopySignalStyle_Click(Nothing, Nothing)
                                    Case TypeOf objSbContextObj Is RRAutoLib.CTC.Label
                                        CopyLabelStyle_Click(Nothing, Nothing)
                                    Case TypeOf objSbContextObj Is RRAutoLib.CTC.Button
                                        CopyButtonStyle_Click(Nothing, Nothing)
                                End Select

                            Case Key.V
                                Select Case True
                                    Case TypeOf objSbContextObj Is Signal
                                        PasteSignalStyle_Click(Nothing, Nothing)
                                    Case TypeOf objSbContextObj Is RRAutoLib.CTC.Label
                                        PasteLabelStyle_Click(Nothing, Nothing)
                                    Case TypeOf objSbContextObj Is RRAutoLib.CTC.Button
                                        PasteButtonStyle_Click(Nothing, Nothing)
                                End Select

                            Case Key.Left
                                If TypeOf objSbContextObj Is ITopLayer Then CycleAlignment(False)

                            Case Key.Right
                                If TypeOf objSbContextObj Is ITopLayer Then CycleAlignment(True)

                            Case Key.Up
                                If TypeOf objSbContextObj Is ITopLayer Then ScaleObject(+0.04)

                            Case Key.Down
                                If TypeOf objSbContextObj Is ITopLayer Then ScaleObject(-0.04)

                        End Select
                    End If

                Case Else
                    'zooming
                    Select Case e.Key
                        Case Key.Add, Key.OemPlus
                            ZoomInOut(srtInOut:=+1)
                        Case Key.Subtract, Key.OemMinus
                            ZoomInOut(srtInOut:=-1)
                    End Select

                    If My.Application.AppMode = AppMode.Edit Then
                        Select Case e.Key
                            Case Key.D1
                                PlaceTrack(_sctCurrentCell.Location, New Track.TrackType() {Track.TrackType.TrackStraight})

                            Case Key.D2
                                PlaceTrack(_sctCurrentCell.Location, New Track.TrackType() {Track.TrackType.TrackCurved1, Track.TrackType.TrackCurved2})

                            Case Key.D3
                                PlaceTrack(_sctCurrentCell.Location, New Track.TrackType() {Track.TrackType.TrackCrossing1, Track.TrackType.TrackCrossing2})

                            Case Key.D4
                                PlaceTrack(_sctCurrentCell.Location, New Track.TrackType() {Track.TrackType.TrackTerminator})

                            Case Key.D5
                                PlaceTrack(_sctCurrentCell.Location, New Track.TrackType() {Track.TrackType.TurnoutLeft, Track.TrackType.TurnoutRight})

                            Case Key.D6
                                PlaceTrack(_sctCurrentCell.Location, New Track.TrackType() {Track.TrackType.TurnoutSingleSlip, Track.TrackType.TurnoutDoubleSlip})

                            Case Key.D7
                                PlaceTrack(_sctCurrentCell.Location, New Track.TrackType() {Track.TrackType.TurnoutThreeWay})

                            Case Key.Q
                                AddSignal(_sctCurrentCell.Location, Nothing)

                            Case Key.W
                                AddLabel(_sctCurrentCell.Location)

                            Case Key.E
                                AddButton(_sctCurrentCell.Location)

                            Case Key.A
                                Select Case True
                                    Case TypeOf objSbContextObj Is Track
                                        OrientationLeft_Click(Nothing, Nothing)

                                    Case TypeOf objSbContextObj Is Signal
                                        SetSignalType(New SignalLookKey() {
                                            SignalLookKey.x2_Gen_Sing, SignalLookKey.x2_Gen_Mult, SignalLookKey.x2_DE_Block, SignalLookKey.x2_DE_Shunt1,
                                            SignalLookKey.x2_DE_Shunt2, SignalLookKey.x2_DE_Wait, SignalLookKey.x2_UK_Block, SignalLookKey.x2_UK_Shunt
                                        })
                                End Select

                            Case Key.S
                                Select Case True
                                    Case TypeOf objSbContextObj Is Block
                                        Dim objTrack As Track = CtcService.Tracks(_sctCurrentCell.Location).LastOrDefault
                                        If objTrack IsNot Nothing Then
                                            Dim objControl As New Control  'emulates structure passed by context menu item to TrackToBlock_Click()
                                            objControl.Tag = objTrack
                                            TrackToBlock_Click(objControl, Nothing)
                                        End If

                                    Case TypeOf objSbContextObj Is Route
                                        Dim objTrack As Track = CtcService.Tracks(_sctCurrentCell.Location).LastOrDefault
                                        If objTrack IsNot Nothing Then
                                            Dim objControl As New Control  'emulates structure passed by context menu item to TrackToRoute_Click()
                                            objControl.Tag = objTrack
                                            TrackToRoute_Click(objControl, Nothing)
                                        End If

                                    Case TypeOf objSbContextObj Is Signal
                                        SetSignalType(New SignalLookKey() {
                                            SignalLookKey.x3_Gen_Sing, SignalLookKey.x3_Gen_Mult, SignalLookKey.x3_DE_Entry,
                                            SignalLookKey.x3_Automotive, SignalLookKey.x3_UK_Block
                                        })
                                End Select

                            Case Key.D
                                Select Case True
                                    Case TypeOf objSbContextObj Is Track
                                        OrientationRight_Click(Nothing, Nothing)

                                    Case TypeOf objSbContextObj Is Route
                                        Dim objTrack As Track = CtcService.Tracks(_sctCurrentCell.Location).LastOrDefault
                                        If objTrack IsNot Nothing Then
                                            Dim objRoute As Route = objSbContextObj
                                            If objTrack.IsTurnout And objRoute.RouteElements.Contains(objTrack) Then
                                                Dim objControl As New Control  'emulates structure passed by context menu item to TrackToRoute_Click()
                                                objControl.Tag = New Object() {objTrack, objTrack.States.Next(objRoute.RouteElements(objTrack).State).Value}
                                                CycleRouteState_Click(objControl, Nothing)
                                            End If
                                        End If

                                    Case TypeOf objSbContextObj Is Signal
                                        SetSignalType(New SignalLookKey() {
                                            SignalLookKey.x4_Gen_Sing, SignalLookKey.x4_Gen_Mult, SignalLookKey.x4_DE_Depart,
                                            SignalLookKey.x4_DE_Distant, SignalLookKey.x4_UK_Block
                                        })
                                End Select

                            Case Key.F
                                If TypeOf objSbContextObj Is Signal Then
                                    SetSignalType(New SignalLookKey() {SignalLookKey.x5_Gen_Sing})
                                End If

                            Case Key.Delete
                                If TypeOf objSbContextObj Is Track OrElse TypeOf objSbContextObj Is Block OrElse
                                   TypeOf objSbContextObj Is Route OrElse TypeOf objSbContextObj Is Signal OrElse
                                   TypeOf objSbContextObj Is RRAutoLib.CTC.Label OrElse TypeOf objSbContextObj Is RRAutoLib.CTC.Button Then
                                    RemoveObject_Click(Nothing, Nothing)
                                End If

                            Case Key.Up
                                If TypeOf objSbContextObj Is ILocation Then ShiftLocation(0, -1)

                            Case Key.Down
                                If TypeOf objSbContextObj Is ILocation Then ShiftLocation(0, 1)

                            Case Key.Left
                                If TypeOf objSbContextObj Is ILocation Then ShiftLocation(-1, 0)

                            Case Key.Right
                                If TypeOf objSbContextObj Is ILocation Then ShiftLocation(1, 0)

                        End Select
                    End If

            End Select

        End Sub


        Private Sub LeftClickUp()

            Select Case My.Application.AppMode
                Case AppMode.Edit

                    Dim blnObjectHit As Boolean = False

                    'try to select items from the top layer first (signals, labels, buttons)
                    Dim objFirstHitObject As CtcObjectBase = Nothing
                    _objVisuals.TopLayer.HitTest(Nothing,
                        Function(result As HitTestResult) As HitTestResultBehavior
                            blnObjectHit = True
                            Dim objVisual As DrawingVisual = result.VisualHit
                            Dim objCtcObject As CtcObjectBase = _objVisuals.ToCtcObj(objVisual)
                            Select Case True
                                Case objFirstHitObject IsNot Nothing
                                    'second hit visual should be selected and first hit pushed back
                                    _objVisuals.SendToBack(objFirstHitObject)
                                    My.Application.SetContextObj(Me, objCtcObject)
                                    Return HitTestResultBehavior.Stop

                                Case My.Application.SbContextObj Is objCtcObject
                                    'first hit object is already selected
                                    objFirstHitObject = objCtcObject
                                    Return HitTestResultBehavior.Continue

                                Case Else
                                    'first hit object was not selected so select it now
                                    My.Application.SetContextObj(Me, objCtcObject)
                                    Return HitTestResultBehavior.Stop
                            End Select
                        End Function,
                        New PointHitTestParameters(_sctCurrentPoint))

                    If Not blnObjectHit Then

                        'auto switch select track mode during selection with the <Alt-Click> combination
                        If Keyboard.IsKeyDown(Key.LeftAlt) Or Keyboard.IsKeyDown(Key.RightAlt) Then
                            Select Case My.Application.SbTrackMode
                                Case SbTrackMode.Tracks
                                    My.Application.SbTrackMode = SbTrackMode.Blocks
                                Case SbTrackMode.Blocks
                                    My.Application.SbTrackMode = SbTrackMode.Routes
                                Case SbTrackMode.Routes
                                    My.Application.SbTrackMode = SbTrackMode.Tracks
                            End Select
                        End If

                        'try to select items from the track layer (tracks, blocks, routes)
                        Dim objTracks As List(Of Track) = CtcService.Tracks(_sctCurrentCell.Location)
                        Select Case My.Application.SbTrackMode
                            Case SbTrackMode.Tracks
                                For intIdx As Integer = objTracks.Count - 1 To 0 Step -1
                                    Select Case True
                                        Case My.Application.SbContextObj Is objTracks(intIdx) AndAlso objTracks.Count > 1
                                            _objVisuals.SendToBack(objTracks(intIdx))   'rearange the z-order to be ready for next selection cycle

                                        Case My.Application.SbContextObj IsNot objTracks(intIdx)
                                            My.Application.SetContextObj(Me, objTracks(intIdx))
                                            Exit For
                                    End Select
                                Next

                            Case SbTrackMode.Blocks
                                For intIdx As Integer = objTracks.Count - 1 To 0 Step -1
                                    Select Case True
                                        Case objTracks(intIdx).Block Is Nothing
                                            _objVisuals.SendToBack(objTracks(intIdx))   'rearange the z-order to be ready for next selection cycle

                                        Case My.Application.SbContextObj Is objTracks(intIdx).Block AndAlso objTracks.Count > 1
                                            _objVisuals.SendToBack(objTracks(intIdx))   'rearange the z-order to be ready for next selection cycle

                                        Case My.Application.SbContextObj IsNot objTracks(intIdx).Block
                                            My.Application.SetContextObj(Me, objTracks(intIdx).Block)
                                            Exit For
                                    End Select
                                Next

                            Case SbTrackMode.Routes
                                For intIdx As Integer = objTracks.Count - 1 To 0 Step -1
                                    Dim objRoutes As List(Of Route) = CtcService.Routes(objTracks(intIdx))
                                    Select Case True
                                        Case objRoutes.Count = 0                                'track not part of a route
                                            _objVisuals.SendToBack(objTracks(intIdx))

                                        Case Not TypeOf My.Application.SbContextObj Is Route    'track is part of one or more routes but no route is selected
                                            My.Application.SetContextObj(Me, objRoutes.First)
                                            Exit For

                                        Case objRoutes.Last Is My.Application.SbContextObj      'track is part of one or more routes and the last route is selected
                                            Dim blnMoreTracksWithRoutes As Boolean = False
                                            For intIdx2 As Integer = intIdx - 1 To 0 Step -1
                                                Dim objRoutes2 As List(Of Route) = CtcService.Routes(objTracks(intIdx2))
                                                If objRoutes2.Count > 0 Then
                                                    blnMoreTracksWithRoutes = True
                                                    Exit For
                                                End If
                                            Next
                                            'if there is at least one track that has routes lower in the z-index
                                            If blnMoreTracksWithRoutes Then
                                                _objVisuals.SendToBack(objTracks(intIdx))
                                            Else
                                                My.Application.SetContextObj(Me, objRoutes.First)
                                                Exit For
                                            End If

                                        Case objRoutes.Contains(My.Application.SbContextObj)    'track is part of one or more routes and a route other than the last is selected
                                            My.Application.SetContextObj(Me, objRoutes(objRoutes.IndexOf(My.Application.SbContextObj) + 1))
                                            Exit For

                                        Case Else                                               'track is part of one or more routes but a route in another track is selected
                                            My.Application.SetContextObj(Me, objRoutes.First)
                                            Exit For
                                    End Select
                                Next

                        End Select

                    End If

                Case AppMode.Operation
                    OperationAction(False)

            End Select

        End Sub

        Private Sub LeftClickDown()

            If My.Application.AppMode = AppMode.Operation Then
                OperationAction(True)
            End If

        End Sub

        Private Sub OperationAction(blnPress As Boolean)

            Dim objHitResult As HitTestResult = _objVisuals.TopLayer.HitTest(_sctCurrentPoint)
            If objHitResult IsNot Nothing Then
                Dim objVisual As DrawingVisual = objHitResult.VisualHit
                Dim objCtcObject As CtcObjectBase = _objVisuals.ToCtcObj(objVisual)
                With DirectCast(objCtcObject, ISwBrdOperable)
                    If blnPress Then
                        .Press()
                    Else
                        .Release()
                    End If
                End With
            Else
                Dim objTrack As Track = CtcService.Tracks(_sctCurrentCell.Location).LastOrDefault
                If objTrack IsNot Nothing Then
                    If blnPress Then
                        objTrack.OperatePress()
                    Else
                        objTrack.OperateRelease()
                    End If
                End If
            End If

        End Sub

#End Region

#Region "Context menus"

        Private _objPlaceTrack As New MenuItem()
        Private _objPlaceSignal As New MenuItem()
        Private _objContextMenu As New ContextMenu()

        Private Sub InitContextMenus()
            'static menu segments

            With _objPlaceTrack.Items
                .Add(NewMenuItem("Straight", False, GetTrackDrawingImage(New Track(Track.TrackType.TrackStraight, 1)), "1", New Track.TrackType() {Track.TrackType.TrackStraight}, New RoutedEventHandler(AddressOf PlaceTrack_Click)))
                .Add(NewMenuItem("Mild Curve", False, GetTrackDrawingImage(New Track(Track.TrackType.TrackCurved1, 1)), "2", New Track.TrackType() {Track.TrackType.TrackCurved1}, New RoutedEventHandler(AddressOf PlaceTrack_Click)))
                .Add(NewMenuItem("Sharp Curve", False, GetTrackDrawingImage(New Track(Track.TrackType.TrackCurved2, 2)), "2", New Track.TrackType() {Track.TrackType.TrackCurved2}, New RoutedEventHandler(AddressOf PlaceTrack_Click)))
                .Add(NewMenuItem("Square Crossing", False, GetTrackDrawingImage(New Track(Track.TrackType.TrackCrossing1, 1)), "3", New Track.TrackType() {Track.TrackType.TrackCrossing1}, New RoutedEventHandler(AddressOf PlaceTrack_Click)))
                .Add(NewMenuItem("Diagonal Crossing", False, GetTrackDrawingImage(New Track(Track.TrackType.TrackCrossing2, 1)), "3", New Track.TrackType() {Track.TrackType.TrackCrossing2}, New RoutedEventHandler(AddressOf PlaceTrack_Click)))
                .Add(NewMenuItem("Terminator", False, GetTrackDrawingImage(New Track(Track.TrackType.TrackTerminator, 1)), "4", New Track.TrackType() {Track.TrackType.TrackTerminator}, New RoutedEventHandler(AddressOf PlaceTrack_Click)))
                .Add(New Separator)
                .Add(NewMenuItem("Turnout Left", False, GetTrackDrawingImage(New Track(Track.TrackType.TurnoutLeft, 1)), "5", New Track.TrackType() {Track.TrackType.TurnoutLeft}, New RoutedEventHandler(AddressOf PlaceTrack_Click)))
                .Add(NewMenuItem("Turnout Right", False, GetTrackDrawingImage(New Track(Track.TrackType.TurnoutRight, 1)), "5", New Track.TrackType() {Track.TrackType.TurnoutRight}, New RoutedEventHandler(AddressOf PlaceTrack_Click)))
                .Add(NewMenuItem("Single Slip", False, GetTrackDrawingImage(New Track(Track.TrackType.TurnoutSingleSlip, 1)), "6", New Track.TrackType() {Track.TrackType.TurnoutSingleSlip}, New RoutedEventHandler(AddressOf PlaceTrack_Click)))
                .Add(NewMenuItem("Double Slip", False, GetTrackDrawingImage(New Track(Track.TrackType.TurnoutDoubleSlip, 1)), "6", New Track.TrackType() {Track.TrackType.TurnoutDoubleSlip}, New RoutedEventHandler(AddressOf PlaceTrack_Click)))
                .Add(NewMenuItem("Three Way", False, GetTrackDrawingImage(New Track(Track.TrackType.TurnoutThreeWay, 1)), "7", New Track.TrackType() {Track.TrackType.TurnoutThreeWay}, New RoutedEventHandler(AddressOf PlaceTrack_Click)))
            End With

            _objPlaceSignal.Header = "Place Signal              Q"
            _objPlaceSignal.Icon = ToMenuImage(TryFindResource("IconSignal"))
            '_objPlaceSignal.InputGestureText = "N"  does not work with sub menus
            With _objPlaceSignal.Items
                .Add(NewSignalMenuItem(SignalLookKey.x2_Gen_Sing, "A"))
                .Add(NewSignalMenuItem(SignalLookKey.x2_Gen_Mult, "A"))
                .Add(NewSignalMenuItem(SignalLookKey.x2_DE_Block, "A"))
                .Add(NewSignalMenuItem(SignalLookKey.x2_DE_Shunt1, "A"))
                .Add(NewSignalMenuItem(SignalLookKey.x2_DE_Shunt2, "A"))
                .Add(NewSignalMenuItem(SignalLookKey.x2_DE_Wait, "A"))
                .Add(NewSignalMenuItem(SignalLookKey.x2_UK_Block, "A"))
                .Add(NewSignalMenuItem(SignalLookKey.x2_UK_Shunt, "A"))
                .Add(New Separator)
                .Add(NewSignalMenuItem(SignalLookKey.x3_Gen_Sing, "S"))
                .Add(NewSignalMenuItem(SignalLookKey.x3_Gen_Mult, "S"))
                .Add(NewSignalMenuItem(SignalLookKey.x3_Automotive, "S"))
                .Add(NewSignalMenuItem(SignalLookKey.x3_DE_Entry, "S"))
                .Add(NewSignalMenuItem(SignalLookKey.x3_UK_Block, "S"))
                .Add(New Separator)
                .Add(NewSignalMenuItem(SignalLookKey.x4_Gen_Sing, "D"))
                .Add(NewSignalMenuItem(SignalLookKey.x4_Gen_Mult, "D"))
                .Add(NewSignalMenuItem(SignalLookKey.x4_DE_Depart, "D"))
                .Add(NewSignalMenuItem(SignalLookKey.x4_DE_Distant, "D"))
                .Add(NewSignalMenuItem(SignalLookKey.x4_UK_Block, "D"))
                .Add(New Separator)
                .Add(NewSignalMenuItem(SignalLookKey.x5_Gen_Sing, "F"))
            End With

        End Sub

        Private Sub ShowContextMenu()
            With _objContextMenu.Items
                .Clear()

                Select Case My.Application.AppMode
                    Case AppMode.Edit

                        'place/replace track
                        Dim objLocTrack As Track = CtcService.Tracks(_sctCurrentCell.Location).LastOrDefault
                        If objLocTrack Is Nothing Then
                            _objPlaceTrack.Header = "Place Track"
                            _objPlaceTrack.Icon = ToMenuImage(TryFindResource("IconTrack"))
                        Else
                            _objPlaceTrack.Header = "Replace Track"
                            _objPlaceTrack.Icon = ToMenuImage(GetTrackDrawingImage(objLocTrack))
                        End If
                        _objPlaceTrack.Tag = _sctCurrentCell.Location
                        .Add(_objPlaceTrack)

                        'add block/route
                        .Add(NewMenuItem("Add Block", False, TryFindResource("IconBlock"), Nothing, Nothing, New RoutedEventHandler(AddressOf AddBlock_Click)))
                        .Add(NewMenuItem("Add Route", False, TryFindResource("IconRoute"), Nothing, Nothing, New RoutedEventHandler(AddressOf AddRoute_Click)))

                        'add signal
                        _objPlaceSignal.Tag = _sctCurrentCell.Location
                        .Add(_objPlaceSignal)

                        'add label/button
                        .Add(NewMenuItem("Place Label", False, TryFindResource("IconLabel"), "W", _sctCurrentCell.Location, New RoutedEventHandler(AddressOf AddLabel_Click)))
                        .Add(NewMenuItem("Place Button", False, TryFindResource("IconButton"), "E", _sctCurrentCell.Location, New RoutedEventHandler(AddressOf AddButton_Click)))

                        .Add(New Separator())

                        Dim objSbContextObj As CtcObjectBase = My.Application.SbContextObj

                        Select Case True
                            Case TypeOf objSbContextObj Is Track
                                .Add(NewMenuItem("Rotate Track Left", False, TryFindResource("IconRotateLeft"), "A", Nothing, New RoutedEventHandler(AddressOf OrientationLeft_Click)))
                                .Add(NewMenuItem("Rotate Track Right", False, TryFindResource("IconRotateRight"), "D", Nothing, New RoutedEventHandler(AddressOf OrientationRight_Click)))
                                .Add(NewMenuItem("Remove Track", False, TryFindResource("IconDelete"), "Del", Nothing, New RoutedEventHandler(AddressOf RemoveObject_Click)))

                            Case TypeOf objSbContextObj Is Block
                                Dim objTrack As Track = CtcService.Tracks(_sctCurrentCell.Location).LastOrDefault
                                If objTrack IsNot Nothing Then
                                    .Add(NewMenuItem("Toggle Track to Block", False, TryFindResource("IconToggle"), "S", objTrack, New RoutedEventHandler(AddressOf TrackToBlock_Click)))
                                End If
                                .Add(NewMenuItem("Clear Block", False, Nothing, Nothing, Nothing, New RoutedEventHandler(AddressOf ClearBlock_Click)))
                                .Add(NewMenuItem("Remove Block", False, TryFindResource("IconDelete"), "Del", Nothing, New RoutedEventHandler(AddressOf RemoveObject_Click)))

                            Case TypeOf objSbContextObj Is Route
                                Dim objTrack As Track = CtcService.Tracks(_sctCurrentCell.Location).LastOrDefault
                                If objTrack IsNot Nothing Then
                                    .Add(NewMenuItem("Toggle Track to Route", False, TryFindResource("IconToggle"), "S", objTrack, New RoutedEventHandler(AddressOf TrackToRoute_Click)))
                                    Dim objRoute As Route = objSbContextObj
                                    If objTrack.IsTurnout And objRoute.RouteElements.Contains(objTrack) Then
                                        Dim objTurnoutStates As MenuItem = NewMenuItem("Tunout State", False, GetTrackDrawingImage(objTrack), Nothing, Nothing, Nothing)
                                        For Each objState As PkStatesList.State In objTrack.States
                                            objTurnoutStates.Items.Add(NewMenuItem(objState.Name, objRoute.RouteElements(objTrack).State = objState.Value, Nothing, "D", New Object() {objTrack, objState.Value}, New RoutedEventHandler(AddressOf CycleRouteState_Click)))
                                        Next
                                        .Add(objTurnoutStates)
                                    End If
                                End If
                                .Add(NewMenuItem("Clear Route", False, Nothing, Nothing, Nothing, New RoutedEventHandler(AddressOf ClearRoute_Click)))
                                .Add(NewMenuItem("Remove Route", False, TryFindResource("IconDelete"), "Del", Nothing, New RoutedEventHandler(AddressOf RemoveObject_Click)))

                            Case TypeOf objSbContextObj Is Signal
                                .Add(NewMenuItem("Copy Signal Style", False, Nothing, "Ctrl+C", Nothing, New RoutedEventHandler(AddressOf CopySignalStyle_Click)))
                                If My.Application.SignalStyle IsNot Nothing Then _
                                    .Add(NewMenuItem("Paste Signal Style", False, Nothing, "Ctrl+V", Nothing, New RoutedEventHandler(AddressOf PasteSignalStyle_Click)))
                                .Add(NewMenuItem("Remove Signal", False, TryFindResource("IconDelete"), "Del", Nothing, New RoutedEventHandler(AddressOf RemoveObject_Click)))

                            Case TypeOf objSbContextObj Is RRAutoLib.CTC.Label
                                .Add(NewMenuItem("Copy Label Style", False, Nothing, "Ctrl+C", Nothing, New RoutedEventHandler(AddressOf CopyLabelStyle_Click)))
                                If My.Application.LabelStyle IsNot Nothing Then _
                                    .Add(NewMenuItem("Paste Label Style", False, Nothing, "Ctrl+V", Nothing, New RoutedEventHandler(AddressOf PasteLabelStyle_Click)))
                                .Add(NewMenuItem("Remove Label", False, TryFindResource("IconDelete"), "Del", Nothing, New RoutedEventHandler(AddressOf RemoveObject_Click)))

                            Case TypeOf objSbContextObj Is RRAutoLib.CTC.Button
                                .Add(NewMenuItem("Copy Button Style", False, Nothing, "Ctrl+C", Nothing, New RoutedEventHandler(AddressOf CopyButtonStyle_Click)))
                                If My.Application.ButtonStyle IsNot Nothing Then _
                                    .Add(NewMenuItem("Paste Button Style", False, Nothing, "Ctrl+V", Nothing, New RoutedEventHandler(AddressOf PasteButtonStyle_Click)))
                                .Add(NewMenuItem("Remove Button", False, TryFindResource("IconDelete"), "Del", Nothing, New RoutedEventHandler(AddressOf RemoveObject_Click)))

                        End Select

                        If Not TypeOf .Item(.Count - 1) Is Separator Then .Add(New Separator())

                        .Add(NewMenuItem("Select Tracks", My.Application.SbTrackMode = SbTrackMode.Tracks, Nothing, "Space+1", SbTrackMode.Tracks, New RoutedEventHandler(AddressOf EditMode_Click)))
                        .Add(NewMenuItem("Select Blocks", My.Application.SbTrackMode = SbTrackMode.Blocks, Nothing, "Space+2", SbTrackMode.Blocks, New RoutedEventHandler(AddressOf EditMode_Click)))
                        .Add(NewMenuItem("Select Routes", My.Application.SbTrackMode = SbTrackMode.Routes, Nothing, "Space+3", SbTrackMode.Routes, New RoutedEventHandler(AddressOf EditMode_Click)))

                    Case AppMode.Operation
                        'get tracks clicked
                        Dim objTracks As List(Of Track) = CtcService.Tracks(_sctCurrentCell.Location)

                        'get routes clicked
                        Dim objRoutes As New List(Of Route)
                        For Each objTrack As Track In objTracks
                            'collect all routes that have this track as a member
                            'since two overlaping tracks can be part of the same route, duplicate routes inserted here are eliminated later with objRoutes.Distinct()
                            objRoutes.AddRange(CtcService.Routes(objTrack))
                        Next

                        'get top layer objects clicked
                        Dim objSignals As New List(Of Signal)
                        Dim objLabels As New List(Of RRAutoLib.CTC.Label)
                        Dim objButtons As New List(Of RRAutoLib.CTC.Button)
                        _objVisuals.TopLayer.HitTest(Nothing,
                            Function(result As HitTestResult) As HitTestResultBehavior
                                Dim objVisual As DrawingVisual = result.VisualHit
                                Dim objCtcObject As CtcObjectBase = _objVisuals.ToCtcObj(objVisual)
                                Select Case True
                                    Case TypeOf objCtcObject Is Signal
                                        objSignals.Add(objCtcObject)

                                    Case TypeOf objCtcObject Is RRAutoLib.CTC.Label
                                        objLabels.Add(objCtcObject)

                                    Case TypeOf objCtcObject Is RRAutoLib.CTC.Button
                                        objButtons.Add(objCtcObject)

                                End Select

                                Return HitTestResultBehavior.Continue
                            End Function,
                            New PointHitTestParameters(_sctCurrentPoint))

                        '------------------------------------

                        'show turnouts to operate
                        For Each objTrack As Track In objTracks
                            If objTrack.IsTurnout Then
                                Dim objTurnoutMenu As MenuItem = NewMenuItem(String.Format("Set Turnout: ""{0}""", objTrack.Name), False, GetTrackDrawingImage(objTrack), Nothing, Nothing, Nothing)
                                For Each objState As PkStatesList.State In objTrack.States
                                    objTurnoutMenu.Items.Add(NewMenuItem(objState.Name, objTrack.State Is objState, Nothing, Nothing, New Object() {objTrack, objState.Value}, New RoutedEventHandler(AddressOf SetObjectState_Click)))
                                Next
                                .Add(objTurnoutMenu)
                            End If
                        Next

                        'show routes to operate
                        For Each objRoute As Route In objRoutes.Distinct()
                            If objRoute.State = Route.RouteState.Locked Then
                                .Add(NewMenuItem(String.Format("Unlock Route: ""{0}""", objRoute.Name), False, TryFindResource("IconRouteLocked"), Nothing, New Object() {objRoute, "Unlock"}, New RoutedEventHandler(AddressOf SetRoute_Click)))
                            Else
                                Dim strIconRes As String
                                Select Case objRoute.State
                                    Case Route.RouteState.Unlocked : strIconRes = "IconRoute"
                                    Case Route.RouteState.Pending : strIconRes = "IconRoutePending"
                                End Select
                                Dim objSetRouteMenu As MenuItem = NewMenuItem(String.Format("Set Route: ""{0}""", objRoute.Name), False, TryFindResource(strIconRes), Nothing, Nothing, Nothing)
                                objSetRouteMenu.Items.Add(NewMenuItem("Set Only", False, Nothing, Nothing, New Object() {objRoute, "Set"}, New RoutedEventHandler(AddressOf SetRoute_Click)))
                                objSetRouteMenu.Items.Add(NewMenuItem("Set and Lock", False, Nothing, Nothing, New Object() {objRoute, "Lock"}, New RoutedEventHandler(AddressOf SetRoute_Click)))
                                .Add(objSetRouteMenu)
                            End If
                        Next

                        'show sensors to operate
                        For Each objTrack As Track In objTracks
                            If objTrack.Block IsNot Nothing AndAlso objTrack.Block.Sensor IsNot Nothing Then
                                Dim objSensor As Sensor = objTrack.Block.Sensor
                                Dim strIconRes As String
                                Select Case objSensor.State
                                    Case OnOff.On : strIconRes = "IconSensor"
                                    Case OnOff.Off : strIconRes = "IconSensorOff"
                                End Select
                                Dim objSensorMenu As MenuItem = NewMenuItem(String.Format("Set Sensor: ""{0}""", objSensor.Name), False, TryFindResource(strIconRes), Nothing, Nothing, Nothing)
                                For Each enuState As OnOff In New OnOff() {OnOff.On, OnOff.Off}
                                    objSensorMenu.Items.Add(NewMenuItem(enuState.ToString, objSensor.State = enuState, Nothing, Nothing, New Object() {objSensor, enuState}, New RoutedEventHandler(AddressOf SetSensor_Click)))
                                Next
                                .Add(objSensorMenu)
                            End If
                        Next

                        'show signals to operate
                        For Each objSignal As Signal In objSignals
                            Dim objSignalMenu As MenuItem = NewMenuItem(String.Format("Set Signal: ""{0}""", objSignal.Name), False, GetSignalDrawingImage(objSignal), Nothing, Nothing, Nothing)
                            For Each objState As PkStatesList.State In objSignal.Aspects
                                objSignalMenu.Items.Add(NewMenuItem(objState.Name, objSignal.Aspect Is objState, Nothing, Nothing, New Object() {objSignal, objState.Value}, New RoutedEventHandler(AddressOf SetObjectState_Click)))
                            Next
                            .Add(objSignalMenu)
                        Next

                        'inspection menu items ----------------------------------------------------------------------------

                        Dim objInspectMenu As MenuItem = NewMenuItem("Inspect Objects", False, Nothing, Nothing, Nothing, Nothing)

                        'show tracks select
                        For Each objTrack As Track In objTracks
                            objInspectMenu.Items.Add(NewMenuItem(String.Format("{0}: ""{1}""", If(objTrack.IsTurnout, "Turnout", "Track"), objTrack.Name),
                                False, GetTrackDrawingImage(objTrack), Nothing, objTrack, New RoutedEventHandler(AddressOf SelectObject_Click)))
                        Next

                        'show blocks select
                        For Each objTrack As Track In objTracks
                            If objTrack.Block IsNot Nothing Then
                                objInspectMenu.Items.Add(NewMenuItem(String.Format("Block: ""{0}""", objTrack.Block.Name),
                                    False, TryFindResource("IconBlock"), Nothing, objTrack.Block, New RoutedEventHandler(AddressOf SelectObject_Click)))
                            End If
                        Next

                        'show routes select
                        For Each objRoute As Route In objRoutes.Distinct()
                            Dim strIconRes As String
                            Select Case objRoute.State
                                Case Route.RouteState.Unlocked : strIconRes = "IconRoute"
                                Case Route.RouteState.Locked : strIconRes = "IconRouteLocked"
                                Case Route.RouteState.Pending : strIconRes = "IconRoutePending"
                            End Select
                            objInspectMenu.Items.Add(NewMenuItem(String.Format("Route: ""{0}""", objRoute.Name),
                                False, TryFindResource(strIconRes), Nothing, objRoute, New RoutedEventHandler(AddressOf SelectObject_Click)))
                        Next

                        'show sensor select
                        For Each objTrack As Track In objTracks
                            If objTrack.Block IsNot Nothing AndAlso objTrack.Block.Sensor IsNot Nothing Then
                                Dim objSensor As Sensor = objTrack.Block.Sensor
                                Dim strIconRes As String
                                Select Case objSensor.State
                                    Case OnOff.On : strIconRes = "IconSensor"
                                    Case OnOff.Off : strIconRes = "IconSensorOff"
                                End Select
                                objInspectMenu.Items.Add(NewMenuItem(String.Format("Sensor: ""{0}""", objSensor.Name),
                                    False, TryFindResource(strIconRes), Nothing, objSensor, New RoutedEventHandler(AddressOf SelectObject_Click)))
                            End If
                        Next

                        'show signal select
                        For Each objSignal As Signal In objSignals
                            objInspectMenu.Items.Add(NewMenuItem(String.Format("Signal: ""{0}""", objSignal.Name),
                                False, GetSignalDrawingImage(objSignal), Nothing, objSignal, New RoutedEventHandler(AddressOf SelectObject_Click)))
                        Next

                        'show label select
                        For Each objLabel As RRAutoLib.CTC.Label In objLabels
                            objInspectMenu.Items.Add(NewMenuItem(String.Format("Label: ""{0}""", objLabel.Name),
                                False, TryFindResource("IconLabel"), Nothing, objLabel, New RoutedEventHandler(AddressOf SelectObject_Click)))
                        Next

                        'show button select
                        For Each objButton As RRAutoLib.CTC.Button In objButtons
                            objInspectMenu.Items.Add(NewMenuItem(String.Format("Button: ""{0}""", objButton.Name),
                                False, GetButtonDrawingImage(objButton), Nothing, objButton, New RoutedEventHandler(AddressOf SelectObject_Click)))
                        Next

                        If objInspectMenu.Items.Count > 0 Then
                            If .Count > 0 Then .Add(New Separator())
                            .Add(objInspectMenu)
                        End If

                End Select

                If .Count > 0 Then
                    _objContextMenu.PlacementTarget = Me
                    _objContextMenu.IsOpen = True
                End If

            End With
        End Sub


        Private Function ToMenuImage(objImageSource As ImageSource) As Image
            If objImageSource IsNot Nothing Then
                Dim objImage As New Image()
                objImage.Width = 16
                objImage.Height = 16
                objImage.Source = objImageSource
                Return objImage
            End If
        End Function

        Private Function NewMenuItem(strText As String, blnChecked As Boolean, objImageSource As ImageSource, strShortcut As String, objTag As Object, delHandler As RoutedEventHandler) As MenuItem
            Dim objMenuItem As New MenuItem()
            With objMenuItem
                .Header = strText
                .IsChecked = blnChecked
                .Icon = ToMenuImage(objImageSource)
                .InputGestureText = strShortcut
                .Tag = objTag
                If delHandler IsNot Nothing Then AddHandler .Click, delHandler
            End With
            Return objMenuItem
        End Function

        Private Function NewSignalMenuItem(enuSignalLookKey As SignalLookKey, strShortcut As String) As MenuItem
            Dim sctSignalLook As SignalLook = GetSignalLook(enuSignalLookKey)
            Dim objSignal As New Signal(sctSignalLook.Config, enuSignalLookKey.ToString)
            Return NewMenuItem(sctSignalLook.Name, False, GetSignalDrawingImage(objSignal), strShortcut, enuSignalLookKey, New RoutedEventHandler(AddressOf AddSignal_Click))
        End Function

        '------ event handlers --------------------------------------------------------------------

        Private Sub SelectObject_Click(sender As Object, e As RoutedEventArgs)
            Dim objClickedObject As CtcObjectBase = sender.Tag
            If objClickedObject IsNot My.Application.ContextObj Then
                My.Application.SetContextObj(Me, objClickedObject)
            End If
        End Sub


        '------ track edit

        Private Sub PlaceTrack_Click(sender As Object, e As RoutedEventArgs)
            PlaceTrack(sender.Parent.Tag, sender.Tag)
        End Sub

        Private Sub PlaceTrack(objCurrentCellLocation As Location, enuTrackTypes() As Track.TrackType)
            Dim objTrack As Track = CtcService.Tracks(objCurrentCellLocation).LastOrDefault
            If objTrack Is Nothing Then
                objTrack = CtcService.Tracks.Add()
                objTrack.Location = objCurrentCellLocation
                objTrack.Orientation = My.Application.LastTrackOrientation
            End If
            With objTrack
                'set track type or toggle variant
                Dim blnToFirstOption As Boolean = True
                For bytIdx As Byte = 0 To enuTrackTypes.Length - 1
                    'if current track type is already one of the given track types and there are more toggle alternates, then toggle to the next one in the list
                    If .Type = enuTrackTypes(bytIdx) AndAlso bytIdx < enuTrackTypes.Length - 1 Then
                        .Type = enuTrackTypes(bytIdx + 1)
                        blnToFirstOption = False
                        Exit For
                    End If
                Next
                If blnToFirstOption Then .Type = enuTrackTypes(0)

                .NotifyObjectChanged(Me)
            End With
            My.Application.SetLayoutDirty()
            My.Application.SetContextObj(Me, objTrack)
        End Sub

        Private Sub OrientationRight_Click(sender As Object, e As RoutedEventArgs)
            Dim objTrack As Track = My.Application.SbContextObj
            With objTrack
                If .Orientation < 8 Then
                    .Orientation += 1
                Else
                    .Orientation = 1
                End If
                My.Application.LastTrackOrientation = .Orientation
                .NotifyObjectChanged(Me)
            End With
            My.Application.SetLayoutDirty()
        End Sub

        Private Sub OrientationLeft_Click(sender As Object, e As RoutedEventArgs)
            Dim objTrack As Track = My.Application.SbContextObj
            With objTrack
                If .Orientation > 1 Then
                    .Orientation -= 1
                Else
                    .Orientation = 8
                End If
                My.Application.LastTrackOrientation = .Orientation
                .NotifyObjectChanged(Me)
            End With
            My.Application.SetLayoutDirty()
        End Sub


        '------ block edit

        Private Sub TrackToBlock_Click(sender As Object, e As RoutedEventArgs)
            Dim objTrack As Track = sender.Tag
            Dim objBlock As Block = My.Application.SbContextObj
            If objTrack.Block Is objBlock Then
                objTrack.Block = Nothing
            Else
                objTrack.Block = objBlock
            End If
            objTrack.NotifyObjectChanged(Me)
            My.Application.SetLayoutDirty()
        End Sub

        Private Sub AddBlock_Click(sender As Object, e As RoutedEventArgs)
            Dim objBlock As Block = CtcService.Blocks.Add()
            objBlock.NotifyObjectChanged(Me)
            My.Application.SetLayoutDirty()
            My.Application.SetContextObj(Me, objBlock)
        End Sub

        Private Sub ClearBlock_Click(sender As Object, e As RoutedEventArgs)
            Dim objBlock As Block = My.Application.SbContextObj
            CtcService.Tracks.RemoveBlockAssoc(objBlock)
            My.Application.SetLayoutDirty()
        End Sub


        '------ route edit

        Private Sub TrackToRoute_Click(sender As Object, e As RoutedEventArgs)
            Dim objTrack As Track = sender.Tag
            Dim objRoute As Route = My.Application.SbContextObj
            If objRoute.RouteElements.Contains(objTrack) Then
                objRoute.RouteElements.Remove(objTrack)
            Else
                Dim enuTrackState As Track.TrackState
                If objTrack.IsTurnout Then
                    enuTrackState = objTrack.State.Value
                Else
                    enuTrackState = Track.TrackState.NoState
                End If
                objRoute.RouteElements.Add(New Route.RouteElement(objTrack, enuTrackState))
            End If
            objRoute.NotifyObjectChanged(Me)
            My.Application.SetLayoutDirty()
        End Sub

        Private Sub AddRoute_Click(sender As Object, e As RoutedEventArgs)
            Dim objRoute As Route = CtcService.Routes.Add()
            objRoute.NotifyObjectChanged(Me)
            My.Application.SetLayoutDirty()
            My.Application.SetContextObj(Me, objRoute)
        End Sub

        Private Sub ClearRoute_Click(sender As Object, e As RoutedEventArgs)
            Dim objRoute As Route = My.Application.SbContextObj
            With objRoute
                .RouteElements.Clear()
                .NotifyObjectChanged(Me)
            End With
            My.Application.SetLayoutDirty()
        End Sub

        Private Sub CycleRouteState_Click(sender As Object, e As RoutedEventArgs)
            Dim objTrack As Track = sender.Tag(0)
            Dim objRoute As Route = My.Application.SbContextObj
            Dim objRouteElement As Route.RouteElement = objRoute.RouteElements(objTrack)
            objRouteElement.State = CType(sender.Tag(1), Byte)
            objRoute.NotifyObjectChanged(Me)
            My.Application.SetLayoutDirty()
        End Sub


        '------ signal edit

        Private Sub AddSignal_Click(sender As Object, e As RoutedEventArgs)
            Dim sctLocation As Location = sender.Parent.Tag
            Dim enuLookKey As SignalLookKey = sender.Tag
            AddSignal(sctLocation, enuLookKey)
        End Sub

        Private Sub AddSignal(objCurrentCellLocation As Location, enuLookKey As SignalLookKey)
            Dim objSignal As Signal = CtcService.Signals.Add()
            With objSignal
                .Location = objCurrentCellLocation
                If enuLookKey > 0 Then
                    'if config is given, apply it
                    .Config = GetSignalLook(enuLookKey).Config
                    .LookKey = enuLookKey.ToString
                Else
                    If My.Application.SignalStyle IsNot Nothing Then
                        'if a style exists in the clipboard, applied it
                        .Config = My.Application.SignalStyle.Config
                        .LookKey = My.Application.SignalStyle.LookKey
                    Else
                        'if style clipboard is empty, default the config
                        .Config = GetSignalLook(SignalLookKey.x2_Gen_Mult).Config
                        .LookKey = SignalLookKey.x2_Gen_Mult.ToString
                    End If
                End If
                If My.Application.SignalStyle IsNot Nothing Then
                    .DrawScale = My.Application.SignalStyle.DrawScale
                    .CellAlign = My.Application.SignalStyle.CellAlign
                    .AlignOffsetX = My.Application.SignalStyle.AlignOffsetX
                    .AlignOffsetY = My.Application.SignalStyle.AlignOffsetY
                End If
                .NotifyObjectChanged(Me)
            End With
            My.Application.SetLayoutDirty()
            My.Application.SetContextObj(Me, objSignal)
        End Sub

        Private Sub SetSignalType(enuSignalLookKeys() As SignalLookKey)
            Dim objSignal As Signal = My.Application.SbContextObj
            With objSignal
                'set signal look or toggle variant
                Dim blnToFirstOption As Boolean = True
                For bytIdx As Byte = 0 To enuSignalLookKeys.Length - 1
                    'if current signal look is already one of the given signal looks and there are more toggle alternates, then toggle to the next one in the list
                    If .LookKey = enuSignalLookKeys(bytIdx).ToString AndAlso bytIdx < enuSignalLookKeys.Length - 1 Then
                        .Config = GetSignalLook(enuSignalLookKeys(bytIdx + 1)).Config
                        .LookKey = enuSignalLookKeys(bytIdx + 1).ToString
                        blnToFirstOption = False
                        Exit For
                    End If
                Next
                If blnToFirstOption Then
                    .Config = GetSignalLook(enuSignalLookKeys(0)).Config
                    .LookKey = enuSignalLookKeys(0).ToString
                End If
                .NotifyObjectChanged(Me)
            End With
            My.Application.SetLayoutDirty()
        End Sub

        Private Sub CopySignalStyle_Click(sender As Object, e As RoutedEventArgs)
            My.Application.SignalStyle = My.Application.SbContextObj
        End Sub

        Private Sub PasteSignalStyle_Click(sender As Object, e As RoutedEventArgs)
            If My.Application.SignalStyle Is Nothing Then Exit Sub

            With DirectCast(My.Application.SbContextObj, Signal)
                .Config = My.Application.SignalStyle.Config
                .LookKey = My.Application.SignalStyle.LookKey
                .DrawScale = My.Application.SignalStyle.DrawScale
                .CellAlign = My.Application.SignalStyle.CellAlign
                .AlignOffsetX = My.Application.SignalStyle.AlignOffsetX
                .AlignOffsetY = My.Application.SignalStyle.AlignOffsetY
                .NotifyObjectChanged(Me)
            End With
            My.Application.SetLayoutDirty()
        End Sub


        '------ label edit

        Private Sub AddLabel_Click(sender As Object, e As RoutedEventArgs)
            AddLabel(sender.Tag)
        End Sub

        Private Sub AddLabel(objCurrentCellLocation As Location)
            Dim objLabel As RRAutoLib.CTC.Label = CtcService.Labels.Add()
            With objLabel
                .Location = objCurrentCellLocation
                If My.Application.LabelStyle IsNot Nothing Then
                    .FontFamily = My.Application.LabelStyle.FontFamily
                    .TextPadding = My.Application.LabelStyle.TextPadding
                    .TextColor = My.Application.LabelStyle.TextColor
                    .BackColor = My.Application.LabelStyle.BackColor
                    .BackCornerRadius = My.Application.LabelStyle.BackCornerRadius
                    .DrawScale = My.Application.LabelStyle.DrawScale
                    .CellAlign = My.Application.LabelStyle.CellAlign
                    .AlignOffsetX = My.Application.LabelStyle.AlignOffsetX
                    .AlignOffsetY = My.Application.LabelStyle.AlignOffsetY
                End If
                .NotifyObjectChanged(Me)
            End With
            My.Application.SetLayoutDirty()
            My.Application.SetContextObj(Me, objLabel)
        End Sub

        Private Sub CopyLabelStyle_Click(sender As Object, e As RoutedEventArgs)
            My.Application.LabelStyle = My.Application.SbContextObj
        End Sub

        Private Sub PasteLabelStyle_Click(sender As Object, e As RoutedEventArgs)
            If My.Application.LabelStyle Is Nothing Then Exit Sub

            With DirectCast(My.Application.SbContextObj, RRAutoLib.CTC.Label)
                .FontFamily = My.Application.LabelStyle.FontFamily
                .TextPadding = My.Application.LabelStyle.TextPadding
                .TextColor = My.Application.LabelStyle.TextColor
                .BackColor = My.Application.LabelStyle.BackColor
                .BackCornerRadius = My.Application.LabelStyle.BackCornerRadius
                .DrawScale = My.Application.LabelStyle.DrawScale
                .CellAlign = My.Application.LabelStyle.CellAlign
                .AlignOffsetX = My.Application.LabelStyle.AlignOffsetX
                .AlignOffsetY = My.Application.LabelStyle.AlignOffsetY
                .NotifyObjectChanged(Me)
            End With
            My.Application.SetLayoutDirty()
        End Sub


        '------ button edit

        Private Sub AddButton_Click(sender As Object, e As RoutedEventArgs)
            AddButton(sender.Tag)
        End Sub

        Private Sub AddButton(objCurrentCellLocation As Location)
            Dim objButton As RRAutoLib.CTC.Button = CtcService.Buttons.Add()
            With objButton
                .Location = objCurrentCellLocation
                If My.Application.ButtonStyle IsNot Nothing Then
                    .Behavior = My.Application.ButtonStyle.Behavior
                    .Look = My.Application.ButtonStyle.Look
                    .DrawScale = My.Application.ButtonStyle.DrawScale
                    .CellAlign = My.Application.ButtonStyle.CellAlign
                    .AlignOffsetX = My.Application.ButtonStyle.AlignOffsetX
                    .AlignOffsetY = My.Application.ButtonStyle.AlignOffsetY
                End If
                .NotifyObjectChanged(Me)
            End With
            My.Application.SetLayoutDirty()
            My.Application.SetContextObj(Me, objButton)
        End Sub

        Private Sub CopyButtonStyle_Click(sender As Object, e As RoutedEventArgs)
            My.Application.ButtonStyle = My.Application.SbContextObj
        End Sub

        Private Sub PasteButtonStyle_Click(sender As Object, e As RoutedEventArgs)
            If My.Application.ButtonStyle Is Nothing Then Exit Sub

            With DirectCast(My.Application.SbContextObj, RRAutoLib.CTC.Button)
                .Behavior = My.Application.ButtonStyle.Behavior
                .Look = My.Application.ButtonStyle.Look
                .DrawScale = My.Application.ButtonStyle.DrawScale
                .CellAlign = My.Application.ButtonStyle.CellAlign
                .AlignOffsetX = My.Application.ButtonStyle.AlignOffsetX
                .AlignOffsetY = My.Application.ButtonStyle.AlignOffsetY
                .NotifyObjectChanged(Me)
            End With
            My.Application.SetLayoutDirty()
        End Sub


        '------ global edit

        Private Sub RemoveObject_Click(sender As Object, e As RoutedEventArgs)
            With My.Application.SbContextObj
                .DeleteSelf()
                .NotifyObjectChanged(Me)
            End With
            My.Application.SetLayoutDirty()
        End Sub

        Private Sub EditMode_Click(sender As Object, e As RoutedEventArgs)
            My.Application.SbTrackMode = sender.tag
        End Sub

        Private Sub ShiftLocation(srtCol As Short, srtRow As Short)
            Dim objSbContextObj As CtcObjectBase = My.Application.SbContextObj
            With DirectCast(objSbContextObj, ILocation)
                .Location = .Location.Offset(srtCol, srtRow)
            End With
            objSbContextObj.NotifyObjectChanged(Me)
            My.Application.SetLayoutDirty()
        End Sub

        Private Sub ScaleObject(sinIncrement As Single)
            Dim objSbContextObj As CtcObjectBase = My.Application.SbContextObj
            DirectCast(objSbContextObj, ITopLayer).DrawScale += sinIncrement
            objSbContextObj.NotifyObjectChanged(Me)
            My.Application.SetLayoutDirty()
        End Sub

        Private Sub CycleAlignment(blnForward As Boolean)
            Dim objSbContextObj As CtcObjectBase = My.Application.SbContextObj
            Dim enuaAlignments() As Alignment = [Enum].GetValues(GetType(Alignment))
            With DirectCast(objSbContextObj, ITopLayer)
                If blnForward Then
                    .CellAlign = If(.CellAlign = enuaAlignments.Last, enuaAlignments.First, .CellAlign + 1)
                Else
                    .CellAlign = If(.CellAlign = enuaAlignments.First, enuaAlignments.Last, .CellAlign - 1)
                End If
                .AlignOffsetX = 0
                .AlignOffsetY = 0
            End With
            objSbContextObj.NotifyObjectChanged(Me)
            My.Application.SetLayoutDirty()
        End Sub

        Private Sub ShiftAlignOffset(sinX As Single, sinY As Single)
            Dim objSbContextObj As CtcObjectBase = My.Application.SbContextObj

            With DirectCast(objSbContextObj, ITopLayer)

                Select Case .AlignOffsetX + sinX
                    Case Is > SbConst.CellSize.Width / 2
                        .Location = .Location.Offset(1, 0)
                        .AlignOffsetX = .AlignOffsetX + sinX - SbConst.CellSize.Width
                    Case Is < -SbConst.CellSize.Width / 2
                        .Location = .Location.Offset(-1, 0)
                        .AlignOffsetX = .AlignOffsetX + sinX + SbConst.CellSize.Width
                    Case Else
                        .AlignOffsetX += sinX
                End Select

                Select Case .AlignOffsetY + sinY
                    Case Is > SbConst.CellSize.Height / 2
                        .Location = .Location.Offset(0, 1)
                        .AlignOffsetY = .AlignOffsetY + sinY - SbConst.CellSize.Height
                    Case Is < -SbConst.CellSize.Height / 2
                        .Location = .Location.Offset(0, -1)
                        .AlignOffsetY = .AlignOffsetY + sinY + SbConst.CellSize.Height
                    Case Else
                        .AlignOffsetY += sinY
                End Select

            End With
            objSbContextObj.NotifyObjectChanged(Me)
            My.Application.SetLayoutDirty()
        End Sub


        '------ setting state

        Private Sub SetObjectState_Click(sender As Object, e As RoutedEventArgs)
            DirectCast(sender.Tag(0), IPkStates).SetState(DirectCast(sender.Tag(1), Byte))
        End Sub

        Private Sub SetRoute_Click(sender As Object, e As RoutedEventArgs)
            Select Case sender.Tag(1)
                Case "Set"
                    DirectCast(sender.Tag(0), Route).Set()
                Case "Lock"
                    DirectCast(sender.Tag(0), Route).Lock()
                Case "Unlock"
                    DirectCast(sender.Tag(0), Route).Unlock()
            End Select
        End Sub

        Private Sub SetSensor_Click(sender As Object, e As RoutedEventArgs)
            DirectCast(sender.Tag(0), Sensor).SetState(sender.Tag(1))
        End Sub

#End Region

#Region "Custom Event Handling"

        Private Sub ObjectChanged(objSender As Object, objChangedObject As CtcObjectBase, objPreviousObject As CtcObjectBase)
            Select Case True
                Case TypeOf objChangedObject Is Track
                    DrawTrack(objChangedObject, False)

                Case TypeOf objChangedObject Is Block
                    If My.Application.AppMode = AppMode.Operation Then
                        'only in operation mode does a block have values that would change the appearance of the switchboard 
                        'in edit mode block/track associations are stored in track
                        DrawObjectOnce(objChangedObject, New List(Of CtcObjectBase))
                    End If

                Case TypeOf objChangedObject Is Route
                    Dim objDrawnObjects As New List(Of CtcObjectBase)       'prevents drawing tracks, that were shared by the old route and the new route, twice
                    DrawObjectOnce(objChangedObject, objDrawnObjects)
                    DrawObjectOnce(objPreviousObject, objDrawnObjects)

                Case TypeOf objChangedObject Is Signal
                    DrawSignal(objChangedObject)

                Case TypeOf objChangedObject Is RRAutoLib.CTC.Label
                    DrawLabel(objChangedObject)

                Case TypeOf objChangedObject Is RRAutoLib.CTC.Button
                    DrawButton(objChangedObject)

            End Select
        End Sub

        Private Sub ModeChanged()
            DrawAll()
        End Sub

        Private Sub SbColorChanged()
            DrawAll()
        End Sub

        Private Sub ContextObjChanged(objSender As Object, objPrevContextObj As Object, objCurrContextObj As Object)
            'only draw context highlighting in edit mode 
            If My.Application.AppMode = AppMode.Edit Then

                Dim objPrevSbContextObj As CtcObjectBase = My.Application.SbContextObj(objPrevContextObj)
                Dim objCurrSbContextObj As CtcObjectBase = My.Application.SbContextObj(objCurrContextObj)

                If objPrevSbContextObj IsNot objCurrSbContextObj Then

                    'make sure for overlapping objects of the same type that the context one is top-most
                    Select Case True
                        Case TypeOf objCurrSbContextObj Is Track OrElse
                             TypeOf objCurrSbContextObj Is Signal OrElse
                             TypeOf objCurrSbContextObj Is RRAutoLib.CTC.Label OrElse
                             TypeOf objCurrSbContextObj Is RRAutoLib.CTC.Button
                            _objVisuals.BringToFront(objCurrSbContextObj)

                        Case TypeOf objCurrSbContextObj Is Block
                            'BringToFront modifies the CtcService.Tracks collection which is not allowed while iterating the same collection
                            'so I used an intermediary objTracks collection to separate the iteration from the BringToFront call
                            Dim objTracks As New List(Of Track)(CtcService.Tracks.Where(Function(t As Track) t.Block Is objCurrSbContextObj))
                            For Each objTrack As Track In objTracks
                                _objVisuals.BringToFront(objTrack)
                            Next

                        Case TypeOf objCurrSbContextObj Is Route
                            For Each objRouteElement As Route.RouteElement In DirectCast(objCurrSbContextObj, Route).RouteElements
                                _objVisuals.BringToFront(objRouteElement.Track)
                            Next

                    End Select

                    'refresh selection
                    Dim objDrawnObjects As New List(Of CtcObjectBase)             'prevents drawing the same object twice
                    If TypeOf objPrevSbContextObj Is Track OrElse
                       TypeOf objPrevSbContextObj Is Block OrElse
                       TypeOf objPrevSbContextObj Is Route Then
                        DrawObjectOnce(objPrevSbContextObj, objDrawnObjects)      'redraw old context object so selection can be undone
                    End If
                    Select Case True
                        Case TypeOf objCurrSbContextObj Is Signal
                            DrawSignal(objCurrSbContextObj, True)                 'redraw selection around current signal

                        Case TypeOf objCurrSbContextObj Is RRAutoLib.CTC.Label
                            DrawLabel(objCurrSbContextObj, True)                  'redraw selection around current label

                        Case TypeOf objCurrSbContextObj Is RRAutoLib.CTC.Button
                            DrawButton(objCurrSbContextObj, True)                 'redraw selection around current button

                        Case Else
                            DrawSelAdorner(Nothing)                               'erase selection adorner
                            DrawObjectOnce(objCurrSbContextObj, objDrawnObjects)  'redraw new context object so selection can be applied                    
                    End Select

                End If

            End If
        End Sub

#End Region

        Public Sub Dispose()
            'remove event handlers
            RemoveHandler CtcService.ObjectChanged, AddressOf ObjectChanged
            RemoveHandler CtcService.ModeChanged, AddressOf ModeChanged
            RemoveHandler My.Application.SbColorChanged, AddressOf SbColorChanged
            RemoveHandler My.Application.ContextObjChanged, AddressOf ContextObjChanged
        End Sub

    End Class

End Namespace