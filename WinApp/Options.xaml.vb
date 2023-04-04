Imports RRAuto.PropertyGridHelpers
Imports RRAutoLib.CTC
Imports RRAutoLib.Remoting

Namespace Windows

    Partial Public Class Options

        Private Sub Options_Initialized(sender As Object,  e As System.EventArgs) Handles Me.Initialized
            Me.PropertyGrid.SelectedObject = New PgOptions
        End Sub

        Private Async Sub cmdOK_Click(sender As Object, e As RoutedEventArgs) Handles cmdOK.Click
            Dim objPgOptions As PgOptions = Me.PropertyGrid.SelectedObject
            With My.Settings

                'application ----------------------------------------

                .LoadLastFile = objPgOptions.LoadLastFile
                .ListMaxLines = objPgOptions.ListMaxLines

                'loconet --------------------------------------------

                .ComPort = objPgOptions.ComPort.Replace("COM", "")
                .BaudRate = objPgOptions.BaudRate
                .FlowControl = objPgOptions.FlowControl

                If .SimulationMode <> objPgOptions.SimulationMode Then
                    'unbind all local engines so that they can be re-bound correctly in simulation mode
                    For Each objEngine As Engine In CtcService.Engines
                        Await objEngine.UnbindSlot(False)
                    Next

                    'has to be set after the unbind so old SimulationMode value is used during the UnbindSlot
                    CtcService.SimulationMode = objPgOptions.SimulationMode

                    .SimulationMode = objPgOptions.SimulationMode
                End If

                'operation ------------------------------------------

                .BroadcastStates = objPgOptions.BroadcastStates
                .QuerySensors = objPgOptions.QuerySensors
                .ClearCtcEvents = objPgOptions.ClearCtcEvents

                .ForceSetState = objPgOptions.ForceSetState
                CtcService.ForceSetState = objPgOptions.ForceSetState

                'remoting -------------------------------------------

                .RemServPort = objPgOptions.RemServPort

                'switchboard ----------------------------------------

                .DefaultZoomLevel = objPgOptions.DefaultZoomLevel
                .ZoomIncrement = objPgOptions.ZoomIncrement

                Dim blnColorUpdated As Boolean = False

                If .BackColor <> objPgOptions._sctBackColor Then
                    .BackColor = objPgOptions._sctBackColor
                    blnColorUpdated = True
                End If

                If .GridColor <> objPgOptions._sctGridColor Then
                    .GridColor = objPgOptions._sctGridColor
                    blnColorUpdated = True
                End If

                If .TurnoutSetColor <> objPgOptions._sctTurnoutSetColor Then
                    .TurnoutSetColor = objPgOptions._sctTurnoutSetColor
                    blnColorUpdated = True
                End If

                If .TurnoutPendingColor <> objPgOptions._sctTurnoutPendingColor Then
                    .TurnoutPendingColor = objPgOptions._sctTurnoutPendingColor
                    blnColorUpdated = True
                End If

                If .TrackNoBlockColor <> objPgOptions._sctTrackNoBlockColor Then
                    .TrackNoBlockColor = objPgOptions._sctTrackNoBlockColor
                    blnColorUpdated = True
                End If

                If .TurnoutLockedColor <> objPgOptions._sctTurnoutLockedColor Then
                    .TurnoutLockedColor = objPgOptions._sctTurnoutLockedColor
                    blnColorUpdated = True
                End If

                If .FreeBlockColor <> objPgOptions._sctFreeBlockColor Then
                    .FreeBlockColor = objPgOptions._sctFreeBlockColor
                    blnColorUpdated = True
                End If

                If .ResBlockColor <> objPgOptions._sctResBlockColor Then
                    .ResBlockColor = objPgOptions._sctResBlockColor
                    blnColorUpdated = True
                End If

                If .ResOccBlockColor <> objPgOptions._sctResOccBlockColor Then
                    .ResOccBlockColor = objPgOptions._sctResOccBlockColor
                    blnColorUpdated = True
                End If

                If .OccBlockColor <> objPgOptions._sctOccBlockColor Then
                    .OccBlockColor = objPgOptions._sctOccBlockColor
                    blnColorUpdated = True
                End If

                If .SelectionColor <> objPgOptions._sctSelectionColor Then
                    .SelectionColor = objPgOptions._sctSelectionColor
                    blnColorUpdated = True
                End If

                If blnColorUpdated Then My.Application.NotifySbColorChanged()
            End With

            Me.Close()
        End Sub

        Private Sub cmdCancel_Click(sender As Object,  e As RoutedEventArgs) Handles cmdCancel.Click
            Me.Close()
        End Sub

    End Class

End Namespace

