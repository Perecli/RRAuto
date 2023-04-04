Imports System.Speech.Recognition
Imports System.Speech.Synthesis
Imports RRAutoLib.Loconet
Imports RRAutoLib.CTC
Imports RRAutoLib.Scripting

Public NotInheritable Class SpeechRecService

    Private Shared _objRecognizer As SpeechRecognitionEngine
    Private Shared _objSpeaker As SpeechSynthesizer

    Private Shared _blnPaused As Boolean
    Private Shared _objContextEngine As Engine

    Friend Shared Property Paused() As Boolean
        Get
            Return _blnPaused
        End Get
        Set(value As Boolean)
            _blnPaused = value
        End Set
    End Property

    Friend Shared Sub Start()
        'if speech recognition is disabled or already started, then do nothing
        If Not My.Settings.SpeechService OrElse _objRecognizer IsNot Nothing Then Exit Sub

        _blnPaused = False              'always listen when going to operation mode even if paused from previous session
        _objContextEngine = Nothing     'release context engine from previous operation session

        _objRecognizer = New SpeechRecognitionEngine
        With _objRecognizer
            Try
                .SetInputToDefaultAudioDevice()
            Catch ex As PlatformNotSupportedException
                MessageBox.Show(My.Application.MainWindow,
                                "The speech recognition engine was not found." & vbCrLf &
                                "This feature is only supported on Windows Vista and above." & vbCrLf &
                                "To avoid this message, disable speech recognition.",
                                My.Application.Name, MessageBoxButton.OK, MessageBoxImage.Exclamation)
                _objRecognizer = Nothing
                Exit Sub
            Catch ex As InvalidOperationException
                MessageBox.Show(My.Application.MainWindow, ex.Message & vbCrLf &
                                "A recording device for speech recognition may not be present." & vbCrLf &
                                "To avoid this message, disable speech recognition.",
                                My.Application.Name, MessageBoxButton.OK, MessageBoxImage.Exclamation)
                _objRecognizer = Nothing
                Exit Sub
            Catch ex As Exception
                MessageBox.Show(My.Application.MainWindow,
                                "Error occurred initializing the speech recognition engine:" & vbCrLf &
                                ex.Message & vbCrLf &
                                "To avoid this message, disable speech recognition.",
                                My.Application.Name, MessageBoxButton.OK, MessageBoxImage.Exclamation)
                _objRecognizer = Nothing
                Exit Sub
            End Try
            .UnloadAllGrammars()
            AddHandler .SpeechRecognized, AddressOf SpeechRecognized
            AddHandler .SpeechHypothesized, AddressOf SpeechHypothesized
            AddHandler .SpeechRecognitionRejected, AddressOf SpeechRecognitionRejected
            .LoadGrammar(GetGrammars())
            .RecognizeAsync(RecognizeMode.Multiple)
        End With

        _objSpeaker = New SpeechSynthesizer
        RaiseEvent SpeechActivity(ActivityType.SysMessage, "Listening for commands")
    End Sub

    Friend Shared Sub [Stop]()
        If _objRecognizer Is Nothing Then Exit Sub

        With _objRecognizer
            .RecognizeAsyncStop()
            RaiseEvent SpeechActivity(ActivityType.Null, Nothing)
            RemoveHandler .SpeechRecognized, AddressOf SpeechRecognized
            RemoveHandler .SpeechHypothesized, AddressOf SpeechHypothesized
            RemoveHandler .SpeechRecognitionRejected, AddressOf SpeechRecognitionRejected
            _objRecognizer.Dispose()
            _objRecognizer = Nothing
            _objSpeaker.Dispose()
            _objSpeaker = Nothing
        End With
    End Sub


    Private Shared Function GetGrammars() As Grammar

        Dim objCommands As New Choices

        'static choices ---------------------------------------------------------------------------
        Dim objOnOffStates As New Choices("on", "off")

        Dim objPercentage As New Choices
        For bytPercent As Byte = 0 To 100
            objPercentage.Add(bytPercent.ToString)
        Next

        Dim objSpeedIncrements As New Choices
        For bytNumber As Byte = 1 To 50
            objSpeedIncrements.Add(bytNumber.ToString)
        Next

        Dim blnTurnoutsExist As Boolean = False
        Dim objTurnoutNames As New Choices
        For Each objTrack As Track In CtcService.Tracks
            If objTrack.IsTurnout Then
                objTurnoutNames.Add(objTrack.Name)
                blnTurnoutsExist = True
            End If
        Next

        Dim objTurnoutStates As New Choices
        For Each strStateName As String In [Enum].GetNames(GetType(Track.TrackState))
            If strStateName <> "NoState" Then objTurnoutStates.Add(strStateName)
        Next

        Dim objRouteNames As New Choices
        For Each objRoute As Route In CtcService.Routes
            objRouteNames.Add(objRoute.Name)
        Next

        Dim objSignalNames As New Choices
        For Each objSignal As Signal In CtcService.Signals
            objSignalNames.Add(objSignal.Name)
        Next

        Dim objAspects As New Choices
        For Each strAspectName As String In [Enum].GetNames(GetType(Signal.SignalAspect))
            objAspects.Add(strAspectName)
        Next

        Dim objAccessoryNames As New Choices
        For Each objAccessory As Accessory In CtcService.Accessories
            objAccessoryNames.Add(objAccessory.Name)
        Next

        Dim objEngineNames As New Choices
        For Each objEngine As Engine In CtcService.Engines
            objEngineNames.Add(objEngine.Name)
        Next

        Dim objSequenceNames As New Choices
        For Each objSequence As Sequence In CtcService.Sequences
            objSequenceNames.Add(objSequence.Name)
        Next

        Dim objScriptNames As New Choices
        For Each objEventScript As EventScript In CtcService.EventScripts
            objScriptNames.Add(objEventScript.Name)
        Next

        'basic commands ---------------------------------------------------------------------------

        objCommands.Add(New SemanticResultValue("stop listening", "ListenOff").ToGrammarBuilder)
        objCommands.Add(New SemanticResultValue("start listening", "ListenOn").ToGrammarBuilder)
        objCommands.Add(New SemanticResultValue("hello", "Hello").ToGrammarBuilder)
        objCommands.Add(New SemanticResultValue("speech grammar", "SpeechGrammar").ToGrammarBuilder)
        objCommands.Add(New SemanticResultValue("slot status", "SlotStatus").ToGrammarBuilder)
        objCommands.Add(New SemanticResultValue("toggle network connection", "Connection").ToGrammarBuilder)
        objCommands.Add(New SemanticResultValue("toggle track power", "TrackPower").ToGrammarBuilder)
        objCommands.Add(New SemanticResultValue("broadcast states", "BroadcastStates").ToGrammarBuilder)
        objCommands.Add(New SemanticResultValue("query sensors", "QuerySensors").ToGrammarBuilder)
        objCommands.Add(New SemanticResultValue("all stop", "StopRailroad").ToGrammarBuilder)

        'switchboard control ----------------------------------------------------------------------

        If blnTurnoutsExist Then
            Dim objGbTurnouts As New GrammarBuilder()
            objGbTurnouts.Append("set turnout")
            objGbTurnouts.Append(New SemanticResultKey("TrackName", objTurnoutNames))
            objGbTurnouts.Append(New Choices("to", " "))
            objGbTurnouts.Append(New SemanticResultKey("TrackState", objTurnoutStates))
            objCommands.Add(New SemanticResultValue(objGbTurnouts, "SetTurnoutState").ToGrammarBuilder)
        End If

        If CtcService.Routes.Count > 0 Then
            Dim objGbSetRoutes As New GrammarBuilder()
            objGbSetRoutes.Append("set route")
            objGbSetRoutes.Append(New SemanticResultKey("RouteName", objRouteNames))
            objCommands.Add(New SemanticResultValue(objGbSetRoutes, "SetRoute").ToGrammarBuilder)

            Dim objGbLockRoutes As New GrammarBuilder()
            objGbLockRoutes.Append("lock route")
            objGbLockRoutes.Append(New SemanticResultKey("RouteName", objRouteNames))
            objCommands.Add(New SemanticResultValue(objGbLockRoutes, "LockRoute").ToGrammarBuilder)

            Dim objGbUnlockRoutes As New GrammarBuilder()
            objGbUnlockRoutes.Append("unlock route")
            objGbUnlockRoutes.Append(New SemanticResultKey("RouteName", objRouteNames))
            objCommands.Add(New SemanticResultValue(objGbUnlockRoutes, "UnlockRoute").ToGrammarBuilder)

            'objCommands.Add(New SemanticResultValue("release all routes", "ReleaseAllRoutes").ToGrammarBuilder)
        End If

        If CtcService.Signals.Count > 0 Then
            Dim objGbSignals As New GrammarBuilder()
            objGbSignals.Append("set signal")
            objGbSignals.Append(New SemanticResultKey("SignalName", objSignalNames))
            objGbSignals.Append(New Choices("to", " "))
            objGbSignals.Append(New SemanticResultKey("Aspect", objAspects))
            objCommands.Add(New SemanticResultValue(objGbSignals, "SetSignalAspect").ToGrammarBuilder)
        End If

        If CtcService.Accessories.Count > 0 Then
            'Dim objGbAccessories As New GrammarBuilder()
            'objGbAccessories.Append("set accessory")
            'objGbAccessories.Append(objAccessoryNames)
            'objGbAccessories.Append(New Choices("to", " "))
            ''add states
            'objCommands.Add(objGbAccessories)
        End If

        If CtcService.Engines.Count > 0 Then
            Dim objGbEngines As New GrammarBuilder()
            objGbEngines.Append("control engine")
            objGbEngines.Append(New SemanticResultKey("EngineName", objEngineNames))
            objCommands.Add(New SemanticResultValue(objGbEngines, "SetContextEngine").ToGrammarBuilder)

            objCommands.Add(New SemanticResultValue("get current engine", "QueryContextEngine").ToGrammarBuilder)
            objCommands.Add(New SemanticResultValue("release engine control", "ReleaseContextEngine").ToGrammarBuilder)
        End If

        If CtcService.Sequences.Count > 0 Then
            Dim objGbRecordSequence As New GrammarBuilder()
            objGbRecordSequence.Append("record sequence")
            objGbRecordSequence.Append(New SemanticResultKey("SequenceName", objSequenceNames))
            objCommands.Add(New SemanticResultValue(objGbRecordSequence, "RecordSequence").ToGrammarBuilder)

            Dim objGbPlaySequence As New GrammarBuilder()
            objGbPlaySequence.Append("play sequence")
            objGbPlaySequence.Append(New SemanticResultKey("SequenceName", objSequenceNames))
            objCommands.Add(New SemanticResultValue(objGbPlaySequence, "PlaySequence").ToGrammarBuilder)

            Dim objGbStopSequence As New GrammarBuilder()
            objGbStopSequence.Append("stop sequence")
            objGbStopSequence.Append(New SemanticResultKey("SequenceName", objSequenceNames))
            objCommands.Add(New SemanticResultValue(objGbStopSequence, "StopSequence").ToGrammarBuilder)
        End If

        If CtcService.EventScripts.Count > 0 Then
            Dim objGbScripts As New GrammarBuilder()
            objGbScripts.Append("run script")
            objGbScripts.Append(New SemanticResultKey("ScriptName", objScriptNames))
            objCommands.Add(New SemanticResultValue(objGbScripts, "ExecuteScript").ToGrammarBuilder)
        End If

        'context engine control -------------------------------------------------------------------

        objCommands.Add(New SemanticResultValue("e stop", "EngineEStop").ToGrammarBuilder)

        Dim objGbSetSpeed As New GrammarBuilder()
        objGbSetSpeed.Append("set speed")
        objGbSetSpeed.Append(New Choices("to", " "))
        objGbSetSpeed.Append(New SemanticResultKey("Percentage", objPercentage))
        objGbSetSpeed.Append(New Choices("percent", " "))
        objCommands.Add(New SemanticResultValue(objGbSetSpeed, "EngineSetSpeed").ToGrammarBuilder)

        Dim objGbRampSpeed As New GrammarBuilder()
        objGbRampSpeed.Append("ramp speed")
        objGbRampSpeed.Append(New SemanticResultKey("Rate", New Choices("fast", " ", "slow")))
        objGbRampSpeed.Append(New Choices("to", " "))
        objGbRampSpeed.Append(New SemanticResultKey("Percentage", objPercentage))
        objGbRampSpeed.Append(New Choices("percent", " "))
        'objGbRampSpeed.Append(New SemanticResultKey("Rate", New Choices("fast", " ", "slow")))   'this did not work at this location
        objCommands.Add(New SemanticResultValue(objGbRampSpeed, "EngineRampSpeed").ToGrammarBuilder)

        objCommands.Add(New SemanticResultValue("hold speed", "EngineRampHalt").ToGrammarBuilder)

        Dim objGbIncSpeed As New GrammarBuilder()
        objGbIncSpeed.Append("increase speed")
        objGbIncSpeed.Append(New Choices("by", " "))
        objGbIncSpeed.Append(New SemanticResultKey("Steps", objSpeedIncrements))
        objGbIncSpeed.Append(New Choices("steps", " "))
        objCommands.Add(New SemanticResultValue(objGbIncSpeed, "EngineIncSpeed").ToGrammarBuilder)

        Dim objGbDecSpeed As New GrammarBuilder()
        objGbDecSpeed.Append("decrease speed")
        objGbDecSpeed.Append(New Choices("by", " "))
        objGbDecSpeed.Append(New SemanticResultKey("Steps", objSpeedIncrements))
        objGbDecSpeed.Append(New Choices("steps", " "))
        objCommands.Add(New SemanticResultValue(objGbDecSpeed, "EngineDecSpeed").ToGrammarBuilder)

        Dim objGbSetDirection As New GrammarBuilder()
        objGbSetDirection.Append("set direction")
        objGbSetDirection.Append(New SemanticResultKey("Direction", New Choices("forward", "reverse")))
        objCommands.Add(New SemanticResultValue(objGbSetDirection, "EngineSetDirection").ToGrammarBuilder)

        objCommands.Add(New SemanticResultValue("change direction", "EngineChangeDirection").ToGrammarBuilder)

        Dim objGbLights As New GrammarBuilder()
        objGbLights.Append("turn")
        objGbLights.Append(New SemanticResultKey("State", objOnOffStates))
        objGbLights.Append("lights")
        objCommands.Add(New SemanticResultValue(objGbLights, "EngineLights").ToGrammarBuilder)

        Dim objGbFunctions As New GrammarBuilder()
        objGbFunctions.Append("turn")
        objGbFunctions.Append(New SemanticResultKey("State", objOnOffStates))
        objGbFunctions.Append("function")
        objGbFunctions.Append(New SemanticResultKey("Number", New Choices("1", "2", "3", "4", "5", "6", "7", "8")))
        objCommands.Add(New SemanticResultValue(objGbFunctions, "EngineFunctions").ToGrammarBuilder)

        objCommands.Add(New SemanticResultValue("turn off all functions", "EngineAllFuncOff").ToGrammarBuilder)

        '------------------------------------------------------------------------------------------

        'test
        'Dim objGbListen As New GrammarBuilder()
        'objGbListen.Append(".")  'crude workaround for speech recognition bug
        'objGbListen.Append(New SemanticResultKey("Paused", New Choices(_
        '    New GrammarBuilder(New SemanticResultValue("start", False)), _
        '    New GrammarBuilder(New SemanticResultValue("stop", True)))))
        'objGbListen.Append("listening")
        'objCommands.Add(New SemanticResultValue(objGbListen, "Listen").ToGrammarBuilder)

        '------------------------------------------------------------------------------------------

        Return New Grammar(New SemanticResultKey("Command", objCommands).ToGrammarBuilder)

    End Function


    Friend Enum ActivityType As Byte
        Null
        SysMessage
        Hypothesized
        Rejected
        Accepted
        Paused
    End Enum

    Friend Shared Event SpeechActivity(enuType As ActivityType,  strText As String)


    Private Shared Sub SpeechRecognized(sender As Object,  e As SpeechRecognizedEventArgs)

        Dim objCommand As SemanticValue = e.Result.Semantics("Command")

        'always listen to this command
        If _blnPaused Then
            If objCommand.Value = "ListenOn" Then
                _blnPaused = False
                Say("started listening")
            End If
        Else
            Select Case objCommand.Value
                Case "ListenOff"
                    _blnPaused = True
                    RaiseEvent SpeechActivity(ActivityType.Paused, e.Result.Text)
                    Say("stopped listening")

                Case "Hello"
                    Say("waiting for commands")

                Case "SpeechGrammar"
                    Dim objMainWin As Windows.Main = My.Application.MainWindow
                    objMainWin.ToggleDockableContent("DocSpeechGrammar")

                Case "SlotStatus"
                    Dim objMainWin As Windows.Main = My.Application.MainWindow
                    objMainWin.ToggleDockableContent("DocSlotStatus")

                Case "Connection"
                    Dim objMainWin As Windows.Main = My.Application.MainWindow
                    objMainWin.ToggleConnection(Nothing, Nothing)
                    Say("toggling network connection")

                Case "TrackPower"
                    Dim objMainWin As Windows.Main = My.Application.MainWindow
                    objMainWin.ToggleTrackPower(Nothing, Nothing)
                    Say("toggling track power")

                Case "BroadcastStates"
                    CtcService.BroadcastStates()
                    Say("states broadcast complete")

                Case "QuerySensors"
                    CtcService.QuerySensors()
                    Say("sensors query complete")

                Case "StopRailroad"
                    'not implemented yet
                    Say("this command has not been implemented yet")

                Case "SetTurnoutState"
                    Dim objTrack As Track = CtcService.Tracks(objCommand("TrackName").Value)
                    If objTrack.States.Contains([Enum].Parse(GetType(Track.TrackState), objCommand("TrackState").Value)) Then
                        objTrack.SetState([Enum].Parse(GetType(Track.TrackState), objCommand("TrackState").Value))
                        Say(String.Format("setting turnout {0} to {1}", objTrack.Name, objCommand("TrackState").Value))
                    Else
                        Say(String.Format("turnout {0} does not have a state named {1}", objTrack.Name, objCommand("TrackState").Value))
                    End If

                Case "SetRoute"
                    Dim objRoute As Route = CtcService.Routes(objCommand("RouteName").Value)
                    objRoute.Set()
                    Say(String.Format("setting route, {0}", objRoute.Name))

                Case "LockRoute"
                    Dim objRoute As Route = CtcService.Routes(objCommand("RouteName").Value)
                    objRoute.Lock()
                    Say(String.Format("locking route, {0}", objRoute.Name))

                Case "UnlockRoute"
                    Dim objRoute As Route = CtcService.Routes(objCommand("RouteName").Value)
                    objRoute.Unlock()
                    Say(String.Format("unlocking route, {0}", objRoute.Name))

                Case "SetSignalAspect"
                    Dim objSignal As Signal = CtcService.Signals(objCommand("SignalName").Value)
                    If objSignal.Aspects.Contains([Enum].Parse(GetType(Signal.SignalAspect), objCommand("Aspect").Value)) Then
                        objSignal.SetAspect([Enum].Parse(GetType(Signal.SignalAspect), objCommand("Aspect").Value))
                        Say(String.Format("setting signal {0} to {1}", objSignal.Name, objCommand("Aspect").Value))
                    Else
                        Say(String.Format("signal {0} does not have an aspect named {1}", objSignal.Name, objCommand("Aspect").Value))
                    End If

                Case "SetContextEngine"
                    _objContextEngine = CtcService.Engines(objCommand("EngineName").Value)
                    If Not _objContextEngine.IsBound Then _objContextEngine.BindSlot(True)
                    Say(String.Format("engine {0} is under control", _objContextEngine.Name))

                Case "QueryContextEngine"
                    If ContextEngineSelected() Then
                        Say(String.Format("current engine is {0}", _objContextEngine.Name))
                    End If

                Case "ReleaseContextEngine"
                    If ContextEngineSelected() Then
                        Say(String.Format("engine {0} released from control", _objContextEngine.Name))
                        _objContextEngine = Nothing
                    End If

                Case "RecordSequence"
                    Dim objSequence As Sequence = CtcService.Sequences(objCommand("SequenceName").Value)
                    objSequence.Stop()
                    If objSequence.Record() Then
                        Say(String.Format("recording sequence {0}", objSequence.Name))
                        My.Application.SetLayoutDirty()
                    Else
                        Say(String.Format("sequence {0} is not idle", objSequence.Name))
                    End If

                Case "PlaySequence"
                    Dim objSequence As Sequence = CtcService.Sequences(objCommand("SequenceName").Value)
                    objSequence.Stop()
                    If objSequence.Play() Then
                        Say(String.Format("playing sequence {0}", objSequence.Name))
                    Else
                        Say(String.Format("sequence {0} is not idle", objSequence.Name))
                    End If

                Case "StopSequence"
                    Dim objSequence As Sequence = CtcService.Sequences(objCommand("SequenceName").Value)
                    objSequence.Stop()
                    Say(String.Format("sequence {0} stopped", objSequence.Name))
                

                Case "ExecuteScript"
                    Dim objEventScript As EventScript = CtcService.EventScripts(objCommand("ScriptName").Value)
                    objEventScript.Execute(New ScriptEventArgs())
                    Say(String.Format("running event script {0}", objEventScript.Name))

                Case "EngineEStop"
                    If ContextEngineSelected() Then
                        _objContextEngine.EmergencyStop()
                        Say("stopping engine")
                    End If

                Case "EngineSetSpeed"
                    If ContextEngineSelected() Then
                        _objContextEngine.Speed = CType(objCommand("Percentage").Value / 100 * _objContextEngine.SpeedStepMax, Byte)
                        Say(String.Format("setting speed to {0} percent", objCommand("Percentage").Value))
                    End If

                Case "EngineRampSpeed"
                    If ContextEngineSelected() Then
                        Dim srtInterval As UShort
                        Select Case objCommand("Rate").Value
                            Case "fast"
                                srtInterval = 100
                            Case ""
                                srtInterval = 200
                            Case "slow"
                                srtInterval = 300
                        End Select
                        _objContextEngine.StartSpeedRamp(CType(objCommand("Percentage").Value / 100 * _objContextEngine.SpeedStepMax, Byte), srtInterval)
                        Say(String.Format("ramping speed {0} to {1} percent", objCommand("Rate").Value, objCommand("Percentage").Value))
                    End If

                Case "EngineRampHalt"
                    If ContextEngineSelected() Then
                        _objContextEngine.StopSpeedRamp()
                        Say("holding speed")
                    End If

                Case "EngineIncSpeed"
                    If ContextEngineSelected() Then
                        _objContextEngine.Speed = Math.Min(_objContextEngine.Speed + objCommand("Steps").Value, 255)
                        Say(String.Format("increasing speed by {0} step{1}", objCommand("Steps").Value, If(objCommand("Steps").Value = 1, "", "s")))
                    End If

                Case "EngineDecSpeed"
                    If ContextEngineSelected() Then
                        _objContextEngine.Speed = Math.Max(_objContextEngine.Speed - objCommand("Steps").Value, 0)
                        Say(String.Format("decreasing speed by {0} step{1}", objCommand("Steps").Value, If(objCommand("Steps").Value = 1, "", "s")))
                    End If

                Case "EngineSetDirection"
                    If ContextEngineSelected() Then
                        Select Case objCommand("Direction").Value
                            Case "forward"
                                _objContextEngine.Direction = LocoDirection.Forward
                                Say("setting direction forward")

                            Case "reverse"
                                _objContextEngine.Direction = LocoDirection.Reverse
                                Say("setting direction reverse")

                        End Select
                    End If

                Case "EngineChangeDirection"
                    If ContextEngineSelected() Then
                        Select Case _objContextEngine.Direction
                            Case LocoDirection.Reverse
                                _objContextEngine.Direction = LocoDirection.Forward
                                Say("setting direction forward")

                            Case LocoDirection.Forward
                                _objContextEngine.Direction = LocoDirection.Reverse
                                Say("setting direction reverse")

                        End Select
                    End If

                Case "EngineLights"
                    If ContextEngineSelected() Then
                        _objContextEngine.Functions(0) = If(objCommand("State").Value = "on", OnOff.On, OnOff.Off)
                        Say(String.Format("turning {0} lights", objCommand("State").Value))
                    End If

                Case "EngineFunctions"
                    If ContextEngineSelected() Then
                        _objContextEngine.Functions(objCommand("Number").Value) = If(objCommand("State").Value = "on", OnOff.On, OnOff.Off)
                        Say(String.Format("turning {0} function {1}", objCommand("State").Value, objCommand("Number").Value))
                    End If

                Case "EngineAllFuncOff"
                    If ContextEngineSelected() Then
                        For bytIdx As Byte = 0 To 8
                            _objContextEngine.Functions(bytIdx) = OnOff.Off
                        Next
                        Say("turning off all functions")
                    End If

            End Select

        End If

        If Not _blnPaused Then
            RaiseEvent SpeechActivity(ActivityType.Accepted, e.Result.Text)
        End If
    End Sub

    Private Shared Sub SpeechHypothesized(sender As Object,  e As SpeechHypothesizedEventArgs)
        If Not _blnPaused Then
            RaiseEvent SpeechActivity(ActivityType.Hypothesized, e.Result.Text)
        End If
    End Sub

    Private Shared Sub SpeechRecognitionRejected(sender As Object,  e As SpeechRecognitionRejectedEventArgs)
        If Not _blnPaused Then
            RaiseEvent SpeechActivity(ActivityType.Rejected, e.Result.Text)
            Say("unknown command")
        End If
    End Sub


    Private Shared Sub Say(strText As String)
        If My.Settings.VoiceFeedback Then _objSpeaker.SpeakAsync(strText)
    End Sub

    Private Shared Function ContextEngineSelected() As Boolean
        If _objContextEngine Is Nothing Then
            Say("no engine is under control")
            Return False
        Else
            Return True
        End If
    End Function

End Class

