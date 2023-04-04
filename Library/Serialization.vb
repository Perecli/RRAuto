Imports System.Runtime.Serialization
Imports System.Security.Permissions

Namespace CTC

    ''' <summary>This is used in place of IDeserializationCallback.</summary>
    ''' <remarks>IDeserializationCallback does not guarantee the desired object graph call order.</remarks>
    Friend Interface IDeserializationComplete
        ''' <summary>Used to convert depricated class members to new ones after deserialization is complete.</summary>
        Sub PostDeserialize()
        ''' <summary>Used to initialize states of deserialized objects after <see cref="PostDeserialize" /> is complete.</summary>
        Sub Initialize()
    End Interface

    ''' <summary>Root object used to serialize a layout definition.</summary>
    <Serializable()> Friend NotInheritable Class RootData
        Implements ISerializable, IDeserializationComplete

        Friend Tracks As TracksList
        Friend Blocks As BlocksList
        Friend Routes As RoutesList
        Friend Sensors As SensorsList
        Friend Signals As SignalsList
        Friend Accessories As AccessoriesList
        Friend Labels As LabelsList
        Friend Buttons As ButtonsList
        Friend Engines As EnginesList
        Friend Sequences As SequencesList
        Friend EventScripts As EventScriptsList
        Friend StepScripts As StepScriptsList
        Friend GlobalScripts As GlobalScriptsList
        Friend PersistedTag As Byte()

        Public Sub New()
            Me.Tracks = New TracksList()
            Me.Blocks = New BlocksList()
            Me.Routes = New RoutesList()
            Me.Sensors = New SensorsList()
            Me.Signals = New SignalsList()
            Me.Accessories = New AccessoriesList()
            Me.Labels = New LabelsList()
            Me.Buttons = New ButtonsList()
            Me.Engines = New EnginesList()
            Me.Sequences = New SequencesList()
            Me.EventScripts = New EventScriptsList()
            Me.StepScripts = New StepScriptsList()
            Me.GlobalScripts = New GlobalScriptsList()
        End Sub

        Private Sub New(info As SerializationInfo, context As StreamingContext)

            Me.Tracks = info.GetValue("Tracks", GetType(Object))
            Me.Blocks = info.GetValue("Blocks", GetType(Object))
            Me.Routes = info.GetValue("Routes", GetType(Object))
            Me.Sensors = info.GetValue("Sensors", GetType(Object))
            Me.Signals = info.GetValue("Signals", GetType(Object))
            Me.Accessories = info.GetValue("Accessories", GetType(Object))

            Try
                Me.Labels = info.GetValue("Labels", GetType(Object))
            Catch
                Me.Labels = New LabelsList()

                'compatibility added 05/27/12
                CtcService.CompatibilityMode = True
            End Try

            Try
                Me.Buttons = info.GetValue("Buttons", GetType(Object))
            Catch
                Me.Buttons = New ButtonsList()

                'compatibility added 09/08/12
                CtcService.CompatibilityMode = True
            End Try

            Me.Engines = info.GetValue("Engines", GetType(Object))
            Me.Sequences = info.GetValue("Sequences", GetType(Object))

            Try
                Me.EventScripts = info.GetValue("EventScripts", GetType(Object))
            Catch
                Me.EventScripts = info.GetValue("Scripts", GetType(Object))

                'compatibility added 07/14/13
                CtcService.CompatibilityMode = True
            End Try

            Try
                Me.StepScripts = info.GetValue("StepScripts", GetType(Object))
            Catch
                Me.StepScripts = New StepScriptsList()

                'compatibility added 07/11/13
                CtcService.CompatibilityMode = True
            End Try

            Try
                Me.GlobalScripts = info.GetValue("GlobalScripts", GetType(Object))
            Catch
                Me.GlobalScripts = New GlobalScriptsList()
                Dim strOldGlobalScript As String = info.GetString("GlobalScript")
                If strOldGlobalScript.Trim <> Nothing Then
                    Dim objGlobalScript As GlobalScript = Me.GlobalScripts.Add()
                    objGlobalScript.Script = strOldGlobalScript
                End If

                'compatibility added 07/26/13
                CtcService.CompatibilityMode = True
            End Try

            Me.PersistedTag = info.GetValue("PersistedTag", GetType(Object))

        End Sub

        Friend Sub PostDeserialize() Implements IDeserializationComplete.PostDeserialize
            Me.Tracks.PostDeserialize()
            Me.Blocks.PostDeserialize()
            Me.Routes.PostDeserialize()
            Me.Sensors.PostDeserialize()
            Me.Signals.PostDeserialize()
            Me.Accessories.PostDeserialize()
            Me.Labels.PostDeserialize()
            Me.Buttons.PostDeserialize()
            Me.Engines.PostDeserialize()
            Me.Sequences.PostDeserialize()
            Me.EventScripts.PostDeserialize()
            Me.StepScripts.PostDeserialize()
            Me.GlobalScripts.PostDeserialize()
        End Sub

        Friend Sub Initialize() Implements IDeserializationComplete.Initialize
            Me.Tracks.Initialize()
            Me.Blocks.Initialize()
            Me.Routes.Initialize()
            Me.Sensors.Initialize()
            Me.Signals.Initialize()
            Me.Accessories.Initialize()
            Me.Labels.Initialize()
            Me.Buttons.Initialize()
            Me.Engines.Initialize()
            Me.Sequences.Initialize()
            Me.EventScripts.Initialize()
            Me.StepScripts.Initialize()
            Me.GlobalScripts.Initialize()
        End Sub

        <SecurityPermission(SecurityAction.LinkDemand, Flags:=SecurityPermissionFlag.SerializationFormatter)> _
        Private Sub GetObjectData(info As SerializationInfo, context As StreamingContext) Implements ISerializable.GetObjectData
            info.AddValue("Tracks", Me.Tracks)
            info.AddValue("Blocks", Me.Blocks)
            info.AddValue("Routes", Me.Routes)
            info.AddValue("Sensors", Me.Sensors)
            info.AddValue("Signals", Me.Signals)
            info.AddValue("Accessories", Me.Accessories)
            info.AddValue("Labels", Me.Labels)
            info.AddValue("Buttons", Me.Buttons)
            info.AddValue("Engines", Me.Engines)
            info.AddValue("Sequences", Me.Sequences)
            info.AddValue("EventScripts", Me.EventScripts)
            info.AddValue("StepScripts", Me.StepScripts)
            info.AddValue("GlobalScripts", Me.GlobalScripts)
            info.AddValue("PersistedTag", Me.PersistedTag)
        End Sub

    End Class

    ''' <summary>Layout content type being saved and/or loaded.</summary>
    Public Enum SerializationContent
        ''' <summary>Layout content consisting of topology and configuration.</summary>
        Definition
        ''' <summary>Layout content consisting of operational states.</summary>
        State
    End Enum

    Friend NotInheritable Class VersionDeserializationBinder
        Inherits SerializationBinder

        Public Overrides Function BindToType(assemblyName As String, typeName As String) As Type

            'for backwards compatibility
            Select Case True
                Case typeName.StartsWith("System.Collections.Generic.List") AndAlso typeName.Contains("RRAutoLib.CTC.StatesList+State")
                    'class was renamed 1/21/11
                    CtcService.CompatibilityMode = True
                    Return GetType(List(Of PkStatesList.State))

                Case typeName = "RRAutoLib.CTC.StatesList+State"
                    'class was renamed 1/21/11
                    CtcService.CompatibilityMode = True
                    Return GetType(PkStatesList.State)

                Case typeName = "RRAutoLib.CTC.ScriptsList"
                    'class was renamed 7/14/13
                    CtcService.CompatibilityMode = True
                    Return GetType(EventScriptsList)

                Case typeName.Contains("RRAutoLib.CTC.Script")
                    'class was renamed 8/11/13
                    'this is the best implementation of renaming a class
                    CtcService.CompatibilityMode = True
                    Return Type.GetType(typeName.Replace("RRAutoLib.CTC.Script", "RRAutoLib.CTC.EventScript"))

                Case typeName.Contains("RRAuto.")  'if type was declared in the Railroad Automation application
                    'all classes defined in RRAuto have been depricated 8/9/10
                    'it was a bad idea to allow classes defined outside this assembly to be serialized/deserialized in this assembly; makes it impossible to maintaian compatibility  
                    CtcService.CompatibilityMode = True
                    Return GetType(DepricatedSerializedType)

                Case typeName.Contains("RRAutoLib.") AndAlso Type.GetType(typeName) Is Nothing  'if type was declared in this library but no longer exists
                    '"typeName.Contains()" is used because the namespace could appear in the middle of the string for generics                    
                    Throw New ApplicationException(String.Format("Type name '{0}' has been depricated and no substitute type was provided for deserialization.", typeName))

                Case Else
                    'return the new version type of the serialized type with the same name
                    Return Type.GetType(typeName)

            End Select
        End Function
    End Class

    ''' <summary>Dummy type for deserializing obsolete objects.</summary>
    <Serializable()> Friend NotInheritable Class DepricatedSerializedType
        Implements ISerializable  'must be implemented for custom deserialization to occur

        Protected Sub New(info As SerializationInfo,  context As StreamingContext)
            'don't deserialize anything
        End Sub

        <SecurityPermission(SecurityAction.LinkDemand, Flags:=SecurityPermissionFlag.SerializationFormatter)> _
        Protected Sub GetObjectData(info As SerializationInfo,  context As StreamingContext) Implements ISerializable.GetObjectData
            'nothing is ever serialized from this class
        End Sub

    End Class

End Namespace
