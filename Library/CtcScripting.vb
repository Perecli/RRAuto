'Imports System.CodeDom.Compiler
Imports System.Reflection
Imports NAudio.Wave
Imports RRAutoLib.CTC

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.VisualBasic

Namespace Scripting

    ''' <summary>Provides user script compilation support.</summary>
    ''' <remarks>Members of this class are used for compiling the user script into an assembly and binding it to work in concert with the CTC.</remarks>
    Public Module CompilerService

        Private _objScriptAssembly As Assembly

        ''' <summary>Represents a script compilation error.</summary>
        Public Class ScriptError

            ''' <summary>Type of code where the compilation error occurred.</summary>
            ''' <returns>'User Script' for code entered by user. 'Auto Generated' for code that is used to glue the user scripts together into an assembly.</returns>
            Public Property CodeType As String

            ''' <summary>Location of object where the compilation error occurred.</summary>
            ''' <returns>A script name or 'Global Script'.</returns>
            Public Property Location As String

            ''' <summary>Line number within the object specified by <see cref="Location" /> where the compilation error occurred.</summary>                 
            Public Property Line As Integer

            ''' <summary>Column number within the line number specified by <see cref="Line" /> where the compilation error occurred.</summary> 
            ''' <remarks>Currently this value is not reliable due to what seems to be a bug in the .NET framework.</remarks>
            Public Property Column As Integer

            ''' <summary>Compilation error description.</summary> 
            Public Property ErrorText As String

        End Class

        ''' <summary>Gets the VB.NET code auto generated from the user scrips.</summary>
        Public Function GenerateCode() As String

            Dim strCode As String
            Dim strUserScript As String

            strCode &= String.Format(
                "'•Assembly{0}" &
                "Option Strict Off{0}" &
                "Option Explicit On{0}" &
                "Option Infer On{0}" &
                "{0}" &
                "Imports Microsoft.VisualBasic{0}" &
                "Imports System{0}" &
                "Imports System.Collections{0}" &
                "Imports System.Collections.Generic{0}" &
                "Imports System.Threading{0}" &
                "Imports System.Threading.Tasks{0}" &
                "Imports System.Linq{0}" &
                "Imports RRAutoLib.Loconet{0}" &
                "Imports RRAutoLib.CTC{0}" &
                "Imports RRAutoLib.Scripting{0}" &
                "{0}" &
                "Namespace CompiledUserScript{0}",
                vbCrLf)

            '-- Event Scripts --------------------------------------------------

            strCode &= String.Format(
                "{0}" &
                "    '•EventScriptsModule{0}" &
                "    Public Module EventScripts{0}",
                vbCrLf)

            For Each objEventScript As EventScript In CtcService.EventScripts.Where(Function(s) s.Enabled)

                strCode &= String.Format(
                    "{0}" &
                    "        '•EventScriptMethod-{1}{0}" &
                    "        Public Async Function _{1}(e As ScriptEventArgs) As Task{0}" &
                    "            '•EventScriptUserCode-{1}{0}",
                    vbCrLf, objEventScript.ID.ToString("N"))

                strUserScript = objEventScript.Script
                If strUserScript <> Nothing Then
                    For Each strLine As String In strUserScript.Split(vbLf)
                        strCode &= String.Format("            {1}{0}", vbCrLf, strLine)
                    Next
                End If

                strCode &= String.Format(
                    "            '•EventScriptUserCode-{1}{0}" &
                    "        End Function{0}" &
                    "        '•EventScriptMethod-{1}{0}",
                    vbCrLf, objEventScript.ID.ToString("N"))

            Next

            strCode &= String.Format(
                "{0}" &
                "    End Module{0}" &
                "    '•EventScriptsModule{0}",
                vbCrLf)

            '-- Step Scripts ---------------------------------------------------

            strCode &= String.Format(
                "{0}" &
                "    '•StepScriptsModule{0}" &
                "    Public Module StepScripts{0}",
                vbCrLf)

            For Each objStepScript As StepScript In CtcService.StepScripts.Where(Function(s) s.Enabled)

                strCode &= String.Format(
                    "{0}" &
                    "        '•StepScriptMethod-{1}{0}" &
                    "        Public Sub _{1}(o as StepScript){0}" &
                    "            '•StepScriptUserCode-{1}{0}",
                    vbCrLf, objStepScript.ID.ToString("N"))

                strUserScript = objStepScript.Script
                If strUserScript <> Nothing Then
                    For Each strLine As String In strUserScript.Split(vbLf)
                        strCode &= String.Format("                {1}{0}", vbCrLf, strLine)
                    Next
                End If

                strCode &= String.Format(
                    "            '•StepScriptUserCode-{1}{0}" &
                    "        End Sub{0}" &
                    "        '•StepScriptMethod-{1}{0}",
                    vbCrLf, objStepScript.ID.ToString("N"))

            Next

            strCode &= String.Format(
                "{0}" &
                "    End Module{0}" &
                "    '•StepScriptsModule{0}",
                vbCrLf)

            '-- Global Scripts -------------------------------------------------

            strCode &= String.Format(
                "{0}" &
                "    '•GlobalScriptsModule{0}" &
                "    Public Module GlobalScripts{0}",
                vbCrLf)

            For Each objGlobalScript As GlobalScript In CtcService.GlobalScripts.Where(Function(s) s.Enabled)

                strCode &= String.Format(
                    "{0}" &
                    "        '•GlobalScriptUserCode-{1}{0}",
                    vbCrLf, objGlobalScript.ID.ToString("N"))

                strUserScript = objGlobalScript.Script
                If strUserScript <> Nothing Then
                    For Each strLine As String In strUserScript.Split(vbLf)
                        strCode &= String.Format("        {1}{0}", vbCrLf, strLine)
                    Next
                End If

                strCode &= String.Format(
                    "        '•GlobalScriptUserCode-{1}{0}",
                    vbCrLf, objGlobalScript.ID.ToString("N"))

            Next

            strCode &= String.Format(
                "{0}" &
                "    End Module{0}" &
                "    '•GlobalScriptsModule{0}",
                vbCrLf)

            '-------------------------------------------------------------------

            strCode &= String.Format(
                "{0}" &
                "End Namespace{0}" &
                "'•Assembly",
                vbCrLf)

            Return strCode

        End Function

        'this deprecated version uses the old non-Roslyn compiler 
        'Friend Function CompileScript(blnBindScript As Boolean) As List(Of ScriptError)
        '    Dim objParms As New CompilerParameters

        '    With objParms
        '        .ReferencedAssemblies.Add("System.dll")
        '        .ReferencedAssemblies.Add("System.Core.dll")   'needed for LINQ
        '        .ReferencedAssemblies.Add("RRAutoLib.dll")
        '        .GenerateExecutable = False
        '        .GenerateInMemory = True
        '        .IncludeDebugInformation = False
        '    End With

        '    Dim strCode As String = GenerateCode()

        '    'compile script and create List(Of ScriptError)
        '    Dim objCodeProvider As CodeDomProvider = New VBCodeProvider
        '    Dim objResults As CompilerResults = objCodeProvider.CompileAssemblyFromSource(objParms, strCode)
        '    Dim objErrors As New List(Of ScriptError)
        '    If objResults.Errors.HasErrors Then
        '        'convert the collection of CompilerError objects to my List(of ScriptError) objects which also contains the Location member
        '        For Each objCompilerError As CompilerError In objResults.Errors
        '            If Not objCompilerError.IsWarning Then  'don't show warnings
        '                objErrors.Add(New ScriptError With {
        '                    .Line = objCompilerError.Line,
        '                    .Column = objCompilerError.Column,
        '                    .ErrorText = objCompilerError.ErrorText
        '                })
        '            End If
        '        Next

        '        'populate objErrors with RRA context
        '        Dim straScriptLines() As String = strCode.Split(vbCrLf)
        '        Dim intCurrLinePos As Integer = 0           'current line number that is being parsed
        '        Dim objMarkers As New Stack()
        '        objMarkers.Push(New With {.Context = "Root"})  'dummy item to prevent exceptions when objMarkers.Peek() is called the first time
        '        While intCurrLinePos < straScriptLines.Length
        '            intCurrLinePos += 1
        '            Select Case True
        '                Case ParseMarker(straScriptLines, intCurrLinePos, "Assembly", objMarkers, objErrors)

        '                Case ParseMarker(straScriptLines, intCurrLinePos, "EventScriptsModule", objMarkers, objErrors)
        '                Case ParseMarker(straScriptLines, intCurrLinePos, "EventScriptMethod", objMarkers, objErrors)
        '                Case ParseMarker(straScriptLines, intCurrLinePos, "EventScriptUserCode", objMarkers, objErrors)

        '                Case ParseMarker(straScriptLines, intCurrLinePos, "StepScriptsModule", objMarkers, objErrors)
        '                Case ParseMarker(straScriptLines, intCurrLinePos, "StepScriptMethod", objMarkers, objErrors)
        '                Case ParseMarker(straScriptLines, intCurrLinePos, "StepScriptUserCode", objMarkers, objErrors)

        '                Case ParseMarker(straScriptLines, intCurrLinePos, "GlobalScriptsModule", objMarkers, objErrors)
        '                Case ParseMarker(straScriptLines, intCurrLinePos, "GlobalScriptMethod", objMarkers, objErrors)
        '                Case ParseMarker(straScriptLines, intCurrLinePos, "GlobalScriptUserCode", objMarkers, objErrors)
        '            End Select
        '        End While
        '    End If

        '    'bind dynamically compiled script instances to their complementary RRAuto script objects 
        '    If blnBindScript AndAlso objErrors.Count = 0 Then

        '        'bind EventScripts
        '        For Each objEventScript As EventScript In CtcService.EventScripts.Where(Function(s) s.Enabled)
        '            Dim objMethod As MethodInfo = objResults.CompiledAssembly.GetType("CompiledUserScript.EventScripts").GetMethod("_" & objEventScript.ID.ToString("N"))
        '            objEventScript.InvocTargetHandler = [Delegate].CreateDelegate(GetType(Func(Of ScriptEventArgs, Task)), objMethod)
        '            For Each objEvent As EventScript.Event In objEventScript.EventBindings
        '                objEvent.Owner.ScriptEventBinder.BindInvocTarget(objEvent.Name, objEventScript)
        '            Next

        '            'left here for reference in case we need to create an instance of a class from user generated code
        '            'Dim objInstance As MyInterfaceImplementedByUserClass = objResults.CompiledAssembly.CreateInstance("CompiledUserScript._" & objEventScript.ID.ToString("N"))
        '        Next

        '        'execute user provided step initializers for StepScrips
        '        For Each objStepScript As StepScript In CtcService.StepScripts.Where(Function(s) s.Enabled)
        '            Dim objMethod As MethodInfo = objResults.CompiledAssembly.GetType("CompiledUserScript.StepScripts").GetMethod("_" & objStepScript.ID.ToString("N"))
        '            Try
        '                objMethod.Invoke(Nothing, New Object() {objStepScript})
        '            Catch ex As Exception
        '                CtcService.PostMessage(CtcService.MessageCat.Error,
        '                    String.Format("Runtime error occurred initializing [{0}]: {1}", objStepScript, If(ex.InnerException Is Nothing, ex.Message, ex.InnerException.Message)))
        '            End Try
        '        Next

        '        _objScriptAssembly = objResults.CompiledAssembly
        '    End If

        '    Return objErrors

        'End Function

        Friend Function CompileScript(blnBindScript As Boolean) As List(Of ScriptError)

            Dim strCode = GenerateCode()

            Dim objCodeTree = SyntaxFactory.ParseSyntaxTree(strCode)
            Dim objCompilation = VisualBasicCompilation.Create("UserScript", {objCodeTree}, {
                    MetadataReference.CreateFromFile(GetType(Object).Assembly.Location),            'adds mscorelib.dll reference; any type can be given to these that resolve to the desired assembly
                    MetadataReference.CreateFromFile(GetType(Stack(Of Byte)).Assembly.Location),    'adds System.dll reference
                    MetadataReference.CreateFromFile(GetType(Constants).Assembly.Location),         'adds Microsoft.VisualBasic.dll reference; needed for method attributes specific to VB
                    MetadataReference.CreateFromFile(GetType(Enumerable).Assembly.Location),        'adds System.Core.dll reference; 'needed for LINQ
                    MetadataReference.CreateFromFile(GetType(CtcService).Assembly.Location)         'adds RRAutoLib.dll reference
                 },
                 New VisualBasicCompilationOptions(OutputKind.DynamicallyLinkedLibrary)
            )

            Dim objDiagnostics As IEnumerable(Of Diagnostic)
            If blnBindScript Then
                'attempt to build new assembly
                'CompileScript(True) is only called when script is unbound and thus _objScriptAssembly should always be Nothing here
                '_objScriptAssembly is later used to check if assembly was successfully built here

                Using objStream As New IO.MemoryStream()
                    Dim objCompResult = objCompilation.Emit(objStream)
                    If objCompResult.Success Then
                        _objScriptAssembly = Assembly.Load(objStream.GetBuffer())
                    End If
                    objDiagnostics = objCompResult.Diagnostics
                End Using
            Else
                'don't build assembly (much faster); just check for errors
                objDiagnostics = objCompilation.GetDiagnostics()
            End If

            'keep only errors; get rid of warnings
            objDiagnostics = objDiagnostics.Where(Function(d) d.WarningLevel = 0)

            Dim objErrors As New List(Of ScriptError)
            If objDiagnostics.Count = 0 Then  'if no errors encountered
                If blnBindScript AndAlso _objScriptAssembly IsNot Nothing Then  'both being true indicates we successfully built a new assembly

                    'bind EventScripts
                    For Each objEventScript In CtcService.EventScripts.Where(Function(s) s.Enabled)
                        Dim objMethod As MethodInfo = _objScriptAssembly.GetType("CompiledUserScript.EventScripts").GetMethod("_" & objEventScript.ID.ToString("N"))
                        objEventScript.InvocTargetHandler = [Delegate].CreateDelegate(GetType(Func(Of ScriptEventArgs, Task)), objMethod)
                        For Each objEvent As EventScript.Event In objEventScript.EventBindings
                            objEvent.Owner.ScriptEventBinder.BindInvocTarget(objEvent.Name, objEventScript)
                        Next

                        'left here for reference in case we need to create an instance of a class from user generated code
                        'Dim objInstance As MyInterfaceImplementedByUserClass = _objScriptAssembly.CreateInstance("CompiledUserScript._" & objEventScript.ID.ToString("N"))
                    Next

                    'execute user provided step initializers for StepScrips
                    For Each objStepScript In CtcService.StepScripts.Where(Function(s) s.Enabled)
                        Dim objMethod As MethodInfo = _objScriptAssembly.GetType("CompiledUserScript.StepScripts").GetMethod("_" & objStepScript.ID.ToString("N"))
                        Try
                            objMethod.Invoke(Nothing, New Object() {objStepScript})
                        Catch ex As Exception
                            CtcService.PostMessage(CtcService.MessageCat.Error,
                                String.Format("Runtime error occurred initializing [{0}]: {1}", objStepScript, If(ex.InnerException Is Nothing, ex.Message, ex.InnerException.Message)))
                        End Try
                    Next

                End If
            Else
                'initially populate objErrors with data from error data from Diagnostics
                For Each objDiagnostic In objDiagnostics
                    Dim objLineSpan = objDiagnostic.Location.GetLineSpan
                    objErrors.Add(New ScriptError With {
                        .Line = objLineSpan.StartLinePosition.Line + 1,
                        .Column = objLineSpan.StartLinePosition.Character + 1,
                        .ErrorText = objDiagnostic.GetMessage
                    })
                Next

                'further populate objErrors with script specific contextual data
                Dim straScriptLines() As String = strCode.Split(vbCrLf)
                Dim intCurrLinePos As Integer = 0           'current line number that is being parsed
                Dim objMarkers As New Stack()
                objMarkers.Push(New With {.Context = "Root"})  'dummy item to prevent exceptions when objMarkers.Peek() is called the first time
                While intCurrLinePos < straScriptLines.Length
                    intCurrLinePos += 1
                    Select Case True
                        Case ParseMarker(straScriptLines, intCurrLinePos, "Assembly", objMarkers, objErrors)

                        Case ParseMarker(straScriptLines, intCurrLinePos, "EventScriptsModule", objMarkers, objErrors)
                        Case ParseMarker(straScriptLines, intCurrLinePos, "EventScriptMethod", objMarkers, objErrors)
                        Case ParseMarker(straScriptLines, intCurrLinePos, "EventScriptUserCode", objMarkers, objErrors)

                        Case ParseMarker(straScriptLines, intCurrLinePos, "StepScriptsModule", objMarkers, objErrors)
                        Case ParseMarker(straScriptLines, intCurrLinePos, "StepScriptMethod", objMarkers, objErrors)
                        Case ParseMarker(straScriptLines, intCurrLinePos, "StepScriptUserCode", objMarkers, objErrors)

                        Case ParseMarker(straScriptLines, intCurrLinePos, "GlobalScriptsModule", objMarkers, objErrors)
                        Case ParseMarker(straScriptLines, intCurrLinePos, "GlobalScriptMethod", objMarkers, objErrors)
                        Case ParseMarker(straScriptLines, intCurrLinePos, "GlobalScriptUserCode", objMarkers, objErrors)
                    End Select
                End While
            End If

            Return objErrors

        End Function

        Private Function ParseMarker(straScriptLines() As String, intCurrLinePos As Integer, strMarkerName As String, objMarkers As Stack, objErrors As List(Of ScriptError)) As Boolean

            If straScriptLines(intCurrLinePos - 1).Trim.StartsWith("'•" & strMarkerName) Then
                Dim objLastMarker As Object = objMarkers.Peek()

                If objLastMarker.Context = strMarkerName Then
                    objMarkers.Pop()
                Else
                    Dim straMarkerArgs As String() = straScriptLines(intCurrLinePos - 1).Split("-")
                    Dim strLocation As String
                    Dim strScriptName As String
                    If straMarkerArgs.Count = 2 Then  'if a GUID is present
                        Dim sctGUID As New Guid(straMarkerArgs(1))
                        If strMarkerName.EndsWith("UserCode") Then
                            strLocation = strMarkerName.Replace("UserCode", "")  'remove user code sufix because the CodeType is already marked as "User Script"
                        Else
                            strLocation = strMarkerName
                        End If
                        Select Case True
                            Case strMarkerName.StartsWith("EventScript")
                                strScriptName = CtcService.EventScripts(sctGUID).Name
                            Case strMarkerName.StartsWith("StepScript")
                                strScriptName = CtcService.StepScripts(sctGUID).Name
                            Case strMarkerName.StartsWith("GlobalScript")
                                strScriptName = CtcService.GlobalScripts(sctGUID).Name
                        End Select
                        strLocation = String.Format("{0}: {1}", strLocation, strScriptName)
                    Else
                        strLocation = strMarkerName
                    End If

                    objMarkers.Push(New With {
                        .Context = strMarkerName,
                        .Location = strLocation,
                        .Line = intCurrLinePos
                    })
                End If
                For Each objScriptError As ScriptError In objErrors
                    If objScriptError.Location = Nothing AndAlso objScriptError.Line < intCurrLinePos Then
                        Dim blnIsUserCode As Boolean = objLastMarker.Context.EndsWith("UserCode")
                        objScriptError.CodeType = If(blnIsUserCode, "User Script", "Auto Generated")
                        objScriptError.Location = objLastMarker.Location
                        If blnIsUserCode Then objScriptError.Line -= objLastMarker.Line 'use relative line reporting on user script only; for other scripts debugging should be done though the Generated Assembly view
                    End If
                Next

                Return True
            Else
                Return False
            End If

        End Function

        Friend Function InvokeGlobalMethod(strMethodName As String) As Object

            If _objScriptAssembly IsNot Nothing Then
                Dim objMethod As MethodInfo = _objScriptAssembly.GetType("CompiledUserScript.GlobalScripts").GetMethod(strMethodName)
                If objMethod IsNot Nothing Then
                    Try
                        Return objMethod.Invoke(Nothing, Nothing)
                    Catch ex As Exception
                        CtcService.PostMessage(CtcService.MessageCat.Error,
                            String.Format("Runtime error occurred in {0}(): {1}", strMethodName, If(ex.InnerException Is Nothing, ex.Message, ex.InnerException.Message)))
                    End Try
                End If
            End If

        End Function

        Friend Sub UnbindScript()

            If _objScriptAssembly IsNot Nothing Then

                'remove static script bindings
                For Each objEventScript As EventScript In CtcService.EventScripts.Where(Function(s) s.Enabled)
                    objEventScript.InvocTargetHandler = Nothing
                    For Each objEvent As EventScript.Event In objEventScript.EventBindings
                        objEvent.Owner.ScriptEventBinder.UnbindInvocTarget(objEvent.Name, objEventScript)
                    Next
                Next

                'remove adhoc dynamic bindings
                For Each objAdhocBinding As ScriptEventBinder.AdhocBinding In ScriptEventBinder.AdhocBindings
                    For Each objEvent As EventScript.Event In objAdhocBinding.Events
                        objEvent.Owner.ScriptEventBinder.UnbindInvocTarget(objEvent.Name, objAdhocBinding)
                    Next
                Next
                ScriptEventBinder.AdhocBindings.Clear()

                _objScriptAssembly = Nothing
            End If

        End Sub

        ''' <summary>Indicates if the user scripts have been successfully compiled and bound for operation.</summary>
        ''' <remarks>
        ''' User scripts are compiled during the CtcService start phase and bound for operation. 
        ''' Binding will fail, disabling all scripting for that operation session, if compilation errors are encountered.
        ''' </remarks>
        Public ReadOnly Property IsScriptBound() As Boolean
            Get
                Return _objScriptAssembly IsNot Nothing
            End Get
        End Property

        ''' <summary>Validates the scripts for compilation errors.</summary>
        ''' <returns>Returns a list of compiler errors.</returns>
        Public Function ValidateScript() As List(Of ScriptError)
            Return CompileScript(False)
        End Function


        '-- this was experimental and currently disabled 
        '<summary>Internally called by user generated script to check the call stack depth.</summary>
        '<param name="intDepth">Call count above which the stack is assumed to be too deep.</param>
        '<exclude />
        'Public Function StackedTooDeep(intDepth As Integer) As Boolean
        '    Dim objStackTrace As New StackTrace

        '    'get the caller's method
        '    Dim objCurrMethod As MethodBase = objStackTrace.GetFrames(1).GetMethod

        '    Dim objCallCnt As Integer
        '    For Each objFrame As StackFrame In objStackTrace.GetFrames()
        '        If objFrame.GetMethod Is objCurrMethod Then
        '            objCallCnt += 1
        '        End If
        '    Next
        '    Return objCallCnt > intDepth
        'End Function

    End Module

#Region "Script Helpers"

    ''' <summary>Provides helper methods for scripting.</summary>
    ''' <remarks>Its members are designed to be called from the CTC thread within the scripting context.</remarks>
    Public Module Scripting

        ''' <summary>Provides a key indexed variable bag.</summary>
        ''' <remarks>This type simplifies global variable declaration within scripting. Declare one instance of this class and have an infinite number of globals.</remarks>
        Public Class VariableBag
            Private _objList As New Dictionary(Of String, Object)

            ''' <summary>Gets or set a keyed variable value.</summary>
            ''' <param name="strKey">The key name uniquely identifying the variable in the bag.</param>
            Default Public Property Item(strKey As String) As Object
                Get
                    If _objList.ContainsKey(strKey) Then
                        Return _objList(strKey)
                    Else
                        Return Nothing
                    End If
                End Get
                Set(value As Object)
                    If _objList.ContainsKey(strKey) Then
                        If value Is Nothing Then
                            _objList.Remove(strKey)
                        Else
                            _objList(strKey) = value
                        End If
                    Else
                        If value IsNot Nothing Then
                            _objList.Add(strKey, value)
                        End If
                    End If
                End Set
            End Property

        End Class

        ''' <summary>Collection initializer used to encapsulate a list of scripting events.</summary>
        Public Class EventsList
            Implements IEnumerable

            Private _objEvents As New List(Of EventScript.Event)

            Public Sub Add(objOwner As ISupportsScriptEvents, strName As String)
                _objEvents.Add(New EventScript.Event(objOwner, strName))
            End Sub

            Public Function GetEnumerator() As IEnumerator Implements IEnumerable.GetEnumerator
                Return _objEvents.GetEnumerator
            End Function

            Public ReadOnly Property Count As Integer
                Get
                    Return _objEvents.Count
                End Get
            End Property
        End Class


        ''' <summary>Posts a text message to the CTC event stream.</summary>
        ''' <remarks>This method was designed specifically for use within the scripting context.</remarks>
        Public Sub PostMsg(strMessage As String, ParamArray objArgs() As Object)
            CtcService.PostMessage(CtcService.MessageCat.User, String.Format(strMessage, objArgs))
        End Sub

        ''' <summary>Posts the scripting call stack to the CTC event stream.</summary>
        ''' <param name="blnRaw">When <i>True</i> the call stack is shown in full and unaltered. When <i>False</i>, internal methods are filtered out and <see cref="EventScript" /> names resolved.</param>
        ''' <remarks>
        ''' Used for debugging.
        ''' This method was designed specifically for use within the scripting context.
        ''' </remarks>
        Public Sub PostCallStack(Optional blnRaw As Boolean = False)

            'be aware that some stack frames will not be shown when the app run from .exe (optimized) as opposed to runing it from VS because of Jit inline optimizations
            'see: http://blogs.msdn.com/b/jmstall/archive/2005/03/20/399287.aspx
            'found that if any method is optimized out you can put "Try : Catch : End Try" anywhere in the method and inline optimization will be defeated

            Dim objStackTrace As New StackTrace

            Dim intIdx As Integer = 0
            Do While True
                Dim objFrame As StackFrame = objStackTrace.GetFrame(intIdx)
                If objFrame Is Nothing Then Exit Do

                Dim objMethod As MethodBase = objFrame.GetMethod()
                With objMethod.DeclaringType
                    If blnRaw Then
                        PostMsg("{0} : {1} : {2}", .Namespace, .Name, objMethod.Name)
                    Else
                        If .Namespace.StartsWith("RRAutoLib") OrElse .Namespace = "CompiledUserScript" Then  'only show these name spaces
                            Select Case True 'ignore these matches
                                Case .Name.StartsWith("VB$StateMachine")
                                Case .Name.StartsWith("_Closure$")
                                Case .Name = "ScriptEventBinder"
                                Case objMethod.Name.StartsWith("_Lambda$")
                                Case Else
                                    'convert EventScript's method name from GUID to object name
                                    Dim strMethodName As String = objMethod.Name
                                    If .Namespace = "CompiledUserScript" AndAlso .Name = "EventScripts" Then
                                        strMethodName = CtcService.EventScripts(Guid.Parse(strMethodName.Remove(0, 1))).Name
                                    End If

                                    PostMsg("{0} : {1} : {2}", .Namespace, .Name, strMethodName)
                            End Select
                        End If
                    End If
                End With
                intIdx += 1
            Loop

        End Sub

        ''' <summary>Introduces a delay in script execution.</summary>
        ''' <param name="intDuration">Number of milliseconds to wait before continuing with the next line of script.</param>
        ''' <param name="sctCancelToken">Token provided by a controlling source to notify this method that it should cancel its execution.</param>
        ''' <returns>The <see cref="Tasks.Task" /> that must be awaited, for the delay to occur.</returns>
        ''' <remarks>This method was designed specifically for use within the scripting context.</remarks>
        Public Function Delay(intDuration As UInteger, Optional sctCancelToken As CancellationToken = Nothing) As Task
            Return Task.Delay(intDuration, sctCancelToken)
        End Function

        ''' <summary>Adds a timeout expiration to a given awaitable task.</summary>
        ''' <param name="objTask">The task being given a timeout expiration.</param>
        ''' <param name="intTimeout">Time in milliseconds to await the task before giving up.</param>
        ''' <param name="sctCancelToken">Token provided by a controlling source to notify this method that it should cancel its execution.</param>
        ''' <returns>
        ''' A <see cref="Tasks.Task(Of Boolean)" /> that must be awaited, for the timeout.
        ''' Returns <i>True</i> if the given task completed before the timeout expiration, or <i>False</i> if the task timed out.
        ''' </returns>
        ''' <remarks>This method was designed specifically for use within the scripting context.</remarks>
        Public Async Function WithTimeout(objTask As Task, intTimeout As UInteger, Optional sctCancelToken As CancellationToken = Nothing) As Task(Of Boolean)

            'not sure if this functions is even needed; I can't think of a scenario where it would be helpfull;
            'OnEvent() might need a timeout but an internal one would be better because it can unbind;
            'this method has no control over the internals of the given objTask implementation

            Dim objTimeoutTask As Task = Task.Delay(intTimeout, sctCancelToken)
            Dim objFinishedTask As Task = Await Task.WhenAny(objTask, objTimeoutTask)

            Select Case True
                Case objTask.Status = TaskStatus.RanToCompletion
                    Return True

                Case objTimeoutTask.Status = TaskStatus.RanToCompletion
                    Return False

                Case Else
                    'this causes cancel/faulted exceptions to be raised if they exists
                    'Task.WhenAny does not raise exceptions but rather places the exceptions in the returned Task
                    'this call will return instantaneously since the task is alredy complete
                    Await objFinishedTask

            End Select

        End Function

        ''' <summary>Creates an adhoc event binding to act on.</summary>
        ''' <param name="objEvents">One or more events to be observed.</param>
        ''' <param name="sctCancelToken">Token provided by a controlling source to notify this method that it should cancel its execution.</param>
        ''' <returns>The <see cref="Tasks.Task" /> that can be awaited, if execution must halt while the events are bound.</returns>
        ''' <remarks>This method was designed specifically for use within the scripting context and is only safe to be called on the CTC thread.</remarks>
        ''' <example>
        ''' In the following example, execution will halt until either of the two bound buttons are activated (pressed).
        ''' Optionally the <i>e.</i><see cref="ScriptEventArgs.CancelToken" /> passed from the hosting script object, can be handed down to this method so it can participate 
        ''' in a cancellation order received from the parent script.
        '''	<code lang="VB">        
        '''     Await OnEvent(New EventsList() From {
        '''         {CtcButton("Button1"), "Activated"}, 
        '''         {CtcButton("Button2"), "Activated"}},
        '''         e.CancelToken)
        '''	</code>
        ''' See other overload, for a version of this method, that can additionally execute some script, when the events are raised.
        '''	</example>
        Public Function OnEvent(objEvents As EventsList, Optional sctCancelToken As CancellationToken = Nothing) As Task
            Return OnEvent(objEvents, Nothing, sctCancelToken)
        End Function

        ''' <summary>Creates an adhoc event binding to act on.</summary>
        ''' <param name="objEvents">One or more events to be observed.</param>
        ''' <param name="delHandler">A function to be excuted when any one of the bound events are raised.</param>
        ''' <param name="sctCancelToken">Token provided by a controlling source to notify this method that it should cancel its execution.</param>
        ''' <returns>The <see cref="Tasks.Task" /> that can be awaited, if execution must halt while the events are bound.</returns>
        ''' <remarks>This method was designed specifically for use within the scripting context and is only safe to be called on the CTC thread.</remarks>
        ''' <example>
        ''' In the following example, execution will halt until either of the two bound sensors report an <i>On</i> state.
        ''' <i>e2.</i><see cref="ScriptEventArgs.Sender" /> returns the first object that raised the event (in this example, one of the two bound sensors). 
        ''' Setting <i>e2.</i><see cref="ScriptEventArgs.Dispose" />, in the function unbinds the events and completes the task this method returns.
        ''' The function could optionally be left out in which case the task completes immediately when either event occurs.  
        ''' Optionally the <i>e.</i><see cref="ScriptEventArgs.CancelToken" /> passed from the hosting script object, can be handed down to this method so it can participate 
        ''' in a cancellation order received from the parent script. Note that although both <i>e.</i> and <i>e2.</i> are <see cref="ScriptEventArgs" />, they are given 
        ''' different names to disambiguate the upstream event argument from the downstream one. 
        '''	<code lang="VB">        
        '''     Await OnEvent(New EventsList() From {
        '''         {CtcSensor("Sensor1"), "StateReported"}, 
        '''         {CtcSensor("Sensor2"), "StateReported"}},
        '''         Function(e2 as ScriptEventArgs)
        '''             If e2.Sender.State = 1 Then e2.Dispose()
        '''         End Function,
        '''         e.CancelToken)
        '''	</code>
        '''	</example>
        Public Function OnEvent(objEvents As EventsList, delHandler As Func(Of ScriptEventArgs, Task), Optional sctCancelToken As CancellationToken = Nothing) As Task

            If objEvents Is Nothing OrElse objEvents.Count = 0 Then
                CtcService.PostMessage(CtcService.MessageCat.Error, "No events were given to OnEvent(). Delay aborted.")
                Return Task.FromResult(0)
            End If

            Dim objAdhocBinding As New ScriptEventBinder.AdhocBinding("OnEvent()", delHandler)
            For Each objEvent As EventScript.Event In objEvents

                Select Case True
                    Case objEvent.Owner Is Nothing
                        CtcService.PostMessage(CtcService.MessageCat.Error, "Blank event owner given to OnEvent(). Binding skipped.")

                    Case objAdhocBinding.Events.Exists(Function(objListEvent As EventScript.Event) As Boolean
                                                           Return objListEvent.Owner Is objEvent.Owner AndAlso objListEvent.Name = objEvent.Name
                                                       End Function)
                        CtcService.PostMessage(CtcService.MessageCat.Error, "Duplicate event given to OnEvent(). Binding skipped.")

                    Case Not objEvent.Owner.ScriptEventBinder.BindInvocTarget(objEvent.Name, objAdhocBinding)
                        'error generated by BindInvocTarget()

                    Case Else
                        objAdhocBinding.Events.Add(objEvent)  'this event was successfuly bound

                End Select
            Next

            If objAdhocBinding.Events.Count = 0 Then
                CtcService.PostMessage(CtcService.MessageCat.Error, "OnEvent() couldn't bind any events it was given. Delay aborted.")
                Return Task.FromResult(0)
            Else
                ScriptEventBinder.AdhocBindings.Add(objAdhocBinding)

                Dim objTcs As New TaskCompletionSource
                objAdhocBinding.Dispose = Sub()
                                              objTcs.TrySetResult()
                                              ScriptEventBinder.AdhocBindings.Remove(objAdhocBinding)
                                              For Each objEvent As EventScript.Event In objAdhocBinding.Events
                                                  objEvent.Owner.ScriptEventBinder.UnbindInvocTarget(objEvent.Name, objAdhocBinding)
                                              Next
                                          End Sub

                Dim sctCtr As CancellationTokenRegistration
                sctCtr = sctCancelToken.Register(Sub()
                                                     objTcs.TrySetCanceled()
                                                     ScriptEventBinder.AdhocBindings.Remove(objAdhocBinding)
                                                     For Each objEvent As EventScript.Event In objAdhocBinding.Events
                                                         objEvent.Owner.ScriptEventBinder.UnbindInvocTarget(objEvent.Name, objAdhocBinding)
                                                     Next
                                                     sctCtr.Dispose()
                                                 End Sub)

                Return objTcs.Task
            End If

        End Function

        ''' <summary>Plays a sound file.</summary>
        ''' <param name="strSoundFile">Qualified path and file name of the sound file. Supported file types are: .wav and .mp3</param>
        ''' <param name="sinVolume">Volume of this sound channel. Full volume is 1.0 and 0.5 is half volume.</param>
        ''' <param name="sctCancelToken">Token provided by a controlling source to notify this method that it should cancel its execution.</param>
        ''' <returns>The <see cref="Tasks.Task" /> that can be awaited, if execution must halt while the sound is playing.</returns>
        ''' <remarks>This method was designed specifically for use within the scripting context.</remarks>
        Public Function PlaySound(strSoundFile As String, sinVolume As Single, Optional sctCancelToken As CancellationToken = Nothing) As Task

            Dim objReader As WaveStream
            Try
                Select Case True
                    Case strSoundFile.ToLower.EndsWith(".wav")
                        objReader = New WaveFileReader(strSoundFile)
                    Case strSoundFile.ToLower.EndsWith(".mp3")
                        objReader = New Mp3FileReader(strSoundFile)
                    Case Else
                        Throw New ApplicationException("Unsupported file type.")
                End Select
            Catch ex As Exception
                If objReader IsNot Nothing Then objReader.Dispose()
                Throw New ApplicationException("Problem playing sound: " & ex.Message)
            End Try

            Dim objChannel As New WaveChannel32(objReader) With {.PadWithZeroes = False}
            objChannel.Volume = sinVolume
            Dim objAudioOutput As New DirectSoundOut()  'WaveOut() also works but can't do concurent sounds
            objAudioOutput.Init(objChannel)

            Dim objTcs As New TaskCompletionSource
            Dim sctCtr As CancellationTokenRegistration

            sctCtr = sctCancelToken.Register(
                Sub()
                    objTcs.TrySetCanceled()
                    objAudioOutput.Stop()  'this causes the While loop below to exit

                    'in case the cancellation occurred before objAudioOutput.Play() had a chance to be called 
                    'since we are disposing the objects, the While loop below will exit because the audio stream is no longer available
                    sctCtr.Dispose()
                    objAudioOutput.Dispose()
                    objChannel.Dispose()
                    objReader.Dispose()
                End Sub)

            If Not sctCancelToken.IsCancellationRequested Then
                Task.Run(
                    Sub()
                        objAudioOutput.Play()
                        While objAudioOutput.PlaybackState <> PlaybackState.Stopped
                            Thread.Sleep(20)
                        End While

                        'disposing here for when the sound stream ends
                        objTcs.TrySetResult()
                        sctCtr.Dispose()
                        objAudioOutput.Dispose()
                        objChannel.Dispose()
                        objReader.Dispose()
                    End Sub)
            End If

            Return objTcs.Task

        End Function

        ''' <summary>Gets a reference to a track object.</summary>
        ''' <param name="strName">The name given to the track at design time.</param>
        ''' <remarks>This method was designed specifically for use within the scripting context.</remarks>
        Public Function CtcTrack(strName As String) As Track
            Dim objTrack As Track = CtcService.Tracks(strName)
            If objTrack Is Nothing Then
                Throw New ApplicationException(String.Format("A track with name [{0}] could not be found.", strName))
            Else
                Return objTrack
            End If
        End Function

        ''' <summary>Gets a reference to a track object guaranteed to be a turnout.</summary>
        ''' <param name="strName">The name given to the turnout at design time.</param>
        ''' <remarks>This method was designed specifically for use within the scripting context.</remarks>
        Public Function CtcTurnout(strName As String) As Track
            Dim objTrack As Track = CtcTrack(strName)
            If objTrack.IsTurnout Then
                Return objTrack
            Else
                Throw New ApplicationException(String.Format("A turnout with name [{0}] could not be found.", strName))
            End If
        End Function

        ''' <summary>Gets a reference to a block object.</summary>
        ''' <param name="strName">The name given to the block at design time.</param>
        ''' <remarks>This method was designed specifically for use within the scripting context.</remarks>
        Public Function CtcBlock(strName As String) As Block
            Dim objBlock As Block = CtcService.Blocks(strName)
            If objBlock Is Nothing Then
                Throw New ApplicationException(String.Format("A block with name [{0}] could not be found.", strName))
            Else
                Return objBlock
            End If
        End Function

        ''' <summary>Gets a reference to a route object.</summary>
        ''' <param name="strName">The name given to the route at design time.</param>
        ''' <remarks>This method was designed specifically for use within the scripting context.</remarks>
        Public Function CtcRoute(strName As String) As Route
            Dim objRoute As Route = CtcService.Routes(strName)
            If objRoute Is Nothing Then
                Throw New ApplicationException(String.Format("A route with name [{0}] could not be found.", strName))
            Else
                Return objRoute
            End If
        End Function

        ''' <summary>Gets a reference to a sensor object.</summary>
        ''' <param name="strName">The name given to the sensor at design time.</param>
        ''' <remarks>This method was designed specifically for use within the scripting context.</remarks>
        Public Function CtcSensor(strName As String) As Sensor
            Dim objSensor As Sensor = CtcService.Sensors(strName)
            If objSensor Is Nothing Then
                Throw New ApplicationException(String.Format("A sensor with name [{0}] could not be found.", strName))
            Else
                Return objSensor
            End If
        End Function

        ''' <summary>Gets a reference to a signal object.</summary>
        ''' <param name="strName">The name given to the signal at design time.</param>
        ''' <remarks>This method was designed specifically for use within the scripting context.</remarks>
        Public Function CtcSignal(strName As String) As Signal
            Dim objSignal As Signal = CtcService.Signals(strName)
            If objSignal Is Nothing Then
                Throw New ApplicationException(String.Format("A signal with name [{0}] could not be found.", strName))
            Else
                Return objSignal
            End If
        End Function

        ''' <summary>Gets a reference to an accessory object.</summary>
        ''' <param name="strName">The name given to the accessory at design time.</param>
        ''' <remarks>This method was designed specifically for use within the scripting context.</remarks>
        Public Function CtcAccessory(strName As String) As Accessory
            Dim objAccessory As Accessory = CtcService.Accessories(strName)
            If objAccessory Is Nothing Then
                Throw New ApplicationException(String.Format("An accessory with name [{0}] could not be found.", strName))
            Else
                Return objAccessory
            End If
        End Function

        ''' <summary>Gets a reference to a label object.</summary>
        ''' <param name="strName">The name given to the label at design time.</param>
        ''' <remarks>This method was designed specifically for use within the scripting context.</remarks>
        Public Function CtcLabel(strName As String) As Label
            Dim objLabel As Label = CtcService.Labels(strName)
            If objLabel Is Nothing Then
                Throw New ApplicationException(String.Format("A label with name [{0}] could not be found.", strName))
            Else
                Return objLabel
            End If
        End Function

        ''' <summary>Gets a reference to a button object.</summary>
        ''' <param name="strName">The name given to the button at design time.</param>
        ''' <remarks>This method was designed specifically for use within the scripting context.</remarks>
        Public Function CtcButton(strName As String) As Button
            Dim objButton As Button = CtcService.Buttons(strName)
            If objButton Is Nothing Then
                Throw New ApplicationException(String.Format("A button with name [{0}] could not be found.", strName))
            Else
                Return objButton
            End If
        End Function

        ''' <summary>Gets a reference to an engine object.</summary>
        ''' <param name="strName">The name given to the engine at design time.</param>
        ''' <remarks>This method was designed specifically for use within the scripting context.</remarks>
        Public Function CtcEngine(strName As String) As Engine
            Dim objEngine As Engine = CtcService.Engines(strName)
            If objEngine Is Nothing Then
                Throw New ApplicationException(String.Format("An engine with name [{0}] could not be found.", strName))
            Else
                Return objEngine
            End If
        End Function

        ''' <summary>Gets a reference to an engine object guaranteed to be bound to a slot.</summary>
        ''' <param name="strName">The name given to the engine at design time.</param>
        ''' <remarks>This method was designed specifically for use within the scripting context.</remarks>
        Public Function CtcBoundEngine(strName As String) As Engine
            Dim objEngine As Engine = CtcEngine(strName)
            If objEngine.IsBound Then
                Return objEngine
            Else
                Throw New ApplicationException(String.Format("An engine with name [{0}] is not bound.", strName))
            End If
        End Function

        ''' <summary>Gets a reference to a layout sequence object.</summary>
        ''' <param name="strName">The name given to the sequence at design time.</param>
        ''' <remarks>This method was designed specifically for use within the scripting context.</remarks>
        Public Function CtcSequence(strName As String) As Sequence
            Dim objSequence As Sequence = CtcService.Sequences(strName)
            If objSequence Is Nothing Then
                Throw New ApplicationException(String.Format("A sequence with name [{0}] could not be found.", strName))
            Else
                Return objSequence
            End If
        End Function

        ''' <summary>Gets a reference to a layout event script object.</summary>
        ''' <param name="strName">The name given to the event script at design time.</param>
        ''' <remarks>This method was designed specifically for use within the scripting context.</remarks>
        Public Function CtcEventScript(strName As String) As EventScript
            Dim objScript As EventScript = CtcService.EventScripts(strName)
            If objScript Is Nothing Then
                Throw New ApplicationException(String.Format("An event script with name [{0}] could not be found.", strName))
            Else
                Return objScript
            End If
        End Function

        ''' <summary>Gets a reference to a layout step script object.</summary>
        ''' <param name="strName">The name given to the step script at design time.</param>
        ''' <remarks>This method was designed specifically for use within the scripting context.</remarks>
        Public Function CtcStepScript(strName As String) As StepScript
            Dim objScript As StepScript = CtcService.StepScripts(strName)
            If objScript Is Nothing Then
                Throw New ApplicationException(String.Format("A step script with name [{0}] could not be found.", strName))
            Else
                Return objScript
            End If
        End Function

    End Module

#End Region

#Region "Event Binding"

    ''' <summary>Interface implemented by objects that support bindable scripting events.</summary>
    Public Interface ISupportsScriptEvents
        ReadOnly Property ScriptEventBinder() As ScriptEventBinder
    End Interface

    Friend Interface IInvocationTarget
        Property Name As String
        Property Handler As Func(Of ScriptEventArgs, Task)
    End Interface

    ''' <summary>Data exchange class passed to script event handlers.</summary>
    Public NotInheritable Class ScriptEventArgs

        'values passed to handler
        Private _objSender As Object
        Private _strEventName As String
        Private _objArgs As Object
        Private _sctCancelToken As CancellationToken

        'return values from handler
        Friend Property ReqCancel As New [Boolean]  'this is boxed on purpose so the value can be shared by all clones

        Friend Property Awaiter As TaskCompletionSource
        Friend Property OnDispose As Action

        Public Sub New()
        End Sub

        Public Sub New(objSender As Object, strEventName As String, objArgs As Object, sctCancelToken As CancellationToken)
            _objSender = objSender
            _strEventName = strEventName
            _objArgs = objArgs
            _sctCancelToken = sctCancelToken
        End Sub


        ''' <summary>Object that raised the event.</summary>
        Public ReadOnly Property Sender As Object
            Get
                Return _objSender
            End Get
        End Property

        ''' <summary>The name of the raised event.</summary>
        Public ReadOnly Property EventName As String
            Get
                Return _strEventName
            End Get
        End Property

        ''' <summary>Contextual arguments specific to an event type.</summary>
        Public ReadOnly Property Args As Object
            Get
                Return _objArgs
            End Get
        End Property

        ''' <summary>Notifies downstream asynchronous methods to cancel their execution.</summary>
        ''' <remarks>This token is provided by a controlling source and should be passed down the call stack to asynchronous methods so the chain of execution can be canceled.</remarks>
        Public ReadOnly Property CancelToken As CancellationToken
            Get
                Return _sctCancelToken
            End Get
        End Property


        ''' <summary>Notifies the event raiser to cancel the opperation it's about to perform.</summary>
        ''' <remarks>This hint is only observed for events that support opperation canceling. These are events with names that start with "Before" (i.e. "BeforeStateChange").</remarks>
        Public Sub Cancel()
            Me.ReqCancel.Value = True
        End Sub


        ''' <summary>Notifies the event raiser to await the event handler before proceeding execution.</summary>
        ''' <remarks>
        ''' By default, event handlers execute in a fire and forget fashion which is the most common use case. But in some scenarios the handler may want to instruct
        ''' the event raiser to wait for the handler's execution. In such a case, this hint should be set before the first <i>Await</i> is encountered in the
        ''' handler. If no <i>Await</i> is used in the event handler this hint is superfluous and will not be observed.
        ''' A use case might be when needing to set the <see cref="Cancel" /> hint, in the handler, after an <i>Await</i>.
        ''' </remarks>
        ''' <seealso cref="EndAwait" />
        Public Sub AwaitMe()
            If Me.Awaiter Is Nothing Then
                Me.Awaiter = New TaskCompletionSource
            End If
        End Sub

        ''' <summary>Notifies the event raiser to end awaiting the handler and proceed execution.</summary>
        ''' <remarks>
        ''' This hint will cancel a previous <see cref="AwaitMe" /> hint. By doing so, the event raiser will cease to wait for the handler and continue to execute
        ''' even if the handler has not finished. If <i>EndAwait</i> is not set, after an <see cref="AwaitMe" />, the event raiser will wait until the handler fully completes execution.
        ''' </remarks>
        ''' <seealso cref="AwaitMe" />
        Public Sub EndAwait()
            If Me.Awaiter IsNot Nothing Then
                Me.Awaiter.TrySetResult()
            End If
        End Sub

        ''' <summary>Disposes an adhoc event binding.</summary>
        ''' <remarks>
        ''' This hint is only observed in handlers defined by <see cref="OnEvent" />. 
        ''' When set, it unregisters the event listeners from the event source and completes the task being awaited on by <i>OnEvent</i>.
        ''' </remarks>
        Public Sub Dispose()
            If Me.OnDispose IsNot Nothing Then
                Me.OnDispose.Invoke()
            End If
        End Sub


        Friend Function Clone() As ScriptEventArgs
            Dim objClone As New ScriptEventArgs(_objSender, _strEventName, _objArgs, _sctCancelToken)
            objClone.ReqCancel = Me.ReqCancel   'all clones will share this boxed boolean value
            Return objClone
        End Function

    End Class

    ''' <summary>This class provides script binding services between railroad object events and delegate handlers.</summary>
    Public NotInheritable Class ScriptEventBinder

        '---- Shared Member (for adhoc event binding) --------------------------------------------------

        Friend NotInheritable Class AdhocBinding
            Implements IInvocationTarget

            Public Sub New(strName As String, delHandler As Func(Of ScriptEventArgs, Task))
                Me.Name = strName
                Me.Handler = delHandler
            End Sub

            Public Property Name As String Implements IInvocationTarget.Name
            Public Property Events As New List(Of EventScript.Event)
            Public Property Handler As Func(Of ScriptEventArgs, Task) Implements IInvocationTarget.Handler
            Public Property Dispose As Action

        End Class

        Friend Shared Property AdhocBindings As New List(Of AdhocBinding)

        '---- Instance Members (for all objects that implement ISupportsScriptEvents) ------------------

        Private _objOwner As ISupportsScriptEvents
        Private _objInvocTargets As New Dictionary(Of String, List(Of IInvocationTarget))   'keyed by event name

        Friend Sub New(objOwner As ISupportsScriptEvents, strEventNames As String())
            _objOwner = objOwner
            Me.PublishEvents(strEventNames)
        End Sub

        Friend Sub PublishEvents(strEventNames As String())
            For Each strEventName As String In strEventNames
                _objInvocTargets.Add(strEventName, New List(Of IInvocationTarget))
            Next
        End Sub

        Friend Sub UnPublishEvents(strEventNames As String())
            For Each strEventName As String In strEventNames
                _objInvocTargets.Remove(strEventName)
            Next
        End Sub


        ''' <summary>A list of <see cref="EventScript.Event">events</see> published by the binder owner.</summary>
        Public Function PublishedEvents() As List(Of EventScript.Event)
            Dim objEvents As New List(Of EventScript.Event)
            For Each strEventName As String In _objInvocTargets.Keys
                objEvents.Add(New EventScript.Event(_objOwner, strEventName))
            Next
            Return objEvents
        End Function


        Friend Function BindInvocTarget(strEventName As String, objInvocTarget As IInvocationTarget) As Boolean
            'for script invocation target, gets called on the thread that starts the CtcService before starting it; most likely the UI thread
            'for adhoc invocation target, gets called on the CTC thread

            If _objInvocTargets.ContainsKey(strEventName) Then
                _objInvocTargets(strEventName).Add(objInvocTarget)
                Return True
            Else
                CtcService.PostMessage(CtcService.MessageCat.Error, String.Format("[{0}] failed to bind event [{1}.{2}].", objInvocTarget.Name, _objOwner.ToString, strEventName))
                Return False
            End If

        End Function

        Friend Sub UnbindInvocTarget(strEventName As String, objInvocTarget As IInvocationTarget)
            'for script invocation target, gets called on the thread that stops the CtcService after stopping it; most likely the UI thread
            'for adhoc invocation target, gets called on the CTC thread

            If _objInvocTargets.ContainsKey(strEventName) Then
                _objInvocTargets(strEventName).Remove(objInvocTarget)
            End If
        End Sub


        Friend Sub InvokeHandlersOnAny(objScriptEventArgs As ScriptEventArgs)
            If CtcService.IsCurrentThread Then
                InvokeHandlersOnCtc(objScriptEventArgs)
            Else
                CtcService.Execute(Sub() InvokeHandlersOnCtc(objScriptEventArgs))
            End If
        End Sub

        Friend Function InvokeHandlersOnCtc(objScriptEventArgs As ScriptEventArgs) As Task
            'for thread safety, should only be called on the CTC thread

            If _objInvocTargets.ContainsKey(objScriptEventArgs.EventName) Then

                'can't iterate through _objInvocTargets(strEventName) directly because adhoc bindings could call UnbindInvocTarget() within the iteration
                'illegally removing items from the collection we are iterating though; I instead iterate through a clone of the collection
                Dim objInvocTargets As New List(Of IInvocationTarget)(_objInvocTargets(objScriptEventArgs.EventName))

                Dim objAwaitables As New List(Of Task)
                For Each objInvocTarget As IInvocationTarget In objInvocTargets

                    Dim objScriptEventArgsClone As ScriptEventArgs = objScriptEventArgs.Clone

                    Select Case True
                        Case TypeOf objInvocTarget Is EventScript
                            objAwaitables.Add(InvokeAwaitableHandler(objInvocTarget, objScriptEventArgsClone))

                        Case TypeOf objInvocTarget Is AdhocBinding
                            Dim objAdhocBinding As AdhocBinding = objInvocTarget
                            Select Case True
                                Case objAdhocBinding.Handler Is Nothing
                                    objAdhocBinding.Dispose.Invoke()

                                Case Else
                                    objScriptEventArgsClone.OnDispose = objAdhocBinding.Dispose
                                    objAwaitables.Add(InvokeAwaitableHandler(objAdhocBinding, objScriptEventArgsClone))

                            End Select

                    End Select
                Next

                If objAwaitables.Count > 0 Then
                    Return Task.WhenAll(objAwaitables)
                Else
                    Return Task.FromResult(0)
                End If

            End If

        End Function

        Friend Shared Function InvokeAwaitableHandler(objInvocTarget As IInvocationTarget, objScriptEventArgs As ScriptEventArgs) As Task
            'for thread safety, should only be called on the CTC thread

            'don't await this because we use the user created TCS from objScriptEventArgs.Awaiter for task completion
            InvokeHandler(objInvocTarget, objScriptEventArgs)

            If objScriptEventArgs.Awaiter Is Nothing Then
                Return Task.FromResult(0)
            Else
                Return objScriptEventArgs.Awaiter.Task
            End If

        End Function

        Friend Shared Async Sub InvokeHandler(objInvocTarget As IInvocationTarget, objScriptEventArgs As ScriptEventArgs)
            'for thread safety, should only be called on the CTC thread

            'for adhoc handlers, the user can provide a lambda:
            '"Async Function(e As ScriptEventArgs)" - if it contains awaitable methods; this one returns a Task created by the framework
            'or 
            '"Function(e As ScriptEventArgs)" - if it does not contain awaitable methods; this one returns nothing for Task if one is not provided by the user
            'note that the Task return type does not need to be specified on lambdas; it is implied by the signature

            'this method is called in a fire and forget fashion because the awaitable task is managed though the hints, set though objScriptEventArgs, by the handler, 
            'and not by the task returned by the handler; here however we await the handler's returned task for a different purpose
            '1. because we need to capture exceptions through it
            '2. because we must call .EndAwait() at the end to guarantee the actual task completion

            'if errors are to be caught by this Try..Catch, the content of the handler being invoked has to await all awaitable methods it calls 
            'otherwise code execution could fall outside the context of this Try..Catch
            'an example would be the handler not awaiting an async method which later on throws an exception

            'remember the awaits in the handler do not control when the invoker awaits or stops awaiting; that is done through e.AwaitMe and e.EndAwait 

            Try
                Dim objTask As Task = objInvocTarget.Handler.Invoke(objScriptEventArgs)
                If objTask IsNot Nothing Then Await objTask
            Catch ex As Exception
                CtcService.PostMessage(CtcService.MessageCat.Error,
                    String.Format("Runtime error occurred in [{0}]: {1}", objInvocTarget.Name, If(ex.InnerException Is Nothing, ex.Message, ex.InnerException.Message)))
            Finally
                'if the user did not call .EndAwait(), we do it for him at the end to complete the task
                'if no TCS awaiter has been set this will do nothing
                objScriptEventArgs.EndAwait()
            End Try
        End Sub

    End Class

#End Region

End Namespace



