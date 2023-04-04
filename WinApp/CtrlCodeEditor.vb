Imports Syncfusion.Windows.Forms.Edit

Namespace CustomControls

    Public Class CodeEditor
        Inherits Syncfusion.Windows.Forms.Edit.EditControl

        Private Shared _objXmlDoc As Xml.XmlDocument
        Private Shared _objKeyWords As Dictionary(Of String, String)

        Public Sub New()
            MyBase.New()

            'single load the lexem config for all code editors
            If _objXmlDoc Is Nothing Then
                _objXmlDoc = New Xml.XmlDocument()
                _objXmlDoc.LoadXml(My.Resources.CodeEditVBConfig)
            End If

            Me.AllowDrop = True
            Me.AutoIndentMode = Enums.AutoIndentMode.Smart
            Me.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
            Me.ClearStaticDataOnDispose = False
            Me.ContextMenuEnabled = False
            Me.ConvertOnLoad = False
            Me.InsertDroppedFileIntoText = True
            Me.SaveOnClose = False
            Me.ShowIndicatorMargin = False
            Me.ShowLineNumbers = True
            Me.TabSize = 4

            Me.Configurator.Open(_objXmlDoc)
            Me.ApplyConfiguration("VB.NET")
            AddHandler Me.Language("KeyWord").OnCustomDraw, AddressOf FormatKeyWord

            'the code editor does not provide auto case formatting so we have to create a lookup table of key words from the lexem config
            'that can later be used by the FormatKeyWord() event handler to apply the formatting
            'occurs only once for all editors
            If _objKeyWords Is Nothing Then
                _objKeyWords = New Dictionary(Of String, String)
                Dim objNodes As Xml.XmlNodeList = _objXmlDoc.SelectNodes("//lexem[@Type='KeyWord']")
                For Each objXmlNode As Xml.XmlNode In objNodes
                    If Not _objKeyWords.ContainsKey(objXmlNode.Attributes("BeginBlock").Value.ToLower) Then
                        _objKeyWords.Add(objXmlNode.Attributes("BeginBlock").Value.ToLower, objXmlNode.Attributes("BeginBlock").Value)
                    End If
                    If objXmlNode.Attributes("EndBlock") IsNot Nothing AndAlso objXmlNode.Attributes("EndBlock").Value <> Nothing Then
                        For Each strValue As String In objXmlNode.Attributes("EndBlock").Value.Split("|")
                            If Not _objKeyWords.ContainsKey(strValue.ToLower) Then
                                _objKeyWords.Add(strValue.ToLower, strValue)
                            End If
                        Next
                    End If
                Next
            End If

        End Sub

        'used for auto case formatiting of key words
        Public Sub FormatKeyWord(sender As Object, e As CustomSnippetDrawEventArgs)
            If _objKeyWords.ContainsKey(e.Text.ToLower) Then
                'set proper case on key words
                e.Text = _objKeyWords(e.Text.ToLower)
            Else
                'if not found in key word list then just cap first letter
                e.Text = e.Text.Substring(0, 1).ToUpper + e.Text.Substring(1, e.Text.Length - 1).ToLower
            End If

        End Sub

        'no longer used as of 7/28/13
        'was used when the code editors were cached and reused needing them to be reset as if they were newly created 
        Public Sub ResetToDefault()
            Me.ResetUndoInfo()
            Me.ReadOnly = False
            Me.TopVerticalSplitterPosition = 0
            Me.BottomVerticalSplitterPosition = 0
            Me.HorizontalSplitterPosition = 0
        End Sub

        Protected Overrides Sub Dispose(disposing As Boolean)
            'removed dispose code because WPF throws an exception when this is called on application closed
            'I think WPF has its own procedure for automaticaly disposing of controls
        End Sub

    End Class

End Namespace
