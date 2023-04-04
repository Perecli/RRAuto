Imports System.Runtime.CompilerServices

Public Module Extensions

#Region "Color"

    ''' <summary>Convert Media.Color to Drawing.Color for property grid</summary>
    <Extension()> Public Function Convert(sctColor As Color) As System.Drawing.Color
        With sctColor
            Return System.Drawing.Color.FromArgb(.R, .G, .B)    'ignore the aplha channel because the property grid color editor does not edit alpha
        End With
    End Function

    ''' <summary>Convert Drawing.Color to Media.Color for property grid</summary>
    <Extension()> Public Function Convert(sctColor As System.Drawing.Color, bytA As Byte) As Color
        With sctColor
            Return Color.FromArgb(bytA, .R, .G, .B)             'inject back the aplha channel because the property grid color editor does not edit alpha
        End With
    End Function

    ''' <summary>Convert "#FF000000" string color type to Media.Color</summary>
    <Extension()> Public Function ToColor(strColor As String) As Color
        Return ColorConverter.ConvertFromString(strColor)
    End Function

    ''' <summary>Allow chaining when setting the alpha component of a Media.Color</summary>
    <Extension()> Public Function SetAlpha(sctColor As Color, bytA As Byte) As Color
        sctColor.A = bytA
        Return sctColor
    End Function

    ''' <summary>Alpha blend two Media.Colors</summary>
    <Extension()> Public Function AlphaBlend(sctBottomColor As Color, sctTopColor As Color) As Color
        Dim sctNewColor As Color
        sctNewColor.A = 255
        sctNewColor.R = sctBottomColor.R + (CType(sctTopColor.R, Integer) - sctBottomColor.R) * sctTopColor.A / 255
        sctNewColor.G = sctBottomColor.G + (CType(sctTopColor.G, Integer) - sctBottomColor.G) * sctTopColor.A / 255
        sctNewColor.B = sctBottomColor.B + (CType(sctTopColor.B, Integer) - sctBottomColor.B) * sctTopColor.A / 255
        Return sctNewColor
    End Function

#End Region

#Region "Drawing"

    <Extension()> Public Function Opacity(objDrawingImage As DrawingImage, dblOpacity As Double) As DrawingImage
        If objDrawingImage Is Nothing Then Return Nothing
        Dim objDrawingGroup As DrawingGroup = objDrawingImage.Drawing.Clone
        objDrawingGroup.Opacity = dblOpacity
        Return New DrawingImage(objDrawingGroup)
    End Function

    <Extension()> Public Sub ApplyTransform(objDrawing As DrawingGroup, objTransform As Transform)

        'zero out top/left drawing location imported from Expression Design
        If TypeOf objTransform Is TranslateTransform Then
            With DirectCast(objTransform, TranslateTransform)
                .X -= objDrawing.Bounds.X
                .Y -= objDrawing.Bounds.Y
            End With
        End If

        Select Case True
            Case objDrawing.Transform Is Nothing
                'add single transform
                objDrawing.Transform = objTransform

            Case TypeOf objDrawing.Transform Is TransformGroup
                'add to existing transform group
                DirectCast(objDrawing.Transform, TransformGroup).Children.Add(objTransform)

            Case Else
                'convert existing single transform to a transform group so we can add to it
                Dim objTransGroup As New TransformGroup
                objTransGroup.Children.Add(objDrawing.Transform)
                objTransGroup.Children.Add(objTransform)
                objDrawing.Transform = objTransGroup

        End Select

    End Sub

#End Region

#Region "Geometry"

    '''<summary>Gets the x-axis value of the center of the rectangle.</summary>
    <Extension()> Public Function Center(sctRect As Rect) As Double
        Return sctRect.X + sctRect.Width / 2
    End Function

    '''<summary>Gets the y-axis value of the middle of the rectangle.</summary>
    <Extension()> Public Function Middle(sctRect As Rect) As Double
        Return sctRect.Y + sctRect.Height / 2
    End Function

    '''<summary>Gets the distance between two points.</summary>
    <Extension()> Public Function DistanceTo(sctPoint1 As Point, sctPoint2 As Point) As Double
        Return Math.Sqrt((sctPoint2.X - sctPoint1.X) ^ 2 + (sctPoint2.Y - sctPoint1.Y) ^ 2)
    End Function

#End Region

End Module
