Public Class Form1

    Dim original As Bitmap
    Dim original2 As Bitmap

    Dim template As String

    Dim q_Number As Integer
    Dim q_Choice As Integer
    Dim q_perColumn As Integer

    Dim idnumber(5) As Integer
    Dim selected_r(q_Number) As Integer

    Function getLineTXT(ByVal path As String, ByVal lineNo As Integer)

        Dim reader As New System.IO.StreamReader(path)
        Dim allLines As List(Of String) = New List(Of String)
        Do While Not reader.EndOfStream
            allLines.Add(reader.ReadLine())
        Loop
        reader.Close()

        Return ReadLine(lineNo, allLines)

    End Function

    Function ReadLine(lineNumber As Integer, lines As List(Of String)) As String
        Return lines(lineNumber - 1)
    End Function

    Function rotateImage(ByVal img As Bitmap, ByVal offsetX As Integer, ByVal offsetY As Integer, ByVal angle As Double)

        Dim offset As PointF
        offset.X = offsetX
        offset.Y = offsetY

        Dim imgrotated As New Bitmap(img.Width, img.Height, System.Drawing.Imaging.PixelFormat.Format32bppPArgb)

        Dim g As Graphics = Graphics.FromImage(imgrotated)
        g.Clear(Color.White)
        g.TranslateTransform(offset.X, offset.Y)
        g.RotateTransform(angle)
        g.TranslateTransform(-offset.X, -offset.Y)
        g.DrawImage(img, New PointF(0, 0))

        Return imgrotated
    End Function

    Private Function DegreesToRadians(ByVal degrees As Double) As Double
        Return degrees * Math.PI / 180
    End Function
    Private Function RadiansToDegrees(ByVal radians As Double) As Double
        Return radians * 180 / Math.PI
    End Function

    Function countBlackID(ByVal img As Bitmap, ByVal centerX As Integer, ByVal centerY As Integer, ByVal radius As Integer)
        Dim totalBK As Integer

        Dim Xq As Integer 'top left
        Dim Yq As Integer 'top left

        Xq = centerX - radius
        Yq = centerY - radius

        Dim ystep As Integer = Yq
        For x4 As Integer = Xq To Xq + (radius * 2)

            Dim pix As Color = img.GetPixel(x4, ystep)

            If (pix.R < 10) Then
                totalBK = totalBK + 1
            End If

            If ystep = Yq Then
                img.SetPixel(x4, ystep, Color.Red)
            End If
            If ystep = Yq + (radius * 2) Then
                img.SetPixel(x4, ystep, Color.Red)
            End If

            If x4 = ((Xq + (radius * 2))) Then
                If ystep <= (Yq + (radius * 2)) Then
                    x4 = Xq
                    ystep = ystep + 1
                End If
            End If

            If x4 = Xq Then
                img.SetPixel(x4, ystep, Color.Red)
            End If
            If x4 = ((Xq + (radius * 2)) - 1) Then
                img.SetPixel(x4, ystep, Color.Red)
            End If
        Next

        If totalBK > 270 Then
            Return totalBK
        Else
            Return -1
        End If
    End Function

    Private Sub BackgroundWorker1_DoWork(sender As Object, e As System.ComponentModel.DoWorkEventArgs) Handles BackgroundWorker1.DoWork

        template = TextBox2.Text

        q_Number = getLineTXT(template, 3)
        q_Choice = getLineTXT(template, 4)
        q_perColumn = getLineTXT(template, 5)

        ReDim Preserve selected_r(q_Number)

        'set original and create converted BITMAP Variable
        Dim converted As Bitmap = New Bitmap(original.Width, original.Height, System.Drawing.Imaging.PixelFormat.Format32bppPArgb)

        Using gfx As Graphics = Graphics.FromImage(converted)
            gfx.DrawImage(original, 0, 0, original.Width, original.Height)
        End Using

        'get original height and width max-coordinate
        Dim height As Integer = original.Height - 1
        Dim width As Integer = original.Width - 1

        'get topright alignment point
        Dim Xtr As Integer
        Dim Ytr As Integer
        Dim ysteptr As Integer = 0
        For x3 As Integer = width To width - 100 Step -1

            Dim pix As Color = converted.GetPixel(x3, ysteptr)
            If (pix.R < 10) Then
                converted.SetPixel(x3, ysteptr, Color.Red)

                Dim isnoise As Boolean = False

                For y As Integer = ysteptr + 1 To ysteptr + 3
                    If converted.GetPixel(x3, y).R < 10 Then
                        converted.SetPixel(x3, y, Color.Yellow)
                    Else
                        isnoise = True
                    End If
                Next

                If isnoise = False Then

                    Xtr = x3
                    Ytr = ysteptr

                    Exit For
                End If

            End If

            If x3 = (width - 100) Then
                x3 = width
                ysteptr = ysteptr + 1
            End If
        Next

        'get topleft alignment point
        Dim Xtl As Integer
        Dim Ytl As Integer
        Dim ysteptl As Integer = 0
        For x3 As Integer = 0 To 200

            Dim pix As Color = converted.GetPixel(x3, ysteptl)
            If (pix.R < 10) Then

                converted.SetPixel(x3, ysteptl, Color.Red)

                Dim isnoise As Boolean = False

                For y As Integer = ysteptl + 1 To ysteptl + 3
                    If converted.GetPixel(x3, y).R < 10 Then
                        converted.SetPixel(x3, y, Color.Yellow)
                    Else
                        isnoise = True
                    End If
                Next

                If isnoise = False Then

                    Xtl = x3
                    Ytl = ysteptl

                    Exit For
                End If

            End If

            If x3 = 200 Then
                x3 = 0
                ysteptl = ysteptl + 1
            End If
        Next

        'check if down/up/normal
        Dim t As String = ""

        If Ytr < Ytl Then
            t = "down"
        ElseIf Ytr > Ytl Then
            t = "up"
        Else
            t = "normal"
        End If

        ''create top line (red)
        'For x2 As Integer = 0 To width
        '    converted.SetPixel(x2, 0, Color.Red)
        'Next

        Dim Xtop As Integer
        Dim ytop As Integer

        'rotate base on condition
        If t = "up" Then

            'Tan (TOA)

            Dim Tan As Double
            Dim o As Double
            Dim a As Double

            o = Ytr - Ytl
            a = Xtr - Xtl

            o = DegreesToRadians(o)
            a = DegreesToRadians(a)

            Tan = Math.Tan(o / a)
            Tan = RadiansToDegrees(Tan)

            converted = rotateImage(converted, Xtr, Ytr, -Tan)

            Xtop = Xtr - (Xtr - Xtl + 1)
            ytop = Ytr

        ElseIf t = "down" Then

            'Tan (TOA)

            Dim Tan As Double
            Dim o As Double
            Dim a As Double

            o = Ytr - Ytl
            a = Xtl - Xtr

            o = DegreesToRadians(o)
            a = DegreesToRadians(a)

            Tan = Math.Tan(o / a)
            Tan = RadiansToDegrees(Tan)

            converted = rotateImage(converted, Xtl, Ytl, Tan)

            Xtop = Xtl
            ytop = Ytl

        End If

        'get id number shaded on paper
        Dim digit1(10) As Integer
        Dim digit2(10) As Integer
        Dim digit3(10) As Integer
        Dim digit4(10) As Integer
        Dim digit5(10) As Integer
        Dim digitX As Integer
        Dim digitY As Integer
        Dim digitINI As Integer
        Dim Yini2 As Integer
        Dim digitN As Integer = 1
        Dim betweenDigitX As Integer
        Dim digitNo As Integer = 1
        Dim accumulative As Integer = 0
        Dim useacc As Boolean = False
        Dim extra As Integer = 0
        'Read Digit1
        For i As Integer = 2 To 52
            If i = 2 Then
                digitX = getLineTXT(template, 7)
                digitINI = getLineTXT(template, 8)
                Yini2 = getLineTXT(template, 9)
                betweenDigitX = getLineTXT(template, 10)
                Try
                    If getLineTXT(template, 18) = "y" Then
                        useacc = True
                    End If
                Catch
                End Try
                digitY = Yini2 + digitINI
            Else
                If digitNo = 1 Then
                    digit1(digitN) = countBlackID(converted, Xtop + digitX, ytop + digitY + extra, 11)
                ElseIf digitNo = 2 Then
                    digit2(digitN) = countBlackID(converted, Xtop + digitX, ytop + digitY + extra, 11)
                ElseIf digitNo = 3 Then
                    digit3(digitN) = countBlackID(converted, Xtop + digitX, ytop + digitY + extra, 11)
                ElseIf digitNo = 4 Then
                    digit4(digitN) = countBlackID(converted, Xtop + digitX, ytop + digitY + extra, 11)
                ElseIf digitNo = 5 Then
                    digit5(digitN) = countBlackID(converted, Xtop + digitX, ytop + digitY + extra, 11)
                End If

                If useacc = True Then
                    accumulative = accumulative + 1
                    extra = accumulative
                End If

                digitY = digitY + Yini2
                If digitN = 10 Then
                    digitNo = digitNo + 1
                    digitN = 1
                    digitX = digitX + betweenDigitX
                    digitY = digitINI + Yini2
                    accumulative = 0
                    extra = 0
                Else
                    digitN = digitN + 1
                End If
            End If
        Next

        Dim maxInt1 As Integer = digit1.Max()
        Dim b1 As Integer = Array.IndexOf(digit1, maxInt1) - 1
        idnumber(1) = b1
        Dim maxInt2 As Integer = digit2.Max()
        Dim b2 As Integer = Array.IndexOf(digit2, maxInt2) - 1
        idnumber(2) = b2
        Dim maxInt3 As Integer = digit3.Max()
        Dim b3 As Integer = Array.IndexOf(digit3, maxInt3) - 1
        idnumber(3) = b3
        Dim maxInt4 As Integer = digit4.Max()
        Dim b4 As Integer = Array.IndexOf(digit4, maxInt4) - 1
        idnumber(4) = b4
        Dim maxInt5 As Integer = digit5.Max()
        Dim b5 As Integer = Array.IndexOf(digit5, maxInt5) - 1
        idnumber(5) = b5


        'get answer
        Dim choice(q_Choice) As Integer
        Dim Yini As Integer
        Dim Xfirst As Integer
        Dim betweenRow As Integer
        Dim betweenColumn As Integer
        Dim betweenBIG As Integer
        Dim Xd As Integer
        Dim Yd As Integer

        Yini = getLineTXT(template, 12)
        Xfirst = getLineTXT(template, 13)
        betweenRow = getLineTXT(template, 14)
        betweenColumn = getLineTXT(template, 15)
        betweenBIG = getLineTXT(template, 16)

        Xd = Xtop + Xfirst
        Yd = ytop + Yini

        For i As Integer = 1 To q_Number

            For i2 As Integer = 1 To q_Choice
                choice(i2) = countBlackID(converted, Xd, Yd, 11)
                Xd = Xd + betweenRow

                If i2 = q_Choice Then
                    Yd = Yd + betweenColumn
                    Xd = Xtop + Xfirst
                End If
            Next

            Dim maxInt As Integer = choice.Max()
            Dim b As Integer = Array.IndexOf(choice, maxInt)
            selected_r(i) = b

            If i Mod q_perColumn = 0 Then
                Xfirst = Xfirst + (betweenBIG)
                Xd = Xtop + Xfirst
                Yd = ytop + Yini
            End If
        Next

        PictureBox1.Image = converted

        'converted.Save("D:\ASReader\saved.bmp", Imaging.ImageFormat.Bmp)
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        original = New Bitmap(TextBox1.Text)
        Label6.Text = "Processing..."
        Label1.Text = "X"
        Label2.Text = "X"
        Label3.Text = "X"
        Label4.Text = "X"
        Label5.Text = "X"
        Label11.Text = "X"
        BackgroundWorker1.RunWorkerAsync()

    End Sub

    Private Sub BackgroundWorker1_RunWorkerCompleted(sender As Object, e As System.ComponentModel.RunWorkerCompletedEventArgs) Handles BackgroundWorker1.RunWorkerCompleted
        Label1.Text = idnumber(1)
        Label2.Text = idnumber(2)
        Label3.Text = idnumber(3)
        Label4.Text = idnumber(4)
        Label5.Text = idnumber(5)

        If DataGridView1.Rows.Count < 80 Then
            DataGridView1.Rows.Add(80)
        End If

        For i As Integer = 1 To q_Number
            Dim rows As Integer = i - 1
            DataGridView1.Rows(rows).Cells(0).Value = i
            DataGridView1.Rows(rows).Cells(1).Value = selected_r(i)
        Next

        Label6.Text = "Done!"
    End Sub

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        DataGridView1.Columns(0).ReadOnly = True
        DataGridView1.Columns(3).ReadOnly = True
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        Dim a As DialogResult
        a = MessageBox.Show("Confirm Set SELECTED as ANSWER KEY???", "", MessageBoxButtons.YesNo, MessageBoxIcon.Question, MessageBoxDefaultButton.Button1)
        If a = Windows.Forms.DialogResult.Yes Then
            For i As Integer = 1 To q_Number
                Dim rows As Integer = i - 1
                If DataGridView1.Rows(rows).Cells(1).Value = 0 Then
                    DataGridView1.Rows(rows).Cells(2).Value = 9
                Else
                    DataGridView1.Rows(rows).Cells(2).Value = DataGridView1.Rows(rows).Cells(1).Value
                End If
            Next
        End If
    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        Dim points As Integer = 0
        For i As Integer = 1 To q_Number
            Dim rows As Integer = i - 1
            If DataGridView1.Rows(rows).Cells(2).Value = DataGridView1.Rows(rows).Cells(1).Value Then
                points = points + 1
                DataGridView1.Rows(rows).Cells(3).Value = 1
            Else
                DataGridView1.Rows(rows).Cells(3).Value = 0
            End If
        Next
        Label11.Text = points
    End Sub
End Class
