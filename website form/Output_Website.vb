Module Output_Website
    Public ReadOnly Day_Long() As String = {"Sunday", "Monday", "Tuesday", "Wednesday",
                                           "Thursday", "Friday", "Saturday"}
    Public ReadOnly Day_Short() As String = {"Sun", "Mon", "Tue", "Wed",
                                             "Thu", "Fri", "Sat"}
    Public ReadOnly Month_Name() As String = {"January", "February", "March",
                                              "April", "May", "June",
                                               "July", "August", "September",
                                              "October", "November", "December"}
    Public ReadOnly Name_colors() As String = {"pink", "red", "orange", "yellow", "green",
                                               "blue", "purple", "brown", "gray", "black"}
    Public ReadOnly html_colors() As String = {"#DC3CFC", "#FA0000", "#FA6400", "#FCFE03", "#00C800",
                                               "#0000FF", "#8b05A7", "#734309", "#808080", "#000000"}
    Public Const MAPCONST As String = "gregory"
    Public Const CT_WHITE As String = """#FFFFFF"""
    Public Const CT_bLACK As String = """#000000"""
    Public Const CT_DARK_RED As String = """#920707"""
    Public Const CT_ALI_CEN As String = "align=""center"""
    Public Const CT_TR_HEIGHT As String = "<tr height=""30"">"
    Public Const CT_FONT As String = """courier new"""
    Public Const Number_HEADER As Integer = 9

    Function Make_Calendar(ByVal Which_One As Short) As Boolean
        Dim Itworked As Boolean = True
        Dim Give_Me_Line As String = ""
        Dim Looping As Short = 0
        Dim Month As Short = 0
        Dim Week As Short = 0
        Dim Day As Short = 0
        Dim Do_Calendar As Boolean = False
        Dim Month_Day As Short = 0
        Dim dayweek As Short = JanuaryFirst
        Dim File_Name As String = Where_Directory + Name_colors(Which_One) + "\index.htm"
        Dim Fill_Space As String
        Const CT_headYEAR As String = """#18C354"""
        Try
            Dim Calendar As New System.IO.StreamWriter(File_Name)
            ' ******************************************************* < HTML >
            Calendar.WriteLine("<!DOCTYPE html>")
            ' ******************************************************* < HEADING >
            For Looping = 1 To Number_HEADER
                Calendar.WriteLine(Tophead(Which_One, Looping))
            Next Looping
            ' ******************************************************* < BODY >
            Give_Me_Line = "<body bgcolor=""" + html_colors(Which_One) + """>"
            Calendar.WriteLine(Give_Me_Line)
            ' ******************************************************* < MAPPING >
            For Looping = 1 To 13
                Calendar.WriteLine(Map_Link(Looping))
            Next Looping
            ' ******************************************************* < INTRODUCTION >
            For Looping = 1 To 8
                Calendar.WriteLine(Introduction(Which_One, Looping))
            Next Looping
            ' ******************************************************* < START THE PROCESS >
            Give_Me_Line = "<table border=""3"" bgcolor=" + CT_bLACK + "><tr>"
            Calendar.WriteLine(Give_Me_Line)
            Give_Me_Line = "<td bgcolor=" + CT_WHITE + " " + CT_ALI_CEN + ">"
            Calendar.WriteLine(Give_Me_Line)
            Give_Me_Line = "<b><font color=" + CT_headYEAR + " face=" + CT_FONT + " size=""7"">YEAR: " + CType(What_Year, String) + "</font></b>"
            Calendar.WriteLine(Give_Me_Line)
            Calendar.WriteLine("</td></tr><tr><td>")
            Calendar.WriteLine("<table><tr>")
            ' ******************************************************* < MONTH >
            For Month = 1 To 12
                Calendar.WriteLine("<td>")
                Give_Me_Line = "<table border=""1"" bgcolor=" + CT_WHITE + ">"
                Calendar.WriteLine(Give_Me_Line)
                Calendar.WriteLine("<tr>")
                Give_Me_Line = "<td colspan=""7"" " + CT_ALI_CEN + ">"
                Calendar.WriteLine(Give_Me_Line)
                Give_Me_Line = "<b><font face=" + CT_FONT + " size=""6"">" + Month_Name(Month - 1) + "</font</b></td>"
                Calendar.WriteLine(Give_Me_Line)
                Calendar.WriteLine("</tr><tr>")
                ' ******************************************************* < WEEK HEADING >
                For Day = 0 To 6
                    Give_Me_Line = "<td width=""40"" " + CT_ALI_CEN + ">"
                    Calendar.Write(Give_Me_Line)
                    Give_Me_Line = "<font face=" + CT_FONT + ">" + Day_Short(Day) + "</font></td>"
                    Calendar.WriteLine(Give_Me_Line)
                Next Day
                Calendar.WriteLine("</tr>")
                ' ******************************************************* < FILLING IN WEEKS >
                Month_Day = 0
                Do_Calendar = False
                For Week = 0 To 5
                    Calendar.WriteLine(CT_TR_HEIGHT)
                    For Day = 0 To 6
                        Calendar.Write("<td>")
                        If Month_Day = Month_End_Max(Month, What_Year) Then Do_Calendar = False
                        If Week = 0 Then If dayweek = Day Then Do_Calendar = True
                        If Do_Calendar = True Then
                            Month_Day += 1
                            dayweek += 1
                            If dayweek > 6 Then dayweek = 0
                        End If
                        Fill_Space = "<br>"
                        If Month_Day <= Month_End_Max(Month, What_Year) Then Fill_Space = LTrim(CType(Month_Day, String))
                        If Month_Day = 0 Then Fill_Space = "<br>"
                        If Do_Calendar = False Then Fill_Space = "<br>"
                        Calendar.Write(Fill_Space)
                        Calendar.WriteLine("</td>")
                    Next Day
                    Calendar.WriteLine("</tr>")
                Next Week
                ' ******************************************************* < ENDING THE MONTH >
                Calendar.WriteLine("</table></td>")
                If Month Mod 3 = 0 And Month < 12 Then Calendar.WriteLine("</tr><tr>")
                If Month = 12 Then Calendar.WriteLine("</tr>")
            Next Month

            ' ******************************************************* < ENDING >
            Calendar.WriteLine("</td></tr></table>")
            Calendar.WriteLine("</td></tr></table>")
            ' ******************************************************* < LAST UPDATE >
            For Looping = 1 To 5
                Calendar.WriteLine(Last_Update(Looping))
            Next Looping
            Calendar.WriteLine(Map_Link(13))
            Calendar.WriteLine("</body>")
            Calendar.WriteLine("</html>")
            Calendar.Close()
        Catch
            Itworked = False
        End Try
        Return Itworked
    End Function

    Function Make_Link(ByVal Which_One As Short) As Boolean
        Dim Itworked As Boolean = True
        Dim Write_File_Name As String = Where_Directory + Name_colors(Which_One) + "\index.htm"
        Dim Read_File_Name As String = Where_Directory + Name_colors(Which_One) + ".links"
        Dim Give_Me_Line As String = ""
        Dim Looping As Short = 0
        Dim ColumnQ As Short = 0
        Dim Top_Column As String = ""
        Dim Fill_Me_In As String = ""
        Dim FFF As String = """5"""
        Dim KeepReading As Boolean = True
        Dim Reading_Line As String = ""
        Dim before As String = ""
        Dim During As String = ""
        Dim After As String = ""
        Dim Letter As Char = ""
        Dim Split As Short = 0
        Dim Getout As Boolean = False
        Dim IsitText As Boolean
        Try
            Dim ReadingLink As New System.IO.StreamReader(Read_File_Name)
            Dim MakingLink As New System.IO.StreamWriter(Write_File_Name)
            ColumnQ = ReadingLink.ReadLine()
            ' ******************************************************* < HTML >
            Select Case ColumnQ
                Case 1
                    Top_Column = "width=""100%"" "
                Case 2
                    Top_Column = "width=""50%"" "
                Case 3
                    Top_Column = "width=""33%"" "
                Case 4
                    Top_Column = "width=""25%"" "
                Case 5
                    Top_Column = "width=""20%"" "
            End Select
            MakingLink.WriteLine("<!DOCTYPE html>")
            ' ******************************************************* < HEADING >
            For Looping = 1 To Number_HEADER
                MakingLink.WriteLine(Tophead(Which_One, Looping))
            Next Looping
            ' ******************************************************* < BODY >
            Give_Me_Line = "<body bgcolor=""" + html_colors(Which_One) + """>"
            MakingLink.WriteLine(Give_Me_Line)
            MakingLink.WriteLine("<tr>")
            ' ******************************************************* < MAPPING >
            For Looping = 1 To 13
                MakingLink.WriteLine(Map_Link(Looping))
            Next Looping
            ' ******************************************************* < INTRODUCTION >  
            For Looping = 1 To 8
                MakingLink.WriteLine(Introduction(Which_One, Looping))
            Next Looping
            ' ******************************************************* < ADD THE HEADING TITLE >
            Reading_Line = ReadingLink.ReadLine()
            Fill_Me_In = "<table width=""75%"" bgcolor=" + CT_WHITE + " border=" + FFF + " cellpadding=" + FFF + " cellspacing=" + FFF + " " + CT_ALI_CEN + ">"
            MakingLink.WriteLine(Fill_Me_In)
            Fill_Me_In = "<tr height=""middle""><th " + CT_ALI_CEN + ">"
            MakingLink.WriteLine(Fill_Me_In)
            Split = 0
            before = ""
            During = ""
            After = ""
            Letter = ""
            Do While (Getout = False)
                Split += 1
                Letter = Mid(Reading_Line, Split, 1)
                If Split > 2 Then
                    If Letter = "," Then
                        before = "<font size=""" + Mid(Reading_Line, 3, Split - 3) + """>"
                        During = Right(Reading_Line, (Len(Reading_Line) - Split))
                        After = "</font>"
                        Fill_Me_In = before + During + After
                        Getout = True
                    End If
                End If
                If Split > 255 Then
                    Getout = True
                    Reading_Line = "blank"
                End If
            Loop
            MakingLink.WriteLine(Fill_Me_In)
            MakingLink.WriteLine("</th><tr>")
            MakingLink.WriteLine("</table>")
            ' ******************************************************* < START THE PROCESS >
            Looping = 0
            Fill_Me_In = "<table width=""100%"" bgcolor=" + CT_WHITE + " border=" + FFF + " cellpadding=" + FFF + " cellspacing=" + FFF + ">"
            MakingLink.WriteLine(Fill_Me_In)
            MakingLink.WriteLine("<tr>")
            Reading_Line = ReadingLink.ReadLine()
            While (KeepReading = True)
                Looping += 1
                If Looping <= ColumnQ Then Fill_Me_In = "<td " + Top_Column + CT_ALI_CEN + ">"
                If Looping > ColumnQ Then Fill_Me_In = "<td " + CT_ALI_CEN + ">"
                before = ""
                After = ""
                Letter = ""
                Split = 0
                Getout = False
                IsitText = True
                If Reading_Line = "blank" Then Getout = True
                If Reading_Line = "END" Then Getout = True
                Do While (Getout = False)
                    Split += 1
                    Letter = Mid(Reading_Line, Split, 1)
                    If Split = 1 Then If Letter = "T" Then IsitText = True Else IsitText = False
                    If Split > 2 Then
                        If Letter = "," Then
                            Select Case (IsitText)
                                Case True
                                    before = Mid(Reading_Line, 3, Split - 3)
                                    After = Right(Reading_Line, (Len(Reading_Line) - Split))
                                    Fill_Me_In += "<a href=""http://" + before + """>" + After + "</a>"
                                    Getout = True
                                Case False
                                    before = Mid(Reading_Line, 3, Split - 3)
                                    After = Right(Reading_Line, (Len(Reading_Line) - Split))
                                    Fill_Me_In += "<a href=""http://" + before + """>"
                                    Fill_Me_In += "<img src=""" + "link/" + After + """ width=""115"" height=""50"" />"
                                    Fill_Me_In += "</a>"
                                    Getout = True
                            End Select
                        End If
                    End If
                    If Split > 255 Then
                        Getout = True
                        Reading_Line = "blank"
                        KeepReading = False
                    End If
                Loop
                If Reading_Line = "blank" Then Fill_Me_In += "<br>"
                Fill_Me_In += "</td>"
                MakingLink.WriteLine(Fill_Me_In)
                If Looping Mod ColumnQ = 0 Then MakingLink.WriteLine("</tr><tr>")
                Reading_Line = ReadingLink.ReadLine()
                If Reading_Line = "END" Then KeepReading = False
            End While
            MakingLink.WriteLine("</tr></table>")
            ' ******************************************************* < LAST UPDATE >
            For Looping = 1 To 5
                MakingLink.WriteLine(Last_Update(Looping))
            Next Looping
            MakingLink.WriteLine(Map_Link(13))
            MakingLink.WriteLine("</body>")
            MakingLink.WriteLine("</html>")
            MakingLink.Close()
            ReadingLink.Close()
        Catch
            Itworked = False
        End Try
        Return Itworked
    End Function

    Function Make_Custom(ByVal Which_One As Short) As Boolean
        Dim Itworked As Boolean = False
        Return Itworked
    End Function

	Function Colorcap(What As String) As String
		Dim Change_Me As String
		Change_Me = UCase(Left(What, 1)) + Right(What, (Len(What) - 1))
		Return Change_Me
	End Function

	Function Tophead(ByVal Which_One As Short, ByVal Line_Please As Short) As String
        Dim What_To_Say As String = ""
        Select Case Line_Please
            Case 1
                What_To_Say = "<head>"
            Case 2
                What_To_Say = "<title>"
            Case 3
				What_To_Say = "Gregory Davidson's Website: Color " + Colorcap(Name_colors(Which_One))
			Case 4
                What_To_Say = "</title>"
            Case 5
                What_To_Say = "<meta name=""description"" content=""Gregory Davidson's webpage"">"
            Case 6
                What_To_Say = "<meta name=""author"" content=""Gregory Davidson"">"
            Case 7
                What_To_Say = "<meta name=""keywords"" content=""Gregory,Davidson,deaf,calendar"">"
            Case 8
                What_To_Say = "<meta charset=""UTF-8"">"
            Case 9
                What_To_Say = "</head>"
        End Select
        Return What_To_Say
    End Function

	Function Map_Link(ByVal Line_Please As Short) As String
		Dim LNK As String = ""
		Dim Ending As String = ""
		Dim What_To_Say As String = ""
		Dim Number As String = ""
		Dim xx As Short = 0
		Dim yy As Short = 99
		Select Case TESTING
			Case 0
				LNK = """" + "http://www.gregorydavidson.com/"
				Ending = """"
			Case 1
				LNK = """" + Where_Directory
				Ending = "\index.htm"""
		End Select
		If Line_Please = 1 Then What_To_Say = "<map name=""" + MAPCONST + """>"
		If Line_Please >= 2 And Line_Please <= 11 Then
			xx = 0 + (100 * (Line_Please - 2))
			yy = 99 + (100 * (Line_Please - 2))
			Number = """" + LTrim(Str(xx)) + ",0," + LTrim(Str(yy)) + ",49" + """"
			What_To_Say = "<area shape=""rect"" coords=" + Number + " href=" + LNK + Name_colors(Line_Please - 2) + Ending + ">"
		End If
		If Line_Please = 12 Then What_To_Say = "</map>"
		If Line_Please = 13 Then What_To_Say = "<p><img src=""http://www.gregorydavidson.com/images/colorbar.gif" + """ width=""1000"" height=""50"" border=""0"" usemap=""#" + MAPCONST + """></p>"
		Return What_To_Say
	End Function

	Function Introduction(ByVal Which_One As Short, ByVal Line_Please As Short) As String
        Dim What_To_Say As String = ""
        Const Webcolor = """#21b021"""
        Const WebName = "Gregory Davidson's Website"
        Dim Replace_color = CT_bLACK
        Select Case Line_Please
            Case 1
                What_To_Say = "<table bgcolor=" + CT_WHITE + "><tr><td>"
            Case 2
                What_To_Say = "<b><font color=" + Webcolor + " size=""7"">" + WebName + "</font></b>"
            Case 3
                What_To_Say = "</td><td><table border=""5""><tr>"
            Case 4
                If Which_One > 4 Then Replace_color = CT_WHITE
                If Which_One = 4 Then Replace_color = CT_DARK_RED
                What_To_Say = "<td bgcolor=" + Replace_color + ">"
            Case 5
				What_To_Say = "<b><font color=""" + html_colors(Which_One) + """ size=""6"">" + Colorcap(Name_colors(Which_One)) + " Version</font></b>"
			Case 6 To 7
                What_To_Say = "</td></tr></table>"
            Case 8
                What_To_Say = "<br>"
        End Select
        Return What_To_Say
    End Function

    Function Last_Update(ByVal Line_Please As Short) As String
        Dim What_To_Say As String = ""
        Dim Ending_color As String = """#045A04"""
        Dim Today_Date As String = Format(DateTime.Today, "MMMM dd, yyyy")
        Select Case Line_Please
            Case 1
                What_To_Say = "<br>"
            Case 2
                What_To_Say = "<table bgcolor =" + CT_WHITE + " border=""3""><tr><td>"
            Case 3
                What_To_Say = "<b><font color=" + Ending_color + ">Last Update: " + Today_Date + "</font></b>"
            Case 4
                What_To_Say = "</td></tr></table>"
            Case 5
                What_To_Say = "<br>"
        End Select
        Return What_To_Say
    End Function

	Function Month_End_Max(ByVal Which_One As Short, ByVal Year As Short) As Short
		Dim Monthly As Short = 0
		If Which_One = 1 Then Monthly = 31
		If Which_One = 2 Then
			If Year Mod 4 = 0 Then Monthly = 29 Else Monthly = 28
		End If
		If Which_One = 3 Then Monthly = 31
		If Which_One = 4 Then Monthly = 30
		If Which_One = 5 Then Monthly = 31
		If Which_One = 6 Then Monthly = 30
		If Which_One = 7 Then Monthly = 31
		If Which_One = 8 Then Monthly = 31
		If Which_One = 9 Then Monthly = 30
		If Which_One = 10 Then Monthly = 31
		If Which_One = 11 Then Monthly = 30
		If Which_One = 12 Then Monthly = 31
		Return Monthly
	End Function

	Function BIGandLittle(ByVal Which_One) As Boolean
		Const CT_TR_HEIGHT_ALIGN As String = "<tr height=""80"" align=""right"" valign=""bottom"" bgcolor=" + CT_WHITE + ">"
		Const CT_TD_WIDTH_ALIGN As String = "<td width=""120"" align=""center""><font face=""courier new""><b>"
		Const CT_TD_NUMBER As String = "<b><font size=""5"" face=""courier new"">"
		Dim Itworked As Boolean = True
		Dim Give_Me_Line As String = ""
		Dim Looping As Short = 0
		Dim File_Name As String = Where_Directory + Name_colors(Which_One) + "\index.htm"
		Dim Fill_Space As String = ""
		Dim Month As Short
		Dim Year As Short
		Dim Day As Short
		Dim Week As Short
		Dim Month_Day As Short
		Dim Month_END As Short
		Dim dayweek As Short
		Dim Do_Calendar As Boolean
		Dim Before_Month(3) As Short
		Dim Current_Month(3) As Short
		Dim After_Month(3) As Short
		' ************************************************************************************************************
		' Month(0) = Month | Month(1) = Year | Month(2) = First Day of the Month | Month(3) = How many Days in a month
		Current_Month(0) = What_Month
		Current_Month(1) = What_Year
		Current_Month(2) = MonthFirst
		dayweek = Current_Month(2)
		Current_Month(3) = Month_End_Max(Current_Month(0), Current_Month(1))
		Select Case What_Month
			Case 1
				Before_Month(0) = 12
				Before_Month(1) = What_Year - 1
				After_Month(0) = What_Month + 1
				After_Month(1) = What_Year
			Case 2 To 11
				Before_Month(0) = What_Month - 1
				Before_Month(1) = What_Year
				After_Month(0) = What_Month + 1
				After_Month(1) = What_Year
			Case 12
				Before_Month(0) = What_Month - 1
				Before_Month(1) = What_Year
				After_Month(0) = 1
				After_Month(1) = What_Year + 1
		End Select
		Dim beforedate As Date = DateValue(Before_Month(0).ToString & "/1/" & Before_Month(1).ToString)
		Before_Month(2) = Val(beforedate.DayOfWeek)
		Dim afterdate As Date = DateValue(After_Month(0).ToString & "/1/" & After_Month(1).ToString)
		After_Month(2) = Val(afterdate.DayOfWeek)
		Before_Month(3) = Month_End_Max(Before_Month(0), Before_Month(1))
		After_Month(3) = Month_End_Max(After_Month(0), After_Month(1))
		' ************************************************************************************************************
		Try
			Dim Calendar As New System.IO.StreamWriter(File_Name)
			' ******************************************************* < HTML >
			Calendar.WriteLine("<!DOCTYPE html>")
			' ******************************************************* < HEADING >
			For Looping = 1 To Number_HEADER
				Calendar.WriteLine(Tophead(Which_One, Looping))
			Next Looping
			' ******************************************************* < BODY >
			Give_Me_Line = "<body bgcolor=""" + html_colors(Which_One) + """>"
			Calendar.WriteLine(Give_Me_Line)
			' ******************************************************* < MAPPING >
			For Looping = 1 To 13
				Calendar.WriteLine(Map_Link(Looping))
			Next Looping
			' ******************************************************* < INTRODUCTION >
			For Looping = 1 To 8
				Calendar.WriteLine(Introduction(Which_One, Looping))
			Next Looping
			' ******************************************************* < START THE PROCESS >
			Give_Me_Line = "<table border=""3"" bgcolor =" + CT_bLACK + CT_ALI_CEN + "><tr>"
			Calendar.WriteLine(Give_Me_Line)
			Give_Me_Line = "<td bgcolor=" + CT_WHITE + " " + CT_ALI_CEN + "colspan=""7"">"
			Calendar.WriteLine(Give_Me_Line)
			' ******************************************************* < MONTH and YEAR >
			Give_Me_Line = "<b><font color=" + CT_bLACK + " face=" + CT_FONT + " size=""7"">" + Month_Name(Current_Month(0) - 1) + ", " + Current_Month(1).ToString + "</b></font></td></tr>"
			Calendar.WriteLine(Give_Me_Line)
			Give_Me_Line = "<tr bgcolor=" + CT_WHITE + ">"
			Calendar.WriteLine(Give_Me_Line)
			' ******************************************************* < WEEK HEADING >
			For Day = 0 To 6
				Give_Me_Line = CT_TD_WIDTH_ALIGN + Day_Long(Day) + "</font></b></td>"
				Calendar.Write(Give_Me_Line)
			Next Day
			Calendar.WriteLine("</tr>")
			' ******************************************************* < FILLING IN WEEKS >
			Month_Day = 0
			Do_Calendar = False
			For Week = 0 To 5
				Calendar.WriteLine(CT_TR_HEIGHT_ALIGN)
				For Day = 0 To 6
					Calendar.Write("<td>")
					If Month_Day = Current_Month(3) Then Do_Calendar = False
					If Week = 0 Then If Current_Month(2) = Day Then Do_Calendar = True
					If Do_Calendar = True Then
						Month_Day += 1
						dayweek += 1
						If dayweek > 6 Then dayweek = 0
					End If
					Fill_Space = "<br>"
					If Month_Day <= Current_Month(3) Then
						Fill_Space = CT_TD_NUMBER + LTrim(CType(Month_Day, String)) + "</font></b>"
					End If
					If Month_Day = 0 Then Fill_Space = "<br>"
					If Do_Calendar = False Then Fill_Space = "<br>"
					Calendar.Write(Fill_Space)
					Calendar.WriteLine("</td>")
				Next Day
				Calendar.WriteLine("</tr>")
			Next Week
			' ******************************************************* < ENDING THE CURRENT MONTH >
			Calendar.WriteLine("</table>")
			' ******************************************************* < NOW MAKING STARTING HEAD OF BEFORE AND AFTER MONTHS>
			Give_Me_Line = "<table border=""3"" align=""center"" bgcolor=" + CT_bLACK + "><tr>"
			Calendar.WriteLine(Give_Me_Line)
			'******************************************************** < NOW MAKING BEFORE AND AFTER MONTHS >
			For Looping = 1 To 2
				Give_Me_Line = "<td>"
				Calendar.WriteLine(Give_Me_Line)
				Give_Me_Line = "<table border=""1"" bgcolor=" + CT_WHITE + "><tr>"
				Calendar.WriteLine(Give_Me_Line)
				Select Case Looping
					Case 1
						Month = Before_Month(0)
						Year = Before_Month(1)
						dayweek = Before_Month(2)
						Month_END = Before_Month(3)
					Case 2
						Month = After_Month(0)
						Year = After_Month(1)
						dayweek = After_Month(2)
						Month_END = After_Month(3)
				End Select
				' *************************************************** < MONTH AND YEAR >
				Give_Me_Line = "<td colspan=""7"" " + CT_ALI_CEN + ">"
				Calendar.WriteLine(Give_Me_Line)
				Give_Me_Line = "<b><font face=" + CT_FONT + " size=""6"">" + Month_Name(Month - 1) + ", " + Year.ToString + "</font</b></td>"
				Calendar.WriteLine(Give_Me_Line)
				Calendar.WriteLine("</tr><tr>")
				' *************************************************** < WEEK HEADING >
				For Day = 0 To 6
					Give_Me_Line = "<td width=""40"" " + CT_ALI_CEN + ">"
					Calendar.Write(Give_Me_Line)
					Give_Me_Line = "<font face=" + CT_FONT + ">" + Day_Short(Day) + "</font></td>"
					Calendar.Write(Give_Me_Line)
				Next Day
				Calendar.WriteLine("</tr>")
				' *************************************************** < FILLING IN WEEKS >
				Month_Day = 0
				Do_Calendar = False
				For Week = 0 To 5
					Calendar.WriteLine(CT_TR_HEIGHT)
					For Day = 0 To 6
						Calendar.Write("<td>")
						If Month_Day = Month_END Then Do_Calendar = False
						If Week = 0 Then If dayweek = Day Then Do_Calendar = True
						If Do_Calendar = True Then
							Month_Day += 1
							dayweek += 1
							If dayweek > 6 Then dayweek = 0
						End If
						Fill_Space = "<br>"
						If Month_Day <= Month_END Then Fill_Space = LTrim(CType(Month_Day, String))
						If Month_Day = 0 Then Fill_Space = "<br>"
						If Do_Calendar = False Then Fill_Space = "<br>"
						Calendar.Write(Fill_Space)
						Calendar.WriteLine("</td>")
					Next Day
					Calendar.WriteLine("</tr>")
				Next Week
				' *************************************************** < ENDING THE MONTH >
				Give_Me_Line = "</td></tr></table>"
				Calendar.WriteLine(Give_Me_Line)
			Next Looping
			' ******************************************************* < ENDING >
			Calendar.WriteLine("</td></tr></table>")
			Calendar.WriteLine("</td></tr></table>")
			' ******************************************************* < LAST UPDATE >
			For Looping = 1 To 5
				Calendar.WriteLine(Last_Update(Looping))
			Next Looping
			Calendar.WriteLine(Map_Link(13))
			Calendar.WriteLine("</body>")
			Calendar.WriteLine("</html>")
			Calendar.Close()
		Catch
			Itworked = False
		End Try
		Return Itworked
	End Function
End Module
