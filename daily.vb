Imports System
Imports System.Runtime.InteropServices

Namespace HoroCalc

	Public Class Daily
		Public Property Basics As CalcManager.BasicAstroFactors

		Public Sub New(basics As CalcManager.BasicAstroFactors)
			Me.Basics = basics
		End Sub

		Public Function GetChogadiaSpan(chogNum As Integer) As Double
			Dim num As Double = Functions.TimeToDec(Me.Basics.SunriseTime)
			Dim num2 As Double = Functions.TimeToDec(Me.Basics.SunsetTime)
			Dim num3 As Double
			If chogNum <= 8 Then
				num3 = num2 - num
			Else
				num3 = num + 12.0 - (num2 - 12.0)
			End If
			Return num3 / 8.0
		End Function

		Public Shared Function GetChogadiaName(chogNum As Integer, weekday As DayOfWeek) As CalcManager.Chogadia
			Dim chogadia As CalcManager.Chogadia = CalcManager.Chogadia.Udveg
			If chogNum <= 8 Then
				If weekday = DayOfWeek.Sunday Then
					chogadia = CalcManager.Chogadia.Udveg
				End If
				If weekday = DayOfWeek.Monday Then
					chogadia = CalcManager.Chogadia.Amrit
				End If
				If weekday = DayOfWeek.Tuesday Then
					chogadia = CalcManager.Chogadia.Rog
				End If
				If weekday = DayOfWeek.Wednesday Then
					chogadia = CalcManager.Chogadia.Laabh
				End If
				If weekday = DayOfWeek.Thursday Then
					chogadia = CalcManager.Chogadia.Shubh
				End If
				If weekday = DayOfWeek.Friday Then
					chogadia = CalcManager.Chogadia.Chara
				End If
				If weekday = DayOfWeek.Saturday Then
					chogadia = CalcManager.Chogadia.Kaal
				End If
			Else
				If weekday = DayOfWeek.Sunday Then
					chogadia = CalcManager.Chogadia.Shubh
				End If
				If weekday = DayOfWeek.Monday Then
					chogadia = CalcManager.Chogadia.Chara
				End If
				If weekday = DayOfWeek.Tuesday Then
					chogadia = CalcManager.Chogadia.Kaal
				End If
				If weekday = DayOfWeek.Wednesday Then
					chogadia = CalcManager.Chogadia.Udveg
				End If
				If weekday = DayOfWeek.Thursday Then
					chogadia = CalcManager.Chogadia.Amrit
				End If
				If weekday = DayOfWeek.Friday Then
					chogadia = CalcManager.Chogadia.Rog
				End If
				If weekday = DayOfWeek.Saturday Then
					chogadia = CalcManager.Chogadia.Laabh
				End If
			End If
			If chogNum <= 8 Then
				chogadia = CType(Functions.Expunger(Of CalcManager.Chogadia)(chogadia + chogNum - 1, 7.0), CalcManager.Chogadia)
			ElseIf chogNum > 9 Then
				Dim num As Integer = chogNum - 9
				num *= 5
				chogadia = CType(Functions.Expunger(Of CalcManager.Chogadia)(chogadia + num, 7.0), CalcManager.Chogadia)
			End If
			Return chogadia
		End Function

		Public Function GetChogadiaStart(chogNum As Integer) As Double
			Dim chogadiaSpan As Double = Me.GetChogadiaSpan(chogNum)
			Dim num As Double
			If chogNum <= 8 Then
				num = chogadiaSpan * CDbl(chogNum)
			Else
				num = chogadiaSpan * (CDbl(chogNum) - 8.0)
			End If
			Dim num2 As Double
			If chogNum <= 8 Then
				num2 = num - chogadiaSpan + Functions.TimeToDec(Me.Basics.SunriseTime)
			Else
				num2 = num - chogadiaSpan + Functions.TimeToDec(Me.Basics.SunsetTime)
			End If
			If num2 > 24.0 Then
				num2 -= 24.0
			End If
			Return num2
		End Function

		Public Shared Function GetHoraName(num As Integer, weekday As DayOfWeek) As CalcManager.Hora
			Dim hora As CalcManager.Hora = CalcManager.Hora.Sun
			If num <= 12 Then
				Select Case weekday
					Case DayOfWeek.Sunday
						hora = CalcManager.Hora.Sun
					Case DayOfWeek.Monday
						hora = CalcManager.Hora.Moon
					Case DayOfWeek.Tuesday
						hora = CalcManager.Hora.Mars
					Case DayOfWeek.Wednesday
						hora = CalcManager.Hora.Mercury
					Case DayOfWeek.Thursday
						hora = CalcManager.Hora.Jupiter
					Case DayOfWeek.Friday
						hora = CalcManager.Hora.Venus
					Case DayOfWeek.Saturday
						hora = CalcManager.Hora.Saturn
				End Select
			Else
				Select Case weekday
					Case DayOfWeek.Sunday
						hora = CalcManager.Hora.Jupiter
					Case DayOfWeek.Monday
						hora = CalcManager.Hora.Venus
					Case DayOfWeek.Tuesday
						hora = CalcManager.Hora.Saturn
					Case DayOfWeek.Wednesday
						hora = CalcManager.Hora.Sun
					Case DayOfWeek.Thursday
						hora = CalcManager.Hora.Moon
					Case DayOfWeek.Friday
						hora = CalcManager.Hora.Mars
					Case DayOfWeek.Saturday
						hora = CalcManager.Hora.Mercury
				End Select
			End If
			If num > 12 Then
				num -= 12
			End If
			Return CType(Functions.Expunger(Of Integer)(hora + num - CalcManager.Hora.Sun, 7.0), CalcManager.Hora)
		End Function

		Public Function GetHoraSpan(num As Integer) As Double
			Dim num2 As Double = Functions.TimeToDec(Me.Basics.SunriseTime)
			Dim num3 As Double = Functions.TimeToDec(Me.Basics.SunsetTime)
			Dim num4 As Double
			If num <= 12 Then
				num4 = num3 - num2
			Else
				num4 = num2 + 12.0 - (num3 - 12.0)
			End If
			Return num4 / 12.0
		End Function

		Public Function GetHoraStart(num As Integer) As Double
			Dim horaSpan As Double = Me.GetHoraSpan(num)
			Dim num2 As Double
			If num <= 12 Then
				num2 = horaSpan * CDbl(num)
			Else
				num2 = horaSpan * (CDbl(num) - 12.0)
			End If
			Dim num3 As Double
			If num <= 12 Then
				num3 = num2 - horaSpan + Functions.TimeToDec(Me.Basics.SunriseTime)
			Else
				num3 = num2 - horaSpan + Functions.TimeToDec(Me.Basics.SunsetTime)
			End If
			If num3 > 24.0 Then
				num3 -= 24.0
			End If
			Return num3
		End Function

		Public Function GetRahuKaalStart() As Double
			Dim num As Double = 0.0
			Dim num2 As Double = Functions.TimeToDec(Me.Basics.SunriseTime)
			Dim rahuKaalSpan As Double = Me.GetRahuKaalSpan(num2, Functions.TimeToDec(Me.Basics.SunsetTime))
			Select Case Me.Basics.CalcBase.BirthDetails.GetBirthDate().DayOfWeek
				Case DayOfWeek.Sunday
					num = 8.0
				Case DayOfWeek.Monday
					num = 2.0
				Case DayOfWeek.Tuesday
					num = 7.0
				Case DayOfWeek.Wednesday
					num = 5.0
				Case DayOfWeek.Thursday
					num = 6.0
				Case DayOfWeek.Friday
					num = 4.0
				Case DayOfWeek.Saturday
					num = 3.0
			End Select
			Return rahuKaalSpan * num + num2 - rahuKaalSpan
		End Function

		Public Function GetRahuKaalSpan(sunrise As Double, sunset As Double) As Double
			Return(sunset - sunrise) / 8.0
		End Function
	End Class
End Namespace
