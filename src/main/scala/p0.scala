import scala.util.parsing.json._
import scala.io.Source._
import Console._
import java.text.SimpleDateFormat
import java.util.Date

class Weather(weather: Map[String, Any], freq: String) {

    val weather_icons_dir = "C:/Users/vincey/Desktop/Revature/P0/src/graphic/weather_icons/"

    val frequency = freq
    val id = weather("id").asInstanceOf[Double].toInt
    val main = weather("main").asInstanceOf[String]
    val description = weather("description").asInstanceOf[String]
    val icon = weather("icon").asInstanceOf[String]

    def printWeatherIcon(code: String) = {
        val lines = fromFile(weather_icons_dir + s"${code}.txt")
        println(lines.mkString)
        lines.close
    }

    def printReport(withIcon: Boolean=true) = {
        if (withIcon) {
            printWeatherIcon(icon)
        }

        println(s"Weather: ${main}")
        println(s"${description}")
    }
}

class Current(current: Map[String, Any], city: String, state: String, timezone_offset: Int) {

    val dt = current("dt").asInstanceOf[Double].toLong
    val sunrise = current("sunrise").asInstanceOf[Double].toInt
    val sunset = current("sunset").asInstanceOf[Double].toInt
    val temp = current("temp").asInstanceOf[Double].toFloat
    val feels_like = current("feels_like").asInstanceOf[Double].toFloat
    val pressure = current("pressure").asInstanceOf[Double].toFloat
    val humidity = current("humidity").asInstanceOf[Double].toFloat
    val dew_point = current("dew_point").asInstanceOf[Double].toFloat
    val uvi = current("uvi").asInstanceOf[Double].toFloat
    val clouds = current("clouds").asInstanceOf[Double].toFloat
    val visibility = current("visibility").asInstanceOf[Double].toFloat
    val wind_speed = current("wind_speed").asInstanceOf[Double].toFloat
    val wind_deg = current("wind_deg").asInstanceOf[Double].toFloat
    val weather = new Weather(current("weather").asInstanceOf[List[Map[String, Any]]](0), "current")
    val rain = current.getOrElse("rain", null)
    val snow = current.getOrElse("snow", null)

    def printReport() = {
        val date = new SimpleDateFormat("EEE, d MMM yyyy, h:mm a").format(new Date ((dt - timezone_offset) * 1000))
        println(date)
        println(s"${city}, ${state} \n")

        println("Current Weather: ")
        weather.printReport(withIcon=false)
        println(s"Temperature: ${temp}")
        println(s"Feels Like: ${feels_like}")
        println(s"Visibility: ${visibility}")
        println(s"Humidity: ${humidity}\n\n")
    }
}

// Minutely data forecast for the next 60 minutes
class Minutely(minutely: List[Map[String, Any]], city: String, state: String, dt_now: Long, timezone_offset: Int) {

    class Minute(minute: Map[String, Any], ind: Int) {

        val index = ind
        val dt = minute("dt").asInstanceOf[Double].toLong
        val precipitation = minute("precipitation").asInstanceOf[Double].toFloat
    }

    val _data = minutely.zipWithIndex.map({case (ele, ind) => new Minute(ele, ind)})

    def atMinute(minute: Int): Minute = {
        return _data(minute)
    }

    def printReport() = {
        val date = new SimpleDateFormat("EEE, d MMM yyyy").format(new Date ((dt_now + timezone_offset) * 1000))
        println(date)
        println(s"${city}, ${state} \n")

        println("Precipitation for the next hour: ")
        for (min <- _data) {
            println(s"${new SimpleDateFormat("h:mm a").format(new Date ((min.dt + timezone_offset) * 1000))} ${min.precipitation}\n")
        }
    }
}

// Hourly data forecast for the next 24 hours
class Hourly(hourly: List[Map[String, Any]], city: String, state: String, dt_now: Long, timezone_offset: Int) {

    class Hour(hour: Map[String, Any], ind: Int) {

        val index = ind
        val dt = hour("dt").asInstanceOf[Double].toLong
        val temp = hour("temp").asInstanceOf[Double].toFloat
        val feels_like = hour("feels_like").asInstanceOf[Double].toFloat
        val pressure = hour("pressure").asInstanceOf[Double].toFloat
        val humidity = hour("humidity").asInstanceOf[Double].toFloat
        val dew_point = hour("dew_point").asInstanceOf[Double].toFloat
        val uvi = hour("uvi").asInstanceOf[Double].toFloat
        val clouds = hour("clouds").asInstanceOf[Double].toFloat
        val visibility = hour("visibility").asInstanceOf[Double].toFloat
        val wind_speed = hour("wind_speed").asInstanceOf[Double].toFloat
        val wind_deg = hour("wind_deg").asInstanceOf[Double].toFloat
        val wind_gust = hour("wind_gust").asInstanceOf[Double].toFloat
        val weather = new Weather(hour("weather").asInstanceOf[List[Map[String, Any]]](0), "hourly")
        val pop = hour("pop").asInstanceOf[Double].toFloat
        val rain = hour.getOrElse("rain", null)
        val snow = hour.getOrElse("snow", null)
    }

    val _data = hourly.zipWithIndex.map({case (ele, ind) => new Hour(ele, ind)})

    def atHour(hour: Int): Hour = {
        return _data(hour)
    }

    def printReport() = {
        val date = new SimpleDateFormat("h:mm a").format(new Date ((dt_now + timezone_offset) * 1000))
        println(date)
        println(s"${city}, ${state} \n")
        println("Weather for the next 24 hours: ")

        println("Now")
        atHour(0).weather.printReport(withIcon=false)
        println(s"Temperature: ${atHour(0).temp}\n\n")

        for (hour <- _data.slice(1, 24)) {
            println(s"${new SimpleDateFormat("h a").format(new Date ((hour.dt + timezone_offset) * 1000))}")
            hour.weather.printReport(withIcon=false)
            println(s"Temperature: ${hour.temp}\n\n")
        }
    }
}

// Daily data forecast for the next 7 days
class Daily(daily: List[Map[String, Any]], city: String, state: String, dt_now: Long, timezone_offset: Int) {

    class Day(day: Map[String, Any], ind: Int) {

        val index = ind
        val dt = day("dt").asInstanceOf[Double].toLong
        val temp = day("temp").asInstanceOf[Map[String, Double]]
        val feels_like = day("feels_like")
        val pressure = day("pressure").asInstanceOf[Double].toFloat
        val humidity = day("humidity").asInstanceOf[Double].toFloat
        val dew_point = day("dew_point").asInstanceOf[Double].toFloat
        val wind_speed = day("wind_speed").asInstanceOf[Double].toFloat
        val wind_deg = day("wind_deg").asInstanceOf[Double].toFloat
        val wind_gust = day("wind_gust").asInstanceOf[Double].toFloat
        val weather = new Weather(day("weather").asInstanceOf[List[Map[String, Any]]](0), "daily")
        val clouds = day("clouds").asInstanceOf[Double].toFloat
        val pop = day("pop").asInstanceOf[Double].toFloat
        val rain = day.getOrElse("rain", null)
        val snow = day.getOrElse("snow", null)
        val uvi = day("uvi").asInstanceOf[Double].toFloat
    }

    val _data = daily.zipWithIndex.map({case (ele, ind) => new Day(ele, ind)})

    def atDay(day: Int): Day = {
        return _data(day)
    }

    def printReport() = {
        val date = new SimpleDateFormat("EEE, d MMM yyyy, h:mm a").format(new Date ((dt_now + timezone_offset) * 1000))
        println(date)
        println(s"${city}, ${state} \n")

        println("Weather for the next 7 days: ")
        for (day <- _data) {
            val date = new SimpleDateFormat("EEE, d MMM yyyy").format(new Date ((day.dt + timezone_offset) * 1000))
            println(date)
            day.weather.printReport(withIcon=false)
            println("Temperature: ")
            println(s"Morning: ${day.temp("morn")} -> Day: ${day.temp("day")} -> Evening: ${day.temp("eve")} -> Night: ${day.temp("night")}")
            println(s"Lowest: ${day.temp("min")}, Highest: ${day.temp("max")}\n\n")
        }
    }
}

// Current weather alerts, if available
class Alerts(alerts: List[Map[String, Any]], city: String, state: String, dt_now: Long, timezone_offset: Int) {

    class Alert(alert: Map[String, Any], ind: Int) {

        val index = ind
        val sender_name = alert("sender_name").asInstanceOf[String]
        val event = alert("event").asInstanceOf[String]
        val start = alert("start").asInstanceOf[Double].toInt
        val end = alert("end").asInstanceOf[Double].toInt
        val description = alert("description").asInstanceOf[String]
    }

    val _data = alerts.zipWithIndex.map({case (ele, ind) => new Alert(ele, ind)})

    def atInd(ind: Int): Alert = {
        return _data(ind)
    }
}

class WeatherData(weatherRes: Map[String, Any], city: String, state: String) {

    val lat_lon = (weatherRes("lat").asInstanceOf[Double].toFloat, weatherRes("lon").asInstanceOf[Double].toFloat)
    val timezone = weatherRes("timezone").asInstanceOf[String]
    val timezone_offset = weatherRes("timezone_offset").asInstanceOf[Double].toInt
    val current = new Current(weatherRes("current").asInstanceOf[Map[String, Any]], city, state, timezone_offset)
    val minutely = new Minutely(weatherRes("minutely").asInstanceOf[List[Map[String, Any]]], city, state, current.dt, timezone_offset)
    val hourly = new Hourly(weatherRes("hourly").asInstanceOf[List[Map[String, Any]]], city, state, current.dt, timezone_offset)
    val daily = new Daily(weatherRes("daily").asInstanceOf[List[Map[String, Any]]], city, state, current.dt, timezone_offset)
    var alerts = weatherRes.getOrElse("alerts", null)
    if (alerts != null) {
        alerts = new Alerts(weatherRes("alerts").asInstanceOf[List[Map[String, Any]]], city, state, current.dt, timezone_offset)
    }
}

object p0 {

    val apiKey = "92918be308104905debec438abdf41b0"
    val excludeOpts = Set("current", "minutely", "hourly", "daily", "alerts")
    val units = "imperial"

    def getGeo(city: String, state: String, limit: Int = 10): Map[String, String] = {
        val url = s"http://api.openweathermap.org/geo/1.0/direct?q=${city},${state},US&limit=${limit}&appid=${apiKey}"
        
        val res = JSON.parseFull(fromURL(url).mkString) match {
            case Some(res) => res.asInstanceOf[List[Map[String, String]]]
            case _ => List()
        }

        if (res.length == 0) return Map[String, String]()

        val coordinates = res.find(x => city.equalsIgnoreCase(x.getOrElse("name", "")) && state.equalsIgnoreCase(x.getOrElse("state", "")) && "us".equalsIgnoreCase(x.getOrElse("country", ""))) match {
            case Some(place) => {
                val res = place.asInstanceOf[Map[String, String]]
                return Map("lon" -> res("lon"), "lat" -> res("lat"))
            }
            case None => Map[String, String]()
        }

        return coordinates
    }

    def getWeather(lat_lon: Map[String, String], exclude: String = null): Map[String, Any] = {
        if (exclude != null && !excludeOpts(exclude)) throw new IllegalArgumentException("Invalid argument for arg except")

        val url = s"https://api.openweathermap.org/data/2.5/onecall?lat=${lat_lon("lat")}&lon=${lat_lon("lon")}&exclude=${exclude}&appid=${apiKey}&units=${units}"
        val res = JSON.parseFull(fromURL(url).mkString) match {
            case Some(res) => res.asInstanceOf[Map[String, Any]]
            case _ => Map[String, Any]()
        }

        return res
    }

    def extractInput(input: String): (String, String) = {
        val reg = "[A-Za-z -]+,[A-Za-z ]+".r
        val res = reg.findFirstIn(input) match {
            case Some(res) => res.asInstanceOf[List[String]]
            case _ => List[String](",")
        }

        val cityState = res(0).split(",")

        return (cityState(0), cityState(1))
    }

    def main(args: Array[String]): Unit = {
        println("")
        // val place = readLine(s"${BOLD}Which city's weather report would you like to see? Input as: city, state ${RESET}")
        // val cityState = extractInput(place)
        // val lat_lon = getGeo(cityState._1, cityState._2)
        val lat_lon = Map("lon" -> "-73.7976", "lat" -> "40.7498")
        val weatherData = new WeatherData(getWeather(lat_lon), "Queens", "NY")
        weatherData.current.printReport()
        weatherData.minutely.printReport()
        weatherData.hourly.printReport()
        weatherData.daily.printReport()
    }
}