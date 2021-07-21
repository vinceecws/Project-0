import scala.util.parsing.json._
import scala.math.BigDecimal
import scala.io.Source._
import Console._
import java.text.SimpleDateFormat
import java.util.Date
import java.util.TimeZone

class Weather(weather: Map[String, Any], freq: String) {

    val weather_icons_dir = "C:/Users/vincey/Desktop/Revature/P0/src/graphic/weather_icons/"

    val frequency = freq
    val id = weather("id").asInstanceOf[Double].toInt
    val main = weather("main").asInstanceOf[String]
    val description = weather("description").asInstanceOf[String]
    val icon = weather("icon").asInstanceOf[String]

    def printWeatherIcon(code: String) = {
        // val lines = fromFile(weather_icons_dir + s"${code}.txt")
        val lines = fromFile(weather_icons_dir + "02d.txt")("UTF-8")
        println(lines.mkString)
        lines.close
    }

    def printReport(withIcon: Boolean=true) = {
        // if (withIcon) {
        //     printWeatherIcon(icon)
        // }

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
        val date = new SimpleDateFormat("EEE, d MMM yyyy, h:mm a").format(new Date ((dt + timezone_offset) * 1000))
        println(date)
        println(s"${city}, ${state} \n")

        println("Current Weather: ")
        weather.printReport(withIcon=true)
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

    def plotData() = {
        println(s"${BOLD}Minutely Precipitation for the Next 60 minutes:${RESET}")
        WeatherData.plot_time_data(_data.map(x => s"${new SimpleDateFormat("h:mm a").format(new Date ((x.dt + timezone_offset) * 1000))}"), _data.map(x => x.precipitation))
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

    def plotData() = {
        println(s"${BOLD}Hourly Temperature for the Next 24 hours:${RESET}")
        WeatherData.plot_time_data(_data.slice(0, 24).map(x => s"${new SimpleDateFormat("h a").format(new Date ((x.dt + timezone_offset) * 1000))}"), _data.slice(0, 24).map(x => x.temp))
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

        println("Weather for the next 7 days: \n\n")

        WeatherData.print_horizontal(_data.slice(0, 7).map(day => Map[String, String](
            "Date" -> s"${new SimpleDateFormat("EEE, d MMM").format(new Date ((day.dt + timezone_offset) * 1000))}",
            "Weather" -> s"${day.weather.main}",
            "Desc" -> s"${day.weather.description}",
            "Temp. High" -> s"${day.temp("max")}",
            "Temp. Low" -> s"${day.temp("min")}"
        )), List("Date", "Weather", "Desc", "Temp. High", "Temp. Low"))
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

object WeatherData {

    def print_horizontal(data: List[Map[String, String]], rows: List[String], margin:Int=3): Unit = {

        var max_width = 0
        var str = ""
        val buffer = Array.ofDim[String](rows.length, data.length) // Keys as Rows, Elements as Columns

        for ((key, i) <- rows.zipWithIndex) {
            for ((ele, j) <- data.zipWithIndex) {
                buffer(i)(j) = s"${key}: ${ele(key)}"
                if (buffer(i)(j).length > max_width)
                    max_width = buffer(i)(j).length
            }
        }

        max_width += 2

        for (i <- 0 to buffer.size - 1) {
            Range(0, margin).map(x => print(" "))
            for (j <- 0 to buffer(0).size - 1) {
                print(buffer(i)(j))
                Range(buffer(i)(j).length, max_width).map(x => print(" "))
            }
            Range(0, margin).map(x => print(" "))
            print("\n")
        }
    }

    def plot_time_data(times: List[String], values: List[Float], x_res: Int=100, y_res: Int=20, x_offset: Int=10, y_offset: Int=4): Unit = {
        val max = values.max
        val min = values.min
        val mean = values.sum / values.length

        val y_bin_size = ((max - min) / (y_res - 1).toFloat)
        val y_bins = values.map(x => ((x - min) / y_bin_size).toInt)

        val x_bin_size = (times.length / x_res.toFloat)
        val x_bins = Range(0, times.length).map(x => (x / x_bin_size).round.toInt).toList

        val mat = Array.ofDim[Boolean](y_res, x_res) // Array of Y * X, where Y are rows, X are columns
        (x_bins, y_bins).zipped.foreach{ (x, y) => mat(y_res - 1 - y)(x) = true} // Scatter points

        val x_tick_ind = (0, (x_res / 2), x_res - 1)
        val y_tick_ind = (0, (y_res / 2), y_res - 1)

        Range(0, y_offset).foreach(x => print("\n"))
        var y_tick = ""

        //FIRST Y-TICK
        y_tick = s"${BigDecimal(max).setScale(2, BigDecimal.RoundingMode.HALF_UP).toFloat}"
        Range(0, x_offset - y_tick.length - 1).foreach(x => print(" "))
        print(y_tick + " |")

        for (j <- 0 to x_res - 1) {
            if (mat(y_tick_ind._1)(j)) {
                print("X")
            }
            else {
                print(" ")
            }
        }
        print("\n")

        for (i <- 1 to y_tick_ind._2 - 1) {
            Range(0, x_offset).foreach(x => print(" "))
            print("|")
            for (j <- 0 to x_res - 1) {
                if (mat(i)(j)) {
                    print("X")
                }
                else {
                    print(" ")
                }
            }
            print("\n")
        }

        //SECOND Y-TICK
        y_tick = s"${BigDecimal(mean).setScale(2, BigDecimal.RoundingMode.HALF_UP).toFloat}"
        Range(0, x_offset - y_tick.length - 1).foreach(x => print(" "))
        print(y_tick + " |")

        for (j <- 0 to x_res - 1) {
            if (mat(y_tick_ind._2)(j)) {
                print("X")
            }
            else {
                print(" ")
            }
        }
        print("\n")

        for (i <- (y_tick_ind._2 + 1) to y_res - 2) {
            Range(0, x_offset).foreach(x => print(" "))
            print("|")
            for (j <- 0 to x_res - 1) {
                if (mat(i)(j)) {
                    print("X")
                }
                else {
                    print(" ")
                }
            }
            print("\n")
        }

        //THIRD Y-TICK
        y_tick = s"${BigDecimal(min).setScale(2, BigDecimal.RoundingMode.HALF_UP).toFloat}"
        Range(0, x_offset - y_tick.length - 1).foreach(x => print(" "))
        print(y_tick + " |")

        for (j <- 0 to x_res - 1) {
            if (mat(y_tick_ind._3)(j)) {
                print("X")
            }
            else {
                print(" ")
            }
        }
        print("\n")

        Range(0, x_offset).foreach(x => print(" "))
        Range(0, x_res).foreach(x => print("_"))
        print("\n")

        var x_tick = ""
        Range(0, x_offset).foreach(x => print(" "))
        var i = 0
        while (i < x_res) {
            if (i == x_tick_ind._1) {
                x_tick = s"${times(0)}"
                i += x_tick.length
                print(x_tick)
            }
            else if (i == x_tick_ind._2) {
                x_tick = s"${times(times.length / 2)}"
                i += x_tick.length
                print(x_tick)
            }
            else if (i == x_tick_ind._3) {
                x_tick = s"${times(times.length - 1)}"
                i += x_tick.length
                print(x_tick)
            }
            else {
                print(" ")
                i += 1
            }
        }

        Range(0, y_offset).foreach(x => print("\n"))
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

    TimeZone.setDefault(TimeZone.getTimeZone("UTC"))
    val apiKey = "92918be308104905debec438abdf41b0"
    val excludeOpts = Set("current", "minutely", "hourly", "daily", "alerts")
    val units = "imperial"
    val graphicDir = "C:/Users/vincey/Desktop/Revature/P0/src/graphic/"

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
                return Map("lon" -> res("lon"), "lat" -> res("lat"), "city" -> res("name"), "state" -> res("state"))
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
        // Match like city,state (ignoring subsequent inputs)
        val reg = "[A-Za-z -]+,[A-Za-z ]+".r 
        val res = reg.findFirstIn(input) match {
            case Some(res) => res.asInstanceOf[String]
            case _ => " , "
        }

        // Trim leading & trailing whitespace, limit intermediate whitespaces to 1
        val cityState = res.split(",").map(x => x.trim().replaceAll(" +", " "))

        return (cityState(0), cityState(1))
    }

    def printBanner() = {
        val lines = fromFile(graphicDir + "home_banner.txt")
        println(lines.mkString)
        println("\n\n\n\n")
        lines.close
    }

    def clear() = {
        print("\u001b[2J")
    }

    def clearAndPrintBanner() = {
        clear()
        printBanner()
    }

    def main(args: Array[String]): Unit = {
        while (true) {
            clearAndPrintBanner()

            var place = ""
            var lat_lon = Map[String, String]()
            var cityState = ("", "")
            do  {
                place = readLine(s"${BOLD}Which city's weather would you like to see? Input as: city, state ${RESET}")
                cityState = extractInput(place)
                lat_lon = getGeo(cityState._1, cityState._2)

                if (!lat_lon.contains("lat") || !lat_lon.contains("lon")) {
                    println("\n There does not seem to be any good matches to your search. Try something like: Queens, NY")
                }

            } while (!lat_lon.contains("lat") || !lat_lon.contains("lon"))

            var weatherData = new WeatherData(getWeather(lat_lon), lat_lon("city"), lat_lon("state"))
            
            var selection = -1
            do {
                clearAndPrintBanner()
                weatherData.current.printReport()

                println(s"${BOLD}Check out one of the forecasts: ${RESET}")
                println("1) Minutely-precipitation for the next 60-minutes")
                println("2) Hourly-weather for the next 24-hours")
                println("3) Daily-weather for the next 7-days")
                println("Or, 4) Try a different city")

                selection = readLine(s"${BOLD}Your selection: ${RESET}") match {

                    case "1" => 
                        weatherData.minutely.plotData()
                        1
                    case "2" => 
                        weatherData.hourly.plotData()
                        2
                    case "3" =>
                        weatherData.daily.printReport()
                        3
                    case "4" => 4
                    case default => -1
                }
                
                if (selection != 4) {
                    if (selection == -1) {
                        println(s"\n${BOLD}Invalid selection. ${RESET}")
                    }
                    println(s"\n${BOLD}Press ENTER to go back: ${RESET}")

                    try {
                        readChar()
                    }
                    catch {
                        case e: StringIndexOutOfBoundsException => //Ignore, even if user just presses "Enter"
                    }
                }

            } while (selection != 4)
        }
    }
}