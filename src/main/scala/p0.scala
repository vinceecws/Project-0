import scala.util.parsing.json._
import scala.io.Source._
object p0 {

    val apiKey = "92918be308104905debec438abdf41b0"

    def getGeo(city: String, state: String, limit: Int = 10): Any = {
        val url = s"http://api.openweathermap.org/geo/1.0/direct?q=${city},${state},US&limit=${limit}&appid=${apiKey}"

        val res = JSON.parseFull(fromURL(url).mkString)

        return res
    }

    def main(args: Array[String]): Unit = {
        
    }
}