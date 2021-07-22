# Project-0 
## Weather App

This app was built entirely on Scala 2.13 and compiles with sbt. It uses the OpenWeather API's One-Call API endpoint to get weather data from any U.S. city. To run it, ensure that your local machine has Scala 2.13 and sbt installed. 

After cloning the repository, navigate to the project's root directory (i.e. where build.sbt is located). Get your API key from OpenWeather and replace with it the value to the variable apiKey in p0's main() method.
Then, do `run sbt` on a shell.

### Functionalities
1) Reports current weather, temperature & other meteorological data for the requested city.
2) Plots a scatter graph of the 60-minute forecast of precipitation in the city.
3) Plots a scatter graph of the hourly forecast of temperature in the city for the next 24 hours.
4) Displays the weather forecast of the city for the next 7 days.

