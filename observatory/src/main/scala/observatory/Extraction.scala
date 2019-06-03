package observatory

import java.time.LocalDate

import org.apache.log4j.{Level, Logger}
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.types.{DoubleType, IntegerType, StructField, StructType}

/**
  * 1st milestone: data extraction
  */
object Extraction {

  val spark: SparkSession = SparkSession.builder().appName("observatory").config("spark.master", "local").getOrCreate()

  import spark.implicits._

  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

  val stationSchema = StructType(List(
    StructField("stn", IntegerType, nullable = true),
    StructField("wban", IntegerType, nullable = true),
    StructField("lat", DoubleType, nullable = true),
    StructField("lon", DoubleType, nullable = true)
  ))

  val tempSchema = StructType(List(
    StructField("stn", IntegerType, nullable = true),
    StructField("wban", IntegerType, nullable = true),
    StructField("month", IntegerType, nullable = true),
    StructField("day", IntegerType, nullable = true),
    StructField("temp", DoubleType, nullable = true)
  ))

  case class Date(year: Int, month: Int, day: Int)

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    val defValues = Map("stn" -> -1, "wban" -> -1)
    val stationsDF = spark.read.schema(stationSchema).csv(path(stationsFile))
      .where(($"stn".isNotNull or $"wban".isNotNull) and $"lon".isNotNull and $"lat".isNotNull)
      .na.fill(defValues)

    val tempDF = spark.read.schema(tempSchema).csv(path(temperaturesFile))
      .where(($"stn".isNotNull or $"wban".isNotNull) and $"temp".isNotNull and $"temp".geq(0.0))
      .na.fill(defValues)

    tempDF.join(stationsDF, Seq("stn", "wban"))
      .select($"month", $"day", $"lat", $"lon", $"temp")
      .map(r => (Date(year, r.getInt(0), r.getInt(1)), Location(r.getDouble(2), r.getDouble(3)), toCelsius(r.getDouble(4))))
      .collect()
      .map(r => (LocalDate.of(r._1.year, r._1.month, r._1.day), r._2, r._3))
  }

  private def path(filePath: String): String = getClass.getResource(filePath).toURI.toString


  private def toCelsius(fahrenheit: Temperature): Temperature = (fahrenheit - 32) * 5 / 9

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    spark.sparkContext.parallelize(records.toSeq).map(t => (t._2, (t._3, 1)))
      .reduceByKey((v1, v2) => (v1._1 + v2._1, v1._2 + v2._2))
      .mapValues(v => v._1 / v._2)
      .collect()
  }

}
