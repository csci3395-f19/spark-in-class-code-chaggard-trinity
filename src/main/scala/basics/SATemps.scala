package basics

case class TempRow(day: Int, doy: Int, month: Int, year: Int, precip: Double,
    tave: Double, tmax: Double, tmin: Double)

object SATemps{

  def parseLine(line: String): TempRow ={
    val p = line.split(",")
    TempRow(p(0).toInt, p(1).toInt, p(2).toInt, p(4).toInt, p(5).toDouble,
      p(6).toDouble, p(7).toDouble, p(8).toDouble);
  }

  def main(args: Array[String]): Unit = {
    val source = scala.io.Source.fromFile("/users/mlewis/CSCI3395-F19/InClassBD/data/SanAntonioTemps.csv")  
    val lines = source.getLines()
    val data = lines.drop(2).map(parseLine).toArray
    data.take(5).foreach(println)

    val hotDay1 = data.maxBy(_.tmax)
    val hotDay2 = data.reduce((d1,d2) => if(d1.tmax > d2.tmax) d1 else d2)
    println(hotDay1)
    println(hotDay2)

    val wetDay1 = data.maxBy(_.precip)
    val wetDay2 = data.reduce((d1,d2) => if(d1.precip > d2.precip) d1 else d2)
    println(wetDay1)
    println(wetDay2)

    val wetdaycount = data.count(_.precip > 1.0)
    println(wetdaycount)
    println((wetdaycount.toDouble / data.length.toDouble)*100)

    val wetdays = data.filter(x => x.precip > 0.0)
    val hotWet = wetdays.map(_.tmax).sum / wetdays.length
    println(hotWet)
    }

}
