package HospitalBeds

import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.math.Fractional.Implicits.infixFractionalOps



case class HospitalData(date: String, state: String, beds: Int, bedsCovid: Int, bedsNoncrit: Int, admittedPui: Int, admittedCovid: Int, admittedTotal: Int, dischargedPui: Int, dischargedCovid: Int, dischargedTotal: Int, hospCovid: Int, hospPui: Int, hospNoncovid: Int)

object HospitalDataAnalysis {
  def main(args: Array[String]): Unit = {
    val filename = "hospital.csv"
    val lines = Source.fromFile(filename).getLines().drop(1) // Skip the header line
    val data = ListBuffer[HospitalData]()

    for (line <- lines) {
      val fields = line.split(",")
      val hospitalData = HospitalData(
        fields(0), fields(1), fields(2).toInt, fields(3).toInt, fields(4).toInt,
        fields(5).toInt, fields(6).toInt, fields(7).toInt, fields(8).toInt,
        fields(9).toInt, fields(10).toInt, fields(11).toInt, fields(12).toInt, fields(13).toInt
      )
      data += hospitalData
    }

    // Question 1: Which state has the highest total hospital bed?
    val stateWithHighestBeds = data.groupBy(_.state).mapValues(_.map(_.beds).sum).maxBy(_._2)._1
    println(s"State with the most beds: $stateWithHighestBeds")

    // Question 2: What are the ratio of bed dedicated for COVID-19 to total of available hospital bed in the dataset?
    val totalBeds = data.map(_.beds).sum
    val totalCovidBeds = data.map(_.bedsCovid).sum
    val covidBedRatio = totalCovidBeds.toDouble / totalBeds
    println(f"COVID Bed Ratio: $covidBedRatio%.4f")

    // Question 3: What are the averages of individuals in category x where x can be suspected/probable, COVID-19 positive, or non-COVID is being admitted to hospitals for each state?
    val stateAdmissions = data.groupBy(_.state).mapValues(group => {
      val totalAdmittedPui = group.map(_.admittedPui).sum
      val totalAdmittedCovid = group.map(_.admittedCovid).sum
      val totalAdmittedNoncovid = group.map(_.admittedTotal - group.map(_.admittedCovid).sum - group.map(_.admittedPui).sum)
      val count = group.size
      (totalAdmittedPui.toDouble / count, totalAdmittedCovid.toDouble / count, totalAdmittedNoncovid.toDouble / count)
    })

    stateAdmissions.foreach { case (state, (avgPui, avgCovid, avgNoncovid)) =>
      println(f"State: $state, Avg Admitted PUI: $avgPui%.2f, Avg Admitted COVID: $avgCovid%.2f, Avg Admitted Non-COVID: $avgNoncovid%.2f")
    }
  }
}