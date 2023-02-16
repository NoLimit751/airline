package com.patson.init

import com.patson.model.Airport
import com.patson.data.AirportSource
import scala.io.Source
import scala.collection.mutable.ListBuffer

object StorageBasesLoader extends App {

  val loadedBases : List[Airport] = loadStorageBases()

  AirportSource.saveAirports(loadedBases)
  
  if (loadedBases.isEmpty) {
    println("The list is empty or there was an error")
    } else {
      println("Storage bases added:")
      loadedBases.foreach(println)
    }
  
  def loadStorageBases() : List[Airport] = {
    val storageBasesSource = scala.io.Source.fromFile("storagebases.csv").getLines()
    val storageBases = ListBuffer[Airport]()
    
    storageBasesSource.foreach { line =>
      if (!line.startsWith("#")) {
        val tokens = line.trim().split(",(?=([^\"]*\"[^\"]*\")*[^\"]*$)", -1).transform { token =>
          if (token.startsWith("\"") && token.endsWith("\"")) { 
            token.substring(1, token.length() - 1) 
          } else {
            token
          }
        }

        //iata : String, icao : String, name : String, latitude : Double, longitude : Double, countryCode : String, city : String, zone : String, var size : Int, baseIncome : Int, basePopulation : Long, var runwayLength : Int = Airport.MIN_RUNWAY_LENGTH, var id : Int = 0
        val airport = Airport(iata = tokens(0), icao = tokens(1), name = tokens(2), latitude = tokens(3).toDouble, longitude = tokens(4).toDouble, countryCode = tokens(5), city = tokens(6), zone = tokens(7), size = tokens(8).toInt, baseIncome = 0, basePopulation = 1, runwayLength = tokens(9).toInt)
        storageBases += airport
      } else {
      }
    }
    storageBases.toList
  }
}