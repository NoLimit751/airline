package com.patson.init

import com.patson.data.CountrySource

import scala.collection.mutable
import scala.collection.mutable.Map

object CountryMutualRelationshipGenerator extends App {
  lazy val AFFILIATIONS = List(
    Affiliation("EU", 4, List( // Europeoan union, alliance level (4)
      "BE", "GR", "LT", "PT", "BG", "ES", "LU", "RO", "CZ", "FR", "HU", "SI", "DK", "HR", "MT", "SK", "DE", "IT", "NL", "FI", "EE", "CY", "AT", "SE", "IE", "LV", "PL"
    )),
    Affiliation("WestBalk", 4, List( // Western Balkans, alliance level (4)
      "BA", "RS", "ME", "XK", "MK", "AL"
    )),
    Affiliation("ECAA", 3, List( // European common aviation market (plus I added Israel and Greenland), friendly level (3)
      "BE", "GR", "LT", "PT", "BG", "ES", "LU", "RO", "CZ", "FR", "HU", "SI", "DK", "HR", "MT", "SK", "DE", "IT", "NL", "FI", "EE", "CY", "AT", "SE", "IE", "LV", "PL", "GB", "GI", "NO", "GL", "IS", "CH", "BA", "RS", "ME", "XK", "MK", "AL", "MD", "UA", "GE", "IL"
    )),
    Affiliation("TR+ECAA", 2, List( //Turkey with ECAA, friendly level(2)
      "TR", "BE", "GR", "LT", "PT", "BG", "ES", "LU", "RO", "CZ", "FR", "HU", "SI", "DK", "HR", "MT", "SK", "DE", "IT", "NL", "FI", "EE", "CY", "AT", "SE", "IE", "LV", "PL", "GB", "GI", "GL", "NO", "IS", "CH", "BA", "RS", "ME", "XK", "MK", "AL", "MD", "UA", "GE", "IL"
    )),
    Affiliation("NAFTA+ECAA", 1, List( //NAFTA with ECAA, warm level(1)
      "US", "CA", "MX", "BE", "GR", "LT", "PT", "BG", "ES", "LU", "RO", "CZ", "FR", "HU", "SI", "DK", "HR", "MT", "SK", "DE", "IT", "NL", "FI", "EE", "CY", "AT", "SE", "IE", "LV", "PL", "GB", "GI", "GL", "NO", "IS", "CH", "BA", "RS", "ME", "XK", "MK", "AL", "MD", "UA", "GE", "IL"
    )),
    Affiliation("RUBY", 5, List( // Russia and Belarus confederation, one country level (5)
      "RU", "BY"
    )),
    Affiliation("EAEU", 3, List( // Euroasian economic union, frendly level (3)
      "RU", "BY", "AM", "KZ", "KG", "UZ"
    )),
    Affiliation("TR+EAEU", 1, List( //Turkey with EAEU, warm level(1)
      "TR", "RU", "BY", "AM", "KZ", "KG", "UZ"
    )),
    Affiliation("TR+AZ", 4, List( //Turkey and AZ, alliance level (4)
      "TR", "AZ"
    )),
    Affiliation("Turkic", 4, List( //Turkic states, frendly level (3)
      "TR", "AZ", "KZ", "UZ", "KG", "TM"
    )),
    Affiliation("OneChina", 4, List( // All chinese territories, alliance level (4)
      "CN", "HK", "MO", "TW"
    )),
    Affiliation("IndiaPlus", 43, List( // Invented India, Sri Lanka, Nepal, Butan travel area, friendly level (3)
      "IN", "LK", "NP", "BT"
    )),
    Affiliation("UKUSA", 4, List( // UKUSA agreement, alliance level (4)
      "US", "CA", "GB", "AU", "NZ"
    )),
    Affiliation("SouthPac", 3, List( // Invented South Pacific common aviation market, friendly level (3)
      "AU", "NZ", "NC", "FJ", "VU", "SB", "PG", "TO", "NU", "WS", "AS", "CK", "PF", "WF", "TV", "KI", "NR", "FM", "PW", "MP", "GU", "MH"
    )),
    Affiliation("ASEAN", 3, List( // Invented ASEAN common aviation market, friendly level (3)
      "PH", "ID", "TL", "SG", "MY", "VN", "LA", "KH", "TH", "MM"
    )),
    Affiliation("FrenchTerritories", 5, List( // French overseas territories, same country level (5)
      "FR", "GF", "GP", "MQ", "YT", "RE", "PF", "BL", "MF", "PM", "WF", "NC"
    )),
    Affiliation("BritishTerritories", 5, List( // British overseas territories, same country level (5)
      "GB", "IM", "AI", "BM", "VG", "KY", "FK", "GI", "MS", "SH", "TC"
    )),
    Affiliation("NetherlandsTerritories", 5, List( // Dutch overseas territories, same country level (5)
      "NL", "AW", "CW", "SX", "BQ"
    )),
    Affiliation("USTerritories", 5, List( // US overseas territories, same country level (5)
      "US", "AS", "GU", "MP", "PR", "VI"
    )),
    Affiliation("Caribbean", 3, List( // Invented Caribbean common aviation market, friendly level (3)
      "BM", "BS", "CU", "KY", "JM", "TC", "HT", "DO", "PR", "VI", "VG", "SX", "KN", "AG", "GP", "MS", "BL", "BQ", "DM", "MQ", "LC", "VC", "GD", "TT", "BB", "CW"   
    )),
    Affiliation("NAFTA", 4, List( // NAFTA, alliance level (4)
      "US", "CA", "MX"
    ))
  )


  mainFlow()



  def mainFlow() = {
    val mutualRelationshipMap = getCountryMutualRelationship()
    val mutualRelationshipPatchMap = getCountryMutualRelationshipPatch()


    val finalMutualRelationshipMap = affiliationAdjustment(mutualRelationshipMap ++ mutualRelationshipPatchMap)

    println("Saving country mutual relationships: " + finalMutualRelationshipMap)

    CountrySource.updateCountryMutualRelationships(finalMutualRelationshipMap)

    println("DONE")
  }

  def affiliationAdjustment(existingMap : mutable.Map[(String, String), Int]) : Map[(String, String), Int] = {
    println(s"affiliations: $AFFILIATIONS")
    AFFILIATIONS.foreach {
      case Affiliation(id, relationship, members) =>
        members.foreach { memberX =>
          if (CountrySource.loadCountryByCode(memberX).isDefined) {
            members.foreach { memberY =>
              if (memberX != memberY) {
                val shouldPatch = existingMap.get((memberX, memberY)) match {
                  case Some(existingValue) => existingValue < relationship
                  case None => true
                }
                if (shouldPatch) {
                  println(s"patching $memberX vs $memberY from $id with $relationship")
                  existingMap.put((memberX, memberY), relationship)
                } else {
                  println(s"Not patching $memberX vs $memberY from $id with $relationship as existing value is greater")
                }
              }
            }
          } else {
            println(s"Country code $memberX not found")
          }
        }
    }
    existingMap
  }

  /**
   * get from country-mutual-relationship.csv
   */
  def getCountryMutualRelationship() = {
    val nameToCode = CountrySource.loadAllCountries().map( country => (country.name, country.countryCode)).toMap
    val linesIter = scala.io.Source.fromFile("country-mutual-relationship.csv").getLines()
    val headerLine = linesIter.next()

    val countryHeader = headerLine.split(',').filter(!_.isEmpty())

    val mutualRelationshipMap = Map[(String, String), Int]()

    while (linesIter.hasNext) {
      val tokens = linesIter.next().split(',').filter(!_.isEmpty())
      //first token is the country name itself
      val fromCountry = tokens(0)
      for (i <- 1 until tokens.size) {
        val relationship = tokens(i)
        val strength = relationship.count( _ == '1') //just count the number of ones should be sufficient
        val toCountry = countryHeader(i - 1)
        //println(fromCountry + " " + toCountry + " " + strength)
        if (strength > 0) {
          if (nameToCode.contains(fromCountry) && nameToCode.contains(toCountry)) {
            mutualRelationshipMap.put((nameToCode(fromCountry), nameToCode(toCountry)), strength)
          }
        }
      }
    }

    nameToCode.values.foreach { countryCode =>
      mutualRelationshipMap.put((countryCode, countryCode), 5) //country with itself is 5 HomeCountry
    }

    mutualRelationshipMap
  }

  /**
   * patch from country-mutual-relationship-patch.csv
   */
  def getCountryMutualRelationshipPatch() = {
    val linesIter = scala.io.Source.fromFile("country-mutual-relationship-patch.csv").getLines()
    val mutualRelationshipMap = Map[(String, String), Int]()
    
    while (linesIter.hasNext) {
      val tokens = linesIter.next().split(',')
      //first token is the country name itself
      val fromCountry = tokens(0)
      val toCountry = tokens(1)
      val strength = Integer.valueOf(tokens(2))
      mutualRelationshipMap.put((fromCountry, toCountry), strength)
      mutualRelationshipMap.put((toCountry, fromCountry), strength)
    }
    mutualRelationshipMap
  }

  case class Affiliation(id : String, relationship: Int, members : List[String])



}