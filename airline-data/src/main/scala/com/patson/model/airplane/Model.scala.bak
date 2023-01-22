package com.patson.model.airplane

import com.patson.model.IdObject
import com.patson.model.Airline
import com.patson.model.airplane.Model.Category
import com.patson.util.AirplaneModelCache

case class Model(name : String, family : String = "", capacity : Int, fuelBurn : Int, speed : Int, range : Int, price : Int, lifespan : Int, constructionTime : Int, manufacturer: Manufacturer, runwayRequirement : Int, imageUrl : String = "", var id : Int = 0) extends IdObject {
  import Model.Type._

  val countryCode = manufacturer.countryCode
  val SUPERSONIC_SPEED_THRESHOLD = 1236
  val airplaneType : Type = {
    if (speed > SUPERSONIC_SPEED_THRESHOLD) {
      SUPERSONIC
    } else {
      capacity match {
        case x if (x <= 15) => LIGHT
        case x if (x <= 60) => SMALL
        case x if (x <= 148) => REGIONAL // Reduced by 2 so that 737-600 and 737-700 fall into medium range
        case x if (x <= 260) => MEDIUM // Increased by 10 so that Irkut MC-21-400 falls into the medium range
        case x if (x <= 350) => LARGE
        case x if (x <= 500) => X_LARGE
        case _ => JUMBO
      }
    }
  }
  val category = Category.fromType(airplaneType)

  private[this]val BASE_TURNAROUND_TIME = 40
  val turnaroundTime : Int = {
    BASE_TURNAROUND_TIME +
      (airplaneType match {
        case LIGHT => capacity / 3 //45 - old value
        case SMALL =>  capacity / 3 //70
        case REGIONAL => capacity / 3 //100
        case MEDIUM =>  capacity / 2.5 //140
        case LARGE => capacity / 2.5 //180
        case X_LARGE => capacity / 2.5 //200
        case JUMBO => capacity / 2.5 //220
        case SUPERSONIC => capacity / 2.5
      }).toInt
  }

  val airplaneTypeLabel : String = label(airplaneType)

  //weekly fixed cost
  val baseMaintenanceCost : Int = {
    (capacity * 150).toInt //for now
  }

  def applyDiscount(discounts : List[ModelDiscount]) = {
    var discountedModel = this
    discounts.groupBy(_.discountType).foreach {
      case (discountType, discounts) => discountType match {
        case DiscountType.PRICE =>
          val totalDiscount = discounts.map(_.discount).sum
          discountedModel = discountedModel.copy(price = (price * (1 - totalDiscount)).toInt)
        case DiscountType.CONSTRUCTION_TIME =>
          var totalDiscount = discounts.map(_.discount).sum
          totalDiscount = Math.min(1, totalDiscount)
          discountedModel = discountedModel.copy(constructionTime = (constructionTime * (1 - totalDiscount)).toInt)
      }
    }
    discountedModel
  }

  val purchasableWithRelationship = (relationship : Int) => {
    relationship >= Model.BUY_RELATIONSHIP_THRESHOLD
  }
}

object Model {
  val BUY_RELATIONSHIP_THRESHOLD = 0

  def fromId(id : Int) = {
    val modelWithJustId = Model("Unknown", "Unknown", 0, 0, 0, 0, 0, 0, 0, Manufacturer("Unknown", countryCode = ""), runwayRequirement = 0)
    modelWithJustId.id = id
    modelWithJustId
  }

  object Type extends Enumeration {
    type Type = Value
    val LIGHT, SMALL, REGIONAL, MEDIUM, LARGE, X_LARGE, JUMBO, SUPERSONIC = Value

    val label = (airplaneType : Type) => { airplaneType match {
        case LIGHT => "Light"
        case SMALL => "Small"
        case REGIONAL => "Regional"
        case MEDIUM => "Medium"
        case LARGE => "Large"
        case X_LARGE => "Extra large"
        case JUMBO => "Jumbo"
        case SUPERSONIC => "Supersonic"
      }
    }
  }

  object Category extends Enumeration {
    type Category = Value
    val LIGHT, REGIONAL, MEDIUM, LARGE, SUPERSONIC = Value
    val grouping = Map(
      LIGHT -> List(Type.LIGHT, Type.SMALL),
      REGIONAL -> List(Type.REGIONAL),
      MEDIUM -> List(Type.MEDIUM),
      LARGE -> List(Type.LARGE, Type.X_LARGE, Type.JUMBO),
      SUPERSONIC -> List(Type.SUPERSONIC)
    )

    val fromType = (airplaneType : Type.Value) => {
      grouping.find(_._2.contains(airplaneType)).get._1
    }

    val capacityRange : Map[Category.Value, (Int, Int)]= {
      AirplaneModelCache.allModels.map(_._2).groupBy(_.category).view.mapValues { models =>
        val sortedByCapacity = models.toList.sortBy(_.capacity)
        (sortedByCapacity.head.capacity, sortedByCapacity.last.capacity)
      }.toMap
    }

    def getCapacityRange(category: Category.Value) = {
      capacityRange.get(category).getOrElse((0, 0))
    }

  }

  // Added only the airplanes I want to use (removed the old retired models, added some upcoming models)
  //https://en.wikipedia.org/wiki/List_of_jet_airliners
  val models = List(Model("Cessna 421", "Cessna", capacity = 7, fuelBurn = (7 * 1).toInt, speed = 300, range = 1555, price = 550000, lifespan = 35 * 52, constructionTime = 0, Manufacturer("Cessna", countryCode = "US"), runwayRequirement = 708),
    Model("Cessna Caravan", "Cessna", capacity = 14, fuelBurn = (14 * 1).toInt, speed = 344, range = 2400, price = 2500000, lifespan = 35 * 52, constructionTime = 0, Manufacturer("Cessna", countryCode = "US"), runwayRequirement = 762, imageUrl = "https://www.norebbo.com/2017/06/cessna-208-grand-caravan-blank-illustration-templates/"),
    Model("Cessna Citation X", "Cessna", capacity = 20, fuelBurn = (20 * 1.7).toInt, speed = 850, range = 5053, price = 9000000, lifespan = 35 * 52, constructionTime = 0, Manufacturer("Cessna", countryCode = "US"), runwayRequirement = 1600, imageUrl = "https://www.norebbo.com/cessna-citation-x-template/"),
    Model("Britten-Norman BN-2 Islander", "Britten-Norman", capacity = 9, fuelBurn = (9 * 1).toInt, speed = 273, range = 1400, price = 2000000, lifespan = 35 * 52, constructionTime = 0, Manufacturer("Britten-Norman", countryCode = "GB"), runwayRequirement = 370, imageUrl = ""),
    Model("Beechcraft Super King Air 200", "Beechcraft King Air", capacity = 10, fuelBurn = (10 * 1.4).toInt, speed = 573, range = 3185, price = 1900000, lifespan = 35 * 52, constructionTime = 0, Manufacturer("Beechcraft", countryCode = "US"), runwayRequirement = 643, imageUrl = "https://www.norebbo.com/beechcraft-b200-king-air-side-view/"),
	Model("Dassault Falcon 50", "Dassault Falcon", capacity = 9, fuelBurn = (9 * 1.6).toInt, speed = 800, range = 5660, price = 2_800_000, lifespan = 35 * 52, constructionTime = 0, Manufacturer("Dassault", countryCode = "FR"), runwayRequirement = 1524, imageUrl = "https://www.norebbo.com/dassault-falcon-50/"),
    Model("Pilatus PC-12", "Pilatus", capacity = 9, fuelBurn = (9 * 1.2).toInt, speed = 528, range = 3417, price = 1800000, lifespan = 35 * 52, constructionTime = 0, Manufacturer("Pilatus", countryCode = "CH"), runwayRequirement = 758),
    Model("Let L-410NG", "Let L-410", capacity = 19, fuelBurn = (19 * 2.2).toInt, speed = 417, range = 2570, price = 5000000, lifespan = 25 * 52, constructionTime = 0, Manufacturer("Let Kunovice", countryCode = "RU"), runwayRequirement = 600, imageUrl = ""),
    Model("Let L-410UVP-E20", "Let L-410", capacity = 19, fuelBurn = (19 * 1.9).toInt, speed = 417, range = 1356, price = 4200000, lifespan = 25 * 52, constructionTime = 0, Manufacturer("Let Kunovice", countryCode = "RU"), runwayRequirement = 510, imageUrl = ""),
    Model("Gulfstream G650ER", "Gulfstream", capacity = 30, fuelBurn = (19 * 1.8).toInt, speed = 904, range = 13890, price = 30000000, lifespan = 40 * 52, constructionTime = 0, Manufacturer("Gulfstream", countryCode = "US"), runwayRequirement = 1920, imageUrl = "https://www.norebbo.com/gulfstream-g650er-template/"),
    Model("CASA CN-235", "CASA CN-235", capacity = 40, fuelBurn = (40 * 1.9).toInt, speed = 460, range = 3658, price = 15000000, lifespan = 30 * 52, constructionTime = 0, Manufacturer("CASA", countryCode = "ES"), runwayRequirement = 1204, imageUrl = ""),
    Model("ATR 42-600", "ATR-Regional", capacity = 48, fuelBurn = (48 * 1.6).toInt, speed = 556, range = 1326, price = 17000000, lifespan = 20 * 52, constructionTime = 0, Manufacturer("ATR", countryCode = "FR"), runwayRequirement = 1050, imageUrl = "https://www.norebbo.com/2018/06/atr-42-blank-illustration-templates/"),
    Model("ATR 72-600", "ATR-Regional", capacity = 78, fuelBurn = (78 * 2.4).toInt, speed = 556, range = 1528, price = 26000000, lifespan = 20 * 52, constructionTime = 4, Manufacturer("ATR", countryCode = "FR"), runwayRequirement = 1367, imageUrl = "https://www.norebbo.com/2017/04/atr-72-blank-illustration-templates/"),
    Model("Airbus A220-100", "Airbus A220", capacity = 133, fuelBurn = (133 * 3.2).toInt, speed = 828, range = 3690, price = 80000000, lifespan = 35 * 52, constructionTime = 8, Manufacturer("Airbus", countryCode = "NL"), runwayRequirement = 1463, imageUrl = "https://www.norebbo.com/2016/02/bombardier-cs100-blank-illustration-templates/"),
    Model("Airbus A220-300", "Airbus A220", capacity = 160, fuelBurn = (160 * 3.4).toInt, speed = 828, range = 6667, price = 98000000, lifespan = 35 * 52, constructionTime = 20, Manufacturer("Airbus", countryCode = "NL"), runwayRequirement = 1890, imageUrl = "https://www.norebbo.com/2016/02/bombardier-cs300-blank-illustration-templates/"),
    Model("Airbus A318", "Airbus A320", capacity = 132, fuelBurn = (132 * 3.3).toInt, speed = 829, range = 7800, price = 90000000, lifespan = 35 * 52, constructionTime = 8, Manufacturer("Airbus", countryCode = "NL"), runwayRequirement = 1780, imageUrl = "https://www.norebbo.com/airbus-a318-blank-illustration-templates-with-pratt-whitney-and-cfm56-engines/"),
    Model("Airbus A319", "Airbus A320", capacity = 156, fuelBurn = (156 * 3.4).toInt, speed = 830, range = 6940, price = 95000000, lifespan = 35 * 52, constructionTime = 16, Manufacturer("Airbus", countryCode = "NL"), runwayRequirement = 1850, imageUrl = "https://www.norebbo.com/2014/05/airbus-a319-blank-illustration-templates/"),
    Model("Airbus A320", "Airbus A320", capacity = 180, fuelBurn = (180 * 3.8).toInt, speed = 828, range = 6150, price = 100000000, lifespan = 35 * 52, constructionTime = 24, Manufacturer("Airbus", countryCode = "NL"), runwayRequirement = 2100, imageUrl = "https://www.norebbo.com/2013/08/airbus-a320-blank-illustration-templates/"),
    Model("Airbus A321", "Airbus A320", capacity = 236, fuelBurn = (236 * 4.3).toInt, speed = 830, range = 5930, price = 120000000, lifespan = 35 * 52, constructionTime = 36, Manufacturer("Airbus", countryCode = "NL"), runwayRequirement = 2210, imageUrl = "https://www.norebbo.com/2014/03/airbus-a321-blank-illustration-templates/"),
    Model("Airbus A319neo", "Airbus A320", capacity = 160, fuelBurn = (160 * 3.5).toInt, speed = 828, range = 6850, price = 100000000, lifespan = 35 * 52, constructionTime = 20, Manufacturer("Airbus", countryCode = "NL"), runwayRequirement = 2164, imageUrl = "https://www.norebbo.com/2017/09/airbus-a319-neo-blank-illustration-templates/"),
    Model("Airbus A320neo", "Airbus A320", capacity = 195, fuelBurn = (195 * 4).toInt, speed = 833, range = 6500, price = 110000000, lifespan = 35 * 52, constructionTime = 24, Manufacturer("Airbus", countryCode = "NL"), runwayRequirement = 2100, imageUrl = "https://www.norebbo.com/2017/08/airbus-a320-neo-blank-illustration-templates/"),
    Model("Airbus A321neo", "Airbus A320", capacity = 244, fuelBurn = (244 * 4.1).toInt, speed = 828, range = 6850, price = 130000000, lifespan = 35 * 52, constructionTime = 36, Manufacturer("Airbus", countryCode = "NL"), runwayRequirement = 1988, imageUrl = "https://www.norebbo.com/2017/09/airbus-a321-neo-blank-illustration-templates/"),
    Model("Airbus A321neoLR", "Airbus A320", capacity = 240, fuelBurn = (240 * 4.15).toInt, speed = 828, range = 7400, price = 132000000, lifespan = 35 * 52, constructionTime = 36, Manufacturer("Airbus", countryCode = "NL"), runwayRequirement = 2300, imageUrl = "https://www.norebbo.com/2018/10/airbus-a321neo-lr-long-range-blank-illustration-templates/"),
    Model("Airbus A321neoXLR", "Airbus A320", capacity = 236, fuelBurn = (236 * 4.4).toInt, speed = 828, range = 8700, price = 133000000, lifespan = 35 * 52, constructionTime = 36, Manufacturer("Airbus", countryCode = "NL"), runwayRequirement = 2400, imageUrl = "https://www.norebbo.com/2018/10/airbus-a321neo-lr-long-range-blank-illustration-templates/"),
    Model("Airbus A330-800neo", "Airbus A330", capacity = 406, fuelBurn = (406 * 4.5).toInt, speed = 912, range = 13900, price = 260000000, lifespan = 35 * 52, constructionTime = 36, Manufacturer("Airbus", countryCode = "NL"), runwayRequirement = 2770, imageUrl = "https://www.norebbo.com/2018/06/airbus-a330-800-neo-blank-illustration-templates/"),
    Model("Airbus A330-900neo", "Airbus A330", capacity = 440, fuelBurn = (440 * 4.5).toInt, speed = 912, range = 12130, price = 290000000, lifespan = 35 * 52, constructionTime = 36, Manufacturer("Airbus", countryCode = "NL"), runwayRequirement = 2770, imageUrl = "https://www.norebbo.com/2018/06/airbus-a330-900-neo-blank-illustration-templates/"),
    Model("Airbus A350-0900", "Airbus A350", capacity = 440, fuelBurn = (440 * 4.9).toInt, speed = 903, range = 15000, price = 280000000, lifespan = 35 * 52, constructionTime = 36, Manufacturer("Airbus", countryCode = "NL"), runwayRequirement = 2600, imageUrl = "https://www.norebbo.com/2013/07/airbus-a350-900-blank-illustration-templates/"),
    Model("Airbus A350-0900ULR", "Airbus A350", capacity = 440, fuelBurn = (440 * 5.2).toInt, speed = 910, range = 17960, price = 325000000, lifespan = 35 * 52, constructionTime = 36, Manufacturer("Airbus", countryCode = "NL"), runwayRequirement = 2600, imageUrl = "https://www.norebbo.com/2013/07/airbus-a350-900-blank-illustration-templates/"),
    Model("Airbus A350-1000", "Airbus A350", capacity = 475, fuelBurn = (475 * 4.5).toInt, speed = 910, range = 15550, price = 360000000, lifespan = 35 * 52, constructionTime = 40, Manufacturer("Airbus", countryCode = "NL"), runwayRequirement = 2600, imageUrl = "https://www.norebbo.com/2015/11/airbus-a350-1000-blank-illustration-templates/"),
    Model("Airbus A350-1000ULR", "Airbus A350", capacity = 440, fuelBurn = (390 * 5.0).toInt, speed = 910, range = 18100, price = 390000000, lifespan = 35 * 52, constructionTime = 36, Manufacturer("Airbus", countryCode = "NL"), runwayRequirement = 2600, imageUrl = "https://www.norebbo.com/2015/11/airbus-a350-1000-blank-illustration-templates/"),
	Model("Airbus A380-800", "Airbus A380", capacity = 853, fuelBurn = (853 * 6).toInt, speed = 945, range = 15700, price = 450000000, lifespan = 35 * 52, constructionTime = 54, Manufacturer("Airbus", countryCode = "NL"), runwayRequirement = 3000, imageUrl = "https://www.norebbo.com/2013/06/airbus-a380-800-blank-illustration-templates/"),
    Model("Boeing 737-600", "Boeing 737 Next Generation", capacity = 149, fuelBurn = (149 * 3.9).toInt, speed = 830, range = 7200, price = 82500000, lifespan = 35 * 52, constructionTime = 16, Manufacturer("Boeing", countryCode = "US"), runwayRequirement = 1878, imageUrl = "https://www.norebbo.com/2018/09/boeing-737-600-blank-illustration-templates/"),
    Model("Boeing 737-700", "Boeing 737 Next Generation", capacity = 149, fuelBurn = (149 * 3.8).toInt, speed = 830, range = 7630, price = 85000000, lifespan = 35 * 52, constructionTime = 16, Manufacturer("Boeing", countryCode = "US"), runwayRequirement = 2042, imageUrl = "https://www.norebbo.com/2014/04/boeing-737-700-blank-illustration-templates/"),
    Model("Boeing 737-700C", "Boeing 737 Next Generation", capacity = 140, fuelBurn = (140 * 3.3).toInt, speed = 825, range = 6083, price = 85000000, lifespan = 35 * 52, constructionTime = 12, Manufacturer("Boeing", countryCode = "US"), runwayRequirement = 2042, imageUrl = "https://www.norebbo.com/2014/04/boeing-737-700-blank-illustration-templates/"),
    Model("Boeing 737-700ER", "Boeing 737 Next Generation", capacity = 149, fuelBurn = (149 * 4.2).toInt, speed = 830, range = 10200, price = 90000000, lifespan = 35 * 52, constructionTime = 16, Manufacturer("Boeing", countryCode = "US"), runwayRequirement = 2042, imageUrl = "https://www.norebbo.com/2014/04/boeing-737-700-blank-illustration-templates/"),
    Model("Boeing 737-800", "Boeing 737 Next Generation", capacity = 184, fuelBurn = (184 * 3.8).toInt, speed = 825, range = 5436, price = 100000000, lifespan = 35 * 52, constructionTime = 24, Manufacturer("Boeing", countryCode = "US"), runwayRequirement = 2316, imageUrl = "https://www.norebbo.com/2012/11/boeing-737-800-blank-illustration-templates/"),
    Model("Boeing 737-900", "Boeing 737 Next Generation", capacity = 189, fuelBurn = (189 * 4).toInt, speed = 830, range = 6660, price = 105000000, lifespan = 35 * 52, constructionTime = 24, Manufacturer("Boeing", countryCode = "US"), runwayRequirement = 3000, imageUrl = "https://www.norebbo.com/2014/08/boeing-737-900-blank-illustration-templates/"),
    Model("Boeing 737-900ER", "Boeing 737 Next Generation", capacity = 220, fuelBurn = (220 * 4).toInt, speed = 830, range = 6500, price = 110000000, lifespan = 35 * 52, constructionTime = 24, Manufacturer("Boeing", countryCode = "US"), runwayRequirement = 3000, imageUrl = "https://www.norebbo.com/2016/07/boeing-737-900er-with-split-scimitar-winglets-blank-illustration-templates/"),
    Model("Boeing 737 MAX 7", "Boeing 737 MAX", capacity = 172, fuelBurn = (172 * 3.6).toInt, speed = 830, range = 6500, price = 105000000, lifespan = 35 * 52, constructionTime = 20, Manufacturer("Boeing", countryCode = "US"), runwayRequirement = 2100, imageUrl = "https://www.norebbo.com/2016/07/boeing-737-max-7-blank-illustration-templates/"),
    Model("Boeing 737 MAX 8", "Boeing 737 MAX", capacity = 189, fuelBurn = (189 * 3.9).toInt, speed = 830, range = 6500, price = 110000000, lifespan = 35 * 52, constructionTime = 24, Manufacturer("Boeing", countryCode = "US"), runwayRequirement = 2500, imageUrl = "https://www.norebbo.com/2016/07/boeing-737-max-8-blank-illustration-templates/"),
    Model("Boeing 737 MAX 9", "Boeing 737 MAX", capacity = 220, fuelBurn = (220 * 3.9).toInt, speed = 839, range = 6570, price = 124000000, lifespan = 35 * 52, constructionTime = 36, Manufacturer("Boeing", countryCode = "US"), runwayRequirement = 2600, imageUrl = "https://www.norebbo.com/2018/05/boeing-737-9-max-blank-illustration-templates/"),
    Model("Boeing 737 MAX 10", "Boeing 737 MAX", capacity = 230, fuelBurn = (230 * 3.9).toInt, speed = 830, range = 6500, price = 132000000, lifespan = 35 * 52, constructionTime = 36, Manufacturer("Boeing", countryCode = "US"), runwayRequirement = 2700, imageUrl = "https://www.norebbo.com/2019/01/737-10-max-side-view/"),
    Model("Boeing 747-8i", "Boeing 747", capacity = 605, fuelBurn = (605 * 5.2).toInt, speed = 920, range = 14815, price = 420000000, lifespan = 35 * 52, constructionTime = 48, Manufacturer("Boeing", countryCode = "US"), runwayRequirement = 3100, imageUrl = "https://www.norebbo.com/2015/12/boeing-747-8i-blank-illustration-templates/"),
    Model("Boeing 777-8", "Boeing 777", capacity = 440, fuelBurn = (440 * 4.4).toInt, speed = 896, range = 16090, price = 340000000, lifespan = 35 * 52, constructionTime = 36, Manufacturer("Boeing", countryCode = "US"), runwayRequirement = 3050, imageUrl = "https://www.norebbo.com/2019/12/boeing-777-8-side-view/"),
    Model("Boeing 777-9", "Boeing 777", capacity = 550, fuelBurn = (550 * 4.9).toInt, speed = 896, range = 13940, price = 450000000, lifespan = 35 * 52, constructionTime = 44, Manufacturer("Boeing", countryCode = "US"), runwayRequirement = 3050, imageUrl = "https://www.norebbo.com/2019/12/boeing-777-9-side-view/"),
    Model("Boeing 787-8 Dreamliner", "Boeing 787", capacity = 250, fuelBurn = (250 * 4.5).toInt, speed = 907, range = 13621, price = 125000000, lifespan = 35 * 52, constructionTime = 36, Manufacturer("Boeing", countryCode = "US"), runwayRequirement = 2600, imageUrl = "https://www.norebbo.com/2013/02/boeing-787-8-blank-illustration-templates/"),
    Model("Boeing 787-9 Dreamliner", "Boeing 787", capacity = 300, fuelBurn = (300 * 4.5).toInt, speed = 903, range = 13950, price = 175000000, lifespan = 35 * 52, constructionTime = 36, Manufacturer("Boeing", countryCode = "US"), runwayRequirement = 2800, imageUrl = "https://www.norebbo.com/2014/04/boeing-787-9-blank-illustration-templates/"),
    Model("Boeing 787-10 Dreamliner", "Boeing 787", capacity = 360, fuelBurn = (360 * 4.4).toInt, speed = 903, range = 11750, price = 225000000, lifespan = 35 * 52, constructionTime = 36, Manufacturer("Boeing", countryCode = "US"), runwayRequirement = 2800, imageUrl = "https://www.norebbo.com/2017/06/boeing-787-10-blank-illustration-templates/"),
    Model("Bombardier DHC-6-400", "Bombardier DHC-8", capacity = 19, fuelBurn = (19 * 1.2).toInt, speed = 337, range = 1480, price = 5200000, lifespan = 35 * 52, constructionTime = 0, Manufacturer("Bombardier", countryCode = "CA"), runwayRequirement = 366, imageUrl = ""),
    Model("Bombardier Q200", "Bombardier DHC-8", capacity = 40, fuelBurn = (40 * 1.3).toInt, speed = 535, range = 2084, price = 18000000, lifespan = 35 * 52, constructionTime = 0, Manufacturer("Bombardier", countryCode = "CA"), runwayRequirement = 1000, imageUrl = "https://www.norebbo.com/2018/01/de-havilland-dhc-8-200-dash-8-blank-illustration-templates/"),
    Model("Bombardier Q300", "Bombardier DHC-8", capacity = 56, fuelBurn = (56 * 1.7).toInt, speed = 532, range = 1711, price = 22000000, lifespan = 35 * 52, constructionTime = 0, Manufacturer("Bombardier", countryCode = "CA"), runwayRequirement = 1085, imageUrl = "https://www.norebbo.com/2018/05/de-havilland-dhc-8-300-blank-illustration-templates/"),
	Model("Bombardier Q400", "Bombardier DHC-8", capacity = 90, fuelBurn = (90 * 2.5).toInt, speed = 667, range = 2040, price = 33000000, lifespan = 30 * 52, constructionTime = 8, Manufacturer("Bombardier", countryCode = "CA"), runwayRequirement = 1885, imageUrl = "https://www.norebbo.com/2015/08/bombardier-dhc-8-402-q400-blank-illustration-templates/"),
    Model("Bombardier CRJ0200", "Bombardier CRJ", capacity = 50, fuelBurn = (50 * 1.9).toInt, speed = 830, range = 3150, price = 30000000, lifespan = 35 * 52, constructionTime = 0, Manufacturer("Bombardier", countryCode = "CA"), runwayRequirement = 1920, imageUrl = "https://www.norebbo.com/2015/04/bombardier-canadair-regional-jet-200-blank-illustration-templates/"),
    Model("Bombardier CRJ0700", "Bombardier CRJ", capacity = 78, fuelBurn = (78 * 2.3).toInt, speed = 828, range = 3045, price = 42000000, lifespan = 35 * 52, constructionTime = 4, Manufacturer("Bombardier", countryCode = "CA"), runwayRequirement = 1605, imageUrl = "https://www.norebbo.com/2015/05/bombardier-canadair-regional-jet-700-blank-illustration-templates/"),
	Model("Bombardier CRJ0900", "Bombardier CRJ", capacity = 90, fuelBurn = (90 * 2.9).toInt, speed = 870, range = 2876, price = 45000000, lifespan = 35 * 52, constructionTime = 8, Manufacturer("Bombardier", countryCode = "CA"), runwayRequirement = 1939, imageUrl = "https://www.norebbo.com/2016/07/bombardier-canadair-regional-jet-900-blank-illustration-templates/"),
    Model("Bombardier CRJ1000", "Bombardier CRJ", capacity = 104, fuelBurn = (104 * 3).toInt, speed = 870, range = 3004, price = 50000000, lifespan = 35 * 52, constructionTime = 8, Manufacturer("Bombardier", countryCode = "CA"), runwayRequirement = 2120, imageUrl = "https://www.norebbo.com/2019/06/bombardier-crj-1000-side-view/"),
	Model("Bombardier Global 5000", "Bombardier Global", capacity = 13, fuelBurn = (13 * 1.5).toInt, speed = 902, range = 9630, price = 7_000_000, lifespan = 35 * 52, constructionTime = 0, Manufacturer("Bombardier", countryCode = "CA"), runwayRequirement = 1689, imageUrl = "https://www.norebbo.com/bombardier-global-5000-blank-illustration-templates/"),
	Model("Bombardier Global 7500", "Bombardier Global", capacity = 19, fuelBurn = (19 * 1.6).toInt, speed = 902, range = 14260, price = 13_000_000, lifespan = 35 * 52, constructionTime = 0, Manufacturer("Bombardier", countryCode = "CA"), runwayRequirement = 1768, imageUrl = "https://www.norebbo.com/bombardier-global-7500-side-view/"),
    Model("Embraer ERJ135", "Embraer ERJ", capacity = 37, fuelBurn = (37 * 2.2).toInt, speed = 850, range = 3241, price = 17500000, lifespan = 30 * 52, constructionTime = 0, Manufacturer("Embraer", countryCode = "BR"), runwayRequirement = 1580, imageUrl = "https://www.norebbo.com/2018/05/embraer-erj-135-blank-illustration-templates/"),
    Model("Embraer ERJ140", "Embraer ERJ", capacity = 44, fuelBurn = (44 * 2.5).toInt, speed = 828, range = 2315, price = 15000000, lifespan = 30 * 52, constructionTime = 0, Manufacturer("Embraer", countryCode = "BR"), runwayRequirement = 1850, imageUrl = "https://www.norebbo.com/2018/05/embraer-erj-140-blank-illustration-templates/"),
    Model("Embraer ERJ145", "Embraer ERJ", capacity = 50, fuelBurn = (50 * 2.6).toInt, speed = 850, range = 2800, price = 20500000, lifespan = 30 * 52, constructionTime = 0, Manufacturer("Embraer", countryCode = "BR"), runwayRequirement = 1410, imageUrl = "https://www.norebbo.com/2018/04/embraer-erj-145-blank-illustration-templates/"),
    Model("Embraer ERJ145XR", "Embraer ERJ", capacity = 50, fuelBurn = (50 * 2.7).toInt, speed = 850, range = 3700, price = 22000000, lifespan = 30 * 52, constructionTime = 0, Manufacturer("Embraer", countryCode = "BR"), runwayRequirement = 1720, imageUrl = "https://www.norebbo.com/2018/04/embraer-erj-145xr-blank-illustration-templates/"),
    Model("Embraer ERJ170", "Embraer ERJ", capacity = 72, fuelBurn = (72 * 3).toInt, speed = 870, range = 3982, price = 31500000, lifespan = 30 * 52, constructionTime = 4, Manufacturer("Embraer", countryCode = "BR"), runwayRequirement = 1644, imageUrl = "https://www.norebbo.com/2015/10/embraer-erj-175-templates-with-the-new-style-winglets/"),
    Model("Embraer ERJ175", "Embraer ERJ", capacity = 78, fuelBurn = (78 * 3.2).toInt, speed = 870, range = 4074, price = 35000000, lifespan = 30 * 52, constructionTime = 4, Manufacturer("Embraer", countryCode = "BR"), runwayRequirement = 2244, imageUrl = "https://www.norebbo.com/embraer-erj-175-templates-with-the-new-style-winglets/"),
    Model("Embraer EMB170-200", "Embraer ERJ", capacity = 88, fuelBurn = (88 * 3).toInt, speed = 871, range = 2200, price = 32000000, lifespan = 30 * 52, constructionTime = 6, Manufacturer("Embraer", countryCode = "BR"), runwayRequirement = 2244, imageUrl = "https://www.norebbo.com/2015/10/embraer-erj-175-templates-with-the-new-style-winglets/"),
    Model("Embraer EMB190", "Embraer ERJ", capacity = 100, fuelBurn = (100 * 3.3).toInt, speed = 823, range = 4537, price = 40000000, lifespan = 30 * 52, constructionTime = 8, Manufacturer("Embraer", countryCode = "BR"), runwayRequirement = 2100, imageUrl = "https://www.norebbo.com/2015/06/embraer-190-blank-illustration-templates/"),
    Model("Embraer E175-E2", "Embraer E-Jet E2", capacity = 88, fuelBurn = (88 * 3).toInt, speed = 870, range = 3735, price = 35000000, lifespan = 30 * 52, constructionTime = 6, Manufacturer("Embraer", countryCode = "BR"), runwayRequirement = 1800, imageUrl = "https://www.norebbo.com/2019/03/e175-e2-side-view/"),
    Model("Embraer E190-E2", "Embraer E-Jet E2", capacity = 106, fuelBurn = (106 * 3.5).toInt, speed = 870, range = 5278, price = 45000000, lifespan = 30 * 52, constructionTime = 8, Manufacturer("Embraer", countryCode = "BR"), runwayRequirement = 1450, imageUrl = "https://www.norebbo.com/2019/03/e190-e2-blank-side-view/"),
    Model("Embraer E195-E2", "Embraer E-Jet E2", capacity = 146, fuelBurn = (146 * 3.7).toInt, speed = 833, range = 4800, price = 60400000, lifespan = 30 * 52, constructionTime = 12, Manufacturer("Embraer", countryCode = "BR"), runwayRequirement = 1970, imageUrl = "https://www.norebbo.com/2019/03/embraer-e195-e2-side-view/"),
    Model("Antonov An148", "Antonov-A", capacity = 85, fuelBurn = (85 * 3.8).toInt, speed = 835, range = 3500, price = 30000000, lifespan = 20 * 52, constructionTime = 4, Manufacturer("Antonov", countryCode = "UA"), runwayRequirement = 1885),
    Model("Irkut MC-21-200", "Irkut MC-21", capacity = 165, fuelBurn = (165 * 3.9).toInt, speed = 825, range = 6400, price = 355000000, lifespan = 35 * 52, constructionTime = 22, Manufacturer("Irkut", countryCode = "RU"), runwayRequirement = 2200, imageUrl = "https://www.norebbo.com/irkut-mc-21-300/"),
	Model("Irkut MC-21-300", "Irkut MC-21", capacity = 211, fuelBurn = (211 * 4.1).toInt, speed = 825, range = 6000, price = 115000000, lifespan = 35 * 52, constructionTime = 24, Manufacturer("Irkut", countryCode = "RU"), runwayRequirement = 2400, imageUrl = "https://www.norebbo.com/irkut-mc-21-300/"),
	Model("Irkut MC-21-300LR", "Irkut MC-21", capacity = 211, fuelBurn = (211 * 4.5).toInt, speed = 825, range = 12000, price = 120000000, lifespan = 35 * 52, constructionTime = 24, Manufacturer("Irkut", countryCode = "RU"), runwayRequirement = 2600, imageUrl = "https://www.norebbo.com/irkut-mc-21-300/"),
	Model("Irkut MC-21-400", "Irkut MC-21", capacity = 256, fuelBurn = (256 * 4.4).toInt, speed = 825, range = 6000, price = 130000000, lifespan = 35 * 52, constructionTime = 30, Manufacturer("Irkut", countryCode = "RU"), runwayRequirement = 2600, imageUrl = "https://www.norebbo.com/irkut-mc-21-300/"),
	Model("Irkut MC-21-400LR", "Irkut MC-21", capacity = 256, fuelBurn = (256 * 4.9).toInt, speed = 825, range = 12000, price = 135000000, lifespan = 35 * 52, constructionTime = 30, Manufacturer("Irkut", countryCode = "RU"), runwayRequirement = 2800, imageUrl = "https://www.norebbo.com/irkut-mc-21-300/"),
    Model("Ilyushin Il-96-300", "Ilyushin Il-96", capacity = 300, fuelBurn = (300 * 5.2).toInt, speed = 850, range = 11500, price = 100000000, lifespan = 25 * 52, constructionTime = 36, Manufacturer("Ilyushin", countryCode = "RU"), runwayRequirement = 2340),
    Model("Ilyushin Il-96-400", "Ilyushin Il-96", capacity = 436, fuelBurn = (436 * 6.4).toInt, speed = 870, range = 10000, price = 150000000, lifespan = 25 * 52, constructionTime = 36, Manufacturer("Ilyushin", countryCode = "RU"), runwayRequirement = 2700),
    Model("Tupolev Tu-204", "Tupolev Tu", capacity = 210, fuelBurn = (210 * 4.5).toInt, speed = 810, range = 4300, price = 50000000, lifespan = 25 * 52, constructionTime = 24, Manufacturer("Tupolev", countryCode = "RU"), runwayRequirement = 1870),
    Model("Sukhoi Superjet 100", "Sukhoi Superjet", capacity = 108, fuelBurn = (108 * 4.1).toInt, speed = 828, range = 4578, price = 38000000, lifespan = 30 * 52, constructionTime = 8, Manufacturer("JSC Sukhoi", countryCode = "RU"), runwayRequirement = 1731, imageUrl = "https://www.norebbo.com/2016/02/sukhoi-ssj-100-blank-illustration-templates/"),
    Model("Comac ARJ21-700", "Comac ARJ", capacity = 90, fuelBurn = (90 * 3.5).toInt, speed = 828, range = 2200, price = 45000000, lifespan = 25 * 52, constructionTime = 8, Manufacturer("COMAC", countryCode = "CN"), runwayRequirement = 1700),
	Model("Comac ARJ21-900", "Comac ARJ", capacity = 105, fuelBurn = (90 * 3.7).toInt, speed = 828, range = 2200, price = 55000000, lifespan = 25 * 52, constructionTime = 12, Manufacturer("COMAC", countryCode = "CN"), runwayRequirement = 1700),
    Model("Comac ARJ21-700ER", "Comac ARJ", capacity = 90, fuelBurn = (90 * 3.5).toInt, speed = 828, range = 3700, price = 47000000, lifespan = 25 * 52, constructionTime = 8, Manufacturer("COMAC", countryCode = "CN"), runwayRequirement = 1900),
	Model("Comac ARJ21-900ER", "Comac ARJ", capacity = 105, fuelBurn = (90 * 3.7).toInt, speed = 828, range = 3300, price = 57000000, lifespan = 25 * 52, constructionTime = 12, Manufacturer("COMAC", countryCode = "CN"), runwayRequirement = 1950),
    Model("Comac C919", "Comac C", capacity = 168, fuelBurn = (168 * 3.6).toInt, speed = 834, range = 4075, price = 70000000, lifespan = 25 * 52, constructionTime = 20, Manufacturer("Comac", countryCode = "CN"), runwayRequirement = 2000, imageUrl = "https://www.norebbo.com/comac-c919-side-view/"),
	Model("Comac C919ER", "Comac C", capacity = 168, fuelBurn = (168 * 3.8).toInt, speed = 834, range = 5555, price = 73000000, lifespan = 25 * 52, constructionTime = 20, Manufacturer("Comac", countryCode = "CN"), runwayRequirement = 2000, imageUrl = "https://www.norebbo.com/comac-c919-side-view/"),
	Model("CRAIC CR929-500", "CRAIC CR", capacity = 390, fuelBurn = (390 * 4.7).toInt, speed = 908, range = 14000, price = 240000000, lifespan = 25 * 52, constructionTime = 30, Manufacturer("Comac", countryCode = "CN"), runwayRequirement = 3000, imageUrl = "https://www.norebbo.com/comac-c919-side-view/"),
	Model("CRAIC CR929-600", "CRAIC CR", capacity = 440, fuelBurn = (440 * 4.9).toInt, speed = 908, range = 12000, price = 275000000, lifespan = 25 * 52, constructionTime = 30, Manufacturer("Comac", countryCode = "CN"), runwayRequirement = 3150, imageUrl = "https://www.norebbo.com/comac-c919-side-view/"),
	Model("CRAIC CR929-700", "CRAIC CR", capacity = 500, fuelBurn = (500 * 5.1).toInt, speed = 908, range = 10000, price = 310000000, lifespan = 25 * 52, constructionTime = 36, Manufacturer("Comac", countryCode = "CN"), runwayRequirement = 3300, imageUrl = "https://www.norebbo.com/comac-c919-side-view/"))
  val modelByName = models.map { model => (model.name, model) }.toMap
}


