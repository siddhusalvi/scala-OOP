import java.lang

import com.fasterxml.jackson.databind.ObjectMapper
import com.google.gson.Gson

import scala.util.parsing.json.{JSON, Parser}
import scala.io.Source
import play.api.libs.json._
import net.liftweb.json._
import net.liftweb.json.DefaultFormats

import scala.collection.mutable._
import scala.util.parsing.combinator.JavaTokenParsers


/*
Filename: inventory_data_management
Created: Siddhesh Salvi
Change history:11.2.2020 / Siddhesh Salvi
2. JSON Inventory Data Management of Rice, Pulses and Wheats
a. Desc -> Create a JSON file having Inventory Details for Rice, Pulses and Wheats
with properties name, weight, price per kg.
b. Use Library : Java JSON Library, For IOS JSON Library use
NSJSONSerialization for parsing the JSON.
c. I/P -> read in JSON File
d. Logic -> Get JSON Object in Java or NSDictionary in iOS. Create Inventory
Object from JSON. Calculate the value for every Inventory.
e. O/P -> Create the JSON from Inventory Object and output the JSON String
*/


object inventory_data_management {
  case class Rice(name:String, quantity:Int, price: Int){
    def getString(): String ={
      var strData = name +" "+ quantity.toString +" "+price.toString
      strData
    }

  }

  case class Wheat(name:String, quantity:Int, price: Int){
    def getString(): String ={
      var strData = name +" "+ quantity.toString +" "+price.toString
      strData
    }
  }

  case class Pulse(name:String, quantity:Int, price: Int){
    def getString(): String ={
      var strData = name +" "+ quantity.toString +" "+price.toString
      strData
    }
  }





  def main(args: Array[String]): Unit = {



    var pulese:Array[Pulse] = new Array[Pulse](3)

    pulese(0) = new Pulse("basmati",100,50)
    pulese(1) = new Pulse("wadaKolam",100,30)
    pulese(2) = new Pulse("biryani",80,40)

    var wheat:Array[Wheat] = new Array[Wheat](3)

    wheat(0) = new Wheat("basmati",100,50)
    wheat(1) = new Wheat("wadaKolam",100,30)
    wheat(2) = new Wheat("biryani",80,40)


    var rice:Array[Rice] = new Array[Rice](3)

    rice(0) = new Rice("basmati",100,50)
    rice(1) = new Rice("wadaKolam",100,30)
    rice(2) = new Rice("biryani",80,40)






    var gson = new Gson()

    var jsonStr = gson.toJson(pulese)
    println(jsonStr)

    var temp = gson.fromJson(jsonStr,classOf[Array[Pulse]])

    for(i <- temp.indices){
      println(temp(i).getString())
    }




  }






}