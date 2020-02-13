import com.google.gson.Gson

import scala.io.Source

/*
Filename: inventory_manager
Created: Siddhesh Salvi
Change history:13.2.2020 / Siddhesh Salvi
3. Inventory Management Program
a. Desc -> Extend the above program to Create InventoryManager to manage the
Inventory. The Inventory Manager will use InventoryFactory to create Inventory
Object from JSON. The InventoryManager will call each Inventory Object in its list
to calculate the Inventory Price and then call the Inventory Object to return the
JSON String. The main program will be with InventoryManager
b. I/P -> read in JSON File
c. Logic -> Get JSON Object in Java or NSDictionary in iOS. Create Inventory
Object from JSON. Calculate the value for every Inventory.
d. O/P -> Create the JSON from Inventory Object and output the JSON String.
*/


object inventory_manager {
  def main(args: Array[String]): Unit = {
    var flag = true
    while (flag) {
      try {
        var mgr = new InventoryManager()
        var data = mgr.loadData()
        mgr.calculate_price(data)
        print("\nOutput Json file is " + mgr.produce_json(data))
        flag = false
      }
      catch {
        case _ => print("Something went wrong Error occurred.")
      }
    }
  }

  //Rice class to store rice data
  case class Inventory(grain: String, name: String, quantity: Int, price: Int) {
    def getString: String = {
      val strData = this.grain + " " + this.name + " " + this.quantity.toString + " " + this.price.toString
      strData
    }
  }

  //class Inventory manager to manage inventory
  class InventoryManager {
    //Function to load data from file to class
    def loadData(): Array[Inventory] = {
      var json_mgr = new Gson()
      var grain_data: String = "/home/admin1/IdeaProjects/OOPs/src/main/scala/JsonData/grains.json"
      var strGrains = getFileData(grain_data)
      var data: Array[Inventory] = json_mgr.fromJson(strGrains, classOf[Array[Inventory]])
      data
    }

    //Function to get data from file to the String
    def getFileData(path: String): String = {
      var file = path
      val data = Source.fromFile(file)
      var sentence: String = ""
      for (line <- data.getLines) {
        sentence += line
      }
      sentence
    }

    //Function to calculate total price of all objects
    def calculate_price(data: Array[Inventory]): Unit = {
      var total = 0
      for (i <- 0 until data.length) {
        var report_str = "Grain : " + data(i).grain.toString + "   Name : " + data(i).name.toString + "   Quantity : " + data(i).quantity.toString + "    Price: " + data(i).price.toString + "   Total Money : " + (data(i).quantity * data(i).price).toString + "\n"
        print(report_str)
        total += data(i).quantity * data(i).price
      }
      print("Total price of all items is : " + total)
    }

    //Function to generate json file from object array
    def produce_json(data: Array[Inventory]): String = {
      var json_mgr = new Gson()
      var json_str = json_mgr.toJson(data)
      json_str
    }
  }

}