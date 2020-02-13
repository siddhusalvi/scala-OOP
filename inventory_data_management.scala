import java.io.FileWriter
import com.google.gson.Gson
import scala.io.Source

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

  def main(args: Array[String]): Unit = {
    var flag = true
    var load_flag = false
    var add_flag = false
    var json_mgr = new Gson()
    var grains = loadData()
    while (flag) {

      print("1:load data from JSON \t2:Create new data \t3:Calculate total report \t4:Save data \t5:exit : ")
      var choice = scala.io.StdIn.readInt()
      if (choice == 5) {
        flag = false
      } else if (choice == 1) {
        grains = loadData()
        print("Data loaded from JSON : ")
        println("\nRice : ")
        for (i <- grains) {
          println("\t" + i.getString)
        }
        load_flag = true
      } else if (choice == 2) {
        grains = createData()
        add_flag = true
      } else if (choice == 3) {
        if(load_flag || add_flag){
          calculateReport(grains)
        } else{
          print("Please add data to inventory to create report \n")
        }

      } else if (choice == 4) {
        if(load_flag || add_flag){
          saveData(grains)
          println("Data saved in JSON file")
        } else{
          print("Please add data to inventory to store it in local file \n")
        }

      }else {
        print("Please enter valid input : ")
      }
    }
  }

  //Function to load data from file to class
  def loadData():Array[Inventory]={
    var json_mgr = new Gson()
    var grain_data: String = "/home/admin1/IdeaProjects/OOPs/src/main/scala/JsonData/grains.json"
    var strGrains = getFileData(grain_data)
    var data:Array[Inventory] = json_mgr.fromJson(strGrains, classOf[Array[Inventory]])
    data
  }

  //Function to create new report
  def createData(): Array[Inventory] = {
    print("How many whole grains do you want to add ")
    var grain_count = scala.io.StdIn.readInt()
    var data: Array[Inventory] = new Array[Inventory](grain_count)
    for (i <- data.indices) {
      println("Enter the grain : ")
      var grain = scala.io.StdIn.readLine()
      println("Enter the name of grain ")
      var name = scala.io.StdIn.readLine()
      println("Enter the quantity : ")
      var quantity = scala.io.StdIn.readInt()
      println("Enter the price : ")
      var price = scala.io.StdIn.readInt()
      data(i) = Inventory(grain, name, quantity, price)
      println(data(i).getString)
    }
    data
  }

  //Function to calculate report
  def calculateReport(data:Array[Inventory]): Unit ={
    println()
    var report_str:String = ""
    for(i <-data.indices){
      report_str += "Grain : "+ data(i).grain.toString + "   Name : " + data(i).name.toString + "   Quantity : " + data(i).quantity.toString + "    Price: " + data(i).price.toString + "   Total Money : " + (data(i).quantity * data(i).price).toString + "\n"
    }
    println(report_str)
  }
  //Function to save data in json
  def saveData(data: Array[Inventory]):Unit ={
    var json_mgr = new Gson()
    var grain_data: String = "/home/admin1/IdeaProjects/OOPs/src/main/scala/JsonData/grains.json"
    var grain_file = new FileWriter(grain_data)
    var json_str = json_mgr.toJson(data)
    grain_file.write(json_str)
    println("Following data will be stored in file : "+json_str)
    grain_file.close()
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

  //Rice class to store rice data
  case class Inventory(grain: String, name: String, quantity: Int, price: Int) {
    def getString: String = {
      val strData = this.grain + " " + this.name + " " + this.quantity.toString + " " + this.price.toString
      strData
    }
  }
}