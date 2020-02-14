import scala.io.Source

/*
Filename: stock_account
Created: Siddhesh Salvi
Change history:13.2.2020 / Siddhesh Salvi
4. Stock Account Management
a. Desc -> Write a program to read in Stock Names, Number of Share, Share Price.
Print a Stock Report with total value of each Stock and the total value of Stock.
b. I/P -> N number of Stocks, for Each Stock Read In the Share Name, Number of
Share, and Share Price
c. Logic -> Calculate the value of each stock and the total value
d. O/P -> Print the Stock Report.
e. Hint -> Create Stock and Stock Portfolio Class holding the list of Stocks read
from the input file. Have functions in the Class to calculate the value of each
stock and the value of total stocks
*/
object stock_account {
  def main(args: Array[String]): Unit = {
    var flag = true
    while (flag) {
      //      try{
      var path = "/home/admin1/IdeaProjects/OOPs/src/main/scala/JsonData/stocks"
      var file_data = getFileData(path)
      var data_arr = file_data.split(" ")
      if (data_arr.length % 3 != 0) {
        println("Error occured while opening stock file")
      }
      var stock_count = data_arr.length / 3
      var stocks = new Array[Stock](stock_count)

      for (index <- 0 until stocks.length) {
        var obj_index = index * 3
        var stock_name = data_arr(obj_index)
        var stock_count = data_arr(obj_index + 1).toInt
        var stock_price = data_arr(obj_index + 2).toInt
        stocks(index) = new Stock(stock_name, stock_count, stock_price)
        println("Stock is : "+stocks(index).getString)
      }
      calculate_report(stocks)

      flag = false
      //      }
      //      catch{
      //        case _=>print("Something went wrong Error occurred.")
      //      }
    }
  }

  def calculate_report(data: Array[Stock]): Unit = {
    var total = 0
    println()
    println("Stock report is : ")
    for (i <- 0 until data.length) {
      var amount = data(i).price * data(i).count
      total += amount
      println(data(i).name +" :  "+data(i).price+" * "+data(i).count+" = amount " + amount)
    }
    println("Total Price of all shares : " + total)

  }

  //Function to get data from file to the String
  def getFileData(path: String): String = {
    var file = path
    val data = Source.fromFile(file)
    var sentence: String = ""
    for (line <- data.getLines) {
      sentence += line + " "
    }
    sentence
  }

  class Stock {
    var name = ""
    var count = 0
    var price = 0

    def this(name: String, count: Int, price: Int) {
      this()
      this.name = name
      this.count = count
      this.price = price
    }

    def getString(): String = {
      this.name + " " + this.count.toString + " " + this.price.toString
    }
  }
}