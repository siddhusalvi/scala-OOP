import java.io.FileWriter
import com.google.gson.Gson
import scala.io.Source

/*
Filename: commercial_data
Created: Siddhesh Salvi
Change history:17.2.2020 / Siddhesh Salvi
5. Commercial data processing - StockAccount.java implements a data type that
might be used by a financial institution to keep track of customer information. The
StockAccount class implements following methods
The StockAccount class also maintains a list of CompanyShares object which has
Stock Symbol and Number of Shares as well as DateTime of the transaction. When
buy or sell is initiated StockAccount checks if CompanyShares are available and
accordingly update or create an Object.
 */


//class stock account to manage stock data
class StockAccount {
  var len: Int = 0
  var head: Node = null

  //Function to get value of
  def valueOf(stock_name: String): Double = {
    if (isContains(stock_name)) {
      var temp = this.head
      while (!temp.share_name.equals(stock_name) && temp != null) {
        temp = temp.next
      }
      (temp.price * temp.quantity).toDouble
    } else {
      var zero: Double = 0f
      zero
    }
  }

  //Function to validate sell request
  def requestIsValid(stock_name: String, count: Int): Boolean = {
    if (isEmpty) {
      false
    } else if (isNotContains(stock_name)) {
      false
    } else {
      var temp_node = this.head
      while (!temp_node.share_name.equals(stock_name) && temp_node != null) {
        temp_node = temp_node.next
      }
      if (temp_node.quantity >= count) {
        true
      } else {
        false
      }
    }
  }

  //Function to sell shares
  def sell(stock_name: String, value: Double, count: Int): Unit = {
    if (isContains(stock_name)) {
      if (requestIsValid(stock_name, count)) {
        if (size == 1) {
          if (this.head.quantity == count) {
            this.head = null
            this.len -= 1
          } else {
            this.head.quantity = this.head.quantity - count
            this.head.price = value
          }

        } else if (this.head.share_name.equals(stock_name)) {
          if (this.head.quantity == count) {
            this.head = this.head.next
            this.len -= 1
          } else {
            this.head.quantity = this.head.quantity - count
            this.head.price = value
          }
        } else {
          var previous: Node = this.head
          var temp_node = this.head
          while (temp_node != null && !temp_node.share_name.equals(stock_name)) {
            previous = temp_node
            temp_node = temp_node.next
          }
          if (temp_node.quantity == count) {
            previous.next = temp_node.next
            this.len -= 1
          } else {
            temp_node.quantity = temp_node.quantity - count
            temp_node.price = value
          }
        }
      } else {
        print("request is not valid")
      }
    } else {
      print("\nShare Not found can't delete")
    }
  }

  //Function to buy shares
  def buy(stock_name: String, value: Double, count: Int): Unit = {
    if (isEmpty) {
      var temp_node = new Node(stock_name, value, count)
      this.head = temp_node
      this.len += 1
    } else {
      var new_node = new Node(stock_name, value, count)
      if (isNotContains(stock_name)) {
        var temp = this.head
        while (temp.next != null) {
          temp = temp.next
        }
        temp.next = new_node
        this.len += 1
      } else {
        var temp = this.head
        while (!temp.share_name.equals(stock_name) && temp != null) {
          temp = temp.next
        }
        temp.quantity = temp.quantity + count
        temp.price = value
      }
    }

  }

  //Function to check list not contains share or not
  def isNotContains(share: String): Boolean = {
    if (isContains(share)) {
      false
    } else {
      true
    }
  }

  //Function to check list contains share or not
  def isContains(share: String): Boolean = {
    if (isEmpty) {
      false
    } else {
      var temp_node = this.head
      while (temp_node != null) {
        if (temp_node.share_name.equals(share)) {
          return true
        }
        temp_node = temp_node.next
      }
      false
    }
  }

  //Function to save data
  def saveData():Unit={
    if(isNotEmpty){
      var data_arr = new Array[Data](this.len)
      var temp_node = this.head
      var index = 0
      while(temp_node!=isEmpty && index<this.len){
        data_arr(index) = new Data(temp_node.share_name,temp_node.price,temp_node.quantity)
        temp_node = temp_node.next
        index += 1
      }
      var json_mgr = new Gson()
      var json_str = json_mgr.toJson(data_arr)
      var path = "/home/admin1/IdeaProjects/OOPs/src/main/scala/JsonData/stocks.json"
      var file = new FileWriter(path)
      file.write(json_str)
      file.close()


    }else{
      print("Record is empty")
    }
  }

  //Function to load data
  def loadData():Unit={
    var path = "/home/admin1/IdeaProjects/OOPs/src/main/scala/JsonData/stocks.json"
    var json_arr = getFileData(path)
    var json_mgr = new Gson()
    var data_arr = json_mgr.fromJson(json_arr,classOf[Array[Data]])
    for(i <- data_arr){
      buy(i.share_name,i.price,i.quantity)
    }
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


  //Function to print list
  def printReport: Unit = {
    if (isNotEmpty) {
      var total: Double = 0
      var output = ""
      var temp: Node = this.head
      while (temp != null) {
        output += temp.share_name + "  " + temp.quantity + "  " + temp.price + " " + (temp.quantity * temp.price) + "\n"
        total += (temp.quantity * temp.price)
        temp = temp.next
      }
      output += "Total is : " + total
      print(output)
    } else {
      print("list is empty")
    }
  }

  //Function to check list is not empty
  def isNotEmpty: Boolean = {
    if (isEmpty) {
      false
    } else {
      true
    }
  }

  //function to check list is empty or not
  def isEmpty: Boolean = {
    if (size == 0) {
      true
    } else {
      false
    }
  }

  //Function to return length of list
  def size: Int = {
    this.len
  }

  //class Node to store data
  class Node {
    var share_name = ""
    var price: Double = 0
    var quantity: Int = 0
    var next: Node = _

    def this(stock_name: String, value: Double, lot: Int) {
      this()
      this.share_name = stock_name
      this.price = value
      this.quantity = lot
    }
  }

  class Data{
    var share_name = ""
    var price: Double = 0
    var quantity: Int = 0
    def this(stock_name: String, value: Double, lot: Int) {
      this()
      this.share_name = stock_name
      this.price = value
      this.quantity = lot
    }
  }

}

//class to manage company share
class CompanyShares{
  var len: Int = 0
  var head: Node = null

  //Function to get value of
  def valueOf(stock_name: String): Double = {
    if (isContains(stock_name)) {
      var temp = this.head
      while (!temp.share_name.equals(stock_name) && temp != null) {
        temp = temp.next
      }
      (temp.price * temp.quantity).toDouble
    } else {
      var zero: Double = 0f
      zero
    }
  }

  //Function to validate sell request
  def requestIsValid(stock_name: String, count: Int): Boolean = {
    if (isEmpty) {
      false
    } else if (isNotContains(stock_name)) {
      false
    } else {
      var temp_node = this.head
      while (!temp_node.share_name.equals(stock_name) && temp_node != null) {
        temp_node = temp_node.next
      }
      if (temp_node.quantity >= count) {
        true
      } else {
        false
      }
    }
  }

  //Function to sell shares
  def sell(stock_name: String, value: Double, count: Int): Unit = {
    if (isContains(stock_name)) {
      if (requestIsValid(stock_name, count)) {
        if (size == 1) {
          if (this.head.quantity == count) {
            this.head = null
            this.len -= 1
          } else {
            this.head.quantity = this.head.quantity - count
            this.head.price = value
          }

        } else if (this.head.share_name.equals(stock_name)) {
          if (this.head.quantity == count) {
            this.head = this.head.next
            this.len -= 1
          } else {
            this.head.quantity = this.head.quantity - count
            this.head.price = value
          }
        } else {
          var previous: Node = this.head
          var temp_node = this.head
          while (temp_node != null && !temp_node.share_name.equals(stock_name)) {
            previous = temp_node
            temp_node = temp_node.next
          }
          if (temp_node.quantity == count) {
            previous.next = temp_node.next
            this.len -= 1
          } else {
            temp_node.quantity = temp_node.quantity - count
            temp_node.price = value
          }
        }
      } else {
        print("request is not valid")
      }
    } else {
      print("\nShare Not found can't delete")
    }
  }

  //Function to Add shares
  def add(stock_name: String, value: Double, count: Int): Unit = {
    if (isEmpty) {
      var temp_node = new Node(stock_name, value, count)
      this.head = temp_node
      this.len += 1
    } else {
      var new_node = new Node(stock_name, value, count)
      if (isNotContains(stock_name)) {
        var temp = this.head
        while (temp.next != null) {
          temp = temp.next
        }
        temp.next = new_node
        this.len += 1
      } else {
        var temp = this.head
        while (!temp.share_name.equals(stock_name) && temp != null) {
          temp = temp.next
        }
        temp.quantity = temp.quantity + count
        temp.price = value
      }
    }
  }

  //Function to check list not contains share or not
  def isNotContains(share: String): Boolean = {
    if (isContains(share)) {
      false
    } else {
      true
    }
  }

  //Function to check list contains share or not
  def isContains(share: String): Boolean = {
    if (isEmpty) {
      false
    } else {
      var temp_node = this.head
      while (temp_node != null) {
        if (temp_node.share_name.equals(share)) {
          return true
        }
        temp_node = temp_node.next
      }
      false
    }
  }

  //Function to save data
  def saveData():Unit={
    if(isNotEmpty){
      var data_arr = new Array[Data](this.len)
      var temp_node = this.head
      var index = 0
      while(temp_node!=isEmpty && index<this.len){
        data_arr(index) = new Data(temp_node.share_name,temp_node.price,temp_node.quantity)
        temp_node = temp_node.next
        index += 1
      }
      var json_mgr = new Gson()
      var json_str = json_mgr.toJson(data_arr)
      var path = "/home/admin1/IdeaProjects/OOPs/src/main/scala/JsonData/CompanyStocks.json"
      var file = new FileWriter(path)
      file.write(json_str)
      file.close()


    }else{
      print("Record is empty")
    }
  }

  //Function to load data
  def loadData():Unit={
    var path = "/home/admin1/IdeaProjects/OOPs/src/main/scala/JsonData/CompanyStocks.json"
    var json_arr = getFileData(path)
    var json_mgr = new Gson()
    var data_arr = json_mgr.fromJson(json_arr,classOf[Array[Data]])
    for(i <- data_arr){
      add(i.share_name,i.price,i.quantity)
    }
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


  //Function to print list
  def printReport: Unit = {
    if (isNotEmpty) {
      var total: Double = 0
      var output = ""
      var temp: Node = this.head
      while (temp != null) {
        output += temp.share_name + "  " + temp.quantity + "  " + temp.price + " " + (temp.quantity * temp.price) + "\n"
        total += (temp.quantity * temp.price)
        temp = temp.next
      }
      output += "Total is : " + total
      print(output)
    } else {
      print("list is empty")
    }
  }

  //Function to check list is not empty
  def isNotEmpty: Boolean = {
    if (isEmpty) {
      false
    } else {
      true
    }
  }

  //function to check list is empty or not
  def isEmpty: Boolean = {
    if (size == 0) {
      true
    } else {
      false
    }
  }

  //Function to return length of list
  def size: Int = {
    this.len
  }

  //class Node to store data
  class Node {
    var share_name = ""
    var price: Double = 0
    var quantity: Int = 0
    var next: Node = _

    def this(stock_name: String, value: Double, lot: Int) {
      this()
      this.share_name = stock_name
      this.price = value
      this.quantity = lot
    }
  }

  class Data{
    var share_name = ""
    var price: Double = 0
    var quantity: Int = 0
    def this(stock_name: String, value: Double, lot: Int) {
      this()
      this.share_name = stock_name
      this.price = value
      this.quantity = lot
    }
  }

}

object commercial_data {
  def main(args: Array[String]): Unit = {
    var flag = true
    while (flag) {
      try {
        flag = false
      }
      catch {
        case _ => print("Something went wrong Error occurred.")
      }

    }
  }
}