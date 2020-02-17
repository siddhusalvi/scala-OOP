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

  //Function to save data
  def saveData(): Unit = {
    if (isNotEmpty) {
      var data_arr = new Array[Data](this.len)
      var temp_node = this.head
      var index = 0
      while (temp_node != isEmpty && index < this.len) {
        data_arr(index) = new Data(temp_node.share_name, temp_node.price, temp_node.quantity)
        temp_node = temp_node.next
        index += 1
      }
      var json_mgr = new Gson()
      var json_str = json_mgr.toJson(data_arr)
      var path = "/home/admin1/IdeaProjects/OOPs/src/main/scala/JsonData/stocks.json"
      var file = new FileWriter(path)
      file.write(json_str)
      file.close()


    } else {
      print("Record is empty")
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

  //Function to load data
  def loadData(): Unit = {
    var path = "/home/admin1/IdeaProjects/OOPs/src/main/scala/JsonData/stocks.json"
    var json_arr = getFileData(path)
    var json_mgr = new Gson()
    var data_arr = json_mgr.fromJson(json_arr, classOf[Array[Data]])
    for (i <- data_arr) {
      buy(i.share_name, i.price, i.quantity)
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
      println("User Portfolio : \n" +"Price\tQuantity\tamount\tprice")
      while (temp != null) {
        output += temp.price +"\t"+temp.quantity  + "\t"+ (temp.quantity * temp.price) +"\t"+temp.share_name +"\n"
        total += (temp.quantity * temp.price)
        temp = temp.next
      }
      output += "Total is : " + total
      print(output)
    } else {
      print("list is empty")
    }
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

  class Data {
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
class CompanyShares {
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

  //Function to save data
  def saveData(): Unit = {
    if (isNotEmpty) {
      var data_arr = new Array[Data](this.len)
      var temp_node = this.head
      var index = 0
      while (temp_node != isEmpty && index < this.len) {
        data_arr(index) = new Data(temp_node.share_name, temp_node.price, temp_node.quantity)
        temp_node = temp_node.next
        index += 1
      }
      var json_mgr = new Gson()
      var json_str = json_mgr.toJson(data_arr)
      var path = "/home/admin1/IdeaProjects/OOPs/src/main/scala/JsonData/CompanyStocks.json"
      var file = new FileWriter(path)
      file.write(json_str)
      file.close()


    } else {
      print("Record is empty")
    }
  }

  //Function to load data
  def loadData(): Unit = {
    var path = "/home/admin1/IdeaProjects/OOPs/src/main/scala/JsonData/CompanyStocks.json"
    var json_arr = getFileData(path)
    var json_mgr = new Gson()
    var data_arr = json_mgr.fromJson(json_arr, classOf[Array[Data]])
    for (i <- data_arr) {
      add(i.share_name, i.price, i.quantity)
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
      println("Share market listed Companies : ")
      println("\nPrice \tQuantity \tName ")
      while (temp != null) {
        output += temp.price + "\t" + temp.quantity + "\t " + temp.share_name + "\n"
        temp = temp.next
      }
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

  class Data {
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
    var company_data = new CompanyShares
    var stock_account = new StockAccount
    company_data.loadData()
    println("There is company list in Comapany Share class if you issue new company share and if its not present in the company share class then\n it will be direclty listed to the share market with initial share size 10000")
    println("Initial Stock market : ")
    company_data.printReport

    while (flag) {
      try {

        println("\nWhat do you want to do : ")
        println("1:buy shares \t2:sell shares \t3:View companies in share market \n4:View user data \t5:Save company data \t6:Load company data \t7:Save user data \t8:load user data 9:exit")


        var choice = scala.io.StdIn.readInt()
        if (choice == 9) {
          flag = false
        } else if (choice == 1) {
          buyShare(company_data, stock_account)
        } else if (choice == 2) {
          sellShare(company_data, stock_account)
        } else if (choice == 3) {
          company_data.printReport
        } else if (choice == 4) {
          stock_account.printReport
        } else if (choice == 5) {
          company_data.saveData()
          println("Operation successful")
        } else if (choice == 6) {
          company_data.loadData()
          println("Operation successful")

        } else if (choice == 7) {
          stock_account.saveData()
          println("Operation successful")
        }else if (choice == 8) {
          stock_account.loadData()
          println("Operation successful")
        } else {
          println("Enter valid input : ")
        }
      }
      catch {
        case exception1: NullPointerException => println("File is empty no data found")
        case exception2: NumberFormatException => println("Enter valid input")
        case _ => print("Something went wrong Error occurred.")
      }

    }
  }

  //Function to buy shares
  def buyShare(shares: CompanyShares, account: StockAccount): Unit = {
    var initialQuantity = 10000
    println("Enter the share name : ")
    var name = scala.io.StdIn.readLine()
    println("Enter the share price : ")
    var price = scala.io.StdIn.readDouble()
    println("Enter the share quantity : ")
    var quantity = scala.io.StdIn.readInt()
    if (shares.isContains(name)) {
      println("stock is available at market")
      shares.sell(name, price, quantity)
      account.buy(name, price, quantity)
      println("Operation successful")
    } else {
      println("stock is not available at market adding new stock : " + name + "with 10000 quantity :")
      shares.add(name, price, initialQuantity)
      shares.sell(name, price, quantity)
      account.buy(name, price, quantity)
      println("Operation successful")
    }

  }

  //Function to sell shares
  def sellShare(shares: CompanyShares, account: StockAccount): Unit = {

    println("Enter the share name : ")
    var name = scala.io.StdIn.readLine()
    println("Enter the share price : ")
    var price = scala.io.StdIn.readDouble()
    println("Enter the share quantity : ")
    var quantity = scala.io.StdIn.readInt()
    if (account.isNotContains(name)) {
      println("Share not found in record : ")
    } else if (!account.requestIsValid(name, quantity)) {
      println("Please enter valid sell request : ")
    } else {
      account.sell(name, price, quantity)
      shares.add(name, price, quantity)
    }
  }
}