import java.io.FileWriter

import com.google.gson.Gson

import scala.io.Source

/*
Filename: address_book

Created: Siddhesh Salvi
Change history:10.2.2020 / Siddhesh Salvi

The software to be designed is a program that can be used to maintain an address book. An address book
holds a collection of entries, each recording a person's first and last names, address, city, state, zip, and
phone number.
It must be possible to add a new person to an address book, to edit existing information about a person
(except the person's name), and to delete a person. It must be possible to sort the entries in the address
book alphabetically by last name (with ties broken by first name if necessary), or by ZIP code (with ties
broken by name if necessary). It must be possible to print out all the entries in the address book in
"mailing label" format.
It must be possible to create a new address book, to open a disk file containing an existing address book to
close an address book, and to save an address book to a disk file, using standard New, Open, Close, Save
and Save As ... File menu options. The program's File menu will also have a Quit option to allow closing
all open address books and terminating the program.
*/

//class person to store address book entries
class Person {
  var name_first = ""
  var name_last = ""
  var address = ""
  var city = ""
  var state = ""
  var zip = 0
  var contact = 0L

  //Constructor
  def this(f_name: String, l_name: String, given_address: String, given_city: String, given_state: String, code: Int, number: Long) {
    this()
    this.name_first = f_name
    this.name_last = l_name
    this.address = given_address
    this.city = given_city
    this.state = given_state
    this.zip = code
    this.contact = number
  }

  //Converting detail in String
  def getString: String = {
    "First name : " + this.name_first + "\n" + "Last name : " + this.name_last + "\n" + "Address : " + this.address + "\n" + "City : " + this.city + "\n" + "State : " + this.state + "\n" + "Zip code : " + this.zip + "\n" + "Contack no : " + this.contact + "\n"
  }


}

object address_book {
  def main(args: Array[String]): Unit = {
    var flag = true
    var register = getEmptyContact
    var len = 0
    var load_flag = false
    var new_flag = false
    while (flag) {
      try {
        println("What do you want to do ?")
        print("1.add user  2.Load address book from file  3.Edit details 4.Delete details 5.Save details 6.Display details 7.Exit ")
        var choice = scala.io.StdIn.readInt()
        if (choice == 7) {
          flag = false
        } else if (choice == 1) {
          register = add_user(register)
          len = getLength(register)
        } else if (choice == 2) {
          register = loadData
          len = getLength(register)
          print("Data is loaded from files \n")
        } else if (choice == 3) {
          if (len == 0) {
            println("There is no record in address book can't edit.")
          } else {
            register = editDetails(register)
            len = getLength(register)
          }

        } else if (choice == 4) {
          if (len == 0) {
            println("There is no record in address book can't delete.")
          } else {
            register = deleteDetails(register)
            len = getLength(register)
          }

        } else if (choice == 5) {
          saveData(register)
        } else if (choice == 6) {
          if (len == 0) {
            println("There is no record in address book can't display")
          } else {
            println("Data is :")
            printDetails(register)
          }

        } else {
          print("\nYou have entered invalid choice : \n")
        }
      }
      catch {
        case a: NullPointerException => {
          println("file is empty can't load data :")
        }
        case _ => print("Something went wrong Error occurred.\n")
      }
    }

  }

  //============================================================================ main function close

  //Function to update length
  def getLength(persons: Array[Person]): Int = {
    return persons.length
  }

  def add_user(persons: Array[Person]): Array[Person] = {
    var new_person = getDeatailsFromUser()
    if (persons.length == 0) {
      var temp = new Array[Person](1)
      temp(0) = new_person
      return temp
    } else {
      var temp = new Array[Person](persons.length + 1)
      for (i <- 0 until persons.length) {
        temp(i) = persons(i)
      }
      temp(persons.length) = new_person
      return temp
    }
  }

  //Function to load data from file
  def loadData: Array[Person] = {
    var json_mgr = new Gson()
    var address_book: String = "/home/admin1/IdeaProjects/OOPs/src/main/scala/JsonData/contact.json"
    var strGrains = getFileData(address_book)
    var data: Array[Person] = json_mgr.fromJson(strGrains, classOf[Array[Person]])
    return data
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

  //Function to edit details
  def editDetails(persons: Array[Person]): Array[Person] = {
    var check_flag = true
    while (check_flag) {
      print("Enter user first name search its details : ")
      var name = scala.io.StdIn.readLine()
      if (userIsPresent(persons, name)) {
        var index = findUserIndex(persons, name)
        persons(index) = modifyUser(persons(index))
        return persons
        check_flag = false
      } else {
        println("User is absent do yo still want to continue (y)(n) : ")
        var choice = scala.io.StdIn.readLine()
        if (choice.equals("n")) {
          check_flag = false
        }
      }

    }
    persons
  }

  //Function to modify person
  def modifyUser(person: Person): Person = {
    var choice_flag = true
    while (choice_flag) {
      println("What do you want to edit : 1:address  2:City  3:State  4:Zip code 5: Contact number : 6:exit:")
      var choice = scala.io.StdIn.readInt()
      if (choice == 1) {
        print("Enter Person's Address : ")
        person.address = scala.io.StdIn.readLine()
      } else if (choice == 2) {
        print("Enter Person's City ")
        person.city = scala.io.StdIn.readLine()
      } else if (choice == 3) {
        print("Enter Person's State :")
        person.state = scala.io.StdIn.readLine()
      } else if (choice == 4) {
        var zip = 0
        var check_flag = true
        while (check_flag) {
          print("Enter Person's zip code : ")
          zip = scala.io.StdIn.readInt()
          if (zip.toString.length == 6) {
            person.zip = zip
            check_flag = false
          } else {
            print("Enter valid zip code :\n")
          }
        }
      } else if (choice == 5) {
        var phone = 0L
        var check_flag = true
        while (check_flag) {
          print("Enter Person's phone no : ")
          phone = scala.io.StdIn.readLong()
          if (phone.toString.length == 10) {
            person.contact = phone
            check_flag = false
          } else {
            print("Enter valid phone number :\n")
          }
        }
      } else if (choice == 6) {
        choice_flag = false
      } else {
        print("\n Enter valid choice : ")
      }
    }
    person
  }

  //Function to find user is valid or not
  def userIsPresent(persons: Array[Person], name: String): Boolean = {
    for (i <- persons) {
      if (i.name_first.equals(name)) {
        return true
      }
    }
    false
  }

  //Function to find user index to delete it or edit it
  def findUserIndex(persons: Array[Person], name: String): Int = {
    for (index <- persons.indices) {
      if (persons(index).name_first.equals(name)) {
        return index
      }
    }
    -1
  }

  //Function to print details
  def printDetails(persons: Array[Person]): Unit = {
    for (i <- persons) {
      println(i.getString)
    }
  }

  //Function to delete details
  def deleteDetails(persons: Array[Person]): Array[Person] = {
    var choice_flag = true
    print("Enter User'first name to delete its record ")
    var name = scala.io.StdIn.readLine()
    if (userIsPresent(persons, name)) {
      var index = findUserIndex(persons, name)
      var temp = deleteUser(persons, index)
      return temp
    } else {
      print("\nUser not found")
      persons
    }
  }

  //Function to delete user
  def deleteUser(persons: Array[Person], index: Int): Array[Person] = {
    if (persons.length == 1) {
      return getEmptyContact
    } else {
      var len = persons.length - 1
      var temp_index = 0
      var temp: Array[Person] = new Array[Person](len)
      for (i <- 0 until persons.length) {
        if (i != index) {
          temp(temp_index) = persons(i)
          temp_index += 1
        }
      }
      temp

    }
  }

  //Function to return empty person data
  def getEmptyContact: Array[Person] = {
    new Array[Person](0)
  }

  //Delete user
  def saveData(persons: Array[Person]): Unit = {
    var path = "/home/admin1/IdeaProjects/OOPs/src/main/scala/JsonData/contact.json"
    var filemanager = new FileWriter(path)
    var gson = new Gson()
    var bookstr = gson.toJson(persons)
    println(bookstr)
    filemanager.write(bookstr)
    println("data saved in file.\n")
    filemanager.close()
  }

  //Function to create new address book
  def newRegister: Array[Person] = {
    var error_flag = true
    var people = 0
    while (error_flag) {
      print("\nHow many contacts do you want to create : ")
      people = scala.io.StdIn.readInt()
      if (people > 0) {
        error_flag = false
      } else {
        print("You can not create " + people + " Entries\n")
      }
    }
    var book = new Array[Person](people)
    for (i <- book.indices) {
      println("Enter Person " + (i + 1) + " data : ")
      book(i) = getDeatailsFromUser()
    }
    return book
  }

  //Function to take input from user
  def getDeatailsFromUser(): Person = {

    print("Enter Person's first name : ")
    var f_name = scala.io.StdIn.readLine()

    print("Enter Person's last name : ")
    var l_name = scala.io.StdIn.readLine()

    print("Enter Person's Address : ")
    var address = scala.io.StdIn.readLine()

    print("Enter Person's City ")
    var city = scala.io.StdIn.readLine()

    print("Enter Person's State :")
    var state = scala.io.StdIn.readLine()

    var zip = 0

    var check_flag = true
    while (check_flag) {
      print("Enter Person's zip code : ")
      zip = scala.io.StdIn.readInt()
      if (zip.toString.length == 6) {
        check_flag = false
      } else {
        print("Enter valid zip code :\n")
      }
    }
    var phone = 0L
    check_flag = true
    while (check_flag) {
      print("Enter Person's phone no : ")
      phone = scala.io.StdIn.readLong()
      if (phone.toString.length == 10) {
        check_flag = false
      } else {
        print("Enter valid phone number :\n")
      }
    }

    var person = new Person(f_name, l_name, address, city, state, zip, phone)


    person

  }
}


