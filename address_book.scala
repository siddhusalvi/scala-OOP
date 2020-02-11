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

import java.io.{File, PrintWriter}

object address_book {
  def main(args: Array[String]): Unit = {
    var flag = true
    while (flag) {
      try {
        print("How much People do you want to add in Address book : ")
        val people = scala.io.StdIn.readInt()
        var personData: Array[Person] = new Array[Person](people)
        var filled_persons = 0
        for (index <- personData.indices) {
          personData(index) = new Person()
        }
        var operation_flag = true
        while (operation_flag) {
          print("\nWhat do you want to do :\n1.add details \n2.delete detail \n3.Save details \n4.Edit details \n5.display details \n6.sort \n7.Exit  : \n")
          var choice = scala.io.StdIn.readInt()
          if (choice == 7) {
            operation_flag = false
          } else if (choice == 1) {
            if (filled_persons < people) {
              personData(filled_persons).addInfo()
              filled_persons += 1
            } else {
              print("\nCannot add more details Address book is full.\n")
            }
          } else if (choice == 2) {
            if (filled_persons == 0) {
              print("Address book is already empty\n")
            } else if (filled_persons == 1) {
              print("there is only single person data do you want to delete it ? (y) : ")
              val input = scala.io.StdIn.readLine()
              if (input.equals('y')) {
                personData = null
              }
              filled_persons -= 1
            } else {
              var new_person_data = delete_info(personData)
              personData = new_person_data
              filled_persons -= 1
            }

          } else if (choice == 3) {
            if (filled_persons == 0) {
              print("\nAddress book is empty!\n")
            } else {
              val writer = new PrintWriter(new File("/home/admin1/IdeaProjects/OOPs/src/main/scala/Addressbook.txt"))
              writer.write(getPersonData(personData))
              writer.close()
              print("Operation successful")
            }


          } else if (choice == 4) {
            if (filled_persons == 0) {
              print("\nAddress book is not filled\n")
            } else {
              var index = find_id_deletion(personData)
              if (index >= 0) {
                personData(index).editInfo()
              }
            }

          } else if (choice == 5) {
            for (index <- personData.indices) {
              personData(index).display()
            }
          } else if (choice == 6) {

          } else {
            print("\nInvalid input Please Enter valid input : \n")
          }
        }


        flag = false
      }
      catch {
        case _ => print("Something went wrong Error occurred.")
      }
    }
  }

  //Function to delete user
  def delete_info(people: Array[Person]): Array[Person] = {
    var operation_flag: Boolean = true
    while (operation_flag) {
      print("Enter user's first name to delete its record : ")
      var name = scala.io.StdIn.readLine()
      var present_flag: Boolean = false
      var index = 0
      for (man <- people.indices) {
        if (people(man).first_name.equals(name)) {
          print("First name is present." + index)
          present_flag = true
          operation_flag = false
          var new_data = delete_user(people, index)
          print("Informataion deleted.")
          return new_data
        }
        if (!present_flag) {
          index += 1
        }
      }
      if (!present_flag) {
        print("User not fount do you still want to delete (y) : ")
        var input = scala.io.StdIn.readLine()
        if (!input.equals("y")) {
          operation_flag = false
        }
      }
    }
    people
  }
  //Function to delete user by its index
  def delete_user(people: Array[Person], index: Int): Array[Person] = {
    var _temp_array: Array[Person] = new Array[Person](people.length - 1)
    for (index <- people.indices) {
      if (index != index) {
        _temp_array(index) = people(index)
      }
    }
    _temp_array
  }
  //Function to find user id for its deletion
  def find_id_deletion(people: Array[Person]): Int = {
    var check_flag = true
    while (check_flag) {
      print("Enter user firstname to edit its details : ")
      var username = scala.io.StdIn.readLine()
      var index = 0
      for (person <- people) {
        if (person.first_name.equals(username)) {
          return index
        }
        index += 1
      }
      print("User not found Do you still want to continue (y): ")
      var input = scala.io.StdIn.readLine()
      if (!input.equals("y")) {
        check_flag = false
      }
    }
    -1
  }
  //function to get user data in string format
  def getPersonData(people: Array[Person]): String = {
    var user_data = " "
    for (index <- people) {
      user_data += "First name : " + index.first_name + "\n"
      user_data += "Last name : " + index.last_name + "\n"
      user_data += "Address : " + index.address + "\n"
      user_data += "City : " + index.city + "\n"
      user_data += "Zip code : " + index.zip + "\n"
      user_data += "Phone number : " + index.phone + "\n"

    }
    user_data
  }
  //function to sort user by last name
  def sortByLastname(people: Array[Person]): Array[Person] = {
    for (outer <- 0 until people.length - 1) {
      for (inner <- 0 until people.length - 1 - outer) {
        if (people(inner).last_name.compareTo(people(inner + 1).last_name) > 0) {
          var temp = people(inner)
          people(inner) = people(inner + 1)
          people(inner + 1) = temp
        }
      }
    }
    people
  }

  class Person {
    var first_name: String = _
    var last_name: String = _
    var address: String = _
    var city: String = _
    var state: String = _
    var zip: Long = _
    var phone: Long = _

    //Function to add data in contacts
    def addInfo(): Unit = {
      println("Enter the first name : ")
      this.first_name = scala.io.StdIn.readLine()

      println("Enter the last name : ")
      this.last_name = scala.io.StdIn.readLine()

      println("Enter your Address : ")
      this.address = scala.io.StdIn.readLine()

      println("Enter your City : ")
      this.city = scala.io.StdIn.readLine()

      var input_flag = true
      while (input_flag) {
        try {
          println("Enter Your zip code : ")
          this.zip = scala.io.StdIn.readLong()
          if (zip >= 100000 && zip <= 999999) {
            input_flag = false
          }
        } catch {
          case _ => {
            print("Please enter valid input : \n")
          }
        }
      }
      input_flag = true
      while (input_flag) {
        try {
          println("Enter Your Phone number : ")
          this.phone = scala.io.StdIn.readLong()
          if (phone.toString.length == 10) {
            input_flag = false
          }
        } catch {
          case _ => {
            print("Please enter valid input : \n")
          }
        }
      }
    }

    //Function to display data
    def display(): Unit = {
      if (this == null) {
        print("Adressbook  is empty ")
      } else {
        var data: String = ""
        data += "First name : " + this.first_name + "\n"
        data += "Last name : " + this.last_name + "\n"
        data += "Address : " + this.address + "\n"
        data += "City : " + this.city + "\n"
        data += "Zip code : " + this.zip + "\n"
        data += "Phone number : " + this.phone + "\n"
        print(data)
        println()
      }

    }

    //Function to edit user data
    def editInfo(): Unit = {
      var choice_flag = true
      while (choice_flag) {
        print("\nWhat do you want to edit ? :\n" +
          "1.Address\n2.City\n3.Zip code\n4.Phone number")
        var choice = scala.io.StdIn.readInt()
        if (choice == 1) {
          println("Enter your Address : ")
          this.address = scala.io.StdIn.readLine()


        } else if (choice == 2) {
          println("Enter your City : ")
          this.city = scala.io.StdIn.readLine()

        } else if (choice == 3) {
          var input_flag = true
          while (input_flag) {
            try {
              println("Enter Your zip code : ")
              this.zip = scala.io.StdIn.readLong()
              if (zip >= 100000 && zip <= 999999) {
                input_flag = false
              }
            } catch {
              case _ => {
                print("Please enter valid input : \n")
              }
            }
          }


        } else if (choice == 4) {

          var input_flag = true
          while (input_flag) {
            try {
              println("Enter Your Phone number : ")
              this.phone = scala.io.StdIn.readLong()
              if (phone.toString.length == 10) {
                input_flag = false
              }
            } catch {
              case _ => {
                print("Please enter valid input : \n")
              }
            }
          }
        } else {
          print("please enter valid choice : \n")
        }
        print("Do you want more edits (n) :")
        var ans: String = scala.io.StdIn.readLine()
        if (ans.equals("n")) {
          choice_flag = false
        }

      }
    }
  }
} 