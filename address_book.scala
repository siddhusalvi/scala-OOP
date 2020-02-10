

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


object address_book {
  def main(args: Array[String]): Unit = {
    var flag = true
    while (flag) {
      try {

        print("How much People do you want to add in Address book : ")
        var people = scala.io.StdIn.readInt()
        var personData: Array[Person] = new Array[Person](people)
        for (i <- personData.indices) {
          personData(i) = new Person()
          personData(i).addInfo()
          delete_info(personData)
        }

        flag = false
      }
      catch {
        case _ => print("Something went wrong Error occurred.")
      }
    }
  }

  def sort_By_Lastname(people: Array[Person]): Array[Person] = {
    for (i <- 0 until people.length - 1) {
      for (j <- 0 until people.length - 1 - i) {
        if (people(j).last_name.compareTo(people(j + 1).last_name) > 0) {
          var temp = people(j)
          people(j) = people(j + 1)
          people(j + 1) = temp
        }
      }
    }
    people
  }
  def delete_info(people: Array[Person]): Array[Person] = {
     var operation_flag:Boolean = true
    while(operation_flag) {
      print("Enter user's first name to delete its record : ")
      var name = scala.io.StdIn.readLine()
      var presnt_flag: Boolean = false
      var index = 0
      for (i <- people.indices) {
        if (people(i).first_name.equals(name)) {
          print("First name is present."+index)
          presnt_flag = true
          operation_flag = false

        }

        if(!presnt_flag){
          index += 1
        }

      }
      if(!presnt_flag){
        print("User not fount do you still want to delete (y) : ")
        var input = scala.io.StdIn.readLine()
        if(!input.equals("y")){

          operation_flag = false
        }
      }
    }
    people
  }

  def delete_user(people: Array[Person],name:String): Array[Person] = {


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

      var inputflag = true
      while (inputflag) {
        try {
          println("Enter Your zip code : ")
          this.zip = scala.io.StdIn.readLong()
          if (zip >= 100000 && zip <= 999999) {
            inputflag = false
          }
        } catch {
          case _ => {
            print("Please enter valid input : \n")
          }
        }
      }
      inputflag = true
      while (inputflag) {
        try {
          println("Enter Your Phone number : ")
          this.phone = scala.io.StdIn.readLong()
          if (phone.toString.length == 10) {
            inputflag = false
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
      var data: String = ""
      data += "First name : " + this.first_name + "\n"
      data += "Last name : " + this.last_name + "\n"
      data += "Address : " + this.address + "\n"
      data += "City : " + this.city + "\n"
      data += "Zip code : " + this.zip + "\n"
      data += "Phone number : " + this.phone + "\n"
      print(data)
    }


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
          var inputflag = true
          while (inputflag) {
            try {
              println("Enter Your zip code : ")
              this.zip = scala.io.StdIn.readLong()
              if (zip >= 100000 && zip <= 999999) {
                inputflag = false
              }
            } catch {
              case _ => {
                print("Please enter valid input : \n")
              }
            }
          }


        } else if (choice == 4) {

          var inputflag = true
          while (inputflag) {
            try {
              println("Enter Your Phone number : ")
              this.phone = scala.io.StdIn.readLong()
              if (phone.toString.length == 10) {
                inputflag = false
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