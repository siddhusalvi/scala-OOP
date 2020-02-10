import scala.util.Random

/*
Filename: DeckOfCardsPlus

Created: Siddhesh Salvi
Change history:10.2.2020 / Siddhesh Salvi

Write a Program DeckOfCards.java, to initialize deck of cards having suit ("Clubs",
"Diamonds", "Hearts", "Spades") & Rank ("2", "3", "4", "5", "6", "7", "8", "9", "10",
"Jack", "Queen", "King", "Ace"). Shuffle the cards using Random method and then
distribute 9 Cards to 4 Players and Print the Cards the received by the 4 Players
using 2D Array
Extend the above program to create a Player Object having Deck of Cards, and
having ability to Sort by Rank and maintain the cards in a Queue implemented using
Linked List. Do not use any Collection Library. Further the Player are also arranged
in Queue. Finally Print the Player and the Cards received by each Player.

*/


object deckOfCardsPlus {
  def main(args: Array[String]): Unit = {
    var flag = true
    while (flag) {
      try {
        var total_cards = 52
        var card_Flag = true
        var pack: Cards = new Cards()
        while (card_Flag) {
          print("Enter the count of players : ")
          var players = scala.io.StdIn.readInt()
          if (players < 1 || players > total_cards) {
            print("Please enter valid players\n")
          } else {
            card_Flag = false
          }
          pack.shuffle()
          var card_set = total_cards / players
          println("Each player will get " + card_set + " Cards")


          var playerArray: Array[Player] = new Array[Player](players)
          for (i <- playerArray.indices) {
            playerArray(i) = new Player()
          }

          for (i <- playerArray.indices) {
            for (j <- 0 until card_set) {
              playerArray(i).add(pack.get_card())
            }
          }

          for (i <- playerArray.indices) {
            playerArray(i).display()
          }
        }
        flag = false
      } catch {
        case _ => {
          print("Something went wrong! Exception occurred")
        }
      }
    }

  }

  //Class to store card data
  class Cards {
    var total_cards = 52
    var current_card = 0
    var pack: Array[Int] = new Array[Int](total_cards)

    for (i <- 0 until pack.length) {
      pack(i) = i
    }

    //Fuction to reset cards
    def reset(): Unit = {
      for (i <- pack.indices) {
        pack(i) = i
      }
    }

    //function to shuffle cards
    def shuffle(): Unit = {
      for (i <- 0 until pack.length) {
        var rnd: Int = Random.nextInt(pack.length - 1)
        var temp: Int = pack(rnd)
        pack(rnd) = pack(i)
        pack(i) = temp
      }
    }

    //Function to display pack
    def display(): Unit = {
      println(pack.mkString(" "))
    }

    //Function to distribute cards in players
    def distribute(players: Int): Unit = {
      if (players < 0 || players > 52) {
        println("Invalid input Not possible to distribute : ")
      } else {
        val card_set = total_cards / players
        println("Each player will get " + card_set + " Cards")
        var distributed_cards = Array.ofDim[String](players, card_set)
        for (i <- 0 until players) {
          for (j <- 0 until card_set) {
            distributed_cards(i)(j) = get_card().toString
          }
        }
        for (i <- 0 until players) {
          print("player " + (i + 1) + " : ")
          for (j <- 0 until card_set) {
            printCard(distributed_cards(i)(j).toInt)
            print(" - ")
          }
          println()
        }
      }
    }

    //Function to print given card by its number
    def printCard(given_card: Int): Unit = {
      if (given_card <= 12) {
        val card_type = "♣"
        var card_str = card_type + " "
        val type_card = given_card % 13
        if (type_card == 9) {
          card_str += "Jack"
        } else if (type_card == 10) {
          card_str += "♛"
        } else if (type_card == 11) {
          card_str += "♚"
        } else if (type_card == 12) {
          card_str += "Ace"
        } else {
          card_str += (type_card + 2).toString
        }
        print(card_str)

      } else if (given_card <= 25) {

        val card_type = "♦"
        var card_str = card_type + ""
        val type_card = given_card % 13
        if (type_card == 9) {
          card_str += "Jack"
        } else if (type_card == 10) {
          card_str += "♛"
        } else if (type_card == 11) {
          card_str += "♚"
        } else if (type_card == 12) {
          card_str += "Ace"
        } else {
          card_str += (type_card + 2).toString
        }
        print(card_str)

      } else if (given_card <= 38) {
        val card_type = "♥"
        var card_str = card_type + ""
        val type_card = given_card % 13
        if (type_card == 9) {
          card_str += "Jack"
        } else if (type_card == 10) {
          card_str += "♛"
        } else if (type_card == 11) {
          card_str += "♚"
        } else if (type_card == 12) {
          card_str += "Ace"
        } else {
          card_str += (type_card + 2).toString
        }
        print(card_str + " ")

      } else if (given_card <= 51) {
        val card_type = "♠ "
        var card_str = card_type + " "
        val type_card = given_card % 13
        if (type_card == 9) {
          card_str += "Jack"
        } else if (type_card == 10) {
          card_str += "♛"
        } else if (type_card == 11) {
          card_str += "♚"
        } else if (type_card == 12) {
          card_str += "Ace"
        } else {
          card_str += (type_card + 2).toString
        }
        print(card_str)
      } else {
        print("card is out of index ")
      }
    }

    //Function to distribute card
    def get_card(): Int = {
      var card = pack(current_card)
      current_card += 1
      card
    }


  }

  //Class to store Player data
  class Player() {
    var myPack: Queue = new Queue()
    var cat: Cards = new Cards()

    //Function to add card in queue
    def add(num: Int): Unit = {
      myPack.enqueue(num)
    }

    //Function to display user cards
    def display(): Unit = {
      print("Cards are : ")
      while (myPack.isNotEmpty) {
        cat.printCard(myPack.first())
        print("    ")
        myPack.dequeue()
      }
      println()
    }


  }

  //class Linked list
  class Queue {

    var len: Int = 0
    var front: Node = _
    var rear: Node = _
    var min: Int = 0
    var max: Int = 0

    //Function to display queue
    def display(): Unit = {
      if (isEmpty) {
        print("queue is Empty")
      } else {
        var temp = this.front
        while (temp != null) {
          print(temp.data + " ")
          temp = temp.next
        }
      }
    }

    //Function to check queue is empty
    def isEmpty: Boolean = {
      if (size == 0) {
        true
      } else {
        false
      }
    }

    //Function to check queue size
    def size: Int = {
      len
    }

    //Function to add data in queue
    def enqueue(num: Int): Unit = {
      var temp: Node = new Node(num)
      if (isEmpty) {
        this.front = temp
        this.rear = temp
        this.min = num
        this.max = num
      } else {
        if (num < this.min) {
          this.front.prev = temp
          temp.next = this.front
          front = temp
          this.min = num
        } else if (num > this.max) {
          this.rear.next = temp
          temp.prev = rear
          rear = temp
          this.max = num
        } else {
          var temp1 = this.front
          while (temp1.next.data < num) {
            temp1 = temp1.next
          }
          temp.next = temp1.next
          temp.prev = temp1
          temp1.next = temp
        }

      }
      len += 1
    }

    //Function to get min
    def getMin: Int = {
      this.min
    }

    //Function to get max
    def getMax: Int = {
      this.max
    }


    //Function to check queue is not empty
    def isNotEmpty: Boolean = {
      if (isEmpty) {
        false
      } else {
        true
      }
    }

    //Function to delete data from front
    def dequeue(): Unit = {
      if (isEmpty) {
        print("Queue is empty")
      } else {
        if (this.len == 1) {
          this.front = null
          this.rear = null
          this.max = 0
          this.min = 0
        } else {
          this.front = front.next
          this.front.prev = null
          this.min = this.front.data
        }
        len -= 1
      }
    }

    def first(): Int = {
      this.front.data
    }

    //class Node to store data
    class Node {
      var data = 0
      var next: Node = _
      var prev: Node = _

      def this(num: Int) {
        this()
        this.data = num
      }
    }

  }


}