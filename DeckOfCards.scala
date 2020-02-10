/*
Filename: DeckOfCards
Created: Siddhesh Salvi
Change history:10.2.2020 / Siddhesh Salvi

9. Write a Program DeckOfCards.java, to initialize deck of cards having suit ("Clubs",
"Diamonds", "Hearts", "Spades") & Rank ("2", "3", "4", "5", "6", "7", "8", "9", "10",
"Jack", "Queen", "King", "Ace"). Shuffle the cards using Random method and then
distribute 9 Cards to 4 Players and Print the Cards the received by the 4 Players
using 2D Array…
*/

import scala.util.Random
object DeckOfCards {
  def main(args: Array[String]): Unit = {
    var flag = true
    while (flag) {
      try {
        var cat: Cards = new Cards()
        print("In how many players do you want to distribute the cards : ")
        var players = scala.io.StdIn.readInt()
        cat.shuffle()
        cat.distribute(players)
        flag = false

      }
      catch {
        case _ => print("Something went wrong Error occurred.")
      }
    }
  }

  class Cards {
    var total_cards = 52
    var current_card = 0
    var pack: Array[Int] = new Array[Int](total_cards)

    for (i <- 0 until pack.length){
      pack(i) = i
    }

    def reset(): Unit ={
      for (i <- 0 until pack.length){
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
        var card_set = total_cards / players
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
        var card_type = "♣"
        var card_str = card_type + " "
        var type_card = given_card % 13
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

        var card_type = "♦ "
        var card_str = card_type + " "
        var type_card = given_card % 13
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
        var card_type = "♥ "
        var card_str = card_type + " "
        var type_card = given_card % 13
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

      } else if (given_card <= 51) {
        var card_type = "♠ "
        var card_str = card_type + " "
        var type_card = given_card % 13
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

} 