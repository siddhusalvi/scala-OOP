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
object DeckOfCards{
  def main(args: Array[String]): Unit = {
    var flag = true
    while(flag){
      try{
        var cat:Cards = new Cards()
        //cat.shuffle()
        //cat.display()
        flag = false
        var dt = Array(1,2,3,4,5,6,7,8,9,10,11,12,13,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52)
        cat.view(dt)
      }

      catch{
        case _=>print("Something went wrong Error occurred.")
      }
    }
  }
  class Cards{
    var cat:Array[Int] = new Array[Int](52)
    for(i <- 0 until 52){
      cat(i) = i
    }

    def shuffle(): Unit ={
      for(i <- 0 until cat.length){
        var rnd:Int = Random.nextInt(cat.length-1)
        var temp:Int =  cat(rnd)
        cat(rnd) = cat(i)
        cat(i) = temp
      }
    }

    def display(): Unit ={
      println(cat.mkString(" "))
    }

    def view(dt:Array[Int]):Unit={

      for(i <- 0 until cat.length){
        var normal_card = cat(i) //remove cat add dt and rename dt

        if(normal_card <= 12){
          var card_type = "Club"
          var card_str = card_type + " "
          var type_card = normal_card%13
          if( type_card == 9){
            card_str += "Jack"
          }else if(type_card == 10){
            card_str += "Queen"
          }else if(type_card == 11){
            card_str += "King"
          }else if(type_card == 12) {
            card_str += "Ace"
          }else{
              card_str += (type_card+2).toString
          }
          println(card_str)

        }else if (normal_card <= 25){

          var card_type = "Diamond "
          var card_str = card_type + " "
          var type_card = normal_card%13
          if( type_card == 9){
            card_str += "Jack"
          }else if(type_card == 10){
            card_str += "Queen"
          }else if(type_card == 11){
            card_str += "King"
          }else if(type_card == 12) {
            card_str += "Ace"
          }else{
            card_str += (type_card+2).toString
          }
          println(card_str)

        }else if (normal_card <= 38){
          var card_type = "Heart "
          var card_str = card_type + " "
          var type_card = normal_card%13
          if( type_card == 9){
            card_str += "Jack"
          }else if(type_card == 10){
            card_str += "Queen"
          }else if(type_card == 11){
            card_str += "King"
          }else if(type_card == 12) {
            card_str += "Ace"
          }else{
            card_str += (type_card+2).toString
          }
          println(card_str)

        }else if(normal_card <=51){
          var card_type = "Spade "
          var card_str = card_type + " "
          var type_card = normal_card%13
          if( type_card == 9){
            card_str += "Jack"
          }else if(type_card == 10){
            card_str += "Queen"
          }else if(type_card == 11){
            card_str += "King"
          }else if(type_card == 12) {
            card_str += "Ace"
          }else{
            card_str += (type_card+2).toString
          }
          println(card_str)


        }else{
          println("card is out of index ")
        }
      }
    }



  }

} 