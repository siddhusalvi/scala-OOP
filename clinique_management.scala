import com.google.gson.Gson

import scala.io.Source

/*
Filename: clinique_management {
Created: Siddhesh Salvi
11. Clinique Management Programme. This programme is used to manage a list of
Doctors associated with the Clinique. This also manages the list of patients who use the
clinique. It manages Doctors by Name, Id, Specialization and Availability (AM, PM or
both). It manages Patients by Name, ID, Mobile Number and Age. The Program allows
users to search Doctor by name, id, Specialization or Availability. Also the programs
allows users to search patient by name, mobile number or id. The programs allows
patients to take appointment with the doctor. A doctor at any availability time can see
only 5 patients. If exceeded the user can take appointment for patient at different date or
availability time. Print the Doctor Patient Report. Also show which Specialization is
popular in the Clinique as well as which Doctor is popular. For .NET Engineers use the
following
a. ADO.NET Connection Pooling to maintain Doctor, Patient and Appointment Info
in the Database
b. Use Log4NET to Log Data
c. Read Patient and Doctor Data from JSON File using File IO and latter with
Firebase. Use Factory Pattern and Interface Approach to read Doctor and Patient
information.
*/
class Doctor {
  var name: String = ""
  var id: Int = 0
  var topic: String = ""
  var active = ""
  var status: Boolean = false

  def this(name: String, id: Int, speciality: String, time_slot: String, status: Boolean) {
    this()
    this.name = name
    this.id = id
    this.topic = speciality
    this.active = time_slot
    this.status = status
  }

  //Function to display data
  def display(): Unit = {
    var temp_str = "name : " + this.name + " id : " + this.id + " speciality : " + this.topic + " availability : " + this.active + " status " + this.status
    println(temp_str)
  }
}

//companion class for id
object Doctor {
  var id_auto = 1

  def get_id(): Int = {
    id_auto += 1
    return id_auto - 1
  }

}

class Patient {
  var name: String = ""
  var id: Int = 0
  var contact: Long = 0L
  var age: Int = 0

  def this(name: String, id: Int, contact: Long, age: Int) {
    this()
    this.name = name
    this.id = id
    this.contact = contact
    this.age = age
  }
  //Function to display data
  def display(): Unit = {
    var temp_str = "name : " + this.name + " id : " + this.id + " contact : " + this.contact + " age : " + this.age
    println(temp_str)
  }
}
//companion class for id
object Patient {
  var id_auto = 1

  def get_id(): Int = {
    id_auto += 1
    return id_auto - 1
  }

}




object clinique_management {
  def main(args: Array[String]): Unit = {
    var flag = true
    var patient = getEmptyPatient()
    var doctor = getEmptyDoctor()
    while (flag) {
        patient = loadPatient()
        doctor = loadDoctor()

        doctor(4).display()
       searchDoctor(doctor)
        flag = false
      }
//      catch {
//        case _ => print("Something went wrong Error occurred.")
//      }
    }

  //Function to search doctor
  def searchDoctor(data:Array[Doctor]):Unit={
  println("Enter preference for searching : 1:name 2:id 3:specialization 4:Availability ")
    var choice = scala.io.StdIn.readInt()
    if(choice ==1){
      println("Enter name of doctor to search : ")
      var id = scala.io.StdIn.readInt()
      var ack = getIndexByIdDoc(data,id)
      if(ack > 0) {
      data(ack).display()
      }else{
        println("Doctor not found ")
      }


    }else if(choice == 2){

      println("Enter id of doctor to search : ")
      var id = scala.io.StdIn.readInt()
      var ack = getIndexByIdDoc(data,id)
      println(ack)
      if(ack > 0) {
        data(ack).display()
      }else{
        println("Doctor not found ")
      }

    }else if(choice == 3){



      println("Enter specialization of doctor to search : ")
      var topic = scala.io.StdIn.readLine()
      var ack = getIndexByTopicDoc(data,topic)
      if(ack > 0) {
        data(ack).display()
      }else{
        println("Doctor not found ")
      }





    }else if(choice == 4){

      println("Enter time slot of doctor to search : ")
      var time = scala.io.StdIn.readLine()
      var ack = getIndexByTimeDoc(data,time)
      if(ack > 0) {
        data(ack).display()
      }else{
        println("Doctor not found ")
      }


    }else{
      println("Invalid choice ")
    }

  }
  //Function to get incex of doctor by time
  def getIndexByTimeDoc(doctors: Array[Doctor], str: String): Int ={
    if(searchDocByTime(doctors,str)){
      -1
    }else{
      -1
    }
  }
  //Function to check time slot of doc
  def searchDocByTime(doctors: Array[Doctor], str: String): Boolean ={
    for(i <- )
  }

  //Function to get index of doctor by topic
  def getIndexByTopicDoc(doctors: Array[Doctor], value: String): Int ={
    if(searchDocByTopic(doctors,value)){
      for(i <- 0 until doctors.length){
        if(doctors(i).topic.equals(value)){
          return i
        }
      }
      -1
    }else{
      -1
    }
  }
  //Function to check doctor with specialization
  def searchDocByTopic(doctors: Array[Doctor], str: String):Boolean = {
    for(i <- doctors){
      if(i.topic.equals(str)){
        return true
      }
    }
    false
  }

  //Function to get index of doc by id
  def getIndexByIdDoc(data:Array[Doctor], id:Int):Int={
    if(searchDocById(data,id)){
      for(index <- 0 until data.length){
        if(data(index).id == id){
          return index
        }
      }
      -1
    }else{
      -1
    }
  }
  //Function to search id
  def searchDocById(data:Array[Doctor],id:Int):Boolean = {
    for(i <-data){
      if(i.id==id){
        return true
      }
    }
    false
  }
  //Function to search name
  def searchDocByName(data:Array[Doctor],name:String):Boolean = {
    for(i <-data){
      if(i.name.equals(name)){
        return true
      }
    }
    false
  }


  //Function to get index by doc name
  def getIndexbyNameDoc(data:Array[Doctor], name:String):Int={
    if(searchDocByName(data,name)){
      for(index <- 0 until data.length){
        if(data(index).name.equals(name)){
          return index
        }
      }
      -1
    }else{
      -1
    }
  }
  //Function to load doctors
  def loadDoctor(): Array[Doctor] = {
    var file = "/home/admin1/IdeaProjects/OOPs/src/main/scala/JsonData/doctors.json"
    var data_str = getFileData(file)
    var json_mgr = new Gson()
    var doctor_obj_array = json_mgr.fromJson(data_str, classOf[Array[Doctor]])
    doctor_obj_array
  }

  def loadPatient():Array[Patient]={
    var file = "/home/admin1/IdeaProjects/OOPs/src/main/scala/JsonData/patient.json"
    var data_str = getFileData(file)
    var json_mgr = new Gson()
    var patient_obj_array = json_mgr.fromJson(data_str, classOf[Array[Patient]])
    patient_obj_array
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

  //function to get empty doc
  def getEmptyDoctor(): Array[Doctor] = {
    new Array[Doctor](0)
  }
  //Function to get empty patient
  def getEmptyPatient():Array[Patient]={
    new Array[Patient](0)
  }

} 