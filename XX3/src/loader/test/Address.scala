package loader.test

import scala.collection.mutable.HashMap

protected object print {
  def apply[X](a:Array[X]):String = if (a!=null) a.mkString("[",",","]") else ""
}

object AddressStatus extends Enumeration {
  class AddressStatus extends Val
  val main,secondary,other=new AddressStatus
}
object PhoneKind extends Enumeration {
  class PhoneKind extends Val
  val fix,internet,satellite,mobile=new PhoneKind
}

class Person {
  var name:String = _
  var surname:String = _
  var birthday:java.util.Date = _
  var address:Array[Address] = _
  override def toString = s"$name $surname, born on $birthday, living at ${print(address)}"
}

class Address {
  import AddressStatus._
  var status:AddressStatus = _
  var street:String = _
  var number:Int = _
  var complement:String = _
  var city:String = _
  var phone:Array[Phone] = _
  override def toString = s"$number $complement, $city ; $status ; phone:${print(phone)}"
}

class City {
  var name:String = _
  var country:String = _
  var inhabitants:Int = _
  var airport:Array[String] = _
  override def toString = s"name, $country, $inhabitants inhabitants, airports:${print(airport)}"
}

class Phone {
  import PhoneKind._
  var number:String = _
  var kind:PhoneKind = _
  override def toString = s"tel ($kind): $number"
}

class DataAddress {
  var person:List[Person] = _
  var city:HashMap[String,City] = _
  override def toString = s"${person.mkString("", "\n\n", "")}${city.mkString("", "\n\n", "")}"
}