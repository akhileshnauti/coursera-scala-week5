package patmat

object PhoneNumberTask {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(84); 
  println("Welcome to the Scala worksheet");$skip(117); 
  
  val mnemonics=Map('2'->"ABC",'3'->"DEF",'4'->"GHI",
  '5'->"JKL",'6'->"MNO",'7'->"PQRS",'8'->"TUV",'9'->"WXYZ");System.out.println("""mnemonics  : scala.collection.immutable.Map[Char,String] = """ + $show(mnemonics ));$skip(310); 
   def translate(phoneNumber:String):List[String]={
   			val lst:List[List[Char]]=phoneNumber.map { x => (mnemonics(x))}.toList.map { x => x.toCharArray().toList }
   			println(lst)
   			val tst=List(lst.foreach { x => x.foreach { y => { y::x;println(y::x)} } })
   			println(tst)
   			List()
   			
  	};System.out.println("""translate: (phoneNumber: String)List[String]""");$skip(30); val res$0 = 
  	
  	
  	translate("72252");System.out.println("""res0: List[String] = """ + $show(res$0))}
}
