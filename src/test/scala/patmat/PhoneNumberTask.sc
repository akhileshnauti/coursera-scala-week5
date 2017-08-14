package patmat

object PhoneNumberTask {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val mnemonics=Map('2'->"ABC",'3'->"DEF",'4'->"GHI",
  '5'->"JKL",'6'->"MNO",'7'->"PQRS",'8'->"TUV",'9'->"WXYZ")
                                                  //> mnemonics  : scala.collection.immutable.Map[Char,String] = Map(8 -> TUV, 4 -
                                                  //| > GHI, 9 -> WXYZ, 5 -> JKL, 6 -> MNO, 2 -> ABC, 7 -> PQRS, 3 -> DEF)
   def translate(phoneNumber:String):List[String]={
   			val lst:List[List[Char]]=phoneNumber.map { x => (mnemonics(x))}.toList.map { x => x.toCharArray().toList }
   			println(lst)
   			val tst=List(lst.foreach { x => x.foreach { y => { y::x;println(y::x)} } })
   			println(tst)
   			List()
   			
  	}                                         //> translate: (phoneNumber: String)List[String]
  	
  	
  	translate("72252")                        //> List(List(P, Q, R, S), List(A, B, C), List(A, B, C), List(J, K, L), List(A, 
                                                  //| B, C))
                                                  //| List(P, P, Q, R, S)
                                                  //| List(Q, P, Q, R, S)
                                                  //| List(R, P, Q, R, S)
                                                  //| List(S, P, Q, R, S)
                                                  //| List(A, A, B, C)
                                                  //| List(B, A, B, C)
                                                  //| List(C, A, B, C)
                                                  //| List(A, A, B, C)
                                                  //| List(B, A, B, C)
                                                  //| List(C, A, B, C)
                                                  //| List(J, J, K, L)
                                                  //| List(K, J, K, L)
                                                  //| List(L, J, K, L)
                                                  //| List(A, A, B, C)
                                                  //| List(B, A, B, C)
                                                  //| List(C, A, B, C)
                                                  //| List(())
                                                  //| res0: List[String] = List()
}