package patmat

object Week5 {
  val n=7                                         //> n  : Int = 7
  (1 until n) map (i=>
  	(1 until i) map(j=>(i,j)))                //> res0: scala.collection.immutable.IndexedSeq[scala.collection.immutable.Indexe
                                                  //| dSeq[(Int, Int)]] = Vector(Vector(), Vector((2,1)), Vector((3,1), (3,2)), Vec
                                                  //| tor((4,1), (4,2), (4,3)), Vector((5,1), (5,2), (5,3), (5,4)), Vector((6,1), (
                                                  //| 6,2), (6,3), (6,4), (6,5)))
                                                  
   /*def scalarProduct(xs1:List[Double],xs2:List[Double]):Double={
   
    for {
    		i<-1 until xs1.size
    		j<-1 until xs2.size
    		if(i==j)
    } yield (xs1.i*xs2.j)
   }*/
   
   class Polynomials(val terms :Map[Int,Double]){
   		/*def +(other:Polynomials):Polynomials={
   				val interimMap=this.terms.keys.map { x => (if (other.terms.contains(x)) {(x,(this.terms(x)+ other.terms(x)))} else {(x,this.terms(x))})}
   				val finalMap=other.terms.filterKeys { x =>(!this.terms.contains(x)) }
   				println(finalMap)
   				
   				return new Polynomials((interimMap++finalMap).toMap)
   		}*/
   		
   		//def +(other:Polynomials)=new Polynomials(other.terms.foldLeft(z)(op)
   		
   		override def toString() = {
   				(for((x,y) <- terms.toList.sorted.reverse) yield y+"X>"+x) mkString "+"
   				
   		}
   		
   }
   
   		val p1=new Polynomials(Map(1->2.0,3->4.0,5->6.2))
                                                  //> p1  : patmat.Week5.Polynomials = 6.2X>5+4.0X>3+2.0X>1
   		val p2=new Polynomials(Map(0->3.0,3->7))
                                                  //> p2  : patmat.Week5.Polynomials = 7.0X>3+3.0X>0
   		//p1+p2
 
}