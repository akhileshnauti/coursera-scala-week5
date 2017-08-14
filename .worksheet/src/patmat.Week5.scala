package patmat

object Week5 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(40); 
  val n=7;System.out.println("""n  : Int = """ + $show(n ));$skip(53); val res$0 = 
  (1 until n) map (i=>
  	(1 until i) map(j=>(i,j)))
                                                  
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
   		
   };System.out.println("""res0: scala.collection.immutable.IndexedSeq[scala.collection.immutable.IndexedSeq[(Int, Int)]] = """ + $show(res$0));$skip(940); 
   
   		val p1=new Polynomials(Map(1->2.0,3->4.0,5->6.2));System.out.println("""p1  : patmat.Week5.Polynomials = """ + $show(p1 ));$skip(46); 
   		val p2=new Polynomials(Map(0->3.0,3->7));System.out.println("""p2  : patmat.Week5.Polynomials = """ + $show(p2 ))}
   		//p1+p2
 
}
