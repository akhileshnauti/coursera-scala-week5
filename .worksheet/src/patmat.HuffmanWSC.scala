package patmat

object HuffmanWSC {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(79); 
  println("Welcome to the Scala worksheet");$skip(73); val res$0 = 
  Huffman.times(List('a', 'b', 'a','b','b','c','d','a','b','c','d','e'));System.out.println("""res0: List[(Char, Int)] = """ + $show(res$0));$skip(24); val res$1 = 
  Huffman.times(List());System.out.println("""res1: List[(Char, Int)] = """ + $show(res$1));$skip(62); val res$2 = 
  Huffman.makeOrderedLeafList(List(('a',6),('c',3), ('b',2)));System.out.println("""res2: List[(Char, Int)] = """ + $show(res$2));$skip(81); val res$3 = 
  Huffman.makeOrderedLeafList(List(('a',3), ('b',6), ('c',1), ('d',5), ('e',2)));System.out.println("""res3: List[(Char, Int)] = """ + $show(res$3))}
}
