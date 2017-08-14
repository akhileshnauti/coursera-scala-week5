package patmat

object HuffmanWSC {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  Huffman.times(List('a', 'b', 'a','b','b','c','d','a','b','c','d','e'))
                                                  //> res0: List[(Char, Int)] = List((a,3), (b,4), (c,2), (d,2), (e,1))
  Huffman.times(List())                           //> res1: List[(Char, Int)] = List()
  Huffman.makeOrderedLeafList(List(('a',6),('c',3), ('b',2)))
                                                  //> res2: List[(Char, Int)] = List((b,2), (c,3))
  Huffman.makeOrderedLeafList(List(('a',3), ('b',6), ('c',1), ('d',5), ('e',2)))
                                                  //> res3: List[(Char, Int)] = List((c,1), (e,2), (a,3), (d,5))
}