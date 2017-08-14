package patmat

import common._
import scala.collection.immutable.Nil

/**
 *
 * Assignment 4: Huffman coding
 *
 *
 *
 */

object Huffman extends App {

  /**
   *
   * A huffman code is represented by a binary tree.
   *
   *
   *
   * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
   *
   * The weight of a `Leaf` is the frequency of appearance of the character.
   *
   *
   *
   * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
   *
   * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
   *
   * leaves.
   *
   */

  abstract class CodeTree

  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree

  case class Leaf(char: Char, weight: Int) extends CodeTree

  // Part 1: Basics

  def weight(tree: CodeTree): Int = tree match {

    case Leaf(char, weight) => weight

    case Fork(left, right, chars, wt) => (weight(left) + weight(right))

  }

  def chars(tree: CodeTree): List[Char] = tree match {

    case Leaf(char, weight) => List(char)

    case Fork(left, right, chrs, wt) => List.concat(chars(left), chars(right))

  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =

    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  // Part 2: Generating Huffman trees

  /**
   *
   * In this assignment, we are working with lists of characters. This function allows
   *
   * you to easily create a character list from a given string.
   *
   */

  def string2Chars(str: String): List[Char] = str.toList

  /**
   *
   * This function computes for each unique character in the list `chars` the number of
   *
   * times it occurs. For example, the invocation
   *
   *
   *
   *   times(List('a', 'b', 'a'))
   *
   *
   *
   * should return the following (the order of the resulting list is not important):
   *
   *
   *
   *   List(('a', 2), ('b', 1))
   *
   *
   *
   * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
   *
   * character and an integer. Pairs can be constructed easily using parentheses:
   *
   *
   *
   *   val pair: (Char, Int) = ('c', 1)
   *
   *
   *
   * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
   *
   *
   *
   *   val theChar = pair._1
   *
   *   val theInt  = pair._2
   *
   *
   *
   * Another way to deconstruct a pair is using pattern matching:
   *
   *
   *
   *   pair match {
   *
   *     case (theChar, theInt) =>
   *
   *       println("character is: "+ theChar)
   *
   *       println("integer is  : "+ theInt)
   *
   *   }
   *
   */

  def times(chars: List[Char]): List[(Char, Int)] = {

    def countTimes(chars: List[Char], acc: List[(Char, Int)]): List[(Char, Int)] = {

      if (chars.isEmpty) {

        return acc

      }

      val head = chars.head

      val tail = chars.tail

      if (!tail.contains(head)) {

        val pair: (Char, Int) = (head, 1)

        countTimes(tail, List.concat(acc, List(pair)))

      } else {

        val pair: (Char, Int) = (head, 1 + tail.count { x => (head == x) })

        val tailRem = tail.filter { x => (head != x) }

        countTimes(tailRem, List.concat(acc, List(pair)))

      }

    }

    countTimes(chars, List())

  }

  /**
   *
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   *
   *
   * The returned list should be ordered by ascending weights (i.e. the
   *
   * head of the list should have the smallest weight), where the weight
   *
   * of a leaf is the frequency of the character.
   *
   */

  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {

    def shiftElement(pair: (Char, Int), freqs: List[(Char, Int)], acc: List[(Char, Int)]): List[(Char, Int)] = {

      if (freqs.isEmpty) {

        return acc

      }

      val leafName = freqs.head._1

      val freq = freqs.head._2

      if (freq > pair._2) {

        val newList: List[(Char, Int)] = List.concat(List(freqs.head), freqs.tail)

        if (!acc.isEmpty && (acc.head._2 < pair._2)) {

          shiftElement(newList.head, newList.tail, acc)

        } else {

          shiftElement(newList.head, newList.tail, List(pair))

        }

      } else {

        val newList: List[(Char, Int)] = List.concat(List(pair), freqs.tail)

        if (!acc.isEmpty && (acc.head._2 < freqs.head._2)) {

          shiftElement(newList.head, newList.tail, acc)

        } else {

          shiftElement(newList.head, newList.tail, List(freqs.head))

        }

      }

    }

    def makeOrderedLeafListWitAcc(freqs: List[(Char, Int)], acc: List[(Char, Int)]): List[(Char, Int)] = {

      val newList: List[(Char, Int)] = shiftElement(freqs.head, freqs.tail, List())

      val acc_f: List[(Char, Int)] = List.concat(acc, newList)

      if (!newList.isEmpty) {

        makeOrderedLeafListWitAcc(freqs.filter(x => x._1 != newList.head._1), acc_f)

      } else {

        List.concat(acc_f, List(freqs.head))

      }

    }
    if(freqs.isEmpty) return List() 
    makeOrderedLeafListWitAcc(freqs, List()).map(x => Leaf(x._1, x._2))

  }

  /**
   *
   * Checks whether the list `trees` contains only one single code tree.
   *
   */

  def singleton(trees: List[CodeTree]): Boolean = {

    trees.size == 1

  }

  /**
   *
   * The parameter `trees` of this function is a list of code trees ordered
   *
   * by ascending weights.
   *
   *
   *
   * This function takes the first two elements of the list `trees` and combines
   *
   * them into a single `Fork` node. This node is then added back into the
   *
   * remaining elements of `trees` at a position such that the ordering by weights
   *
   * is preserved.
   *
   *
   *
   * If `trees` is a list of less than two elements, that list should be returned
   *
   * unchanged.
   *
   */

  def combine(trees: List[CodeTree]): List[CodeTree] = {

    if (trees.size < 2) {

      return trees

    }

    val tree1: CodeTree = trees.head

    val tree2: CodeTree = trees.tail.head

    val result: Fork = Fork(tree1, tree2, List.concat(chars(tree1), chars(tree2)), (weight(tree1) + weight(tree2)))

    val remList: List[CodeTree] = trees.filter(x => (x != tree1 && x != tree2))

    def adjustFork(fork: Fork, listOfCodeTree: List[CodeTree], acc: List[CodeTree]): List[CodeTree] = {

      if (listOfCodeTree.isEmpty) {
        if (acc.size == 1) {
          List(makeCodeTree(fork, acc.head))
        } else {
          acc
        }

      } else {

        val head = listOfCodeTree.head

        if (fork.weight > weight(head)) {

          adjustFork(fork, listOfCodeTree.tail, List.concat(acc, List(head)))

        } else {

          adjustFork(fork, List(), List.concat(List.concat(acc, List(fork)), listOfCodeTree))

        }

      }

    }

    adjustFork(result, remList, List())

  }

  /**
   *
   * This function will be called in the following way:
   *
   *
   *
   *   until(singleton, combine)(trees)
   *
   *
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   *
   * the two functions defined above.
   *
   *
   *
   * In such an invocation, `until` should call the two functions until the list of
   *
   * code trees contains only one single tree, and then return that singleton list.
   *
   *
   *
   * Hint: before writing the implementation,
   *
   *  - start by defining the parameter types such that the above example invocation
   *
   *    is valid. The parameter types of `until` should match the argument types of
   *
   *    the example invocation. Also define the return type of the `until` function.
   *
   *  - try to find sensible parameter names for `xxx`, `yyy` and `zzz`.
   *
   */

  def until(f_1: List[CodeTree] => Boolean, f_2: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = {

    if (f_1(trees)) {

      trees

    } else {

      until(f_1, f_2)(f_2(trees))

    }

  }

  /**
   *
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   *
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   *
   * frequencies from that text and creates a code tree based on them.
   *
   */

  def createCodeTree(chars: List[Char]): CodeTree = {

    until(singleton, combine)(makeOrderedLeafList(times(chars))).head

  }

  // Part 3: Decoding

  type Bit = Int

  /**
   *
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   *
   * the resulting list of characters.
   *
   */

  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {

    def addCharacter(origTree: CodeTree, currentTree: CodeTree, bits: List[Bit], acc: List[Char]): List[Char] = currentTree match {
      case Leaf(char, weight) => {
        val acc_new = List.concat(acc, List(currentTree.asInstanceOf[Leaf].char))
        if (bits.isEmpty) {
          return acc_new
        } else {
          List.concat(addCharacter(origTree, origTree, bits, acc_new))
        }

      }

      case Fork(left, right, chars, wt) => {
        if (bits.isEmpty) {
          return acc
        }
        val bit: Int = bits.head
        if (bit == 0) {
          addCharacter(origTree, currentTree.asInstanceOf[Fork].left, bits.tail, acc)
        } else {
          addCharacter(origTree, currentTree.asInstanceOf[Fork].right, bits.tail, acc)
        }
      }

    }
    if(tree==null) return List()
    addCharacter(tree, tree, bits, List())

  }

  /**
   *
   * A Huffman coding tree for the French language.
   *
   * Generated from the data given at
   *
   *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   *
   */

  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s', 121895), Fork(Leaf('d', 56269), Fork(Fork(Fork(Leaf('x', 5928), Leaf('j', 8351), List('x', 'j'), 14279), Leaf('f', 16351), List('x', 'j', 'f'), 30630), Fork(Fork(Fork(Fork(Leaf('z', 2093), Fork(Leaf('k', 745), Leaf('w', 1747), List('k', 'w'), 2492), List('z', 'k', 'w'), 4585), Leaf('y', 4725), List('z', 'k', 'w', 'y'), 9310), Leaf('h', 11298), List('z', 'k', 'w', 'y', 'h'), 20608), Leaf('q', 20889), List('z', 'k', 'w', 'y', 'h', 'q'), 41497), List('x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 72127), List('d', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 128396), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 250291), Fork(Fork(Leaf('o', 82762), Leaf('l', 83668), List('o', 'l'), 166430), Fork(Fork(Leaf('m', 45521), Leaf('p', 46335), List('m', 'p'), 91856), Leaf('u', 96785), List('m', 'p', 'u'), 188641), List('o', 'l', 'm', 'p', 'u'), 355071), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u'), 605362), Fork(Fork(Fork(Leaf('r', 100500), Fork(Leaf('c', 50003), Fork(Leaf('v', 24975), Fork(Leaf('g', 13288), Leaf('b', 13822), List('g', 'b'), 27110), List('v', 'g', 'b'), 52085), List('c', 'v', 'g', 'b'), 102088), List('r', 'c', 'v', 'g', 'b'), 202588), Fork(Leaf('n', 108812), Leaf('t', 111103), List('n', 't'), 219915), List('r', 'c', 'v', 'g', 'b', 'n', 't'), 422503), Fork(Leaf('e', 225947), Fork(Leaf('i', 115465), Leaf('a', 117110), List('i', 'a'), 232575), List('e', 'i', 'a'), 458522), List('r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 881025), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u', 'r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 1486387)

  /**
   *
   * What does the secret message say? Can you decode it?
   *
   * For the decoding use the `frenchCode' Huffman tree defined above.
   *
   */

  val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)

  /**
   *
   * Write a function that returns the decoded secret
   *
   */

  def decodedSecret: List[Char] = decode(frenchCode, secret)

  // Part 4a: Encoding using Huffman tree

  /**
   *
   * This function encodes `text` using the code tree `tree`
   *
   * into a sequence of bits.
   *
   */

  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def traverseList(tree: CodeTree, chars: List[Char], acc: List[Bit]): List[Bit] = {
      if (chars.isEmpty) {
        return acc
      } else {
        traverseList(tree, chars.tail, encodeCharacter(tree, chars.head, acc))
      }

    }
    def encodeCharacter(tree: CodeTree, char: Char, acc: List[Bit]): List[Bit] = tree match {
      case Leaf(char, weight) => {
        acc
      }
      case Fork(left, right, chars, wt) => {
        if (getChars(left).contains(char)) {
          encodeCharacter(left, char, List.concat(acc, List(0)))
        } else if (getChars(right).contains(char)) {
          encodeCharacter(right, char, List.concat(acc, List(1)))
        } else {
          acc
        }

      }
    }
    if(tree==null) return List()
    traverseList(tree, text, List())
  }
  def getChars(tree: CodeTree): List[Char] = tree match {
    case Leaf(char, weight) => {
      List(tree.asInstanceOf[Leaf].char)
    }
    case Fork(left, right, chars, wt) => {
      makeCodeTree(left, right).chars
    }
  }

  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
   *
   * This function returns the bit sequence that represents the character `char` in
   *
   * the code table `table`.
   *
   */

  def codeBits(table: CodeTable)(char: Char): List[Bit] =table match {
    case Nil=> List()
    case n :: rest => if (n._1.equals(char)) n._2 else  codeBits(rest)(char)
  }

  /**
   *
   * Given a code tree, create a code table which contains, for every character in the
   *
   * code tree, the sequence of bits representing that character.
   *
   *
   *
   * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
   *
   * a valid code tree that can be represented as a code table. Using the code tables of the
   *
   * sub-trees, think of how to build the code table for the entire tree.
   *
   */
  def flatten(table: CodeTable, acc: List[Bit]): List[Bit] = {
    if (table.tail.isEmpty) {
      List.concat(acc, table.head._2)
    } else {
      flatten(table.tail, List.concat(acc, table.head._2))
    }
  }
  
  
  
  
  

  def convert(tree: CodeTree): CodeTable = {

    def convertWithAcc(tree: CodeTree, acc: CodeTable): CodeTable = tree match {

      case Leaf(char, weight) => {
        List(List.concat(acc, List[(Char, List[Bit])]() :+ (char, flatten(acc, List()))).last)

      }

      case Fork(left, right, chars, wt) => {

        val leftCount: List[Bit] = List[(Bit)](0)

        val rightCount: List[Bit] = List[(Bit)](1)

        mergeCodeTables(convertWithAcc(tree.asInstanceOf[Fork].left, List.concat(acc, List[(Char, List[Bit])]() :+ (chars.head, leftCount))),
                        convertWithAcc(tree.asInstanceOf[Fork].right, List.concat(acc, List[(Char, List[Bit])]() :+ (chars.head, rightCount))))
        
      }

    }

    convertWithAcc(tree, List())

  }

  /**
   *
   * This function takes two code tables and merges them into one. Depending on how you
   *
   * use it in the `convert` method above, this merge method might also do some transformations
   *
   * on the two parameter code tables.
   *
   */

  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = {
    List.concat(a, b)
  }

  /**
   *
   * This function encodes `text` according to the code tree `tree`.
   *
   *
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   *
   * and then uses it to perform the actual encoding.
   *
   */

  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def quickEncodeViaCodeTable(table: CodeTable,acc:List[Bit])(text: List[Char]): List[Bit] = {
      if(text.isEmpty){
        acc
      }else{
        quickEncodeViaCodeTable(table,List.concat(acc,codeBits(table)(text.head)))(text.tail)
      }
    }
    quickEncodeViaCodeTable(convert(tree),List())(text)
  }
  def flatten_4(xs:List[Any]): List[Any] =xs match{
    case Nil => xs
	    case (y:List[Any]) :: ys => flatten_4(y) ::: flatten_4(ys)
	    case y :: ys => y :: flatten_4(ys)
    
    
  }
  
  def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 =>xs.takeWhile { y => (y==x)}::(pack(xs.dropWhile { y => (y==x) }))
}
  
  def concat(xs:List[Any],ys:List[Any]): List[Any] =xs match{
    case List()=>ys
    case z::zs=>z::concat(zs,ys)
  }
  
  
  def encode_new(xs:List[Char]):List[(Char,Int)]={
     val lst:List[List[Char]]=pack(xs)
     lst.map { x => (x.head,x.size) }
  }
  
  
  
  
  println(concat(List(1,2),List(3,4)))
  //println(List(1,2)::List())
  println(flatten_4(List(List(1, 1), 2, List(3, List(5, 8)))))
  println(encode_new(List('a', 'a', 'a', 'b', 'c', 'c', 'a')))

  }
