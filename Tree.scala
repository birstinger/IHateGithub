package Exmaple

import Exmaple.MicroV3.e

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.{JavaTokenParsers, PackratParsers}

abstract class Tree
case class E(t: Tree, tore: Option[Tree]) extends Tree
case class T(fANDt : Tree, f: Option[Tree]) extends Tree
case class F(nota : Option[Exclaim],A:Tree) extends Tree // ,nota:Option[exclaim]
case class A(einParenthesis : Option[Tree],c : Option[Tree]) extends Tree
case class C(tru :Option[Boolean], falsee: Option[Boolean], Ch :Option[Tree] ) extends Tree
case class Ch(anychar:Character) extends Tree

case class OR(t:Tree,e:Tree)extends Tree
case class and(t:Tree,e:Tree)extends Tree
case class not(t:Tree,e:Tree)extends Tree
case class leftP(t:Tree)extends Tree
case class RightP(t:Tree)extends Tree
case class falso(t:String)extends Tree
case class cierto(t:String)extends Tree
case class Exclaim(s:String) extends Tree


//case object NIL extends Tree

object MicroV3 extends CombinatorsV1{
  def main(args: Array[String]){
    do
    {
      System.out.println("Please enter a logical expression")
      val a = scala.io.StdIn.readLine()
      val exp2c: Tree = parseAll(e, a).get
      //println("The value of a is "+ a)
      println("Whole tamale: "+ exp2c)
      // assert(logicalSimplification(exp2c)=="a")
      println(logicalSimplification(exp2c))


    }while(5>4)
  }


  def logicalSimplification(exp: Tree):String= exp match {
    //case class E(tORe: Tree, t: Option[Tree]) extends Tree
    // def e: Parser[Tree] = t ~ orc ~ e ^^ { case l ~ o ~ r => E(l, Some(r)) } | t
    //case E(t,Some(e))if((logicalSimplification(t).contains("true")&& !logicalSimplification(t).contains("!true")) || logicalSimplification(e).contains("true")&& !logicalSimplification(e).contains("!true")) =>"true"

    case E(t,Some(e))if(logicalSimplification(t).equals("false") && ((!logicalSimplification(e).equals("false") && !logicalSimplification(e).equals("!true")) && (!logicalSimplification(e).equals("true") && !logicalSimplification(e).equals("!false")) )) =>logicalSimplification(e).toString//.toString
    case E(t,Some(e))if(logicalSimplification(e).equals("false") && ((!logicalSimplification(t).equals("false") && !logicalSimplification(t).equals("!true")) && (!logicalSimplification(t).equals("true") && !logicalSimplification(t).equals("!false")) )) =>logicalSimplification(t).toString//.toString

    case E(t,Some(e))if((!logicalSimplification(t).equals("false") && !logicalSimplification(t).equals("true")&& (logicalSimplification(e).equals("false")))) =>  logicalSimplification(t).toString
    case E(t,Some(e))if((!logicalSimplification(e).equals("false") && !logicalSimplification(e).equals("true")&& (logicalSimplification(t).equals("false")))) =>  logicalSimplification(e).toString

    case E(t,Some(e))if((logicalSimplification(t).contains("true"))||(logicalSimplification(e).contains("true"))||(logicalSimplification(t).contains("!false"))||(logicalSimplification(e).contains("!false")) ) =>"true" // was all .equals before
    case E(t,Some(e))if((logicalSimplification(t).equals("false")|| (logicalSimplification(t).equals("!true")))&& logicalSimplification(e).equals("false")) =>"false"                                                                                                   // was .equals before
    case E(t,Some(e)) => logicalSimplification(t)+" || "+logicalSimplification(e) //d   val exp2c: Tree = parseAll(e, a).get
    case E(t,None) => logicalSimplification(t)


    case T(f,Some(t)) if(logicalSimplification(f).equals("false") || logicalSimplification(t).equals("false") )=> "false"  //do one for true to ? maybe idk im too tired to put the effort into knowing for sure, but probably. yes, do another one.
    //case T(f,Some(t)) if(logicalSimplification(f).contains("false") || logicalSimplification(t).contains("false") )=> "false"//(logicalSimplification(f)+" && "+logicalSimplification(t)).replace("false","").replace("||","")
    case T(f,Some(t)) if(logicalSimplification(f).equals("true") && logicalSimplification(t).equals("true") )=> "true"

    case T(f,Some(t)) if(logicalSimplification(f).equals("true") && (!logicalSimplification(t).equals("true") && (!logicalSimplification(t).equals("false"))))=> logicalSimplification(t) //and
    case T(f,Some(t)) if(logicalSimplification(t).equals("true") && (!logicalSimplification(f).equals("true") && (!logicalSimplification(f).equals("false"))))=> logicalSimplification(f)// and

    case T(f,Some(t)) => logicalSimplification(f)+" && "+logicalSimplification(t)
    case T(f,None) => logicalSimplification(f)



    case F(Some(exclaim),a) if(logicalSimplification(a).equals("true"))=> "false "
    case F(Some(exclaim),a) if(logicalSimplification(a).equals("false"))=> "true "
    case F(None,a) => logicalSimplification(a)
    case F(Some(exclaim),a) => "!"+logicalSimplification(a) // THIS IS GONA BE MORE WORK IN FUTURE! SHORT CUTTING HERE !<<<



    case A(Some(e),None) if(logicalSimplification(e).equals("true"))=> "true"
    case A(Some(e),None) if(logicalSimplification(e).equals("false"))=> "false"
    //case A(Some(e),None) =>  logicalSimplification(parseAll(elem(e = e), e.toString).get).toString //logicalSimplification(e)                //e.asInstanceOf[MicroV3.Parser[Tree]]
    case A(Some(e),None) => logicalSimplification(e)

    //case A(Some(e),None) if(!logicalSimplification(e).equals("true") && !logicalSimplification(e).equals("false"))=> logicalSimplification(e.en).toString//logicalSimplification(e)      // check these two: (1)
    case A(None,Some(c)) => logicalSimplification(c)     //                   (2)


    case C(None,None,Some(a)) => logicalSimplification(a) //true false or char
    case C(Some(t),None,None) => "true"
    case C(None,Some(f),None) => "false"
    case Ch(anychar)=> anychar.toString
  }
}

class CombinatorsV1 extends JavaTokenParsers{
  def plusc[Tree] = "+"
  def orc[Tree] = "||"
  def andc[Tree] = "&&"
  def notc[Tree] = "!"
  def prightc[Tree] = "("
  def pleftc[Tree] = ")"
  def trut[Tree]="true"
  def fals[Tree]="false"

  // E:= T '||' E | T

  def e: Parser[Tree] = t ~ orc ~ e ^^ { case l ~ o ~ r => E(l, Some(r)) } | t//^^ { case l => E(l, None) } //good
  // T:= F '&&' T | F
  // def t: Parser[Tree] = "[A-Za-z]".r ^^ { str => Ch(str.charAt(0)) }
  def t: Parser[Tree] = f ~ andc ~ t ^^ {case l ~ o ~ r => T(l,Some(r))} | f

  def f : Parser[Tree] = notc ~ a ^^ {case notc ~ a => F(Some(Exclaim(notc)),a)} | a //^^{case  a => F(None,a)}//^^ {case normalA => F(normalA,None)}
  //ef f : Parser[Tree] = a ^^ {str => (str.charAt(0).equals('!') => F(notc,a))} |a
  def a : Parser[Tree]= prightc ~ e ~ pleftc ^^{case x ~ y ~ z => A(Some(y),None)} | c //^^ {case justchar =>A(justchar,None)}
  def c : Parser[Tree] =  trut ^^ {case tru =>C(Some(true),None,None)} | fals ^^ {case falser =>C(None,Some(false),None)} | ch ^^ {case letr =>C(None,None,Some(letr))}
  def ch: Parser[Tree] ="[A-Za-z]".r ^^ { str => Ch(str.charAt(0)) } //good


}


//package Exmaple
//
//import scala.util.parsing.combinator._
//import scala.util.parsing.combinator.{JavaTokenParsers, PackratParsers}
//
//abstract class Tree
//case class E(t: Tree, tore: Option[Tree]) extends Tree
//case class T(fANDt : Tree, f: Option[Tree]) extends Tree
//case class F(a:Tree) extends Tree // ,nota:Option[exclaim]
//case class A(einParenthesis : Option[Tree],c : Option[Tree]) extends Tree
//case class C(tru :Option[Boolean], falsee: Option[Boolean], Ch :Option[Tree] ) extends Tree
//case class Ch(anychar:Character) extends Tree
//
//case class OR(t:Tree,e:Tree)extends Tree
//case class and(t:Tree,e:Tree)extends Tree
//case class not(t:Tree,e:Tree)extends Tree
//case class leftP(t:Tree)extends Tree
//case class RightP(t:Tree)extends Tree
//case class falso(t:String)extends Tree
//case class cierto(t:String)extends Tree
//case class Exclaim(s:String, t: Tree) extends Tree
//
//
////case object NIL extends Tree
//
//object MicroV3 extends CombinatorsV1{
//  def main(args: Array[String]){
//    do
//    {
//      System.out.println("Please enter a logical expression")
//      val a = scala.io.StdIn.readLine()
//      val exp2c: Tree = parseAll(e, a).get
//      println("The value of a is "+ a)
//      println("Whole tamale: "+ exp2c)
//
//      println(logicalSimplification(exp2c))
//
//
//    }while(5>4)
//  }
//
//
//  def logicalSimplification(exp: Tree):String= exp match {
//    //case class E(tORe: Tree, t: Option[Tree]) extends Tree
// // def e: Parser[Tree] = t ~ orc ~ e ^^ { case l ~ o ~ r => E(l, Some(r)) } | t
//
//    //case E(t,Some(e))if((logicalSimplification(t).contains("true")&& !logicalSimplification(t).contains("!true")) || logicalSimplification(e).contains("true")&& !logicalSimplification(e).contains("!true")) =>"true"            //OR
//    case E(t,Some(e)) => logicalSimplification(t)+" || "+logicalSimplification(e)
//    case E(t,None) => logicalSimplification(t)
//    case E(t,Some(e))if((logicalSimplification(t).contains("true")&& !logicalSimplification(t).contains("!true")) || logicalSimplification(e).contains("true")&& !logicalSimplification(e).contains("!true")) =>"true"          //OR
//
//
//    case T(f,Some(t)) if(logicalSimplification(f).contains("false") || logicalSimplification(t).contains("false") )=> "false "
//    case T(f,Some(t)) => logicalSimplification(f)+" && "+logicalSimplification(t)
//    case T(f,None) => logicalSimplification(f)                                                                                    // AND
//
//    case F(a) if(a.toString.equals("!true"))=> "false"
//    case F(a) if(a.toString.equals("!false"))=> "true"
//    case F(a) => logicalSimplification(a)
//    case F(a) => " !"+logicalSimplification(a) // THIS IS GONA BE MORE WORK IN FUTURE! SHORT CUTTING HERE !<<<       //NOT
//
//
//
//    case A(Some(e),None) => logicalSimplification(e)      // check these two: (1)
//    case A(None,Some(c)) => logicalSimplification(c)     //                   (2)
//    case C(None,None,Some(a)) => logicalSimplification(a)
//    case C(Some(t),None,None) => t.toString
//    case C(None,Some(f),None) => f.toString
//    case Ch(anychar)=> anychar.toString
//    //case Exclaim(_,a) if(a.toString.equals("!true"))=> "false "
//    //case Exclaim(_,a) if(a.toString.equals("!false"))=> "true "
//
//
//  }
//}
//
//class CombinatorsV1 extends JavaTokenParsers{
//  def plusc[Tree] = "+"
//  def orc[Tree] = "||"
//  def andc[Tree] = "&&"
//  def notc[Tree] = "!"
//  def prightc[Tree] = "("
//  def pleftc[Tree] = ")"
//  def trut[Tree]="true"
//  def fals[Tree]="false"
//
//  // E:= T '||' E | T
//
//  def e: Parser[Tree] = t ~ orc ~ e ^^ { case l ~ o ~ r => E(l, Some(r)) } | t //^^ { case l => E(l, None) } //good
//  // T:= F '&&' T | F
// // def t: Parser[Tree] = "[A-Za-z]".r ^^ { str => Ch(str.charAt(0)) }
//  def t: Parser[Tree] = f ~ andc ~ t ^^ {case l ~ o ~ r => T(l,Some(r))} | f
//
//  def f : Parser[Tree] = notc ~ a ^^ {case notc ~ a => F(Exclaim(notc,a))} | a //^^{case  a => F(a)}//^^ {case normalA => F(normalA,None)}
//   //ef f : Parser[Tree] = a ^^ {str => (str.charAt(0).equals('!') => F(notc,a))} |a
//  def a : Parser[Tree]= prightc ~ e ~ pleftc ^^{case x ~ y ~ z => A(Some(y),None)} | c //^^ {case justchar =>A(justchar,None)}
//  def c : Parser[Tree] =  trut ^^ {case tru =>C(Some(true),None,None)} | fals ^^ {case falser =>C(None,Some(false),None)} | ch ^^ {case letr =>C(None,None,Some(letr))}
//  def ch: Parser[Tree] ="[A-Za-z]".r ^^ { str => Ch(str.charAt(0)) } //good
//
//
//}
