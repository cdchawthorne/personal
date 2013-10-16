import scala.io._

def toInt(in: String) : Option[Int] =
  try {
    Some(Integer.parseInt(in.trim))
  } catch {
    case e: NumberFormatException => None
  }

def sum(in: Seq[String]) = {
  val ints = in.flatMap(s => toInt(s))
  ints.foldLeft(0)((a,b) => a + b)
}

println("Enter some numbers, then EOF")
val input = Source.fromInputStream(System.in)
val lines = input.getLines.collect

println("Sum " + sum(lines))
