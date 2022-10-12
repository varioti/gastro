import scala.io.Source
import scala.util.{Random, Using}

object Main {
  def main(args: Array[String]): Unit =
    GastroExtractor.extract("./products.csv") match {
      case Left(error) => println(f"AH ! Something wrong happened: $error")
      case Right(products) => runCli(products)
    }

  private def runCli(products: List[Product]): Unit = while (true) {
      val input = scala.io.StdIn.readLine(
        "How many kcal do you need ?"
      )
      println((new MenuComposer(products, input.toInt)).compose)
    }

}

class MenuComposer(products: List[Product], nb_kcal:Int) {

  private def getRandomProducts(howMany: Int): List[Product] = {
    products.length match {
      case n if n > 0 =>
        for { _ <- List.range(0, howMany) } yield products(Random.between(0, n))
      case _ => Nil
    }

  }

  def compose: String = {
    val randomProducts = getRandomProducts(4)
    println("----- I will try assembling the following products:")
    (0 until 3).foreach(i => println(f"--- $i --- " + randomProducts(i)))

    if (randomProducts.map(_.energy).foldLeft(0.0)(_ + _) >= nb_kcal)
      "The 4 selected products are at least --- kcal"
    else
      "The 4 selected products are less than --- kcal"
  }
}

case class Product(id: Int, name: String, new_energy: Int, new_protein: Float) {

  def energy: Int = new_energy
  def protein : Float = new_protein

  override def toString: String = f"$name (${id.toString})"
}

object GastroExtractor {

  def extract(path: String): Either[Throwable, List[Product]] =
    Using(Source.fromFile(path)) { source =>
      (for {
        line <- source.getLines.drop(1)
        cols = line.split(";")
      } yield Product(
        cols(0).toInt,
        cols(1),
        cols(4).toInt,
        cols(5).toFloat
      )).toList
    }.toEither

}
