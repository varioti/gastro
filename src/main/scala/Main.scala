import scala.io.Source
import scala.util.{Random, Using}

object Main {
  def main(args: Array[String]): Unit =
    GastroExtractor.extract("./products.csv") match {
      case Left(error) => println(f"AH ! Something wrong happened: $error")
      case Right(products) => runCli(products)
    }

  private def runCli(products: List[Product]): Unit = while (true) {
      println((new MenuComposer(products)).compose)
      scala.io.StdIn.readLine(
        "Not satisfied? Hit any key to try another random menu composition out!"
      )
    }

}

class MenuComposer(products: List[Product]) {

  private def getRandomProducts(howMany: Int): List[Product] = {
    products.length match {
      case n if n > 0 =>
        for { _ <- List.range(0, howMany) } yield products(Random.between(0, n))
      case _ => Nil
    }

  }

  def compose: String = {
    val randomProducts = getRandomProducts(3)
    println("----- I will try assembling the following products:")
    (0 until 3).foreach(i => println(f"--- $i --- " + randomProducts(i)))

    if (randomProducts.map(_.quality).foldLeft(0.0)(_ + _) <= 1)
      "The three selected products are delicious together and form a healthy meal!"
    else
      "Beurk! never eat those together, it's either too fat, too sugarry or both..."
  }
}

case class Product(id: Int, name: String, energy: Int, protein: Float) {

  def quality: Double = if (energy < 30) 0.3 else if (protein > 20) 0.2 else 0.6

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
