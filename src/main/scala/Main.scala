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

  // ANONYMOUS FUNCTIONS
  var isApproximate = (value:Float, refvalue:Float, approximation:Float) => (value <= refvalue + approximation && value >= refvalue - approximation)

  private def getRandomProducts(howMany: Int): List[Product] = {
    products.length match {
      case n if n > 0 =>
        for { _ <- List.range(0, howMany) } yield products(Random.between(0, n))
      case _ => Nil
    }
  }

  private def getBalancedProducts(): List[Product] = {
    products.length match {
      case n if n > 0 =>
        // COMPREHENSION LIST
        for {
          product <- products
          totalNutr = product.protein + product.glucides + product.lipides 
          if (isApproximate(product.protein/totalNutr,0.25.toFloat,0.1.toFloat) && 
            isApproximate(product.glucides/totalNutr,0.4.toFloat,0.1.toFloat) && 
            isApproximate(product.lipides/totalNutr,0.35.toFloat,0.05.toFloat)) 
        } yield product
      case _ => Nil
    }
  }

  private def getMenu(nb_kcal: Int): List[List[Product]] = {
    val menus = products.combinations(2).toList
    for {
          menu <- menus
          totalEnergy = (menu.map(_.energy).foldLeft(0.0)(_ + _)).toFloat
          totalProt = (menu.map(_.protein).foldLeft(0.0)(_ + _)).toFloat
          totalGluc = (menu.map(_.glucides).foldLeft(0.0)(_ + _)).toFloat
          totalLip = (menu.map(_.lipides).foldLeft(0.0)(_ + _)).toFloat
          totalNutr = totalProt + totalGluc + totalLip 
          if (isApproximate(totalEnergy,nb_kcal,25.toFloat) &&
            isApproximate(totalProt/totalNutr,0.25.toFloat,0.05.toFloat) && 
            isApproximate(totalGluc/totalNutr,0.4.toFloat,0.05.toFloat) && 
            isApproximate(totalLip/totalNutr,0.35.toFloat,0.05.toFloat))
        } yield menu
  }

  def compose: String = {
    val balancedProducts = getBalancedProducts()
    val menu = getMenu(nb_kcal).head
    println("----- I will try assembling the following products:")
    (0 until menu.length).foreach(i => println(f"--- $i --- " + menu(i)))

    val total_energy = (menu.map(_.energy).foldLeft(0.0)(_ + _)).toFloat
    if (true)
      "The 4 selected products have a total of :" + total_energy.toString + "kcal"
    else
      "The 4 selected products are not " + nb_kcal.toString + "kcal, but " + total_energy.toString + "kcal"
  }
}

case class Product(id: Int, name: String, energy: Int, protein: Float,  glucides: Float, lipides: Float) {

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
        cols(5).toFloat,
        cols(6).toFloat,
        cols(9).toFloat
      )).toList
    }.toEither

}
