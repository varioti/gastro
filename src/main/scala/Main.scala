import scala.io.Source
import scala.util.{Random, Using}

object Main {
  def main(args: Array[String]): Unit =
    GastroExtractor.extract("./products.csv") match {
      case Left(error) => println(f"AH ! Something wrong happened: $error")
      case Right(products) => runCli(products)
    }

  private def runCli(products: List[Product]): Unit = {
    println("Choices:") 
    println(" [1]: Menu composed by 3 healthy ingredients")
    println(" [2]: Menu composed by 1 proteined ingredient and 2 others ingredients")
    println(" [3]: Menu composed by 1 balanced ingredient (25% proteins - 40% glucids - 35% lipids)")
    println(" [4]: Menu composed by 3 random ingredients")
    while (true) {
      val input = scala.io.StdIn.readLine("What menu do you want ?")
      MenuComposer(products, input.toString).compose
    }
  }
}

trait Menu {
  def menu: List[Product]

  def show(){
    println(this)
    (0 until menu.length).foreach(i => println(f"--- $i --- " + menu(i) + "[" + menu(i).energy.toString + " kcal]"))
    val total_energy = (menu.map(_.energy).foldLeft(0.0)(_ + _)).toFloat
    println("----- Total energy: " + total_energy + " kcal")
  }
}

case class RandomMenu(menu:List[Product], nb_ingredients:Int) extends Menu {
  override def toString: String = f"----- Random menu of ${nb_ingredients.toString} ingredients:"
}

case class HealthyMenu(menu:List[Product], nb_ingredients:Int, max_sugar:Float, max_fat:Float) extends Menu {
  override def toString: String = f"----- Menu of ${nb_ingredients.toString} healthy ingredients (max ${max_sugar.toString}g of sugar & max ${max_fat.toString}g of fat each):"
}

case class ProteinMenu(menu:List[Product], nb_side_ingredients:Int, min_protein:Float) extends Menu {
  override def toString: String = f"----- Menu of 1 ingredient with min ${min_protein.toString}g of protein and ${nb_side_ingredients.toString} side ingredients"
}

case class SimpleBalancedMenu(menu:List[Product]) extends Menu {
  override def toString: String = "----- Menu with 1 balanced ingredient (25% proteins - 40% glucids - 35% lipids)"
}

case class MenuComposer(products: List[Product], choice: Any) {

  // ANONYMOUS FUNCTIONS
  var isApproximate = (value:Float, refvalue:Float, approximation:Float) => (value <= refvalue + approximation && value >= refvalue - approximation)

  private def getRandomProducts(howMany: Int): List[Product] = {
    products.length match {
      case n if n > 0 =>
        for { _ <- List.range(0, howMany) } yield products(Random.between(0, n))
      case _ => Nil
    }
  }

  private def getBalancedProducts(howMany: Int): List[Product] = {
    products.length match {
      case n if n > 0 =>
        // COMPREHENSION LIST
        val bal_products = for {
          product <- products
          totalNutr = product.protein + product.sugar + product.fat 
          if (isApproximate(product.protein/totalNutr,0.25.toFloat,0.1.toFloat) && 
            isApproximate(product.sugar/totalNutr,0.4.toFloat,0.1.toFloat) && 
            isApproximate(product.fat/totalNutr,0.35.toFloat,0.05.toFloat)) 
        } yield product
        for { _ <- List.range(0, howMany) } yield bal_products(Random.between(0, bal_products.length))
      case _ => Nil
    }
  }

  private def getProteinProduct(howMany: Int, min_protein: Float): List[Product] = {
    products.length match {
      case n if n > 0 =>
        // FILTER FUNCTION
        val protein = products.filter(p=>(p.protein > min_protein))
        for { _ <- List.range(0, howMany) } yield protein(Random.between(0, protein.length))
      case _ => Nil
    }
  }

  private def getHealthyProducts(howMany: Int, max_sugar: Float, max_fat:Float ): List[Product] = {
    products.length match {
      case n if n > 0 =>
        // FILTER FUNCTION
        val healthy = products.filter(p=>(p.sugar < max_sugar && p.fat < max_fat))
        for { _ <- List.range(0, howMany) } yield products(Random.between(0, healthy.length))
      case _ => Nil
    }
  }

  private def getMenu(choice: Any): Menu = {
    choice match {
      case "1" => HealthyMenu(getHealthyProducts(3,5,8),3,5,8)
      case "2" => ProteinMenu((getProteinProduct(1,20) ++ getRandomProducts(2)),2,20)
      case "3" => SimpleBalancedMenu(getBalancedProducts(3))
      case _ => RandomMenu(getRandomProducts(3),3)
    }
  }

  def compose: Unit = {
    val menu = getMenu(choice)
    menu.show
  }
}

case class Product(id: Int, name: String, energy: Int, protein: Float,  sugar: Float, fat: Float) {

  override def toString: String = f"$name [${id.toString}]"
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