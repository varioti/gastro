import scala.io.Source
import scala.util.{Random, Using, Try, Success, Failure}

object Main {
  def main(args: Array[String]): Unit =
    while (true) {
      GastroExtractor.extract("./products.csv") match {
      case Right(products) => runCli(products)
      case Left(error) => {
        println(f"AH ! Something wrong happened while extracting products from the file : $error")
        val input = scala.io.StdIn.readLine("Press anything to retry.")
        }
      }
    }

  private def runCli(products: List[Product]): Unit = {
    // Print all menu available 
    println("Choices:") 
    println(" [1]: Menu composed by 3 healthy ingredients")
    println(" [2]: Menu composed by 1 proteined ingredient and 2 others ingredients")
    println(" [3]: Menu composed by 1 balanced ingredient (25% proteins - 40% glucids - 35% lipids)")
    println(" [4]: Menu composed by 3 random ingredients")
    
    // Loop : ask wich menu user want and print a proposal menu 
    while (true) {
      val input = scala.io.StdIn.readLine("What menu do you want ?")
      MenuComposer(products, input.toString).compose
    }
  }
}

// TRAIT, CASE CLASSES AND HIERARCHY
/* Each menu has a list of products and a method show used to print 
   the ingredients and compute the total energy of the menu*/
trait Menu {
  def menu: List[Product]

  def show(){
    println(this) // Print menu type
    (0 until menu.length).foreach(i => println(f"--- $i --- " + menu(i) + "[" + menu(i).energy.toString + " kcal]")) // Print each ingredient
    val total_energy = (menu.map(_.energy).foldLeft(0.0)(_ + _)).toFloat // Compute the total energy amount of the menu
    println("----- Total energy: " + total_energy + " kcal") // Print the total energy amount of the menu
  }
}

/* Case classes rewriting the toString method to show 
   the relevant informations of the specific menu */
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

/* Class which composes the menu depending on the user choice, 
   adds the ingrdients in a Menu class and then prints it*/
case class MenuComposer(products: List[Product], choice: Any) {

  // ANONYMOUS FUNCTION
  /* Compares a value to a refvalue with an approximation factor
     True if refvalue - approximation <= value <= refvalue + approximation 
     False otherwise */
  var isApproximate = (value:Float, refvalue:Float, approximation:Float) => (value <= refvalue + approximation && value >= refvalue - approximation)

  /* Returns a list of <howMany> random products */
  private def getRandomProducts(howMany: Int): List[Product] = {
    products.length match {
      case n if n > 0 =>
        for { _ <- List.range(0, howMany) } yield products(Random.between(0, n))
      case _ => Nil
    }
  }

  /* Return a list of <howMany> balanced products 
      - Balaned product has this ratio (25% proteins - 40% glucides - 35% lipids)
        with an approx factor of 5% */
  private def getBalancedProducts(howMany: Int): List[Product] = {
    val approx = 0.05
    products.length match {
      case n if n > 0 =>
        // COMPREHENSION LIST
        val bal_products = for {
          product <- products
          totalNutr = product.protein + product.sugar + product.fat // Computes the total amouny of nutriments to compute after the ratio 
          // Check that ratio computed is <approx> approximatively good
          if (isApproximate(product.protein/totalNutr,0.25.toFloat,approx.toFloat) && 
            isApproximate(product.sugar/totalNutr,0.4.toFloat,approx.toFloat) && 
            isApproximate(product.fat/totalNutr,0.35.toFloat,approx.toFloat)) 
        } yield product
        // Choose <howMany> random balanced products in this list
        for { _ <- List.range(0, howMany) } yield bal_products(Random.between(0, bal_products.length))
      case _ => Nil
    }
  }

  /* Return list of products where the amount of protein is more than <min_protein> */
  private def getProteinProduct(howMany: Int, min_protein: Float): List[Product] = {
    products.length match {
      case n if n > 0 =>
        // FILTER FUNCTION
        val protein = products.filter(p=>(p.protein > min_protein))
        // Chooses <howMany> random proteined products in this list
        for { _ <- List.range(0, howMany) } yield protein(Random.between(0, protein.length))
      case _ => Nil
    }
  }

  /* Return list of products where the amount of :
     - sugar is less than <max_sugar> and
     - fat is less than  <max_fat> */
  private def getHealthyProducts(howMany: Int, max_sugar: Float, max_fat:Float ): List[Product] = {
    products.length match {
      case n if n > 0 =>
        // FILTER FUNCTION
        val healthy = products.filter(p=>(p.sugar < max_sugar && p.fat < max_fat))
        // Chooses <howMany> random healthy products in this list
        for { _ <- List.range(0, howMany) } yield products(Random.between(0, healthy.length))
      case _ => Nil
    }
  }
  
  /* Generate the correct type of menu depending of the choice of the user */
  private def getMenu(choice: Any): Menu = {
    // PATTERN MATCHING
    choice match {
      case "1" => HealthyMenu(getHealthyProducts(3,5,8),3,5,8)
      case "2" => ProteinMenu((getProteinProduct(1,25) ++ getBalancedProducts(2)),2,25) //Protein menu composed of 1 proteined product and 2 balanced products
      case "3" => SimpleBalancedMenu(getBalancedProducts(1))
      case _ => RandomMenu(getRandomProducts(3),3) // Default (in case user enter other thing than 1,2,3 or 4)
    }
  }

  /* Generate the menu and show it */
  def compose: Unit = {
    val menu = getMenu(choice)
    menu.show
  }
}

/* Represent a product with needed parameters for the composer */
case class Product(id: Int, name: String, energy: Int, protein: Float,  sugar: Float, fat: Float) {
  override def toString: String = f"$name [${id.toString}]"
}

/* Extract the products from the file */
object GastroExtractor {

  def extract(path: String): Either[Throwable, List[Product]] =
    Using(Source.fromFile(path)) { source =>
      (for {
        line <- source.getLines.drop(1) 
        cols = line.split(";")
      } yield Product(
        cols(0).toInt,      // id
        cols(1),            // name
        cols(4).toInt,      // energy (in kcal)
        cols(5).toFloat,    // toal protein (in g)
        cols(6).toFloat,    // total sugar (in g)
        cols(9).toFloat     // total fat (in g)
      )).toList
    }.toEither
}