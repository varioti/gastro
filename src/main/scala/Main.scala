import scala.io.Source
import scala.util.{Random, Using, Try, Success, Failure}

import akka.actor.typed.{ActorRef, Behavior, Scheduler, ActorSystem}
import akka.actor.typed.scaladsl.Behaviors
import akka.util.Timeout

import scala.async.Async._

import scala.concurrent.{Future, ExecutionContext}
import scala.concurrent.duration.DurationInt

object Main {
  def main(args: Array[String]): Unit = {
    // Init coq
    val coqActor = ActorSystem(coq, "coq")
    ActorSystem.apply(coq, "coq")

    // Print all menus available 
    println("Choices:") 
    println(" [1]: Menu composed by 3 healthy ingredients")
    println(" [2]: Menu composed by 1 proteined ingredient and 2 others ingredients")
    println(" [3]: Menu composed by 1 balanced ingredient (25% proteins - 40% glucids - 35% lipids)")
    println(" [4]: Menu composed by 3 random ingredients")

    while (true) {
      Thread.sleep(1000) // Let Coq initiate the process for a menu before asking for new menu 
      val input = scala.io.StdIn.readLine("What menu do you want ?")
      coqActor ! AnyAnswer(input) // [1] Ask a menu to the Coq
    }
  }


  // ACTORS
  // Coq
  val coq: Behavior[Answer] = Behaviors.setup { context =>
    import akka.actor.typed.scaladsl.AskPattern._
    implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.global

    val intendant = context.spawn(intendantActor, "intendant")
    val proteinDispenser = context.spawn(proteinDispenserActor, "proteinDispenser")
    val fatDispenser = context.spawn(fatDispenserActor, "fatDispenser")

    implicit val timeout: Timeout = Timeout(3.seconds)
    implicit val scheduler: Scheduler = context.system.scheduler

    def ask_protein_products() : List[Product] = {
      proteinDispenser.ask(x => Question("protein", x))
      var protein = List[Product]()
      proteinDispenser ? (Question("protein", _)) onComplete {
        case Success(ProductsAnswer(result)) => protein = result
        case Failure(exception) => println(f"A failure occured: $exception")
      }
      protein
    }

    def ask_fat_products() : List[Product] = {
      fatDispenser.ask(x => Question("fat", x))
      var fat = List[Product]()
      fatDispenser ? (Question("fat", _)) onComplete {
        case Success(ProductsAnswer(result)) => fat = result
        case Failure(exception) => println(f"A failure occured: $exception")
      }
      fat
    }

    def ask_products() : Future[List[Product]] = {
      val protein = Future {ask_protein_products}
      val fat = Future {ask_fat_products}
      for {
        p <- protein
        f <- fat
      } yield (p ::: f)
    }

    // [1] Behaviour when receiving an order from client 
    Behaviors.receiveMessage { order => 
      order match {
        case AnyAnswer(menu_type) => {
          println(f"Order receiveid for menu $menu_type. Your menu will be ready soon.")
          
          // [2] Ask main product to Intendant
          intendant.ask(x => Question("main product", x))
          intendant ? (Question("main product", _)) onComplete {
            // Problem during product extraction by the Intendant : Coq must stop this menu
            case Success(MainProductAnswer(None)) => println("Please retry with correct products.csv file.")
            case Failure(exception) => println(f"A failure occured: $exception")
            // Intendant gived a product to start the menu : Coq can continue
            case Success(MainProductAnswer(Some(value))) => {
              val main_product = value
              println(main_product)

              val side_products = await(ask_products())
              println(side_products)
              
            }
          }
        }
        Behaviors.same
      }
    }
  }

  // Intendant
  val intendantActor: Behavior[Question] = Behaviors.receive { (_, message) =>
    {
      /* Select a product randomly in the products.csv file */
      def selectProduct(): Option[Product] = {
        GastroExtractor.extract("./products.csv") match {
          // Extraction OK -> Select 1 random product and put it in Some
          case Right(products) => Some(products(Random.between(0, products.length)))
          
          // Extraction KO -> Print error and return None
          case Left(error) => {
            println(f"AH ! Something wrong happened while extracting products from the file : $error")
            None
          }
        }
      }

      message match {
        // [2] Behaviour when Coq ask a main product
        case Question("main product", sender) =>
          val product = selectProduct()
          sender ! MainProductAnswer(product)
          Behaviors.same
      }
    }
  }

  // ProteinDispenser
  val proteinDispenserActor: Behavior[Question] = Behaviors.receive { (_, message) =>
    {
      /* Returns a list of <howMany> products where the amount of protein is more than <min_protein> */
      def selectProteinedProducts(howMany: Int, min_protein: Float): List[Product] = {
        GastroExtractor.extract("./products.csv") match {
          // Extraction OK -> Select random proteined products and put it in Some
          case Right(products) => {
            products.filter(p=>(p.protein > min_protein))
            // FILTER FUNCTION
            val protein = products.filter(p=>(p.protein > min_protein))
            // Chooses <howMany> random proteined products in this list
            for { _ <- List.range(0, howMany) } yield protein(Random.between(0, protein.length))
          }

          // Extraction KO -> Print error and return None
          case Left(error) => {
            println(f"AH ! Something wrong happened while extracting proteined products from the file : $error")
            List()
          }
        }
      }

      message match {
        // [3] Behavior when Coq ask proteined products
        case Question(_, sender) =>
          val products = selectProteinedProducts(1,20)
          sender ! ProductsAnswer(products)
          Behaviors.same
      }
    }
  }

  // FatDispenser
  val fatDispenserActor: Behavior[Question] = Behaviors.receive { (_, message) =>
    {
      /* Returns a list of <howMany> products where the amount of fat is more than <min_fat> */
      def selectFatProducts(howMany: Int, min_fat: Float): List[Product] = {
        GastroExtractor.extract("./products.csv") match {
          // Extraction OK -> Select random dat products and put it in Some
          case Right(products) => {
            products.filter(p=>(p.fat > min_fat))
            // FILTER FUNCTION
            val fat = products.filter(p=>(p.fat > min_fat))
            // Chooses <howMany> random fat products in this list
            for { _ <- List.range(0, howMany) } yield fat(Random.between(0, fat.length))
          }

          // Extraction KO -> Print error and return None
          case Left(error) => {
            println(f"AH ! Something wrong happened while extracting fat products from the file : $error")
            List()
          }
        }
      }

      message match {
        // [3] Behavior when Coq ask fat products
        case Question(_, sender) =>
          val products = selectFatProducts(1,20)
          sender ! ProductsAnswer(products)
          Behaviors.same
      }
    }
  }

  // SugarDispenser
  val sugarDispenser: Behavior[Question] = Behaviors.receive { (_, message) =>
    {
      def doSomething(): Unit = println(f"doing something...")

      message match {
        case Question(_, sender) =>
          doSomething()
          sender ! AnyAnswer("Done")
          Behaviors.same
      }
    }
  }

}

sealed trait Communication
case class Question(message: String, sender: ActorRef[Answer]) extends Communication
sealed trait Answer
case class AnyAnswer(message: Any) extends Answer
case class MainProductAnswer(message: Option[Product]) extends Answer
case class ProductsAnswer(message: List[Product]) extends Answer

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
   adds the ingredients in a Menu class and then prints it*/
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

  /* Returns a list of <howMany> balanced products 
     Balanced product has this ratio (25% proteins - 40% glucides - 35% lipids)
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

  /* Returns a list of <howMany> products where the amount of protein is more than <min_protein> */

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

  /* Returns a list of <howMany> products where the amount of sugar and fat is less than <max_sugar> and <max_fat> */
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
      case "2" => ProteinMenu((getProteinProduct(1,20) ++ getRandomProducts(2)),2,20) //Protein menu composed of 1 proteined product and 2 random products
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
        cols(5).toFloat,    // total protein (in g)
        cols(6).toFloat,    // total sugar (in g)
        cols(9).toFloat     // total fat (in g)
      )).toList
    }.toEither
}