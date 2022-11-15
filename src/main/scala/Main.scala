import scala.io.Source
import scala.util.{Random, Using, Try, Success, Failure}

import akka.actor.typed.{ActorRef, Behavior, Scheduler, ActorSystem}
import akka.actor.typed.scaladsl.Behaviors
import akka.util.Timeout

import scala.concurrent.{Future, ExecutionContext, Await}
import scala.concurrent.duration.DurationInt

object Main {

  def apply(): Behavior[Option[String]] =
    Behaviors.setup { context =>

      Behaviors.receiveMessage { message =>
        message match {
          case Some(menu) => println(menu)
          case None => println("Sorry I could't make a menu for your order. You can retry.")
        }
        Behaviors.same
      }
    }

  def main(args: Array[String]): Unit = {
    // Init coq
    val system: ActorSystem[Option[String]] = ActorSystem(Main(), "system")
    val coqActor = ActorSystem(coq, "coq")

    // Print all menus available 
    println("To order a menu, please indicate the number of kcal you want and press Enter.") 
    println(" - The number of kcal indicated must be over 200")
    println(" - Otherwise (or if you press nothing), a default value of 500 kcal will be used.")
    println(" - Each time you enter an input, an order for a new menu is created.")
    println(" - You can ask a new menu even if the precedent menu is not ready.")

    while (true) {
      Thread.sleep(100) // Let Coq initiate the process for a menu before asking for new menu 
      val input = scala.io.StdIn.readLine("I am listening to your order.")
      coqActor.tell(MenuOrder(input, system.ref)) // [1] Ask a menu to the Coq
    }
  }


  // ANONYMOUS FUNCTION
  /* Compares a value to a refvalue with an approximation factor
     True if refvalue - approximation <= value <= refvalue + approximation 
     False otherwise */
  var isApproximate = (value:Float, refvalue:Float, approximation:Float) => (value <= refvalue + approximation && value >= refvalue - approximation)


  // ACTORS:
  // COQ /////////////////////////////////////////////////////////////////////////
  val coq: Behavior[Answer] = Behaviors.setup { context =>
    import akka.actor.typed.scaladsl.AskPattern._
    implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.global

    val intendant = context.spawn(intendantActor, "intendant")
    val proteinDispenser = context.spawn(proteinDispenserActor, "proteinDispenser")
    val lipideDispenser = context.spawn(lipideDispenserActor, "lipideDispenser")
    val glucideDispenser = context.spawn(glucideDispenserActor, "glucideDispenser")

    implicit val timeout: Timeout = Timeout(3.seconds)
    implicit val scheduler: Scheduler = context.system.scheduler

    /* Ask to all dispensers  some of them products and returns a Future merged list of all products
       Note: Because Future are used, nothing is blocking inside this function */
    def ask_products : Future[List[Product]] = {
      val protein : Future[Answer] = proteinDispenser.ask(x => Request("protein", x))
      val lipide : Future[Answer] = lipideDispenser.ask(x => Request("lipide", x))
      val glucide : Future[Answer] = glucideDispenser.ask(x => Request("glucide", x))
      for {
        p <- protein
        f <- lipide
        s <- glucide
        lp : List[Product] = p.message match {
          case x: List[Product] => x
          case _ => List[Product]()
        }
        lf : List[Product] = f.message match {
          case x: List[Product] => x
          case _ => List[Product]()
        }
        ls : List[Product] = s.message match {
          case x: List[Product] => x
          case _ => List[Product]()
        }
      } yield lp ::: lf ::: ls
    }

    /* Choose which side ingredients will compose the menu in addition of the main product */
    def choose_products(main_product: Product, side_products: List[Product], nb_kcal: Int) : List[Product] = {
      val menus : List[List[Product]] = side_products.combinations(3).toList ::: side_products.combinations(2).toList ::: side_products.combinations(1).toList
      val balanced_menus = for {
        menu <- menus
        totalEnergy = (menu.map(_.energy).foldLeft(0.0)(_ + _)).toFloat + main_product.energy
        totalProt = (menu.map(_.protein).foldLeft(0.0)(_ + _)).toFloat + main_product.protein
        totalGluc = (menu.map(_.glucide).foldLeft(0.0)(_ + _)).toFloat + main_product.glucide
        totalLip = (menu.map(_.lipide).foldLeft(0.0)(_ + _)).toFloat + main_product.lipide
        totalNutr = totalProt + totalGluc + totalLip 
        if (isApproximate(totalEnergy,nb_kcal.toFloat,75.toFloat) &&
          isApproximate(totalProt/totalNutr,0.25.toFloat,0.08.toFloat) && 
          isApproximate(totalGluc/totalNutr,0.4.toFloat,0.08.toFloat) && 
          isApproximate(totalLip/totalNutr,0.35.toFloat,0.08.toFloat))
      } yield menu
      // Choose a menu respecting the constraints randomly 
      balanced_menus.length match {
        // If no menu found, print message and return empty list for side products
        case 0 => {
          println("No side products respecting the constraints found")
          List[Product]()
        }
        case n => balanced_menus(Random.between(0, n))
      } 
    }

    // [1] Behaviour when receiving an order from client 
    Behaviors.receiveMessage { order => 
      order match {
        case MenuOrder(request, sender) => {
          // Get the amount of KCAL wanted by the user: if not a valid number use default value of 500 KCAL
          val nb_kcal = request.toString.toInt match {
            case x : Int if x >= 200 => x
            case _ => 500
          }
          println(f"Order received for menu with $nb_kcal kcal. Your menu will be ready soon.")
          
          // [2] Ask main product to Intendant
          intendant.ask(x => MainProductRequest(nb_kcal, x))
          intendant ? (MainProductRequest(nb_kcal, _)) onComplete {
            // Problem during product extraction by the Intendant : Coq must stop this menu
            case Success(MainProductAnswer(None)) => {
              println("Please retry with correct products.csv file.")
              sender ! None
            }
            case Failure(exception) => {
              println(f"A failure occured: $exception")
              sender ! None
            }

            // Intendant gived a product to start the menu : Coq can continue
            case Success(MainProductAnswer(Some(main_product))) => {
              val side_products = Await.result(ask_products, 20.second) // [3] Ask side products to all dispensers and wait until the co
              val ingredients = choose_products(main_product, side_products, nb_kcal) :+ main_product // [4] Coq choose the final ingredients and

              // [5] Ask portions to intendant
              intendant.ask(x => QTTRequest(ingredients, x))
              intendant ? (QTTRequest(ingredients, _)) onComplete {
                case Success(PortionsAnswer(portions)) => sender ! Some(Menu(ingredients, portions).toString)
                case Failure(exception) => {
                  println(f"A failure occured while getting the portions: $exception")
                  sender ! None
                }
                case _ => sender ! None
              }
            }

            case _ => sender ! None
          }
        }
        // Coq should not receive other messages
        case _ => println("Not a valid request for Coq")
      } 
      Behaviors.same
    }
  }

  // INTENDANT /////////////////////////////////////////////////////////////////////////
  val intendantActor: Behavior[Question] = Behaviors.receive { (_, message) =>
    {
      /* Select a product randomly in the products.csv file */
      def selectProduct(nb_kcal: Int): Option[Product] = {
        GastroExtractor.extract("./products.csv") match {
          // Extraction OK -> Select 1 random product and put it in Some
          case Right(products) => {
            val products_under = products.filter(_.energy < nb_kcal*0.75)
            if (products_under.length == 0) {None}
            else {Some(products_under(Random.between(0, products_under.length)))}
          }
          
          // Extraction KO -> Print error and return None
          case Left(error) => {
            println(f"AH ! Something wrong happened while extracting products from the file : $error")
            None
          }
        }
      }

      /* Get the quantity of each product in ref and return a new list of produts with the new quantities 
         Note: if no quantities found in file, quantity of the product remains 1*/
      def getQTT(ref: List[Product]) : List[String] = {
          GastroExtractor.extractQTT("./portions.csv") match {
            // Extraction KO -> Print error and return the initial list (with all portions to 1)
            case Left(error) => {
              println(f"AH ! Something wrong happened while extracting portions from the file : $error")
              ref.map( _ => "1" )

            }
          // Extraction OK -> Choose randomly a qtt
          case Right(portions) => {
            for {
              product <- ref
              qtts = findQTT(product, portions)
              chosen_qtt = qtts.length match {
                case 0 => "1"
                case n => qtts(Random.between(0, n))
              }
            } yield chosen_qtt
          }
        }
      }

      /* For a product, search all qtt available in portions */
      def findQTT(refProduct:Product, portions:List[Portion]) : List[String] = {
        // FOR COMPREHENSION => MAP AND FILTER
        /* 
          for { p <- portions if p.id == refProduct.id } yield p.name
        */
        portions filter (p => p.id == refProduct.id) map (p => p.name)
      }

      message match {
        // [2] Behaviour when Coq ask a main product
        case MainProductRequest(nb_kcal, sender) =>
          val product = selectProduct(nb_kcal)
          sender ! MainProductAnswer(product)
          Behaviors.same

        // [5] Behaviour when Coq ask portions
        case QTTRequest(products, sender) =>
          val qtt = getQTT(products)
          sender ! PortionsAnswer(qtt)
          Behaviors.same
        
        case _ => 
          println("Not a valid request for Intendant")
          Behaviors.same
      }
    }
  }

  // PROTEIN DISPENSER /////////////////////////////////////////////////////////////////////////
  val proteinDispenserActor: Behavior[Request] = Behaviors.receive { (_, message) =>
    {
      /* Returns a list of <howMany> products where the amount of protein is more than <min_protein> */
      def selectProteinedProducts(howMany: Int): List[Product] = {
        GastroExtractor.extract("./products.csv") match {
          // Extraction OK -> Select random proteined products and put it in Some
          case Right(products) => {
            // FILTER FUNCTION
            val protein = products.filter(p=>(p.protein > p.lipide + p.glucide))
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
        case Request(_, sender) =>
          val products = selectProteinedProducts(50)
          sender ! ProductsAnswer(products)
          Behaviors.same

        case _ => 
          println("Not a valid request for Dispenser")
          Behaviors.same
      }
    }
  }

  // LIPIDE DISPENSER /////////////////////////////////////////////////////////////////////////
  val lipideDispenserActor: Behavior[Request] = Behaviors.receive { (_, message) =>
    {
      /* Returns a list of <howMany> products where the amount of lipide is more than <min_lipide> */
      def selectLipideProducts(howMany: Int): List[Product] = {
        GastroExtractor.extract("./products.csv") match {
          // Extraction OK -> Select random dat products and put it in Some
          case Right(products) => {
            // FILTER FUNCTION
            val lipide = products.filter(p=>(p.lipide > p.protein + p.glucide))
            // Chooses <howMany> random lipide products in this list
            for { _ <- List.range(0, howMany) } yield lipide(Random.between(0, lipide.length))
          }

          // Extraction KO -> Print error and return Empty list
          case Left(error) => {
            println(f"AH ! Something wrong happened while extracting lipide products from the file : $error")
            List()
          }
        }
      }

      message match {
        // [3] Behavior when Coq ask lipide products
        case Request(_, sender) =>
          val products = selectLipideProducts(50)
          sender ! ProductsAnswer(products)
          Behaviors.same

        case _ => 
          println("Not a valid request for Dispenser")
          Behaviors.same
      }
    }
  }

  // GLUCIDE DISPENSER /////////////////////////////////////////////////////////////////////////
  val glucideDispenserActor: Behavior[Request] = Behaviors.receive { (_, message) =>
    {
      /* Returns a list of <howMany> products where the amount of glucide is more than <min_glucide> */
      def selectGlucideProducts(howMany: Int): List[Product] = {
        GastroExtractor.extract("./products.csv") match {
          // Extraction OK -> Select random glucide products and put it in Some
          case Right(products) => {
            // FILTER FUNCTION
            val glucide = products.filter(p=>(p.glucide > p.lipide + p.protein))
            // Chooses <howMany> random lipide products in this list
            for { _ <- List.range(0, howMany) } yield glucide(Random.between(0, glucide.length))
          }

          // Extraction KO -> Print error and return Empty list
          case Left(error) => {
            println(f"AH ! Something wrong happened while extracting glucide products from the file : $error")
            List()
          }
        }
      }

      message match {
        // [3] Behavior when Coq ask lipide products
        case Request(_, sender) =>
          val products = selectGlucideProducts(50)
          sender ! ProductsAnswer(products)
          Behaviors.same

        case _ => 
          println("Not a valid request for Dispenser")
          Behaviors.same
      }
    }
  }
}


// COMMUNICATION /////////////////////////////////////////////////////////////////////////
// Question: Coq -> Intendant & Dispensers
sealed trait Question { 
  def message: Any
  def sender: ActorRef[Answer]
}
case class Request(message: String, sender: ActorRef[Answer]) extends Question // -> Coq ask Dispensers
case class MainProductRequest(message: Int, sender: ActorRef[Answer]) extends Question // -> Coq ask Intendant for Main Product (message is nb_kcal of the menu)
case class QTTRequest(message: List[Product], sender: ActorRef[Answer]) extends Question // -> Coq ask Intendant for Portions (corresponding to the list of products in message)

// Answer: Main, Intendant & Dispensers -> Coq
sealed trait Answer {
  def message: Any
}
case class MenuOrder(message: Any, sender: ActorRef[Option[String]]) extends Answer // -> Main (= sender) send to Coq for a menu, message contains the input of the user
case class MainProductAnswer(message: Option[Product]) extends Answer // -> Intendant send to Coq the product selected in Option (None mean problem)
case class ProductsAnswer(message: List[Product]) extends Answer // -> Dispensers send to Coq the a list of products
case class PortionsAnswer(message: List[String]) extends Answer // -> Intendant send to Coq the portions (in string) of the selected products

// Menu suggestion: Coq -> Main
case class MenuSuggestion(message: Option[String]) // message is the suggested menu in an Option monad

// Menu: represents a Menu and is usefull to get a menu in String format
case class Menu(menu: List[Product], portions: List[String]) {
  override def toString: String = {
    val total_energy = (menu.map(_.energy).foldLeft(0.0)(_ + _)).toFloat // Compute the total energy amount of the menu
    val str_ingredients: List[String] = for {
      (ingr, qtt) <- menu zip portions
      ingr_energy = ingr.energy
    } yield f"       - $qtt of $ingr [$ingr_energy kcal]\n" // Create a string for each ingredient
    
    // Final string: Header (1 line) + Ingredients (1 line/ingredient) + Footer (1 line)
    f"\n\n----- Menu of ${menu.length.toString} ingredients:\n" + str_ingredients.mkString + "----- Total energy: " + total_energy + " kcal\n"
  }
}

// EXTRACTION /////////////////////////////////////////////////////////////////////////
/* Represent a product with needed parameters for the composer */
case class Product(id: Int, name: String, energy: Int, protein: Float,  glucide: Float, lipide: Float) {
  override def toString: String = f"$name [${id.toString}]"
}

/* Represent a portion with needed parameters for the composer */
case class Portion(id: Int, name: String, qtt: Float)

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
        cols(6).toFloat,    // total glucide (in g)
        cols(9).toFloat     // total lipide (in g)
      )).toList
    }.toEither

  /* Extract the portions from the file */
  def extractQTT(path: String): Either[Throwable, List[Portion]] =
    Using(Source.fromFile(path)) { source =>
      (for {
        line <- source.getLines.drop(1) 
        cols = line.split(";")
      } yield Portion(
        cols(0).toInt,      // id
        cols(7),            // name
        cols(8).replace(",",".").toFloat,    // weight (in g)
      )).toList
    }.toEither
}