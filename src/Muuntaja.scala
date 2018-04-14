package Smart_Cookbook

/*
 * Muuntaja käsittelee ohjelman mittayksikkömuunnokset. Ohjelma tunnistaa ja käsittelee mittayksiköitä String-muodossa. 
 * 
 * Ohjelman käyttämät massan mittayksiköt 
 * ovat:
 *  -gramma 			("g")			
 *  -kilogramma 	("kg")		= 1000.0 g
 *  -naula 				("lb")		= 453.6 g
 *  -unssi 				("oz")		= 28.4 g
 *  
 *  Ohjelman käyttämät tilavuuden mittayksiköt ovat:
 *  -millilitra 	("ml")
 *  -desilitra 		("dl")		= 100.0 ml
 *  -litra				("l")			= 10.0 dl
 *  -teelusikka 	("tl")		= 5.0 ml
 *  -ruokalusikka ("rkl")		= 15.0 ml
 *  -kuppi 				("cup")		= 2.4 dl
 *  -pintti				("pint")	= 4.7 dl
 *  
 */



object Muuntaja {
  
  /*
   * Metodit tunnistaX ottavat parametreinaan mittayksikön (String) ja muuttavat sen Muuntajan käyttämään perusmittayksikköön (ml tai g).
   */
  private def tunnistaMassa(s: String): Double = {
    val massaGrammoina = match s {
      case "g"    => 1.0
      case "kg"   => 1000.0
      case "lb"   => 453.6
      case "oz"   => 28.4
      case _ => throw new Exception() //TODO: Metodin tulisi käsitellä poikkeukset, jolloin sille annetaan vääränlainen parametri (muu mittayksikkö tai kirjoitusvirhe tjsp).
    }
    massaGrammoina
  }
  
  private def tunnistaTilavuus(s: String): Double = {
    val massaMillilitroina = match s {
      case "ml"   => 1.0
      case "dl"   => 100.0
      case "l"    => 1000.0
      case "tl"   => 5.0
      case "rkl"  => 15.0
      case "cup"  => 240.0
      case "pint" => 470.0
      case _ => throw new Exception() // TODO
    }
    massaMillilitroina
  }
  
  /*
   * suhde-metodit ottavat parametreinaan kaksi mittayksikköä, ja palauttavat niiden suhteen.
   */
  
  // suhdeMassa laskee kahden massayksikön suhteen (esim kg / g = 1000)
  
  def suhdeMassa(a: String, b: String): Double = tunnistaMassa(a) / tunnistaMassa(b)
  
  // suhdeTilavuus laskee kahden tilavuuden yksikön suhteen (esim. l / dl = 10)
  
  def suhdeTilavuus(a: String, b: String): Double = tunnistaTilavuus(a) / tunnistaTilavuus(b)
  
  def suhde = ???
  
  // laske ottaa aineen tiheyden, aloitusmittayksikön ja määrän, ja palauttaa määrän halutussa mittayksikössä.
  def laske(d: Double, yksikkö1: String, määrä: Double, yksikkö2: String): Double = ???
  
  // muunna käyttää laske-metodia, mutta saa tiheyden ja aloitusmittayksikön annetulta Aine-oliolta.
  def muunna(aine: Aine, määrä: Double, yksikkö: String): Double = ???
  
}