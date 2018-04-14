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
   * suhde-metodit ottavat parametreinaan kaksi mittayksikköä, ja palauttavat niiden suhteen.
   */
  
  // suhdeMassa laskee kahden massayksikön suhteen (esim kg / g = 1000)
  
  def suhdeMassa(a: String, b: String): Double = {
    ???
  }
  
  // suhdeTilavuus laskee kahden tilavuuden yksikön suhteen (esim. l / dl = 10)
  
  def suhdeTilavuus(a: String, b: String): Double = {
    ???
  }
  
  def suhde = ???
  
  // laske ottaa aineen tiheyden, aloitusmittayksikön ja määrän, ja palauttaa määrän halutussa mittayksikössä.
  def laske(d: Double, yksikkö1: String, määrä: Double, yksikkö2: String): Double = ???
  
  // muunna käyttää laske-metodia, mutta saa tiheyden ja aloitusmittayksikön annetulta Aine-oliolta.
  def muunna(aine: Aine, määrä: Double, yksikkö: String): Double = ???
  
}