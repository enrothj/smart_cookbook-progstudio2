package Smart_Cookbook

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