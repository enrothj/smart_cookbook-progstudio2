package Smart_Cookbook

import scala.collection.mutable.Map
import scala.collection.mutable.Buffer

object Varasto {

  var varasto: Map[Aine, Double] = Map() // Muuttuja tallentaa kaikki jääkaapin sisältämät aineet ja niiden määrän.
  
  
  /*
   * Metodi onOlemassa tarkistaa onko ohjelmaan tallennettu parametrina annetun niminen aine.
   */
  def onOlemassa(nimi: String): Boolean = varasto.exists(_._1.nimi == nimi)
  
  // Näillä metodeilla voidaan lisätä uusi aine varastoon tai poistaa olemassa oleva.
  def uusiAine(aine: Aine, määrä: Double) = varasto(aine) = määrä
  
  def poistaAine(nimi: String) = {
    if (!onOlemassa(nimi)) throw new OlematonAine("Kyseistä ainetta ei ole ohjelman muistissa.", nimi)
    else varasto -= Hakukone.haeNimi(nimi).get
  }
  
  
  // Näillä metodeilla voidaan kasvattaa tai vähentää aineen määrää varastossa. Aineen määrän varastossa on aina oltava >= 0.0.
  def lisaaAinetta(aine: Aine, määrä: Double) = varasto(aine) = varasto.get(aine).getOrElse(0.0) + määrä
  
  def vahennaAinetta(aine: Aine, määrä: Double) = varasto(aine) = if (varasto(aine) > määrä) varasto(aine) - määrä else 0.0 // Jos vähennettävä määrä on suurempi kuin varastoitu, varastoon jää 0.0
  
  
  // Tällä metodilla voidaan asettaa tietty arvo tietyn aineen määrälle (>= 0.0)
  def asetaMäärä(aine: Aine, määrä: Double) = {
    require(määrä >= 0.0)
    varasto(aine) = määrä
  }
  
  // Tämä metodi asettaa kaikkien varaston aineiden määräksi 0.0
  def nollaa() = varasto.foreach(x => x._2 * 0.0)
  
  // Tämä metodi poistaa kaikki varaston tiedot
  def tyhjennä() = varasto.empty
  
  
  // Metodi listaaAineet palauttaa kaksiulotteisen taulukon, jossa on jokaisen aineen nimi, määrä ja allergeenit. Käytetään käyttöliittymän pääikkunan listaa varten
  def listaaAineet: Array[Array[Any]] = {
    
    var rivit: Buffer[Array[Any]] = Buffer()
    
    for (aine <- varasto) {
      val aineenNimi        = aine._1.nimi
      val aineenMäärä       = aine._2
      val aineenAllergeenit = aine._1.allergeenit.mkString(", ")
      
      val taulukko = Array(aineenNimi, aineenMäärä, aineenAllergeenit)
      
      rivit += taulukko
    }
    
    rivit.toArray
  }
  
  /*
   * Metodilla muutaYksikkö voidaan muuttaa Aine-olion mittayksikkö, siten, että samalla lasketaan sen määrä varastossa uudessa mittayksikössä.
   * Aineen aiempi yksikkö ei saa olla "kpl" eikä myöskään kohdeyksikkö, koska kappaleista ei voi tehdä muunnoksia.
   */
  def muutaYksikkö(aine: Aine, yksikkö: String) = {
    require(Muuntaja.massat.contains(yksikkö) || Muuntaja.tilavuudet.contains(yksikkö) || yksikkö == "kpl" || aine.mittayksikkö != "kpl")
    
    aine.määrä        = Muuntaja.muunna(aine, aine.määrä, yksikkö)    // Asetetaan aineen uusi määrä uuden yksikön mukaiseksi
    
    asetaMäärä( aine, Muuntaja.muunna(aine, varasto(aine), yksikkö) ) // Muutetaan aineen määrä varastossa uuden yksikön mukaiseksi
    
    aine.mittayksikkö = yksikkö                                       // Muutetaan aineen perusmittayksiköksi annettu yksikkö.
  }
}