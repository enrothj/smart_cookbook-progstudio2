package Smart_Cookbook

import scala.collection.mutable.Map
import scala.io.Source
import java.io.File
import java.io.PrintWriter
import scala.math

object Varasto {

  var varasto: Map[Aine, Double] = Map() // Muuttuja tallentaa kaikki jääkaapin sisältämät aineet ja niiden määrän.
  
  
  /*
   * Metodi onOlemassa tarkistaa onko ohjelmaan tallennettu parametrina annetun niminen aine.
   */
  def onOlemassa(nimi: String): Boolean = varasto.exists(_._1.nimi == nimi)
  
  // Näillä metodeilla voidaan lisätä uusi aine varastoon tai poistaa olemassa oleva.
  def uusiAine(aine: Aine, määrä: Double) = varasto(aine) = määrä
  
  def poistaAine(aine: Aine) = varasto -= aine
  
  
  // Näillä metodeilla voidaan kasvattaa tai vähentää aineen määrää varastossa. Aineen määrän varastossa on aina oltava >= 0.0.
  def lisaaAinetta(aine: Aine, määrä: Double) = varasto(aine) = varasto.get(aine).getOrElse(0.0) + määrä
  
  def vahennaAinetta(aine: Aine, määrä: Double) = varasto(aine) = if (varasto(aine) > määrä) varasto(aine) - maara else 0.0 // Jos vähennettävä määrä on suurempi kuin varastoitu, varastoon jää 0.0
  
  
  // Tällä metodilla voidaan asettaa tietty arvo tietyn aineen määrälle (>= 0.0)
  def asetaMäärä(aine: Aine, määrä: Double) = if (määrä < 0.0) throw new Exception("Annettu määrä oli alle 0.0") else varasto(aine) = määrä
  
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
  
  // TODO: metodi jolla voidaan muuttaa tallennetun aineen mittayksikkö ja samalla määrä
}