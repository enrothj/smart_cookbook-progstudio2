package Smart_Cookbook

import scala.collection.mutable.Map
import scala.io.Source
import java.io.File
import java.io.PrintWriter
import scala.math

object Varasto {

  var varasto: Map[Aine, Double] = Map() // Muuttuja tallentaa kaikki jääkaapin sisältämät aineet ja niiden määrän.
  
  
  // Näillä metodeilla voidaan lisätä uusi aine varastoon tai poistaa olemassa oleva.
  def uusiAine(aine: Aine, maara: Double) = varasto(aine) = maara
  
  def poistaAine(aine: Aine) = varasto -= aine
  
  
  // Näillä metodeilla voidaan kasvattaa tai vähentää aineen määrää varastossa. Aineen määrän varastossa on aina oltava >= 0.0.
  def lisaaAinetta(aine: Aine, maara: Double) = varasto(aine) = varasto.get(aine).getOrElse(0.0) + maara
  
  def vahennaAinetta(aine: Aine, maara: Double) = varasto(aine) = if (varasto(aine) > maara) varasto(aine) - maara else 0.0 // Jos vähennettävä määrä on suurempi kuin varastoitu, varastoon jää 0.0
  
  
  // Tällä metodilla voidaan asettaa tietty arvo tietyn aineen määrälle (>= 0.0)
  def asetaMäärä(aine: Aine, maara: Double) = if (maara < 0.0) throw new Exception("Annettu määrä oli alle 0.0") else varasto(aine) = maara
  
  // Tämä metodi asettaa kaikkien varaston aineiden määräksi 0.0
  def nollaa() = varasto.foreach(x => x._2 * 0.0)
  
  // Tämä metodi poistaa kaikki varaston tiedot
  def tyhjennä() = varasto.empty
  
  
  // TODO: metodi jolla voidaan muuttaa tallennetun aineen mittayksikkö ja samalla määrä
}