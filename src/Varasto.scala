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
  
  
}