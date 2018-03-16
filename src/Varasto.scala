package Smart_Cookbook

import scala.collection.mutable.Map
import scala.io.Source
import java.io.File
import java.io.PrintWriter

object Varasto {

  var varasto: Map[Aine, Double] = Map() // Muuttuja tallentaa kaikki jääkaapin sisältämät aineet ja niiden määrän.
  
  def uusiAine(aine: Aine, maara: Double) = varasto(aine) = maara
  
  def lisaaAinetta(aine: Aine, maara: Double) = varasto(aine) = varasto.get(aine).getOrElse(0.0) + maara
  
  def vahennaAinetta(aine: Aine, maara: Double) = {
    
    if (maara < varasto.get(aine).getOrElse(0.0)) varasto(aine) = varasto.get(aine).getOrElse(0.0) - maara
    else varasto -= aine
      
  }
  
  def tallenna() = {
    
    val tiedosto = new PrintWriter("jaakaappi.txt")
    
    try {
      
      for (rivi <- varasto) {
        tiedosto.println(rivi._1)
        tiedosto.println(rivi._2.toString)
        tiedosto.println("*")
      }
      
    } finally {
      tiedosto.close()
    }    
  }
  
  def lue() = {
    
    
    
  }
  
  
  
}