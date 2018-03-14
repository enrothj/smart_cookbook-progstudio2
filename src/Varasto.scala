package Smart_Cookbook

import scala.collection.mutable.Map
import scala.io.Source
import java.io.File
import java.io.PrintWriter

object Jaakaappi {

  var varasto: Map[String, Double] = Map() // Muuttuja tallentaa kaikki jääkaapin sisältämät aineet ja niiden määrän.
  
  def uusiAine(nimi: String, maara: Double) = varasto(nimi) = maara
  
  def lisaaAinetta(nimi: String, maara: Double) = varasto(nimi) = varasto.get(nimi).getOrElse(0.0) + maara
  
  def vahennaAinetta(nimi: String, maara: Double) = {
    
    if (maara < varasto.get(nimi).getOrElse(0.0)) varasto(nimi) = varasto.get(nimi).getOrElse(0.0) - maara
    else varasto -= nimi
      
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