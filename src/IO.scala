package Smart_Cookbook

import scala.io.Source
import scala.collection.mutable.Buffer
import java.io.File
import java.io.PrintWriter

object IO {
  
  //TODO: korjaa metodit
  
  
  def kirjoita() = {
    
    val tiedosto = new PrintWriter("reseptit/" + this.nimi + ".txt")
    
    try {
      tiedosto.println(this.nimi)
      
      for (aine <- ainesosat) {
        tiedosto.println(aine._1 + "," + aine._2 + "," + aine._3)
      }
      tiedosto.println("*")
      tiedosto.println(allergeenit.mkString(","))
      tiedosto.println(this.kuvaus)
    } finally {
      tiedosto.close()
    }
    
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
    
    
  def lue(tiedostonimi: String): Aine = {
    
    var nimi: String = ""; var kuvaus: String = ""
    
    val tiedosto = Source.fromFile(tiedostonimi)
    
    var ainesosat: Buffer[Tuple3[String, Int, Int]] = Buffer()
    var allergeenit: Vector[String] = Vector()
    
    try {
      
      val riveja = tiedosto.getLines().toVector
      
      var rivinro = 1 // Muuttujan avulla tiedetään, mitä tietoja kyseiseltä tiedoston riviltä pitäisi löytyä, esimerkiksi ensimmäiseltä riviltä
                      // tulisi löytyä Aineen nimi.
      
      for (rivi <- riveja) {
        if (rivinro == 1) {nimi = rivi; rivinro += 1} // Ensimmäiseltä riviltä haetaan aineen nimi
        
        else if (rivinro == 2 && rivi != "*") { // Seuraavilta riveiltä (tähtimerkkiin asti) haetaan kaikki eri ainesosat, jotka kuuluvat aineeseen.
          
          val aines = rivi.split(",").toVector
          ainesosat += Tuple3(aines(1), aines(2).toInt, aines(3).toInt)
          
        } else if (rivinro == 2 && rivi == "*") rivinro += 1 // Tähtimerkki tarkoittaa, että enää ei käsitellä ainesosia, vaan siirrytään allergeeneihin.
        
        else if (rivinro == 3) { // Tältä riviltä löytyy lista Aineen sisältämistä allergeeneista.
          allergeenit = rivi.split(",").toVector
          rivinro += 1
        }
        
        else kuvaus = rivi // Viimeisillä riveillä on Aineen kuvaus.
        //TODO: Jos haluaa kuvauksen useammalle riville, tee muuttuja kuvaukselle ja lisää riveittäin
        
      }
        
    }
    
    apply(nimi, ainesosat.toVector, allergeenit, kuvaus) // Lopulta metodi palauttaa Aine-olion saatujen tietojen perusteella.
    
  }
  
}