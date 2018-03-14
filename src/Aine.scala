package Smart_Cookbook

import scala.io.Source
import scala.collection.mutable.Buffer
import java.io.File
import java.io.PrintWriter

class Aine(val nimi: String, val ainesosat: Vector[Tuple3[String, Int, Int]], val allergeenit: Vector[String], val kuvaus: String) {

  /*
   * Metodi tallentaa Aineen tekstitiedostolle, Reseptikirjan reseptikansioon. Sieltä se voidaan lukea myöhemmin tarvittaessa.
   * Ensimmäiselle riville tulee Aineen nimi, seuraaville riveille tulevat ainesosat, jokainen omalle rivilleen. Ainesosien jälkeen
   * seuraavalla rivillä on tähtimerkki ("*"), jonka jälkeen seuraavalla rivillä on Aineen allergeenit. Viimeisillä riveillä on
   * Aineen kuvaus. 
   */
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
  
}


// Kumppaniolio, jolla helpotetaan Aine-olioiden luomista, ja jolla voidaan luoda uusia Aine-olioita lukemalla niitä tekstitiedostoista.
object Aine {
  
  def apply(nimi: String, ainesosat: Vector[Tuple3[String, Int, Int]], allergeenit: Vector[String], kuvaus: String) = {
    new Aine(nimi: String, ainesosat: Vector[Tuple3[String, Int, Int]], allergeenit: Vector[String], kuvaus: String)
  }
  
  
  // metodi lukee tekstitiedoston ja luo tietojen perusteella uuden Aine-olion
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