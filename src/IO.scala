package Smart_Cookbook

import scala.io.Source
import scala.collection.mutable.Buffer
import java.io.File
import java.io.PrintWriter

object IO {
  
  //TODO: korjaa metodit
  
  
  /*
   * Metodi tallentaa Aineen tekstitiedostolle, Reseptikirjan reseptikansioon. Sieltä se voidaan lukea myöhemmin tarvittaessa.
   * Ensimmäiselle riville tulee Aineen nimi. Toiselle riville tulee aineen tiheys, määrä ja mittayksikkö. Seuraaville riveille tulevat ainesosat, 
   * jokainen omalle rivilleen. Ainesosien jälkeen seuraavalla rivillä on tähtimerkki ("*") erottimena, jonka jälkeen seuraavalla rivillä on Aineen 
   * allergeenit. Viimeisillä riveillä on Aineen kuvaus.
   * 
   * Esimerkki:
   * Spagetti bolognese
   * 0.0,4.0,kpl
   * spagetti,300.0,g
   * kastike,800.0,g
   * *
   * liha,tomaatti
   * Spagetti bolognese, neljä annosta. Paista jauheliha... jne.
   * 
   */
  def kirjoita(aine: Aine) = {
    
    val tiedosto = new PrintWriter("reseptit/" + aine.nimi + ".txt")
    
    try {
      tiedosto.println(aine.nimi)                                                // 1. rivi: aineen kuvaus
      
      tiedosto.println(aine.tiheys + "," + aine.määrä + "," + aine.mittayksikkö) // 2. rivi: tiheys,määrä,mittayksikkö
      
      for (aines <- aine.ainesosat) {                                            // seuraavat rivit: ainesosa, sen määrä, sen mittayksikkö (jokainen omalla rivillään)
        tiedosto.println(aines._1 + "," + aines._2 + "," + aines._3)
      }
      tiedosto.println("*")                                                      // ainesosien loppu erotetaan tähtimerkillä      
      
      tiedosto.println(aine.allergeenit.mkString(","))                           // allergeenit tähtimerkin jälkeen olevalle riville
      
      tiedosto.println(aine.kuvaus)                                              // Viimeiselle riville aineen kuvaus
      
    } finally {
      tiedosto.close()
    }
    
  }  // TODO: poikkeusten käsittely
    
  
  /*
   * Metodi tallenna tallentaa Varaston sisältämät tiedot tekstitiedostolle. Jokaselle riville tulee aineen nimi ja sen määrä, esim. "spagetti bolognese, 4.0".
   */
  def tallenna() = {
    
    val tiedosto = new PrintWriter("jaakaappi.txt")
    
    try {
      
      for (rivi <- Varasto.varasto) {
        tiedosto.println(rivi._1.nimi + ", " + rivi._2)
      }
      
    } finally {
      tiedosto.close()
    }    
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
    
    Aine(nimi, ainesosat.toVector, allergeenit, kuvaus) // Lopulta metodi palauttaa Aine-olion saatujen tietojen perusteella.
    
  }
  
}