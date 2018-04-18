package Smart_Cookbook

import scala.io.Source
import scala.collection.mutable.Buffer
import java.io.File
import java.io.PrintWriter

/*
 * 
 */

class Aine(val nimi: String, var ainesosat: Array[Tuple3[Aine, Double, String]],
    var allergeenit: Buffer[String],
    var kuvaus: String,
    var tiheys: Double, var määrä: Double,
    var mittayksikkö: String) {

  
  
  
  /*
   * Metodi aineetYhteensä laskee mitä ja paljonko raaka-aineita vaaditaan, jos aineen raaka-aineet pitää valmistaa erikseen. Metodi palauttaa kokoelman monikoita,
   * jotka sisältävät raaka-aineet, niiden määrän ja yksikön. Jos metodia kutsutaan raaka-aineelle, eli aineelle, jolla ei ole ainesosia, metodi palauttaa tyhjän 
   * taulukon.
   * 
   * Esimerkki:       Spaghetti bolognese
   *                     /              \
   *           spagetti 300g     kastike 800g
   *                              /          \
   *                     jauheliha 400g     tomaattikastike 3 dl
   * 
   * Tässä esimerkissä "Spagetti bolognese"-olio.aineetYhteensä palauttaisi Array( (spagetti,300,"g"), (jauheliha,400,"g"), (tomaattikastike,3,"dl") ).
   * 
   * spagetti.aineetYhteensä puolestaan palauttaisi Array(), koska sillä ei ole ainesosia (tämän esimerkin puitteissa).
   */
  
  def aineetYhteensä: Array[Tuple3[Aine, Double, String]] = {
    
    var aineet: Buffer[Tuple3[Aine, Double, String]] = Buffer[Tuple3[Aine, Double, String]]()  // aineet-muuttujaan kootaan kaikki raaka-aineet
    
    
    if (!this.ainesosat.isEmpty) {                             // Jos ainesosat-muuttuja ei ole tyhjä, aineella on raaka-aineita

      
      for (aine <- this.ainesosat) {                           // Käydään läpi ainesosat.
        
        if (aine._1.ainesosat.isEmpty) {                       // Jos ainesosalla ei ole raaka-aineita,
          aineet += aine                                       // lisätään aineet-muuttujaan reseptissä mainittu ainesosa-alkio (aine, määrä, yksikkö).
        } 
        
        else {
          aine._1.aineetYhteensä                               // Jos aineella on omat ainesosansa, kutsutaan rekursiivisesti tätä metodia ainesosalle.
          
        }
        
      }
      
    }
    
    
    aineet.toArray
    
  }
  
  /*
   *  Metodilla muunnaAinesosa voidaan muuttaa ainesosa-monikoita haluttuun mittayksikköön samalla muuttaen määrä vastaavaksi. Esimerkiksi vehnäjauho 1 dl -> vehnäjauho 70 g.
   *  Metodi kutsuu Muuntaja-objektin laske-metodeita muunnokseen.
   */
  private def muunnaAinesosa(ainesosa: Tuple3[Aine, Double, String], kohdeyksikkö: String): Tuple3[Aine, Double, String] = {
    val aine = ainesosa._1 // tallennetaan ainesosaa vastaava Aine-olio muuttujaan helpompaa koodin yksinkertaistamiseksi.
    
    val uusiAines = Tuple3(ainesosa._1,                                                                   // Aine-olio pysyy samana.
                          Muuntaja.laske(aine.tiheys, ainesosa._3, ainesosa._2, kohdeyksikkö),    // Muuntajan laske-metodiin syötetään tiheys, aloitusyksikkö, määrä ja kohdemittayksikkö.
                          kohdeyksikkö)                                                           // Uudeksi mittayksiköksi annetaan parametrina annettu kohdeyksikkö.
                          
    uusiAines
  }
  
  // muutaYksikkö muuttaa aineen oletusmittayksikön halutuksi. TODO: x pitää olla tunnistettu mittayksikkö.
  def muutaYksikkö(x: String) = this.mittayksikkö = x
  
  // muutaTiheys muuttaa aineen tiheyden halutuksi
  def muutaTiheys(x: Double) = this.tiheys = x
  
  // muutaMäärä muuttaa määrän, joka syntyy ainetta valmistettaessa
  def muutaMäärä(x: Double) = this.määrä = x
  
  // muutaKuvaus muuttaa aineen kuvausta
  def muutaKuvaus(x: String) = this.kuvaus = x
  
  /*
   * Näillä metodeilla voidaan muuttaa Aineen allergeenilistaa.
   */
  def lisääAllergeeni(x: String) = ???
  
  def poistaAllergeeni(x: String) = ???
  
  def uudetAllergeenit(x: Array[String]) = ???
  
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
  
  /*
   * Tehdasmetodi, jolla helpotetaan uusien Aine-olioiden luomista.
   */
  
  def apply(nimi: String, ainesosat: Array[Tuple3[Aine, Double, String]], allergeenit: Buffer[String], kuvaus: String,
      tiheys: Double, määrä: Double, mittayksikkö: String) = {
    new Aine(nimi: String, ainesosat: Array[Tuple3[Aine, Double, String]], allergeenit: Buffer[String], kuvaus: String,
      tiheys: Double, määrä: Double, mittayksikkö: String)
  }
  
  // TODO: SIIRRÄ "IO"-OBJEKTIIN ja lisää uudet muutokset
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