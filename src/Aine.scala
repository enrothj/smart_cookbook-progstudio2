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
  

  
}