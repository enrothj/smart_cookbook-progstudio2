package Smart_Cookbook

import scala.collection.mutable.Buffer


/*
 * 
 */

class Aine(val nimi: String,
    var allergeenit: Buffer[String],
    var kuvaus: String,
    var tiheys: Double, var määrä: Double,
    var mittayksikkö: String) {

  var ainesosat: Array[Tuple3[Aine, Double, String]] = Array()
  
  // metodi muuttaa ainesosan helposti luettavaan muotoon
  def ainesosaTekstiksi(ainesosa: Tuple3[Aine, Double, String]): String = {
    ainesosa._1.nimi + " " + ainesosa._2.toString + ainesosa._3
  }
  
  def ainekset: Array[String] = ainesosat.map(ainesosaTekstiksi)
  
  /*
   *  metodi luo GUI:lle aineen tietotaulukon.
   *  
   *  taulukko on muotoa
   *  
   *  Ominaisuus - arvo
   *  nimi
   *  allergeenit
   *  tiheys
   *  määrä
   *  mittayksikkö
   *  ainesosat
   */
  def tietoTaulukko: Array[Array[Any]] = {
    var sarakkeet: Buffer[Array[Any]] = Buffer()
    
    // Lisätään jokainen tietorivi muodossa Array(ominaisuus, arvo)
    sarakkeet += Array("nimi", this.nimi)
    sarakkeet += Array("allergeenit", this.allergeenit)
    sarakkeet += Array("tiheys (g/ml)", this.tiheys)
    sarakkeet += Array("määrä", this.määrä)
    sarakkeet += Array("mittayksikkö", this.mittayksikkö)
    sarakkeet += Array("ainesosat", this.ainekset)
    
    sarakkeet.toArray
  }
  
  
  // metodilla voidaan lisätä aineelle ainesosia
  def lisääAinesosa(aine: Aine, määrä: Double, mittayksikkö: String) = {
    var uudetAinesosat = ainesosat.toBuffer
    
    uudetAinesosat += Tuple3(aine, määrä, mittayksikkö)
    ainesosat = uudetAinesosat.toArray
  }
  
  // metodilla poistetaan annetun niminen ainesosa listasta
  def poistaAinesosa(nimi: String) = {
    val uudetAinesosat = ainesosat.filterNot(_._1.nimi == nimi) // valitaan alkiot, joiden aine ei ole parametrina määritellyn niminen
    ainesosat = uudetAinesosat
  }
  
  // metodilla voidaan muuttaa annetun nimisen ainesosan määrää ja mittayksikköä ainesosalistassa.
  def muutaAinesosaa(nimi: String, määrä: Double, mittayksikkö: String) = {
    require(Muuntaja.tunnistettu(mittayksikkö))
    val indeksi = ainesosat.indexWhere(_._1.nimi == nimi)  // Etsitään alkion, jossa halutun niminen aine on, indeksi.
    val aine = ainesosat(indeksi)._1                       // Tallennetaan muuttujaan, kyseistä ainetta vastaava olio
    
    ainesosat(indeksi) = Tuple3(aine, määrä, mittayksikkö) // Muutetaan löydetyssä indeksissä olevan monikon tiedot parametreja vastaaviksi.
  }
  
  
  
  
  // Metodi tarkistaa sisältääkö aine tietyn nimistä ainetta raaka-aineenaan.
  def sisältääAineen(nimi: String): Boolean = {
    var sisältää: Boolean = false
    
    for (aines <- this.ainesosat) {
      
      if (aines._1.nimi == nimi) sisältää = true
      else if ( aines._1.sisältääAineen(nimi) ) sisältää = true
      
    }
    
    sisältää
  }
  
  
  // Metodi palauttaa true jos aine on raaka-aine. Ohjelmassa raaka-aine on määritelty aineeksi, jolla ei ole omia ainesosia.
  def onRaakaAine: Boolean = this.ainesosat.isEmpty
  
  
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
    
    
    if (!onRaakaAine) {                                                 // Jos kyseessä ei ole raaka-aine...

      
      for (aine <- this.ainesosat) {                                    // ... käydään läpi ainesosat.
        var ainekset: Buffer[Tuple3[Aine, Double, String]] = Buffer()   // ainekset- muuttujaan kerätään tämän kyseisen aineen mahdolliset raaka-aineet.
        
        if (aine._1.onRaakaAine) {                                      // Jos ainesosa on raaka-aine,
          ainekset += aine                                              // lisätään aineet-muuttujaan reseptissä mainittu ainesosa-alkio (aine, määrä, yksikkö).
        } 
        
        else {
          ainekset.union(aine._1.aineetYhteensä)                        // Jos aineella on omat ainesosansa, kutsutaan rekursiivisesti tätä metodia ainesosalle.
                                                                        // Lisätään raaka-aineet ainekset-muuttujaan.
        }
        
        aineet.union(ainekset)                                          // Lisätään nämä raaka-aineet aineet-muuttujaan.               
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
  
  // muutaYksikkö muuttaa aineen oletusmittayksikön halutuksi.
  def muutaYksikkö(x: String) = {
                                 require(Muuntaja.tunnistettu(x))
                                 this.mittayksikkö = x
                                 }
  
  // muutaTiheys muuttaa aineen tiheyden halutuksi
  def muutaTiheys(x: Double) = this.tiheys = x
  
  // muutaMäärä muuttaa määrän, joka syntyy ainetta valmistettaessa
  def muutaMäärä(x: Double) = this.määrä = x
  
  // muutaKuvaus muuttaa aineen kuvausta
  def muutaKuvaus(x: String) = this.kuvaus = x
  
  /*
   * Näillä metodeilla voidaan muuttaa Aineen allergeenilistaa.
   */
  def lisääAllergeeni(x: String)          = this.allergeenit += x
  
  def poistaAllergeeni(x: String)         = this.allergeenit -= x
  
  def uudetAllergeenit(x: Buffer[String]) = this.allergeenit  = x
  

  
}


// Kumppaniolio, jolla helpotetaan Aine-olioiden luomista, ja jolla voidaan luoda uusia Aine-olioita lukemalla niitä tekstitiedostoista.
object Aine {
  
  /*
   * Tehdasmetodi, jolla helpotetaan uusien Aine-olioiden luomista.
   */
  
  def apply(nimi: String, allergeenit: Buffer[String], kuvaus: String,
      tiheys: Double, määrä: Double, mittayksikkö: String) = {
    new Aine(nimi: String, allergeenit: Buffer[String], kuvaus: String,
      tiheys: Double, määrä: Double, mittayksikkö: String)
  }
  

  
}