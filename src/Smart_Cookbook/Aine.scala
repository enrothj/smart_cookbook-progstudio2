package Smart_Cookbook

import scala.collection.mutable.Buffer


/*
 * Luokka Aine mallintaa kaikkia erilaisia ruoka-aineita. Dokumentaatiossa Aine-olioista puhutaan yleensä näillä käsitteillä:
 * -aine       - mikä tahansa Aine-olio
 * -ainesosa   - Aine-olio, josta jokin toinen aine valmistetaan (yleensä resepti) esim. vehnäjauho voi olla pullan ainesosa
 * -raaka-aine - raaka-aine on aine, jolla ei ole omia ainesosia. Esim. jauho, kaupasta ostettavat tuotteet, jne.
 * -aines      - voi olla ainesosa tai raaka-aine
 * 
 * Konstruktorit:
 * nimi         - aineen nimi
 * allergeenit  - aineen allergeeni
 * kuvaus       - Kuvaus aineesta, esim. valmistusohje
 * tiheys       - aineen tiheys yksikössä g/ml
 * määrä        - kuinka paljon tätä ainetta syntyy valmistettaessa
 * mittayksikkö - määrän mittayksikkö, Aineen oletusmittayksikkö. Mittayksiköistä tarkemmin Muuntaja-yksittäisoliossa.
 * 
 * 
 * 
 * 
 */

class Aine(val nimi: String,
    var allergeenit: Buffer[String] = Buffer(),
    var kuvaus: String = "",
    var tiheys: Double = 0.0, var määrä: Double = 0.0,
    var mittayksikkö: String = "kpl") {

  
  /*
   * Muuttujaan ainesosat tallennetaan tiedot aineen kaikista ainesosista. Ainesosat tallennetaan kolmialkioisina monikoina.
   * Ensimmäisessä alkiossa on ainesta vastaava Aine-olio, toisessa määrä, kolmannessa mittayksikkö tälle määrälle.
   * 
   * HUOM: Ainesosan mittayksikkö voi olla eri kuin sitä vastaavan Aine-olion oletusmittayksikkö. Esim. Vehnäjauho voi olla
   * oletusyksiköltään kg, mutta se voidaan merkitä reseptiin yksikössä dl. Muuntaja suorittaa laskutoimitukset näiden
   * välillä.
   */
  var ainesosat: Array[Tuple3[Aine, Double, String]] = Array()
  
  
  override def toString = this.nimi
  
  def listaaAllergeenit: String = allergeenit.mkString(", ")
  
  
  // Metodi muuttaa ainesosan helposti luettavaan merkkijonomuotoon
  def ainesosaTekstiksi(ainesosa: Tuple3[Aine, Double, String]): String = {
    ainesosa._1.nimi + " " + ainesosa._2.toString + ainesosa._3
  }
  
  def listaaAinesosat: String = ainesosat.map(ainesosaTekstiksi(_)).mkString("\n")
  
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
  def tietoTaulukko: Array[Array[String]] = {
    var sarakkeet: Buffer[Array[String]] = Buffer()
    
    // Lisätään jokainen tietorivi muodossa Array(ominaisuus, arvo)
    sarakkeet += Array("nimi", this.nimi)
    sarakkeet += Array("allergeenit", this.listaaAllergeenit)
    sarakkeet += Array("tiheys (g/ml)", this.tiheys.toString())
    sarakkeet += Array("määrä", this.määrä.toString())
    sarakkeet += Array("mittayksikkö", this.mittayksikkö)
    sarakkeet += Array("ainesosat", this.listaaAinesosat)
    
    sarakkeet.toArray
  }
  
  // Metodi palauttaa tietotaulukon merkkijonomuodossa.
  def tietotaulukkoTekstinä: String = {
    var tietotaulukko = "%-40s".format("Ominaisuus") + "%-100s".format("Arvo") + " \n"
    
    // Jokaiselle riville tulee vasempaan laitaan ominaisuuden nimi ja oikealle vastaava arvo.
    //                           Ominaisuus                        Arvo                                Rivinvaihto
    val nimiRivi         = "%-40s".format("Nimi")          + "%-100s".format(this.nimi)              + " \n"
    val allergeeniRivi   = "%-40s".format("Allergeenit")   + "%-100s".format(this.listaaAllergeenit) + " \n"
    val tiheysRivi       = "%-40s".format("Tiheys (g/ml)") + "%-100s".format(this.tiheys.toString)   + " \n"
    val määräRivi        = "%-40s".format("Määrä")         + "%-100s".format(this.määrä.toString)    + " \n"
    val mittayksikköRivi = "%-40s".format("Mittayksikkö")  + "%-100s".format(this.mittayksikkö)      + " \n"
    val ainesosaRivi     = "%-40s".format("Ainesosat:")    + "\n" + this.listaaAinesosat + " \n"
    
    tietotaulukko += nimiRivi + allergeeniRivi + tiheysRivi + määräRivi + mittayksikköRivi + ainesosaRivi
    
    tietotaulukko
  }
  
  
  // Metodilla voidaan lisätä aineelle ainesosia
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
  
  
  // Metodi muuttaa merkkijonoksi aineetYhteensä metodin tuottamat ainekset. Eli siis listaa kaikki raaka-aineet merkkijonona.
  def listaaRaakaAineet: String = {
    
    var raakaAineet: Buffer[String] = Buffer()
    
    for (ainesosa <- this.aineetYhteensä) {
      
      raakaAineet += this.ainesosaTekstiksi(ainesosa)
      
    }
    
    raakaAineet.mkString(", ")
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
  def muutaTiheys(x: Double) = {
                                 require(x >= 0.0)
                                 this.tiheys = x
                                }
  
  // muutaMäärä muuttaa määrän, joka syntyy ainetta valmistettaessa
  def muutaMäärä(x: Double) = {
                                 require(x >= 0.0)
                                 this.määrä = x
                               }
  
  // muutaKuvaus muuttaa aineen kuvausta
  def muutaKuvaus(x: String) = this.kuvaus = x
  
  /*
   * Näillä metodeilla voidaan muuttaa Aineen allergeenilistaa.
   */
  def lisääAllergeeni(x: String)          = if (!this.allergeenit.contains(x)) this.allergeenit += x
  
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