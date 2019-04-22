package Smart_Cookbook

import scala.collection.mutable.Buffer


/*
 * Luokka Aine mallintaa kaikkia erilaisia ruoka-aineita. Dokumentaatiossa Aine-olioista puhutaan yleensa nailla kasitteilla:
 * -aine       - mika tahansa Aine-olio
 * -ainesosa   - Aine-olio, josta jokin toinen aine valmistetaan (yleensa resepti) esim. vehnajauho voi olla pullan ainesosa
 * -raaka-aine - raaka-aine on aine, jolla ei ole omia ainesosia. Esim. jauho, kaupasta ostettavat tuotteet, jne.
 * -aines      - voi olla ainesosa tai raaka-aine
 * 
 * Konstruktorit:
 * nimi         - aineen nimi
 * allergeenit  - aineen allergeeni
 * kuvaus       - Kuvaus aineesta, esim. valmistusohje
 * tiheys       - aineen tiheys yksikossa g/ml
 * maara        - kuinka paljon tata ainetta syntyy valmistettaessa
 * mittayksikko - maaran mittayksikko, Aineen oletusmittayksikko. Mittayksikoista tarkemmin Muuntaja-yksittaisoliossa.
 * 
 * 
 * 
 * 
 */

class Aine(val nimi: String,
    var allergeenit: Buffer[String] = Buffer(),
    var kuvaus: String = "",
    var tiheys: Double = 0.0, var maara: Double = 0.0,
    var mittayksikko: String = "kpl") {

  
  /*
   * Muuttujaan ainesosat tallennetaan tiedot aineen kaikista ainesosista. Ainesosat tallennetaan kolmialkioisina monikoina.
   * Ensimmaisessa alkiossa on ainesta vastaava Aine-olio, toisessa maara, kolmannessa mittayksikko talle maaralle.
   * 
   * HUOM: Ainesosan mittayksikko voi olla eri kuin sita vastaavan Aine-olion oletusmittayksikko. Esim. Vehnajauho voi olla
   * oletusyksikoltaan kg, mutta se voidaan merkita reseptiin yksikossa dl. Muuntaja suorittaa laskutoimitukset naiden
   * valilla.
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
   *  maara
   *  mittayksikko
   *  ainesosat
   */
  def tietoTaulukko: Array[Array[String]] = {
    var sarakkeet: Buffer[Array[String]] = Buffer()
    
    // Lisataan jokainen tietorivi muodossa Array(ominaisuus, arvo)
    sarakkeet += Array("nimi", this.nimi)
    sarakkeet += Array("allergeenit", this.listaaAllergeenit)
    sarakkeet += Array("tiheys (g/ml)", this.tiheys.toString())
    sarakkeet += Array("maara", this.maara.toString())
    sarakkeet += Array("mittayksikko", this.mittayksikko)
    sarakkeet += Array("ainesosat", this.listaaAinesosat)
    
    sarakkeet.toArray
  }
  
  // Metodi palauttaa tietotaulukon merkkijonomuodossa.
  def tietotaulukkoTekstina: String = {
    var tietotaulukko = "%-40s".format("Ominaisuus") + "%-100s".format("Arvo") + " \n"
    
    // Jokaiselle riville tulee vasempaan laitaan ominaisuuden nimi ja oikealle vastaava arvo.
    //                           Ominaisuus                        Arvo                                Rivinvaihto
    val nimiRivi         = "%-40s".format("Nimi")          + "%-100s".format(this.nimi)              + " \n"
    val allergeeniRivi   = "%-40s".format("Allergeenit")   + "%-100s".format(this.listaaAllergeenit) + " \n"
    val tiheysRivi       = "%-40s".format("Tiheys (g/ml)") + "%-100s".format(this.tiheys.toString)   + " \n"
    val maaraRivi        = "%-40s".format("Maara")         + "%-100s".format(this.maara.toString)    + " \n"
    val mittayksikkoRivi = "%-40s".format("Mittayksikko")  + "%-100s".format(this.mittayksikko)      + " \n"
    val ainesosaRivi     = "%-40s".format("Ainesosat:")    + "\n" + this.listaaAinesosat + " \n"
    
    tietotaulukko += nimiRivi + allergeeniRivi + tiheysRivi + maaraRivi + mittayksikkoRivi + ainesosaRivi
    
    tietotaulukko
  }
  
  
  // Metodilla voidaan lisata aineelle ainesosia
  def lisaaAinesosa(aine: Aine, maara: Double, mittayksikko: String) = {
    var uudetAinesosat = ainesosat.toBuffer
    
    uudetAinesosat += Tuple3(aine, maara, mittayksikko)
    ainesosat = uudetAinesosat.toArray
  }
  
  // metodilla poistetaan annetun niminen ainesosa listasta
  def poistaAinesosa(nimi: String) = {
    val uudetAinesosat = ainesosat.filterNot(_._1.nimi == nimi) // valitaan alkiot, joiden aine ei ole parametrina maaritellyn niminen
    ainesosat = uudetAinesosat
  }
  
  // metodilla voidaan muuttaa annetun nimisen ainesosan maaraa ja mittayksikkoa ainesosalistassa.
  def muutaAinesosaa(nimi: String, maara: Double, mittayksikko: String) = {
    require(Muuntaja.tunnistettu(mittayksikko))
    val indeksi = ainesosat.indexWhere(_._1.nimi == nimi)  // Etsitaan alkion, jossa halutun niminen aine on, indeksi.
    val aine = ainesosat(indeksi)._1                       // Tallennetaan muuttujaan, kyseista ainetta vastaava olio
    
    ainesosat(indeksi) = Tuple3(aine, maara, mittayksikko) // Muutetaan loydetyssa indeksissa olevan monikon tiedot parametreja vastaaviksi.
  }
  
  
  
  
  // Metodi tarkistaa sisaltaako aine tietyn nimista ainetta raaka-aineenaan.
  def sisaltaaAineen(nimi: String): Boolean = {
    var sisaltaa: Boolean = false
    
    for (aines <- this.ainesosat) {
      
      if (aines._1.nimi == nimi) sisaltaa = true
      else if ( aines._1.sisaltaaAineen(nimi) ) sisaltaa = true
      
    }
    
    sisaltaa
  }
  
  
  // Metodi palauttaa true jos aine on raaka-aine. Ohjelmassa raaka-aine on maaritelty aineeksi, jolla ei ole omia ainesosia.
  def onRaakaAine: Boolean = this.ainesosat.isEmpty
  
  
  /*
   * Metodi aineetYhteensa laskee mita ja paljonko raaka-aineita vaaditaan, jos aineen raaka-aineet pitaa valmistaa erikseen. Metodi palauttaa kokoelman monikoita,
   * jotka sisaltavat raaka-aineet, niiden maaran ja yksikon. Jos metodia kutsutaan raaka-aineelle, eli aineelle, jolla ei ole ainesosia, metodi palauttaa tyhjan 
   * taulukon.
   * 
   * Esimerkki:       Spaghetti bolognese
   *                     /              \
   *           spagetti 300g     kastike 800g
   *                              /          \
   *                     jauheliha 400g     tomaattikastike 3 dl
   * 
   * Tassa esimerkissa "Spagetti bolognese"-olio.aineetYhteensa palauttaisi Array( (spagetti,300,"g"), (jauheliha,400,"g"), (tomaattikastike,3,"dl") ).
   * 
   * spagetti.aineetYhteensa puolestaan palauttaisi Array(), koska silla ei ole ainesosia (taman esimerkin puitteissa).
   */
  
  def aineetYhteensa: Array[Tuple3[Aine, Double, String]] = {
    
    var aineet: Buffer[Tuple3[Aine, Double, String]] = Buffer[Tuple3[Aine, Double, String]]()  // aineet-muuttujaan kootaan kaikki raaka-aineet
    
    
    if (!onRaakaAine) {                                                 // Jos kyseessa ei ole raaka-aine...

      
      for (aine <- this.ainesosat) {                                    // ... kaydaan lapi ainesosat.
        var ainekset: Buffer[Tuple3[Aine, Double, String]] = Buffer()   // ainekset- muuttujaan kerataan taman kyseisen aineen mahdolliset raaka-aineet.
        
        if (aine._1.onRaakaAine) {                                      // Jos ainesosa on raaka-aine,
          ainekset += aine                                              // lisataan aineet-muuttujaan reseptissa mainittu ainesosa-alkio (aine, maara, yksikko).
        } 
        
        else {
          ainekset.union(aine._1.aineetYhteensa)                        // Jos aineella on omat ainesosansa, kutsutaan rekursiivisesti tata metodia ainesosalle.
                                                                        // Lisataan raaka-aineet ainekset-muuttujaan.
        }
        
        aineet.union(ainekset)                                          // Lisataan nama raaka-aineet aineet-muuttujaan.               
      }
      
    }
    
    
    aineet.toArray
    
  }
  
  
  // Metodi muuttaa merkkijonoksi aineetYhteensa metodin tuottamat ainekset. Eli siis listaa kaikki raaka-aineet merkkijonona.
  def listaaRaakaAineet: String = {
    
    var raakaAineet: Buffer[String] = Buffer()
    
    for (ainesosa <- this.aineetYhteensa) {
      
      raakaAineet += this.ainesosaTekstiksi(ainesosa)
      
    }
    
    raakaAineet.mkString(", ")
  }
  
  /*
   *  Metodilla muunnaAinesosa voidaan muuttaa ainesosa-monikoita haluttuun mittayksikkoon samalla muuttaen maara vastaavaksi. Esimerkiksi vehnajauho 1 dl -> vehnajauho 70 g.
   *  Metodi kutsuu Muuntaja-objektin laske-metodeita muunnokseen.
   */
  private def muunnaAinesosa(ainesosa: Tuple3[Aine, Double, String], kohdeyksikko: String): Tuple3[Aine, Double, String] = {
    val aine = ainesosa._1 // tallennetaan ainesosaa vastaava Aine-olio muuttujaan helpompaa koodin yksinkertaistamiseksi.
    
    val uusiAines = Tuple3(ainesosa._1,                                                                   // Aine-olio pysyy samana.
                          Muuntaja.laske(aine.tiheys, ainesosa._3, ainesosa._2, kohdeyksikko),    // Muuntajan laske-metodiin syotetaan tiheys, aloitusyksikko, maara ja kohdemittayksikko.
                          kohdeyksikko)                                                           // Uudeksi mittayksikoksi annetaan parametrina annettu kohdeyksikko.
                          
    uusiAines
  }
  
  // muutaYksikko muuttaa aineen oletusmittayksikon halutuksi.
  def muutaYksikko(x: String) = {
                                 require(Muuntaja.tunnistettu(x))
                                 this.mittayksikko = x
                                 }
  
  // muutaTiheys muuttaa aineen tiheyden halutuksi
  def muutaTiheys(x: Double) = {
                                 require(x >= 0.0)
                                 this.tiheys = x
                                }
  
  // muutaMaara muuttaa maaran, joka syntyy ainetta valmistettaessa
  def muutaMaara(x: Double) = {
                                 require(x >= 0.0)
                                 this.maara = x
                               }
  
  // muutaKuvaus muuttaa aineen kuvausta
  def muutaKuvaus(x: String) = this.kuvaus = x
  
  /*
   * Nailla metodeilla voidaan muuttaa Aineen allergeenilistaa.
   */
  def lisaaAllergeeni(x: String)          = if (!this.allergeenit.contains(x)) this.allergeenit += x
  
  def poistaAllergeeni(x: String)         = this.allergeenit -= x
  
  def uudetAllergeenit(x: Buffer[String]) = this.allergeenit  = x
  

  
}


// Kumppaniolio, jolla helpotetaan Aine-olioiden luomista, ja jolla voidaan luoda uusia Aine-olioita lukemalla niita tekstitiedostoista.
object Aine {
  
  /*
   * Tehdasmetodi, jolla helpotetaan uusien Aine-olioiden luomista.
   */
  
  def apply(nimi: String, allergeenit: Buffer[String], kuvaus: String,
      tiheys: Double, maara: Double, mittayksikko: String) = {
    new Aine(nimi: String, allergeenit: Buffer[String], kuvaus: String,
      tiheys: Double, maara: Double, mittayksikko: String)
  }
  

  
}