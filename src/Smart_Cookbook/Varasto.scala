package Smart_Cookbook

import scala.collection.mutable.Map
import scala.collection.mutable.Buffer

/**
 * Varasto-olio pitaa kirjaa ohjelman tuntemista aineista ja niiden maarista, eli mallintaa kaytannossa kayttajan jaakaappia. 
 * Merkittavin osa on varasto-muuttuja. Se on hakemisto,johon on tallennettu kaikki Aine-oliot ja niita vastaavat maarat
 * (aineen oletusmittayksikossa). Varaston tiedot taytetaan aina, kun ohjelma kaynnistetaan, ja tiedot tallennetaan tekstitiedostolle 
 * suljettaessa. Oliolla on paljon metodeja naiden tietojen hallitsemista varten.
 * 
 */



object Varasto {

  var varasto: Map[Aine, Double] = Map() // Muuttuja tallentaa kaikki jaakaapin sisaltamat aineet ja niiden maaran.
  
  
  /*
   * Metodi onOlemassa tarkistaa onko ohjelmaan tallennettu parametrina annetun niminen aine.
   */
  def onOlemassa(nimi: String): Boolean = varasto.exists(_._1.nimi == nimi)
  
  def aineNimelta(nimi: String): Aine = if (onOlemassa(nimi)) varasto.keys.find(_.nimi == nimi).get else throw new OlematonAinePoikkeus("Aine " + nimi + " ei ole ohjelman tiedossa.", nimi)
  
  // Nailla metodeilla voidaan lisata uusi aine varastoon tai poistaa olemassa oleva.
  def uusiAine(aine: Aine, maara: Double = 0.0) = {
    if (!varasto.keys.exists(_.nimi == aine.nimi)) varasto(aine) = maara // Jos annetun nimista ainetta ei ole olemassa, luodaan uusi
    else {
      poistaAine(aine.nimi) // Jos annetun niminen aine on olemassa, poistetaan vanha versio, ja lisataan uusi tilalle.
      varasto(aine) = 0.0
    }
  }
  
  def poistaAine(nimi: String): Boolean = {
    if (!onOlemassa(nimi)) throw new OlematonAinePoikkeus("Kyseista ainetta ei ole ohjelman muistissa.", nimi)
    else varasto -= Hakukone.haeNimi(nimi).get
    onOlemassa(nimi)
  }
  
  
  // Nailla metodeilla voidaan kasvattaa tai vahentaa aineen maaraa varastossa. Aineen maaran varastossa on aina oltava >= 0.0.
  def lisaaAinetta(aine: Aine, maara: Double) = varasto(aine) = varasto.get(aine).getOrElse(0.0) + maara
  
  def vahennaAinetta(aine: Aine, maara: Double) = varasto(aine) = if (varasto(aine) > maara) varasto(aine) - maara else 0.0 // Jos vahennettava maara on suurempi kuin varastoitu, varastoon jaa 0.0
  
  
  // Talla metodilla voidaan asettaa tietty arvo tietyn aineen maaralle (>= 0.0)
  def asetaMaara(aine: Aine, maara: Double) = {
    require(maara >= 0.0)
    varasto(aine) = maara
  }
  
  // Tama metodi asettaa kaikkien varaston aineiden maaraksi 0.0
  def nollaa() = {
    for (aine <- varasto) {
      varasto(aine._1) = 0.0
    }
  }
  
  // Tama metodi poistaa kaikki varaston tiedot
  def tyhjenna() = varasto = varasto.empty
  
  
  // Metodi listaaAineet palauttaa kaksiulotteisen taulukon, jossa on jokaisen aineen nimi, maara ja allergeenit. Kaytetaan kayttoliittyman paaikkunan listaa varten
  def listaaAineet: Array[Array[(String, Double, String)]] = {
    
    var rivit: Buffer[Array[(String, Double, String)]] = Buffer()
    
    for (aine <- varasto) {
      val aineenNimi        = aine._1.nimi
      val aineenMaara       = aine._2
      val aineenAllergeenit = aine._1.allergeenit.mkString(", ")
      
      val taulukko = Array((aineenNimi, aineenMaara, aineenAllergeenit))
      
      rivit += taulukko
    }
    
    rivit.toArray
  }
  
  /*
   * Metodilla muutaYksikko voidaan muuttaa Aine-olion mittayksikko, siten, etta samalla lasketaan sen maara varastossa uudessa mittayksikossa.
   * Aineen aiempi yksikko ei saa olla "kpl" eika myoskaan kohdeyksikko, koska kappaleista ei voi tehda muunnoksia.
   */
  def muutaYksikko(aine: Aine, yksikko: String) = {
    require(Muuntaja.tunnistettu(yksikko) || aine.mittayksikko != "kpl")
    
    aine.maara        = Muuntaja.muunna(aine, aine.maara, yksikko)    // Asetetaan aineen uusi maara uuden yksikon mukaiseksi
    
    asetaMaara( aine, Muuntaja.muunna(aine, varasto(aine), yksikko) ) // Muutetaan aineen maara varastossa uuden yksikon mukaiseksi
    
    aine.mittayksikko = yksikko                                       // Muutetaan aineen perusmittayksikoksi annettu yksikko.
  }
  
  /* Metodilla voidaan lisata aineita reseptikansiosta. Parametrina annetaan kansio, josta reseptit halutaan hakea.
   *  Metodi saa Aine-kokoelman IO.lueReseptit metodin kautta.
   * Metodia käytetään esim. jos jaakaappi.txt on tyhjennetty, mutta Varasto halutaan tayttaa uudelleen resepteista,
   *  tai jos on saatu uusi resepti tekstitiedostona
   */
  def lisaaReseptit(sijainti: String) = {
    val kokoelma = IO.lueReseptit(sijainti)
    
    for (aine <- kokoelma) {
      try {
        if (!onOlemassa(aine.nimi)) uusiAine(aine) // Uusi aine lisätään vain, jos se ei ole jo varastossa.
      } catch {
        case e: NullPointerException =>
      }
    }
  }
  
}