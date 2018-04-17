package Smart_Cookbook

/*
 * Muuntaja käsittelee ohjelman mittayksikkömuunnokset. Ohjelma tunnistaa ja käsittelee mittayksiköitä String-muodossa. 
 * 
 * Ohjelman käyttämät massan mittayksiköt 
 * ovat:
 *  -gramma 			("g")			
 *  -kilogramma 	("kg")		= 1000.0 g
 *  -naula 				("lb")		= 453.6 g
 *  -unssi 				("oz")		= 28.4 g
 *  
 *  Ohjelman käyttämät tilavuuden mittayksiköt ovat:
 *  -millilitra 	("ml")
 *  -desilitra 		("dl")		= 100.0 ml
 *  -litra				("l")			= 10.0 dl
 *  -teelusikka 	("tl")		= 5.0 ml
 *  -ruokalusikka ("rkl")		= 15.0 ml
 *  -kuppi 				("cup")		= 2.4 dl
 *  -pintti				("pint")	= 4.7 dl
 *  
 */



object Muuntaja {
  
  
  // Muuttujat massat ja tilavuudet sisältävät ohjelman tuntemat mittayksiköt.
  private val massat: Array[String] = Array("g", "kg", "lb", "oz")
  private val tilavuudet: Array[String] = Array("ml", "dl", "l", "tl", "rkl", "cup", "pint")
  
  /*
   * Metodit tunnistaX ottavat parametreinaan mittayksikön (String) ja muuttavat sen Muuntajan käyttämään perusmittayksikköön (ml tai g).
   */
  private def tunnistaMassa(s: String): Double = {
    val massaGrammoina = s match {
      case "g"    => 1.0
      case "kg"   => 1000.0
      case "lb"   => 453.6
      case "oz"   => 28.4
      case _ => throw new Exception() //TODO: Metodin tulisi käsitellä poikkeukset, jolloin sille annetaan vääränlainen parametri (muu mittayksikkö tai kirjoitusvirhe tjsp).
    }
    massaGrammoina
  }
  
  private def tunnistaTilavuus(s: String): Double = {
    val massaMillilitroina = s match {
      case "ml"   => 1.0
      case "dl"   => 100.0
      case "l"    => 1000.0
      case "tl"   => 5.0
      case "rkl"  => 15.0
      case "cup"  => 240.0
      case "pint" => 470.0
      case _ => throw new Exception() // TODO
    }
    massaMillilitroina
  }
  
  /*
   *  Metodi onkoMassa palauttaa arvon true, jos annettu parametri s on ohjelman tunnistama massan mittayksikkö. Jos se on tilaavuden yksikkö, metodi palauttaa false.
   *  Jos annettu ei ole listattu muuttujissa massat tai tilavuudet, metodi heittää poikkeuksen.
   */
  private def onkoMassa(s: String): Boolean = if (massat.contains(s)) true else if (tilavuudet.contains(s)) false else throw new Exception() // TODO: poikkeuksen käsittely
  
  /*
   * suhde-metodit ottavat parametreinaan kaksi mittayksikköä, ja palauttavat niiden suhteen. Käytännössä parametri a on aloitusmittayksikkö, josta pyritään kohdeyksikköön b.
   */
  
  // suhdeMassa laskee kahden massayksikön suhteen (esim kg / g = 1000)
  
  def suhdeMassa(a: String, b: String): Double = tunnistaMassa(a) / tunnistaMassa(b)
  
  // suhdeTilavuus laskee kahden tilavuuden yksikön suhteen (esim. l / dl = 10)
  
  def suhdeTilavuus(a: String, b: String): Double = tunnistaTilavuus(a) / tunnistaTilavuus(b)
  
  // metodi suhde tunnistaa onko kyseessä massa- vai tilavuusmuunnos, ja käyttää sopivaa metodia palauttaakseen mittayksiköiden suhteen.
  def suhde(a: String, b: String): Double = {
    if (onkoMassa(a) && onkoMassa(b)) suhdeMassa(a, b)                  // Jos parametrit a ja b löytyvät massayksiköiden listasta, käytetään suhdeMassat-metodia.
    else if (!onkoMassa(a) && !onkoMassa(b)) suhdeTilavuus(a, b)  // Samoin tilavuuksien puolesta. Jos a ja b eivät täytä näitä ehtoja, heitetään poikkeus.
    else throw new Exception() //TODO: väärien parametrien käsittely
  }
  
  /*
   * Metodit massaMassa, tilavuusTilavuus, massaTilavuus ja tilavuusMassa laskevat yksikkömuunnokset omissa kategorioissaan. Metodit toimivat kaavan 
   * tiheys = massa / tilavuus (d = m / V) perusteella. Metodit massaMassa ja tilavuusTilavuus eivät tarvitse tiheyttä; ainoastaan mittayksiköiden
   * suhteen suhde-metodeilta. Tiheys on ohjelmassa aina mittayksikössä g/ml.
   */
  
  def massaMassa(a: String, b: String, m: Double): Double = m * suhdeMassa(a, b)
  
  def tilavuusTilavuus(a: String, b: String, v: Double): Double = v * suhdeTilavuus(a, b)
  
  /*
   *  massaTilavuus ottaa parametreinaan aineen tiheyden d, massan mittayksikön, halutun tilavuusyksikön ja aineen massan m. Palautusarvo on tilavuus anettussa tilavuusyksikössä.
   *  Metodilla voidaan tehdä muunnos massayksiköistä tilavuuden yksiköihin, aineen massan ja tiheyden avulla. Esimerkiksi jos annetaan m=1kg vettä (tiheys 1000g/1000ml => d = 1.00) ja 
   *  kohdeyksiköksi "dl": metodi laskee vastaukseksi 10.0 (dl).
   */
  
  def massaTilavuus(d: Double, massayksikkö: String, tilavuusyksikkö: String, m: Double): Double = {
    
    val perustilavuus = d / (m * suhdeMassa(massayksikkö, "g")) // Muuttujaan perustilavuus lasketaan aineen tilavuus ohjelman perusyksikössä "ml",
    val kohdetilavuus = tilavuusTilavuus("ml", tilavuusyksikkö) // josta se muutetaan kohdemittayksikköön.
    
    kohdetilavuus
  }
  
  // tilavuusMassa toimii samalla periaatteella kuin massaTilavuus, mutta lasketaan kaavalla m = d * V.
  def tilavuusMassa(d: Double, tilavuusyksikkö: String, massayksikkö: String, v: Double): Double = {
    
    val perusmassa = d * v * suhdeTilavuus(tilavuusyksikkö, "ml") // Muuttujaan perusmassa lasketaan aineen massa ohjelman perusyksikössä "g",
    val kohdemassa = massaMassa("g", massayksikkö)                // josta se muutetaan kohdemittayksikköön.
    
    kohdemassa
  }
  
  
  /*
   * Metodi laske ottaa parametreinaan aineen tiheyden d (yksikössä g/ml), aloitusmittayksikön, aineen määrän tässä mittayksikössä sekä kohdemittayksikön. Metodi palauttaa aineen määrän kohdemittayksikössä.
   * Esimerkiksi jos halutaan selvittää kuinka monta desilitraa yksi kilo vettä on, syötetään d = 1.00 (1kg/l = 1g/ml), yksikkö1 = "kg", määrä = 1.0, yksikkö2 = "dl". Metodi selvittää
   * ensin if-lauseilla metodia onkoMassa hyödyntäen, että kyseessä on muunnos massasta tilavuuteen, ja syöttää tarvittavat arvot massaTilavuus-metodiin, jolta se saa tulokseksi 10.0.
   */
  def laske(d: Double, yksikkö1: String, määrä: Double, yksikkö2: String): Double = {
    
    /*
     *  Muuttujaan uusiMäärä lasketaan tarvittava muunnos. Ensin selvitetään onko kyseessä 1. massa->massa 2. tilavuus->tilavuus 3. massa->tilavuus 4. tilavuus->massa -muunnos.
     *  Uuden arvon laskemiseen käytetään xX-metodeita (esim. massaMassa).
     */
    val uusiMäärä = {
      if (onkoMassa(yksikkö1) && onkoMassa(yksikkö2)) massaMassa(yksikkö1, yksikkö2, määrä)                  // Jos sekä aloitusmittayksikkö että kohdemittayksikkö ovat massan yksiköitä, käytetään
      else if (!onkoMassa(yksikkö1) && !onkoMassa(yksikkö2)) tilavuusTilavuus(yksikkö1, yksikkö2, määrä)     // metodia massaMassa. Muut osat toimivat samalla periaatteella.
      else if (onkoMassa(yksikkö1) && !onkoMassa(yksikkö2)) massaTilavuus(d, yksikkö1, yksikkö2, määrä)
      else tilavuusMassa(d, yksikkö1, yksikkö2, määrä)                                                       // Jos edellisiä kolmea ehtoa ei ole täytetty, kyseessä on tilavuus->massa -muunnos.
    }
    
    uusiMäärä
  }
  
  // muunna käyttää laske-metodia, mutta saa tiheyden ja aloitusmittayksikön annetulta Aine-oliolta.
  def muunna(aine: Aine, määrä: Double, yksikkö: String): Double = ???
  
}