package Smart_Cookbook



/*
 * Muuntaja kasittelee ohjelman mittayksikkomuunnokset. Ohjelma tunnistaa ja kasittelee mittayksikoita String-muodossa. 
 * 
 * Ohjelman kayttamat massan mittayksikot 
 * ovat:
 *  -gramma 			("g")			
 *  -kilogramma 	("kg")		= 1000.0 g
 *  -naula 				("lb")		= 453.6 g
 *  -unssi 				("oz")		= 28.4 g
 *  
 *  Ohjelman kayttamat tilavuuden mittayksikot ovat:
 *  -millilitra 	("ml")
 *  -desilitra 		("dl")		= 100.0 ml
 *  -litra				("l")			= 10.0 dl
 *  -teelusikka 	("tl")		= 5.0 ml
 *  -ruokalusikka ("rkl")		= 15.0 ml
 *  -kuppi 				("cup")		= 2.4 dl
 *  -pintti				("pint")	= 4.7 dl
 *  
 *  Ohjelma kayttaa myos 'yksikkoa' "kpl" kuvaamaan annoksia tai kappaleita. Naita ei kuitenkaan ole tarkoitus muuntaa muihin mittayksikoihin,
 *  joten jos naita syottaa Muuntajalle, ohjelma heittaa KappaleMuunnos-poikkeuksen.
 */



object Muuntaja {
  
  
  // Muuttujat massat ja tilavuudet sisaltavat ohjelman tuntemat mittayksikot.
  val massat: Array[String] = Array("g", "kg", "lb", "oz")
  val tilavuudet: Array[String] = Array("ml", "dl", "l", "tl", "rkl", "cup", "pint")
  
  /*
   * Metodit tunnistaX ottavat parametreinaan mittayksikon (String) ja muuttavat sen Muuntajan kayttamaan perusmittayksikkoon (ml tai g).
   */
  private def tunnistaMassa(s: String): Double = {
    val massaGrammoina = s match {
      case "g"    => 1.0
      case "kg"   => 1000.0
      case "lb"   => 453.6
      case "oz"   => 28.4
      case "kpl"  => throw new KappaleMuunnos("Yritettiin muuntaa kappaletta massaksi", s)
      case _      => throw new VirheellinenMittayksikko("Annettiin tuntematon mittayksikko.", s)
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
      case "kpl"  => throw new KappaleMuunnos("Yritettiin muuntaa kappaletta tilavuudeksi", s)
      case _      => throw new VirheellinenMittayksikko("Annettiin tuntematon mittayksikko.", s)
    }
    massaMillilitroina
  }
  
  /*
   *  Metodi onkoMassa palauttaa arvon true, jos annettu parametri s on ohjelman tunnistama massan mittayksikko. Jos se on tilaavuden yksikko, metodi palauttaa false.
   *  Jos annettu ei ole listattu muuttujissa massat tai tilavuudet, metodi heittaa poikkeuksen.
   */
  private def onkoMassa(s: String): Boolean = if (massat.contains(s)) true else if (tilavuudet.contains(s)) false 
                                              else if (s == "kpl") throw new KappaleMuunnos("Yritettiin tarkistaa onko 'kpl' massan yksikko", s)
                                              else throw new VirheellinenMittayksikko("Annettiin tuntematon mittayksikko.", s)
  
  
  // Metodi tunnistettu tarkistaa onko annettu mittayksikko ohjelman tunnistama.
  def tunnistettu(s: String): Boolean =  s == "kpl" || massat.contains(s) || tilavuudet.contains(s)
  
  /*
   * suhde-metodit ottavat parametreinaan kaksi mittayksikkoa, ja palauttavat niiden suhteen. Kaytannossa parametri a on aloitusmittayksikko, josta pyritaan kohdeyksikkoon b.
   */
  
  // suhdeMassa laskee kahden massayksikon suhteen (esim kg / g = 1000)
  
  def suhdeMassa(a: String, b: String): Double = tunnistaMassa(a) / tunnistaMassa(b)
  
  // suhdeTilavuus laskee kahden tilavuuden yksikon suhteen (esim. l / dl = 10)
  
  def suhdeTilavuus(a: String, b: String): Double = tunnistaTilavuus(a) / tunnistaTilavuus(b)
  
  // metodi suhde tunnistaa onko kyseessa massa- vai tilavuusmuunnos, ja kayttaa sopivaa metodia palauttaakseen mittayksikoiden suhteen.
  def suhde(a: String, b: String): Double = {
    if (onkoMassa(a) && onkoMassa(b)) suhdeMassa(a, b)                  // Jos parametrit a ja b loytyvat massayksikoiden listasta, kaytetaan suhdeMassat-metodia.
    else if (!onkoMassa(a) && !onkoMassa(b)) suhdeTilavuus(a, b)  // Samoin tilavuuksien puolesta. Jos a ja b eivat tayta naita ehtoja, heitetaan poikkeus.
    else throw new IllegalArgumentException("Parametrit olivat eri muotoa.")
  }
  
  /*
   * Metodit massaMassa, tilavuusTilavuus, massaTilavuus ja tilavuusMassa laskevat yksikkomuunnokset omissa kategorioissaan. Metodit toimivat kaavan 
   * tiheys = massa / tilavuus (d = m / V) perusteella. Metodit massaMassa ja tilavuusTilavuus eivat tarvitse tiheytta; ainoastaan mittayksikoiden
   * suhteen suhde-metodeilta. Tiheys on ohjelmassa aina mittayksikossa g/ml.
   */
  
  def massaMassa(a: String, b: String, m: Double): Double = {
    require(m >= 0.0)
    m * suhdeMassa(a, b)
  }
  
  def tilavuusTilavuus(a: String, b: String, v: Double): Double = {
    require(v >= 0.0)
    v * suhdeTilavuus(a, b)
  }
  
  /*
   *  massaTilavuus ottaa parametreinaan aineen tiheyden d, massan mittayksikon, halutun tilavuusyksikon ja aineen massan m. Palautusarvo on tilavuus anettussa tilavuusyksikossa.
   *  Metodilla voidaan tehda muunnos massayksikoista tilavuuden yksikoihin, aineen massan ja tiheyden avulla. Esimerkiksi jos annetaan m=1kg vetta (tiheys 1000g/1000ml => d = 1.00) ja 
   *  kohdeyksikoksi "dl": metodi laskee vastaukseksi 10.0 (dl).
   */
  
  def massaTilavuus(d: Double, massayksikko: String, tilavuusyksikko: String, m: Double): Double = {
    
    val perustilavuus = d / (m * suhdeMassa(massayksikko, "g")) // Muuttujaan perustilavuus lasketaan aineen tilavuus ohjelman perusyksikossa "ml",
    val kohdetilavuus = tilavuusTilavuus("ml", tilavuusyksikko, perustilavuus) // josta se muutetaan kohdemittayksikkoon.
    
    kohdetilavuus
  }
  
  // tilavuusMassa toimii samalla periaatteella kuin massaTilavuus, mutta lasketaan kaavalla m = d * V.
  def tilavuusMassa(d: Double, tilavuusyksikko: String, massayksikko: String, v: Double): Double = {
    
    val perusmassa = d * v * suhdeTilavuus(tilavuusyksikko, "ml") // Muuttujaan perusmassa lasketaan aineen massa ohjelman perusyksikossa "g",
    val kohdemassa = massaMassa("g", massayksikko, perusmassa)                // josta se muutetaan kohdemittayksikkoon.
    
    kohdemassa
  }
  
  
  /*
   * Metodi laske ottaa parametreinaan aineen tiheyden d (yksikossa g/ml), aloitusmittayksikon, aineen maaran tassa mittayksikossa seka kohdemittayksikon. Metodi palauttaa aineen maaran kohdemittayksikossa.
   * Esimerkiksi jos halutaan selvittaa kuinka monta desilitraa yksi kilo vetta on, syotetaan d = 1.00 (1kg/l = 1g/ml), yksikko1 = "kg", maara = 1.0, yksikko2 = "dl". Metodi selvittaa
   * ensin if-lauseilla metodia onkoMassa hyodyntaen, etta kyseessa on muunnos massasta tilavuuteen, ja syottaa tarvittavat arvot massaTilavuus-metodiin, jolta se saa tulokseksi 10.0.
   */
  def laske(d: Double, yksikko1: String, maara: Double, yksikko2: String): Double = {
    require(maara >= 0.0)
    /*
     *  Muuttujaan uusiMaara lasketaan tarvittava muunnos. Ensin selvitetaan onko kyseessa 1. massa->massa 2. tilavuus->tilavuus 3. massa->tilavuus 4. tilavuus->massa -muunnos.
     *  Uuden arvon laskemiseen kaytetaan xX-metodeita (esim. massaMassa).
     */
    val uusiMaara = {
      if (onkoMassa(yksikko1) && onkoMassa(yksikko2)) massaMassa(yksikko1, yksikko2, maara)                  // Jos seka aloitusmittayksikko etta kohdemittayksikko ovat massan yksikoita, kaytetaan
      else if (!onkoMassa(yksikko1) && !onkoMassa(yksikko2)) tilavuusTilavuus(yksikko1, yksikko2, maara)     // metodia massaMassa. Muut osat toimivat samalla periaatteella.
      else if (onkoMassa(yksikko1) && !onkoMassa(yksikko2)) massaTilavuus(d, yksikko1, yksikko2, maara)
      else tilavuusMassa(d, yksikko1, yksikko2, maara)                                                       // Jos edellisia kolmea ehtoa ei ole taytetty, kyseessa on tilavuus->massa -muunnos.
    }
    
    uusiMaara
  }
  
  // muunna kayttaa laske-metodia, mutta saa tiheyden ja aloitusmittayksikon annetulta Aine-oliolta.
  def muunna(aine: Aine, maara: Double, yksikko: String): Double = laske(aine.tiheys, aine.mittayksikko, maara, yksikko)
  
  // Metodi laskee tiheyden annetuilla parametreilla.
  def laskeTiheys(massa: Double, mYksikko: String, tilavuus: Double, tYksikko: String): Double = {
    
    // Tarkistetaan ovatko annetut mittayksikot sopivat
    if (!onkoMassa(mYksikko)) throw new VirheellinenMittayksikko(mYksikko + " ei ole tunnistettu massan mittayksikko.", mYksikko)
    if (onkoMassa(tYksikko)) throw new VirheellinenMittayksikko(tYksikko + " ei ole tunnistettu tilavuuden mittayksikko.", tYksikko)
    
    // Maarien tulee olla > 0.0
    require(massa > 0.0 && tilavuus > 0.0)
    
    // Muutetaan annetut arvot yksikkoihin g ja ml
    val massag     = massaMassa(mYksikko, "g", massa)
    val tilavuusml = tilavuusTilavuus(tYksikko, "ml", tilavuus)
    
    // Tiheys saadaan laskemalla m / V, eli massag / tilavuusml.
    massag / tilavuusml
  }
  
}