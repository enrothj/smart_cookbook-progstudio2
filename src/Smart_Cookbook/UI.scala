package Smart_Cookbook

import scala.collection.mutable.Buffer

object UI extends App {
  
  // metodi kutsuu IO-olion metodia lataa, joka tayttaa Varaston varasto-muuttujaan kaikki tunnetut Aine-oliot
  def taytaVarasto() = IO.lataa("jaakaappi.txt")
  
  // Metodi tallentaa Varaston tiedot ja tunnetut Aine-oliot tekstitiedostoille, kutsumalla IO:n metodeja.
  def tallennaTiedot() = {
    
    IO.tallenna("jaakaappi.txt") // metodi tallentaa varasto-muuttujan tiedot
    
    for (aine <- Varasto.varasto.keys) { // tallennetaan jokainen Aine-olio
      IO.kirjoita(aine)
    }
    
  }
  
  def ainelista: Array[Array[(String, Double, String)]] = if (!Varasto.varasto.isEmpty) Varasto.listaaAineet else Array(Array(("Varastossa ei ole aineita", 0.0, "")))
  
  // Metodi palauttaa varaston tiedot merkkijonona. Jokaisella rivilla on aine, sen maara ja sen allergeenit
  def listaaVarasto: String = {
    var ainelista: String = "%-60s".format("Aine") + "%30s".format("Maara") + "%70s".format("Allergeenit") + " \n"
    
    // Lisataan riveittain jokaisen aineen nimi, sen maara ja sen allergeenit.
    for (tiedot <- Varasto.varasto) {
      val aine = tiedot._1
      val maara = tiedot._2
      
      // Muuttujissa on formatotuna jokaisen "sarakkeen" teksti.
      val nimi  = "%-60s".format(aine.nimi)
      val arvo = "%30s".format( maara + " (" + aine.mittayksikko + ")" )
      val allergeenit = "%70s".format( aine.listaaAllergeenit )
      
      ainelista += nimi + arvo + allergeenit + " \n"
    }
    
    ainelista
  }
  
  
  // Metodilla voidaan hakea Aineita annetusta kansiosta
  def haeReseptit(sijainti: String): Boolean = {
    try {
      Varasto.lisaaReseptit(sijainti)
      true
    } catch {
      case e: IllegalArgumentException => virhe("Sijainti "+ sijainti + " ei ole kansio." ,GUI.paaikkuna); false
    }
  }
  
  
  // Metodi palauttaa taulukon, GUI:n hakutuloksia varten. HUOM TARPEETON TaLLa HETKELLa, koska GUI ei kayta enaa Table-olioita
  def haeAineetTaulukkoon(nimi: String, allergeeniSuodatin: String, maxPuuttuvatAineet: String): Array[Array[(String, Double)]] = {

    // Tarkistetaan, etta annettu maxPuuttuvatAineet vastaa muotoa Int (jos se on ylipaataan annettu).
    try {
      if (maxPuuttuvatAineet.length > 0) maxPuuttuvatAineet.toInt
    } catch {
      case e: NumberFormatException => throw new IllegalArgumentException("Annettu parametri (" + maxPuuttuvatAineet + ") ei ole numero")
    }
    // Ensin  muutetaan parametrina saadut tekstit hae-metodille sopivaan muotoon.
    
    val allergeenit = if (allergeeniSuodatin.length > 0) poistaValit(allergeeniSuodatin).split(",").toBuffer else Buffer[String]()
    
    var nPuuttuvat: Int = if (maxPuuttuvatAineet.length > 0) maxPuuttuvatAineet.toInt else 20
    
    var hakutulos = Hakukone.hae(nPuuttuvat)
    
    println(hakutulos.length.toString)
    
  // Jos on maaritelty haettava nimi, suodatetaan pois aineet, jotka eivat sisalla maariteltya ainetta.
    try {
      if (nimi.length > 0) hakutulos = Hakukone.sisaltaa(nimi, hakutulos)
    } catch {
      case e: NullPointerException => println("Ainetta " + nimi + " ei ole ohjelman tiedoissa")
    }
    
    if (allergeenit.length > 0) hakutulos = Hakukone.suodataAllergeenit(allergeenit, hakutulos)
      
    val tulostaulukko = {  // Taulukko, jossa on kaikki loydetyt aineet ja niiden maarat
      var tulokset: Buffer[Array[(String, Double)]] = Buffer()
      for (aine <- hakutulos) {
        tulokset += Array((aine.nimi, Varasto.varasto(aine)))
      }
      tulokset.toArray
    }
    
    /* Lisataan hakutulokset hakutulosikkunaan, ja tehdaan se nakyvaksi
      hakutulokset = tulostaulukko
      hakutulosIkkuna.visible = true
      hakutulosIkkuna.repaint() */
      
    tulostaulukko

  }
  
  def haeAineet(nimi: String, allergeeniSuodatin: String, maxPuuttuvatAineet: String): String = {
    
    //Tarkistetaan, etta puuttuvien aineiden kentta on joko Int tai tyhja.
    try {
      if (maxPuuttuvatAineet.length > 0) maxPuuttuvatAineet.toInt
    } catch {
      case e: NumberFormatException => throw new IllegalArgumentException("Annettu parametri (" + maxPuuttuvatAineet + ") ei ole numero")
    }
    // Ensin  muutetaan parametrina saadut tekstit hae-metodille sopivaan muotoon.
    
    val allergeenit = if (allergeeniSuodatin.length > 0) poistaValit(allergeeniSuodatin).split(",").toBuffer else Buffer[String]()
    
    var nPuuttuvat: Int = if (maxPuuttuvatAineet.length > 0) maxPuuttuvatAineet.toInt else 20
    
    var hakutulos = Hakukone.hae(nPuuttuvat)
    // Jos tassa vaiheessa saatu kokoelma on tyhja, seuraavia tarkistuksia ei tehda.
    
    // Jos on maaritelty haettava nimi, suodatetaan pois aineet, jotka eivat sisalla maariteltya ainetta.
    
    if (nimi.length > 0 && !hakutulos.isEmpty) hakutulos = Hakukone.sisaltaa(nimi, hakutulos)
      
    if (allergeenit.length > 0 && !hakutulos.isEmpty) hakutulos = Hakukone.suodataAllergeenit(allergeenit, hakutulos)
    
    // Kaytetaan apumetodia palauttamaan hakutulokset taulukoituna. Jos ei ole hakutuloksia, metodi jatetaan kutsumatta.
    if (hakutulos.isEmpty) "" else hakutuloksetTekstiksi(hakutulos)
    
  }
  
  private def hakutuloksetTekstiksi(tulokset: Vector[Aine]): String = {
    var tulostaulukko: String = "Hakusi tuotti seuraavat aineet: \n"
    
    // Lisataan riveittain aine ja sen maara varastossa (mittayksikon kera).
    for (aine <- tulokset) {
      val nimi          = "%-60s".format(aine.nimi)
      val maara         = Varasto.varasto(aine)
      val mittayksikko  = aine.mittayksikko
      val maaraValmiina = if (maara > 0.0) maara + mittayksikko else "Valmistettava"
      
      tulostaulukko += nimi + maaraValmiina + " \n"
      
    }
    
    tulostaulukko
  }
  
  // Metodilla luodaan Aine-olio, ja tallennetaan se tekstitiedostolle ja ohjelman varastoon. Parametrit taytetaan tekstikenttien
  // kautta GUI:n Reseptinluonti-ikkunassa.
  def luoAine(uusiNimi: String,
    allergeenit: String,
    uusiKuvaus: String,
    maaraJaMitta: String) {
    
    val nimi = korjaaNimi(uusiNimi)
    val allergeenilista: Buffer[String] = if (allergeenit.length > 0) poistaValit(allergeenit).split(",").toBuffer else Buffer()
    val kuvaus = uusiKuvaus
    
    try {
      // Tunnistetaan annetut parametrit tekstista ja tarkistetaan niiden formaatti
      val mitat: Array[String] = if (maaraJaMitta.length > 0) poistaValit(maaraJaMitta).split(",") else Array("0.0","0.0","kpl")
      val tiheys               = if (!mitat(0).isEmpty()) mitat(0).toDouble else 0.0
      val maara                = if (!mitat(1).isEmpty()) mitat(1).toDouble else 0.0
      val mittayksikko         = if (Muuntaja.tunnistettu(mitat(2))) mitat(2) else "kpl"
      
      require(tiheys >= 0.0 && maara >= 0.0)
      
      val aine = Aine(nimi, allergeenilista, kuvaus, tiheys, maara, mittayksikko)
      
      Varasto.uusiAine(aine)
      IO.kirjoita(aine)
      tallennaTiedot()
      
    } catch {
      case e: IllegalArgumentException       => virhe("annettu vaarat parametrit: " + e.toString(), GUI.aineikkuna)
      case e: NumberFormatException          => virhe("Jokin mitoista on vaarassa formaatissa", GUI.aineikkuna)
      case e: VirheellinenMittayksikko       => virhe("Annettiin tunnistamaton mittayksikko:" +e.virheData, GUI.aineikkuna)
      case e: ArrayIndexOutOfBoundsException => virhe("Johonkin kenttaan annettiin vaara maara tietoja.", GUI.aineikkuna)
    }
    
    
  }
  

  
  // Nailla metodeilla kutsutaan Varasto-olion metodeja, jotta voidaan hallita sen tietoja.
  

  // Metodi tarkistaa onko annetun niminen aine olemassa ja poistaa sen, jos on. Palauttaa true, jos toimenpide onnistuu.
  def poistaAine(nimi: String): Boolean = {
    try {
      val poistettuVarastosta: Boolean = Varasto.poistaAine(nimi)
      val poistettuTiedosto: Boolean   = IO.poistaAine(nimi)
      tallennaTiedot()
      poistettuVarastosta && poistettuTiedosto
    } catch {
      case e: OlematonAinePoikkeus => virhe(e.kuvaus, GUI.paaikkuna); true
    }
  }
  
  // Metodi tarkistaa onko annettun niminen aine olemassa, ja onko kohdeyksikko tunnistettu, ja muuttaa sitten aineen yksikon.
  // HUOM: kappaleyksikkoon muunnettaessa pitaa muuttaa aineen omia tietoja suoraan.
  def muutaYksikko(komento: String): Boolean = {
    
    try {
      
      val komennonOsat = komento.toLowerCase.split(" ")
      val nimi         = komennonOsat(0)
      val yksikko      = komennonOsat(1)
    
        if ( Varasto.onOlemassa(nimi) && Muuntaja.tunnistettu(yksikko) && yksikko != "kpl"){
        Varasto.muutaYksikko( Varasto.aineNimelta(nimi), yksikko)
      
        // tallennetaan tiedot tekstitiedostoille, etteivat tiedot katoa
        tallennaTiedot()
        true
        } else false 
        
    } catch {
      case e: KappaleMuunnos           => virhe("Kappalemuotoa kasitellessa mittayksikko pitaa muuttaa aineikkunan kautta.", GUI.paaikkuna); false
      case e: VirheellinenMittayksikko => virhe(e.kuvaus+" "+e.virheData, GUI.paaikkuna); false
      case e: IllegalArgumentException => virhe("Annettiin vääränlaista dataa:" + komento, GUI.paaikkuna); false
    }
  }
  
  def avaaAine(nimi: String): Aine = {
    try {
      Varasto.aineNimelta(korjaaNimi(nimi))
    } catch {
      case e: OlematonAinePoikkeus => IO.lue("reseptit/"+nimi+".txt") // Jos aine ei ole varastossa, se yritetään löytää reseptikansiosta
    }
  }
  
  def asetaMaara(aine: Aine, maara: Double) = Varasto.asetaMaara(aine, maara)
  
  def lisaaAinetta(aine: Aine, maara: Double) = Varasto.lisaaAinetta(aine, maara)
  def vahennaAinetta(aine: Aine, maara: Double) = Varasto.vahennaAinetta(aine, maara)
  
  def nollaaVarasto() = {
    for (aine <- Varasto.varasto.keys) {
      Varasto.asetaMaara(aine, 0.0)
    }
  }
  
  def tyhjennaVarasto() = {
    for (aine <- Varasto.varasto.keys) {
      Varasto.poistaAine(aine.nimi)
    }
    
    tallennaTiedot()
  }
  
  
  /*
   *  Metodille muutaMaaraa annetaan parametrina merkkijono, joka on muotoa "[aineen nimi] [+/-/=] [haluttu maara]". Eli esimerkiksi
   *  jos halutaan lisata vehnajauhoja 500g, kirjoitetaan "vehnajauho + 500.0". Metodi palauttaa true, jos toimenpide onnistui.
   */
 
  def muutaMaaraa(komento: String): Boolean = {
    var onnistui: Boolean = false
    
    try {
      
      val komennonOsat = komento.split(" ")
      val nimi         = komennonOsat(0).toLowerCase
      val operaattori  = komennonOsat(1)                    // taman taytyy olla joko +, - tai = .
      val maara        = komennonOsat(2).toDouble           // taman taytyy olla Double
      
      require(operaattori == "+" || operaattori == "-" || operaattori == "=") // varmistetaan, etta operaattori yksi edellamainituista
      
      if (!Varasto.onOlemassa(nimi)) throw new OlematonAinePoikkeus("Annettua ainetta ei ole ohjelman tiedossa.", nimi)
      
      val aine = Varasto.aineNimelta(nimi)
      
      operaattori match {
        case "+" => lisaaAinetta(aine, maara)
        case "-" => vahennaAinetta(aine, maara)
        case "=" => asetaMaara(aine, maara)
      }
      
      // tallennetaan tiedot tekstitiedostoille, etteivat tiedot katoa
      tallennaTiedot()
      onnistui
      
    } catch {
      case e: NumberFormatException => virhe("Annettu maara on vaarassa formaatissa", GUI.reseptiIkkuna); onnistui
      case e: IllegalArgumentException => virhe("Annettiin tuntematon operaattori. Hyvaksytyt operaattorit ovat +, - ja =", GUI.reseptiIkkuna); onnistui
      case e: OlematonAinePoikkeus  => virhe("Ohjelma ei tunne ainetta " + e.virheData, GUI.reseptiIkkuna); onnistui
      case e: ArrayIndexOutOfBoundsException => virhe("Johonkin kenttaan annettiin vaara maara tietoja.", GUI.reseptiIkkuna); onnistui
    }
  }
  
  // Jos annettu komento on joko "nollaa" tai "tyhjenna", suoritetaan annettu komento ja palautetaan true.
  def tyhjennys(komento: String): Boolean = {
    
    if (komento == "nollaa") {nollaaVarasto(); tallennaTiedot(); true}
    else if (komento == "tyhjenna") {tyhjennaVarasto(); tallennaTiedot(); true}
    else false
    
  }
  
  
  /**
   * Aine-ikkunan metodit:
   */
  
  /*
   *  hallitseAinesosia kutsuu Aine-luokan metodeja lisaaAinesosa (+), poistaAinesosa (-) ja muutaAinesosaa (=). Parametrina otettu komento on merkkijono,
   *  jossa annetaan aineen nimi, operaattori +/-/=, ainesosan nimi, maara ja mittayksikko. Jos operaattori on "-", ei tarvita maaraa ja mittayksikkoa.
   *  Esimerkki: "makaronilaatikko + jauheliha 500.0 g" lisaa makaronilaatikko-oliolle ainesosaksi 500g jauhelihaa. "makaronilaatikko = maito 5.0 dl" 
   *  muuttaa maito-ainesosan maaraksi 5.0 dl. "makaronilaatikko - kananmuna" poistaa ainesosan kananmuna.
   *  
   *  Jos pyydetty toiminto onnistuu, palautetaan true.
   */
  def hallitseAinesosia(komento: String): Boolean = {
    var onnistui: Boolean = false
    
    try {
    
    val komennonOsat = komento.toLowerCase.split(" ") // kerataan kokoelmaan kaikki komennon osat.
    require(komennonOsat.length >= 3)
    
    val aine         = Varasto.aineNimelta(komennonOsat(0))
    val operaattori  = komennonOsat(1)
    val aineksenNimi = komennonOsat(2)
    val aines        = Varasto.aineNimelta(aineksenNimi)
    

      
    operaattori match {
        
        case "+" => {
          require(komennonOsat.length == 5)
          val maara        = komennonOsat(3).toDouble
          val mittayksikko = komennonOsat(4)
          
          aine.lisaaAinesosa(aines, maara, mittayksikko)
         
          onnistui = true
          IO.kirjoita(aine)
        }
        
        case "-" => aine.poistaAinesosa(aineksenNimi); onnistui = true; IO.kirjoita(aine)
        
        case "=" => {
          require(komennonOsat.length == 5)
          val maara        = komennonOsat(3).toDouble
          val mittayksikko = komennonOsat(4)
          
          aine.muutaAinesosaa(aineksenNimi, maara, mittayksikko)
          
          onnistui = true
          IO.kirjoita(aine)
        }
        
        case _ => throw new IllegalArgumentException
      }
      
    } catch {
      
      case e: OlematonAinePoikkeus     => virhe("Annettua ainetta " + e.virheData + " ei ole ohjelman tiedoissa", GUI.aineikkuna)
      case e: NumberFormatException    => virhe("Annettu maara on vaarassa formaatissa", GUI.aineikkuna)
      case e: IllegalArgumentException => virhe("Annettiin vaaranlainen syote", GUI.aineikkuna)
      case e: ArrayIndexOutOfBoundsException => virhe("Johonkin kenttaan annettiin vaara maara tietoja.", GUI.aineikkuna)
    }
    
    // tallennetaan tiedot tekstitiedostoille, etteivat tiedot katoa
    tallennaTiedot()
    onnistui
  }
  
  /*
   *  hallitseAllergeeneja toimii samoin kuin hallitseAinesosia, mutta ei ota parametrina aineen maaraa ja mittayksikkoa.
   *  Jos kaytetaan operaattoria "=", voidaan antaa mielivaltainen maara allergeeneja, mutta + ja - ottavat vain yhden.
   */
  def hallitseAllergeeneja(komento: String): Boolean = {
    var onnistui: Boolean = false
    
    try {
      val komennonOsat = komento.toLowerCase.split(" ")
      
      require(komennonOsat.length >= 3) // komennossa tulee olla vahintaan aineen nimi, operaattori ja yksi allergeeni.
      val aine = Varasto.aineNimelta( komennonOsat(0) )  // samoin kuin aiemmin, nimen tulee olla ensimmainen merkkijono
      val operaattori = komennonOsat(1)
      val allergeeni = komennonOsat(2)
      
      // Jos allergeeneihin on eksynyt ""-niminen allergeeni, poistetaan kaikki sen ilmentymat.
      aine.allergeenit.filter(_ != "")
      
      operaattori match {
        
        case "+" => aine.lisaaAllergeeni(allergeeni); onnistui = true; IO.kirjoita(aine)
        case "-" => aine.poistaAllergeeni(allergeeni); onnistui = true; IO.kirjoita(aine)
        
        case "=" => {
          var uudetAllergeenit: Buffer[String] = Buffer()
          var laskuri = 2
          
          while (laskuri < komennonOsat.length) { // lisataan jokainen kokoelman allergeeni muuttujaan uudetAllergeenit
            
            uudetAllergeenit += komennonOsat(laskuri)
            laskuri += 1
            
          }
          
          aine.uudetAllergeenit(uudetAllergeenit) // saatu kokoelma on parametri metodille uudetAllergeenit
          onnistui = true
          IO.kirjoita(aine)
        }
        
        case _ => throw new IllegalArgumentException
        
      }
      
      
    } catch {
      case e: OlematonAinePoikkeus           => virhe("Annettua ainetta " + e.virheData + " ei ole ohjelman tiedoissa", GUI.aineikkuna)
      case e: IllegalArgumentException       => virhe("Annettiin vaaranlainen syote", GUI.aineikkuna)
      case e: ArrayIndexOutOfBoundsException => virhe("Johonkin kenttaan annettiin vaara maara tietoja.", GUI.aineikkuna)
    }
    
    // tallennetaan tiedot tekstitiedostoille, etteivat tiedot katoa
    tallennaTiedot()
    onnistui
  }
  
  /*
   * Metodi hallitseOminaisuuksia ottaa parametrina merkkijonona annetun komennon. Sen on tarkoitus kutsua Aine-luokan muutaX-metodeja.
   * Merkkijonossa tulee olla ensiksi aineen nimi, toiseksi muutettava ominaisuus ja kolmanneksi haluttu arvo. Nama erotetaan valilyonnilla.
   * Ominaisuudet ovat yksikko, tiheys, maara ja kuvaus.
   * 
   * Metodi palauttaa true, jos haluttu toiminto onnistui.
   */
  
  def hallitseOminaisuuksia(komento: String): Boolean = {
    var onnistui: Boolean = false
    
    try {
      val komennonOsat = komento.toLowerCase.split(" ")
      require(komennonOsat.length >= 3)
      
      val aine        = Varasto.aineNimelta(komennonOsat(0))
      val ominaisuus  = komennonOsat(1)
      val x           = komennonOsat(2)
      // Kuvausta varten kaikki alkiot paitsi ensimmaiset kaksi (aine ja ominaisuus) yhdistetaan merkkijonoksi.
      val kuvaus      = if (ominaisuus == "kuvaus") komennonOsat.drop(2).mkString(" ") else x
      
      ominaisuus match {
        
        case "yksikko" => aine.muutaYksikko(x); onnistui = true; IO.kirjoita(aine)
        
        case "tiheys"  => aine.muutaTiheys(x.toDouble); onnistui = true; IO.kirjoita(aine)
        
        case "maara"   => aine.muutaMaara(x.toDouble); onnistui = true; IO.kirjoita(aine)
        
        case "kuvaus"  => aine.muutaKuvaus(kuvaus); onnistui = true; IO.kirjoita(aine)
        
        case _         => throw new IllegalArgumentException
      }
      
    } catch {
      
      case e: OlematonAinePoikkeus           => virhe("Annettua ainetta " + e.virheData + " ei ole ohjelman tiedoissa", GUI.aineikkuna)
      case e: NumberFormatException          => virhe("Annettu maara on vaarassa formaatissa", GUI.aineikkuna)
      case e: IllegalArgumentException       => virhe("Annettiin vaaranlainen syote", GUI.aineikkuna)
      case e: ArrayIndexOutOfBoundsException => virhe("Johonkin kenttaan annettiin vaara maara tietoja.", GUI.aineikkuna)
    }
    // tallennetaan tiedot tekstitiedostoille, etteivat tiedot katoa
    tallennaTiedot()
    onnistui
  }
  
  
  def listaaRaakaAineet(aine: Aine): String = {
    
    val ainesosat = aine.listaaAinesosat
    val raakaAineet = aine.listaaRaakaAineet
    
    "Ainesosat: " + ainesosat + ".\nRaaka-aineet: " + raakaAineet + "."
    
  }
  
  
  
  
  // Metodi korjaa annetun nimen ohjelman vaatimaan formaattiin, eli valilyonnittomaksi pienikirjaimiseksi merkkijonoksi.
  private def korjaaNimi(nimi: String): String = {
    
    var kirjaimet = nimi.toLowerCase.toCharArray
    val uusiNimi = kirjaimet.map(x => if (x == ' ') '_' else x)
    
    uusiNimi.mkString("")
  }
  
  // Metodi poista valilyonnit annetusta merkkijonosta.
  private def poistaValit(s: String) = {
    
    var kirjaimet = s.toLowerCase.toCharArray
    val uusiTeksti = kirjaimet.filter(_ != ' ')
    
    uusiTeksti.mkString("")
  }
  
  /*
   *  Metodi laskee aineen tiheyden muodossa g/ml, kun sille syotetaan aineen massa ja tilavuus.
   *  Syote annetaan merkkijonona muodossa [massan arvo] [massayksikko] [tilavuuden arvo] [tilavuusyksikko].
   */
  def tiheyslaskuri(komento: String): Double = {
    
    try {
      val komennonOsat = komento.toLowerCase.split(" ")
      require(komennonOsat.length == 4)
      
      val massa    = komennonOsat(0).toDouble
      val mYksikko = komennonOsat(1)
      val tilavuus = komennonOsat(2).toDouble
      val tYksikko = komennonOsat(3)
      
      Muuntaja.laskeTiheys(massa, mYksikko, tilavuus, tYksikko)
      
    } catch {
      case e: NumberFormatException    => virhe("Annettiin virheellinen lukuarvo: " + komento, GUI.paaikkuna); 0.0
      case e: IllegalArgumentException => virhe("Annettiin virheellinen syote: " + komento, GUI.paaikkuna); 0.0
      case e: KappaleMuunnos           => virhe("Kappalemuodosta ei voi laskea tiheytta", GUI.paaikkuna); 0.0
      case e: VirheellinenMittayksikko => virhe(e.kuvaus, GUI.paaikkuna); 0.0
    }
    
    
  }
  
  private def virhe(viesti: String, ikkuna: scala.swing.Window) = GUI.virheviesti(viesti, ikkuna)
  
}