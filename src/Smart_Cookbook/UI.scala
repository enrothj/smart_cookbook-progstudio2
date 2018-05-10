package Smart_Cookbook

import scala.collection.mutable.Buffer

object UI extends App {
  
  // metodi kutsuu IO-olion metodia lataa, joka täyttää Varaston varasto-muuttujaan kaikki tunnetut Aine-oliot
  def täytäVarasto() = IO.lataa
  
  // Metodi tallentaa Varaston tiedot ja tunnetut Aine-oliot tekstitiedostoille, kutsumalla IO:n metodeja.
  def tallennaTiedot() = {
    
    IO.tallenna() // metodi tallentaa varasto-muuttujan tiedot
    
    for (aine <- Varasto.varasto.keys) { // tallennetaan jokainen Aine-olio
      IO.kirjoita(aine)
    }
    
  }
  
  def ainelista: Array[Array[Any]] = if (!Varasto.varasto.isEmpty) Varasto.listaaAineet else Array(Array("Varastossa ei ole aineita", 0.0, ""))
  
  // Metodi palauttaa varaston tiedot merkkijonona. Jokaisella rivillä on aine, sen määrä ja sen allergeenit
  def listaaVarasto: String = {
    var ainelista: String = "%-60s".format("Aine") + "%30s".format("Määrä") + "%70s".format("Allergeenit") + " \n"
    
    // Lisätään riveittäin jokaisen aineen nimi, sen määrä ja sen allergeenit.
    for (tiedot <- Varasto.varasto) {
      val aine = tiedot._1
      val määrä = tiedot._2
      
      // Muuttujissa on formatotuna jokaisen "sarakkeen" teksti.
      val nimi  = "%-60s".format(aine.nimi)
      val arvo = "%30s".format( määrä + " (" + aine.mittayksikkö + ")" )
      val allergeenit = "%70s".format( aine.listaaAllergeenit )
      
      ainelista += nimi + arvo + allergeenit + " \n"
    }
    
    ainelista
  }
  
  
  // Metodi palauttaa taulukon, GUI:n hakutuloksia varten
  def haeAineetTaulukkoon(nimi: String, allergeeniSuodatin: String, maxPuuttuvatAineet: String): Array[Array[Any]] = {

    // Tarkistetaan, että annettu maxPuuttuvatAineet vastaa muotoa Int (jos se on ylipäätään annettu).
    try {
      if (maxPuuttuvatAineet.length > 0) maxPuuttuvatAineet.toInt
    } catch {
      case e: NumberFormatException => throw new IllegalArgumentException("Annettu parametri (" + maxPuuttuvatAineet + ") ei ole numero")
    }
    // Ensin  muutetaan parametrina saadut tekstit hae-metodille sopivaan muotoon.
    
    val allergeenit = if (allergeeniSuodatin.length > 0) allergeeniSuodatin.trim.toLowerCase.split(",").toBuffer else Buffer[String]()
    
    var nPuuttuvat: Int = if (maxPuuttuvatAineet.length > 0) maxPuuttuvatAineet.toInt else 20
    
    var hakutulos = Hakukone.hae(nPuuttuvat)
    
    println(hakutulos.length.toString)
    
  // Jos on määritelty haettava nimi, suodatetaan pois aineet, jotka eivät sisällä määriteltyä ainetta.
    try {
      if (nimi.length > 0) hakutulos = Hakukone.sisältää(nimi, hakutulos)
    } catch {
      case e: NullPointerException => println("Ainetta " + nimi + " ei ole ohjelman tiedoissa")
    }
    
    if (allergeenit.length > 0) hakutulos = Hakukone.suodataAllergeenit(allergeenit, hakutulos)
      
    val tulostaulukko = {  // Taulukko, jossa on kaikki löydetyt aineet ja niiden määrät
      var tulokset: Buffer[Array[Any]] = Buffer()
      for (aine <- hakutulos) {
        tulokset += Array(aine.nimi, Varasto.varasto(aine))
      }
      tulokset.toArray
    }
    
    /* Lisätään hakutulokset hakutulosikkunaan, ja tehdään se näkyväksi
      hakutulokset = tulostaulukko
      hakutulosIkkuna.visible = true
      hakutulosIkkuna.repaint() */
      
    tulostaulukko

  }
  
  def haeAineet(nimi: String, allergeeniSuodatin: String, maxPuuttuvatAineet: String): String = {
    
    //Tarkistetaan, että puuttuvien aineiden kenttä on joko Int tai tyhjä.
    try {
      if (maxPuuttuvatAineet.length > 0) maxPuuttuvatAineet.toInt
    } catch {
      case e: NumberFormatException => throw new IllegalArgumentException("Annettu parametri (" + maxPuuttuvatAineet + ") ei ole numero")
    }
    // Ensin  muutetaan parametrina saadut tekstit hae-metodille sopivaan muotoon.
    
    val allergeenit = if (allergeeniSuodatin.length > 0) allergeeniSuodatin.trim.toLowerCase.split(",").toBuffer else Buffer[String]()
    
    var nPuuttuvat: Int = if (maxPuuttuvatAineet.length > 0) maxPuuttuvatAineet.toInt else 20
    
    var hakutulos = Hakukone.hae(nPuuttuvat)
    // Jos tässä vaiheessa saatu kokoelma on tyhjä, seuraavia tarkistuksia ei tehdä.
    
    // Jos on määritelty haettava nimi, suodatetaan pois aineet, jotka eivät sisällä määriteltyä ainetta.
    
    if (nimi.length > 0 && !hakutulos.isEmpty) hakutulos = Hakukone.sisältää(nimi, hakutulos)
      
    if (allergeenit.length > 0 && !hakutulos.isEmpty) hakutulos = Hakukone.suodataAllergeenit(allergeenit, hakutulos)
    
    hakutulos.map(x => x.nimi).mkString(", ")
    
  }
  
  
  // Metodilla luodaan Aine-olio, ja tallennetaan se tekstitiedostolle ja ohjelman varastoon. Parametrit täytetään tekstikenttien
  // kautta GUI:n Reseptinluonti-ikkunassa.
  def luoAine(uusiNimi: String,
    allergeenit: String,
    uusiKuvaus: String,
    määräJaMitta: String) {
    
    val nimi = uusiNimi
    val allergeenilista: Buffer[String] = if (allergeenit.length > 0) allergeenit.trim.toLowerCase.split(",").toBuffer else Buffer()
    val kuvaus = uusiKuvaus
    
    try {
      // Tunnistetaan annetut parametrit tekstistä ja tarkistetaan niiden formaatti
      val mitat: Array[String] = if (määräJaMitta.length > 0) määräJaMitta.trim.toLowerCase.split(",") else Array("0.0","0.0","kpl")
      val tiheys               = if (!mitat(0).isEmpty()) mitat(0).toDouble else 0.0
      val määrä                = if (!mitat(1).isEmpty()) mitat(1).toDouble else 0.0
      val mittayksikkö         = if (Muuntaja.tunnistettu(mitat(2))) mitat(2) else "kpl"
      
      require(tiheys >= 0.0 && määrä >= 0.0)
      
      val aine = Aine(nimi, allergeenilista, kuvaus, tiheys, määrä, mittayksikkö)
      
      Varasto.uusiAine(aine)
      IO.kirjoita(aine)
      tallennaTiedot()
      
    } catch {
      case e: IllegalArgumentException       => println("annettu väärät parametrit: " + e.toString())
      case e: NumberFormatException          => println("Jokin mitoista on väärässä formaatissa")
      case e: VirheellinenMittayksikkö       => println("Annettiin tunnistamaton mittayksikkö")
      case e: ArrayIndexOutOfBoundsException => println("Johonkin kenttään annettiin väärä määrä tietoja.")
    }
    
    
  }
  
  // Näillä metodeilla kutsutaan Varasto-olion metodeja, jotta voidaan hallita sen tietoja.
  

  // Metodi tarkistaa onko annetun niminen aine olemassa ja poistaa sen, jos on. Palauttaa true, jos toimenpide onnistuu.
  def poistaAine(nimi: String): Boolean = {
    if (Varasto.onOlemassa(nimi)) {
      Varasto.poistaAine(nimi)
      IO.poistaAine(nimi)
      // tallennetaan tiedot tekstitiedostoille, etteivät tiedot katoa
      tallennaTiedot()
      true
      } else false
  }
  
  // Metodi tarkistaa onko annettun niminen aine olemassa, ja onko kohdeyksikkö tunnistettu, ja muuttaa sitten aineen yksikön.
  // HUOM: kappaleyksikköön muunnettaessa pitää muuttaa aineen omia tietoja suoraan.
  def muutaYksikkö(komento: String): Boolean = {
    
    val komennonOsat = komento.toLowerCase.split(" ")
    val nimi         = komennonOsat(0)
    val yksikkö      = komennonOsat(1)
    
      if ( Varasto.onOlemassa(nimi) && Muuntaja.tunnistettu(yksikkö) && yksikkö != "kpl"){
      Varasto.muutaYksikkö( Varasto.aineNimeltä(nimi), yksikkö)
      
      // tallennetaan tiedot tekstitiedostoille, etteivät tiedot katoa
      tallennaTiedot()
      true
      } else false 
    
  }
  
  def asetaMäärä(aine: Aine, määrä: Double) = Varasto.asetaMäärä(aine, määrä)
  
  def lisääAinetta(aine: Aine, määrä: Double) = Varasto.lisaaAinetta(aine, määrä)
  def vähennäAinetta(aine: Aine, määrä: Double) = Varasto.vahennaAinetta(aine, määrä)
  
  def nollaaVarasto() = Varasto.nollaa(); tallennaTiedot()
  def tyhjennäVarasto() = Varasto.tyhjennä(); tallennaTiedot()
  
  
  /*
   *  Metodille muutaMäärää annetaan parametrina merkkijono, joka on muotoa "[aineen nimi] [+/-/=] [haluttu määrä]". Eli esimerkiksi
   *  jos halutaan lisätä vehnäjauhoja 500g, kirjoitetaan "vehnäjauho + 500.0". Metodi palauttaa true, jos toimenpide onnistui.
   */
 
  def muutaMäärää(komento: String): Boolean = {
    var onnistui: Boolean = false
    
    try {
      
      val komennonOsat = komento.split(" ")
      val nimi         = komennonOsat(0).toLowerCase
      val operaattori  = komennonOsat(1)                    // tämän täytyy olla joko +, - tai = .
      val määrä        = komennonOsat(2).toDouble           // tämän täytyy olla Double
      
      require(operaattori == "+" || operaattori == "-" || operaattori == "=") // varmistetaan, että operaattori yksi edellämainituista
      
      if (!Varasto.onOlemassa(nimi)) throw new OlematonAinePoikkeus("Annettua ainetta ei ole ohjelman tiedossa.", nimi)
      
      val aine = Varasto.aineNimeltä(nimi)
      
      operaattori match {
        case "+" => lisääAinetta(aine, määrä)
        case "-" => vähennäAinetta(aine, määrä)
        case "=" => asetaMäärä(aine, määrä)
      }
      
      // tallennetaan tiedot tekstitiedostoille, etteivät tiedot katoa
      tallennaTiedot()
      onnistui
      
    } catch {
      case e: NumberFormatException => println("Annettu määrä on väärässä formaatissa"); onnistui
      case e: IllegalArgumentException => println("Annettiin tuntematon operaattori"); onnistui
      case e: OlematonAinePoikkeus  => println("Ohjelma ei tunne ainetta " + e.virheData); onnistui
      case e: ArrayIndexOutOfBoundsException => println("Johonkin kenttään annettiin väärä määrä tietoja."); onnistui
    }
  }
  
  // Jos annettu komento on joko "nollaa" tai "tyhjennä", suoritetaan annettu komento ja palautetaan true.
  def tyhjennys(komento: String): Boolean = {
    
    if (komento == "nollaa") {nollaaVarasto(); true}
    else if (komento == "tyhjennä") {tyhjennäVarasto(); true}
    else false
    
  }
  
  
  /**
   * Aine-ikkunan metodit:
   */
  
  /*
   *  hallitseAinesosia kutsuu Aine-luokan metodeja lisääAinesosa (+), poistaAinesosa (-) ja muutaAinesosaa (=). Parametrina otettu komento on merkkijono,
   *  jossa annetaan aineen nimi, operaattori +/-/=, ainesosan nimi, määrä ja mittayksikkö. Jos operaattori on "-", ei tarvita määrää ja mittayksikköä.
   *  Esimerkki: "makaronilaatikko + jauheliha 500.0 g" lisää makaronilaatikko-oliolle ainesosaksi 500g jauhelihaa. "makaronilaatikko = maito 5.0 dl" 
   *  muuttaa maito-ainesosan määräksi 5.0 dl. "makaronilaatikko - kananmuna" poistaa ainesosan kananmuna.
   *  
   *  Jos pyydetty toiminto onnistuu, palautetaan true.
   */
  def hallitseAinesosia(komento: String): Boolean = {
    var onnistui: Boolean = false
    
    try {
    
    val komennonOsat = komento.toLowerCase.split(" ") // kerätään kokoelmaan kaikki komennon osat.
    require(komennonOsat.length >= 3)
    
    val aine         = Varasto.aineNimeltä(komennonOsat(0))
    val operaattori  = komennonOsat(1)
    val aineksenNimi = komennonOsat(2)
    val aines        = Varasto.aineNimeltä(aineksenNimi)
    

      
    operaattori match {
        
        case "+" => {
          require(komennonOsat.length == 5)
          val määrä        = komennonOsat(3).toDouble
          val mittayksikkö = komennonOsat(4)
          
          aine.lisääAinesosa(aines, määrä, mittayksikkö)
         
          onnistui = true
          IO.kirjoita(aine)
        }
        
        case "-" => aine.poistaAinesosa(aineksenNimi); onnistui = true; IO.kirjoita(aine)
        
        case "=" => {
          require(komennonOsat.length == 5)
          val määrä        = komennonOsat(3).toDouble
          val mittayksikkö = komennonOsat(4)
          
          aine.muutaAinesosaa(aineksenNimi, määrä, mittayksikkö)
          
          onnistui = true
          IO.kirjoita(aine)
        }
        
        case _ => throw new IllegalArgumentException
      }
      
    } catch {
      
      case e: OlematonAinePoikkeus     => println("Annettua ainetta " + e.virheData + " ei ole ohjelman tiedoissa")
      case e: NumberFormatException    => println("Annettu määrä on väärässä formaatissa")
      case e: IllegalArgumentException => println("Annettiin vääränlainen syöte")
      case e: ArrayIndexOutOfBoundsException => println("Johonkin kenttään annettiin väärä määrä tietoja.")
    }
    
    // tallennetaan tiedot tekstitiedostoille, etteivät tiedot katoa
    tallennaTiedot()
    onnistui
  }
  
  /*
   *  hallitseAllergeeneja toimii samoin kuin hallitseAinesosia, mutta ei ota parametrina aineen määrää ja mittayksikköä.
   *  Jos käytetään operaattoria "=", voidaan antaa mielivaltainen määrä allergeeneja, mutta + ja - ottavat vain yhden.
   */
  def hallitseAllergeeneja(komento: String): Boolean = {
    var onnistui: Boolean = false
    
    try {
      val komennonOsat = komento.toLowerCase.split(" ")
      
      require(komennonOsat.length >= 3) // komennossa tulee olla vähintään aineen nimi, operaattori ja yksi allergeeni.
      val aine = Varasto.aineNimeltä( komennonOsat(0) )  // samoin kuin aiemmin, nimen tulee olla ensimmäinen merkkijono
      val operaattori = komennonOsat(1)
      val allergeeni = komennonOsat(2)
      
      operaattori match {
        
        case "+" => aine.lisääAllergeeni(allergeeni); onnistui = true; IO.kirjoita(aine)
        case "-" => aine.poistaAllergeeni(allergeeni); onnistui = true; IO.kirjoita(aine)
        
        case "=" => {
          var uudetAllergeenit: Buffer[String] = Buffer()
          var laskuri = 2
          
          while (laskuri < komennonOsat.length) { // lisätään jokainen kokoelman allergeeni muuttujaan uudetAllergeenit
            
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
      case e: OlematonAinePoikkeus => println("Annettua ainetta " + e.virheData + " ei ole ohjelman tiedoissa")
      case e: IllegalArgumentException => println("Annettiin vääränlainen syöte")
      case e: ArrayIndexOutOfBoundsException => println("Johonkin kenttään annettiin väärä määrä tietoja.")
    }
    
    // tallennetaan tiedot tekstitiedostoille, etteivät tiedot katoa
    tallennaTiedot()
    onnistui
  }
  
  /*
   * Metodi hallitseOminaisuuksia ottaa parametrina merkkijonona annetun komennon. Sen on tarkoitus kutsua Aine-luokan muutaX-metodeja.
   * Merkkijonossa tulee olla ensiksi aineen nimi, toiseksi muutettava ominaisuus ja kolmanneksi haluttu arvo. Nämä erotetaan välilyönnillä.
   * Ominaisuudet ovat yksikkö, tiheys, määrä ja kuvaus.
   * 
   * Metodi palauttaa true, jos haluttu toiminto onnistui.
   */
  
  def hallitseOminaisuuksia(komento: String): Boolean = {
    var onnistui: Boolean = false
    
    try {
      val komennonOsat = komento.toLowerCase.split(" ")
      require(komennonOsat.length >= 3)
      
      val aine        = Varasto.aineNimeltä(komennonOsat(0))
      val ominaisuus  = komennonOsat(1)
      val x        = komennonOsat(2)
      
      ominaisuus match {
        
        case "yksikkö" => aine.muutaYksikkö(x); onnistui = true; IO.kirjoita(aine)
        
        case "tiheys"  => aine.muutaTiheys(x.toDouble); onnistui = true; IO.kirjoita(aine)
        
        case "määrä"   => aine.muutaMäärä(x.toDouble); onnistui = true; IO.kirjoita(aine)
        
        case "kuvaus"  => aine.muutaKuvaus(x); onnistui = true; IO.kirjoita(aine)
        
        case _         => throw new IllegalArgumentException
      }
      
    } catch {
      
      case e: OlematonAinePoikkeus     => println("Annettua ainetta " + e.virheData + " ei ole ohjelman tiedoissa")
      case e: NumberFormatException    => println("Annettu määrä on väärässä formaatissa")
      case e: IllegalArgumentException => println("Annettiin vääränlainen syöte")
      case e: ArrayIndexOutOfBoundsException => println("Johonkin kenttään annettiin väärä määrä tietoja.")
    }
    // tallennetaan tiedot tekstitiedostoille, etteivät tiedot katoa
    tallennaTiedot()
    onnistui
  }
  
  
  def listaaRaakaAineet(aine: Aine): String = {
    
    val ainesosat = aine.listaaAinesosat
    val raakaAineet = aine.listaaRaakaAineet
    
    "Ainesosat: " + ainesosat + ".\nRaaka-aineet: " + raakaAineet + "."
    
  }
  
  
}