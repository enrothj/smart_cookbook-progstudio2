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
  
  def ainelista: Array[Array[Any]] = Varasto.listaaAineet
  
  
  // Metodi palauttaa taulukon, GUI:n hakutuloksia varten
  def haeAineetTaulukkoon(nimi: String, allergeeniSuodatin: String, maxPuuttuvatAineet: String): Array[Array[Any]] = {

    // Tarkistetaan, että annettu maxPuuttuvatAineet vastaa muotoa Int (jos se on ylipäätään annettu).
    try {
      if (maxPuuttuvatAineet.length > 0) maxPuuttuvatAineet.toInt
    } catch {
      case e: NumberFormatException => throw new IllegalArgumentException("Annettu parametri (" + maxPuuttuvatAineet + ") ei ole numero")
    }
    // Ensin  muutetaan parametrina saadut tekstit hae-metodille sopivaan muotoon.
    
    val allergeenit = allergeeniSuodatin.trim.toLowerCase.split(",").toBuffer
    
    var nPuuttuvat: Int = if (maxPuuttuvatAineet.length > 0) maxPuuttuvatAineet.toInt else 20
    
    var hakutulos = Hakukone.hae(nPuuttuvat)
    

    
  // Jos on määritelty haettava nimi, suodatetaan pois aineet, jotka eivät sisällä määriteltyä ainetta.
    if (nimi.length > 0) hakutulos = Hakukone.sisältää(nimi, hakutulos)
    
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
      
    } catch {
      case e: IllegalArgumentException => println("annettu väärät parametrit: " + e.toString())
      case e: NumberFormatException    => println("Jokin mitoista on väärässä formaatissa")
      case e: VirheellinenMittayksikkö => println("Annettiin tunnistamaton mittayksikkö")
    }
    
    
  }
  
  // Näillä metodeilla kutsutaan Varasto-olion metodeja, jotta voidaan hallita sen tietoja.
  

  // Metodi tarkistaa onko annetun niminen aine olemassa ja poistaa sen, jos on. Palauttaa true, jos toimenpide onnistuu.
  def poistaAine(nimi: String): Boolean = if (Varasto.onOlemassa(nimi)) {Varasto.poistaAine(nimi); true} else false
  
  // Metodi tarkistaa onko annettun niminen aine olemassa, ja onko kohdeyksikkö tunnistettu, ja muuttaa sitten aineen yksikön.
  // HUOM: kappaleyksikköön muunnettaessa pitää muuttaa aineen omia tietoja suoraan.
  def muutaYksikkö(aine: Aine, yksikkö: String): Boolean = {
    
      if ( Varasto.onOlemassa(aine.nimi) && Muuntaja.tunnistettu(yksikkö) && yksikkö != "kpl"){
      Varasto.muutaYksikkö(aine, yksikkö)
      true
      } else false 
    
  }
  
  def asetaMäärä(aine: Aine, määrä: Double) = Varasto.asetaMäärä(aine, määrä)
  
  def lisääAinetta(aine: Aine, määrä: Double) = Varasto.lisaaAinetta(aine, määrä)
  def vähennäAinetta(aine: Aine, määrä: Double) = Varasto.vahennaAinetta(aine, määrä)
  
  def nollaaVarasto() = Varasto.nollaa()
  def tyhjennäVarasto() = Varasto.tyhjennä()
  
  
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
      
      onnistui
      
    } catch {
      case e: NumberFormatException => println("Annettu määrä on väärässä formaatissa"); onnistui
      case e: IllegalArgumentException => println("Annettiin tuntematon operaattori"); onnistui
      case e: OlematonAinePoikkeus  => println("Ohjelma ei tunne ainetta " + e.virheData); onnistui
    }
  }
  
  // Jos annettu komento on joko "nollaa" tai "tyhjennä", suoritetaan annettu komento ja palautetaan true.
  def tyhjennys(komento: String): Boolean = {
    
    if (komento == "nollaa") {nollaaVarasto(); true}
    else if (komento == "tyhjennä") {tyhjennäVarasto(); true}
    else false
    
  }
  
}