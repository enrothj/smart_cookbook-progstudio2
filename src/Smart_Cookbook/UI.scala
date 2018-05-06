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
  
  
  // Metodilla luodaan Aine-olio, ja tallennetaan se tekstitiedostolle ja ohjelman varastoon.
  def luoAine(uusiNimi: String,
    allergeenit: String,
    uusiKuvaus: String,
    määräJaMitta: String) {
    
    val nimi = uusiNimi
    val allergeenilista = allergeenit.trim.toLowerCase.split(",").toBuffer
    val kuvaus = uusiKuvaus
    
    try {
      // Tunnistetaan annetut parametrit tekstistä ja tarkistetaan niiden formaatti
      val mitat: Array[String] = määräJaMitta.trim.toLowerCase.split(",")
      val tiheys               = if (!mitat(0).isEmpty()) mitat(0).toDouble else throw new IllegalArgumentException("Vääränlainen tiheys (" + mitat(0) + ").")
      val määrä                = if (!mitat(1).isEmpty()) mitat(1).toDouble else throw new IllegalArgumentException("Vääränlainen määrä (" + mitat(1) + ").")
      val mittayksikkö         = if (Muuntaja.tunnistettu(mitat(2))) mitat(2) else throw new VirheellinenMittayksikkö("Ohjelma ei tunnista mittayksikköä " + mitat(2), mitat(2))
      
      require(tiheys >= 0.0 && määrä >= 0.0)
      
      val aine = Aine(nimi, allergeenilista, kuvaus, tiheys, määrä, mittayksikkö)
      
      Varasto.uusiAine(aine)
      IO.kirjoita(aine)
      
    } catch {
      case e: IllegalArgumentException => println("annettu väärät parametrit: " + e.toString())
      case e: NumberFormatException    => println("Jokin mitoista on väärässä formaatissa")
      case e: VirheellinenMittayksikkö => println(e.toString())
    }
    
    
  }
  
  // Näillä metodeilla kutsutaan Varasto-olion metodeja, jotta voidaan hallita sen tietoja.
  
  def poistaAine(nimi: String) = Varasto.poistaAine(nimi)
  
  def asetaMäärä(aine: Aine, määrä: Double) = Varasto.asetaMäärä(aine, määrä)
  
  def muutaYksikkö(aine: Aine, yksikkö: String) = Varasto.muutaYksikkö(aine, yksikkö)
  
  def lisääAinetta(aine: Aine, määrä: Double) = Varasto.lisaaAinetta(aine, määrä)
  def vähennäAinetta(aine: Aine, määrä: Double) = Varasto.vahennaAinetta(aine, määrä)
  
  def nollaaVarasto() = Varasto.nollaa()
  def tyhjennäVarasto() = Varasto.tyhjennä()
  
}