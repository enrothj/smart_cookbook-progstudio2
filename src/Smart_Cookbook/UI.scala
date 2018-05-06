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
  def haeAineetTaulukkoon(nimi: String, allergeenit: Buffer[String], nPuuttuvat: Int = 20): Array[Array[Any]] = {

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
  def luoAine(nimi: String,
    allergeenit: Buffer[String],
    kuvaus: String,
    tiheys: Double, määrä: Double,
    mittayksikkö: String) {
    
    try {
      val aine = Aine(nimi, allergeenit, kuvaus, tiheys, määrä, mittayksikkö)
      
      Varasto.uusiAine(aine, 0.0)
      IO.kirjoita(aine)
      
    } catch {
      case e: IllegalArgumentException => println("annettu väärät parametrit")
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