package Smart_Cookbook

import scala.collection.mutable.Buffer

object UI extends App {
  
  
  // Metodi palauttaa taulukon, GUI:n hakutuloksia varten
  def haeAineet(nimi: String, allergeenit: Buffer[String], nPuuttuvat: Int = 20): Array[Array[Any]] = {

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
  
}