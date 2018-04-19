package Smart_Cookbook

import scala.collection.mutable.Buffer

/*
 * Hakukone etsii ohjelman tiedoista halutuilla kriteereillä aineita. Kriteerejä ovat mm. allergeenit,
 * aineen määrä varastossa ja nimi.
 */

object Hakukone {
  
  //TODO: Luo erilaisia hae-metodeja, jotka täyttävät tarvittavan hakutoiminnallisuuden.
  //TODO: Luo metodeja, joilla voi suodattaa tietyillä kriteereillä.
  
  /*
   * Metodi hae käy läpi Varaston listan Aine-olioista ja palauttaa listan niistä, jotka täyttävät
   * hakuehdot. Parametri n määrittelee kuinka monta ainetta saa puuttua halutuista resepteistä.
   */
  def hae(n: Int): Vector[Aine] = {
    
    var lista: Buffer[Aine] = Buffer[Aine]()
    
    for (aine <- Varasto.varasto) {
      
      
      
    }
    
    
    
    lista.toVector
  }
  
  /*
   * Metodi onValmiina tarkistaa onko varastossa annettun nimistä ainetta vähintään n määrä (aineen oletusmittayksikössä).
   */
  def onValmiina(nimi: String, n: Double): Boolean = {
    
    Varasto.varasto.filter(_._1.nimi == nimi) // suodatetaan väärännimiset aineet pois
                      .exists(_._2 >= n)      // tarkistetaan onko näiden joukossa ainetta, jonka määrä on vähintään n.
    
  }
  
  /*
   * Metodi voiValmistaa ottaa parametrina Aine-olion ja tarkistaa riittävätkö varastossa olevat aineet
   * halutun aineen valmistamiseen.
   */
  def voiValmistaa(aine: Aine): Boolean = ???
  
  /*
   * Metodi onOlemassa tarkistaa onko ohjelmaan tallennettu parametrina annetun niminen aine.
   */
  def onOlemassa(nimi: String): Boolean = ???
  
}