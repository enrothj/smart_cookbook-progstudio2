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
   * Metodi onValmiina tarkistaa onko varastossa annettun nimistä ainetta vähintään n määrä annetussa mittayksikössä.
   */
  def onValmiina(nimi: String, n: Double, mitta: String): Boolean = {
    
    val oikeanniminen = Varasto.varasto.find(_._1.nimi == nimi) // etsitään haluttu aine
    if (oikeanniminen == Option(None)) {                        // Jos sellaista ei ole, palautetaan false.
      false
    } else {
      val aine = oikeanniminen.get._1
      val määrä = oikeanniminen.get._2
      
      Muuntaja.muunna(aine, määrä, mitta) >= n   // Muunnetaan varaston määrä samaan mittayksikköön (metodi muunna palauttaa saman määrän,
                                                 // jos yksiköt ovat samat) ja tarkistetaan, että se on vähintään n.
    }
    
  }
  
  /*
   * Metodi voiValmistaa ottaa parametrina Aine-olion ja tarkistaa riittävätkö varastossa olevat aineet halutun aineen valmistamiseen.
   * Metodi käy läpi aineen ainesosat ja tarkistaa onko niitä riittävästi aineen valmistukseen.
   */
  def voiValmistaa(aine: Aine): Boolean = {
    val ainekset: Buffer[(Aine, Double, String)] = aine.ainesosat.toBuffer     // Kokoelma aineen ainesosista
    var valmistettavissa: Buffer[(Aine, Double, String)] = Buffer()            // Ainesosat, jotka voidaan valmistaa varaston aineista
    var aineetRiittää: Boolean = false                                         
    
    for (aines <- ainekset) {                                                        // Käydään läpi kaikki aineen ainesosat
      if (onValmiina(aines._1.nimi, aines._2, aines._3)) valmistettavissa += aines   // Jos ainesosaa on valmiina varastossa, se lisätään valmistettavissa-muuttujaan
      
      else if (voiValmistaa(aines._1)) {                                             // Jos ei, metodia kutsutaan rekursiivisesti ainesosalle.
        valmistettavissa += aines                                                    // Jos ainesosa voidaan valmistaa omista ainesosistaan, se lisätään valmistettavissa-muuttujaan.
      }
    }
    
    if (ainekset == valmistettavissa) aineetRiittää = true                     // Jos valmistettavissa-muuttujassa on kaikki samat ainesosat kuin ainekset-muuttujassa, se voidaan valmistaa.
    
    /* JÄTETTIIN VANHEMPI TOTEUTUS SÄILÖÖN
    // Tarkistetaan ovatko aineen ainesosat varastossa.                Tarkistetaan ovatko aineen perusraaka-aineet varastossa. 
    if ( aine.ainesosat.forall(x => onValmiina(x._1.nimi, x._2, x._3)) ) {
      aineetRiittää = true}
    
    else if ( aine.aineetYhteensä.forall(x => onValmiina(x._1.nimi, x._2, x._3)) ) {aineetRiittää = true}
    
    else if (ainekset == valmistettavissa) {
      true
    }
    */
    
    aineetRiittää
  }
  
  
  /*
   * Metodi onOlemassa tarkistaa onko ohjelmaan tallennettu parametrina annetun niminen aine.
   */
  def onOlemassa(nimi: String): Boolean = Varasto.varasto.exists(_._1.nimi == nimi)
  
}