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
  def hae(n: Int = 20): Vector[Aine] = {
    
    var lista: Buffer[Aine] = Buffer[Aine]()  // lista aineista, jotka ovat varastossa, tai voidaan valmistaa varaston aineksista
    
    for (aine <- Varasto.varasto.keys) {   // käydään läpi kaikki varaston aineet
      val ainekset    = aine.ainesosat.map(x => x._1)
      val raakaAineet = aine.aineetYhteensä.map(x => x._1)
      
      if (onValmiina(aine.nimi, 0.0, aine.mittayksikkö)) lista += aine  // Jos ainetta on jo valmiina, se lisätään listaan
      
      else if (voiValmistaa(aine)) lista += aine // Jos ainetta voidaan valmistaa olemassa olevista aineksista, se lisätään listaan
      
      //  ainesten kokonaislkm - valmistettavissa olevat ainekset   oltava korkeintaan  sallittu määrä puuttuvia aineksia
      else if (ainekset.length - ainekset.filter(Hakukone.voiValmistaa(_)).length <= n) lista += aine
      
      //  raaka-aineiden kokonaislkm -  valmistettavissa olevat raaka-aineet       <=  sallittu määrä puuttuvia raaka-aineksia
      else if (raakaAineet.length - raakaAineet.filter(Hakukone.voiValmistaa(_)).length <= n) lista += aine // Jos aineksia puuttuu korkeintaan n, aine lisätään listaan.
      
    }
    
    
    
    lista.toVector
  }
  
  /*
   *  suodataAllergeenit ottaa parametrina kokoelman suodatettavia allergeeneja ja listan Aine-olioita. Metodi palauttaa kokoelman, josta on suodatettu pois aineet, jotka 
   *  sisältävät yhden tai useamman parametrina annetun allergeenin.
   */
  def suodataAllergeenit(suodatettavat: Buffer[String], lista: Vector[Aine]): Vector[Aine] = {
    var aineet = lista.toBuffer
    
    for (aine <- aineet) {                                                     // Käydään läpi kokoelman kaikki Aine-oliot
      if (!aine.allergeenit.intersect(suodatettavat).isEmpty) aineet -= aine    // Jos aineen allergeenilistassa ja parametrin listassa on yhteisiä allergeeneja, aine poistetaan listasta.
    }
    
    aineet.toVector
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
    }  TODO: Kun ainesta valmistetaan raaka-aineista, sitä voi tulla enemmän/vähemmän kuin resepti vaatii. Pitää siis verrata ainesmäärää aineksen omaan määrään.
    */
    
    aineetRiittää
  }
  
  /*
   * Metodi suodataNimi ottaa parametrina nimen ja suodattaa sen nimiset Aine-oliot pois annetusta kokoelmasta.
   */
  def suodataNimi(suodatettava: String, lista: Vector[Aine]): Vector[Aine] = lista.filterNot(_.nimi == suodatettava)
  
  // Metodilla haeNimi etsitään halutun niminen Aine-olio.
  def haeNimi(nimi: String): Option[Aine] = Varasto.varasto.keys.find(_.nimi == nimi)
  
  /*
   * Metodi sisältää selvittää onko halutun niminen aine osa ainetta. Se suodattaa annetusta listasta pois aineet, jotka eivät ole annetun nimisiä, tai sisällä ainesosina
   */
  def sisältää(nimi: String, lista: Vector[Aine]): Vector[Aine] = {
    var ainelista = lista.toBuffer
    
    for (aine <- ainelista) {                                        // Käydään läpi kaikki aineet
      var sisältääAineen: Boolean = false
      
      if (aine.nimi == nimi) sisältääAineen = true                   // Jos aine itse on annetun niminen, se sisältää annetun aineen.
      
      else if ( aine.sisältääAineen(nimi) ) sisältääAineen = true    // Tarkistetaan sisältääkö jokin raaka-aine annetun aineen.
      
      if (!sisältääAineen) ainelista -= aine                         // Jos aine ei sisällä annetun nimistä ainetta, se poistetaan ainelistasta
    }
    
    ainelista.toVector
  }
  

  
}