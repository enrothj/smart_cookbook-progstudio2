package Smart_Cookbook

import scala.collection.mutable.Buffer


/*
 * Hakukone etsii ohjelman tiedoista halutuilla kriteereillä aineita. Kriteerejä ovat mm. allergeenit,
 * aineen määrä varastossa ja nimi.
 */

object Hakukone {
  
  
  /*
   * Metodi hae käy läpi Varaston listan Aine-olioista ja palauttaa listan niistä, jotka täyttävät
   * hakuehdot. Parametri n määrittelee kuinka monta ainetta saa puuttua halutuista resepteistä.
   */
  def hae(n: Int = 20): Vector[Aine] = {
    
    var lista: Buffer[Aine] = Buffer[Aine]()  // lista aineista, jotka ovat varastossa, tai voidaan valmistaa varaston aineksista
    
    for (aine <- Varasto.varasto.keys) {   // käydään läpi kaikki varaston aineet
      val ainekset    = aine.ainesosat.map(x => x._1)
      val raakaAineet = aine.aineetYhteensä.map(x => x._1)
      
      // Aine lisätään valmistettavien listaan, jos:
      
      // Ainetta on valmiina varastossa
      if (onValmiina(aine.nimi, 0.1, aine.mittayksikkö)) lista += aine
      
      // Ainetta voidaan valmistaa olemassa olevista aineksista
      else if (voiValmistaa(aine)) lista += aine
      
      //Aine on raaka-aine JA n ei ole nolla. Eli toisin sanoen, aineita saa puuttua, ja tätä ainetta ei voi valmistaa itse (esim. maito haetaan kaupasta yms. ei valmisteta).
      else if (aine.onRaakaAine && n > 0) lista += aine
      
      // Aine ei ole raaka-aine JA puuttuvien AINESOSIEN määrä on korkeintaan n, eli:
      //  ainesten kokonaislkm - valmistettavissa olevat ainekset   oltava korkeintaan  sallittu määrä puuttuvia aineksia
      else if (ainekset.length - ainekset.filter(Hakukone.voiValmistaa(_)).length <= n && !aine.onRaakaAine && n != 0) {
        lista += aine
        }
      
      // Aine ei ole raaka-aine JA puuttuvien RAAKA-AINEIDEN määrä on korkeintaan n, eli:
      //  raaka-aineiden kokonaislkm -  valmistettavissa olevat raaka-aineet       <=  sallittu määrä puuttuvia raaka-aineksia
      else if (raakaAineet.length - raakaAineet.filter(Hakukone.voiValmistaa(_)).length <= n && !aine.onRaakaAine && n != 0) {lista += aine; println("Aineen voi valmistaa raaka-aineistaan")} // Jos aineksia puuttuu korkeintaan n, aine lisätään listaan.
      
    }
    
    
    
    lista.toVector
  }
  
  /*
   *  suodataAllergeenit ottaa parametrina kokoelman suodatettavia allergeeneja ja listan Aine-olioita. Metodi palauttaa kokoelman, josta on suodatettu pois aineet, jotka 
   *  sisältävät yhden tai useamman parametrina annetun allergeenin.
   */
  def suodataAllergeenit(suodatettavat: Buffer[String], lista: Vector[Aine]): Vector[Aine] = {
    var poistettavat: Buffer[Aine] = Buffer()
    
    
    for (aine <- lista) {                                                     // Käydään läpi kokoelman kaikki Aine-oliot
      if (!aine.allergeenit.intersect(suodatettavat).isEmpty) poistettavat += aine    // Jos aineen allergeenilistassa ja parametrin listassa on yhteisiä allergeeneja, aine poistetaan listasta.
    }
    
    lista.diff(poistettavat)
  }
  
  /*
   * Metodi onValmiina tarkistaa onko varastossa annettun nimistä ainetta vähintään n määrä annetussa mittayksikössä.
   */
  def onValmiina(nimi: String, n: Double, mitta: String): Boolean = {
    try {
      val aine = Varasto.aineNimeltä(nimi) // etsitään haluttu aine
      
      val määrä = Varasto.varasto(aine)
        
      if (aine.mittayksikkö != "kpl" && mitta != "kpl") Muuntaja.muunna(aine, määrä, mitta) >= n   // Muunnetaan varaston määrä samaan mittayksikköön (metodi muunna palauttaa saman määrän,
      else if (mitta == "kpl") määrä >= n                                                          // jos yksiköt ovat samat) ja tarkistetaan, että se on vähintään n.
      else false // Kappalemittaista ainetta voi verrata vain kappalemittaiseen.
      
    } catch {
      case e: OlematonAinePoikkeus => println(nimi + " ei ole ohjelman tiedossa oleva aine. Hypätään yli..."); false
      case e: KappaleMuunnos       => println(e.kuvaus + " (" + e.virheData + " --> " + mitta + ")"); false
    }
  }
  
  /*
   * Metodi voiValmistaa ottaa parametrina Aine-olion ja tarkistaa riittävätkö varastossa olevat aineet halutun aineen valmistamiseen.
   * Metodi käy läpi aineen ainesosat ja tarkistaa onko niitä riittävästi aineen valmistukseen.
   */
  def voiValmistaa(aine: Aine): Boolean = {
    val ainekset: Buffer[(Aine, Double, String)] = aine.ainesosat.toBuffer     // Kokoelma aineen ainesosista
    if (ainekset.isEmpty) return false
    var valmistettavissa: Buffer[(Aine, Double, String)] = Buffer()            // Ainesosat, jotka voidaan valmistaa varaston aineista
    var aineetRiittää: Boolean = false                                         
    
    for (aines <- ainekset) {                                                        // Käydään läpi kaikki aineen ainesosat
      if (onValmiina(aines._1.nimi, aines._2, aines._3)) valmistettavissa += aines   // Jos ainesosaa on valmiina varastossa, se lisätään valmistettavissa-muuttujaan
      
      else if (voiValmistaa(aines._1)) {                                             // Jos ei, metodia kutsutaan rekursiivisesti ainesosalle.
        valmistettavissa += aines                                                    // Jos ainesosa voidaan valmistaa omista ainesosistaan, se lisätään valmistettavissa-muuttujaan.
      }
    }
    
    if (ainekset == valmistettavissa) aineetRiittää = true                     // Jos valmistettavissa-muuttujassa on kaikki samat ainesosat kuin ainekset-muuttujassa, se voidaan valmistaa.
    
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
    var nimenSisältävät: Buffer[Aine] = Buffer() // Tähän kerätään kriteerit täyttävät aineet.
    
    require(!lista.isEmpty)
    for (aine <- lista) {                                        // Käydään läpi kaikki aineet
      var sisältääAineen: Boolean = false
      
      if (aine.nimi == nimi) sisältääAineen = true                  // Jos aine itse on annetun niminen, se sisältää annetun aineen.
      
      else if ( aine.sisältääAineen(nimi) ) sisältääAineen = true   // Tarkistetaan sisältääkö jokin raaka-aine annetun aineen.
      
      // Aineet, jotka sisältävät halutun aineen, lisätään listaan.
      if (sisältääAineen) nimenSisältävät += aine
    }
    
    nimenSisältävät.toVector
  }
  

  
}