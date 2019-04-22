package Smart_Cookbook

import scala.collection.mutable.Buffer


/*
 * Hakukone etsii ohjelman tiedoista halutuilla kriteereilla aineita. Kriteereja ovat mm. allergeenit,
 * aineen maara varastossa ja nimi.
 */

object Hakukone {
  
  
  /*
   * Metodi hae kay lapi Varaston listan Aine-olioista ja palauttaa listan niista, jotka tayttavat
   * hakuehdot. Parametri n maarittelee kuinka monta ainetta saa puuttua halutuista resepteista.
   */
  def hae(n: Int = 20): Vector[Aine] = {
    
    var lista: Buffer[Aine] = Buffer[Aine]()  // lista aineista, jotka ovat varastossa, tai voidaan valmistaa varaston aineksista
    
    for (aine <- Varasto.varasto.keys) {   // kaydaan lapi kaikki varaston aineet
      val ainekset    = aine.ainesosat.map(x => x._1)
      val raakaAineet = aine.aineetYhteensa.map(x => x._1)
      
      // Aine lisataan valmistettavien listaan, jos:
      
      // Ainetta on valmiina varastossa
      if (onValmiina(aine.nimi, 0.1, aine.mittayksikko, Varasto.varasto)) lista += aine
      
      // Ainetta voidaan valmistaa olemassa olevista aineksista
      else if (voiValmistaa(aine, Varasto.varasto)) lista += aine
      
      //Aine on raaka-aine JA n ei ole nolla. Eli toisin sanoen, aineita saa puuttua, ja tata ainetta ei voi valmistaa itse (esim. maito haetaan kaupasta yms. ei valmisteta).
      else if (aine.onRaakaAine && n > 0) lista += aine
      
      // Aine ei ole raaka-aine JA puuttuvien AINESOSIEN maara on korkeintaan n, eli:
      //  ainesten kokonaislkm - valmistettavissa olevat ainekset   oltava korkeintaan  sallittu maara puuttuvia aineksia
      else if (ainekset.length - ainekset.filter(Hakukone.voiValmistaa(_, Varasto.varasto)).length <= n && !aine.onRaakaAine && n != 0) {
        lista += aine
        }
      
      // Aine ei ole raaka-aine JA puuttuvien RAAKA-AINEIDEN maara on korkeintaan n, eli:
      //  raaka-aineiden kokonaislkm -  valmistettavissa olevat raaka-aineet       <=  sallittu maara puuttuvia raaka-aineksia
      else if (raakaAineet.length - raakaAineet.filter(Hakukone.voiValmistaa(_, Varasto.varasto)).length <= n && !aine.onRaakaAine && n != 0) {lista += aine; println("Aineen voi valmistaa raaka-aineistaan")} // Jos aineksia puuttuu korkeintaan n, aine lisataan listaan.
      
    }
    
    
    
    lista.toVector
  }
  
  /*
   *  suodataAllergeenit ottaa parametrina kokoelman suodatettavia allergeeneja ja listan Aine-olioita. Metodi palauttaa kokoelman, josta on suodatettu pois aineet, jotka 
   *  sisaltavat yhden tai useamman parametrina annetun allergeenin.
   */
  def suodataAllergeenit(suodatettavat: Buffer[String], lista: Vector[Aine]): Vector[Aine] = {
    var poistettavat: Buffer[Aine] = Buffer()
    
    
    for (aine <- lista) {                                                     // Kaydaan lapi kokoelman kaikki Aine-oliot
      if (!aine.allergeenit.intersect(suodatettavat).isEmpty) poistettavat += aine    // Jos aineen allergeenilistassa ja parametrin listassa on yhteisia allergeeneja, aine poistetaan listasta.
    }
    
    lista.diff(poistettavat)
  }
  
  /*
   * Metodi onValmiina tarkistaa onko varastossa annettun nimista ainetta vahintaan n maara annetussa mittayksikossa, määritetyssä varastossa.
   */
  def onValmiina(nimi: String, n: Double, mitta: String, varasto: scala.collection.mutable.Map[Aine, Double]): Boolean = {
    try {
      val aine = Varasto.aineNimelta(nimi) // etsitaan haluttu aine
      
      val maara = Varasto.varasto(aine)
        
      if (aine.mittayksikko != "kpl" && mitta != "kpl") Muuntaja.muunna(aine, maara, mitta) >= n   // Muunnetaan varaston maara samaan mittayksikkoon (metodi muunna palauttaa saman maaran,
      else if (mitta == "kpl") maara >= n                                                          // jos yksikot ovat samat) ja tarkistetaan, etta se on vahintaan n.
      else false // Kappalemittaista ainetta voi verrata vain kappalemittaiseen.
      
    } catch {
      case e: OlematonAinePoikkeus => println(nimi + " ei ole ohjelman tiedossa oleva aine. Hypataan yli..."); false
      case e: KappaleMuunnos       => println(e.kuvaus + " (" + e.virheData + " --> " + mitta + ")"); false
    }
  }
  
  /*
   * Metodi voiValmistaa ottaa parametrina Aine-olion ja tarkistaa riittavatko varastossa olevat aineet halutun aineen valmistamiseen.
   * Aineita, joilla ei ole ainesosia, ei voi valmistaa: esimerkiksi maito, jota pitää hakea kaupasta.
   * Metodi kay lapi aineen ainesosat ja tarkistaa onko niita riittavasti aineen valmistukseen.
   */
  
  
  
  def voiValmistaa(aine: Aine, varasto: scala.collection.mutable.Map[Aine, Double]): Boolean = {
    val ainekset: Buffer[(Aine, Double, String)] = aine.ainesosat.toBuffer     // Kokoelma aineen ainesosista ja niiden määristä. Jos niitä ei ole, ainetta ei voi valmistaa
    if (ainekset.isEmpty) return false
    
    var valmistettavissa: Buffer[(Aine, Double, String)] = Buffer()            // Ainesosat, jotka voidaan valmistaa varaston aineista
    var aineetRiittaa: Boolean = false
    
    var temp = varasto                                        // Luodaan väliaikainen kopio varastosta. Tätä käytetään, jos täytyy valmistaa aineksia raaka-aineista
    
    for (aines <- ainekset) {                                                        // Kaydaan lapi kaikki aineen ainesosat
      if (onValmiina(aines._1.nimi, aines._2, aines._3, temp)) {
        valmistettavissa += aines                                                    // Jos ainesosaa on valmiina varastossa, se lisataan valmistettavissa-muuttujaan
        temp(aines._1) = temp(aines._1) - Muuntaja.muunna(aines._1, aines._2, aines._3) // Otetaan käytetty määrä pois varastosta
      }
      
      else if (voiValmistaa(aines._1, temp)) {                                       // Jos ei, metodia kutsutaan rekursiivisesti ainesosalle.
        valmistettavissa += aines                                                    // Jos ainesosa voidaan valmistaa omista ainesosistaan, se lisataan valmistettavissa-muuttujaan.
      }
    }
    
    if (ainekset == valmistettavissa) aineetRiittaa = true                     // Jos valmistettavissa-muuttujassa on kaikki samat ainesosat kuin ainekset-muuttujassa, se voidaan valmistaa.
    
    aineetRiittaa
  }
  
  /*
   * Metodi suodataNimi ottaa parametrina nimen ja suodattaa sen nimiset Aine-oliot pois annetusta kokoelmasta.
   */
  def suodataNimi(suodatettava: String, lista: Vector[Aine]): Vector[Aine] = lista.filterNot(_.nimi == suodatettava)
  
  // Metodilla haeNimi etsitaan halutun niminen Aine-olio.
  def haeNimi(nimi: String): Option[Aine] = Varasto.varasto.keys.find(_.nimi == nimi)
  
  /*
   * Metodi sisaltaa selvittaa onko halutun niminen aine osa ainetta. Se suodattaa annetusta listasta pois aineet, jotka eivat ole annetun nimisia, tai sisalla ainesosina
   */
  def sisaltaa(nimi: String, lista: Vector[Aine]): Vector[Aine] = {
    var nimenSisaltavat: Buffer[Aine] = Buffer() // Tahan kerataan kriteerit tayttavat aineet.
    
    require(!lista.isEmpty)
    for (aine <- lista) {                                        // Kaydaan lapi kaikki aineet
      var sisaltaaAineen: Boolean = false
      
      if (aine.nimi == nimi) sisaltaaAineen = true                  // Jos aine itse on annetun niminen, se sisaltaa annetun aineen.
      
      else if ( aine.sisaltaaAineen(nimi) ) sisaltaaAineen = true   // Tarkistetaan sisaltaako jokin raaka-aine annetun aineen.
      
      // Aineet, jotka sisaltavat halutun aineen, lisataan listaan.
      if (sisaltaaAineen) nimenSisaltavat += aine
    }
    
    nimenSisaltavat.toVector
  }
  

  
}