package Smart_Cookbook

import scala.io.Source
import scala.collection.mutable.Buffer
import java.io.PrintWriter

object IO {
   
  /*
   * Metodi tallentaa Aineen tekstitiedostolle, Reseptikirjan reseptikansioon. Sieltä se voidaan lukea myöhemmin tarvittaessa.
   * Ensimmäiselle riville tulee Aineen nimi. Toiselle riville tulee aineen tiheys, määrä ja mittayksikkö. Seuraaville riveille tulevat ainesosat, 
   * jokainen omalle rivilleen. Ainesosien jälkeen seuraavalla rivillä on tähtimerkki ("*") erottimena, jonka jälkeen seuraavalla rivillä on Aineen 
   * allergeenit. Viimeisillä riveillä on Aineen kuvaus.
   * 
   * Esimerkki:
   *   Spagetti bolognese
   *   0.0,4.0,kpl
   *   spagetti,300.0,g
   *   kastike,800.0,g
   *   *
   *   liha,tomaatti
   *   Spagetti bolognese, neljä annosta. Paista jauheliha... jne.
   * 
   */
  def kirjoita(aine: Aine) = {
    
    val tiedosto = new PrintWriter("reseptit/" + aine.nimi + ".txt")
    
    try {
      tiedosto.println(aine.nimi)                                                // 1. rivi: aineen kuvaus
      
      tiedosto.println(aine.tiheys + "," + aine.määrä + "," + aine.mittayksikkö) // 2. rivi: tiheys,määrä,mittayksikkö
      
      for (aines <- aine.ainesosat) {                                            // seuraavat rivit: ainesosa, sen määrä, sen mittayksikkö (jokainen omalla rivillään)
        tiedosto.println(aines._1 + "," + aines._2 + "," + aines._3)
      }
      tiedosto.println("*")                                                      // ainesosien loppu erotetaan tähtimerkillä      
      
      tiedosto.println(aine.allergeenit.mkString(","))                           // allergeenit tähtimerkin jälkeen olevalle riville
      
      tiedosto.println(aine.kuvaus)                                              // Viimeiselle riville aineen kuvaus
      
    } finally {
      tiedosto.close()
    }
    
  }  // TODO: poikkeusten käsittely
    
  
  /*
   * Metodi tallenna tallentaa Varaston sisältämät tiedot tekstitiedostolle. Jokaselle riville tulee aineen nimi ja sen määrä, esim. "spagetti bolognese, 4.0".
   */
  def tallenna() = {
    
    val tiedosto = new PrintWriter("jaakaappi.txt")
    
    try {
      
      for (rivi <- Varasto.varasto) {
        tiedosto.println(rivi._1.nimi + "," + rivi._2)
      }
      
    } finally {
      tiedosto.close()
    }    
  }
    
    

  // metodi lukee tekstitiedoston ja luo tietojen perusteella uuden Aine-olion
  def lue(tiedostonimi: String): Aine = {
    
    var nimi: String = ""; var kuvaus: String = ""
    
    val tiedosto = Source.fromFile(tiedostonimi)
    
    var tiheys: Double = 0.0
    var määrä: Double = 0.0
    var mittayksikkö: String = ""
    
    var ainesosat: Buffer[Tuple3[Aine, Double, String]] = Buffer()
    var allergeenit: Buffer[String] = Buffer()
    
    try {
      
      val riveja = tiedosto.getLines().toVector
      
      var rivinro = 1 // Muuttujan avulla tiedetään, mitä tietoja kyseiseltä tiedoston riviltä pitäisi löytyä, esimerkiksi ensimmäiseltä riviltä
                      // tulisi löytyä Aineen nimi.
      
      for (rivi <- riveja) {
        if (rivinro == 1) {nimi = rivi; rivinro += 1} // Ensimmäiseltä riviltä haetaan aineen nimi
        
        else if (rivinro == 2) {                       // Toiselta riviltä haetaan aineen tiheys, määrä ja mittayksikkö.
          val tiedot   = rivi.split(",").toVector
          tiheys       = tiedot(0).toDouble
          määrä        = tiedot(1).toDouble
          mittayksikkö = tiedot(2)
          
          rivinro += 1
        }
        
        else if (rivinro == 3 && rivi != "*") { // Seuraavilta riveiltä (tähtimerkkiin asti) haetaan kaikki eri ainesosat, jotka kuuluvat aineeseen.
          
          val aines = rivi.split(",").toVector
          ainesosat += Tuple3(Varasto.varasto.keys.find(_.nimi == aines(0)).get, aines(1).toDouble, aines(2))
          
        } else if (rivinro == 3 && rivi == "*") rivinro += 1 // Tähtimerkki tarkoittaa, että enää ei käsitellä ainesosia, vaan siirrytään allergeeneihin.
        
        else if (rivinro == 4) { // Tältä riviltä löytyy lista Aineen sisältämistä allergeeneista.
          allergeenit = rivi.split(",").toBuffer
          rivinro += 1
        }
        
        else kuvaus += rivi // Viimeisillä riveillä on Aineen kuvaus.

        
      }
        
    } catch {
      
      case e: IllegalArgumentException => println("Annettiin väärä parametri");
      
      case e: VirheellinenData => println("Annettiin väärää dataa")
      
      case _: Throwable => println("Tapahtui odottamaton virhe")
      
    }
    
    new Aine(nimi, allergeenit, kuvaus, tiheys, määrä, mittayksikkö) // Lopulta metodi palauttaa Aine-olion saatujen tietojen perusteella.
    
  }
  
  
  // metodi lueAinesosat täyttää aineen ainesosat-muuttujan tekstitiedoston tiedoilla
  def lueAinesosat(tiedostonimi: String) = {
        
    val tiedosto = Source.fromFile(tiedostonimi)
     
    var aine: Aine = null
    var ainekset: Buffer[Tuple3[Aine, Double, String]] = Buffer()

    
    try {
      
      val riveja = tiedosto.getLines().toVector
      
      var rivinro = 1 // Muuttujan avulla tiedetään, mitä tietoja kyseiseltä tiedoston riviltä pitäisi löytyä, esimerkiksi ensimmäiseltä riviltä
                      // tulisi löytyä Aineen nimi.
      
      for (rivi <- riveja) {
        if (rivinro == 1) {
          aine = Varasto.varasto.keys.find(_.nimi == rivi).getOrElse(throw new VirheellinenData("Kyseistä ainetta ei ole tiedossa", rivi))
          rivinro += 1 // Ensimmäiseltä riviltä haetaan kyseessä oleva aine.
        }
        
        else if (rivinro == 2) {rivinro += 1}  // Toisella rivillä on aineen tiheys, määrä ja mittayksikkö, joten mennään seuraavalle riville
        
        else if (rivinro == 3 && rivi != "*") { // Seuraavilta riveiltä (tähtimerkkiin asti) löytyvät kaikki eri ainesosat, jotka kuuluvat aineeseen.
          
          val aines = rivi.split(",").toVector
          val raakaAine = Varasto.varasto.keys.find(_.nimi == aines(0)).getOrElse(throw new VirheellinenData("Kyseistä ainetta ei ole tiedossa", rivi))
          
          ainekset += Tuple3(Varasto.varasto.keys.find(_.nimi == aines(0)).get, aines(1).toDouble, aines(2))
          
        } else if (rivinro == 3 && rivi == "*") rivinro += 1 // Tähtimerkki tarkoittaa, että enää ei käsitellä ainesosia, vaan siirrytään allergeeneihin.
        
      }
    } catch {
      
      case e: IllegalArgumentException => println("Annettiin väärä parametri");
      
      case e: VirheellinenData => println("Annettiin väärää dataa")
      
      case _: Throwable => println("Tapahtui odottamaton virhe")
      
    }
    
    aine.ainesosat = ainekset.toArray   // Muutetaan ainesosat muuttujan sisältö tekstitiedostoa vastaavaksi.
  }
  
  
  /*
   * Metodi lataa hakee tiedot Varastolle tekstitiedostolta.
   */
  
  def lataa() = {
    
    val tiedosto = Source.fromFile("jaakaappi.txt")           // haetaan tallennetut tiedot "jaakaappi.txt"-tiedostolta
    
    // "reseptit/" + aine.nimi + ".txt"
    
    try {
      
      val riveja = tiedosto.getLines().toVector
      
      for (rivi <- riveja) {                                  // käydään jokainen tekstitiedoston rivi läpi
        
        val tiedot = rivi.split(",")
        
        val aineSijainti = "reseptit/" + tiedot(0) + ".txt"   // Aine-tiedoston sijainti
        val määrä        = tiedot(1).toDouble
        
        Varasto.uusiAine( lue(aineSijainti), määrä )          // Kutsutaan Varaston uusiAine-metodia, jolla lisätään aineSijainnin mukainen Aine-olio Varaston muistiin.
        
      } 
      
      for (aine <- Varasto.varasto.keys) {
        lueAinesosat("reseptit/" + aine.nimi + ".txt")
      }
      
    } catch {
      
      case e: IllegalArgumentException => println("Annettiin väärä parametri");
      
      case e: VirheellinenData => println("Annettiin väärää dataa")
      

      
    }
  }
  
}