package Smart_Cookbook

import scala.io.Source
import scala.collection.mutable.Buffer
import java.io.PrintWriter
import java.io.File
import java.nio.file.Files
import java.nio.file.Paths

/**
 * IO-yksittaisolio kasittelee tekstitiedostoja, joille tallennetaan Varaston ja Aine-olioiden tietoja. Sen metodeilla voidaan
 * tallentaa Varaston tiedot yhdelle tekstitiedostolle ("jaakaappi.txt"), josta ne ladataan ohjelmaa kaynnistettaessa. Aine-
 * oliot tallennetaan omille tekstitiedostoilleen "reseptit/"-kansioon. Naista tiedostoista luodaan Aine-oliot, kun Varastoa 
 * ladataan ohjelmaa kaytettaessa.
 */


object IO {
   
  /*
   * Metodi tallentaa Aineen tekstitiedostolle, Reseptikirjan reseptikansioon. Sielta se voidaan lukea myohemmin tarvittaessa.
   * Ensimmaiselle riville tulee Aineen nimi. Toiselle riville tulee aineen tiheys, maara ja mittayksikko. Seuraaville riveille tulevat ainesosat, 
   * jokainen omalle rivilleen. Ainesosien jalkeen seuraavalla rivilla on tahtimerkki ("*") erottimena, jonka jalkeen seuraavalla rivilla on Aineen 
   * allergeenit. Viimeisilla riveilla on Aineen kuvaus.
   * 
   * Esimerkki:
   *   Spagetti bolognese
   *   0.0,4.0,kpl
   *   spagetti,300.0,g
   *   kastike,800.0,g
   *   *
   *   liha,tomaatti
   *   Spagetti bolognese, nelja annosta. Paista jauheliha... jne.
   * 
   */
  def kirjoita(aine: Aine) = {
    
    val tiedosto = new PrintWriter("reseptit/" + aine.nimi + ".txt")
    
    try {
      tiedosto.println(aine.nimi)                                                // 1. rivi: aineen kuvaus
      
      tiedosto.println(aine.tiheys + "," + aine.maara + "," + aine.mittayksikko) // 2. rivi: tiheys,maara,mittayksikko
      
      for (aines <- aine.ainesosat) {                                            // seuraavat rivit: ainesosa, sen maara, sen mittayksikko (jokainen omalla rivillaan)
        tiedosto.println(aines._1 + "," + aines._2 + "," + aines._3)
      }
      tiedosto.println("*")                                                      // ainesosien loppu erotetaan tahtimerkilla      
      
      tiedosto.println(aine.allergeenit.mkString(","))                           // allergeenit tahtimerkin jalkeen olevalle riville
      
      tiedosto.println(aine.kuvaus)                                              // Viimeiselle riville aineen kuvaus
      
    } finally {
      tiedosto.close()
    }
    
  }
    
  
  /*
   * Metodi tallenna tallentaa Varaston sisaltamat tiedot tekstitiedostolle. Jokaselle riville tulee aineen nimi ja sen maara, esim. "spagetti bolognese, 4.0".
   */
  def tallenna(sijainti: String) = {
    
    val tiedosto = new PrintWriter(sijainti)
    
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
    
    var tiheys: Double = 0.0
    var maara: Double = 0.0
    var mittayksikko: String = ""
    
    var ainesosat: Buffer[Tuple3[Aine, Double, String]] = Buffer()
    var allergeenit: Buffer[String] = Buffer()
    
    // Tarkistetaan, etta annetun niminen tiedosto on olemassa.
    if ( !Files.exists(Paths.get(tiedostonimi)) ) throw new OlematonAinePoikkeus("Reseptit kansiossa ei ole tiedostoa nimeltä " + tiedostonimi, tiedostonimi)
      
    val tiedosto = Source.fromFile(tiedostonimi)
    
    try {
      
      
      val riveja = tiedosto.getLines().toVector
      
      var rivinro = 1 // Muuttujan avulla tiedetaan, mita tietoja kyseiselta tiedoston rivilta pitaisi loytya, esimerkiksi ensimmaiselta rivilta
                      // tulisi loytya Aineen nimi.
      
      for (rivi <- riveja) {
        if (rivinro == 1) {nimi = rivi; rivinro += 1} // Ensimmaiselta rivilta haetaan aineen nimi
        
        else if (rivinro == 2) {                       // Toiselta rivilta haetaan aineen tiheys, maara ja mittayksikko.
          val tiedot   = rivi.split(",").toVector
          if (tiedot.length != 3) throw new VirheellinenData("Tiedosto "+tiedostonimi+" on korruptoitunut.", tiedostonimi)
          tiheys       = tiedot(0).toDouble
          maara        = tiedot(1).toDouble
          mittayksikko = tiedot(2)
          
          rivinro += 1
        }
        
        else if (rivinro == 3 && rivi == "*") rivinro += 1 // Tahtimerkki tarkoittaa, etta enaa ei kasitella ainesosia, vaan siirrytaan allergeeneihin.
        
        else if (rivinro == 4) { // Talta rivilta loytyy lista Aineen sisaltamista allergeeneista.
          allergeenit = rivi.split(",").toBuffer
          rivinro += 1
        }
        
        else kuvaus += rivi // Viimeisilla riveilla on Aineen kuvaus.

        
      }
        
    } catch {
      
      case e: OlematonAinePoikkeus       => virhe(e.kuvaus, GUI.paaikkuna);
      
      case e: IllegalArgumentException   => virhe("Ainetiedosto "+tiedostonimi+" on korruptoitunut.", GUI.paaikkuna);
      
      case e: IndexOutOfBoundsException  => virhe("Ainetiedosto " + tiedostonimi+" on korruptoitunut.", GUI.paaikkuna)
      
      case e: VirheellinenData           => virhe(e.kuvaus, GUI.paaikkuna)
      
      //case _: Throwable => println("Tapahtui odottamaton virhe")
      
    } finally {
      tiedosto.close()
    }
    
    if (nimi == "") {
      virhe("Uuden aineen luonti epaonnistui", GUI.paaikkuna)
      return null
    }
    
    new Aine(nimi, allergeenit, kuvaus, tiheys, maara, mittayksikko) // Lopulta metodi palauttaa Aine-olion saatujen tietojen perusteella.
    
  }
  
  
  // metodi lueAinesosat tayttaa aineen ainesosat-muuttujan tekstitiedoston tiedoilla
  def lueAinesosat(tiedostonimi: String) = {
    
    var aine: Aine = null
    var ainekset: Buffer[Tuple3[Aine, Double, String]] = Buffer()
    
    // Tarkistetaan, etta annetun niminen tiedosto on olemassa.
    if ( !Files.exists(Paths.get(tiedostonimi)) ) throw new OlematonAinePoikkeus("Reseptit kansiossa ei ole tiedostoa nimeltä " + tiedostonimi, tiedostonimi)
        
    val tiedosto = Source.fromFile(tiedostonimi)
    
    try {
      
     
      val riveja = tiedosto.getLines().toVector
      
      var rivinro = 1 // Muuttujan avulla tiedetaan, mita tietoja kyseiselta tiedoston rivilta pitaisi loytya, esimerkiksi ensimmaiselta rivilta
                      // tulisi loytya Aineen nimi.
      
      for (rivi <- riveja) {
        if (rivinro == 1) {
          aine = Varasto.varasto.keys.find(_.nimi == rivi).getOrElse(throw new VirheellinenData("Kyseista ainetta "+rivi+" ei ole tiedossa", rivi))
          rivinro += 1 // Ensimmaiselta rivilta haetaan kyseessa oleva aine.
        }
        
        else if (rivinro == 2) {rivinro += 1}  // Toisella rivilla on aineen tiheys, maara ja mittayksikko, joten mennaan seuraavalle riville
        
        else if (rivinro == 3 && rivi != "*") { // Seuraavilta riveilta (tahtimerkkiin asti) loytyvat kaikki eri ainesosat, jotka kuuluvat aineeseen.
          
          val aines = rivi.split(",").toVector
          val raakaAine = Varasto.varasto.keys.find(_.nimi == aines(0)).getOrElse(throw new OlematonAinesosa("Aines nimelta " + aines(0) + " ei ole (enaa) ohjelman tiedoissa.", rivi))
          
          ainekset += Tuple3(Varasto.varasto.keys.find(_.nimi == aines(0)).get, aines(1).toDouble, aines(2))
          
        } else if (rivinro == 3 && rivi == "*") rivinro += 1 // Tahtimerkki tarkoittaa, etta enaa ei kasitella ainesosia, vaan siirrytaan allergeeneihin.
        
      }
    } catch {
      
      case e: IllegalArgumentException => virhe("Annettua tiedostoa " +tiedostonimi+" ei ole olemassa tai se on korruptoitunut.", GUI.paaikkuna);
      
      case e: VirheellinenData => virhe(e.kuvaus, GUI.paaikkuna)
      
      case e: OlematonAinesosa => virhe(e.kuvaus, GUI.paaikkuna)
      
    } finally {
      tiedosto.close()
    }
    
    
    if (aine != null) {
      aine.ainesosat = ainekset.toArray   // Muutetaan ainesosat muuttujan sisalto tekstitiedostoa vastaavaksi.
    }
  }
  
  
  /*
   * Metodi lataa hakee tiedot Varastolle tekstitiedostolta.
   */
  
  def lataa(sijainti: String) = {
    
    // Jos jaakaappitiedostoa ei ole olemassa, se luodaan
    if ( !Files.exists(Paths.get(sijainti)) ) {
      tallenna(sijainti)
    }
    
    val tiedosto = Source.fromFile(sijainti)           // haetaan tallennetut tiedot "jaakaappi.txt"-tiedostolta
    
    try {
      
    
      val riveja = tiedosto.getLines().toVector
      
      // Ensin luodaan kaikki "jaakaappi.txt"-tiedostolla olevat Aineet.
      for (rivi <- riveja) {                                  // kaydaan jokainen tekstitiedoston rivi lapi (jokaisella rivilla aineen nimi ja sen maara)
        
        val tiedot = rivi.split(",")
        
        val aineSijainti = "reseptit/" + tiedot(0) + ".txt"   // Aine-tiedoston sijainti
        val maara        = tiedot(1).toDouble
        
        // Kutsutaan Varaston uusiAine-metodia, jolla lisataan metodin lue avulla aineSijainnin maarittelema Aine-olio Varaston muistiin.
        val aine = lue(aineSijainti)
        if (aine == null) throw new VirheellinenData("Ainetta " + tiedot(0) + " ei onnistuttu luomaan", tiedot(0)) 
        Varasto.uusiAine( aine, maara )          
        
      } 
      
      // Kun jokainen aine on lisatty Varastoon, yritetaan lisata jokaiselle aineelle sen mahdolliset ainesosat.
      for (aine <- Varasto.varasto.keys) {
        lueAinesosat("reseptit/" + aine.nimi + ".txt")
      }
      
    } catch {
      
      case e: IllegalArgumentException => virhe("Jaakaappi-tiedostossa on virheellista dataa.", GUI.paaikkuna);
      
      case e: VirheellinenData => virhe(e.kuvaus, GUI.paaikkuna)
      
    } finally {
      tiedosto.close()
    }
    
  }
  
  // Metodi, jolla voidaan hakea kaikki annetussa kansiossa olevat aineet.
  // Varasto käyttää metodia täyttämään varaston tiedot resepteistä, vaikka niitä ei olisi tallennettu jaakaappi.txt:n
  
  def lueReseptit(sijainti: String): Buffer[Aine] = {
    val kansio = new File(sijainti)
    if ( !kansio.isDirectory() ) throw new IllegalArgumentException // Poikkeus heitetään, jos annettu sijainti ei ole kansio
    
    val tiedostot = kansio.listFiles()
    
    var reseptit = Buffer[Aine]()
    
    for (resepti <- tiedostot) {
      reseptit += lue( resepti.getPath() )
    }
    
    reseptit
  }
  
  // Metodi poistaa aine-tekstitiedoston
  def poistaAine(nimi: String): Boolean = {
    
    val sijainti: String = "reseptit/" + nimi + ".txt"
    
    Files.delete(Paths.get(sijainti))
    
    !Files.exists(Paths.get(sijainti))
  }
  
  private def virhe(viesti: String, ikkuna: scala.swing.Window) = GUI.virheviesti(viesti, ikkuna)
  
  
}