package Smart_Cookbook

import org.scalatest._
import scala.collection.mutable.Buffer
import org.scalactic.source.Position.apply
import java.io.File
import java.nio.file.Files
import java.nio.file.Paths

class Testi extends FlatSpec {
  
  // !HUOM! Yksikkötestien ajo tyhjentää "jääkaappi.txt"-tiedoston 
  
  // Esimerkkiruoka-aineita raaka-aineineen.
  
  /*
   * Esimerkki:       Spaghetti bolognese
   *                     /              \
   *           spagetti 300g     kastike 800g
   *                              /          \
   *                     jauheliha 400g     tomaattikastike 3 dl
   */
  
  val lihapulla = Aine("lihapulla", Buffer("liha"), "Pyorea lihapulla", 1.0, 1.0, "kpl")
  
  val sb        = Aine("Spagetti Bolognese", Buffer("tomaatti", "liha", "vehna"), "Spagetti Bolognese", 4.0, 1.0, "kpl")
  val spagetti  = Aine("Spagetti", Buffer("vehna"), "Spagettia", 400.0, 300.0, "g")
  val kastike   = Aine("bolognese kastike", Buffer("tomaatti", "liha"), "Kastike", 800.0, 800.0, "g")
  val jauheliha = Aine("jauheliha", Buffer("liha"), "jauhelihaa", 0.8, 400.0, "g")
  val tomaattik = Aine("tomaattikastike", Buffer("tomaatti"), "tomaattikastike", 300.0, 3.0, "dl")
  val sima      = Aine("Sima", Buffer(), "Pullollinen simaa", 1.0, 1.0, "l")
  lazy val jauho = Varasto.aineNimelta("jauho")
  
  // Lisätään ainesosat aineille
  sb.lisaaAinesosa(spagetti, 300, "g")
  sb.lisaaAinesosa(kastike, 800, "g")
  kastike.lisaaAinesosa(jauheliha, 400, "g")
  kastike.lisaaAinesosa(tomaattik, 3.0, "dl")
  
  "Aine onRaakaAine" should "palauttaa true, kun kyseessa raaka-aine" in {
    assert(jauheliha.onRaakaAine === true)
  }
  
  "Aine onRaakaAine" should "palauttaa false, kun on kyseessa aines" in {
    assert(kastike.onRaakaAine === false)
  }
  
  "Aine aineetYhteensa" should "palauttaa tyhja Array raaka-aineille" in {
    assert( jauheliha.aineetYhteensa === Array() )
  }
  
  "Aine aineetYhteensa" should "palauttaa Array, jossa kaikki raaka-aineet" in {
    
    assert( sb.aineetYhteensa.length === 3 ) 
  }
  
  
  "UI luoAine" should "lisata uusi aine ohjelman varastoon" in {
    UI.luoAine("jauho", "", "Mysteerijauho", "0.5,1.0,kg")
    Varasto.asetaMaara(jauho, 1.0)
    assert( Varasto.onOlemassa("jauho") )
  }
  
  "Muuntaja muunna" should "laskea yksikkomuunnokset massasta tilavuuteen oikein" in {
    assert( Muuntaja.muunna( jauheliha , 400.0, "dl") === 5.0 ) // Kun d = 0.8, 400 g => 5 dl
  }
  
  "Varasto muutayksikko" should "muuttaa aineen maara ja yksikko oikein" in {
    Varasto.uusiAine(jauheliha, 400.0)
    Varasto.muutaYksikko(jauheliha, "dl")
    assert( Varasto.varasto(jauheliha) === 5.0 )
  }

  
  "IO tallenna" should "luoda tekstitiedosto, jossa on kaikki varaston aineet" in {
    val aiempi = Varasto.varasto
    IO.tallenna("testi.txt")
    IO.lataa("testi.txt")
    assert(aiempi === Varasto.varasto)
  }
  
  "Varasto nollaa" should "asettaa aineiden maaraksi 0.0" in {
    Varasto.uusiAine(sima, 200.0)
    Varasto.nollaa()
    assert( Varasto.varasto(sima) === 0.0 )
  }
  
  "IO kirjoita" should "tallentaa annettu aine tekstitiedostolle" in {
    IO.kirjoita(sima)
    assert( Files.exists( Paths.get("reseptit/sima.txt") ) )
  }
  
  
  
  "IO lue" should "hakea aineet annetusta tekstitiedostosta oikeine maarineen" in {
    val simatesti = IO.lue("reseptit/sima.txt")
    assert( simatesti.maara === sima.maara )
  }
  /* Näissä oli jo omat catch-mekanisminsa
  "IO lue" should "heittaa poikkeus, kun annettua tiedostoa ei ole olemassa" in {
    intercept[OlematonAinePoikkeus] {
      IO.lue("olematon.txt")
    }
  }
  
  "IO lue" should "heittaa poikkeus, kun tekstitiedoston tieto on vaaranlaista" in {
    intercept[IllegalArgumentException] {
      IO.lue("reseptit/virhetesti.txt")
    }
  }*/
  
  "Varasto tyhjenna" should "poistaa kaikki aineet varastosta" in {
    Varasto.tyhjenna()
    assert( Varasto.varasto.isEmpty )
  }
  
  Files.deleteIfExists(Paths.get("reseptit/sima.txt"))
}