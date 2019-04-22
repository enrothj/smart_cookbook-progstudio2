package Smart_Cookbook

import org.scalatest._
import scala.collection.mutable.Buffer
import org.scalactic.source.Position.apply
import java.io.File
import java.nio.file.Files
import java.nio.file.Paths

class Testi extends FlatSpec {
  
  // Esimerkkiruoka-aineita raaka-aineineen.
  
  val lihapulla = Aine("lihapulla", Buffer("liha"), "Pyorea lihapulla", 1.0, 1.0, "kpl")
  
  val sb        = Aine("Spagetti Bolognese", Buffer("tomaatti", "liha", "vehna"), "Spagetti Bolognese", 4.0, 1.0, "kpl")
  val spagetti  = Aine("Spagetti", Buffer("vehna"), "Spagettia", 400.0, 300.0, "g")
  val kastike   = Aine("bolognese kastike", Buffer("tomaatti", "liha"), "Kastike", 800.0, 800.0, "g")
  val jauheliha = Aine("jauheliha", Buffer("liha"), "jauhelihaa", 1000.0, 400.0, "g")
  val tomaattik = Aine("tomaattikastike", Buffer("tomaatti"), "tomaattikastike", 300.0, 3.0, "dl")
  val sima      = Aine("Sima", Buffer(), "Pullollinen simaa", 1.0, 1.0, "l")
  
  println(Varasto.listaaAineet)
  
  
  "Aine aineetYhteensa" should "palauttaa tyhja Array raaka-aineilla" in {
    assert( lihapulla.aineetYhteensa === Array() )
  }
  
  "Aine aineetYhteensa" should "palauttaa Array, jossa kaikki raaka-aineet" in {
    assert( sb.aineetYhteensa.length === 4 ) 
  }
  
  "Varasto uusiAine" should "lisata uusi aine ohjelman varastoon" in {
    Varasto.uusiAine(lihapulla, 1.0)
    assert( Varasto.onOlemassa("lihapulla") )
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