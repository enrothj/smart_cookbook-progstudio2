package Smart_Cookbook

import org.scalatest._
import scala.collection.mutable.Buffer
import org.scalactic.source.Position.apply

class Testi extends FlatSpec {
  
  // Kaksi esimerkki ruoka-ainetta raaka-aineineen.
  
  val lihapulla = Aine("lihapulla", Buffer("liha"), "Pyöreä lihapulla", 1.0, 1.0, "kpl")
  
  val sb        = Aine("Spagetti Bolognese", Buffer("tomaatti", "liha", "vehnä"), "Spagetti Bolognese", 4.0, 1.0, "kpl")
  val spagetti  = Aine("Spagetti", Buffer("vehnä"), "Spagettia", 400.0, 300.0, "g")
  val kastike   = Aine("bolognese kastike", Buffer("tomaatti", "liha"), "Kastike", 800.0, 800.0, "g")
  val jauheliha = Aine("jauheliha", Buffer("liha"), "jauhelihaa", 1000.0, 400.0, "g")
  val tomaattik = Aine("tomaattikastike", Buffer("tomaatti"), "tomaattikastike", 300.0, 3.0, "dl")
  
  "Aine aineetYhteensä" should "palauttaa tyhjä Array raaka-aineilla" in {
    assert( lihapulla.aineetYhteensä === Array() )
  }
  
  "Aine aineetYhteensä" should "palauttaa Array, jossa kaikki raaka-aineet" in {
    assert( sb.aineetYhteensä.length === 4 ) 
  }
  
  "IO kirjoita" should "tallentaa annettu aine tekstitiedostolle" in {
    ???
  }
  
  "IO tallenna" should "luoda tekstitiedosto, jossa on kaikki aineet" in {
    ???
  }
  
  "IO lue" should "hakea aineet annetusta tekstitiedostosta oikeine määrineen" in {
    ???
  }
  
  "IO lue" should "heittää poikkeus, kun tekstitiedoston tieto on vääränlaista" in {
    ???
  }
  
  "IO lue" should "heittää poikkeus, kun annettua tiedostoa ei ole olemassa" in {
    ???
  }
  
  
  
}