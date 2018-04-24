package Smart_Cookbook

import scala.swing._




object GUI extends SimpleSwingApplication {
  
  def top = pääikkuna
  
  /*
   * Pääikkunassa on kolme nappia: Reseptihaku, Luo Resepti ja Varaston hallinta. Näitä painamalla avataan ikkunoita, joilla voi toteuttaa ko. toimintoja.
   * Nappien alla on lista ohjelman aineista, niiden määrästä varastossa ja niiden allergeeneista.
   */
  
  // Pääkomponentit:
  
  val reseptihaku = new Button("Reseptihaku")
  val luoResepti  = new Button("Luo Resepti")
  val varHallinta = new Button("Varaston hallinta")
  
  // Lista aineista, niiden määrästä ja niiden allergeeneista  TODO: Toteuta metodit, joilla tiedot saadaan haettua taulukkoon
  val sarakenimet                = Seq("Aine", "Määrä", "Allergeenit")
  val tiedot: Array[Array[Any]]  = Varasto.listaaAineet
  val ainelista                  = new Table(tiedot, sarakenimet)
  
  // Komponenttien asemointi
  val napit = new BoxPanel(Orientation.Horizontal)
  napit.contents += reseptihaku
  napit.contents += luoResepti
  napit.contents += varHallinta
  
  val pääikkuna = new MainFrame
  pääikkuna.contents = napit
  pääikkuna.title    = "Älykäs reseptikirja"
  pääikkuna.size     = new Dimension(400, 400)
  
  /** RESEPTIHAKU-IKKUNA
   * Reseptihaussa voidaan hakea aineita tekstinä annetuilla kriteereillä. Voidaan antaa haluttuja kriteerejä, 
   * kuten jonkin aineen nimi, tai vältettäviä kriteerejä, kuten allergeenien nimiä. Haussa voidaan myös määritellä
   * halutaanko etsiä ainoastaan aineita, joita on jo varastossa, tai voidaan muodostaa varaston sisältämistä 
   * aineista, vaiko kaikista ohjelman tuntemista aineista.
   * 
   * Ikkunassa on kolme tekstikenttää: haku nimen mukaan, vältettävät allergeenit ja kuinka monta ainesosaa saa
   * puuttua varastosta aineen valmistamiseksi. Ikkunan alaosassa on kaksi nappia: "Hae aineita" ja "Peruuta".
   */
  
  // Pääkomponentit:
  val nimi        = new TextField
  val allergeenit = new TextField
  val puuttuvat   = new TextField
  
  val hakunappi   = new Button("Hae aineita")
  val peruutus    = new Button("Peruuta")
  
  
}