package Smart_Cookbook

import scala.swing._




object GUI extends SimpleSwingApplication {
  
  def top = pääikkuna
  
  /*
   * Pääikkunassa on kolme nappia: Reseptihaku, Luo Resepti ja Varaston hallinta. Näitä painamalla avataan ikkunoita, joilla voi toteuttaa ko. toimintoja.
   * Nappien alla on lista ohjelman aineista, niiden määrästä varastossa ja niiden allergeeneista.
   */
  
  // Pääkomponentit:
  
  val reseptihakuNappi = new Button("Reseptihaku")
  val luoReseptiNappi  = new Button("Luo Resepti")
  val varHallintaNappi = new Button("Varaston hallinta")
  
  // Lista aineista, niiden määrästä ja niiden allergeeneista  TODO: Toteuta metodit, joilla tiedot saadaan haettua taulukkoon
  val sarakenimet                         = Seq("Aine", "Määrä", "Allergeenit")
  val ainelistaTiedot: Array[Array[Any]]  = Varasto.listaaAineet
  val ainelista                           = new Table(ainelistaTiedot, sarakenimet)
  
  
  // Komponenttien asemointi
  val päänapit = new BoxPanel(Orientation.Horizontal)
  päänapit.contents += reseptihakuNappi
  päänapit.contents += luoReseptiNappi
  päänapit.contents += varHallintaNappi
  
  val pääaineet = new BoxPanel(Orientation.Vertical)
  pääaineet.contents += ainelista
  
  val napitJaLista = new BoxPanel(Orientation.Vertical)
  napitJaLista.contents += päänapit
  napitJaLista.contents += pääaineet
  
  val pääikkuna = new MainFrame
  pääikkuna.contents = napitJaLista
  pääikkuna.title    = "Älykäs reseptikirja"
  pääikkuna.size     = new Dimension(800, 800)
  
  
  
  
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
  val aineenNimi           = new TextField
  val allergeeniSuodatin   = new TextField
  val maxPuuttuvatAineet   = new TextField
  
  val ainehakunappi   = new Button("Hae aineita")
  val hakuperuutus    = new Button("Peruuta")
  
  
  // Paneeli, jossa ovat hakukentät
  val hakuTekstit = new BoxPanel(Orientation.Vertical)
  hakuTekstit.contents    += aineenNimi
  hakuTekstit.contents    += allergeeniSuodatin
  hakuTekstit.contents    += maxPuuttuvatAineet
  
  // Paneeli, jossa ovat hakunapit
  val hakuNapit           = new BoxPanel(Orientation.Horizontal)
  hakuNapit.contents     += ainehakunappi
  hakuNapit.contents     += hakuperuutus
  
  // Näiden paneelien yhdistys
  val hakuToiminnot       = new BoxPanel(Orientation.Vertical)
  hakuToiminnot.contents += hakuTekstit
  hakuToiminnot.contents += hakuNapit
  
  // Hakuikkuna
  val hakuikkuna        = new Frame
  hakuikkuna.contents   = hakuToiminnot
  hakuikkuna.visible    = false
  
  
  
  
}