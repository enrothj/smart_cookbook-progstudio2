package Smart_Cookbook

import scala.swing._
import scala.swing.event._




object GUI extends SimpleSwingApplication {
  
  def top = pääikkuna
  
  /*
   * Pääikkunassa on kolme nappia: Reseptihaku, Luo Resepti ja Varaston hallinta. Näitä painamalla avataan ikkunoita, joilla voi toteuttaa ko. toimintoja.
   * Nappien alla on lista ohjelman aineista, niiden määrästä varastossa ja niiden allergeeneista.
   */
  
  // Pääkomponentit:
  
  // Näillä napeilla avataan kolme muuta ikkunaa.
  val reseptihakuNappi = new Button("Reseptihaku")       {hakuikkuna.visible = true}
  val luoReseptiNappi  = new Button("Luo Resepti")       {reseptiIkkuna.visible = true}
  val varHallintaNappi = new Button("Varaston hallinta") {varIkkuna.visible = true}
  
  // Lista aineista, niiden määrästä ja niiden allergeeneista
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
  
  val ainehakunappi        = new Button("Hae aineita")
  val hakuperuutus         = new Button("Peruuta")
  
  
  // Paneeli, jossa ovat hakukentät
  val hakuTekstit          = new BoxPanel(Orientation.Vertical)
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
  
  
  /** RESEPTINLUONTI-IKKUNA
   * Reseptinluonti-ikkunassa voidaan luoda uusia Aine-olioita ohjelman muistiin. Ikkunassa määritellään tekstikenttiin
   * aineen nimi, allergeenit, määrä ja mittayksikkö, mahdolliset raaka-aineet sekä kuvaus (valmistusohje jne.)
   * 
   * Ikkunan ala-laidassa on kolme nappia: "Tallenna", "Avaa" ja "Peruuta". Tallenna-napilla tallennetaan uusi aine
   * annetuilla tiedoilla. Avaa-napilla voidaan täytää kentät olemassa olevan aineen tiedoilla. Peruuta-nappi sulkee ikkunan.
   */
  
  // Pääkomponentit:
  val uusiNimi     = new TextField
  val allergeenit  = new TextField
  val määräJaMitta = new TextField
  val raakaAineet  = new TextField
  val uusiKuvaus   = new TextField
  
  val resTallenna  = new Button("Tallenna")
  val resAvaa      = new Button("Avaa")
  val resPeruuta   = new Button("Peruuta")
  
  // Paneeli tekstikentille
  val resTekstit       = new BoxPanel(Orientation.Vertical)
  resTekstit.contents += uusiNimi
  resTekstit.contents += allergeenit
  resTekstit.contents += määräJaMitta
  resTekstit.contents += raakaAineet
  resTekstit.contents += uusiKuvaus
  
  // Paneeli napeille
  val resNapit           = new BoxPanel(Orientation.Horizontal)
  resNapit.contents     += resTallenna
  resNapit.contents     += resAvaa
  resNapit.contents     += resPeruuta
  
  // Näiden paneelien yhdistys
  val resToiminnot       = new BoxPanel(Orientation.Vertical)
  resToiminnot.contents += resTekstit
  resToiminnot.contents += resNapit
  
  // Reseptinluonti-ikkuna
  val reseptiIkkuna      = new Frame
  reseptiIkkuna.contents = resToiminnot
  reseptiIkkuna.visible  = false
  
  
  
  /** VARASTONHALLINTA-IKKUNA
   * Varastonhallintaikkunassa on kourallinen nappeja, joista aukeaa ponnahdusikkunoita, joihin voi antaa syötteitä. Napit
   * ovat aineen määrän muuttamiseen, yksikön muuttamiseen, aineen poistamiseen varastosta sekä varaston tyhjentämiseen tai
   * nollaamiseen.
   */
  
  // Pääkomponentit
  val varMäärä   = new Button("Muuta aineen määrää")
  val varYksikkö = new Button("Muuta aineen mittayksikköä")
  val varPoista  = new Button("Poista aine varastosta")
  val varNollaa  = new Button("Tyhjennä tai nollaa varasto")
  
  
  // Komponenttien asemointi
  val varPaneeli       = new BoxPanel(Orientation.Vertical)
  varPaneeli.contents += varMäärä
  varPaneeli.contents += varYksikkö
  varPaneeli.contents += varPoista
  varPaneeli.contents += varNollaa
  
  val varIkkuna        = new Frame
  varIkkuna.contents   = varPaneeli
  varIkkuna.visible    = false
  
  /** AINE-IKKUNA
   * Aine-ikkuna aukeaa, kun valitaan jokin aine jostakin ikkunasta. Aine-ikkunassa voidaan katsastaa ja hallita Aine-olioiden 
   * ominaisuuksia. Ikkunassa on taulukko, jossa on listattuna aineen ominaisuuksia ja niiden arvoja.
   * 
   * Ominaisuustaulukon lisäksi ikkunassa on napit ainesosien, allergeenien, ja muiden ominaisuuksien hallitsemiseen.
   * "Raaka-aineet"-napilla ohjelma antaa listan aineen raaka-aineista.
   * 
   */
  
  // Pääkomponentit
  var aine: Aine = null
  var ainetiedot: Array[Array[Any]] = Array()
  val aineSarakenimet: Seq[String]  = Seq("Ominaisuus", "Arvo")
  val ominaisuuslista               = new Table(ainetiedot, aineSarakenimet)
  
  val aineAinesosa    = new Button("Hallitse ainesosia")
  val aineAllergeeni  = new Button("Hallitse allergeeneja")
  val aineOminaisuus  = new Button("Hallitse muita ominaisuuksia")
  val aineRaakaAineet = new Button("Aineen raaka-aineet")
  
  // Komponenttien asemointi
  val aineNapit         = new BoxPanel(Orientation.Vertical)
  aineNapit.contents   += aineAinesosa
  aineNapit.contents   += aineAllergeeni
  aineNapit.contents   += aineOminaisuus
  aineNapit.contents   += aineRaakaAineet
  
  val ainePaneeli       = new BoxPanel(Orientation.Horizontal)
  ainePaneeli.contents += ominaisuuslista
  ainePaneeli.contents += aineNapit
  
  val aineikkuna        = new Frame
  aineikkuna.contents   = ainePaneeli
  aineikkuna.visible    = false
  
  
  /**
   * TAPAHTUMAT:
   */
  
  // Tapahtumat:
  
  // Kuunnellaan kaikkia nappeja
  this.listenTo(reseptihakuNappi)
  this.listenTo(luoReseptiNappi)
  this.listenTo(varHallintaNappi)
  this.listenTo(ainehakunappi)
  this.listenTo(hakuperuutus)
  this.listenTo(resTallenna)
  this.listenTo(resAvaa)
  this.listenTo(resPeruuta)
  this.listenTo(varMäärä)
  this.listenTo(varYksikkö)
  this.listenTo(varPoista)
  this.listenTo(varNollaa)
  this.listenTo(aineAinesosa)
  this.listenTo(aineAllergeeni)
  this.listenTo(aineOminaisuus)
  this.listenTo(aineRaakaAineet)
  
  
}