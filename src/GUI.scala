package Smart_Cookbook

import scala.swing._
import scala.swing.event._
import scala.collection.mutable.Buffer




object GUI extends SimpleSwingApplication {
  

  
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
  
    def top = pääikkuna

  
  
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
  val aineenNimi           = new TextField // Tämä kenttä täytetään, jos halutaan tietyn niminen aine
  val allergeeniSuodatin   = new TextField // Tähän kenttään täytetään vältettävät allergeenit, erotettuna pilkulla
  val maxPuuttuvatAineet   = new TextField // Tähän kenttään määritellään kuinka monta ainesosaa saa puuttua varastosta
  
  /*
   *  Tätä nappia painettaessa ohjelma yrittää hakea ainelistan annetuilla parametreilla. Tulokset avataan 
   *  hakutulosikkunaan.
   */
  val ainehakunappi        = new Button("Hae aineita") {
    
    try {
      
      val nimihaku    = aineenNimi.text
      val allergeenit = allergeeniSuodatin.text.toLowerCase.trim.split(",").toBuffer
      // Jos on määritelty sallittu puuttuvien määrä, käytetään sitä, muuten mielivaltaiseksi ylärajaksi 20
      val nPuuttuvat  = if (maxPuuttuvatAineet.text.length > 0) maxPuuttuvatAineet.text.toInt else 20
      
      var hakutulos = Hakukone.hae(nPuuttuvat)
      
      // Jos on määritelty haettava nimi, suodatetaan pois aineet, jotka eivät sisällä määriteltyä ainetta.
      if (nimihaku.length > 0) hakutulos = Hakukone.sisältää(nimihaku, hakutulos)
      
      if (allergeenit.length > 0) hakutulos = Hakukone.suodataAllergeenit(allergeenit, hakutulos)
      
      val tulostaulukko = {  // Taulukko, jossa on kaikki löydetyt aineet ja niiden määrät
        var tulokset: Buffer[Array[Any]] = Buffer()
        for (aine <- hakutulos) {
          tulokset += Array(aine.nimi, Varasto.varasto(aine))
        }
        tulokset.toArray
      }
      
      // Lisätään hakutulokset hakutulosikkunaan, ja tehdään se näkyväksi
      hakutulokset = tulostaulukko
      hakutulosIkkuna.visible = true
      hakutulosIkkuna.repaint()
      
    } catch {
      case e: IllegalArgumentException => Dialog.showMessage(hakuNapit, "Annettiin virheelliset hakuparametrit!")
    }    
  }
  
  val hakuperuutus         = new Button("Peruuta")      {}
  
  
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
  
  
  // HAKUTULOS-IKKUNA
  val hakusarakkeet: Seq[String] = Seq("Aine", "määrä")
  var hakutulokset: Array[Array[Any]] = Array()
  var hakutaulukko = new Table(hakutulokset, hakusarakkeet)
  
  val hakutulosSulje = Button("Sulje") {hakutulosIkkuna.visible = false}
  
  val hakutulosPaneeli = new BoxPanel(Orientation.Vertical)
  hakutulosPaneeli.contents += hakutaulukko
  hakutulosPaneeli.contents += hakutulosSulje
  
  val hakutulosIkkuna = new Frame
  hakutulosIkkuna.title    = "Hakutulokset"
  hakutulosIkkuna.contents = hakutulosPaneeli
  hakutulosIkkuna.visible  = false
  
  
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
  this.listenTo(hakutulosSulje)
  
  
}