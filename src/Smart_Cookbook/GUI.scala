package Smart_Cookbook

import scala.swing._
import scala.swing.event._
import scala.collection.mutable.Buffer
import scala.collection.Seq





object GUI extends SimpleSwingApplication {
  
  
  
  /*
   * Pääikkunassa on kolme nappia: Reseptihaku, Luo Resepti ja Varaston hallinta. Näitä painamalla avataan ikkunoita, joilla voi toteuttaa ko. toimintoja.
   * Nappien alla on lista ohjelman aineista, niiden määrästä varastossa ja niiden allergeeneista.
   */
  
  // Pääkomponentit:
  
  // Näillä napeilla avataan kolme muuta ikkunaa.
  val reseptihakuNappi = new Button("Reseptihaku")
  val luoReseptiNappi  = new Button("Luo Resepti")
  val varHallintaNappi = new Button("Varaston Hallinta")
  val sulkunappi       = new Button("Sulje Ohjelma")
  val avaaAineNappi    = new Button("Avaa aine")
  
  // Lista aineista, niiden määrästä ja niiden allergeeneista
  val sarakenimet                         = Seq("Aine", "Määrä", "Allergeenit")
  val ainelistaTiedot: Array[Array[Any]]  = UI.ainelista
  val ainelista                           = new Table(ainelistaTiedot, sarakenimet)
  
  
  // Komponenttien asemointi
  val päänapit = new BoxPanel(Orientation.Horizontal)
  päänapit.contents += reseptihakuNappi
  päänapit.contents += luoReseptiNappi
  päänapit.contents += varHallintaNappi
  päänapit.contents += avaaAineNappi
  päänapit.contents += sulkunappi
  
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

  // TAPAHTUMAT:
  
  pääikkuna.listenTo(reseptihakuNappi)
  pääikkuna.listenTo(luoReseptiNappi)
  pääikkuna.listenTo(varHallintaNappi)
  pääikkuna.listenTo(sulkunappi)
  pääikkuna.listenTo(avaaAineNappi)
  
  pääikkuna.reactions += {
    case painallus: ButtonClicked => 
      val nappi = painallus.source
      nappi.text match {
        case "Reseptihaku"       => hakuikkuna.open()
        case "Luo Resepti"       => reseptiIkkuna.open()
        case "Varaston Hallinta" => varIkkuna.open()
        case "Sulje Ohjelma"     => GUI.quit()
        
        case "Avaa aine"         => { // Tätä nappia painamalla aukeaa syötedialogi, josta voidaan avata halutun aineen ikkuna.
         val syöte = Dialog.showInput(pääikkuna, "Syötä aineen nimi.", initial = "")
         syöte match {
           case Some(nimi) => {
             aine = Varasto.aineNimeltä(nimi.toLowerCase)
             aineikkuna.open()
           }
           
           case None => 
         }
         
        }
        
        case _                   => println("Painoit nappia " + nappi.text)
      }
    
  }
  
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
  val ainehakunappi        = new Button("Hae aineita") 
  val hakuperuutus         = new Button("Peruuta")
  
  
  // Paneeli, jossa ovat hakukentät
  val hakuTekstit          = new BoxPanel(Orientation.Vertical)
  hakuTekstit.contents    += aineenNimi
  hakuTekstit.contents    += allergeeniSuodatin
  hakuTekstit.contents    += maxPuuttuvatAineet  // Tämä on käytännössä ainoa, johon voi antaa virheellisen syötteen.
  
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
  
  // TAPAHTUMAT:
  hakuikkuna.listenTo(ainehakunappi)
  hakuikkuna.listenTo(hakuperuutus)
  hakuikkuna.reactions += {
    case painallus: ButtonClicked => 
      val nappi = painallus.source
      nappi.text match {
        case "Hae aineita" => { // Tämä nappi kutsuu UI:n metodia täyttääkseen hakutulosikkunan tulostaulukon.
          try {
            hakutulokset = UI.haeAineetTaulukkoon(aineenNimi.text, allergeeniSuodatin.text, maxPuuttuvatAineet.text)
            if (hakutulokset.isEmpty) Dialog.showMessage(hakuikkuna, "Hakusi ei tuottanut tuloksia") 
            else {
              hakutaulukko.repaint()
              hakutulosIkkuna.open()
            }
            
          } catch {
            case e: IllegalArgumentException => Dialog.showMessage(hakuikkuna, e.toString()) // Dialogi kertoo, että annettu parametri ei ole numero.
          }

        }
        case "Peruuta"     => hakuikkuna.close()
        case _             => println("Painoit nappia " + _)
      }
  }
  
  
  // HAKUTULOS-IKKUNA
  val hakusarakkeet: Seq[String] = Seq("Aine", "määrä")
  var hakutulokset: Array[Array[Any]] = Array()
  var hakutaulukko = new Table(hakutulokset, hakusarakkeet)
  
  val hakutulosSulje = Button("Sulje") {hakutulosIkkuna.close()}
  
  val hakutulosPaneeli = new BoxPanel(Orientation.Vertical)
  hakutulosPaneeli.contents += hakutaulukko
  hakutulosPaneeli.contents += hakutulosSulje
  
  val hakutulosIkkuna = new Frame
  hakutulosIkkuna.title    = "Hakutulokset"
  hakutulosIkkuna.contents = hakutulosPaneeli
  
  
  /** RESEPTINLUONTI-IKKUNA
   * Reseptinluonti-ikkunassa voidaan luoda uusia Aine-olioita ohjelman muistiin. Ikkunassa määritellään tekstikenttiin
   * aineen nimi, allergeenit, tiheys, määrä ja mittayksikkö, sekä kuvaus (valmistusohje jne.). Raaka-aineet lisätään
   * erikseen Aine-ikkunasta.
   * 
   * Ainoa pakollinen parametri on nimi aineelle
   * 
   * Ikkunan ala-laidassa on kaksi nappia: "Tallenna" ja "Peruuta". Tallenna-napilla tallennetaan uusi aine
   * annetuilla tiedoilla. Peruuta-nappi sulkee ikkunan.
   */
  
  // Pääkomponentit:
  val uusiNimi     = new TextField
  val allergeenit  = new TextField
  val määräJaMitta = new TextField // tiheys, määrä ja mitta erotetaan pilkulla (esim. " 0.0,5.0,"kpl")
  val uusiKuvaus   = new TextField
  
  val resTallenna  = new Button("Tallenna")
  val resPeruuta   = new Button("Peruuta")
  
  // Paneeli tekstikentille
  val resTekstit       = new BoxPanel(Orientation.Vertical)
  resTekstit.contents += uusiNimi
  resTekstit.contents += allergeenit
  resTekstit.contents += määräJaMitta
  resTekstit.contents += uusiKuvaus
  
  // Paneeli napeille
  val resNapit           = new BoxPanel(Orientation.Horizontal)
  resNapit.contents     += resTallenna
  resNapit.contents     += resPeruuta
  
  // Näiden paneelien yhdistys
  val resToiminnot       = new BoxPanel(Orientation.Vertical)
  resToiminnot.contents += resTekstit
  resToiminnot.contents += resNapit
  
  // Reseptinluonti-ikkuna
  val reseptiIkkuna      = new Frame
  reseptiIkkuna.contents = resToiminnot
  reseptiIkkuna.visible  = false
  
  // TAPAHTUMAT:
  
  reseptiIkkuna.listenTo(resTallenna)
  reseptiIkkuna.listenTo(resPeruuta)
  reseptiIkkuna.reactions += {
    case painallus: ButtonClicked => 
      val nappi = painallus.source
      nappi.text match {
        
        case "Tallenna" => {
          UI.luoAine(uusiNimi.text, allergeenit.text, uusiKuvaus.text, määräJaMitta.text) // Luodaan uusi aine annetuilla parametreilla
          UI.ainelista
          pääikkuna.repaint()  // päivitetään ainelista
        }
        
        case "Peruuta"  => reseptiIkkuna.close()
        case _ => println("Painoit nappia " + nappi.text)
      }
  }
  
  
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
  
  
  
  // TAPAHTUMAT: 
  
  varIkkuna.listenTo(varMäärä)
  varIkkuna.listenTo(varYksikkö)
  varIkkuna.listenTo(varPoista)
  varIkkuna.listenTo(varNollaa)
  
  varIkkuna.reactions += {
    case painallus: ButtonClicked =>
      val nappi = painallus.source
      
      nappi.text match {
        
        case "Muuta aineen määrää"         => { // Tästä käytetään metodia UI.muutaMäärää
          
          var muutettiin: Boolean = false  // Tähän muuttujaan tallennetaan tieto onnistuneesta muutoksesta.
          val syöte = Dialog.showInput(varIkkuna, "Syötä haluamasi komento: \n+ lisää\n- vähentää\n= asettaa määrän",
                           initial = "[aineen nimi] [+/-/=] [haluttu määrä]")
          syöte match {
            case Some(komento) => muutettiin = UI.muutaMäärää(komento) // Jos annettiin komento, yritetään suorittaa muutos
            case None          => 
          }
          
          if (muutettiin) Dialog.showMessage(varIkkuna, "Tiedot muutettiin onnistuneesti.") // Ilmoitetaan jos muutos onnistui
        }
        
        case "Muuta aineen mittayksikköä"  => { // tästä käytetään metodia UI.muutaYksikkö
          
          var muutettiin: Boolean = false
          val syöte = Dialog.showInput(varIkkuna, "Anna aineen nimi ja haluamasi mittayksikkö (lyhennettynä).\n" +
                                                 "HUOM: Kappalemuotoon voi muuttaa vain aineen omista tiedoista",
                           initial = "[aineen nimi] [haluttu mittayksikkö]")
                           
          syöte match {
            case Some(komento) => muutettiin = UI.muutaYksikkö(komento) // Jos annettiin komento, yritetään suorittaa muutos
            case None          => 
          }
          
          if (muutettiin) Dialog.showMessage(varIkkuna, "Tiedot muutettiin onnistuneesti.") // Ilmoitetaan jos muutos onnistui
        }
        
        case "Poista aine varastosta"      => {// tästä käytetään metodia UI.poistaAine
          
          var muutettiin: Boolean = false
          val syöte = Dialog.showInput(varIkkuna, "Anna poistettavan aineen nimi",
                           initial = "[aineen nimi]")
                           
          syöte match {
            case Some(komento) => muutettiin = UI.poistaAine(komento) // Jos annettiin komento, yritetään suorittaa muutos
            case None          => 
          }
          
          if (muutettiin) Dialog.showMessage(varIkkuna, "Tiedot muutettiin onnistuneesti.") // Ilmoitetaan jos muutos onnistui
          
        }
          
        case "Tyhjennä tai nollaa varasto" => { // Tästä voi nollata tai tyhjentää Varaston.
          
          var muutettiin: Boolean = false
          val syöte = Dialog.showInput(varIkkuna, "Kirjoita 'nollaa' tai 'tyhjennä'.\n"
                                       + "Nollaa asettaa jokaisen aineen määräksi 0.0\n"
                                       + "Tyhjennä poistaa kaikki aineet ohjelman Varastosta",
                           initial = "HUOM: Tyhjentäminen ei poista olemassaolevia tekstitiedostoja.")
                           
          syöte match {
            case Some(komento) => muutettiin = UI.tyhjennys(komento) // Jos annettiin komento, yritetään suorittaa muutos
            case None          => 
          }
          
          if (muutettiin) Dialog.showMessage(varIkkuna, "Tiedot muutettiin onnistuneesti.") // Ilmoitetaan jos muutos onnistui
          
        }
        
        case _ => println("Painoit nappia " + _)
      }
    
    
  }
  
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
  
  aineikkuna.listenTo(aineAinesosa)
  aineikkuna.listenTo(aineAllergeeni)
  aineikkuna.listenTo(aineOminaisuus)
  aineikkuna.listenTo(aineRaakaAineet)
  aineikkuna.reactions += {
    
    case painallus: ButtonClicked =>
      val nappi = painallus.source
      nappi.text match {
        
        case "Hallitse ainesosia" => { // Tästä kutsutaan metodia UI.hallitseAinesosia
          
          var muutettiin: Boolean = false
          val syöte = Dialog.showInput(aineikkuna, "Tästä voi lisätä (+), poistaa (-) tai muuttaa ainesosia (=). Syöteformaatit:\n"
                                       + "+ [ainesosan nimi] [määrä] [mittayksikkö]\n"
                                       + "- [ainesosan nimi]\n"
                                       + "= [ainesosan nimi] [määrä] [mittayksikkö]",
                           initial = "[+/-/=] [ainesosan nimi]  [määrä] [mittayksikkö]")
                           
          syöte match {
            case Some(komento) => muutettiin = UI.hallitseAinesosia(aine.nimi + " " + komento) // HUOM: metodi tarvitsee aineen nimen, mutta käyttäjän ei tarvitse syöttää sitä.
            case None          => 
          }
          
          if (muutettiin) Dialog.showMessage(aineikkuna, "Tiedot muutettiin onnistuneesti.") // Ilmoitetaan jos muutos onnistui
          
        }
        
        case "Hallitse allergeeneja" => {  // Tästä kutsutaan metodia UI.hallitseAllergeeneja
          
          var muutettiin: Boolean = false
          val syöte = Dialog.showInput(aineikkuna, "Tästä voi lisätä (+) tai poistaa (-) allergeeneja tai antaa uusi allergeenilista (=). Syöteformaatit:\n"
                                       + "+ [allergeenin nimi]\n"
                                       + "- [allergeenin nimi]\n"
                                       + "= [allergeeni1] [allergeeni2] [allergeeni3] [jne.]",
                           initial = "[+/-/=] [allergeenin nimi]")
                           
          syöte match {
            case Some(komento) => muutettiin = UI.hallitseAllergeeneja(aine.nimi + " " + komento) // HUOM: metodi tarvitsee aineen nimen, mutta käyttäjän ei tarvitse syöttää sitä.
            case None          => 
          }
          
          if (muutettiin) Dialog.showMessage(aineikkuna, "Tiedot muutettiin onnistuneesti.") // Ilmoitetaan jos muutos onnistui
          
        }
        
        case "Hallitse muita ominaisuuksia" => {
          
          var muutettiin: Boolean = false
          val syöte = Dialog.showInput(aineikkuna, "Tästä voi muuttaa aineen ominaisuuksia. Syötä muutettava ominaisuus ja sopiva uusi arvo.\n"
                                       + "Muutettavia ominaisuuksia ovat yksikkö, tiheys (Double), määrä (Double) ja kuvaus.",
                           initial = "[ominaisuus] [uusi arvo]")
                           
          syöte match {
            case Some(komento) => muutettiin = UI.hallitseOminaisuuksia(aine.nimi + " " + komento) // HUOM: metodi tarvitsee aineen nimen, mutta käyttäjän ei tarvitse syöttää sitä.
            case None          => 
          }
          
          if (muutettiin) Dialog.showMessage(aineikkuna, "Tiedot muutettiin onnistuneesti.") // Ilmoitetaan jos muutos onnistui
        }
        
        case "Aineen raaka-aineet" => Dialog.showMessage(aineikkuna, UI.listaaRaakaAineet(aine)) // Näytetään dialogi, jossa listattuna kaikki ainesosat ja raaka-aineet
        
      }
    
  }
  
  
  // Tällä metodilla päivitetään kaikki ikkunat - yleensä tietojen muuttuessa.
  def päivitä() = {
    
    UI.täytäVarasto()
    pääikkuna.repaint()
    aineikkuna.repaint()
    varIkkuna.repaint()
    hakuikkuna.repaint()
    hakutulosIkkuna.repaint()
    reseptiIkkuna.repaint()
    
  }
  
  this.päivitä()
  
}