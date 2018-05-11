package Smart_Cookbook

import scala.swing._
import scala.swing.event._
import scala.collection.mutable.Buffer
import scala.collection.Seq






object GUI extends SimpleSwingApplication {
  
  // ensin täytetään ohjelman tiedot tekstitiedostoilta
  UI.täytäVarasto()
  

  
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
  val tiheyslaskuri    = new Button("Tiheyslaskuri")
  
  // Lista aineista, niiden määrästä ja niiden allergeeneista
  val ainelista = new TextArea()
  ainelista.text = UI.listaaVarasto
  ainelista.editable = false
  

  
  /*val sarakenimet                         = Seq("Aine", "Määrä", "Allergeenit")
  var ainelistaTiedot: Array[Array[Any]]  = UI.ainelista
  val ainelista                           = new Table(ainelistaTiedot, sarakenimet)
  */
  
  // Komponenttien asemointi
  val päänapit = new BoxPanel(Orientation.Horizontal)
  päänapit.contents += reseptihakuNappi
  päänapit.contents += luoReseptiNappi
  päänapit.contents += varHallintaNappi
  päänapit.contents += avaaAineNappi
  päänapit.contents += tiheyslaskuri
  päänapit.contents += sulkunappi
  
  val pääaineet = new BoxPanel(Orientation.Vertical)
  pääaineet.contents += ainelista
  
  val napitJaLista = new BoxPanel(Orientation.Vertical)
  napitJaLista.contents += päänapit
  napitJaLista.contents += pääaineet
  
  val pääikkuna = new MainFrame
  pääikkuna.contents = napitJaLista
  pääikkuna.title    = "Älykäs reseptikirja"
  pääikkuna.minimumSize     = new Dimension(700, 400)
  pääikkuna.centerOnScreen()
  
  def top = pääikkuna
  
  def päivitäAinelista() = ainelista.text = UI.listaaVarasto; ainelista.repaint(); pääikkuna.repaint()

  // TAPAHTUMAT:
  
  pääikkuna.listenTo(reseptihakuNappi)
  pääikkuna.listenTo(luoReseptiNappi)
  pääikkuna.listenTo(varHallintaNappi)
  pääikkuna.listenTo(sulkunappi)
  pääikkuna.listenTo(avaaAineNappi)
  pääikkuna.listenTo(tiheyslaskuri)
  
  pääikkuna.reactions += {
    case painallus: ButtonClicked => 
      val nappi = painallus.source
      nappi.text match {
        case "Reseptihaku"       => hakuikkuna.open()
        case "Luo Resepti"       => reseptiIkkuna.open()
        case "Varaston Hallinta" => varIkkuna.open()
        
        // "Sulje Ohjelma"-nappi tallentaa ohjelman tiedot ja sulkee ohjelman.
        case "Sulje Ohjelma"     => UI.tallennaTiedot(); GUI.quit()
        
        case "Avaa aine"         => { // Tätä nappia painamalla aukeaa syötedialogi, josta voidaan avata halutun aineen ikkuna.
         val syöte = Dialog.showInput(pääikkuna, "Syötä aineen nimi.", initial = "")
         syöte match {
           case Some(nimi) => {
             try {
               aine = UI.avaaAine(nimi)
               päivitäOminaisuudet()
               aineikkuna.open()
             } catch {
               case e: OlematonAinePoikkeus => Dialog.showMessage(aineikkuna, e.kuvaus)
               
             }
           }
           
           case None => päivitäAinelista()
         }
         
        }
        
        case "Tiheyslaskuri"    => { // Tämä avaa tiheyslaskurin, jolla voidaan helpommin selvittää aineen tiheys
          
          // Pyydetään syöte käyttäjältä
          val syöte = Dialog.showInput(aineikkuna, "Laske tiheys antamalla aineen massa ja tilavuus. Tiheys palautetaan ohjelman käyttämässä yksikössä g/ml.", "Tiheyslaskuri",
                              initial = "[massan arvo] [massayksikkö] [tilavuuden arvo] [tilavuusyksikkö]")
         syöte match {
            
           case Some(komento) => {
             
             val tiheys = UI.tiheyslaskuri(komento)
             
             if (tiheys > 0.0) {
               Dialog.showMessage(pääikkuna, "Laskettu tiheys: " + tiheys + " g/ml")
             }
                              
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
  val hakuSelitys = new Label("Hae lista aineista, jotka täyttävät haluamasi hakukriteerit. Kentät ovat:")
  
  val aineenNimi           = new TextField("Aineen nimi")               // Tämä kenttä täytetään, jos halutaan tietyn niminen aine
  val allergeeniSuodatin   = new TextField("Suodatettavat allergeenit") // Tähän kenttään täytetään vältettävät allergeenit, erotettuna pilkulla
  val maxPuuttuvatAineet   = new TextField("Max puuttuvat aineet")      // Tähän kenttään määritellään kuinka monta ainesosaa saa puuttua varastosta
  
  /*
   *  Tätä nappia painettaessa ohjelma yrittää hakea ainelistan annetuilla parametreilla. Tulokset avataan 
   *  hakutulosikkunaan.
   */
  val ainehakunappi        = new Button("Hae aineita") 
  val hakuperuutus         = new Button("Peruuta")
  
  
  // Paneeli, jossa ovat hakukentät
  val hakuTekstit          = new BoxPanel(Orientation.Vertical)
  hakuTekstit.contents    += hakuSelitys
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
  hakuikkuna.centerOnScreen()
  
  // TAPAHTUMAT:
  hakuikkuna.listenTo(ainehakunappi)
  hakuikkuna.listenTo(hakuperuutus)
  hakuikkuna.reactions += {
    case painallus: ButtonClicked => 
      val nappi = painallus.source
      nappi.text match {
        case "Hae aineita" => { // Tämä nappi kutsuu UI:n metodia täyttääkseen hakutulosikkunan tulostaulukon.
          try {
            
            //hakutulokset = UI.haeAineetTaulukkoon(aineenNimi.text, allergeeniSuodatin.text, maxPuuttuvatAineet.text)   /// TODO: !!!! KORJAA HAKUTOIMINTO !!!!!
            
            // Ensin haetaan annetuilla hakukriteereillä. Saadut tulokset tallennetaan hakutulokset-muuttujaan. (Hakutaulukon tiedot)
            hakutulokset = UI.haeAineet(aineenNimi.text, allergeeniSuodatin.text, maxPuuttuvatAineet.text)
            
            // Jos haku ei löytänyt aineita, ilmestyy dialogi, joka ilmoittaa siitä käyttäjälle.
            if (hakutulokset.isEmpty) Dialog.showMessage(hakuikkuna, "Hakusi ei tuottanut tuloksia") 
            
            // Muussa tapauksessa avataan hakutulosikkuna löydetyillä tiedoilla.
            else {
              päivitäHakutulokset
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
  
  /*val hakusarakkeet: Seq[String] = Seq("Aine", "määrä")
  var hakutulokset: Array[Array[Any]] = Array()
  var hakutaulukko = new Table(hakutulokset, hakusarakkeet)
  * 
  */
  var hakutulokset = ""
  val hakutaulukko = new TextArea(hakutulokset)
  hakutaulukko.editable = false
  
  val hakutulosSulje = Button("Sulje") {hakutulosIkkuna.close()}
  
  val hakutulosPaneeli = new BoxPanel(Orientation.Vertical)
  hakutulosPaneeli.contents += hakutaulukko
  hakutulosPaneeli.contents += hakutulosSulje
  
  val hakutulosIkkuna = new Frame
  hakutulosIkkuna.title    = "Hakutulokset"
  hakutulosIkkuna.contents = hakutulosPaneeli
  hakutulosIkkuna.minimumSize = new Dimension(400, 200)
  hakutulosIkkuna.centerOnScreen()
  
  def päivitäHakutulokset = {    
    hakutaulukko.text = hakutulokset
    hakutaulukko.repaint()
    hakutulosIkkuna.repaint()
  }
  
  
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
  val uusiNimi     = new TextField("Aineen nimi")
  val allergeenit  = new TextField("Allergeenit")
  val määräJaMitta = new TextField("tiheys, määrä, mittayksikkö") // tiheys, määrä ja mitta erotetaan pilkulla (esim. " 0.0,5.0,"kpl")
  val uusiKuvaus   = new TextField("Aineen kuvaus")
  
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
  reseptiIkkuna.centerOnScreen()
  
  // TAPAHTUMAT:
  
  reseptiIkkuna.listenTo(resTallenna)
  reseptiIkkuna.listenTo(resPeruuta)
  reseptiIkkuna.reactions += {
    case painallus: ButtonClicked => 
      val nappi = painallus.source
      nappi.text match {
        
        case "Tallenna" => { // Ohjelma yrittää tallentaa annetuilla tiedoilla uuden aineen.
          try {
            require(uusiNimi.text.length > 0) // Aineella tulee olla vähintään nimi.
            
            UI.luoAine(uusiNimi.text, allergeenit.text, uusiKuvaus.text, määräJaMitta.text) // Luodaan uusi aine annetuilla parametreilla
            
            // Jos aineen luonti onnistuu, päivitetään ainelista ja suljetaan ikkuna.
            päivitäAinelista() 
            reseptiIkkuna.close()
            
          } catch {
            case e: IllegalArgumentException => Dialog.showMessage(reseptiIkkuna, "Aineella tulee olla vähintään nimi.")
          }
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
  varIkkuna.centerOnScreen()
  
  
  
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
            case Some(komento) => muutettiin = UI.muutaMäärää(komento); päivitäAinelista() // Jos annettiin komento, yritetään suorittaa muutos
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
            case Some(komento) => muutettiin = UI.muutaYksikkö(komento); päivitäAinelista() // Jos annettiin komento, yritetään suorittaa muutos
            case None          => 
          }
          
          if (muutettiin) Dialog.showMessage(varIkkuna, "Tiedot muutettiin onnistuneesti.") // Ilmoitetaan jos muutos onnistui
          
          päivitäAinelista()
        }
        
        case "Poista aine varastosta"      => {// tästä käytetään metodia UI.poistaAine
          
          var muutettiin: Boolean = false
          val syöte = Dialog.showInput(varIkkuna, "Anna poistettavan aineen nimi",
                           initial = "[aineen nimi]")
                           
          syöte match {
            case Some(komento) => muutettiin = UI.poistaAine(komento); päivitäAinelista() // Jos annettiin komento, yritetään suorittaa muutos
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
            case Some(komento) => muutettiin = UI.tyhjennys(komento); päivitäAinelista() // Jos annettiin komento, yritetään suorittaa muutos
            case None          => 
          }
          
          if (muutettiin) Dialog.showMessage(varIkkuna, "Tiedot muutettiin onnistuneesti.") // Ilmoitetaan jos muutos onnistui
          
          päivitäAinelista()
          
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
  var ainetiedot: String = ""
  val ominaisuuslista = new TextArea()
  ominaisuuslista.text = ainetiedot
  ominaisuuslista.editable = false
  ominaisuuslista.minimumSize = (new Dimension(200, 300))
  
  /*var ainetiedot: Array[Array[Any]] = Array(Array("Tyhjää täynnä", 0.0))
  val aineSarakenimet: Seq[String]  = Seq("Ominaisuus", "Arvo")
  var ominaisuuslista               = new Table(ainetiedot, aineSarakenimet)
  * 
  */
  
  
  val aineAinesosa    = new Button("Hallitse ainesosia")
  val aineAllergeeni  = new Button("Hallitse allergeeneja")
  val aineOminaisuus  = new Button("Hallitse muita ominaisuuksia")
  val aineRaakaAineet = new Button("Aineen raaka-aineet")
  val näytäKuvaus     = new Button("Näytä kuvaus")
  
  // Komponenttien asemointi
  val aineNapit         = new BoxPanel(Orientation.Vertical)
  aineNapit.contents   += aineAinesosa
  aineNapit.contents   += aineAllergeeni
  aineNapit.contents   += aineOminaisuus
  aineNapit.contents   += aineRaakaAineet
  aineNapit.contents   += näytäKuvaus
  
  val ainePaneeli       = new BoxPanel(Orientation.Horizontal)
  ainePaneeli.contents += ominaisuuslista
  ainePaneeli.contents += aineNapit
  
  val aineikkuna        = new Frame
  aineikkuna.contents   = ainePaneeli
  aineikkuna.minimumSize = (new Dimension(600, 400))
  aineikkuna.centerOnScreen()
  
  
  // Tämä metodi päivittää aineen ominaisuuslistan.
  def päivitäOminaisuudet() = {
    ainetiedot = aine.tietotaulukkoTekstinä
    ominaisuuslista.text = ainetiedot
    ominaisuuslista.repaint()
    aineikkuna.repaint()
  }
  
  /**
   * TAPAHTUMAT:
   */
  
  aineikkuna.listenTo(aineAinesosa)
  aineikkuna.listenTo(aineAllergeeni)
  aineikkuna.listenTo(aineOminaisuus)
  aineikkuna.listenTo(aineRaakaAineet)
  aineikkuna.listenTo(näytäKuvaus)
  aineikkuna.reactions += {
    
    case painallus: ButtonClicked =>
      val nappi = painallus.source
      päivitäOminaisuudet()
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
          
          päivitäOminaisuudet
          päivitäAinelista
          
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
          
          päivitäOminaisuudet
          päivitäAinelista
          
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
          
          päivitäOminaisuudet
          päivitäAinelista
        }
        
        case "Aineen raaka-aineet" => Dialog.showMessage(aineikkuna, UI.listaaRaakaAineet(aine)) // Näytetään dialogi, jossa listattuna kaikki ainesosat ja raaka-aineet
        
        case "Näytä kuvaus" => Dialog.showMessage(aineikkuna, aine.kuvaus)
      }
    
  }
  



  
}