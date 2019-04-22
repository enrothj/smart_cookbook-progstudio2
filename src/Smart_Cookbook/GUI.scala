package Smart_Cookbook

import scala.swing._
import scala.swing.event._
import scala.collection.mutable.Buffer
import scala.collection.Seq






object GUI extends SimpleSwingApplication {
  
  // ensin taytetaan ohjelman tiedot tekstitiedostoilta
  UI.taytaVarasto()
  

  
  /*
   * Paaikkunassa on kolme nappia: Reseptihaku, Luo Resepti ja Varaston hallinta. Naita painamalla avataan ikkunoita, joilla voi toteuttaa ko. toimintoja.
   * Nappien alla on lista ohjelman aineista, niiden maarasta varastossa ja niiden allergeeneista.
   */
  
  // Paakomponentit:
  
  // Nailla napeilla avataan kolme muuta ikkunaa.
  val reseptihakuNappi = new Button("Reseptihaku")
  val luoReseptiNappi  = new Button("Luo Resepti")
  val varHallintaNappi = new Button("Varaston Hallinta")
  val sulkunappi       = new Button("Sulje Ohjelma")
  val avaaAineNappi    = new Button("Avaa aine")
  val tiheyslaskuri    = new Button("Tiheyslaskuri")
  
  // Lista aineista, niiden maarasta ja niiden allergeeneista
  val ainelista = new TextArea()
  ainelista.text = UI.listaaVarasto
  ainelista.editable = false
  

  
  /*val sarakenimet                         = Seq("Aine", "Maara", "Allergeenit")
  var ainelistaTiedot: Array[Array[Any]]  = UI.ainelista
  val ainelista                           = new Table(ainelistaTiedot, sarakenimet)
  */
  
  // Komponenttien asemointi
  val paanapit = new BoxPanel(Orientation.Horizontal)
  paanapit.contents += reseptihakuNappi
  paanapit.contents += luoReseptiNappi
  paanapit.contents += varHallintaNappi
  paanapit.contents += avaaAineNappi
  paanapit.contents += tiheyslaskuri
  paanapit.contents += sulkunappi
  
  val paaaineet = new BoxPanel(Orientation.Vertical)
  paaaineet.contents += ainelista
  
  val napitJaLista = new BoxPanel(Orientation.Vertical)
  napitJaLista.contents += paanapit
  napitJaLista.contents += paaaineet
  
  val paaikkuna = new MainFrame
  paaikkuna.contents = napitJaLista
  paaikkuna.title    = "alykas reseptikirja"
  paaikkuna.minimumSize     = new Dimension(700, 400)
  paaikkuna.centerOnScreen()
  
  def top = paaikkuna
  
  def paivitaAinelista() = ainelista.text = UI.listaaVarasto; ainelista.repaint(); paaikkuna.repaint()

  // TAPAHTUMAT:
  
  paaikkuna.listenTo(reseptihakuNappi)
  paaikkuna.listenTo(luoReseptiNappi)
  paaikkuna.listenTo(varHallintaNappi)
  paaikkuna.listenTo(sulkunappi)
  paaikkuna.listenTo(avaaAineNappi)
  paaikkuna.listenTo(tiheyslaskuri)
  
  paaikkuna.reactions += {
    case painallus: ButtonClicked => 
      val nappi = painallus.source
      nappi.text match {
        case "Reseptihaku"       => hakuikkuna.open()
        case "Luo Resepti"       => reseptiIkkuna.open()
        case "Varaston Hallinta" => varIkkuna.open()
        
        // "Sulje Ohjelma"-nappi tallentaa ohjelman tiedot ja sulkee ohjelman.
        case "Sulje Ohjelma"     => UI.tallennaTiedot(); GUI.quit()
        
        case "Avaa aine"         => { // Tata nappia painamalla aukeaa syotedialogi, josta voidaan avata halutun aineen ikkuna.
         val syote = Dialog.showInput(paaikkuna, "Syota aineen nimi.", initial = "")
         syote match {
           case Some(nimi) => {
             try {
               aine = UI.avaaAine(nimi)
               paivitaOminaisuudet()
               aineikkuna.open()
             } catch {
               case e: OlematonAinePoikkeus => Dialog.showMessage(aineikkuna, e.kuvaus)
               
             }
           }
           
           case None => paivitaAinelista()
         }
         
        }
        
        case "Tiheyslaskuri"    => { // Tama avaa tiheyslaskurin, jolla voidaan helpommin selvittaa aineen tiheys
          
          // Pyydetaan syote kayttajalta
          val syote = Dialog.showInput(aineikkuna, "Laske tiheys antamalla aineen massa ja tilavuus. Tiheys palautetaan ohjelman kayttamassa yksikossa g/ml.", "Tiheyslaskuri",
                              initial = "[massan arvo] [massayksikko] [tilavuuden arvo] [tilavuusyksikko]")
         syote match {
            
           case Some(komento) => {
             
             val tiheys = UI.tiheyslaskuri(komento)
             
             if (tiheys > 0.0) {
               Dialog.showMessage(paaikkuna, "Laskettu tiheys: " + tiheys + " g/ml")
             }
                              
           }
           case None => 
          
          }
        }
        
        case _                   => println("Painoit nappia " + nappi.text)
      }
    
  }
  
  /** RESEPTIHAKU-IKKUNA
   * Reseptihaussa voidaan hakea aineita tekstina annetuilla kriteereilla. Voidaan antaa haluttuja kriteereja, 
   * kuten jonkin aineen nimi, tai valtettavia kriteereja, kuten allergeenien nimia. Haussa voidaan myos maaritella
   * halutaanko etsia ainoastaan aineita, joita on jo varastossa, tai voidaan muodostaa varaston sisaltamista 
   * aineista, vaiko kaikista ohjelman tuntemista aineista.
   * 
   * Ikkunassa on kolme tekstikenttaa: haku nimen mukaan, valtettavat allergeenit ja kuinka monta ainesosaa saa
   * puuttua varastosta aineen valmistamiseksi. Ikkunan alaosassa on kaksi nappia: "Hae aineita" ja "Peruuta".
   */
  
  // Paakomponentit:
  
  val hakuohje = new TextPane{
    text = "Tässä ikkunassa voit hakea listan aineista, jotka täyttävät haluamasi hakukriteerit.\n" +
            "Jos haluat hakea tietyn nimisen aineen, kirjoita nimikenttään. Jos haluat poistaa\n" +
            "hakutuloksista aineet, jotka sisältävät tiettyjä allergeenejä, kirjoita ne toiseen \n" +
            "kenttään, pilkuilla erotettuna (esim. \"maito,vehnä\" ). Kolmanteen kenttään voit\n" +
            "kirjoittaa kuinka monta ainetta saa puuttua löydetyistä resepteistä. Kirjoita 0, \n" +
            "jos haluat, että aineen voi valmistaa varastossa jo olevista aineksista."
    editable = false
  }
  val hakuNimi = new TextPane{text="Aineen nimi"; editable=false}
  val hakuSuodatin = new TextPane{text="Suodatettavat allergeenit"; editable=false}
  val hakuMaxPuuttuvat = new TextPane{text="Puuttuvien aineiden enimmäismäärä"; editable=false}
  
  val aineenNimi           = new TextField      // Tama kentta taytetaan, jos halutaan tietyn niminen aine
  val allergeeniSuodatin   = new TextField      // Tahan kenttaan taytetaan valtettavat allergeenit, erotettuna pilkulla
  val maxPuuttuvatAineet   = new TextField      // Tahan kenttaan maaritellaan kuinka monta ainesosaa saa puuttua varastosta
  
  /*
   *  Tata nappia painettaessa ohjelma yrittaa hakea ainelistan annetuilla parametreilla. Tulokset avataan 
   *  hakutulosikkunaan.
   */
  val ainehakunappi        = new Button("Hae aineita") 
  val hakuperuutus         = new Button("Peruuta")
  
  
  // Paneeli, jossa ovat hakukentat
  val hakuTekstit          = new BoxPanel(Orientation.Vertical)
  hakuTekstit.contents    += hakuohje
  hakuTekstit.contents    += hakuNimi
  hakuTekstit.contents    += aineenNimi
  hakuTekstit.contents    += hakuSuodatin
  hakuTekstit.contents    += allergeeniSuodatin
  hakuTekstit.contents    += hakuMaxPuuttuvat
  hakuTekstit.contents    += maxPuuttuvatAineet  // Tama on kaytannossa ainoa, johon voi antaa virheellisen syotteen.
  
  // Paneeli, jossa ovat hakunapit
  val hakuNapit           = new BoxPanel(Orientation.Horizontal)
  hakuNapit.contents     += ainehakunappi
  hakuNapit.contents     += hakuperuutus
  
  // Naiden paneelien yhdistys
  val hakuToiminnot       = new BoxPanel(Orientation.Vertical)
  hakuToiminnot.contents += hakuTekstit
  hakuToiminnot.contents += hakuNapit
  
  // Hakuikkuna
  val hakuikkuna        = new Frame{title="Hakuikkuna"}
  hakuikkuna.contents   = hakuToiminnot
  hakuikkuna.centerOnScreen()
  
  // TAPAHTUMAT:
  hakuikkuna.listenTo(ainehakunappi)
  hakuikkuna.listenTo(hakuperuutus)
  hakuikkuna.reactions += {
    case painallus: ButtonClicked => 
      val nappi = painallus.source
      nappi.text match {
        case "Hae aineita" => { // Tama nappi kutsuu UI:n metodia tayttaakseen hakutulosikkunan tulostaulukon.
          try {
            
            //hakutulokset = UI.haeAineetTaulukkoon(aineenNimi.text, allergeeniSuodatin.text, maxPuuttuvatAineet.text)   /// TODO: !!!! KORJAA HAKUTOIMINTO !!!!!
            
            // Ensin haetaan annetuilla hakukriteereilla. Saadut tulokset tallennetaan hakutulokset-muuttujaan. (Hakutaulukon tiedot)
            hakutulokset = UI.haeAineet(aineenNimi.text, allergeeniSuodatin.text, maxPuuttuvatAineet.text)
            
            // Jos haku ei loytanyt aineita, ilmestyy dialogi, joka ilmoittaa siita kayttajalle.
            if (hakutulokset.isEmpty) Dialog.showMessage(hakuikkuna, "Hakusi ei tuottanut tuloksia") 
            
            // Muussa tapauksessa avataan hakutulosikkuna loydetyilla tiedoilla.
            else {
              paivitaHakutulokset
              hakutulosIkkuna.open()
            }
            
          } catch {
            case e: IllegalArgumentException => Dialog.showMessage(hakuikkuna, e.toString()) // Dialogi kertoo, etta annettu parametri ei ole numero.
          }

        }
        case "Peruuta"     => hakuikkuna.close()
        case _             => println("Painoit nappia " + _)
      }
  }
  
  
  // HAKUTULOS-IKKUNA
  
  /*val hakusarakkeet: Seq[String] = Seq("Aine", "maara")
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
  
  def paivitaHakutulokset = {    
    hakutaulukko.text = hakutulokset
    hakutaulukko.repaint()
    hakutulosIkkuna.repaint()
  }
  
  
  /** RESEPTINLUONTI-IKKUNA
   * Reseptinluonti-ikkunassa voidaan luoda uusia Aine-olioita ohjelman muistiin. Ikkunassa maaritellaan tekstikenttiin
   * aineen nimi, allergeenit, tiheys, maara ja mittayksikko, seka kuvaus (valmistusohje jne.). Raaka-aineet lisataan
   * erikseen Aine-ikkunasta.
   * 
   * Ainoa pakollinen parametri on nimi aineelle
   * 
   * Ikkunan ala-laidassa on kaksi nappia: "Tallenna" ja "Peruuta". Tallenna-napilla tallennetaan uusi aine
   * annetuilla tiedoilla. Peruuta-nappi sulkee ikkunan.
   */
  
  // Paakomponentit:
  val nimiSelite   = new TextPane{text= "Aineen nimi"; editable = false}
  val uusiNimi     = new TextField()
  val allSelite    = new TextPane{text= "Allergeenit"; editable = false}
  val allergeenit  = new TextField()
  val mittaSelite  = new TextPane{
                       text= {
                         "Tiheys, määrä, mittayksikkö, jotka erotetaan pilkulla (esim.  0.0,5.0,\"kpl\"  )\n"+ 
                         "Sallitut painomitat ovat g, kg, lb, oz ja kpl.\nSallitut tilavuudet ovat ml, dl, l, tl, rkl, cup ja pint."
                          }
                        editable = false
                     }
  val maaraJaMitta = new TextField() // tiheys, maara ja mitta erotetaan pilkulla (esim. " 0.0,5.0,"kpl")
  val kuvausSelite = new TextPane{text= "Aineen kuvaus"; editable = false}
  val uusiKuvaus   = new TextField()
  
  val resTallenna  = new Button("Tallenna")
  val resPeruuta   = new Button("Peruuta")
  
  // Paneeli tekstikentille
  val resTekstit       = new BoxPanel(Orientation.Vertical)
  resTekstit.contents += nimiSelite
  resTekstit.contents += uusiNimi
  resTekstit.contents += allSelite
  resTekstit.contents += allergeenit
  resTekstit.contents += mittaSelite
  resTekstit.contents += maaraJaMitta
  resTekstit.contents += kuvausSelite
  resTekstit.contents += uusiKuvaus
  
  // Paneeli napeille
  val resNapit           = new BoxPanel(Orientation.Horizontal)
  resNapit.contents     += resTallenna
  resNapit.contents     += resPeruuta
  
  // Naiden paneelien yhdistys
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
        
        case "Tallenna" => { // Ohjelma yrittaa tallentaa annetuilla tiedoilla uuden aineen.
          try {
            require(uusiNimi.text.length > 0) // Aineella tulee olla vahintaan nimi.
            
            UI.luoAine(uusiNimi.text, allergeenit.text, uusiKuvaus.text, maaraJaMitta.text) // Luodaan uusi aine annetuilla parametreilla
            
            // Jos aineen luonti onnistuu, paivitetaan ainelista ja suljetaan ikkuna.
            paivitaAinelista() 
            reseptiIkkuna.close()
            
          } catch {
            case e: IllegalArgumentException => Dialog.showMessage(reseptiIkkuna, "Aineella tulee olla vahintaan nimi.")
          }
        }
        
        case "Peruuta"  => reseptiIkkuna.close()
        case _ => println("Painoit nappia " + nappi.text)
      }
  }
  
  
  /** VARASTONHALLINTA-IKKUNA
   * Varastonhallintaikkunassa on kourallinen nappeja, joista aukeaa ponnahdusikkunoita, joihin voi antaa syotteita. Napit
   * ovat aineen maaran muuttamiseen, yksikon muuttamiseen, aineen poistamiseen varastosta seka varaston tyhjentamiseen tai
   * nollaamiseen.
   */
  
  // Paakomponentit
  val varMaara   = new Button("Muuta aineen maaraa")
  val varYksikko = new Button("Muuta aineen mittayksikkoa")
  val varPoista  = new Button("Poista aine varastosta")
  val varNollaa  = new Button("Tyhjenna tai nollaa varasto")
  
  
  // Komponenttien asemointi
  val varPaneeli       = new BoxPanel(Orientation.Vertical)
  varPaneeli.contents += varMaara
  varPaneeli.contents += varYksikko
  varPaneeli.contents += varPoista
  varPaneeli.contents += varNollaa
  
  val varIkkuna        = new Frame
  varIkkuna.contents   = varPaneeli
  varIkkuna.centerOnScreen()
  
  
  
  // TAPAHTUMAT: 
  
  varIkkuna.listenTo(varMaara)
  varIkkuna.listenTo(varYksikko)
  varIkkuna.listenTo(varPoista)
  varIkkuna.listenTo(varNollaa)
  
  varIkkuna.reactions += {
    case painallus: ButtonClicked =>
      val nappi = painallus.source
      
      nappi.text match {
        
        case "Muuta aineen maaraa"         => { // Tasta kaytetaan metodia UI.muutaMaaraa
          
          var muutettiin: Boolean = false  // Tahan muuttujaan tallennetaan tieto onnistuneesta muutoksesta.
          val syote = Dialog.showInput(varIkkuna, "Syota haluamasi komento: \n+ lisaa\n- vahentaa\n= asettaa maaran",
                           initial = "[aineen nimi] [+/-/=] [haluttu maara]")
          syote match {
            case Some(komento) => muutettiin = UI.muutaMaaraa(komento); paivitaAinelista() // Jos annettiin komento, yritetaan suorittaa muutos
            case None          => 
          }
          
          if (muutettiin) Dialog.showMessage(varIkkuna, "Tiedot muutettiin onnistuneesti.") // Ilmoitetaan jos muutos onnistui
        }
        
        case "Muuta aineen mittayksikkoa"  => { // tasta kaytetaan metodia UI.muutaYksikko
          
          var muutettiin: Boolean = false
          val syote = Dialog.showInput(varIkkuna, "Anna aineen nimi ja haluamasi mittayksikko (lyhennettyna).\n" +
                                                 "HUOM: Kappalemuotoon voi muuttaa vain aineen omista tiedoista",
                           initial = "[aineen nimi] [haluttu mittayksikko]")
                           
          syote match {
            case Some(komento) => muutettiin = UI.muutaYksikko(komento); paivitaAinelista() // Jos annettiin komento, yritetaan suorittaa muutos
            case None          => 
          }
          
          if (muutettiin) Dialog.showMessage(varIkkuna, "Tiedot muutettiin onnistuneesti.") // Ilmoitetaan jos muutos onnistui
          
          paivitaAinelista()
        }
        
        case "Poista aine varastosta"      => {// tasta kaytetaan metodia UI.poistaAine
          
          var muutettiin: Boolean = false
          val syote = Dialog.showInput(varIkkuna, "Anna poistettavan aineen nimi",
                           initial = "[aineen nimi]")
                           
          syote match {
            case Some(komento) => muutettiin = UI.poistaAine(komento); paivitaAinelista() // Jos annettiin komento, yritetaan suorittaa muutos
            case None          => 
          }
          
          if (muutettiin) Dialog.showMessage(varIkkuna, "Tiedot muutettiin onnistuneesti.") // Ilmoitetaan jos muutos onnistui
          
        }
          
        case "Tyhjenna tai nollaa varasto" => { // Tasta voi nollata tai tyhjentaa Varaston.
          
          var muutettiin: Boolean = false
          val syote = Dialog.showInput(varIkkuna, "Kirjoita 'nollaa' tai 'tyhjenna'.\n"
                                       + "Nollaa asettaa jokaisen aineen maaraksi 0.0\n"
                                       + "Tyhjenna poistaa kaikki aineet ohjelman Varastosta",
                           initial = "HUOM: Tyhjentaminen ei poista olemassaolevia tekstitiedostoja.")
                           
          syote match {
            case Some(komento) => muutettiin = UI.tyhjennys(komento); paivitaAinelista() // Jos annettiin komento, yritetaan suorittaa muutos
            case None          => 
          }
          
          if (muutettiin) Dialog.showMessage(varIkkuna, "Tiedot muutettiin onnistuneesti.") // Ilmoitetaan jos muutos onnistui
          
          paivitaAinelista()
          
        }
        
        case _ => println("Painoit nappia " + _)
      }
    
    
  }
  
  /** AINE-IKKUNA
   * Aine-ikkuna aukeaa, kun valitaan jokin aine jostakin ikkunasta. Aine-ikkunassa voidaan katsastaa ja hallita Aine-olioiden 
   * ominaisuuksia. Ikkunassa on taulukko, jossa on listattuna aineen ominaisuuksia ja niiden arvoja.
   * 
   * Ominaisuustaulukon lisaksi ikkunassa on napit ainesosien, allergeenien, ja muiden ominaisuuksien hallitsemiseen.
   * "Raaka-aineet"-napilla ohjelma antaa listan aineen raaka-aineista.
   * 
   */
  
  // Paakomponentit
  var aine: Aine = null
  var ainetiedot: String = ""
  val ominaisuuslista = new TextArea()
  ominaisuuslista.text = ainetiedot
  ominaisuuslista.editable = false
  ominaisuuslista.minimumSize = (new Dimension(200, 300))
  
  /*var ainetiedot: Array[Array[Any]] = Array(Array("Tyhjaa taynna", 0.0))
  val aineSarakenimet: Seq[String]  = Seq("Ominaisuus", "Arvo")
  var ominaisuuslista               = new Table(ainetiedot, aineSarakenimet)
  * 
  */
  
  
  val aineAinesosa    = new Button("Hallitse ainesosia")
  val aineAllergeeni  = new Button("Hallitse allergeeneja")
  val aineOminaisuus  = new Button("Hallitse muita ominaisuuksia")
  val aineRaakaAineet = new Button("Aineen raaka-aineet")
  val naytaKuvaus     = new Button("Nayta kuvaus")
  
  // Komponenttien asemointi
  val aineNapit         = new BoxPanel(Orientation.Vertical)
  aineNapit.contents   += aineAinesosa
  aineNapit.contents   += aineAllergeeni
  aineNapit.contents   += aineOminaisuus
  aineNapit.contents   += aineRaakaAineet
  aineNapit.contents   += naytaKuvaus
  
  val ainePaneeli       = new BoxPanel(Orientation.Horizontal)
  ainePaneeli.contents += ominaisuuslista
  ainePaneeli.contents += aineNapit
  
  val aineikkuna        = new Frame
  aineikkuna.contents   = ainePaneeli
  aineikkuna.minimumSize = (new Dimension(600, 400))
  aineikkuna.centerOnScreen()
  
  
  // Tama metodi paivittaa aineen ominaisuuslistan.
  def paivitaOminaisuudet() = {
    ainetiedot = aine.tietotaulukkoTekstina
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
  aineikkuna.listenTo(naytaKuvaus)
  aineikkuna.reactions += {
    
    case painallus: ButtonClicked =>
      val nappi = painallus.source
      paivitaOminaisuudet()
      nappi.text match {
        
        case "Hallitse ainesosia" => { // Tasta kutsutaan metodia UI.hallitseAinesosia
          
          var muutettiin: Boolean = false
          val syote = Dialog.showInput(aineikkuna, "Tasta voi lisata (+), poistaa (-) tai muuttaa ainesosia (=). Syoteformaatit:\n"
                                       + "+ [ainesosan nimi] [maara] [mittayksikko]\n"
                                       + "- [ainesosan nimi]\n"
                                       + "= [ainesosan nimi] [maara] [mittayksikko]",
                           initial = "[+/-/=] [ainesosan nimi]  [maara] [mittayksikko]")
                           
          syote match {
            case Some(komento) => muutettiin = UI.hallitseAinesosia(aine.nimi + " " + komento) // HUOM: metodi tarvitsee aineen nimen, mutta kayttajan ei tarvitse syottaa sita.
            case None          => 
          }
          
          if (muutettiin) Dialog.showMessage(aineikkuna, "Tiedot muutettiin onnistuneesti.") // Ilmoitetaan jos muutos onnistui
          
          paivitaOminaisuudet
          paivitaAinelista
          
        }
        
        case "Hallitse allergeeneja" => {  // Tasta kutsutaan metodia UI.hallitseAllergeeneja
          
          var muutettiin: Boolean = false
          val syote = Dialog.showInput(aineikkuna, "Tasta voi lisata (+) tai poistaa (-) allergeeneja tai antaa uusi allergeenilista (=). Syoteformaatit:\n"
                                       + "+ [allergeenin nimi]\n"
                                       + "- [allergeenin nimi]\n"
                                       + "= [allergeeni1] [allergeeni2] [allergeeni3] [jne.]",
                           initial = "[+/-/=] [allergeenin nimi]")
                           
          syote match {
            case Some(komento) => muutettiin = UI.hallitseAllergeeneja(aine.nimi + " " + komento) // HUOM: metodi tarvitsee aineen nimen, mutta kayttajan ei tarvitse syottaa sita.
            case None          => 
          }
          
          if (muutettiin) Dialog.showMessage(aineikkuna, "Tiedot muutettiin onnistuneesti.") // Ilmoitetaan jos muutos onnistui
          
          paivitaOminaisuudet
          paivitaAinelista
          
        }
        
        case "Hallitse muita ominaisuuksia" => {
          
          var muutettiin: Boolean = false
          val syote = Dialog.showInput(aineikkuna, "Tasta voi muuttaa aineen ominaisuuksia. Syota muutettava ominaisuus ja sopiva uusi arvo.\n"
                                       + "Muutettavia ominaisuuksia ovat yksikko, tiheys (Double), maara (Double) ja kuvaus.",
                           initial = "[ominaisuus] [uusi arvo]")
                           
          syote match {
            case Some(komento) => muutettiin = UI.hallitseOminaisuuksia(aine.nimi + " " + komento) // HUOM: metodi tarvitsee aineen nimen, mutta kayttajan ei tarvitse syottaa sita.
            case None          => 
          }
          
          if (muutettiin) Dialog.showMessage(aineikkuna, "Tiedot muutettiin onnistuneesti.") // Ilmoitetaan jos muutos onnistui
          
          paivitaOminaisuudet
          paivitaAinelista
        }
        
        case "Aineen raaka-aineet" => Dialog.showMessage(aineikkuna, UI.listaaRaakaAineet(aine)) // Naytetaan dialogi, jossa listattuna kaikki ainesosat ja raaka-aineet
        
        case "Nayta kuvaus" => Dialog.showMessage(aineikkuna, aine.kuvaus)
      }
    
  }
  
  // Muut luokat ja metodit voivat kutsua tata metodia nayttaakseen kayttajalle viestin, etta jotain meni pieleen.
  // viesti-parametrissa kerrotaan mika meni pieleen, ja ikkuna-parametri maarittaa missa kayttoliittyman ikkunassa viesti naytetaan 
  def virheviesti(viesti: String, ikkuna: Window) = {
    Dialog.showMessage(ikkuna, viesti, "Virhe")
  }


  
}