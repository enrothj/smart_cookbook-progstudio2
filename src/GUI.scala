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
  
  // TODO: lista ohjelman aineista jne.  val ainelista = jokinlistakomponentti
  
  // Komponenttien asemointi
  val napit = new BoxPanel(Orientation.Horizontal)
  napit.contents += reseptihaku
  napit.contents += luoResepti
  napit.contents += varHallinta
  
  val pääikkuna = new MainFrame
  pääikkuna.contents = napit
  pääikkuna.title    = "Älykäs reseptikirja"
  pääikkuna.size     = new Dimension(400, 400)
  
  
  
  
}