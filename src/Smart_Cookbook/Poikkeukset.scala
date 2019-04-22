package Smart_Cookbook

/*
 * Tahan on luotu muutama poikkeusluokka, joita heitetaan erinaisissa virhetilanteissa.
 */

// Tama poikkeus tapahtuu, kun IO-olio yrittaa kasitella virheellista (korruptoitunutta) dataa.
case class VirheellinenData(kuvaus: String, virheData: String) extends Exception(kuvaus) {}

// Tama poikkeus tapahtuu, kun yritetaan syottaa Muuntajalle tuntemattomia mittayksikoita.
case class VirheellinenMittayksikko(kuvaus: String, virheData: String) extends Exception(kuvaus) {}

// Tama poikkeus tapahtuu, kun yritetaan kutsua Muuntajan muunnosmetodeja kappaleille (mittayksikko = "kpl")
case class KappaleMuunnos(kuvaus: String, virheData: String) extends Exception(kuvaus) {}

// Tama poikkeus tapahtuu, kun yritetaan kutsua Aine-oliota, joka ei ole olemassa ohjelman muistissa.
case class OlematonAinePoikkeus(kuvaus: String, virheData: String) extends Exception(kuvaus) {}

// Tama poikkeus tapahtuu, kun yritetaan kasitella ainesosaa, joka ei ole (enaa) ohjelman tiedossa.
case class OlematonAinesosa(kuvaus: String, virheData: String) extends Exception(kuvaus)