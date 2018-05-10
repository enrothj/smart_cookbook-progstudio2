package Smart_Cookbook

case class VirheellinenData(kuvaus: String, virheData: String) extends Exception(kuvaus) {}

case class VirheellinenMittayksikkö(kuvaus: String, virheData: String) extends Exception(kuvaus) {}

// Tämä poikkeus tapahtuu, kun yritetään kutsua Muuntajan muunnosmetodeja kappaleille (mittayksikkö = "kpl")
case class KappaleMuunnos(kuvaus: String, virheData: String) extends Exception(kuvaus) {}

// Tämä poikkeus tapahtuu, kun yritetään kutsua Aine-oliota, joka ei ole olemassa ohjelman muistissa.
case class OlematonAinePoikkeus(kuvaus: String, virheData: String) extends Exception(kuvaus) {}

// Tämä poikkeus tapahtuu, kun yritetään käsitellä ainesosaa, joka ei ole (enää) ohjelman tiedossa.
case class OlematonAinesosa(kuvaus: String, virheData: String) extends Exception(kuvaus)