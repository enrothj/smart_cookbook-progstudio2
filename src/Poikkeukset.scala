package Smart_Cookbook

import java.lang.Exception

case class VirheellinenData(kuvaus: String, virheData: String) extends Exception(kuvaus) {}

case class VirheellinenMittayksikkö(kuvaus: String, virheData: String) extends Exception(kuvaus) {}

// Tämä poikkeus tapahtuu, kun yritetään kutsua Muuntajan muunnosmetodeja kappaleille (mittayksikkö = "kpl")
case class KappaleMuunnos(kuvaus: String = "Yritettiin muuntaa kappalemuotoista ainetta", virheData: String) extends Exception(kuvaus) {}