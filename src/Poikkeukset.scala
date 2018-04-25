package Smart_Cookbook

import java.lang.Exception

case class VirheellinenData(kuvaus: String, virheData: String) extends Exception(kuvaus) {}

case class VirheellinenMittayksikkö(kuvaus: String, virheData: String) extends Exception(kuvaus) {}

