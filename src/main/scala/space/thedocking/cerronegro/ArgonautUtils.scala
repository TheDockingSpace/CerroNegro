package space.thedocking.cerronegro

import argonaut.Json

object ArgonautUtils {

  def jsonArrayToMap(jsonArray: Json): Map[String, Json] = {
    jsonArray.arrayOrEmpty.indices
      .map(_.toString)
      .zip(jsonArray.arrayOrEmpty)
      .toMap
  }

  def jsonObjectToMap(jsonObject: Json): Map[String, Json] = {
    val fieldsAndValues = for {
      field <- jsonObject.objectFieldsOrEmpty
      value <- jsonObject.field(field)
    } yield field -> value
    fieldsAndValues.toMap
  }
}
