package json

import json.Json._
import json.PathElement.{Field, Index}
import optics.poly.{Iso, Optional}
import optics.poly.functions.{Index => FIndex}

import scala.language.dynamics

case class JsonPath(path: List[PathElement], json: Optional[Json, Json]) extends Dynamic {

  val string: Optional[Json, String] =
    json.andThen(jsonString)

  val int: Optional[Json, Int] =
    json.andThen(jsonInt)

  def selectDynamic(field: String): JsonPath = {
    val newPath = Field(field) :: path
    JsonPath(
      newPath,
      json.andThen(jsonObject).index(field)
    )
  }

  def index(key: Int): JsonPath = {
    val newPath = Index(key) :: path
    JsonPath(
      newPath,
      json.andThen(jsonArray).index(key)
    )
  }
}

object JsonPath {
  val root: JsonPath = JsonPath(Nil, Iso.id[Json].asOptional)
}
