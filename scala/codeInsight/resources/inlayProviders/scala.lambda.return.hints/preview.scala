val lengths = Seq("a", "bb").map { word =>
  val trimmed = word.trim
  trimmed.length/*<# ^map #>*/
}

val sizes = Seq(1, 2).collect { case n if n > 0 =>
  val doubled = n * 2
  doubled/*<# ^collect #>*/
}
