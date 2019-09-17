object SpecialMath extends App{

  private var lookup: Map [Int,BigInt] = Map(0 -> 0, 1 ->1)
  private val goal:Int = args(0).toInt

  for (i <- 1 to goal){
    if (!lookup.contains(i)){
      lookup += (i -> specialMath(i))
    }
  }
  println(lookup.apply(goal))

  private def specialMath(n:Int): BigInt = {
    n + lookup.apply(n-1) + lookup.apply(n-2)
  }

}
