object HexToBase64 extends App{
  private val hexDict: Map [Char,String] = Map('0' -> "0000",'1' -> "0001",'2' -> "0010",'3' -> "0011",'4' -> "0100", '5' -> "0101", '6' -> "0110",'7' -> "0111",'8' -> "1000",'9' -> "1001",'a' -> "1010",'b' -> "1011",'c' -> "1100", 'd' -> "1101",'e' -> "1110", 'f' -> "1111")
  private val hbase64Dict: Map [String,Char] = Map("000000" -> 'A', "000001" -> 'B', "000010" -> 'C', "000011" -> 'D', "000100" -> 'E', "000101" -> 'F', "000110" -> 'G', "000111" -> 'H', "001000" -> 'I', "001001" -> 'J', "001010" -> 'K', "001011" -> 'L', "001100" -> 'M', "001101" -> 'N', "001110" -> 'O', "001111" -> 'P', "010000" -> 'Q', "010001" -> 'R', "010010" -> 'S', "010011" -> 'T', "010100" -> 'U', "010101" -> 'V', "010110" -> 'W', "010111" -> 'X', "011000" -> 'Y', "011001" -> 'Z', "011010" -> 'a', "011011" -> 'b', "011100" -> 'c', "011101" -> 'd', "011110" -> 'e', "011111" -> 'f', "100000" -> 'g', "100001" -> 'h', "100010" -> 'i', "100011" -> 'j', "100100" -> 'k', "100101" -> 'l', "100110" -> 'm', "100111" -> 'n', "101000" -> 'o', "101001" -> 'p', "101010" -> 'q', "101011" -> 'r', "101100" -> 's', "101101" -> 't', "101110" -> 'u', "101111" -> 'v', "110000" -> 'w', "110001" -> 'x', "110010" -> 'y', "110011" -> 'z', "110100" -> '0', "110101" -> '1', "110110" -> '2', "110111" -> '3', "111000" -> '4', "111001" -> '5', "111010" -> '6', "111011" -> '7', "111100" -> '8', "111101" -> '9', "111110" -> '+', "111111" -> '/')

  test()
  println(convertHexToBase64("45766964696e74"))


  def convertHexToBase64(hex: String): String ={
    var base64Binary:List[String] = convertHexToBinary(hex).grouped(6).toList
    base64Binary = base64Binary.dropRight(1) // to remove unnecessary trailing 0's we added
    convertBinaryToBase64(base64Binary)+padding(base64Binary.length)
  }

  private def convertHexToBinary(hex:String): String = {
    var binary = new String("")
    val hexArray: List[Char] = hex.toList

    for (char <- hexArray) {
      binary = binary + hexDict.apply(char).toString
    }

   binary = binary + "000000" // to deal with trailing spaces

   binary
  }

  private def convertBinaryToBase64(base64BinaryArray:List[String]): String = {
    var base64 = new String("")

    for (base64Binary <- base64BinaryArray) {
      base64 = base64 + hbase64Dict.apply(base64Binary).toString
    }

    base64
  }

  private def padding(num: Int):String = {
    var padding: String = ""

    if (num % 4 == 2) {
    padding = "=="
    }

    if (num%4 == 3) {
      padding = "="
    }

    padding
  }

  private def test(): Unit ={
    assert(convertHexToBase64("45766964696e74").equals("RXZpZGludA=="))
    assert(convertHexToBase64("324768508796103489").equals("MkdoUIeWEDSJ"))
    assert(convertHexToBase64("87de9a987e8ef98a94").equals("h96amH6O+YqU"))
    assert(convertHexToBase64("ef879d7e56fe53d456f89e").equals("74edflb+U9RW+J4="))
    assert(convertHexToBase64("809765423576890f8ee6d5789b98976a8c94").equals("gJdlQjV2iQ+O5tV4m5iXaoyU"))
  }
}





