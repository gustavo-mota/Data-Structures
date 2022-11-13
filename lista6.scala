import scala.collection.mutable.ListBuffer

class bin_conj() {
  var conjunto = "00000000"

  def imprime(): Unit = {
    println("Imprimindo o conjunto!")
    var lista_decode = ListBuffer()
    var i = 0
    while(i < 8) {
      var el = decode(conjunto(i), i)
      if (el != "-1") {
        lista_decode.+:(el)
      }
      i = i + 1
    }
    if (lista_decode.length > 0) {
      println("Conjunto: " + lista_decode)
    } else {
      println("Conjunto esta vazio!")
    }
  }

  def checa_pertence(caractere: String): Boolean = {
    // checar se o usuário inseriu um elemento válido
    if(ListBuffer("a", "b", "c", "d", "e", "f", "g", "h").contains(caractere)){
      println("Verificando se o elemento pertence a lista...")
      var char_enc = encode(caractere)
      var and_result = and_bin(conjunto, char_enc)
      if (and_result.contains(1)) return true else return false
    }else{
      println("Caractere nao valido inserido. Encerrando!")
      return false
    }
  }

  def inserir(caractere: String): Boolean = {
    println("Adicionando elemento ao conjunto!")
    println("Verificando se ja pertence ao conjunto...")
    if(checa_pertence(caractere)){
      println("Ja pertence, cancelando operacao...")
      return false
    }else{
      println("Nao pertence, adicionando!")
    }
    var str_bin = encode(caractere)
    var str_zip = conjunto zip str_bin
    var str_map = str_zip.map(x => x(0).toString().toInt + x(1).toString().toInt).mkString("")
    println("Elemento adicionado! O conjunto fica: "+str_map)
    conjunto = str_map
    return true
  }

  def remover(caractere: String): Boolean = {
    println("Removendo elemento do conjunto...")
    println("Verificando se existe no conjunto")
    if(checa_pertence(caractere)){
      println("O elemento pertence, iniciando remocao em "+conjunto+" de "+caractere)
    }else{
      println("O elemento nao pertence, cancelando operacao")
      return false
    }
    var str_bin = encode(caractere)
    var str_zip = conjunto zip str_bin
    var str_map = str_zip.map(x => x(0).toString().toInt - x(1).toString().toInt).mkString("")
    println("Elemento removido! O conjunto fica: "+str_map)
    conjunto = str_map
    return true
  }

  def uniao(conjunto2: String): Unit = {
    println("Iniciando uniao entre "+conjunto+" e "+conjunto2)
    var str_bin = conjunto2
    var el1 = -1
    var el2 = -1
    var str_zip = conjunto zip str_bin
    var str_map = str_zip.map(x => {
      el1 = x(0).toString().toInt
      el2 = x(1).toString().toInt
      if(el1*el2 == 1) 1 else if(el1+el2 == 0) 0 else 1
    }).mkString("")

    println("Uniao conlcuida! O conjunto passa a ser: "+str_map)
    conjunto = str_map
  }

  def interseccao(conjunto2: String): Unit = {
    println("Iniciando interseccao entre "+conjunto+" e "+conjunto2)
    var str_bin = conjunto2
    var el1 = -1
    var el2 = -1
    var str_zip = conjunto zip str_bin
    var str_map = str_zip.map(x => {
      el1 = x(0).toString().toInt
      el2 = x(1).toString().toInt
      if(el1 == el2) el1 else 0
    }).mkString("")
    println("Interseccao conlcuida! O conjunto passa a ser: "+str_map)
    conjunto = str_map
  }

  def diferenca(conjunto2: String): Unit = {
    println("Iniciando subtracao entre "+conjunto+" e "+conjunto2)
    var str_bin = conjunto2
    var el1 = -1
    var el2 = -1
    var str_zip = conjunto zip str_bin
    var str_map = str_zip.map(x => {
      el1 = x(0).toString().toInt
      el2 = x(1).toString().toInt
      if(el1==el2) 0 else el1
    }).mkString("")

    println("Subtracao conlcuida! O conjunto passa a ser: "+str_map)
    conjunto = str_map
  }

  def comprimento(): Int = {
    println("Calculando numero de elementos do conjunto: "+conjunto)
    var int_map = conjunto.map(x => { x.toString().toInt }).sum
    println("O numero de elementos no conjnto e de: "+int_map)
    return int_map
  }

  def subconjunto(conjunto2: String): Boolean = {
    println("Verificando se o conjunto "+conjunto+" e subconjunto de "+conjunto2)
    if(conjunto2.length < 8){
      println("Nao e possivel verificar: Conjunto "+conjunto2+" e menor que "+conjunto+" encerrando")
      return false
    }else if(conjunto2.length == 8){
      if(conjunto==conjunto2){
        println("Os conjuntos tem mesmo tamanho e sao iguais!")
        return true
      }else{
        println("Apesar do mesmo tamanho, sao diferentes")
        return false
      }
    }else{
      var init_idx = 0
      var end_idx = 8
      var limit = conjunto2.length()
      while(end_idx != limit){
        if(conjunto == conjunto2.slice(init_idx, end_idx)){
          println("Encontrado um subconjunto em "+conjunto2+" equivalente a "+conjunto)
          return true
        }
        init_idx = init_idx + 1
        end_idx = end_idx + 1
      }
      println("Nao foi encontrado nenhum subconjunto em "+conjunto2+" equivalente a "+conjunto)
      return false

    }
  }

  def libera(): Unit = {
    println("Liberando o conjunto!")
    conjunto = "00000000"
  }

}

def decode(bin_char: Char, idx: Int): String = {
  var caractere = bin_char.toInt
    if (caractere != 0) {
      if (idx == 0) {
        return "a"
      } else if (idx == 1) {
        return "b"
      } else if (idx == 2) {
        return "c"
      } else if (idx == 3) {
        return "d"
      } else if (idx == 4) {
        return "e"
      } else if (idx == 5) {
        return "f"
      } else if (idx == 6) {
        return "g"
      } else {
          return "h"
        }
    } else {
      return "-1"
    }
  }

def encode(caractere: String): String = {  
  if (caractere == "a") {
    return "00000001"
  } else if (caractere == "b") {
    return "00000010"
  } else if (caractere == "c") {
    return "00000100"
  } else if (caractere == "d") {
    return "00001000"
  } else if (caractere == "e") {
    return "00010000"
  } else if (caractere == "f") {
    return "00100000"
  } else if (caractere == "g") {
    return "01000000"
  } else {
      return "10000000"
    }

}

def and_bin(char1: String, char2: String): ListBuffer[Int] = {
  println("Operaca and bin entre "+char1+" e "+char2)
  var zipped = char1 zip char2
  //zipped.map(x => println(x(0).toString().toInt))
  var out = zipped.map(tupla => if (tupla(0)==tupla(1) && tupla(0).toString().toInt==1 && tupla(1).toString().toInt==1) 1 else 0).toList
  println("Operacao and binario, saida: "+out)
  return ListBuffer.empty ++= out
}

def check_igualdade(conjunto1: bin_conj, conjunto2: bin_conj): Boolean = {
  if(conjunto1.comprimento() != conjunto2.comprimento()){
    println("Conjuntos sao diferentes devido ao comprimento!")
    return false
  }else if(conjunto1.conjunto == conjunto2.conjunto){
    println("Os conjuntos sao iguais!")
    return true
  }else{
    println("Os conjuntos sao diferentes devido aos elementos!")
    return false
  }

  
}

object Main_code {
  def main(args: Array[String]) = {
    var conjunto_binario = new bin_conj()
    conjunto_binario.imprime()
    conjunto_binario.inserir("a")
    conjunto_binario.remover("a")
    println(conjunto_binario.conjunto)
    conjunto_binario.uniao("10101011")
    conjunto_binario.interseccao("01101000")
    conjunto_binario.comprimento()
    //if (conjunto_binario.checa_pertence("a")) println("Contem!!") else println("Nao contem")
    // hashtable.inserir(5)
  }
}
