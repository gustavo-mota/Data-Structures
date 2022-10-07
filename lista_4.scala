import scala.collection.mutable.ListBuffer

class HashTable(var inteiro_n: Int, var bucket: Int) {
  var tamanho = inteiro_n / 2
  var tam_bucket = bucket
  var bucket_counter = 0
  var hash = ListBuffer[ListBuffer[Any]]()
  for (i <- 0 until tamanho) {
    hash = hash :+ ListBuffer()
  }
  println("Hash table criada! ")

  def inserir(valor: Int): Unit = {
    println("Adicionando elemento a hash table! Calculando chave...")
    var chave = valor % tamanho
    println("Adicionando o elemento de chave " + chave + " na hash table!")
    var check = true
    var checker = hash(chave) // tipo listbuffer
    var last = checker // tipo listbuffer
    var inter = 1
    var binario = chave.toBinaryString // 01110111
    var sub_binario = ""

    while (check) { // 01 00 11 10
      // println(checker.getClass == ListBuffer().getClass)
      if (checker.getClass == ListBuffer().getClass) {
        println("Bucket encontrado... procurando subbuckets... " + checker)
        println("Interacao: " + inter + " binario/chave: " + checker)
        last = checker
        if (checker.length > 0) {
          if (checker(binario.takeRight(inter).toInt).getClass == 1.getClass) {
            println("Chegamos a um bucket folha preenchido!")
            var checker = ""
          } else {
            println("Não chegamos a um bucket folha ainda!")
            // var checker = checker(binario.takeRight(inter).toInt)
            var checker = ""
          }

        } else {
          println("chegamos a um bucket folha vazio!")
          var checker = ""
        }
        inter = inter + 1
      } else {
        println("Bucket final encontrado! Elementos: " + last)
        println("Verificando se podemos adicionar o elemento...!")
        if (last.length == tam_bucket) {
          println(
            "Nao podemos adicionar. Testando se podemos criar novos buckets..."
          )
          if (bucket_counter == 8) {
            println("Nao podemos, esta tudo cheio!")
          } else {
            println("Ainda ha espaco! Criando subbuckets...")
            bucket_counter = bucket_counter + 1
            // codigo para criar subbuckets e reordenar toda a distribuicao de elementos nos buckets
          }
        } else {
          println("Podemos adicionar nesse bucket! Adicionando o elemento...")
          // codigo para adicioanr o elemento aqui no bucket
        }
        check = false
      }
    }
    println("Insercao de elemento na hash table concluida! ")
  }

  def imprimir_tudo(): Unit = {
    println("Imprimindo sua hash table!")
    for (i <- 0 until tamanho) {
      println("Hash n " + i + " :" + hash(i))
    }
  }

  def imprimir_posicao(pos: Int): Unit = {
    println("Imprimindo a posicao " + pos + " da sua hash table!")
    println(hash(pos))
  }

  def buscar_elemento(valor: Int): Int = { // refazer todo
    println("Buscando o elemento...")
    var chave = valor % tamanho
    for (i <- 0 until hash(chave).length) {
      if (valor == hash(chave)(i)) {
        println("Encontrado!")
        return 1
      }
    }
    println("Nao encontrado!")
    return 0
  }

  // def remove_elemento(valor: Int): Unit = {  }

  def libera_lista(): Unit = {
    println("Esvaziando a hash table!")
    for (i <- 0 until tamanho) {
      hash(i) = ListBuffer()
    }
    println("Hash table esvaziada!")
  }
}

object Main_code {
  def main(args: Array[String]) = {
    var hashtable = new HashTable(10, 2)
    println(hashtable.hash)
    hashtable.inserir(0)
    // hashtable.inserir(5)
  }
}
