import scala.collection.mutable.ListBuffer

class HashTable(var inteiro_n: Int) {
  var tamanho = inteiro_n / 2
  var hash = ListBuffer[ListBuffer[Int]]()
  for (i <- 0 until tamanho) {
    hash = hash :+ ListBuffer()
  }
  println("Hash table criada! ")

  def inserir(valor: Int): Unit = {
    println("Adicionando elemento a hash table! Calculando chave...")
    var chave = valor % tamanho
    println("Adicionando o elemento de chave " + chave + " na hash table!")
    hash(chave) = hash(chave) :+ (valor)
    println("Elemento adicionado a sua hash table! ")
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

  def buscar_elemento(valor: Int): Int = {
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

  def remove_elemento(valor: Int): Unit = {
    println("Iniciando remocao de elemento...")
    var achado = buscar_elemento(valor)
    if (achado == 1) {
      println("O elemento esta sendo removido!")
      var chave = valor % tamanho
      hash(chave) -= (valor)
      println("Elemento removido!")
    } else {
      println("O elemento nao existe na hash table!")
    }
  }

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
    var hashtable = new HashTable(10)
    hashtable.inserir(2)
    hashtable.inserir(5)
    hashtable.inserir(10)
    hashtable.inserir(21)
    hashtable.imprimir_tudo()
    // hashtable.imprimir_posicao(2)
    hashtable.buscar_elemento(2)
    // hashtable.remove_elemento(3)
    hashtable.remove_elemento(10)
    hashtable.libera_lista()
    hashtable.imprimir_tudo()
  }
}
