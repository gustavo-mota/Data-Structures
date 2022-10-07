import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._

class ListaEncadeada(var valor: Int) { // duplamente encadeada circular
  var head_lista = ListaEncadeadaNode(valor)
  head_lista.pont_prox = head_lista
  def check_values(): Unit = {
    println("Printing values da lista encadeada!")
    println("Node 1 value: " + head_lista.valor)
    breakable {
      var atual = head_lista.pont_prox
      while (true) {
        var iteracao = 2
        if (atual != head_lista && atual != null) {
          println("Node " + iteracao + " value: " + atual.valor)
          atual = atual.pont_prox
          // var new_atual = atual.pont_prox
          // var atual = new_atual
        } else {
          println("Fim da impressao!")
          break
        }
      }
    }
  }
  def criar_node(valor_node: Int): Unit = {
    println("Inserindo novo node!")
    var novo_node = ListaEncadeadaNode(valor_node)
    novo_node.pont_prox = head_lista
    // testando se há apenas 1 valor!
    if (head_lista.pont_prox == head_lista) {
      println(
        "sua lista so possui 1 valor armazenado! Adicionando o segundo..."
      )
      head_lista.pont_prox = novo_node
    } else {
      println(
        "sua lista ja possui +de 1 valor armazenado! Adicionando ao final..."
      )
      breakable {
        var atual = head_lista.pont_prox
        while (true) {
          if (atual.pont_prox == head_lista) {
            atual.pont_prox = novo_node
            break
          } else {
            atual = atual.pont_prox
          }
        }
      }
    }
    println("Novo node inserido!")
  }

  def remover_node(valor_node: Int): Unit = {
    println("Iniciando a remocao de um node!")
    var procurar = true
    breakable {
      println("Procurando o node para remover!")
      if (head_lista.valor == valor_node) {
        println("Node encontrado! E o head!")
        if (head_lista.pont_prox == head_lista) {
          println("A lista so possui 1 unico node! Eliminando a lista...")
          head_lista.valor = 0
          head_lista.pont_prox = null
          println("Remocao concluida, o node e de atributos nulos")
          procurar = false
        } else {
          println("Sua lista possui +de 1 elemento!")
          var old_head = head_lista
          var node_prox = head_lista.pont_prox.pont_prox
          head_lista = head_lista.pont_prox
          head_lista.pont_prox = node_prox
          breakable {
            var atual = head_lista.pont_prox
            while (true) {
              if (atual == head_lista && atual.pont_prox == head_lista) {
                println("Breaking!")
                break
              }
              if (old_head == node_prox) {
                head_lista.pont_prox = head_lista
                break
              }
              if (atual == head_lista) {
                atual = atual.pont_prox
              } else if (atual.pont_prox == old_head) {
                atual.pont_prox = head_lista
                break
              } else {
                atual = atual.pont_prox
              }
            }
          }
          procurar = false
        }
      }
      if (procurar) {
        println("Seu node nao e o head! Seguindo procurando...")
      }
      var atual_antg = head_lista
      var atual = head_lista.pont_prox
      while (procurar) {
        if (atual == head_lista) {
          println("Node nao encontrado! Concluindo operacao de remocao!")
          break
        }
        if (atual.valor == valor_node) {
          println("Node encontrado! Removendo...")
          var node_prox = atual.pont_prox
          atual_antg.pont_prox = node_prox
          // atual.pont_ant = null
          // atual.pont_ant = null
          // encontrado = true
          println("Concluindo a remocao!")
          break
        } else {
          var tmp = atual
          atual_antg = tmp
          atual = atual.pont_prox
        }
      }
    }
  }
}
class ListaEncadeadaNode(val value: Int) {
  var valor: Int = value
  var pont_prox: ListaEncadeadaNode = null
}

class HashTable(var inteiro_n: Int, var buffer_size: Int) {
  var tamanho = inteiro_n / 2
  var buffer_tamanho = buffer_size
  var hash = ListBuffer[ListaEncadeada]()
  for (i <- 0 until tamanho) {
    hash = hash :+ new ListaEncadeada(0)
  }
  println("Hash table criada! ")

  /*def inserir(valor: Int): Unit = {
    println("Adicionando elemento a hash table! Calculando chave...")
    var chave = valor % tamanho
    println("Adicionando o elemento de chave " + chave + " na hash table!")
    // hash(chave) = hash(chave) :+ (valor)
    hash(chave).inputElement(valor)
    println("Elemento adicionado a sua hash table! ")
  }*/

  /*def imprimir_tudo(): Unit = {
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
    println("Escavizando a hash table!")
    for (i <- 0 until tamanho) {
      hash(i) = ListBuffer()
    }
    println("Hash table esvaziada!")
  }*/
}

object Main_code {
  def main(args: Array[String]) = {
    println("Lista 4")
    var hashtable = new HashTable(10, 2)
    // hashtable.inserir(2)
  }
}
