package adt 

import scala.util.control.Breaks._
//
// Questão 01
class ListaEncadeada(var lista: List[Int] = List()) {
// 2 adiciona elemento no início
  def inputElement(el: Int): Unit = {
    lista = lista.+:(el)
    println("Elemento inserido no inicio da lista: " + el)
  }

// 3
  def imprimeLista(): Unit = {
    if (lista.isEmpty == false) {
      println("A Lista tem o seguinte conteudo:")
      for (i <- 0 until lista.length) {
        print(lista(i) + " ")
      }
      println("")
    } else {
      println("A lista esta vazia!")
    }
  }

  // 4
  def imprimeListaRecurs(): Unit = {
    println("Imprimindo lista recursivamente abaixo:")
    if (lista.isEmpty == false) {
      var idx = 0
      printaRecurs(lista, idx)
    } else {
      println("A lista está vazia!")
    }
  }

  def printaRecurs(lista: List[Int], idx: Int): Unit = {
    if (lista.length != idx) {
      print(lista(idx) + " ")
      printaRecurs(lista, idx + 1)
    }
    println("")
  }

  // 5
  def imprimeListaInv(): Unit = {
    if (lista.isEmpty == false) {
      println("Imprimindo a lista inversa:")
      for (i <- 0 until lista.length) {
        print(lista(lista.length - (i + 1)) + " ")
      }
      println("")
    } else {
      println("A lista esta vazia!")
    }
  }

  // 6
  def checkListaVazia(): Int = {
    if (lista.isEmpty) {
      println("Lista vazia: retornando 1")
      return 1
    }
    println("Lista preenchida: retornando 0")
    return 0
  }
  // 7
  def buscarLista(el: Int): Int = {
    for (i <- 0 until lista.length) {
      if (lista(i) == el) {
        println("Elemento encontrado no indice " + i)
        return i
      }
    }
    println("Objeto nao encontrado")
    return -1
  }
  // 8
  def removeElm(el: Int): Unit = {
    lista = lista.filter(_ != el)
  }
  // 9
  def removeElmRecurs(el: Int): Unit = {
    lista = lista.filter(_ != el)
  }
  // 10
  def liberarLista(): Unit = {
    lista = List()
  }
}

// Questão 02
class ListaEncadeadaOrdenada(var lista: List[Int] = List()) {
  // 2 adiciona elemento no início
  def inputElement(el: Int): Unit = {
    if (lista.isEmpty == false) {
      println("A lista já tem algum conteúdo...")
      var lista1 = lista.filter(_ < el).+:(el)
      var lista2 = lista.filter(_ >= el)
      lista = List.concat(lista1, lista2)
    } else {
      println("A lista está vazia!")
      lista = lista.+:(el)
      println("Elemento inserido no inicio da lista: " + el)
    }
  }

// 3
  def imprimeLista(): Unit = {
    if (lista.isEmpty == false) {
      println("A Lista tem o seguinte conteudo:")
      for (i <- 0 until lista.length) {
        print(lista(i) + " ")
      }
      println("")
    } else {
      println("A lista esta vazia!")
    }
  }

  // 4
  def imprimeListaRecurs(): Unit = {
    println("Imprimindo lista recursivamente abaixo:")
    if (lista.isEmpty == false) {
      var idx = 0
      printaRecurs(lista, idx)
    } else {
      println("A lista está vazia!")
    }
  }

  def printaRecurs(lista: List[Int], idx: Int): Unit = {
    if (lista.length != idx) {
      print(lista(idx) + " ")
      printaRecurs(lista, idx + 1)
    }
    println("")
  }

  // 5
  def imprimeListaInv(): Unit = {
    if (lista.isEmpty == false) {
      println("Imprimindo a lista inversa:")
      for (i <- 0 until lista.length) {
        print(lista(lista.length - (i + 1)) + " ")
      }
      println("")
    } else {
      println("A lista esta vazia!")
    }
  }

  // 6
  def checkListaVazia(): Int = {
    if (lista.isEmpty) {
      println("Lista vazia: retornando 1")
      return 1
    }
    println("Lista preenchida: retornando 0")
    return 0
  }
  // 7
  def buscarLista(el: Int): Int = {
    for (i <- 0 until lista.length) {
      if (lista(i) == el) {
        println("Elemento encontrado no indice " + i)
        return i
      }
    }
    println("Objeto nao encontrado")
    return -1
  }
  // 8
  def removeElm(el: Int): Unit = {
    lista = lista.filter(_ != el)
  }
  // 9
  def removeElmRecurs(el: Int): Unit = {
    lista = lista.filter(_ != el)
  }
  // 10
  def liberarLista(): Unit = {
    lista = List()
  }
}

def verificaIgualdade(lista1: List[Int], lista2: List[Int]): Int = {
  println("Verificador de igualdade entre listas")
  if(lista1.length != lista2.length){
    println("As listas são de tamanhos diferentes, portanto são diferentes!")
    return 0
  }
  for (idx <- 0 until lista1.length){
    if(lista1(idx) != lista2(idx)){
      println("As listas são diferentes!")
      return 0
    }
  }
  println("Nenhum elemento entre as listas é diferente po posição, portanto as listas são iguais!")
  return 1
}

// Questão 03
class ListaEncadeadaDupOrdObj(var valor: Int){ // duplamente encadeada circular
  var head_lista = ListaEncadeadaDupOrdNode(valor)
  def check_values(): Unit = {
    println("Printing values da lista duplamente encadeada ordenada!")
    println("Node 1 value: "+head_lista.valor)
    breakable {
      var atual = head_lista.pont_prox
      while(true){
        var iteracao = 2
        if(atual != head_lista && atual != null){
          println("Node "+iteracao+" value: "+atual.valor)
          atual = atual.pont_prox
        }else{
          println("Fim da impressao!")
          break
        }
      }
    }
  }
  def criar_node(valor_node : Int): Unit = {
    println("Inserindo novo node!")
    var novo_node = ListaEncadeadaDupOrdNode(valor_node)
    breakable {
      var atual = head_lista
      while(true){
        if(novo_node.valor >= atual.valor && atual.pont_prox != null){
          atual = atual.pont_prox
        }else if(atual.pont_prox != null){
          if(atual == head_lista){
            println("O novo valor sera posicionado no inicio da lista!")
            var tmp = head_lista
            novo_node.pont_prox = tmp
            novo_node.pont_prox.pont_ant = novo_node
            novo_node.pont_prox.pont_prox = head_lista.pont_prox
            head_lista = novo_node
            break
          }else if(atual != head_lista){
            println("O novo valor nao sera posicionado nem no inicio nem no final da lista!")
            var node_ant = atual.pont_ant
            novo_node.pont_prox = atual
            novo_node.pont_ant = node_ant
            node_ant.pont_prox = novo_node
            atual.pont_ant = novo_node
            break
          }
        }else if(novo_node.valor >= atual.valor && atual.pont_prox == null){
          println("O novo valor sera posicionado no final da lista!")
          atual.pont_prox = novo_node
          novo_node.pont_ant = atual
          break
        }else{
          println("O novo node sera posicionado na penutima posicao da lista!")
          var node_ant = atual.pont_ant
          novo_node.pont_prox = atual
          novo_node.pont_ant = node_ant
          node_ant.pont_prox = novo_node
          atual.pont_ant = novo_node
          break
        }
      }
    }
    println("Novo node inserido!")
  }

  def remover_node(valor_node : Int): Unit = {
    println("Iniciando a remocao de um node!")
    var procurar = true
    breakable{
      println("Procurando o node para remover!")
      if(head_lista.valor == valor_node){
        println("Node encontrado!")
        if(head_lista.pont_prox == null){
          println("A lista so possui 1 unico node! Eliminando a lista...")
          head_lista.valor = 0
          head_lista.pont_prox = null
          head_lista.pont_ant = null
          println("Remocao concluida, o node e de atributos nulos")
          procurar = false
        }else{
          println("Sua lista possui +de 1 elemento!")
          if(head_lista.pont_prox.pont_prox == head_lista){
            head_lista = head_lista.pont_prox
            head_lista.pont_prox = null
            head_lista.pont_ant = null
            procurar = false
          }else{
            var prox_lista = head_lista.pont_prox
            head_lista = head_lista.pont_prox
            head_lista.pont_ant = null
            println("Remocao concluida!")
            procurar = false
          }
        }
      }
      var atual = head_lista.pont_prox
      while(procurar){
        if(atual == head_lista || atual == null){
          println("Node nao encontrado! Concluindo operacao de remocao!")
          break
        }
        if(atual.valor == valor_node){
          println("Node encontrado! Removendo...")
          var node_prox = atual.pont_prox
          var node_ant = atual.pont_ant
          node_ant.pont_prox = node_prox
          if(node_prox != null){
            println("Removido! Era o ultimo node da lista!")
            node_prox.pont_ant = node_ant
          }
          //atual.pont_ant = null
          //atual.pont_ant = null
          //encontrado = true
          println("Concluindo a remocao!")
          break
        }else{
          atual = atual.pont_prox
        }
      }
    }
  }
}
class ListaEncadeadaDupOrdNode(val value : Int){
  var pont_ant : ListaEncadeadaDupOrdNode = null
  var valor : Int = value
  var pont_prox : ListaEncadeadaDupOrdNode = null
}

// Questão 04
class ListaEncadeadaCircSimpObj(var valor: Int){ // duplamente encadeada circular
  var head_lista = ListaEncadeadaCircSimpNode(valor)
  head_lista.pont_prox = head_lista
  def check_values(): Unit = {
    println("Printing values da lista encadeada!")
    println("Node 1 value: "+head_lista.valor)
    breakable {
      var atual = head_lista.pont_prox
      while(true){
        var iteracao = 2
        if(atual != head_lista && atual != null){
          println("Node "+iteracao+" value: "+atual.valor)
          atual = atual.pont_prox
          //var new_atual = atual.pont_prox
          //var atual = new_atual
        }else{
          println("Fim da impressao!")
          break
        }
      }
    }
  }
  def criar_node(valor_node : Int): Unit = {
    println("Inserindo novo node!")
    var novo_node = ListaEncadeadaCircSimpNode(valor_node)
    novo_node.pont_prox = head_lista
    // testando se há apenas 1 valor!
    if(head_lista.pont_prox == head_lista){
      println("sua lista so possui 1 valor armazenado! Adicionando o segundo...")
      head_lista.pont_prox = novo_node
    }else{
      println("sua lista ja possui +de 1 valor armazenado! Adicionando ao final...")
      breakable {
        var atual = head_lista.pont_prox
        while(true){
          if(atual.pont_prox == head_lista){
            atual.pont_prox = novo_node
            break
          }else{
            atual = atual.pont_prox
          }
        }
      }
    }
    println("Novo node inserido!")
  }

  def remover_node(valor_node : Int): Unit = {
    println("Iniciando a remocao de um node!")
    var procurar = true
    breakable{
      println("Procurando o node para remover!")
      if(head_lista.valor == valor_node){
        println("Node encontrado! E o head!")
        if(head_lista.pont_prox == head_lista){
          println("A lista so possui 1 unico node! Eliminando a lista...")
          head_lista.valor = 0
          head_lista.pont_prox = null
          println("Remocao concluida, o node e de atributos nulos")
          procurar = false
        }else{
          println("Sua lista possui +de 1 elemento!")
          var old_head = head_lista
          var node_prox = head_lista.pont_prox.pont_prox
          head_lista = head_lista.pont_prox
          head_lista.pont_prox = node_prox
          breakable {
            var atual = head_lista.pont_prox
            while(true){
              if(atual == head_lista && atual.pont_prox == head_lista){
                println("Breaking!")
                break
              }
              if(old_head == node_prox){
                head_lista.pont_prox = head_lista
                break
              }
              if(atual == head_lista){
                atual = atual.pont_prox
              }else if(atual.pont_prox == old_head){
                atual.pont_prox = head_lista
                break
              }else{
                atual = atual.pont_prox
              }
            }
          }
          procurar = false
        }
        }
      if(procurar){
        println("Seu node nao e o head! Seguindo procurando...")
      }
      var atual_antg = head_lista
      var atual = head_lista.pont_prox
      while(procurar){
        if(atual == head_lista){
          println("Node nao encontrado! Concluindo operacao de remocao!")
          break
        }
        if(atual.valor == valor_node){
          println("Node encontrado! Removendo...")
          var node_prox = atual.pont_prox
          atual_antg.pont_prox = node_prox
          //atual.pont_ant = null
          //atual.pont_ant = null
          //encontrado = true
          println("Concluindo a remocao!")
          break
        }else{
          var tmp = atual
          atual_antg = tmp
          atual = atual.pont_prox
        }
      }
    }
  }
}
class ListaEncadeadaCircSimpNode(val value : Int){
  var valor : Int = value
  var pont_prox : ListaEncadeadaCircSimpNode = null
}

// Questão 05
class ListaEncadeadaObj(var valor: Int){ // duplamente encadeada circular
  var head_lista = ListaEncadeadaNode(valor)
  def check_values(): Unit = {
    println("Printing values da lista encadeada!")
    println("Node 1 value: "+head_lista.valor)
    breakable {
      var atual = head_lista.pont_prox
      while(true){
        var iteracao = 2
        if(atual != head_lista && atual != null){
          println("Node "+iteracao+" value: "+atual.valor)
          atual = atual.pont_prox
          //var new_atual = atual.pont_prox
          //var atual = new_atual
        }else{
          println("Fim da impressao!")
          break
        }
      }
    }
  }
  def criar_node(valor_node : Int): Unit = {
    println("Inserindo novo node!")
    var novo_node = ListaEncadeadaNode(valor_node)
    novo_node.pont_prox = head_lista
    // testando se há apenas 1 valor!
    if(head_lista.pont_prox == null){
      println("sua lista so possui 1 valor armazenado! Adicionando o segundo...")
      head_lista.pont_prox = novo_node
      head_lista.pont_ant = novo_node
    }else{
      println("sua lista ja possui +de 1 valor armazenado! Adicionando ao final...")
      var cauda_lista = head_lista.pont_ant
      //head_lista.pont_ant.pont_prox = novo_node
      cauda_lista.pont_prox = novo_node
      novo_node.pont_ant = cauda_lista
      head_lista.pont_ant = novo_node
    }
    println("Novo node inserido!")
  }

  def remover_node(valor_node : Int): Unit = {
    println("Iniciando a remocao de um node!")
    var procurar = true
    breakable{
      println("Procurando o node para remover!")
      if(head_lista.valor == valor_node){
        println("Node encontrado!")
        if(head_lista.pont_prox == null){
          println("A lista so possui 1 nico node! Eliminando a lista...")
          head_lista.valor = 0
          head_lista.pont_ant = null
          head_lista.pont_ant = null
          println("Remocao concluida, o node e de atributos nulos")
          procurar = false
        }else{
          println("Sua lista possui +de 1 elemento!")
          if(head_lista.pont_prox.pont_prox == head_lista){
            head_lista = head_lista.pont_prox
            head_lista.pont_prox = null
            head_lista.pont_ant = null
            procurar = false
          }else{
            var cauda_lista = head_lista.pont_ant
            var prox_lista = head_lista.pont_prox
            head_lista = head_lista.pont_prox
            head_lista.pont_ant = cauda_lista
            cauda_lista.pont_prox = head_lista
            println("Remocao concluida!")
            procurar = false
          }
        }
      }
      var atual = head_lista.pont_prox
      while(procurar){
        if(atual == head_lista || atual == null){
          println("Node nao encontrado! Concluindo operacao de remocao!")
          break
        }
        if(atual.valor == valor_node){
          println("Node encontrado! Removendo...")
          var node_prox = atual.pont_prox
          var node_ant = atual.pont_ant
          node_ant.pont_prox = node_prox
          node_prox.pont_ant = node_ant
          //atual.pont_ant = null
          //atual.pont_ant = null
          //encontrado = true
          println("Concluindo a remocao!")
          break
        }else{
          atual = atual.pont_prox
        }
      }
    }
  }
}
class ListaEncadeadaNode(val value : Int){ // (pont_ant: ListaEncadeadaNode, valor: Int, pont_prox: ListaEncadeadaNode)
  var pont_ant : ListaEncadeadaNode = null
  var valor : Int = value
  var pont_prox : ListaEncadeadaNode = null

}
// Questão 06
class ContaBancaria(val numero_ : Int, val saldo_ : Float, val divida_ : Float){ // (var numero: Int, var saldo: Float, var divida: Float)
 
  var saldo = saldo_
  var divida = divida_
  def consulta_saldo(): Unit = {
    println("Consulta de saldo!")
    println("Seu saldo e de: R$ "+saldo+" reais!")
  }
  def debito(valorDebitar: Float): Unit = {
    if(valorDebitar > saldo){
      println("Saldo insuficiente, por favor insira outro valor.")
    } else {
      saldo = saldo - valorDebitar
      println("Debito realizado com sucesso. Seu novo saldo e de: " + saldo)
    }
  }
  def depositar(valorDepositar: Float): Unit = {
    saldo += valorDepositar
    println("Deposito realziado com sucesso. Seu novo saldo e de: "+ saldo)
  }
  def credito(valorCreditar: Float): Unit = {
    saldo += valorCreditar
    divida += valorCreditar
    println("Credito realziado com sucesso. Seu novo saldo e de: "+ saldo)
  }
}

class ContaPoupanca(override val numero_ : Int, override val saldo_ : Float, override val divida_ : Float, var juros: Float) extends ContaBancaria(numero_ , saldo_ , divida_)

class ContaFidelidade(override val numero_ : Int, override val saldo_ : Float, override val divida_ : Float, var bonus: Float) extends ContaBancaria(numero_ , saldo_ , divida_){
  override def credito(valorCreditar: Float): Unit = {
    saldo += valorCreditar
    //bonus = bonus + (valorCreditar/100)
    divida += valorCreditar
    println("Credito realizado com sucesso. Seu novo saldo e de: "+ saldo)
  }
  def renderBonus(): Unit = {
    saldo += bonus
    bonus = 0
    println("Bonus adicionado com sucesso. Seu novo saldo e de: "+saldo)
  }
}

def testes_03(): Unit = {
  var listaencaddupord = new ListaEncadeadaDupOrdObj(1)
  listaencaddupord.criar_node(4)
  listaencaddupord.criar_node(0)
  listaencaddupord.criar_node(3)
  listaencaddupord.remover_node(4)
  listaencaddupord.criar_node(5)
  listaencaddupord.criar_node(5)
  listaencaddupord.remover_node(5)
  listaencaddupord.check_values()

}

def testes_04_remocao(): Unit = {
  var listaencad_circsimp = new ListaEncadeadaCircSimpObj(1)
  listaencad_circsimp.check_values()
  listaencad_circsimp.criar_node(3)
  listaencad_circsimp.check_values()
  listaencad_circsimp.criar_node(4)
  listaencad_circsimp.criar_node(2)
  listaencad_circsimp.check_values()
  listaencad_circsimp.remover_node(1)
  listaencad_circsimp.remover_node(3)
  listaencad_circsimp.remover_node(4)
  listaencad_circsimp.remover_node(2)
  listaencad_circsimp.check_values()
  
}

def testes_05_inserir(): Unit = {
  var listaencad_circ = new ListaEncadeadaObj(1)
    listaencad_circ.check_values()
    listaencad_circ.criar_node(3)
    listaencad_circ.check_values()
    listaencad_circ.criar_node(2)
    listaencad_circ.check_values()
    listaencad_circ.criar_node(7)
    listaencad_circ.check_values()
    listaencad_circ.criar_node(10)
    listaencad_circ.criar_node(0)
    listaencad_circ.criar_node(4)
    listaencad_circ.check_values()
}

def testes_05_remocao(): Unit = {
  var listaencad_circ = new ListaEncadeadaObj(1)
    listaencad_circ.check_values()
    listaencad_circ.criar_node(3)
    listaencad_circ.check_values()
    listaencad_circ.criar_node(2)
    listaencad_circ.remover_node(7)
    listaencad_circ.check_values()
    listaencad_circ.criar_node(7)
    listaencad_circ.check_values()
    listaencad_circ.criar_node(10)
    listaencad_circ.remover_node(7)
    listaencad_circ.check_values()
    listaencad_circ.criar_node(0)
    listaencad_circ.check_values()
    listaencad_circ.criar_node(4)
    listaencad_circ.check_values()
    listaencad_circ.remover_node(4)
    listaencad_circ.check_values()
    listaencad_circ.remover_node(1)
    listaencad_circ.check_values()
    listaencad_circ.criar_node(6)
    listaencad_circ.check_values()
    listaencad_circ.remover_node(10)
    listaencad_circ.check_values()
    listaencad_circ.remover_node(3)
    listaencad_circ.check_values()
    listaencad_circ.remover_node(2)
    listaencad_circ.check_values()
    listaencad_circ.remover_node(0)
    listaencad_circ.check_values()
    listaencad_circ.remover_node(6)
    listaencad_circ.check_values()
}
object Hello {
  def main(args: Array[String]) = {
    // println("Hello, world")
    /*var x = new ListaEncadeada
    x.imprimeLista()
    x.inputElement(2)
    x.inputElement(3)
    x.imprimeListaRecurs()
    x.imprimeListaInv()
    x.checkListaVazia()
    x.buscarLista(9)
    x.liberarLista()
    x.imprimeLista()

    var minha_conta = new ContaPoupanca(1000, 100.00, 0.0, 0.1)
    minha_conta.depositar(20.00)
    minha_conta.consulta_saldo()*/
    //testes_04_remocao()
    testes_03()
  }
}
