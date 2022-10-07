import scala.collection.mutable.ListBuffer
import scala.math.floor

class Heap(var heap_type: String, var n: Int) {
  // var head = heap_node(value_input)
  var heap_lista = ListBuffer[Int]()
  var max = n
  var pos_livre = 0

  def node_pai_idx(idx: Int): Int = {
    if (idx == 0) {
      return 0
    }
    return floor((idx - 1) / 2).toInt
  }

  def node_right_idx(idx: Int): Int = {
    var r_idx = (2 * idx) + 2
    if(r_idx > heap_lista.length){
      return -1
    }
    return r_idx
  }
  def node_left_idx(idx: Int): Int = {
    var l_idx = (2 * idx) + 1
    if(l_idx > heap_lista.length){
      return -1
    }
    return l_idx
  }
  def inserir(valor: Int): Int = {
    // heap_lista = heap_lista.+:(valor).reverse
    heap_lista.append(valor)
    return heap_lista.length
  }

  def corrige_acima(input_idx: Int): Unit = {
    var check_idx = -1
    if(input_idx == -1){
      check_idx = heap_lista.length - 1
    }else{
      check_idx = input_idx
    }
    var pai_idx = node_pai_idx(check_idx)
    var check = true
    var i = 1
    if (heap_type == "min" && heap_lista.length > 1) {
      while (check) {
        // println("first try " + i)
        i = i + 1
        println("Indices para checar: " + check_idx + " e " + pai_idx)
        if (heap_lista(check_idx) < heap_lista(pai_idx)) {
          var temp = heap_lista(pai_idx)
          heap_lista(pai_idx) = heap_lista(check_idx)
          heap_lista(check_idx) = temp

          check_idx = pai_idx
          pai_idx = node_pai_idx(check_idx)
        } else {
          check = false
        }
      }
    }
  }

  def remove(elemento: Int): Int = {
    var el_idx = heap_lista.indexOf(elemento)
    if(el_idx != -1){
      var heap_len = heap_lista.length - 1
      heap_lista(el_idx) = heap_lista(heap_len)
      heap_lista = heap_lista.dropRight(1)
      return el_idx
    }
    return -1
  }

  def corrige_abaixo(removido_idx: Int): Unit = {
    var value_check_idx = removido_idx
    var value_check = -1
    var filho_value = -1
    var left_idx = -1
    var right_idx = -1

    while( (2*value_check_idx)+1 < heap_lista.length ){
      value_check = heap_lista(value_check_idx)
      left_idx = node_left_idx(value_check_idx)
      right_idx = node_right_idx(value_check_idx)
      //println("Indices: value_check_idx "+value_check_idx+" left idx: "+left_idx+" right idx: "+right_idx)
      if(left_idx != -1 && (heap_lista(left_idx) < value_check)){
        println("Aqui 01")
        filho_value = heap_lista(left_idx)
        heap_lista(value_check_idx) = filho_value
        heap_lista(left_idx) = value_check

        value_check_idx = left_idx
      }else if(right_idx != -1 && (heap_lista(right_idx) < value_check)){
        println("Aqui 02")
        filho_value = heap_lista(right_idx)
        heap_lista(value_check_idx) = filho_value
        heap_lista(right_idx) = value_check

        value_check_idx = right_idx
      }
    }
    
  }

  def atualiza_valor(idx_atualiza: Int, valor_novo: Int): Unit = {
    var valor_antigo = heap_lista(idx_atualiza)
    heap_lista(idx_atualiza) = valor_novo
    if(heap_type=="min"){
      if(valor_antigo > valor_novo){
        println("Heap de min: Correcao subida!")
        corrige_acima(idx_atualiza)
      }else{
        println("Heap de min: Correcao descida!")
        corrige_abaixo(idx_atualiza)
      }
    }else{ // heap max
      if(valor_antigo > valor_novo){
        println("Heap de max: Correcao descida!")
        corrige_abaixo(idx_atualiza)
      }else{
        println("Heap de max: Correcao subida!")
        corrige_acima(idx_atualiza)
      }
    }
  }
}

def testes_corrige_acima(): Unit = {
  var heap = new Heap("min", 10)
  heap.inserir(3)
  heap.inserir(2)
  println("Antes da correcao: " + heap.heap_lista)
  heap.corrige_acima(-1)
  println("Apos da correcao: " + heap.heap_lista)
  heap.inserir(1)
  println("Antes da correcao: " + heap.heap_lista)
  heap.corrige_acima(-1)
  println("Apos da correcao: " + heap.heap_lista)
}

def testes_corrige_abaixo(): Unit = {
    var heap = new Heap("min", 10)
    heap.inserir(5)
    heap.inserir(4)
    heap.corrige_acima(-1)
    heap.inserir(9)
    heap.corrige_acima(-1)
    heap.inserir(7)
    heap.corrige_acima(-1)
    heap.inserir(3)
    heap.corrige_acima(-1)
    heap.inserir(6)
    heap.corrige_acima(-1)
    heap.inserir(8)
    heap.corrige_acima(-1)
    heap.inserir(2)
    heap.corrige_acima(-1)
    println("Antes remocao: " + heap.heap_lista)
    heap.remove(2)
    heap.corrige_abaixo(0)
    println("Apos remocao: " + heap.heap_lista)
}

def testes_atualiza(): Unit = {
    var heap = new Heap("min", 10)
    heap.inserir(5)
    heap.inserir(4)
    heap.corrige_acima(-1)
    heap.inserir(9)
    heap.corrige_acima(-1)
    heap.inserir(7)
    heap.corrige_acima(-1)
    heap.inserir(3)
    heap.corrige_acima(-1)
    heap.inserir(6)
    heap.corrige_acima(-1)
    heap.inserir(8)
    heap.corrige_acima(-1)
    heap.inserir(2)
    heap.corrige_acima(-1)
    println("Elementos adicionados nesta ordem: "+ListBuffer(5,4,9,7,3,6,8,2))
    println("Heap apos insercoes: "+heap.heap_lista)
    //heap.atualiza_valor(3, 10) // coreção descida
    heap.atualiza_valor(3, 1) // coreção subida
    println("Heap atualizou valor 4 na pos 3 para 1: "+heap.heap_lista)
}

object Main_code {
  def main(args: Array[String]) = {
    testes_corrige_acima()
    //testes_corrige_abaixo()
    //testes_atualiza()
  }
}
