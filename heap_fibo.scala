import scala.collection.mutable.ListBuffer

class heapsf(){
    var heaps = ListBuffer[fibo]()

    def make_heap(): Int = {
        // cria uma heap e retorna o se índice
        var new_heap = fibo()
        heaps.append(new_heap)
        return heaps.length - 1
    }

    def insert_heapf(idx: Int, valor: Int): Unit = {
        if(idx > heaps.length){
            println("Heap nao localizada! nao foi possivel adicionar seu elemento!")
        }else{
            heaps(idx).insert(valor)
        }
    }
}

class fibo(){
    var raiz: node_fibo = null //node_fibo(valor)

    def insert(valor: Int): Unit = {
        if(raiz == null){
            raiz = node_fibo(valor)
        }else{
            var iter = false
            var atual = raiz
            while(iter == false){
                if(atual.valor < valor){
                    if(atual.left != null){
                        atual = atual.left
                    }else{
                        atual.left = node_fibo(valor)
                        iter = true
                    }
                }else{
                    if(atual.right != null){
                        atual = atual.right
                    }else{
                        atual.right = node_fibo(valor)
                        iter = true
                    }
                }
            }
        }
    }
}

class node_fibo(var valor_ : Int){
    var valor = valor_
    var filhos = ListBuffer[node_fibo]() // sem uso no momento
    var left: node_fibo = null
    var right: node_fibo = null
    var next_r: node_fibo = null
    var next_l: node_fibo = null
}

object Main_code {
  def main(args: Array[String]) = {
    println("Aqui")
    var hpf = fibo()
    hpf.insert(3)
    //println(hpf.raiz.)

  }
}