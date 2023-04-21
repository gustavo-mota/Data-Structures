import scala.collection.mutable.ListBuffer
import scala.math.floor

class KDtree() {
    var raiz: KDnode = null
    var dimensao = -1

    def add_node(keys: ListBuffer[Int]): Unit = {
        println("Adicionando: " + keys)
        if(raiz == null){
            println("Kd tree vazia! A dimensao equivale a: " + keys.length)
            raiz = KDnode(keys=keys, disc=0)
            dimensao = keys.length
        }else{
            println("Inserindo seu node na KD Tree!")
            var loop = true
            var actual = raiz
            var d_idx = 0
            while(loop){
                if(keys(d_idx) >= actual.keys(d_idx)){
                    if(actual.node_dir == null){
                        actual.node_dir = KDnode(d_idx + 1, keys=keys)
                        loop = false
                    }else{
                        actual = actual.node_dir
                    }
                }else{
                    if(actual.node_esq == null){
                        actual.node_esq = KDnode(d_idx + 1, keys=keys)
                        loop = false
                    }else{
                        actual = actual.node_esq
                    }
                }
                d_idx = if (d_idx == keys.length-1) 0 else d_idx + 1
            }
        }
    }
    def look_for(keys: ListBuffer[Int], disc: Int = -1): KDnode = {
        var loop = true
        var actual = raiz
        var d_idx = 0 // valor de discriminante atual
        var null_node = new KDnode(-1, ListBuffer(-1))
        
        // func anon verifica se o discriminante bate
        // caso o usuário tenha dado um discriminante válido
        val check_disc = (disc: Int, disc2check: Int) => if (disc == -1) true else disc == disc2check 
        
        while(loop){
            // caso n haja disc válido para testar, o peso recai sobre se as chaves batem
            if(keys == actual.keys && check_disc(disc, actual.disc)){
                println("Encontrado o node!")
                loop = false
            }else{
                // acessando descendentes
                if(keys(d_idx) >= actual.keys(d_idx)){
                    if(actual.node_dir == null){
                        println("Não encontramos o node!")
                        actual = null_node
                        loop = false
                    }else{
                        actual = actual.node_dir
                    }
                }else{
                    if(actual.node_esq == null){
                        println("Não encontramos o node!")
                        actual = null_node
                        loop = false
                    }else{
                        actual = actual.node_esq
                    }
                }
                d_idx = if (d_idx == keys.length-1) 0 else d_idx + 1
            }
        }
        if(actual == raiz){
            println("Seu node encontrado e a raiz!")
            return raiz
        }
        return actual
    }
    def delete(keys: ListBuffer[Int], node_ : KDnode, discre: Int): KDnode = {
        
        var node = node_
        var disc = discre
        disc = if(disc == keys.length-1) 0  else disc+1
        if(node == null){
            println("Erro!!!")
            return node
        }else{
            println("Ponto d einicio da busca: " + node_.chaves)
            println(keys(disc) + " - " + node.chaves(disc) + " - " + disc)
        }
        if(node.chaves == keys){
            println("Encontramos o node para remover!")
            if(node.node_dir != null){
                node = node.node_dir.findmin()
                println("min encontrado dir: "+node.chaves)
                node.node_dir = if(node.node_dir == null) null else delete(node.node_dir.chaves, node.node_dir, disc)
            }else if(node.node_esq != null){
                node = node.node_esq.findmin()
                println("min encontrado esq: "+node.chaves)
                node.node_esq = if(node.node_esq == null) null else delete(node.node_esq.chaves, node.node_esq, disc)
            }else{
                node = null
            }
        }else if (keys(disc) < node.chaves(disc)){
            node.node_esq = delete(keys, node.node_esq, disc)
        }else{
            node.node_dir = delete(keys, node.node_dir, disc)
        }
        return node
    }
}

class KDnode(var disc: Int, var keys: ListBuffer[Int], var node_es: KDnode = null, var node_di: KDnode = null){
    var discriminante = if (disc > keys.length - 1) 0 else disc
    var chaves = keys
    var node_dir = node_di
    var node_esq = node_es

    def findmin(d_idx: Int = discriminante): KDnode = {
        var loop = true
        var null_node = new KDnode(0, ListBuffer(0))
        
        // if(node_dir == null){
        //     println("Nao ha findmin para este node!")
        //     return null_node
        // }

        if(d_idx == discriminante){
            return if(node_esq == null) new KDnode(d_idx, keys, node_esq, node_dir) else node_esq.findmin(if (d_idx == keys.length-1) 0 else d_idx + 1)
        }else{
            return min_node(
                node_esq.findmin(if (d_idx == keys.length-1) 0 else d_idx + 1),
                node_dir.findmin(if (d_idx == keys.length-1) 0 else d_idx + 1),
                if (d_idx == keys.length-1) 0 else d_idx + 1
            )
        }
        
        
        
    }
}

def min_node(node1: KDnode, node2: KDnode, dim: Int): KDnode = {
    if(node1 == null && node2 != null){
        return node2
    }
    if(node1 != null && node2 == null){
        return node1
    }
    return if(node1.chaves(dim) < node2.chaves(dim)) node1 else node2
}

def testes_remo1(): Unit = {
    var kd = new KDtree()
    var keys = ListBuffer(1,2,3)
    kd.add_node(keys)
    kd.add_node(ListBuffer(2, 5, 0))
    kd.add_node(ListBuffer(0, 5, 0))
    kd.add_node(ListBuffer(1, 7, 0))
    kd.add_node(ListBuffer(1, 8, 0))
    kd.add_node(ListBuffer(0, 6, 7))
    kd.add_node(ListBuffer(0, 4, 7))
    
    println("Antes da remocao: " + kd.raiz.node_esq.node_esq.chaves)
    kd.delete(ListBuffer(0,4,7), kd.raiz, 2)
    println("Apos remocao: " + kd.raiz.node_esq.node_esq)
}

def testes_remo2(): Unit = {
    var kd = new KDtree()
    var keys = ListBuffer(1,2,3)
    kd.add_node(keys)
    kd.add_node(ListBuffer(2, 5, 0))
    kd.add_node(ListBuffer(0, 5, 0))
    kd.add_node(ListBuffer(1, 7, 0))
    kd.add_node(ListBuffer(1, 8, 0))
    kd.add_node(ListBuffer(0, 6, 7))
    kd.add_node(ListBuffer(0, 4, 7))
    
    println("Antes da remocao: " + kd.raiz.chaves)
    kd.delete(ListBuffer(1,2, 3), kd.raiz, 2)
    println("Apos remocao: " + kd.raiz)
}

object Main_code {
  def main(args: Array[String]) = {
    var kd = new KDtree()
    var keys = ListBuffer(1,2,3)
    kd.add_node(keys)
    kd.add_node(ListBuffer(2, 5, 0))
    kd.add_node(ListBuffer(0, 5, 0))
    kd.add_node(ListBuffer(1, 7, 0))
    kd.add_node(ListBuffer(1, 8, 0))
    kd.add_node(ListBuffer(0, 6, 7))
    kd.add_node(ListBuffer(0, 4, 7))
    
    // var findmin_ = kd.raiz.node_dir.findmin() //kd.look_for(ListBuffer(1, 2, 3)).findmin()
    // println(findmin_.chaves)
    //var sucessor_raizdir = kd.raiz.node_dir.sucessor()
    //println(sucessor_raizdir.chaves)
    
    println("Antes da remocao: " + kd.raiz.chaves)
    kd.delete(ListBuffer(1,2, 3), kd.raiz, 2)
    println("Apos remocao: " + kd.raiz.chaves)
    
  }
}