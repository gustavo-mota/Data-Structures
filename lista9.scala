// árvore avl

class avl(var valor: Int){
    var raiz = node_avl(valor, 0, true)
    raiz.filho_esq = retorna_folha(raiz)
    raiz.filho_dir = retorna_folha(raiz)
    
    def retorna_folha(pai: node_avl): node_avl = {
        var folha = node_avl(-1, 0, false)
        folha.node_nill = true
        folha.pai = pai
        return folha
    }
}

    /*def insere(avl_ : avl, valor: Int): avl = {
        new_raiz = insere_rec(valor, avl_.raiz)
        new_avl.raiz = new_raiz
        return new_avl    
    }

    def insere_rec(node: node_avl, valor: Int): node_avl = {

    }*/

class node_avl(val valor: Int, val fb: Int, val raiz_ : Boolean){
    var value = valor
    var h = 0
    var fb = fb
    var pai: node_avl = null
    var filho_esq: node_avl = null
    var filho_dir: node_avl = null
    var node_nill = false
    var raiz = raiz_

    def make_folha(): Unit = {
        node_nill = true
        value = -1   
        fb = -1
        h = 0
    }
}

object Main_code {
  def main(args: Array[String]) = {
    println("Aqui")
  }
}