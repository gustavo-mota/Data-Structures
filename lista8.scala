// árvore binária de busca

class arv_binbus(var valor: Int){
    var raiz = node_binbus(valor, true)
    raiz.filho_esq = retorna_folha(raiz)
    raiz.filho_dir = retorna_folha(raiz)
    
    def retorna_folha(pai: node_binbus): node_binbus = {
        var folha = node_binbus(-1, false)
        folha.node_nill = true
        folha.pai = pai
        return folha
    }

    def insere(binbus : arv_binbus, valor: Int): arv_binbus = {
        var new_raiz = insere_rec(binbus.raiz, valor, true)
        binbus.raiz = new_raiz
        return binbus
    }

    def insere_rec(node: node_binbus, valor: Int, raiz_ : Boolean): node_binbus = {
        var new_node = node
        if(node.value < valor){
            // inser a esq
            if(node.filho_esq.node_nill){
                // é agora a inserção
                new_node.filho_esq = node_binbus(valor, false)
                new_node.filho_esq.pai = new_node
                new_node.filho_esq.filho_esq = retorna_folha(new_node.filho_esq)
                new_node.filho_esq.filho_dir = retorna_folha(new_node.filho_esq)
            }else{
                // continua pela esq
                new_node.filho_esq = insere_rec(new_node.filho_esq, valor, false)
            }
        }else{
            // inserir a dir
            if(node.filho_dir.node_nill){
                // é agora a inserção
                new_node.filho_dir = node_binbus(valor, false)
                new_node.filho_dir.pai = new_node
                new_node.filho_dir.filho_esq = retorna_folha(new_node.filho_dir)
                new_node.filho_dir.filho_dir = retorna_folha(new_node.filho_dir)
            }else{
                // continua pela dir
                new_node.filho_dir = insere_rec(new_node.filho_dir, valor, false)
            }
        }
        return new_node
    }

    def remove(binbus : arv_binbus, valor: Int): arv_binbus = {
        println("Inciando remocao")
        var new_raiz = remove_rec(binbus.raiz, valor, true)
        binbus.raiz = new_raiz
        return binbus
    }

    def remove_rec(node: node_binbus, valor: Int, raiz_ : Boolean): node_binbus = {
        if(node.value != valor){
            // segue a busca
            println("Segue a busca")
            if(node.filho_esq.node_nill && node.filho_dir.node_nill){
                // não temos o elemento que desejamos eliminar
                println("O elemento não existe na árvore")
                return node
            }else{
                // temos por onde continuar buscando
                if(node.value < valor){
                    // certamente o elemtno a elimianr esta a esq
                    return remove_rec(node.filho_esq, valor, false)
                }else{
                    // certamente o elemtno a elimianr esta a dir
                    return remove_rec(node.filho_dir, valor, false)
                }
            }
        }else{
            // achamos
            if(node.filho_esq.node_nill && node.filho_dir.node_nill){
                // é um nó folha
                node.make_folha()
                node.filho_dir = null
                node.filho_esq = null
                return node
            }else{
                println("Node nao e folha")
                if(node.filho_esq.node_nill == false && node.filho_dir.node_nill == false){
                    // achar sucessor
                    return node
                }else if(node.filho_esq.node_nill){
                    // filho a esquerda é nulo
                    return node
                }else{
                    // filho a direita é nulo
                    return node
                }
            }
        }
    }
}

class node_binbus(val valor: Int, val raiz_ : Boolean){
    var value = valor
    var pai: node_binbus = null
    var filho_esq: node_binbus = null
    var filho_dir: node_binbus = null
    var node_nill = false
    var raiz = raiz_

    def make_folha(): Unit = {
        node_nill = true
        value = -1   
    }
}

def printa_arv(node: node_binbus, counter: Int): Unit = {
    println("Node numero " + counter + " Value: " + node.value)
    if(node.filho_esq.node_nill ==  false){
        printa_arv(node.filho_esq, counter+1)
    }else{
        println("Subarvore esquerda chegou ao fim")
    }

    if(node.filho_dir.node_nill ==  false){
        printa_arv(node.filho_dir, counter+1)
    }else{
        println("Subarvore direita chegou ao fim")
    }
    
}

object Main_code {
  def main(args: Array[String]) = {
    var av_binbus = new arv_binbus(2)
    var new_av = av_binbus.insere(av_binbus, 3)
    //new_av = new_av.remove(new_av, 3)
    printa_arv(av_binbus.raiz, 0)
    //printa_arv(new_av.raiz, 0)
  }
}