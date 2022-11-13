// árvore rubro-negra sem exclusão de nos
class arv_rn(var valor: Int){
    var raiz = node_arv_rn(valor, "black", true)
    raiz.filho_esq = retorna_folha(raiz)
    raiz.filho_dir = retorna_folha(raiz)

    def retorna_folha(pai: node_arv_rn): node_arv_rn = {
        var folha = node_arv_rn(-1, "black", false)
        folha.node_nill = true
        folha.pai = pai
        return folha
    }

    def retorna_tio(node: node_arv_rn): node_arv_rn = {
        var pai = node.pai
        var avo = pai.pai
        if(avo.filho_esq == pai){
            return avo.filho_dir
        }
        return avo.filho_esq
    }

    def retorna_lado(node: node_arv_rn): String = {
        var pai = node.pai
        if(pai.filho_esq == node){
            return "esq"
        }
        return "dir"
    }

    def rota_simples(lado: String, node: node_arv_rn, pai: node_arv_rn, avo: node_arv_rn, tio: node_arv_rn): node_arv_rn = {
        //já sei que o novo no e seu pai são filhos, respectivamente, no mesmo sentido em relação a seus pais
        // correção de cores
        pai.cor = "black"
        var pai_lado = retorna_lado(pai)
        pai.pai.cor = "red"
        // rotação finalmente,na subarvore com raiz em avo
        if(pai_lado == "esq"){
            pai.filho_dir = pai.pai
            pai.pai.filho_esq = retorna_folha(avo)
            //avo.filho_esq = retorna_folha(avo)
        }else{
            pai.filho_esq = pai.pai
            pai.pai.filho_dir = retorna_folha(avo)
        }
        
        return pai // a nova subarvore enraizada no antigo pai que passou a ser o avo
    }

    def rota_dupla( lado_novoEpai: String, lado_paiEavo: String, 
                    novo: node_arv_rn, pai: node_arv_rn, 
                    avo: node_arv_rn, tio: node_arv_rn): node_arv_rn = {
        // sabemos que o novo no e filho em um sentido, e seu pai (em relação ao avo) e em outro sentido
        // rotacionando pai
        if(lado_novoEpai == "dir"){
            println("Rotacionando pai para a esq")
            novo.filho_esq = novo.pai
        }else{
            println("Rotacionando pai para a dir")
            novo.filho_dir = novo.pai
        }
        novo.pai = novo.pai.pai
        novo.pai.pai = novo
        novo.pai.filho_esq = retorna_folha(novo.pai)
        novo.pai.filho_dir = retorna_folha(novo.pai)
        // rotacionando avo
        novo.cor = "black"
        novo.pai.cor = "red"
        var bisa = novo.pai.pai
        if(lado_paiEavo == "esq"){
            novo.filho_dir = novo.pai
            novo.pai.pai = novo 
            novo.pai.filho_esq = retorna_folha(novo.pai.pai)
            novo.pai = bisa // !!!
        }else{
            novo.filho_esq = novo.pai
            novo.pai.pai = novo 
            novo.pai.filho_dir = retorna_folha(novo.pai.pai)
            novo.pai = bisa // !!!
        }
        return novo // nova subarvore enraizada em novo no inserido
    }

    def insercao(valor: Int): Unit = {
        // determina onde vai estar o novo no, primeiramente
        var atual = raiz
        var fim = false
        var novo: node_arv_rn = null
        while(fim==false){
            if(atual.raiz){ // caso seja a raiz
                if(atual.value < valor ){
                    atual = atual.filho_dir
                }else{
                    atual = atual.filho_esq
                }
            }else{ // não se trata da raiz
                if(atual.node_nill){ // chegamos a uma folha
                    atual = atual.pai
                    if(atual.value < valor){ // estará na esquerda
                        novo = node_arv_rn(valor, "red", false)
                        novo.filho_esq = retorna_folha(novo)
                        novo.filho_dir = retorna_folha(novo)
                        novo.pai = atual
                        atual.filho_esq = novo
                        fim = true
                    }else{ // estará na direita
                        novo = node_arv_rn(valor, "red", false)
                        novo.filho_esq = retorna_folha(novo)
                        novo.filho_dir = retorna_folha(novo)
                        novo.pai = atual
                        atual.filho_dir = novo
                        fim = true
                    }
                }else{ // não chegamos a uma folha
                    if(atual.value < valor){ // ele deverá estar à esquerda
                        atual = atual.filho_esq
                    }else{
                        atual = atual.filho_dir // ele deverá estar a direita
                    }
                }
            }
        }
        println("Insercao de " + valor +  " realizada, ajustes de cores em andamento...")
        if(novo.pai.raiz){
            println("Pai do no e uma raiz, nada a se fazer")
        }else{
            if(novo.pai.cor == "red"){ 
                println("Avaliando o tio do no inserido...")
                var tio: node_arv_rn = retorna_tio(novo)
                var tio_nill = tio.node_nill
                if(tio.cor == "red"){ // caso 2
                    println("Tio não e um node folha e e red, corrigindo cores...")
                    if(tio == novo.pai.pai.filho_esq && tio != novo){
                        println("O tio e o filho da esq do avo, cor corrigida")
                        novo.pai.pai.filho_esq.cor = "black"
                    }else{
                        println("O tio e o filho da dir do avo, cor corrigida")
                        novo.pai.pai.filho_dir.cor = "black"
                    }
                    novo.pai.cor = "black"
                    if(novo.pai.pai.raiz){
                        println("O avo e uma raiz, sem correcoes de cores por enquanto")
                    }else{
                        novo.pai.pai.cor = "red"
                        println("O avo n e uma raiz, correcao de cor realizada")
                    }
                    // falta reverificar os ancestrais do avo em caso de serem dois reds seguidos
                }else{ // caso 3
                    if(tio_nill){
                        println("O tio e um node folha, rotacionando...")
                    }else{
                        println("Tio preto e n e um node folha, corrigindo cores e rotacionando...")
                    }
                }
            }
        }
    }
}

class node_arv_rn(val valor: Int, val color: String, val raiz_ : Boolean){
    var cor = color
    var value = valor
    var pai: node_arv_rn = null
    var filho_esq: node_arv_rn = null
    var filho_dir: node_arv_rn = null
    var node_nill = false
    var raiz = raiz_

    def make_folha(): Unit = {
        node_nill = true
        value = -1
        cor = "black"   
    }
}

def printa_arv(node: node_arv_rn, counter: Int): Unit = {
    println("Node numero " + counter + " Value: " + node.value + " color: " + node.cor)
    if(node.filho_esq.node_nill ==  false){
        printa_arv(node.filho_esq, counter+1)
    }else{
        println("Subarvore esquerda de " + node.value + " chegou ao fim")
    }

    if(node.filho_dir.node_nill ==  false){
        printa_arv(node.filho_dir, counter+1)
    }else{
        println("Subarvore direita de " + node.value + " chegou ao fim")
    }
    
}

object Main_code {
  def main(args: Array[String]) = {
    var new_rn = new arv_rn(2)
    new_rn.insercao(3)
    printa_arv(new_rn.raiz, 0)
    new_rn.insercao(1)
    printa_arv(new_rn.raiz, 0)
    //println(new_rn.raiz.filho_dir.value)
  }
}