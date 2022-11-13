class reversed_tree() {
  var raiz = rt_node(-1, null, true)
  var node_uno: rt_node = null

  def MakeSet(x: Int): Unit = {
    println("Criando um conjunto de um so elemento!")
    node_uno = rt_node(x, raiz, false)
    println("Criado!")
  }

  def Union(y: reversed_tree): Unit = {
    println("Unindo esta arvore a outra estrutura!")
    raiz = y.node_uno
    println("Uniao concluida!")
  }
}

class rt_node(var value: Int, var father: rt_node, var root: Boolean) {
  var raiz = root // bool
  var valor = value
  var pai = father

  def find(): rt_node = {
    println(
      "Find() a partir de um elemento inciado! Buscando a particao ao qual faz parte..."
    )
    var check = true
    var node_pai = pai
    var node_valor = valor
    var node_ret = pai
    while (check) {
      if (node_pai.pai != null) {
        println("A busca continua!")
        node_valor = node_pai.valor
        node_ret = node_pai
        node_pai = node_pai.pai

      } else {
        println("A busca terminou! ")
        println("O elemento pertence a seguinte particao: " + node_valor)
        check = false
      }
    }
    return node_ret
  }
}

def check_part(node_1: rt_node, node_2: rt_node): Boolean = {
    var node_p1 = node_1.find()
    var node_p2 = node_2.find()
    if(node_p1 == node_p2){
        return true
    }
    return false
}

object Main_code {
  def main(args: Array[String]) = {
    //
    // if (conjunto_binario.checa_pertence("a")) println("Contem!!") else println("Nao contem")
    // hashtable.inserir(5)
    var rev_t = reversed_tree()
    rev_t.MakeSet(1)
    rev_t.node_uno.find()
  }
}
