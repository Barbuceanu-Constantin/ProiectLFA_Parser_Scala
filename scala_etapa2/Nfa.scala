import scala.collection.mutable.Queue
import scala.collection.mutable.Set
import scala.collection.mutable.Map

class Node(var value:String, var left:Node, var right:Node)

class AST {
  var root : Node = new Node(null, null, null);

  def generate(queue:Queue[String], node:Node): Unit = {
    if(queue.isEmpty) return

    var value = queue.dequeue()
    node.value = value
    if(!(value.equals("STAR") || value.equals("UNION") || value.equals("CONCAT")))
      return;

    node.left = new Node(null, null, null)
    generate(queue, node.left)
    if(value.equals("STAR"))
      return;

    node.right = new Node(null, null, null)
    generate(queue, node.right)
  }

  def show(node: Node): Unit = {
    if(node == null) return
    println(node.value)
    show(node.left)
    show(node.right)
  }
}

class Nfa[A](var states: Set[A], var init: A, var fin: A, var alphabet: Set[Char],
             var tr_fct: Map[(A, Char), Set[A]]) {

  // The following methods are only the methods directly called by the test suite. You can (and should) define more.
  def map[B](f: A => B) : Nfa[B] = {
    // TODO implement map
    var new_states: Set[B] = states.map(f)
    var new_init: B = f(init)
    var new_fin: B = f(fin)
    var new_tr_fct: Map[(B, Char), Set[B]] = tr_fct map {
      case (state_character_pair, set_of_next_states) => ((f(state_character_pair._1), state_character_pair._2), set_of_next_states.map(f))
    }

    var nfa = new Nfa[B](new_states, new_init, new_fin, alphabet, new_tr_fct)
    nfa
  }

  def one_step(state: A, c: Char): Set[A] = {
    tr_fct.get((state, c)) match {
      case None => Set()
      case Some(s: Set[A]) => s
    }
  }

  def ch_next_states(init_states: Set[A], c: Char): Set[A] = {
    var current_states: Set[A] = Set()
    var copy_current_states: Set[A] = Set()
    var c_init_states = init_states

    do {
      copy_current_states = current_states
      current_states = Set()
      c_init_states.foreach(s => {
                                  current_states = current_states ++ one_step(s, c);
                                  if(one_step(s, c).size == 0)
                                    current_states = current_states ++ Set(s)
                                  })
      c_init_states = current_states
    } while (!copy_current_states.equals(current_states))

    copy_current_states
  }

  def next(state:A, c: Char): Set[A] = {
    // TODO implement next
    var next_states : Set[A] = Set()
    var epsilon_closure_states : Set[A] = Set()
    var epsilon_closure_states2 : Set[A] = Set()
    var c_next_states : Set[A] = Set()

    next_states = tr_fct.get((state, c)) match {
      case None => {
        epsilon_closure_states = ch_next_states(Set(state), Char.MinValue)
        if(epsilon_closure_states.size != 0) {
          epsilon_closure_states.foreach(s => c_next_states = c_next_states ++ one_step(s, c))
          epsilon_closure_states2 = ch_next_states(c_next_states, Char.MinValue)
          if(epsilon_closure_states2.size == 0)
            c_next_states
          else
            epsilon_closure_states2
        } else
          Set()
      }
      case Some(states: Set[A]) => {
        c_next_states = states
        epsilon_closure_states = ch_next_states(c_next_states, Char.MinValue)
        if (epsilon_closure_states.size != 0) {
          c_next_states = epsilon_closure_states
        }
        c_next_states
      }
    }

    next_states
  }

  def next_states(current_states: Set[A], character: Char) : Set[A] = {
    var next_st : Set[A] = Set()

    current_states.foreach(state => next_st = next_st ++ next(state, character))

    next_st
  }

  def accepts(str: String): Boolean = {
    // TODO implement accepts
    var i: Int = 0;
    var nr_inputs: Int = 0
    var current_states: Set[A] = Set(init);
    var list_of_inputs: Map[Int, Char] = Map();
    var accept: Boolean = false

    if (str.length == 0) {
      var next_states: Set[A] = tr_fct.get((init, Char.MinValue)) match {
        case None => current_states
        case Some(s: Set[A]) => s
      }
      next_states.foreach(x => accept = x == fin)
    } else {
      //Numar cate inputuri primeste si creez dictionarul de inputuri
      while (i < str.length) {
        if (str.charAt(i) == 'e' && str.charAt(i + 1) == 'p' && str.charAt(i + 2) == 's') {
          nr_inputs += 1
          list_of_inputs += (nr_inputs -> Char.MinValue)
          i += 3;
        } else {
          nr_inputs += 1
          list_of_inputs += (nr_inputs -> str.charAt(i))
          i += 1;
        }
      }

      /*
        Iterez prin inputurile primite si aflu multimea starilor
        in care se ajunge dupa procesarea fiecarui input.
      */
      i = 1;
      while (i <= nr_inputs) {
        var c: Char = list_of_inputs(i)
        current_states = next_states(current_states, c)
        if(current_states.size == 0) {
          return false
        }
        i += 1
      }

      /*
        Verific daca vreuna din starile in care s-a ajuns dupa
        procesarea tuturor caracterelor este finala.
      */
      current_states.foreach(x => accept = x == fin)
    }

    accept
  }

  def getStates : Set[A] = {
    // TODO implement getStates
    states
  }

  def isFinal(state: A): Boolean = {
    // TODO implement isFinal
    state == fin
  }
}

/*
    This is a companion object to the Nfa class. This allows us to call the
    method fromPrenex without instantiating the Nfa class beforehand.
    You can think of the methods of this object like static methods of the Nfa class
*/
object Nfa {
  var nr_of_state = -1

  def get_state_nr: Int = {
    nr_of_state += 1
    nr_of_state
  }

  def base_case(node: Node): Nfa[Int] = {
    var nfa: Nfa[Int] = new Nfa[Int](Set(), -1, -1, Set(), Map())
    if (node.value.equals("void")) {
      //St. init., St. fin., stari, nu se adauga niciun caracter la alfabet si nici la tr_fct
      nfa.init = get_state_nr
      nfa.fin = get_state_nr
      nfa.states = Set(nfa.init, nfa.fin)
      nfa.alphabet = Set()
      nfa.tr_fct = Map()
    } else if (node.value.equals("eps")) {
      nfa.init = get_state_nr
      nfa.fin = nfa.init
      nfa.states = Set(nfa.init)
      nfa.tr_fct = Map()
      nfa.alphabet += Char.MinValue
    } else if (node.value.equals("space")) {
      nfa.init = get_state_nr
      nfa.fin = get_state_nr
      nfa.states = Set(nfa.init, nfa.fin)
      nfa.tr_fct += ((nfa.init, ' ') -> Set(nfa.fin))
      nfa.alphabet += ' '
    } else if(node.value.equals("\\n")) {
      nfa.init = get_state_nr
      nfa.fin = get_state_nr
      nfa.states = Set(nfa.init, nfa.fin)
      nfa.tr_fct += ((nfa.init, '\n') -> Set(nfa.fin))
      nfa.alphabet += '\n'
    } else if(node.value.equals("\\t")) {
      nfa.init = get_state_nr
      nfa.fin = get_state_nr
      nfa.states = Set(nfa.init, nfa.fin)
      nfa.tr_fct += ((nfa.init, '\t') -> Set(nfa.fin))
      nfa.alphabet += '\t'
    } else {
      nfa.init = get_state_nr
      nfa.fin = get_state_nr
      nfa.states = Set(nfa.init, nfa.fin)
      nfa.tr_fct += ((nfa.init, node.value.charAt(0)) -> Set(nfa.fin))
      nfa.alphabet += node.value.charAt(0)
    }
    nfa
  }

  def STAR(nfa1: Nfa[Int]): Nfa[Int] = {
    var nfa: Nfa[Int] = new Nfa[Int](Set(), -1, -1, Set(), Map())
    nfa.init = get_state_nr
    nfa.fin = get_state_nr
    nfa.states = nfa1.states
    nfa.tr_fct = nfa1.tr_fct

    nfa.states += nfa.init
    nfa.states += nfa.fin

    nfa.tr_fct += ((nfa.init, Char.MinValue) -> Set(nfa1.init, nfa.fin))
    nfa.tr_fct += ((nfa1.fin, Char.MinValue) -> Set(nfa1.init, nfa.fin))

    nfa.alphabet += Char.MinValue
    nfa.alphabet = nfa.alphabet ++ nfa1.alphabet

    nfa
  }

  def CONCAT(nfa1: Nfa[Int], nfa2: Nfa[Int]): Nfa[Int] = {
    var nfa: Nfa[Int] = new Nfa[Int](Set(), -1, -1, Set(), Map())
    nfa.init = nfa1.init
    nfa.fin = nfa2.fin
    nfa.states = nfa1.states ++ nfa2.states
    nfa.tr_fct = nfa1.tr_fct .++ (nfa2.tr_fct)

    nfa.tr_fct += ((nfa1.fin, Char.MinValue) -> Set(nfa2.init))

    nfa.alphabet += Char.MinValue
    nfa.alphabet = nfa.alphabet ++ nfa1.alphabet
    nfa.alphabet = nfa.alphabet ++ nfa2.alphabet

    nfa
  }

  def UNION(nfa1: Nfa[Int], nfa2: Nfa[Int]): Nfa[Int] = {
    var nfa: Nfa[Int] = new Nfa[Int](Set(), -1, -1, Set(), Map())
    nfa.init = get_state_nr
    nfa.fin = get_state_nr
    nfa.states = nfa1.states ++ nfa2.states
    nfa.states = nfa.states ++ Set(nfa.init, nfa.fin)
    nfa.tr_fct = nfa1.tr_fct .++ (nfa2.tr_fct)

    nfa.tr_fct += ((nfa.init, Char.MinValue) -> Set(nfa1.init, nfa2.init))
    nfa.tr_fct += ((nfa1.fin, Char.MinValue) -> Set(nfa.fin))
    nfa.tr_fct += ((nfa2.fin, Char.MinValue) -> Set(nfa.fin))

    nfa.alphabet += Char.MinValue
    nfa.alphabet = nfa.alphabet ++ nfa1.alphabet
    nfa.alphabet = nfa.alphabet ++ nfa2.alphabet

    nfa
  }

  def createNFA_Components(node: Node): Nfa[Int] = {
    var nfa: Nfa[Int] = new Nfa[Int](Set(), -1, -1, Set(), Map())
    if (node.left == null && node.right == null) {
      nfa = base_case(node)
      return nfa
    }

    var nfa1 = createNFA_Components(node.left)
    if (node.value.equals("STAR")) {
      nfa = STAR(nfa1)
      return nfa
    }

    var nfa2 = createNFA_Components(node.right)
    if (node.value.equals("CONCAT")) {
      nfa = CONCAT(nfa1, nfa2)
      return nfa
    } else if(node.value.equals("UNION")) {
      nfa = UNION(nfa1, nfa2)
      return nfa
    }

    return nfa
  }

  def special_char(str: String): Boolean = {
    if (str.size == 1)
      if (str.charAt(0) == 9 || str.charAt(0) == 13 || str.charAt(0) == 10 || str.charAt(0) == 34) {
        return true
      }
    false
  }

  def fromPrenex(str: String): Nfa[Int] = {
    Nfa.nr_of_state = -1
    Dfa.nr_of_state = -1
    // TODO implement Prenex -> Nfa transformation.
    //Fac split dupa spatiu in stringul in forma prenex.
    val splitted_array =  if (!str.contains("' '")) {
                            if(!special_char(str)) {
                              //Aici se intra daca nu este vorba de un caracter special
                              str.split("\\s+")
                            } else {
                              //Aici se intra in restul cazurilor
                              str.split("");
                            }
                          } else {
                            val str2 = str.replaceAll("\\' \\'", "space")
                            str2.split("\\s+")
                          }

    val queue = new Queue[String]
    var nfa = new Nfa[Int](Set(), -1, -1, Set(), Map())
    val ast = new AST
    //Pun toate lexemele formate dupa split in ordine de la stanga la dreapta intr-o coada.
    for (lexema <- splitted_array) {
      queue.enqueue(lexema)
    }

    //Extrag unul cate unul elementele din coada si le introduc in AST.
    ast.generate(queue, ast.root)

    //Creez componentele din definitia nfa-ului.
    nfa = createNFA_Components(ast.root)
    Nfa.nr_of_state = -1
    Dfa.nr_of_state = -1

    nfa
  }

  // You can add more methods to this object
}