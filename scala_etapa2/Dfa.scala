import scala.collection.mutable.Set
import scala.collection.mutable.Map

class Dfa[A] (var states: Set[A], var init: A, var fin: Set[A],
              var alphabet: Set[Char], var tr_fct: Map[(A, Char), A]){

  /*
      The following methods are only the methods directly called by the
      test suite. You can (and should) define more.
      Asta o initializez aici cu init ca sa nu mai tipe compilatorul, ca
      oricum ii dau valoarea reala in cod.
  */
  var sink_state: A = init

  def map[B](f: A => B) : Dfa[B] = {
    // TODO implement map
    var dfaA = new Dfa[A](states, init, fin, alphabet, tr_fct)

    var new_states: Set[B] = states.map(f)
    var new_init: B = f(init)
    var new_fin: Set[B] = fin.map(f)
    var new_tr_fct: Map[(B, Char), B] = tr_fct map {
      case (pair, next_state) => ((f(pair._1), pair._2), f(next_state))
    }

    var dfaB = new Dfa[B](new_states, new_init, new_fin, alphabet, new_tr_fct)
    dfaB.sink_state = f(dfaA.sink_state)

    dfaB
  }

  def next(state:A, c: Char): A = {
    // TODO implement next
    tr_fct.get((state, c)) match {
      case None => sink_state
      case Some(s) => s
    }
  }

  def accepts(str: String): Boolean = {
    // TODO implement accepts
    var i: Int = 0;
    var nr_inputs: Int = 0
    var current_state: A = init;
    var list_of_inputs: Map[Int, Char] = Map();
    var accept: Boolean = false

    if (str.length == 0) {
      return fin.contains(init)
    } else {
      //Numar cate inputuri primeste si creez dictionarul de inputuri
      while (i < str.length) {
        nr_inputs += 1
        if ((str.length - i >= 3) && str.charAt(i) == 'e' && str.charAt(i + 1) == 'p' && str.charAt(i + 2) == 's') {
          list_of_inputs += (nr_inputs -> Char.MinValue)
          i += 3;
        } else {
          list_of_inputs += (nr_inputs -> str.charAt(i))
          i += 1;
        }
      }

      /*
         Iterez prin inputurile primite si aflu multimea starilor in care
         se ajunge dupa procesarea fiecarui input.
      */
      i = 1;
      while (i <= nr_inputs) {
        var c: Char = list_of_inputs(i)
        current_state = next(current_state, c)
        if (current_state == sink_state) {
          return false
        }
        i += 1
      }
    }

    /*
      Verific daca vreuna din starile in care s-a ajuns dupa procesarea
      tuturor caracterelor este finala.
    */
    if (fin.contains(current_state)) {
      accept = true
    }
    accept
  }

  def getStates : Set[A] = {
    // TODO implement getStates
    states
  }

  def isFinal(state: A): Boolean = {
    // TODO implement isFinal
    fin.contains(state)
  }
}

/*
   This is a companion object to the Dfa class. This allows us to call the method
   fromPrenex without instantiating the Dfa class beforehand.
   You can think of the methods of this object like static methods of the Dfa class
*/
object Dfa {
  var nr_of_state = -1
  var set_of_states: Set[Set[Int]] = Set()

  def get_state_nr: Int = {
    nr_of_state += 1
    nr_of_state
  }

  def map_to_int(value: Map[Set[Int], Int], value1: Set[Int]): Int = {
    value.get(value1) match {
      case None => -1
      case Some(s: Int) => s
    }
  }

  def create_DFA_from_NFA(nfa: Nfa[Int]): Dfa[Int] = {
      //The set of initial states of the Nfa
      val initial_states: Set[Int] = Set(nfa.init) ++ nfa.next(nfa.init, Char.MinValue)
      val initial_DFA_repr : Map[(Set[Int], Char), Set[Int]] = Map()
      var copy_set_of_states: Set[Set[Int]] = Set()

      /////////////////////////////////////////////////
      //Aici creez multimea de seturi de stari carora li se va asocia un id unic in forma finala a Dfaului
      set_of_states += initial_states   //Initial state

      do {
        var next_states: Set[Set[Int]] = Set()
        copy_set_of_states = set_of_states

        /*
          Primul foreach parcurge multimile de stari fiecare fiind echivalenta cu o stare din dfa.
          Al 2lea foreach parcurge caractrerele din alfabet pentru fiecare stare de stari si
          apeleaza next_states pe states si caracter
        */
        set_of_states.foreach(states =>
                                    nfa.alphabet.foreach(c =>
                                                          if(c != Char.MinValue) next_states = next_states ++ Set(nfa.next_states(states, c))))

        set_of_states = set_of_states ++ next_states
      } while(set_of_states.size != copy_set_of_states.size)

      set_of_states += Set()            //Sink state
      /////////////////////////////////////////////////
      /////////////////////////////////////////////////
      //Creez mapul de tranzitii
      var next_states: Set[Int] = Set()
      set_of_states.foreach(states => nfa.alphabet.foreach(c => {
                                                                    if (c != Char.MinValue) {
                                                                      next_states = nfa.next_states(states, c)
                                                                      initial_DFA_repr += ((states, c) -> next_states)
                                                                    }
                                                                }
                                                            ))
      /////////////////////////////////////////////////

      var codify_states: Map[Set[Int], Int] = Map()
      var states: Set[Int] = Set()
      var init_state: Int = nr_of_state
      var fin: Set[Int] = Set()
      var alphabet: Set[Char] = nfa.alphabet
      var tr_fct: Map[(Int, Char), Int] = Map()

      set_of_states.foreach(sts => codify_states += (sts -> Dfa.get_state_nr))
      //Aici nu mai iau in considerare si cazul de None pt. ca nu o sa fie cazul.
      set_of_states.foreach(sts => states = states ++ codify_states.get(sts))
      init_state = codify_states.get(initial_states) match {
        case None => -1
        case Some(s: Int) => s
      }
      set_of_states.foreach(sts =>
                                  sts.foreach(s =>
                                                    if(s == nfa.fin) {
                                                      fin = fin ++ codify_states.get(sts)
                                                    }
                                              )
                            )
      tr_fct = initial_DFA_repr map {
        case (pair, setStates) =>
                                (((map_to_int(codify_states, pair._1), pair._2)), map_to_int(codify_states, setStates))
      }

      var dfa: Dfa[Int] = new Dfa[Int](states, init_state, fin, alphabet, tr_fct)
      dfa.sink_state = codify_states.get(Set()) match {
        case None => -1
        case Some(s) => s
      }

      dfa
  }

  def fromPrenex(str: String): Dfa[Int] = {
    Dfa.nr_of_state = -1
    Nfa.nr_of_state = -1
    // TODO implement Prenex -> Dfa transformation. hint: you should make use of Nfa.fromPrenex to build the Dfa
    var dfa: Dfa[Int] = new Dfa[Int](Set(), -1, Set(), Set(), Map())
    var nfa: Nfa[Int] = Nfa.fromPrenex(str)

    dfa = create_DFA_from_NFA(nfa)

    Dfa.nr_of_state = -1
    Nfa.nr_of_state = -1

    dfa
  }
}
