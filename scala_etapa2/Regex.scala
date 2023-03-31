import scala.collection.mutable.Stack
import scala.collection.mutable.Queue
import scala.util.matching.Regex

class Prefix_expr {
  var expr_string: String = ""
}

object Regex {
  /*
    This function should:
    -> Classify input as either character(or string) or operator
    -> Convert special inputs like [0-9] to their correct form
    -> Convert escaped characters
  */
  def control_character(c: Char): Boolean = {
    if (c == '(') return true
    if (c == ')') return true
    if (c == '*') return true
    if (c == '|') return true
    false
  }

  def preprocess(s:List[Char]): List[Either[Char,Char]] = {
    /*
      Pune left daca e vorba de un caracter de control
      si right in caz contrar.
    */
    for (c <- s) yield if (control_character(c)) Left(c) else Right(c)
  }

  def precedence(value: String, str: String): Boolean = {
    if(value.equals("CONCAT") && str.equals("|")) return false
    if(value.equals("CONCAT") && str.equals("CONCAT")) return true

    if(value.equals("*") && str.equals("CONCAT")) return false
    if(value.equals("*") && str.equals("|")) return false
    if(value.equals("*") && str.equals("*")) return true

    if(value.equals("|") && str.equals("|")) return true
    if(str.equals((")"))) return false
    true
  }

  def generate_prefix_expr(node: Node, prefix: Prefix_expr): Unit = {
    if(node == null) return
    if(!node.value.equals(" ") && !node.value.equals("\n") && !node.value.equals("\t")) {
      prefix.expr_string = prefix.expr_string.concat(node.value)
      prefix.expr_string = prefix.expr_string.concat(" ")
    } else {
      if(node.value.equals(" ")) {
        prefix.expr_string = prefix.expr_string.concat("'" + node.value + "' ")
      } else if(node.value.equals("\n")) {
        prefix.expr_string = prefix.expr_string.concat("\\n ")
      } else if(node.value.equals("\t")) {
        prefix.expr_string = prefix.expr_string.concat("\\t ")
      }
    }
    generate_prefix_expr(node.left,prefix)
    generate_prefix_expr(node.right,prefix)
  }

  def f1(value: Either[Char, Char]): Either[String, String] = {
    value match {
      case Left(s) => Left(s.toString)
      case Right(s) => Right(s.toString)
    }
  }

  def add_concat(old_list: List[Either[String, String]]): List[Either[String, String]] = {
    var index: Int = 0
    var last_elem: Either[String, String] = old_list.head
    var new_list: List[Either[String, String]] = List(last_elem)

    for(elem <- old_list.tail) {
      var str_elem: String = ""
      var add: Boolean = false

      elem match {
        case Left(s) => str_elem = s
        case Right(s) => str_elem = s
      }

      last_elem match {
        case Left(s) =>
          if(s.equals("*") && str_elem.equals("(")) {
            add = true
          } else if (s.equals("*") && !control_character(str_elem.charAt(0))) {
            add = true
          } else if (s.equals(")") && str_elem.equals("(")) {
            add = true
          } else if (s.equals(")") && !control_character(str_elem.charAt(0))) {
            add = true
          }
        case _ =>
          if(str_elem.equals("(")) {
            add = true;
          } else if(!control_character(str_elem.charAt(0))) {
            add = true;
          }
      }

      if(add) {
        new_list = new_list ++ List(Left("CONCAT"))
      }

      index += 1
      last_elem = elem
      new_list = new_list ++ List(elem)
    }

    new_list
  }

  def gen_new_list(old_list: List[Either[Char, Char]]): List[Either[String, String]] = {
    //Transform vechile valori in stringuri
    var new_list: List[Either[String, String]] = old_list.map(f1)

    //Adaug CONCAT acolo unde trebuie
    new_list = add_concat(new_list)
    new_list
  }

  def infix_to_prefix(list: List[Either[Char, Char]]): String = {
    var new_list: List[Either[String, String]] = gen_new_list(list)
    var operator_stack: Stack[String] = new Stack();
    var output_stack: Stack[String] = new Stack();
    var output_queue: Queue[String] = new Queue();
    var final_queue: Queue[String] = new Queue();
    val ast = new AST()
    val prefix_expr = new Prefix_expr()

    new_list = new_list.reverse

    //Parcurg new_list de la stanga la dreapta
    for (elem <- new_list) {
      elem match {
        case Right(s) => output_stack.push(s)
        case Left(s) => {
          if (operator_stack.size == 0 || s.charAt(0) == ')') {
            operator_stack.push(s)
          } else {
            if (s.charAt(0) == '(') {
              do {
                output_stack.push(operator_stack.pop)
              } while (operator_stack.top.charAt(0) != ')');
              operator_stack.pop()
            } else if(precedence(s, operator_stack.top)) {
              if(precedence(s, operator_stack.top)) {
                do {
                  output_stack.push(operator_stack.pop)
                } while (operator_stack.size != 0 && precedence(s, operator_stack.top));
                operator_stack.push(s)
              }
            } else if(!precedence(s, operator_stack.top)) {
              /*
                Se intra cand precedence da false,
                adica precedenta e mai mare sau egala, adica se pune in stiva.
              */
              operator_stack.push(s)
            }
          }
        }
      }
    }

    //Extrag operatorii din stiva de operatori care e posibil sa mai fi ramas
    if(operator_stack.size != 0) {
      do {
        output_stack.push(operator_stack.pop)
      } while (operator_stack.size != 0);
    }

    //Creez coada care va fi folosita pentru generarea arborelui
    while(output_stack.size != 0) {
      output_queue.enqueue(output_stack.pop)
    }

    //Mapez peste coada rezultat o functie care transforma operatorii in UNION, CONCAT, STAR.
    while(output_queue.size != 0) {
      var str: String = output_queue.dequeue()
      if(str.equals("|")){
        final_queue.enqueue("UNION")
      } else if(str.equals("*")) {
        final_queue.enqueue("STAR")
      } else {
        final_queue.enqueue(str)
      }
    }

    //Generez arborele
    ast.generate(final_queue, ast.root)
    //ast.show(ast.root)

    generate_prefix_expr(ast.root, prefix_expr)
    prefix_expr.expr_string.dropRight(1)

    prefix_expr.expr_string
  }

  def replaceSpecialChars(str: String): String = {
    var str1:String = str

    //Fac replace pe spatii
    str1 = str1.replaceAll("' '", " ")

    //Fac replace pe newline-uri, taburi, minusuri
    str1 = str1.replaceAll("'\n'", '\n'.toString)
    str1 = str1.replaceAll("'\t'", '\t'.toString)
    str1 = str1.replaceAll("\'-\'", '-'.toString)

    /*
      Fac replace pe orice litera caracter special.
      Pasul 1 e sa fac o lista cu indexul de start al fiecarei astfel de litere.
    */
    var list_of_start_indices: List[Int] = "'[A-Z]|[a-z]'".r.findAllMatchIn(str1).map(_.start).toList

    //Pasul 2 e sa inlocuies fiecare astfel de litera. Folosesc un slice.
    for(i <- list_of_start_indices) {
      str1 = str1.replace(str1.slice(i - 1, i + 2), str1.charAt(i).toString)
    }

    str1
  }

  def aux1(str1: String, list_of_start_indices_copy: List[Int]): String = {
    var replacing_string: String = ""
    var list_of_start_indices: List[Int] = list_of_start_indices_copy

    val initial_letter: Char = str1.charAt (list_of_start_indices.head + 1)
    val final_letter: Char = str1.charAt (list_of_start_indices.head + 3)
    (initial_letter to final_letter).foreach (l =>
                            if (l < final_letter) replacing_string = replacing_string.concat (l.toString.concat ("|") )
                            else replacing_string = replacing_string.concat (l.toString)
                                                  )
    replacing_string
  }

  def replacePlus(str: String): String = {
    var str1: String = str
    var replacing_string: String = ""
    var list_of_start_indices: List[Int] = "\\+".r.findAllMatchIn(str1).map(_.start).toList

    while (list_of_start_indices.size != 0) {
      val prev_char_ind: Int = list_of_start_indices.head - 1
      if(str1.charAt(prev_char_ind) != '\'') {
        if(!control_character(str1.charAt(prev_char_ind))) {
          /*
            If-ul asta ia in considerare doar cazul cand ai caracter+, nu si ()+.
          */
          replacing_string = replacing_string.concat(str1.charAt(prev_char_ind).toString())
          replacing_string = replacing_string.concat(str1.charAt(prev_char_ind).toString())
          replacing_string = replacing_string.concat("*")
          replacing_string = "(".concat(replacing_string).concat(")")
          str1 = str1.replace(str1.slice(prev_char_ind, prev_char_ind + 2), replacing_string)
        } else {
          /*
            If-ul asta ia in considerare cazul cand ai ()+.
            Pas1: Gasesc indexul primei paranteze.
          */
          var start_bracket_ind: Int = prev_char_ind
          while(str1.charAt(start_bracket_ind) != '(') {
            start_bracket_ind -= 1
          }
          replacing_string = replacing_string.concat(str1.slice(start_bracket_ind, prev_char_ind + 1))
          replacing_string = replacing_string.concat(replacing_string)
          replacing_string = replacing_string.concat("*")
          replacing_string = "(".concat(replacing_string).concat(")")
          str1 = str1.replace(str1.slice(start_bracket_ind, prev_char_ind + 2), replacing_string)
          replacing_string = ""
        }
      }
      list_of_start_indices = "\\+".r.findAllMatchIn(str1).map(_.start).toList
    }

    str1
  }

  def replaceQuestionMark(str: String): String = {
    var str1: String = str
    var replacing_string: String = ""
    var list_of_start_indices: List[Int] = "\\?".r.findAllMatchIn(str1).map(_.start).toList

    while (list_of_start_indices.size != 0) {
      val prev_char_ind: Int = list_of_start_indices.head - 1
      if (str1.charAt(prev_char_ind) != '\'') {
        if (!control_character(str1.charAt(prev_char_ind))) {
          /*
            If-ul asta ia in considerare doar cazul cand ai caracter?, nu si ()?.
          */
          replacing_string = replacing_string.concat(str1.charAt(prev_char_ind).toString())
          replacing_string = replacing_string.concat("|")
          replacing_string = replacing_string.concat("eps")
          replacing_string = "(".concat(replacing_string).concat(")")
          str1 = str1.replace(str1.slice(prev_char_ind, prev_char_ind + 2), replacing_string)
        }
        else {
          /*
            If-ul asta ia in considerare cazul cand ai ()?.
            Pas1: Gasesc indexul primei paranteze.
          */
          var start_bracket_ind: Int = prev_char_ind
          while (str1.charAt(start_bracket_ind) != '(') {
            start_bracket_ind -= 1
          }
          replacing_string = replacing_string.concat(str1.slice(start_bracket_ind, prev_char_ind + 1))
          replacing_string = replacing_string.concat("|")
          replacing_string = replacing_string.concat("eps")
          replacing_string = "(".concat(replacing_string).concat(")")
          str1 = str1.replace(str1.slice(start_bracket_ind, prev_char_ind + 2), replacing_string)
          replacing_string = ""
        }
        list_of_start_indices = "\\?".r.findAllMatchIn(str1).map(_.start).toList
      }
    }
    str1
  }

  def replaceSyntacticSugars(str: String): String = {
    var str1:String = str
    var replacing_string: String = ""

    //Expandez [A-Z]
    var list_of_start_indices: List[Int] = "\\[A-Z]".r.findAllMatchIn(str1).map(_.start).toList
    while(list_of_start_indices.size != 0) {
      replacing_string = "(".concat(aux1(str1, list_of_start_indices)).concat(")")
      str1 = str1.replaceAll("\\[A-Z]", replacing_string)
      list_of_start_indices = list_of_start_indices.tail
    }

    //Expandez [a-z]
    replacing_string = ""
    list_of_start_indices = "\\[a-z]".r.findAllMatchIn(str1).map(_.start).toList
    while(list_of_start_indices.size != 0) {
      replacing_string = "(".concat(aux1(str1, list_of_start_indices)).concat(")")
      str1 = str1.replaceAll("\\[a-z]", replacing_string)
      list_of_start_indices = list_of_start_indices.tail
    }

    //Expandez [0-9]
    replacing_string = ""
    list_of_start_indices = "\\[0-9]".r.findAllMatchIn(str1).map(_.start).toList
    while(list_of_start_indices.size != 0) {
      replacing_string = "(".concat(aux1(str1, list_of_start_indices)).concat(")")
      str1 = str1.replaceAll("\\[0-9]", replacing_string)
      list_of_start_indices = list_of_start_indices.tail
    }

    //Expandez +.
    str1 = replacePlus(str1);

    str1 = replaceQuestionMark(str1);

    str1
  }

  // This function should construct a prenex expression out of a normal one.
  def toPrenex(str: String): String = {
    //Fac replace pe caracterele speciale.
    //Fac replace pe syntactic sugaruri. Nu pe + si ?.
    //Fac replace la toate eps-urile.
    var str1: String = str
    str1 = replaceSpecialChars(str1)
    str1 = replaceSyntacticSugars(str1)
    str1 = str1.replaceAll("eps", Char.MinValue.toString)

    //Creez lista de caractere bazata pe stringul primit ca input.
    var list_of_chars: List[Char] = List()
    str1.foreach(c => list_of_chars = list_of_chars ++ List(c))

    /*
      Creez lista de Either-uri care specifica daca este vorba de caracter
      normal sau de caracter de control.
    */
    val list_of_char_types: List[Either[Char, Char]] = preprocess(list_of_chars)

    infix_to_prefix(list_of_char_types);
  }
}