package ch.pascalschwarz.manningfpinscala.Chap08Testing

object Ex01_Ex02 extends App {
  val allTheSame = List.fill(100)(5)
  val lists:List[List[Int]] = List(
    (-13 to 100).toList,
    allTheSame,
    (100 to -100).toList,
    List()
  )

  val ex01_reverseNoMatter = lists.forall(l => l.sum == l.reverse.sum)
  val ex01_allTheSame = allTheSame.sum == 100 * 5
  val ex01_splitted = (0 to 300).forall{ n =>
    lists.forall{ l =>
      lazy val (split1, split2) = l.splitAt(n)
      n < l.length || split1.sum + split2.sum == l.sum
    }
  }

  // does not fail because function is not called for empty list
  val ex02_noLarger = lists.forall(l => l.forall(n => n <= l.max))

  val tests = List(ex01_reverseNoMatter, ex01_allTheSame, ex01_splitted,
                   ex02_noLarger)
  println("all success? " + tests.forall(_ == true))
}