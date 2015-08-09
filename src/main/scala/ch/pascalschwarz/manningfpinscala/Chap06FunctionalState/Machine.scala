package ch.pascalschwarz.manningfpinscala.Chap06FunctionalState

/**
  ex 06_11
 */

case class Machine(locked: Boolean, candies: Int, coins:Int)

sealed trait Input
case object Coin extends Input
case object Turn extends Input

object Candy {
  def update = (i: Input) => (s: Machine) => (i, s) match {
    case (_, Machine(_, 0, _)) => s // "a machine thats out of candy ignores all inputs"
    case (Turn, Machine(true, _, _)) => s // "turning the knob on a locked machine...
    case (Coin, Machine(false, _, _)) => s // ...or inserting a coin in an unlocked machine does nothing"
    case (Coin, Machine(_, candy, coins)) => Machine(false, candy, coins + 1) // false: if candy = 0, first case matches
    case (Turn, Machine(false, candy, coins)) => Machine(true, candy - 1, coins)
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int,Int)] = State{(machine: Machine) =>
    val actions = State.sequence{
      inputs.map(update andThen State.modify[Machine])
    }
    val (_, finalMachine) = actions.run(machine)
    ((finalMachine.coins, finalMachine.candies), finalMachine)
  }
}