package State

case class State[S, +A](run: S => (S, A)) {
  def map[B](f: A => B): State[S, B] = State { s =>
    val (s1, a) = run(s)
    (s1, f(a))
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (s1, a) = run(s)
    f(a).run(s1)
  }
}

object State {
  def pure[S, A](a: A): State[S, A] = State(s => (s, a))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => (s, ()))

  def modify[S](f: S => S): State[S, Unit] = State(s => (f(s), ()))

  def sequence[S, A](states: List[State[S, A]]): State[S, List[A]] = {
    def loop(ss: List[State[S, A]], acc: List[A]): State[S, List[A]] = ss match {
      case Nil => State.pure(acc.reverse)
      case h :: t => h.flatMap(a => loop(t, a :: acc))
    }
    loop(states, Nil)
  }
}