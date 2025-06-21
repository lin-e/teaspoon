package preprocessor.parser

import parsley.Parsley
import parsley.Parsley.{attempt, empty}
import parsley.combinator.option
import parsley.lift.lift2

object ParsleyExtensions {

  implicit class ParsleyStrExtension(p: Parsley[String]) {
    // lift concat
    def <++>(q: Parsley[String]): Parsley[String] = lift2[String, String, String](_ concat _, p, q)

    // convert an optional string to an empty string
    def opt(): Parsley[String] = option(p) map {
      case None => ""
      case Some(value) => value
    }
  }

  implicit class ParsleyListExtension[A](ps: Parsley[List[A]]) {
    // lift snoc?
    def <:+>(q: Parsley[A]): Parsley[List[A]] = lift2[List[A], A, List[A]](_ :+ _, ps, q)

    // lift concact
    def <:::>(qs: Parsley[List[A]]): Parsley[List[A]] = lift2[List[A], List[A], List[A]](_ ::: _, ps, qs)
  }

  // no idea if this works?
  implicit class ParsleyAExtension[A](p: Parsley[A]) {
    // parses both p and q, but fails if p fails or if q succeeds
    def butNot[B](q: Parsley[B]): Parsley[A] = {
      attempt(option(q) flatMap {
        case None => p
        case Some(_) => empty // come up with a better error?
      })
    }
  }

}
