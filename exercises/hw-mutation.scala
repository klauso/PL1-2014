// HOMEWORK ASSIGNMENT
// ===================
//
// Email homework as Scala source file to:
//
// rendel@informatik.uni-tuebingen.de
//
// Work in groups of 1 or 2 students. Send the email CC to the
// other student in your team. Hand in before the morning of
// Monday, December 15.
//
//
// Put "pl1-hw05" in subject, please
//
// 0. write in the email:
// - your names
// - your student ids ("Matrikelnummer")
// 1. Implement assignment to arbitrary variables.
//
// Three different ideas for how to implement assignment:
//
// 1st idea: Add
//
//   case class Assign(lhs: Symbol, rhs: Exp) extends Exp
//
// and change the interpreter so that the environment maps names
// to adresses, and the real values are in a store that maps
// adresses to values.
//
// 2nd idea: Implement assign as syntactic sugar so that every
// expression creates a box with the real result, and then use
// the interpreter for the language with boxes from the lecture.
//
// 3rd idea: Implement assign as syntactic sugar so that every
// value in the environment is in a box, and then use the
// interpreter for the language with boxes from the lecture.
//
// Send question by email to rendel@informatik.uni-tuebingen.de
