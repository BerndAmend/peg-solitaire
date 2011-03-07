/**
 * Peg Solitaire
 * Copyright (C) 2010-2011 Bernd Amend <berndamend+pegsolitaire@googlemail.com>
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License version 3 as
 *  published by the Free Software Foundation.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.googlecode.pegsolitaire

import scala.tools.nsc.Interpreter

object MoveDirections extends Enumeration {
	val Horizontal = Value("horizontal")
	val Vertical = Value("vertical")
	val LeftDiagonal = Value("left diagonal \\")
	val RightDiagonal = Value("right diagonal /")
}

/**
 * Interface to the automatically generated Board Helper functions
 */
trait BoardHelper {
	/**
	 * check if a field is already in the hashSet
	 *
	 * @return true if a rotation/flipped version already exists in the hashSet
	 */
	def isInLongHashSet(field: Long, hashSet: LongHashSet): Boolean

	/**
	 * @return all fields which are equal the provided field
	 */
	def getEquivalentFields(field: Long): Iterable[Long]
}

/**
 *
 * @author Bernd Amend <berndamend+pegsolitaire@googlemail.com>
 */
final class Board(val boardDescription: String, val moveDirections: Array[MoveDirections.Value]) {

	val length = boardDescription.length - boardDescription.replaceAll("o","").length

	require(length < 63, "Only 63 field elements are currently supported")

	private val printMask = boardDescription.replaceAll("\\.", " ").replaceAll("o", "P")

	/**
	 * Describes how (x,y)-positions (map-key) inside the boardDescription correspond
	 * to the bit position used to represent the board
	 *
	 * The lookUpTable is calculated inside the boardDescriptionArray body
	 */
	protected val lookUpTable = new scala.collection.mutable.HashMap[(Int,Int), Int]

	/**
	 * into a Boolean array converted representation of the provided boardDescription 
	 */
	private val boardDescriptionArray: Array[Array[Boolean]] = {
		val cleanString = boardDescription.replaceAll(" ", "").replaceAll("\t", "").replaceAll("o", "1").replaceAll("\\.", "0").split("\n")

		require(cleanString.length > 1, "cleanString=" + cleanString)

		// check if all lines have the same length
		var lineLength = cleanString(0).length

		require(lineLength > 1)

		val result = Array.fill[Boolean](cleanString.length, lineLength)(false)

		var pos = length-1
		var line = 0
		for(s <- cleanString) {
			require(lineLength == s.length)

			val currentLine = result(line)

			for(i <- 0 until lineLength) {
				currentLine(i) = (s(i) == '1')
				if(currentLine(i)) {
					lookUpTable((i,line)) = pos
					pos -= 1
				}
			}

			line += 1
		}

		result
	}

	/**
	 * calculate the 3 required bit masks, to detect if a move is possible and to execute it
	 * (m,_,_) => ...111... (movemask)
	 * (_,m,_) => ...110... (checkmask1)
	 * (_,_,m) => ...011... (checkmask2)
	 */
	private val masks:(Array[Long], Array[Long], Array[Long]) =  {
		val movemask = new java.util.LinkedList[Long]()
		val checkmask1 = new java.util.LinkedList[Long]()
		val checkmask2 = new java.util.LinkedList[Long]()

		def addMove(pos1:(Int,Int), pos2:(Int,Int), pos3:(Int,Int)) {
			movemask   add ((1L<<lookUpTable(pos1)) | (1L<<lookUpTable(pos2)) | (1L<<lookUpTable(pos3)))
			checkmask1 add ((1L<<lookUpTable(pos1)) | (1L<<lookUpTable(pos2)))
			checkmask2 add ((1L<<lookUpTable(pos2)) | (1L<<lookUpTable(pos3)))
		}

		for(y <- 0 until boardDescriptionArray.length) {
			for(x <- 0 until boardDescriptionArray(0).length) {
				val current = boardDescriptionArray(y)(x)

				moveDirections foreach {
					_ match {
						case MoveDirections.Horizontal =>
							val right1 = if(x+1 < boardDescriptionArray(0).length) boardDescriptionArray(y)(x+1) else false
							val right2 = if(x+2 < boardDescriptionArray(0).length) boardDescriptionArray(y)(x+2) else false

							if(current && right1 && right2)
								addMove((x,y), (x+1,y), (x+2,y))

						case MoveDirections.Vertical =>
							val down1 = if(y+1 < boardDescriptionArray.length) boardDescriptionArray(y+1)(x) else false
							val down2 = if(y+2 < boardDescriptionArray.length) boardDescriptionArray(y+2)(x) else false

							if(current && down1 && down2)
								addMove((x,y), (x,y+1), (x,y+2))

						case MoveDirections.LeftDiagonal =>
							val leftDiagonal1 = if(x+1 < boardDescriptionArray(0).length && y+1 < boardDescriptionArray.length) boardDescriptionArray(y+1)(x+1) else false
							val leftDiagonal2 = if(x+2 < boardDescriptionArray(0).length && y+2 < boardDescriptionArray.length) boardDescriptionArray(y+2)(x+2) else false

						if(current && leftDiagonal1 && leftDiagonal2)
								addMove((x,y), (x+1,y+1), (x+2,y+2))

						case MoveDirections.RightDiagonal =>
							val rightDiagonal1 = if(x-1 >= 0 && y+1 < boardDescriptionArray.length) boardDescriptionArray(y+1)(x-1) else false
							val rightDiagonal2 = if(x-2 >= 0 && y+2 < boardDescriptionArray.length) boardDescriptionArray(y+2)(x-2) else false

							if(current && rightDiagonal1 && rightDiagonal2)
								addMove((x,y), (x-1,y+1), (x-2,y+2))
					}

				}
			}
		}

		require(movemask.size == checkmask1.size && checkmask1.size == checkmask2.size)

		// create Array
		val movemaskArray = new Array[Long](movemask.size)
		val checkmaskArray1 = new Array[Long](checkmask1.size)
		val checkmaskArray2 = new Array[Long](checkmask2.size)

		val miter = movemask.iterator
		val c1iter = checkmask1.iterator
		val c2iter = checkmask2.iterator

		for(i <- 0 until movemask.size) {
			movemaskArray(i) = miter.next
			checkmaskArray1(i) = c1iter.next
			checkmaskArray2(i) = c2iter.next

			/**
			 * check if the move masks are corrected
			 */
			require(java.lang.Long.bitCount(movemaskArray(i)) == 3)
			require(java.lang.Long.bitCount(checkmaskArray1(i)) == 2)
			require(java.lang.Long.bitCount(checkmaskArray2(i)) == 2)

			require(java.lang.Long.bitCount(movemaskArray(i) & checkmaskArray1(i)) == 2)
			require(java.lang.Long.bitCount(movemaskArray(i) & checkmaskArray2(i)) == 2)

			require(java.lang.Long.bitCount(movemaskArray(i) | checkmaskArray1(i)) == 3)
			require(java.lang.Long.bitCount(movemaskArray(i) | checkmaskArray2(i)) == 3)
		}

		(movemaskArray, checkmaskArray1, checkmaskArray2)
	}

	val movemask = masks._1     // ...111... required to mask bits effected by a move and execute the move
	val checkmask1 = masks._2   // ...110... required to check if a move is possible
	val checkmask2 = masks._3   // ...011... required to check if a move is possible

	private final def applyMoves(checkfield: Long, field: Long)(func: Long => Unit): Unit = {
		for (i <- 0 until movemask.size) {
			val mask = movemask(i)
			var tmp = checkfield & mask
			if (tmp == checkmask1(i) || tmp == checkmask2(i))
		        func(field ^ mask)
		}
	}

	private val interpreter = {
		val settings = new scala.tools.nsc.Settings
		settings.usejavacp.value = true
		new scala.tools.nsc.Interpreter(settings)
	}

	private val boardHelper: BoardHelper = {
		var result = new Array[BoardHelper](1)
		interpreter.quietBind("result", "Array[com.googlecode.pegsolitaire.BoardHelper]", result)
		var cmd: String = if (boardDescription ==""". . o o o . .
. . o o o . .
o o o o o o o
o o o o o o o
o o o o o o o
. . o o o . .
. . o o o . .""" && moveDirections.sameElements(Array(MoveDirections.Horizontal, MoveDirections.Vertical))) {
			"""result(0) = new com.googlecode.pegsolitaire.BoardHelper {
				def isInLongHashSet(field: Long, hashSet: com.googlecode.pegsolitaire.LongHashSet): Boolean = {
					if(hashSet.contains(field)) return true

					val n90  = rotate90(field)
					if(hashSet.contains(n90)) return true

					val n180 = rotate180(field)
					if(hashSet.contains(n180)) return true

					val n270 = rotate270(field)
					if(hashSet.contains(n270)) return true

					val v    = vflip(field)
					if(hashSet.contains(v)) return true

					val v90  = vflip(n90)
					if(hashSet.contains(v90)) return true

					val v180 = vflip(n180)
					if(hashSet.contains(v180)) return true

					val v270 = vflip(n270)
					if(hashSet.contains(v270)) return true

					false
				}

				def getEquivalentFields(field: Long): Iterable[Long] = {
					val output = new com.googlecode.pegsolitaire.LongHashSet(8)

					val n    = field
					val n90  = rotate90(n)
					val n180 = rotate180(n)
					val n270 = rotate270(n)

					val v    = vflip(n)
					val v90  = vflip(n90)
					val v180 = vflip(n180)
					val v270 = vflip(n270)

					output += n
					output += n90
					output += n180
					output += n270
					output += v
					output += v90
					output += v180
					output += v270
					//output += hflip(n)
					//output += hflip(n90)
					//output += hflip(n180)
					//output += hflip(n270)

					output
				}

				/**
				 *  memory structure
				 *   0123456
				 * 0   012
				 * 1   345
				 * 2 6789abc
				 * 3 defghij
				 * 4 klmnopq
				 * 5   rst
				 * 6   uvw
				 *
				 * output:
				 *   0123456
				 * 0   cjq
				 * 1   bip
				 * 2 25ahotw
				 * 3 149gnsv
				 * 4 038fmru
				 * 5   7el
				 * 6   6dk
				 * 32 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0
				 *  0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f  g  h  i  j  k  l  m n o p q r s t u v w
				 *  c  j  q  b  i  p  2  5  a  h  o  t  w  1  4  9  g  n  s  v  0  3  8 f m r u 7 e l 6 d k
				 */
				private def rotate90(field: Long): Long = (
					((field & (1L << 32)) >> 20) | ((field & (1L << 31)) >> 12) | ((field & (1L << 30)) >>  4)
				| ((field & (1L << 29)) >> 18) | ((field & (1L << 28)) >> 10) | ((field & (1L << 27)) >>  2)
				| ((field & (1L << 26)) >> 24) | ((field & (1L << 25)) >> 20) | ((field & (1L << 24)) >> 14)
				| ((field & (1L << 23)) >>  6) | ((field & (1L << 22)) <<  2) | ((field & (1L << 21)) <<  8)
				| ((field & (1L << 20)) << 12) | ((field & (1L << 19)) >> 18) | ((field & (1L << 18)) >> 14)
				| ((field & (1L << 17)) >>  8) |  (field & (1L << 16))        | ((field & (1L << 15)) <<  8)
				| ((field & (1L << 14)) << 14) | ((field & (1L << 13)) << 18) | ((field & (1L << 12)) >> 12)
				| ((field & (1L << 11)) >>  8) | ((field & (1L << 10)) >>  2) | ((field & (1L <<  9)) <<  6)
				| ((field & (1L <<  8)) << 14) | ((field & (1L <<  7)) << 20) | ((field & (1L <<  6)) << 24)
				| ((field & (1L <<  5)) <<  2) | ((field & (1L <<  4)) << 10) | ((field & (1L <<  3)) << 18)
				| ((field & (1L <<  2)) <<  4) | ((field & (1L <<  1)) << 12) | ((field & (1L <<  0)) << 20)
				)

				private def rotate180(field: Long): Long = rotate90(rotate90(field))
				private def rotate270(field: Long): Long = rotate90(rotate90(rotate90(field)))

				/**
				 * output 90:
				 *   0123456
				 * 0   210
				 * 1   543
				 * 2 cba9876
				 * 3 jihgfed
				 * 4 qponmlk
				 * 5   tsr
				 * 6   wvu
				 * 32 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0
				 *  0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f  g  h  i  j  k  l  m n o p q r s t u v w
				 *  2  1  0  5  4  3  c  b  a  9  8  7  6  j  i  h  g  f  e  d  q  p  o n m l k t s r w v u
				 */
				private def hflip(field: Long): Long = (
					 ((field & (1L << 32)) >> 2) |  (field & (1L << 31))       | ((field & (1L << 30)) << 2)
					|((field & (1L << 29)) >> 2) |  (field & (1L << 28))       | ((field & (1L << 27)) << 2)
					|((field & (1L << 26)) >> 6) | ((field & (1L << 25)) >> 4) | ((field & (1L << 24)) >> 2)
					| (field & (1L << 23))       | ((field & (1L << 22)) << 2) | ((field & (1L << 21)) << 4)
					|((field & (1L << 20)) << 6) | ((field & (1L << 19)) >> 6) | ((field & (1L << 18)) >> 4)
					|((field & (1L << 17)) >> 2) |  (field & (1L << 16))       | ((field & (1L << 15)) << 2)
					|((field & (1L << 14)) << 4) | ((field & (1L << 13)) << 6) | ((field & (1L << 12)) >> 6)
					|((field & (1L << 11)) >> 4) | ((field & (1L << 10)) >> 2) |  (field & (1L <<  9))
					|((field & (1L <<  8)) << 2) | ((field & (1L <<  7)) << 4) | ((field & (1L <<  6)) << 6)
					|((field & (1L <<  5)) >> 2) |  (field & (1L <<  4))       | ((field & (1L <<  3)) << 2)
					|((field & (1L <<  2)) >> 2) |  (field & (1L <<  1))       | ((field & (1L <<  0)) << 2)
					)

				/**
				 *  memory structure
				 *   0123456
				 * 0   012
				 * 1   345
				 * 2 6789abc
				 * 3 defghij
				 * 4 klmnopq
				 * 5   rst
				 * 6   uvw
				 *
				 * output:
				 *   0123456
				 * 0   uvw
				 * 1   rst
				 * 2 klmnopq
				 * 3 defghij
				 * 4 6789abc
				 * 5   345
				 * 6   012
				 * 32 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0
				 *  0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f  g  h  i  j  k  l  m n o p q r s t u v w
				 *  u  v  w  r  s  t  k  l  m  n  o  p  q  d  e  f  g  h  i  j  6  7  8 9 a b c 3 4 5 0 1 2
				 */
				private def vflip(field: Long): Long = (
					  (field & (1L << 32 | 1L << 31 | 1L << 30)) >> 30
					| (field & (1L << 29 | 1L << 28 | 1L << 27)) >> 24
					| (field & (1L << 26 | 1L << 25 | 1L << 24 | 1L << 23 | 1L << 22 | 1L << 21 | 1L << 20)) >> 14
					| (field & (1L << 19 | 1L << 18 | 1L << 17 | 1L << 16 | 1L << 15 | 1L << 14 | 1L << 13))
					| (field & (1L << 12 | 1L << 11 | 1L << 10 | 1L <<  9 | 1L <<  8 | 1L <<  7 | 1L <<  6)) << 14
					| (field & (1L <<  5 | 1L <<  4 | 1L <<  3)) << 24
					| (field & (1L <<  2 | 1L <<  1 | 1L <<  0)) << 30
					)

					// TODO: generate -- priority
					//  vertical flip
					//  rotate90
					//  rotate180
					//  rotate270
					//  horizontal flip

			}"""
		} else {
			"""result(0) = new com.googlecode.pegsolitaire.BoardHelper {
					def isInLongHashSet(field: Long, hashSet: com.googlecode.pegsolitaire.LongHashSet) = hashSet.contains(field)
					def getEquivalentFields(field: Long): Iterable[Long] = List(field)
			}"""
		}
		interpreter.interpret(cmd)
		result(0)
	}

	{ // verify that the BoardHelper ist correct
		movemask foreach {
			mask =>
			// check if all getEquivalentFields are valid moves
			boardHelper.getEquivalentFields(mask) foreach (v => require(movemask.contains(v)))

			// check if the mask is in the getEquivalentFields list
			require(boardHelper.getEquivalentFields(mask).toList.contains(mask))
		}
	}

	lazy val possibleStartFields = {
		val hashSet = new LongHashSet

		val base = (1L << length)-1L

		for(i <- 0 until length) {
			val newElement = base ^ (1L<<i)
			addToLongHashSet(newElement, hashSet)
		}

		hashSet.toList
	}

	/**
	 * creates a human-readable version of a field, the output as described by the boardDescription
	 */
	def toString(field: Long): String = {
		var output = printMask

		for(i <- (length-1).to(0,-1) )
			output = output.replaceFirst("P", (if ((field & (1L << i)) == 0) "." else "x"))

		output
	}

	/**
	 * converts a human-readable version into the internal bit representation 
	 */
	def fromString(field: String): Long = java.lang.Long.parseLong(field.replaceAll("\n", "").replaceAll(" ", "").replaceAll("\t", "").replaceAll("x", "1").replaceAll("\\.", "0"), 2)

  private final def calculateRelatedFields(checkfield: Long, field: Long): Iterable[Long] = {
		var result = List[Long]()
		applyMoves(checkfield, field) { result ::= _ }
    result
	}

	/**
	 * @return complete follower list
	 */
	final def calculateFollower(field: Long): Iterable[Long] = calculateRelatedFields(field, field)

	/**
	 * @return complete predecessor list
	 */
	final def calculatePredecessor(field: Long): Iterable[Long] = calculateRelatedFields(~field, field)

	private final def addRelatedFields(checkfield: Long, field: Long, solutions: LongHashSet): Boolean = {
    val result = calculateRelatedFields(checkfield, field)

    result.foreach{ addToLongHashSet(_, solutions) }
		!result.isEmpty
	}

	/**
	 * @return true : follower was found
	 */
	final def addFollower(field: Long, solutions: LongHashSet): Boolean = addRelatedFields(field, field, solutions)

	/**
	 * @return true : predecessor was found
	 */
	final def addPredecessor(field: Long, solutions: LongHashSet): Boolean = addRelatedFields(~field, field, solutions)

	/**
	 * return true if field has a follower in the solutions HashSet
	 */
	private final def hasRelatedFields(checkfield: Long, field: Long, solutions: LongHashSet): Boolean = {
		applyMoves(checkfield, field) { n => if (boardHelper.isInLongHashSet(n, solutions)) return true }
		false
	}

	final def hasFollower(field: Long, solutions: LongHashSet): Boolean = hasRelatedFields(field, field, solutions)

	final def hasPredecessor(field: Long, solutions: LongHashSet): Boolean = hasRelatedFields(~field, field, solutions)

	/**
	 * @return all related fields that are in the solutions HashSet
	 */
	private final def getRelatedFields(checkfield: Long, field: Long, searchSet: LongHashSet): Iterable[Long] = {
		var result = new LongHashSet
		applyMoves(checkfield, field) { n => if (boardHelper.isInLongHashSet(n, searchSet)) result += n}
		result
	}

	final def getFollower(field: Long, searchSet: LongHashSet): Iterable[Long] = getRelatedFields(field, field, searchSet)

	final def getPredecessor(field: Long, searchSet: LongHashSet): Iterable[Long] = getRelatedFields(~field, field, searchSet)

	/**
	 * Add a field into the hashSet, if isInLongHashSet returns false
	 *
	 * @return true if the field was really added
	 */
	 def addToLongHashSet(field: Long, hashSet: LongHashSet): Boolean = {
		if (!boardHelper.isInLongHashSet(field, hashSet)) {
			hashSet += field
			true
		} else
			false
	}

	/**
	 * @return a complete list with all equivalent fields for the fields HashSet
	 */
	def getCompleteList(fields: Iterable[Long]): Iterable[Long] = {
		val output = new LongHashSet
		fields foreach ( output += boardHelper.getEquivalentFields(_) )
		output
	}
}
