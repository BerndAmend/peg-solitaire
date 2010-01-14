/**
 * Peg Solitaire Solver  Copyright (C) 2010 Bernd Amend <berndamend+pegsolitaire@googlemail.com>
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

object PegSolitaireMove extends Enumeration {
	val Horizontal = Value("horizontal")
	val Vertical = Value("vertical")
	val LeftDiagonal = Value("left diagonal \\")
	val RightDiagonal = Value("right diagonal /")
}

class PegSolitaire(val fieldDescription: String, val moveDirections: Array[PegSolitaireMove.Value]) {

	val length = fieldDescription.length - fieldDescription.replaceAll("o","").length

	require(length < 63, "Only 63 field elements are currently supported")

	private val printMask = fieldDescription.replaceAll("\\.", " ").replaceAll("o", "P")

	protected val lookUpTable = new scala.collection.mutable.HashMap[(Int,Int), Int]
	private val masks:(Array[Long], Array[Long], Array[Long]) =  {

		val cleanString = fieldDescription.replaceAll(" ", "").replaceAll("\t", "").replaceAll("o", "1").replaceAll("\\.", "0").split("\n")

		require(cleanString.length > 1, "cleanString=" + cleanString)

		// check if all lines have the same length
		var lineLength = cleanString(0).length

		require(lineLength > 1)

		val fieldDescriptionArray = Array.fill[Boolean](cleanString.length, lineLength)(false)

		var pos = length-1
		var line = 0
		for(s <- cleanString) {
			require(lineLength == s.length)

			val currentLine = fieldDescriptionArray(line)

			for(i <- 0 until lineLength) {
				currentLine(i) = (s(i) == '1')
				if(currentLine(i)) {
					lookUpTable((i,line)) = pos
					pos -= 1
				}
			}

			line += 1
		}

		val movemask = new java.util.LinkedList[Long]()
		val checkmask1 = new java.util.LinkedList[Long]()
		val checkmask2 = new java.util.LinkedList[Long]()

		def addMove(pos1:(Int,Int), pos2:(Int,Int), pos3:(Int,Int)) {
			movemask   add ((1L<<lookUpTable(pos1)) | (1L<<lookUpTable(pos2)) | (1L<<lookUpTable(pos3)))
			checkmask1 add ((1L<<lookUpTable(pos1)) | (1L<<lookUpTable(pos2)))
			checkmask2 add ((1L<<lookUpTable(pos2)) | (1L<<lookUpTable(pos3)))
		}

		for(y <- 0 until fieldDescriptionArray.length) {
			for(x <- 0 until fieldDescriptionArray(0).length) {
				val current = fieldDescriptionArray(y)(x)

				moveDirections foreach {
					_ match {
						case PegSolitaireMove.Horizontal =>
							val right1 = if(x+1 < fieldDescriptionArray(0).length) fieldDescriptionArray(y)(x+1) else false
							val right2 = if(x+2 < fieldDescriptionArray(0).length) fieldDescriptionArray(y)(x+2) else false

							if(current && right1 && right2)
								addMove((x,y), (x+1,y), (x+2,y))

						case PegSolitaireMove.Vertical =>
							val down1 = if(y+1 < fieldDescriptionArray.length) fieldDescriptionArray(y+1)(x) else false
							val down2 = if(y+2 < fieldDescriptionArray.length) fieldDescriptionArray(y+2)(x) else false

							if(current && down1 && down2)
								addMove((x,y), (x,y+1), (x,y+2))

						case PegSolitaireMove.LeftDiagonal =>
							val leftDiagonal1 = if(x+1 < fieldDescriptionArray(0).length && y+1 < fieldDescriptionArray.length) fieldDescriptionArray(y+1)(x+1) else false
							val leftDiagonal2 = if(x+2 < fieldDescriptionArray(0).length && y+2 < fieldDescriptionArray.length) fieldDescriptionArray(y+2)(x+2) else false

						if(current && leftDiagonal1 && leftDiagonal2)
								addMove((x,y), (x+1,y+1), (x+2,y+2))

						case PegSolitaireMove.RightDiagonal =>
							val rightDiagonal1 = if(x-1 >= 0 && y+1 < fieldDescriptionArray.length) fieldDescriptionArray(y+1)(x-1) else false
							val rightDiagonal2 = if(x-2 >= 0 && y+2 < fieldDescriptionArray.length) fieldDescriptionArray(y+2)(x-2) else false

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

	val movemask = masks._1
	val checkmask1 = masks._2
	val checkmask2 = masks._3

	val possibleStartFields = {
		val hashSet = new LongHashSet

		val base = (1L << length)-1L

		for(i <- 0 until length) {
			val newElement = base ^ (1L<<i)
			addToLongHashSet(newElement, hashSet)
		}

		hashSet.toList
	}

	def toString(field: Long): String = {
		var output = printMask

		var i = length - 1
		do {
			output = output.replaceFirst("P", (if ((field & (1L << i)) == 0) "." else "x"))
			i -= 1
		} while (i >= 0)

		output
	}

	def fromString(field: String): Long = java.lang.Long.parseLong(field.replaceAll("\n", "").replaceAll(" ", "").replaceAll("\t", "").replaceAll("x", "1").replaceAll("\\.", "0"), 2)

	/**
	 * @return true : follower was found
	 */
	final def addFollower(field: Long, solutions: LongHashSet): Boolean = {
		var hasFollower = false
		var i = 0
		while (i < movemask.size) {
			var tmp = field & movemask(i)
			if (tmp == checkmask1(i) || tmp == checkmask2(i)) {
				addToLongHashSet(field ^ movemask(i), solutions)
				hasFollower = true
			}
			i += 1
		}
		hasFollower
	}

	/**
	 * @return true if the field was really added
	 */
	final def addToLongHashSet(field: Long, hashSet: LongHashSet) = {
		if (!isInLongHashSet(field, hashSet)) {
			hashSet += field
			true
		} else
			false
	}

	final def hasFollower(field: Long, solutions: LongHashSet): Boolean = {
		var i = 0
		while (i < movemask.size) {
			var tmp = field & movemask(i)
			if (tmp == checkmask1(i) || tmp == checkmask2(i)) {
				if (isInLongHashSet(field ^ movemask(i), solutions))
					return true

			}
			i += 1
		}
		false
	}

	def isInLongHashSet(field: Long, hashSet: LongHashSet): Boolean = hashSet.contains(field)

	def getEquivalentFields(field: Long) = List(field)

	def getCompleteList(fields: LongHashSet): List[Long] = {
		val output = new LongHashSet

		val iter = fields.iterator
		while(iter.hasNext)
			output += getEquivalentFields(iter.next)

		output.toList
	}

	def getCompleteList(fields: Iterable[Long]): List[Long] = {
		val output = new LongHashSet

		val iter = fields.iterator
		while(iter.hasNext)
			output += getEquivalentFields(iter.next)

		output.toList
	}
}

object PegSolitaireEnglish extends PegSolitaire(
""". . o o o . .
. . o o o . .
o o o o o o o
o o o o o o o
o o o o o o o
. . o o o . .
. . o o o . .""", Array(PegSolitaireMove.Horizontal, PegSolitaireMove.Vertical)) {

	override def isInLongHashSet(field: Long, hashSet: LongHashSet): Boolean = {
		val n90  = rotate90(field)
		val n180 = rotate90(n90)
		val n270 = rotate90(n180)

		val v    = vflip(field)
		val v90  = vflip(n90)
		val v180 = vflip(n180)
		val v270 = vflip(n270)

		(
			hashSet.contains(field) || hashSet.contains(n90 ) ||
			hashSet.contains(n180 ) || hashSet.contains(n270) ||
			hashSet.contains(v    ) || hashSet.contains(v90 ) ||
			hashSet.contains(v180 ) || hashSet.contains(v270)
		)
	}

	override def getEquivalentFields(field: Long) = {
		val output = new LongHashSet

		val n    = field
		val n90  = rotate90(n)
		val n180 = rotate90(n90)
		val n270 = rotate90(n180)

		output += n
		output += n90
		output += n180
		output += n270
		output += vflip(n)
		output += vflip(n90)
		output += vflip(n180)
		output += vflip(n270)

		output.toList
	}

	/**
	 * memory structure
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
	def rotate90(field: Long): Long = {
		var result = 0L
		if ((field & (1L << 32)) != 0) result |= 1L << 12
		if ((field & (1L << 31)) != 0) result |= 1L << 19
		if ((field & (1L << 30)) != 0) result |= 1L << 26

		if ((field & (1L << 29)) != 0) result |= 1L << 11
		if ((field & (1L << 28)) != 0) result |= 1L << 18
		if ((field & (1L << 27)) != 0) result |= 1L << 25

		if ((field & (1L << 26)) != 0) result |= 1L << 2
		if ((field & (1L << 25)) != 0) result |= 1L << 5
		if ((field & (1L << 24)) != 0) result |= 1L << 10
		if ((field & (1L << 23)) != 0) result |= 1L << 17
		if ((field & (1L << 22)) != 0) result |= 1L << 24
		if ((field & (1L << 21)) != 0) result |= 1L << 29
		if ((field & (1L << 20)) != 0) result |= 1L << 32

		if ((field & (1L << 19)) != 0) result |= 1L << 1
		if ((field & (1L << 18)) != 0) result |= 1L << 4
		if ((field & (1L << 17)) != 0) result |= 1L << 9
		result |= (field & (1L << 16))
		if ((field & (1L << 15)) != 0) result |= 1L << 23
		if ((field & (1L << 14)) != 0) result |= 1L << 28
		if ((field & (1L << 13)) != 0) result |= 1L << 31

		if ((field & (1L << 12)) != 0) result |= 1L << 0
		if ((field & (1L << 11)) != 0) result |= 1L << 3
		if ((field & (1L << 10)) != 0) result |= 1L << 8
		if ((field & (1L << 9)) != 0) result |= 1L << 15
		if ((field & (1L << 8)) != 0) result |= 1L << 22
		if ((field & (1L << 7)) != 0) result |= 1L << 27
		if ((field & (1L << 6)) != 0) result |= 1L << 30

		if ((field & (1L << 5)) != 0) result |= 1L << 7
		if ((field & (1L << 4)) != 0) result |= 1L << 14
		if ((field & (1L << 3)) != 0) result |= 1L << 21

		if ((field & (1L << 2)) != 0) result |= 1L << 6
		if ((field & (1L << 1)) != 0) result |= 1L << 13
		if ((field & (1L << 0)) != 0) result |= 1L << 20

		result
	}

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
	def vflip(field: Long): Long = {
		var result = 0L
		if ((field & (1L << 32)) != 0) result |= 1L << 30
		result |= (field & (1L << 31))
		if ((field & (1L << 30)) != 0) result |= 1L << 32

		if ((field & (1L << 29)) != 0) result |= 1L << 27
		result |= (field & (1L << 28))
		if ((field & (1L << 27)) != 0) result |= 1L << 29

		if ((field & (1L << 26)) != 0) result |= 1L << 20
		if ((field & (1L << 25)) != 0) result |= 1L << 21
		if ((field & (1L << 24)) != 0) result |= 1L << 22
		result |= (field & (1L << 23))
		if ((field & (1L << 22)) != 0) result |= 1L << 24
		if ((field & (1L << 21)) != 0) result |= 1L << 25
		if ((field & (1L << 20)) != 0) result |= 1L << 26

		if ((field & (1L << 19)) != 0) result |= 1L << 13
		if ((field & (1L << 18)) != 0) result |= 1L << 14
		if ((field & (1L << 17)) != 0) result |= 1L << 15
		result |= (field & (1L << 16))
		if ((field & (1L << 15)) != 0) result |= 1L << 17
		if ((field & (1L << 14)) != 0) result |= 1L << 18
		if ((field & (1L << 13)) != 0) result |= 1L << 19

		if ((field & (1L << 12)) != 0) result |= 1L << 6
		if ((field & (1L << 11)) != 0) result |= 1L << 7
		if ((field & (1L << 10)) != 0) result |= 1L << 8
		result |= (field & (1L << 9))
		if ((field & (1L << 8)) != 0) result |= 1L << 10
		if ((field & (1L << 7)) != 0) result |= 1L << 11
		if ((field & (1L << 6)) != 0) result |= 1L << 12

		if ((field & (1L << 5)) != 0) result |= 1L << 3
		result |= (field & (1L << 4))
		if ((field & (1L << 3)) != 0) result |= 1L << 5

		if ((field & (1L << 2)) != 0) result |= 1L << 0
		result |= (field & (1L << 1))
		if ((field & (1L << 0)) != 0) result |= 1L << 2

		result
	}


	for (i <- 0 until movemask.size) {
		// check rotate
		require(movemask(i) == rotate90(rotate90(rotate90(rotate90(movemask(i))))))
		require(checkmask1(i) == rotate90(rotate90(rotate90(rotate90(checkmask1(i))))))
		require(checkmask2(i) == rotate90(rotate90(rotate90(rotate90(checkmask2(i))))))

		// check vflip
		require(movemask(i) == vflip(vflip(movemask(i))))
		require(checkmask1(i) == vflip(vflip(checkmask1(i))))
		require(checkmask2(i) == vflip(vflip(checkmask2(i))))
	}
}

/**
 * ToDo implement rotate
 */
object PegSolitaireFrench extends PegSolitaire(
""". . o o o . .
. o o o o o .
o o o o o o o
o o o o o o o
o o o o o o o
. o o o o o .
. . o o o . .""", Array(PegSolitaireMove.Horizontal, PegSolitaireMove.Vertical))


object PegSolitaire15Holes extends PegSolitaire(
"""o . . . .
o o . . .
o o o . .
o o o o .
o o o o o""", Array(PegSolitaireMove.Horizontal, PegSolitaireMove.Vertical, PegSolitaireMove.LeftDiagonal))

