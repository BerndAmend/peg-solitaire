/**
 * Peg Solitaire
 * Copyright (C) 2010-2012 Bernd Amend <berndamend+pegsolitaire@googlemail.com>
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
	 * return the normalform of a gamefield
	 */
	def getNormalform(field: Long): Long

	/**
	 * @return all fields which are equal the provided field
	 */
	def getEquivalentFields(field: Long): LongHashSet
}

/**
 *
 * @author Bernd Amend <berndamend+pegsolitaire@googlemail.com>
 */
final class Board(val boardDescription: String, val moveDirections: Array[MoveDirections.Value]) {

	val length = boardDescription.length - boardDescription.replaceAll("o","").length

	require(length < 63, "Max 63 field elements are currently supported")

	private val printMask = boardDescription.replaceAll("\\.", " ").replaceAll("o", "P")

	/**
	 * Describes how (x,y)-positions (map-key) inside the boardDescription correspond
	 * to the bit position used to represent the board
	 */
	private val lookUpTable: Array[Array[Int]] = {
		val cleanString = boardDescription.replaceAll(" ", "").replaceAll("\t", "").replaceAll("o", "1").replaceAll("\\.", "0").split("\n")

		require(cleanString.length > 1, "cleanString=" + cleanString)

		// check if all lines have the same length
		val lineLength = cleanString(0).length

		require(lineLength > 1)

		val result = Array.fill[Int](cleanString.length, lineLength)(-1)

		var pos = length-1
		var line = 0
		for(s <- cleanString) {
			require(lineLength == s.length)

			val currentLine = result(line)

			for(i <- 0 until lineLength) {
				if(s(i) == '1') {
					currentLine(i) = pos
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
		var movemask = List[Long]()
		var checkmask1 = List[Long]()
		var checkmask2 = List[Long]()

		def addMove(pos1:(Int,Int), pos2:(Int,Int), pos3:(Int,Int)) {
			movemask   ::= ((1L<<lookUpTable(pos1._2)(pos1._1)) | (1L<<lookUpTable(pos2._2)(pos2._1)) | (1L<<lookUpTable(pos3._2)(pos3._1)))
			checkmask1 ::= ((1L<<lookUpTable(pos1._2)(pos1._1)) | (1L<<lookUpTable(pos2._2)(pos2._1)))
			checkmask2 ::= ((1L<<lookUpTable(pos2._2)(pos2._1)) | (1L<<lookUpTable(pos3._2)(pos3._1)))
		}

		for(y <- 0 until lookUpTable.length) {
			for(x <- 0 until lookUpTable(0).length) {
				val current = lookUpTable(y)(x) != -1

				moveDirections foreach {
					_ match {
						case MoveDirections.Horizontal =>
							val right1 = if(x+1 < lookUpTable(0).length) (lookUpTable(y)(x+1) != -1) else false
							val right2 = if(x+2 < lookUpTable(0).length) (lookUpTable(y)(x+2) != -1) else false

							if(current && right1 && right2)
								addMove((x,y), (x+1,y), (x+2,y))

						case MoveDirections.Vertical =>
							val down1 = if(y+1 < lookUpTable.length) (lookUpTable(y+1)(x) != -1) else false
							val down2 = if(y+2 < lookUpTable.length) (lookUpTable(y+2)(x) != -1) else false

							if(current && down1 && down2)
								addMove((x,y), (x,y+1), (x,y+2))

						case MoveDirections.LeftDiagonal =>
							val leftDiagonal1 = if(x+1 < lookUpTable(0).length && y+1 < lookUpTable.length) (lookUpTable(y+1)(x+1) != -1) else false
							val leftDiagonal2 = if(x+2 < lookUpTable(0).length && y+2 < lookUpTable.length) (lookUpTable(y+2)(x+2) != -1) else false

						if(current && leftDiagonal1 && leftDiagonal2)
								addMove((x,y), (x+1,y+1), (x+2,y+2))

						case MoveDirections.RightDiagonal =>
							val rightDiagonal1 = if(x-1 >= 0 && y+1 < lookUpTable.length) (lookUpTable(y+1)(x-1) != -1) else false
							val rightDiagonal2 = if(x-2 >= 0 && y+2 < lookUpTable.length) (lookUpTable(y+2)(x-2) != -1) else false

							if(current && rightDiagonal1 && rightDiagonal2)
								addMove((x,y), (x-1,y+1), (x-2,y+2))
					}

				}
			}
		}

		require(movemask.size == checkmask1.size && checkmask1.size == checkmask2.size)

		// create Array
		val movemaskArray = movemask.toArray
		val checkmaskArray1 = checkmask1.toArray
		val checkmaskArray2 = checkmask2.toArray

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
	val movemask_size = movemask.size
	
	private def vflip(in: Array[Array[Int]]): Array[Array[Int]] = in map {_.reverse}
	private def hflip(in: Array[Array[Int]]): Array[Array[Int]] = vflip(in.transpose).transpose
	private def rotate90(in: Array[Array[Int]]): Array[Array[Int]] = vflip(in.transpose)
	private def rotate180(in: Array[Array[Int]]): Array[Array[Int]] = vflip(hflip(in))
	private def rotate270(in: Array[Array[Int]]): Array[Array[Int]] = hflip(in.transpose)
	private def vflip_rotate90(in: Array[Array[Int]]): Array[Array[Int]] = vflip(rotate90(in))
	private def hflip_rotate90(in: Array[Array[Int]]): Array[Array[Int]] = hflip(rotate90(in))
	
	private def have_equal_shape(in1: Array[Array[Int]], in2: Array[Array[Int]]): Boolean = {
		if(in1.length != in2.length || in1(0).length != in2(0).length)
			return false

		for(y <- 0 until in1.length;
				x <- 0 until in2(0).length) {
				if((in1(y)(x) == -1 || in2(y)(x) == -1) && in1(y)(x) != in2(y)(x))
					return false
		}
		true
	}
	
	private val movemask_transformation_tests = movemask map {toArray(_)}
	
	private def is_transformation_valid(transformation: Array[Array[Int]] => Array[Array[Int]]): Boolean = {
		if (have_equal_shape(lookUpTable, transformation(lookUpTable)))
			true
		else
			false

		movemask_transformation_tests forall{i => movemask.contains(toLong(transformation(i)))}
	}

	private val _board_helper_sourcecode: String = {
		val get_normalform = new StringBuilder
		val get_equivalent_fields = new StringBuilder

		get_normalform append "def getNormalform(f: Long): Long = {\n"
		get_normalform append "var n = f\n\n"

		get_equivalent_fields append "def getEquivalentFields(f: Long) = {\n"
		get_equivalent_fields append "val n = new com.googlecode.pegsolitaire.LongHashSet(8)\n"
		get_equivalent_fields append "n += f\n\n"

		if(is_transformation_valid(rotate180)) {
			val c_rotate180 = CodeGenerator(rotate180(lookUpTable))
			get_normalform append "val n180 = " + c_rotate180 + "\n"
			get_normalform append "if(n180 < n) n = n180\n\n"

			get_equivalent_fields append "n += " + c_rotate180 + "\n"
		}

		if(is_transformation_valid(rotate90)) {
			val c_rotate90 = CodeGenerator(rotate90(lookUpTable))
			get_normalform append "val n90 = " + c_rotate90 + "\n"
			get_normalform append "if(n90 < n) n = n90\n\n"

			get_equivalent_fields append "n += " + c_rotate90 + "\n"
		}

		if(is_transformation_valid(rotate270)) {
			val c_rotate270 = CodeGenerator(rotate270(lookUpTable))
			get_normalform append "val n270 = " + c_rotate270 + "\n"
			get_normalform append "if(n270 < n) n = n270\n\n"

			get_equivalent_fields append "n += " + c_rotate270 + "\n"
		}

		if(is_transformation_valid(vflip)) {
			val c_vflip = CodeGenerator(vflip(lookUpTable))
			get_normalform append "val v = " + c_vflip + "\n"
			get_normalform append "if(v < n) n = v\n\n"

			get_equivalent_fields append "n += " + c_vflip + "\n"
		}

		if(is_transformation_valid(hflip)) {
			val c_hflip = CodeGenerator(hflip(lookUpTable))
			get_normalform append "val h = " + c_hflip + "\n"
			get_normalform append "if(h < n) n = h\n\n"

			get_equivalent_fields append "n += " + c_hflip + "\n"
		}

		if(is_transformation_valid(vflip_rotate90)) {
			val c_v90 = CodeGenerator(vflip_rotate90(lookUpTable))
			get_normalform append "val v90 = " + c_v90 + "\n"
			get_normalform append "if(v90 < n) n = v90\n\n"

			get_equivalent_fields append "n += " + c_v90 + "\n"
		}

		if(is_transformation_valid(hflip_rotate90)) {
			val c_h90 = CodeGenerator(hflip_rotate90(lookUpTable))
			get_normalform append "val h90 = " + c_h90 + "\n"
			get_normalform append "if(h90 < n) n = h90\n\n"

			get_equivalent_fields append "n += " + c_h90 + "\n"
		}

		get_normalform append "n\n"
		get_normalform append "}\n"

		get_equivalent_fields append "n\n"
		get_equivalent_fields append "}\n"
		
		
		val r = new StringBuilder
		
		r append "result(0) = new com.googlecode.pegsolitaire.BoardHelper {\n"
		r append get_normalform.result()
		r append get_equivalent_fields.result()
		r append "}\n"

		r.result()
	}

	def board_helper_sourcecode: String = _board_helper_sourcecode

	private val interpreter = {
		val settings = new scala.tools.nsc.Settings
		settings.embeddedDefaults[BoardHelper]
		settings.usejavacp.value = true
		new scala.tools.nsc.interpreter.IMain(settings)
	}

	val boardHelper: BoardHelper = {
		val result = new Array[BoardHelper](1)
		interpreter.quietBind(scala.tools.nsc.interpreter.NamedParam("result", "Array[com.googlecode.pegsolitaire.BoardHelper]", result))
		interpreter.interpret(_board_helper_sourcecode)
		result(0)
	}

	{ // verify that the BoardHelper ist correct
		movemask foreach {
			mask =>
			// check if all getEquivalentFields are valid moves
			boardHelper.getEquivalentFields(mask) foreach (v => require(movemask.contains(v)))

			// check if the mask is in the getEquivalentFields list
			require(boardHelper.getEquivalentFields(mask).contains(mask))
		}
	}

	lazy val possibleStartFields = {
		val hashSet = new LongHashSet

		val base = (1L << length)-1L

		for(i <- 0 until length) {
			val newElement = base ^ (1L<<i)
			hashSet += boardHelper.getNormalform(newElement)
		}

		hashSet
	}

	/**
	 * blocked fields get -1, empty fields get 0, used fields 1
	 */
	def toArray(field: Long): Array[Array[Int]] = {
		var output = lookUpTable map(_.clone())

		for(y <- 0 until output.length;
		    x <- 0 until output(0).length) {
			if(output(y)(x) != -1) {
				if ((field & (1L << output(y)(x))) == 0) {
					output(y)(x) = 0
				} else {
					output(y)(x) = 1
				}
			}
		}
		output
	}
	
	def toLong(in: Array[Array[Int]]) = {
		var r = 0L
		for(y <- 0 until in.length;
				x <- 0 until in(0).length) {
				if(in(y)(x) == 1) {
					r |= 1L << lookUpTable(y)(x)
				}
		}
		r
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

	private final def addRelatedFields(checkfield: Long, field: Long, solutions: LongHashSet) {
		var i = 0
		val size = movemask_size
		while (i < size) {
			val n = applyMove(checkfield, field, i)
			if (n != Long.MinValue)
				solutions += n
			i += 1
		}
	}

	/**
	 * @return true : follower was found
	 */
	final def addFollower(field: Long, solutions: LongHashSet) = addRelatedFields(field, field, solutions)

	/**
	 * @return true : predecessor was found
	 */
	final def addPredecessor(field: Long, solutions: LongHashSet) = addRelatedFields(~field, field, solutions)

	/**
	 * return true if field has a follower/predecessor in the solutions HashSet
	 */
	private final def hasRelatedFields(checkfield: Long, field: Long, solutions: LongHashSet): Boolean = {
		var i = 0
		val size = movemask_size
		while (i < size) {
			val n = applyMove(checkfield, field, i)
			if (n != Long.MinValue && solutions.contains(n))
				return true
			i += 1
		}
		false
	}

	final def hasFollower(field: Long, solutions: LongHashSet): Boolean = hasRelatedFields(field, field, solutions)

	final def hasPredecessor(field: Long, solutions: LongHashSet): Boolean = hasRelatedFields(~field, field, solutions)

	/**
	 * you may want to call getComplete list on the result
	 */
	private final def getRelatedFields(checkfield: Long, field: Long, searchSet: LongHashSet): LongHashSet = {
		var result = new LongHashSet
		var i = 0
		val size = movemask_size
		while (i < size) {
			val n = applyMove(checkfield, field, i)
			if (n != Long.MinValue && searchSet.contains(n))
				result += n
			i += 1
		}
		result
	}

	final def getFollower(field: Long, searchSet: LongHashSet): LongHashSet = getRelatedFields(field, field, searchSet)

	final def getPredecessor(field: Long, searchSet: LongHashSet): LongHashSet = getRelatedFields(~field, field, searchSet)

	/**
	 * @return a complete list with all equivalent fields for the fields HashSet
	 */
	def getCompleteList(fields: LongHashSet): LongHashSet = {
		val output = new LongHashSet
		fields foreach ( output += boardHelper.getEquivalentFields(_) )
		output
	}
	
	private final def applyMove(checkfield: Long, field: Long, movemaskid: Int): Long = {
		val mask = movemask(movemaskid)
		val tmp = checkfield & mask
		if (tmp == checkmask1(movemaskid) || tmp == checkmask2(movemaskid))
			boardHelper.getNormalform(field ^ mask)
		else
			Long.MinValue
	}
	
	private def print_look_up_table(in: Array[Array[Int]]) {
		val r = new StringBuilder
		print_look_up_table(in, r)
		println(r.result())
	}

	private def print_look_up_table(in: Array[Array[Int]], r: StringBuilder) {
		in foreach { n =>
			n foreach { i =>
				if(i != -1)
					r append  ("%2d ".format(i))
				else
				 r append ("   ")
			}
			r append "\n"
		}
		r append "\n"
	}

	def debug_output: String = {
		val r = new StringBuilder
		
		r append "Look up table\n"
		print_look_up_table(lookUpTable, r)

		movemask_transformation_tests foreach (print_look_up_table(_, r))

		// output movemask
		// output checkmask1
		// output checkmask2
		r append "board_helper sourcecode\n"
		r append board_helper_sourcecode
		
		r.result()
	}

}
