package com.googlecode.pegsolitaire

import java.io.DataOutputStream
import java.io.BufferedOutputStream
import java.io.RandomAccessFile
import java.io.FileOutputStream
import java.nio.ByteBuffer
import java.nio.ByteOrder

/**
 *
 * It is very important that the data are saved as little endian, otherwise the fields get corrupted
 * Description of the used file
 * Header:
 * <version: Short>
 * <Possible Moves: Array[Short](4)>
 * <BoardDescription size: Short>
 *
 * <boardDescription: Array[Char]>
 *
 * Saved LongHashSets
 * <number of bits set:Int><number of values: Int><values: repeated Long>: repeated until eof
 *
 * Version changes
 *   1: Initial version (unsupported since version 0.4)
 *   2: All fields are handled as User defined fields, since equal fields are now detected for all games.
 *      Due to this change it is not possible to correctly load new results with an old program version.
 *      The data are stored more compact but without using a compression
 */
object IO {
	
	final val buffer_size = 1024 // 2 3 4 6 8

	/**
	 * load a solution from file
	 */
	def load(filename: String, observer: StatusObserver): Solver = {
		/*var output: Solver = null

		val in = new java.io.FileInputStream(filename).getChannel

		try {
			// read header
			// Allocate buffer for the header
			val header = java.nio.ByteBuffer.allocateDirect(
				2 + // Version
				2*4 + // Move directions
				2 // Board description length
			)
			header.order(ByteOrder.LITTLE_ENDIAN)
			header.clear()

			in.read(header)
			header.clear()

			val version = header.getShort

			version match {
				case 2 =>
				case _ =>
			    throw new Exception("unsupported version")
			}

			// read and create game
			var possibleMoves = List[MoveDirections.Value]()

			for(i <- 0 until 4) {
				val m = header.getShort
				if(m >= 0)
					possibleMoves ::= MoveDirections(m)
			}

			val description_size = header.getShort

			require(description_size > 2)

			// read board Description
			val bd = java.nio.ByteBuffer.allocateDirect(description_size*2)
			bd.order(ByteOrder.LITTLE_ENDIAN)
			bd.clear()
			in.read(bd)
			bd.clear()
			val boardDescription = new StringBuilder
			while(bd.hasRemaining)
				boardDescription append bd.getChar

			output = new Solver(new Board(new String(boardDescription.result), possibleMoves.toArray), observer)
			
			val buf = java.nio.ByteBuffer.allocateDirect(buffer_size)
			buf.order(ByteOrder.LITTLE_ENDIAN)
			buf.clear

			val bytes_required = scala.math.ceil(sol.game.length / 8.0).asInstanceOf[Int]
			val buf_limit = buffer_size - 8
			
			val mask = (1L << (bytes_required*8)) - 1L

			var run = true
			while (run) {
				in.read(buf)
				
				buf.clear()
				
				// search where we have to save the fields
				val i = java.lang.Long.bitCount(buf.getLong(0) & mask)


				
				var i = 0
				try {
					i = in.readInt
				} catch {
					case _ => run = false
				}

				if (run) {
					if (i < 0)
						throw new Exception("file corruption detected, number of pegs is lower than 0")
					if (i >= output.solution.length)
						throw new Exception("file corruption detected, number of pegs is highter than the field size")

					val size = in.readInt

					val newLongHashSet = new LongHashSet(size)

					output.solution(i) = newLongHashSet

					val requiredNumberOfBitsSet = output.game.length-i

					for( pos <- 0 until size) {
						val n = in.readLong
						if(java.lang.Long.bitCount(n) == requiredNumberOfBitsSet) {
							newLongHashSet += n
						} else {
							throw new Exception("file corruption detected, invalid field")
						}
					}

				}
			}
		} finally {
			in.close
		}
*/
		//output
		null
	}

	/**
	 * save solution to a file
	 */
	def save(filename: String, sol: Solver) {
		require(sol.game.moveDirections.length <= 4)
		val out = new FileOutputStream(filename).getChannel
		try {

			// Allocate buffer for the header
			val header = java.nio.ByteBuffer.allocateDirect(
				2 + // Version
				2*4 + // Move directions
				2 + // Board description length
				2*sol.game.boardDescription.length // Board description
			)
			header.order(ByteOrder.LITTLE_ENDIAN)
			header.clear()

			header.putShort(2.asInstanceOf[Short]) // Version

			for(i <- sol.game.moveDirections)
				header.putShort(i.id.asInstanceOf[Short])
			for(i <- 0 until (4-sol.game.moveDirections.length) )
				header.putShort((-1).asInstanceOf[Short])

			// board description
			header.putShort(sol.game.boardDescription.length.asInstanceOf[Short])
			for(i <- sol.game.boardDescription)
				header.putChar(i)

			header.clear()
			while(header.hasRemaining()) {
				out.write(header);
			}

			// allocate buffer for the HashSets
			val buf = java.nio.ByteBuffer.allocateDirect(buffer_size)
			buf.order(ByteOrder.LITTLE_ENDIAN)
			buf.clear

			val bytes_required = scala.math.ceil(sol.game.length / 8.0).asInstanceOf[Int]
			val buf_limit = buffer_size - 8

			for(hs <- sol.solution ) {
				if(hs != null && hs.size > 0) {
					val iter = hs.iterator
					// we always have to fill the buffer
					var pos = 0

					while(iter.hasNext) {
						val d = iter.unsafe_next
						buf.putLong(pos, d)
						pos += bytes_required

						if(pos >= buf_limit) {
							buf.clear()
							while(buf.hasRemaining()) {
								out.write(buf);
							}
							pos = 0
							buf.clear()
						}
					}

					if(pos !=  0) {
						buf.clear()
						while(buf.hasRemaining()) {
							out.write(buf);
						}
						buf.clear
					}
				}
			}
		} finally {
			out.close
		}
	}

	def fillBuffer(buf: ByteBuffer, from: Long, length: Long) {

	}

	/*def save(out: DataOutputStream, hashset: LongHashSet) {
		if(hashset != null && hashset.size > 0) {
			out.writeInt(hashset.size)
			hashset foreach { out.writeLong(_) }
		} else {
			out.writeInt(0)
		}
	}*/

}
