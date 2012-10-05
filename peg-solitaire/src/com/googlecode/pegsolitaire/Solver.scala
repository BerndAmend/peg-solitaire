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
import Helper._
import scala.concurrent.ops._

trait StatusObserver {
	def begin_forward_calculation()
	def end_forward_calculation(required_time: Long)
	
	def begin_backward_cleaning()
	def end_backward_cleaning(required_time: Long)
	
	def begin_forward_calculation_step(removed_pegs: Int)
	def end_forward_calculation_step(removed_pegs: Int, solutions: LongHashSet)

	def begin_backward_cleaning_step(removed_pegs: Int)
	def end_backward_cleaning_step(removed_pegs: Int, deadends: Long)
	
	def dead_ends(count: Long)
}

class DummyObserver extends StatusObserver {
	def begin_forward_calculation() {}
	def end_forward_calculation(required_time: Long) {}

	def begin_backward_cleaning() {}
	def end_backward_cleaning(required_time: Long) {}

	def begin_forward_calculation_step(removed_pegs: Int) {}
	def end_forward_calculation_step(removed_pegs: Int, solutions: LongHashSet) {}

	def begin_backward_cleaning_step(removed_pegs: Int) {}
	def end_backward_cleaning_step(removed_pegs: Int, deadends: Long) {}

	def dead_ends(count: Long) {}
}

object Boards extends Enumeration {
	val English = Value("english")
	val European = Value("euro")
	val Holes15 = Value("15holes")
	val User = Value("user")

	lazy val EnglishBoard = new Board(
""". . o o o . .
. . o o o . .
o o o o o o o
o o o o o o o
o o o o o o o
. . o o o . .
. . o o o . .""", Array(MoveDirections.Horizontal, MoveDirections.Vertical))

	lazy val EuropeanBoard = new Board(
""". . o o o . .
. o o o o o .
o o o o o o o
o o o o o o o
o o o o o o o
. o o o o o .
. . o o o . .""", Array(MoveDirections.Horizontal, MoveDirections.Vertical))

	lazy val Holes15Board = new Board(
"""o . . . .
o o . . .
o o o . .
o o o o .
o o o o o""", Array(MoveDirections.Horizontal, MoveDirections.Vertical, MoveDirections.LeftDiagonal))
}

	/**
	 * @param threadCount threadCount 0=automatic
	 */
class Solver(val game: Board, val observer: StatusObserver, threadcount: Int) {
	require(threadcount >= 0)
	val thread_count = if(threadcount==0) Runtime.getRuntime.availableProcessors else threadcount

	/**
	 *  solution(0) is unused
	 */
	val solution = Array.fill(game.length)(new LongHashSet)

	/**
	 * used to count in the ctor the dead ends
	 */
	private val deadends = Array.fill[Long](game.length)(0L)

	def this(game: Board, observer: StatusObserver) = this(game, observer, 0)

	def this(game: Board, startFields: Iterable[Long], observer: StatusObserver, threadcount: Int = 0) {
		this(game, observer, threadcount)

		for(e <- startFields) {
			val bc = game.length - java.lang.Long.bitCount(e)
			if(bc <= 0)
				throw new Exception("Invalid number of bits set (" + e.toString + ")")

			solution(bc) += e
		}


		observer.begin_forward_calculation()
		Time(observer.end_forward_calculation _) {
			for (sol <- (getStartNum+1) until game.length) {
				observer.begin_forward_calculation_step(sol)
				calculateForward(sol)
				observer.end_forward_calculation_step(sol, solution(sol))
			}
		}

		// cleaning the gamefield after every step is useless
		observer.begin_backward_cleaning()
		Time(observer.end_backward_cleaning _) {
			cleanBackward(getEndNum)
		}

		observer.dead_ends(deadends.sum)
	}

	protected def getCompleteList(solutionNumber: Int): LongHashSet = game.getCompleteList(solution(solutionNumber))
	def getCompleteList(h: LongHashSet) = game.getCompleteList(h)

	/**
	 *  @return a list of all possible start-fields
	 */
	def getStart: LongHashSet = getCompleteList(getStartNum)

	/**
	 * @return first non empty solution set id
	 */
	def getStartNum: Int = {
		var start = 0
		while(solution(start) == null || solution(start).size == 0) {
			start += 1

			if(start == game.length)
				throw new Exception("no set field found")
		}

		start
	}

	/**
	 *  @return a list of all possible end-fields
	 */
	def getEnd: LongHashSet = getCompleteList(getEndNum)

	/**
	 * @return last non empty solution set id
	 */
	def getEndNum: Int = {
		var end = game.length-1
		while(solution(end) == null || solution(end).size == 0) {
			end -= 1

			if(end == -1)
				throw new Exception("solution is empty")
		}

		end
	}

	/**
	 * @return all follower for a provided field
	 */
	def getFollower(field: Long): LongHashSet = {
		val fieldPos = game.length - java.lang.Long.bitCount(field)
		if (fieldPos + 1 >= game.length)
			return new LongHashSet
		val next = solution(fieldPos + 1)
		if(next == null)
			return new LongHashSet

		game.getFollower(field, next)
	}

	/**
	 * @return all follower for a provided field
	 */
	def getPredecessorSet(field: Long): LongHashSet = {
		val fieldPos = game.length - java.lang.Long.bitCount(field)
		if (fieldPos - 1 <= 0)
			return new LongHashSet
		game.getPredecessor(field, solution(fieldPos - 1))
	}

	/**
	 * Count how many ways are available to solve the board (this may take a while)
	 * ToDo: use getStart and getEnd to allow incomplete games
	 */
	def countPossibleGames(): BigDecimal = {
		var previous: scala.collection.mutable.HashMap[Long, BigDecimal] = null
		var current = new scala.collection.mutable.HashMap[Long, BigDecimal]

		// init the current with BigDecimal = 1
		game.getCompleteList(solution(game.length-1)) foreach {v => current(v) = 1 }

		for (i <- (game.length - 2).to(1, -1)) {
			print("  determine values for " + i + " removed pegs\r")
			Console.flush

			previous = current
			current = new scala.collection.mutable.HashMap[Long, BigDecimal]

			game.getCompleteList(solution(i)) foreach {
					v =>
						getFollower(v) foreach {
							f =>
							try {
								current(v) += previous(f)
							} catch {
								case _ => current(v) = previous(f)
							}
						}
				}
		}

		var count = BigDecimal(0)
		for(s <- current)
			count += s._2

		count
	}

	private def calculateForward (sol: Int): Unit = calculateNextStep(sol, -1, true) //game.addFollower)
	private def calculateBackward(sol: Int): Unit = calculateNextStep(sol,  1, false) //game.addPredecessor)

  private def calculateNextStep(sol: Int, next: Int, follower: Boolean) {
	  val results = (0 until thread_count).map {
      threadID => future[Boolean] {
        val iter = solution(sol + next).iterator(threadID, thread_count)
        var result = new LongHashSet()
	      if(follower) {
	        while (iter.hasNext) {
		        game.addFollower(iter.unsafe_next, result)
          }
	      } else {
		      while (iter.hasNext) {
			      game.addPredecessor(iter.unsafe_next, result)
          }
	      }
	      val current = solution(sol)
	      current.synchronized {
		      current += result
	      }
        true
      }
    }

	  results foreach (_())
  }

	private def cleanForward(pos: Int) = cleanNextStep(pos, 1, pos, getEndNum, false) //game.hasPredecessor)
	private def cleanBackward(pos: Int) = cleanNextStep(pos, -1, pos, getStartNum, true) //game.hasFollower)

	private def cleanNextStep(pos: Int, next: Int, from: Int, to: Int, follower: Boolean) { // func: (Long, LongHashSet) => Boolean) {
		for (i <- from.until(to, next)) {

			observer.begin_backward_cleaning_step(i+next)

			val results = (0 until thread_count).map {
				threadID => future[(Long, LongHashSet)] {
					var deadEndFields = 0L
					val current = solution(i)
					val iter = solution(i + next).iterator(threadID, thread_count)
					var result = new LongHashSet
					if(follower) {
						while (iter.hasNext) {
							val elem = iter.unsafe_next
							if (game.hasFollower(elem, current)) {
								result += elem
							} else {
								deadEndFields += 1L
							}
						}
					} else {
						while (iter.hasNext) {
							val elem = iter.unsafe_next
							if (game.hasPredecessor(elem, current)) {
								result += elem
							} else {
								deadEndFields += 1L
							}
						}
					}
					(deadEndFields, result)
				}
			}

			// merge
			val resultSize = results.foldLeft(0) ( _ + _()._2.size) // wait for results and calculate new hashset size
			val newsol = new LongHashSet(resultSize)
			var deadEndFields = 0L
			for (e <- results) {
				val r = e()
				deadEndFields += r._1 // update dead end counter
				newsol += r._2
			}

			solution(i + next) = newsol
			deadends(i + next) += deadEndFields

			observer.end_backward_cleaning_step(i+next, deadEndFields)
			if (deadEndFields == 0L) {
				return
			}
		}
	}

}
