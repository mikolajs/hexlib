package eu.brosbit.hexlib

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class PathFinderSpec extends  AnyFlatSpec, Matchers:
  val hex = new Hex(10, 10)
  val map = Array(
    Array(1, 1, -1, -1, 1, 1, 1, 1, 1, 2),
    Array(2, -1, -1, 1, 2, 1, 3, 2, 1, 2),
    Array(1, 1, -1, -1, 1, 1, 3, 3, 2, 1),
    Array(1, 2, 1, -1, -1, 1, 1, 1, 1, 3),
    Array(1, 3, 1, -1, -1, -1, 1, 2, 1, 3),
    Array(1, 3, 3, 1, -1, -1, 1, 1, 1, 3),
    Array(1, 1, 3, 1, -1, -1, 1, 2, 1, 1),
    Array(2, 1, 1, 2, 2, 2, 2, 2, 1, 3),
    Array(2, 2, 1, 3, 3, 2, 2, 2, -1, 1),
    Array(1, 3, 1, 1, 1, 3, 1, 1, -1, 2)
  )

  val pf = PathFinder(hex)
  
  "Find path " should " map have  10 lines" in {
     map.length should be (10)
  }
  it should " map have all lines be 10 length " in {
    map.forall(_.length == 10)
  }
  it should " from 0,1 to 6,1 have 6 hexes" in {
    val pathList = pf.findPath(MapPosition(0,1), MapPosition(6, 1), map)
    pathList.size should be (6)
  }
  it should " from 0,1 to 6,1 distance 7" in {
    val pathList = pf.findPath(MapPosition(0, 1), MapPosition(6, 1), map)
    val s = pf.countPath(pathList, map)
    s should be(7)
  }
  it should " from 0,1 to 0,5 have 18 hexes" in {
    val pathList = pf.findPath(MapPosition(0, 1), MapPosition(0, 5), map)
    pathList.size should be(18)
  }
  it should " from 0,1 to 0,5 distance 22" in {
    val pathList = pf.findPath(MapPosition(0, 1), MapPosition(0, 5), map)
    val s = pf.countPath(pathList, map)
    s should be(22)
  }

  it should " from 7,8 to 9,9 have 2 hexes" in {
    val pathList = pf.findPath(MapPosition(7, 8), MapPosition(9, 9), map)
    pathList.size should be(2)
  }
  it should " from 7,8 to 9,9 distance 22" in {
    val pathList = pf.findPath(MapPosition(7, 8), MapPosition(9, 9), map)
    val s = pf.countPath(pathList, map)
    s should be(3)
  }
  it should " from 9,3 to 2,9 have 9 hexes" in {
    val pathList = pf.findPath(MapPosition(9, 3), MapPosition(2, 9), map)
    pathList.size should be(9)
  }
  it should " from 9,3 to 2,9 distance 11" in {
    val pathList = pf.findPath(MapPosition(9, 3), MapPosition(2, 9), map)
    val s = pf.countPath(pathList, map)
    s should be(11)
  }
  it should " from 9,3 to 9,9 have 9 hexes" in {
    val pathList = pf.findPath(MapPosition(9, 3), MapPosition(9, 9), map)
    //maybe 8, 9 or 10 but algorithm choose 10
    pathList.size should be(10)
  }
  it should " from 9,3 to 9,9 distance 13" in {
    val pathList = pf.findPath(MapPosition(9, 3), MapPosition(9, 9), map)
    val s = pf.countPath(pathList, map)
    s should be(13)
  }
end PathFinderSpec
