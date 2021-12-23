package com.sandersme.advent.packet

/**
 * Operator packet isn't defined in part 1, will need to come back and make sure there
 * aren't any specific defintiions.
 *
 * @param value
 * @param version
 * @param pType
 */
case class OperatorPacket(override val version: Version,
                          override val ptype: PType,
                          bits: Bits,
                          override val packets: List[Packet]) extends Packet {

  override def toString: String = {
    s"${version}, Operator ${ptype}, ChildPackets: ${packets}"
  }

  // COOL SEMIGROUPS AND MONOIDS 8)
  // TODO: One small issue with this is that it is not tail recursive.
  // We are recursively calling value, which on operator packets it gets their children. This may cause
  // stack issues on deeply nested children. I think this wouldn't be a problem with literal values, but
  // could be a problem with DEEEPLY nested operator values
  // One quick thought on how we could make this @tailrec is to see if the operator has any children
  // if yes do a thing else not call itsself. BUt we'd move value to it's own function specifically
  // for tailrec. I spent more time typing this out than it would be to meet those requirements. But that's
  // fine because this is simpler to read and reason about (at least while I write this ;) )
  override def value: BigInt = {
    ptype.value match {
      case 0 => packetValues.sum
      case 1 => packetValues.product
      case 2 => packetValues.min
      case 3 => packetValues.max
      case 5 => if (packetValues.head > packetValues.drop(1).head) 1 else 0
      case 6 =>  if (packetValues.head < packetValues.drop(1).head) 1 else 0
      case 7 => if (packetValues.head == packetValues.drop(1).head) 1 else 0
      case _ => throw new Exception(s"Packet value { ${ptype.value} }is not supported")
    }
  }

  private[packet] lazy val packetValues: List[BigInt] = {
    packets.map(_.value)
  }
}