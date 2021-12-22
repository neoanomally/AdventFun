package com.sandersme.advent.packet

case class LiteralPacket(value: PacketValue,
                         pType: PacketType,
                         version: Version) extends Packet(version, pType, value)