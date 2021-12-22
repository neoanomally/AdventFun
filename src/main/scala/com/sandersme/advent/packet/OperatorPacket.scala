package com.sandersme.advent.packet

case class OperatorPacket(value: PacketValue,
                          version: Version,
                          pType: PacketType) extends Packet(version, pType, value)


