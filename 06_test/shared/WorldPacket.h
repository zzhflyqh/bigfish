#ifndef MANGOSSERVER_WORLDPACKET_H
#define MANGOSSERVER_WORLDPACKET_H

#include "ByteBuffer.h"

class WorldPacket : public ByteBuffer
{
    public:
        WorldPacket()                                       : ByteBuffer(0), m_opcode(0)
        {
        }
        explicit WorldPacket(uint16_t opcode, size_t res=200) : ByteBuffer(res), m_opcode(opcode) { }
                                                            // copy constructor
        WorldPacket(const WorldPacket &packet)              : ByteBuffer(packet), m_opcode(packet.m_opcode)
        {
        }

        void Init(uint16_t opcode, size_t newres=200)
        {
            clear();
            _storage.reserve(newres);
            m_opcode = opcode;
        }

        uint16_t GetOpcode() const { return m_opcode; }
        void SetOpcode(uint16_t opcode) { m_opcode = opcode; }

    protected:
        uint16_t m_opcode;
};
#endif
