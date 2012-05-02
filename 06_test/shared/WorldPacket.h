#ifndef MANGOSSERVER_WORLDPACKET_H
#define MANGOSSERVER_WORLDPACKET_H

#include "ByteBuffer.h"

class WorldPacket : public ByteBuffer
{
    public:
        WorldPacket()                                       : ByteBuffer(0), _opcode(0)
        {
            // do nothing
        }

        explicit WorldPacket(uint16_t opcode, size_t res=200) : ByteBuffer(res), _opcode(opcode) 
        {
            // do nothing
        }
                        
        WorldPacket(const WorldPacket &packet) : ByteBuffer(packet), _opcode(packet.m_opcode)
        {
            // do nothing
        }

        void Init(uint16_t opcode, size_t newres=200)
        {
            clear();
            _storage.reserve(newres);
            _opcode = opcode;
        }

        uint16_t GetOpcode() const { return _opcode; }
        void SetOpcode(uint16_t opcode) { _opcode = opcode; }

    protected:
        uint16_t _opcode;
};
#endif
