#ifndef MANGOSSERVER_WORLDPACKET_H
#define MANGOSSERVER_WORLDPACKET_H

#include "ByteBuffer.h"

class WorldPacket : public ByteBuffer
{
    public:
        WorldPacket()                                       : ByteBuffer(0), opcode_(0)
        {
            // do nothing
        }

        explicit WorldPacket(uint16_t opcode, size_t res=200) : ByteBuffer(res), opcode_(opcode) 
        {
            // do nothing
        }
                        
        WorldPacket(const WorldPacket &packet) : ByteBuffer(packet), opcode_(packet.opcode_)
        {
            // do nothing
        }

        void Init(uint16_t opcode, size_t newres=200)
        {
            clear();
            storage_.reserve(newres);
            opcode_ = opcode;
        }

        uint16_t GetOpcode() const { return opcode_; }
        void SetOpcode(uint16_t opcode) { opcode_ = opcode; }

    protected:
        uint16_t opcode_;
};
#endif
