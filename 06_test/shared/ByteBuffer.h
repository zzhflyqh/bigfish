#ifndef _BYTEBUFFER_H
#define _BYTEBUFFER_H
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <vector>
#include <stddef.h>
#include <linux/types.h>
#include <stdint.h>
#include <string.h>
#include <string>
#include <list>
#include <map>

#ifndef ASSERT
#define ASSERT(T) assert(T)
#endif

class ByteBuffer
{
    public:
        const static size_t DEFAULT_SIZE = 0x1000;

        // constructor
        ByteBuffer(): _rpos(0), _wpos(0)
        {
            _storage.reserve(DEFAULT_SIZE);
        }
        // constructor
        ByteBuffer(size_t res): _rpos(0), _wpos(0)
        {
            _storage.reserve(res);
        }
        // copy constructor
        ByteBuffer(const ByteBuffer &buf): _rpos(buf._rpos), _wpos(buf._wpos), _storage(buf._storage) { }

        void clear()
        {
            _storage.clear();
            _rpos = _wpos = 0;
        }

        template <typename T> void append(T value)
        {
            append((uint8_t *)&value, sizeof(value));
        }
        template <typename T> void put(size_t pos,T value)
        {
            put(pos,(uint8_t *)&value,sizeof(value));
        }

        ByteBuffer &operator<<(bool value)
        {
            append<char>((char)value);
            return *this;
        }
        ByteBuffer &operator<<(uint8_t value)
        {
            append<uint8_t>(value);
            return *this;
        }
        ByteBuffer &operator<<(uint16_t value)
        {
            append<uint16_t>(value);
            return *this;
        }
        ByteBuffer &operator<<(uint32_t value)
        {
            append<uint32_t>(value);
            return *this;
        }
        ByteBuffer &operator<<(uint64_t value)
        {
            append<uint64_t>(value);
            return *this;
        }

        // signed as in 2e complement
        ByteBuffer &operator<<(int8_t value)
        {
            append<int8_t>(value);
            return *this;
        }
        ByteBuffer &operator<<(int16_t value)
        {
            append<int16_t>(value);
            return *this;
        }
        ByteBuffer &operator<<(int32_t value)
        {
            append<int32_t>(value);
            return *this;
        }
        ByteBuffer &operator<<(int64_t value)
        {
            append<int64_t>(value);
            return *this;
        }

        // floating points
        ByteBuffer &operator<<(float value)
        {
            append<float>(value);
            return *this;
        }
        ByteBuffer &operator<<(double value)
        {
            append<double>(value);
            return *this;
        }
        ByteBuffer &operator<<(const std::string &value)
        {
            append((uint8_t const *)value.c_str(), value.length());
            append((uint8_t)0);
            return *this;
        }
        ByteBuffer &operator<<(const char *str)
        {
            append((uint8_t const *)str, str ? strlen(str) : 0);
            append((uint8_t)0);
            return *this;
        }

        ByteBuffer &operator>>(bool &value)
        {
            value = read<char>() > 0 ? true : false;
            return *this;
        }
        ByteBuffer &operator>>(uint8_t &value)
        {
            value = read<uint8_t>();
            return *this;
        }
        ByteBuffer &operator>>(uint16_t &value)
        {
            value = read<uint16_t>();
            return *this;
        }
        ByteBuffer &operator>>(uint32_t &value)
        {
            value = read<uint32_t>();
            return *this;
        }
        ByteBuffer &operator>>(uint64_t &value)
        {
            value = read<uint64_t>();
            return *this;
        }

        //signed as in 2e complement
        ByteBuffer &operator>>(int8_t &value)
        {
            value = read<int8_t>();
            return *this;
        }
        ByteBuffer &operator>>(int16_t &value)
        {
            value = read<int16_t>();
            return *this;
        }
        ByteBuffer &operator>>(int32_t &value)
        {
            value = read<int32_t>();
            return *this;
        }
        ByteBuffer &operator>>(int64_t &value)
        {
            value = read<int64_t>();
            return *this;
        }

        ByteBuffer &operator>>(float &value)
        {
            value = read<float>();
            return *this;
        }
        ByteBuffer &operator>>(double &value)
        {
            value = read<double>();
            return *this;
        }
        ByteBuffer &operator>>(std::string& value)
        {
            value.clear();
            while (rpos() < size())                         // prevent crash at wrong string format in packet
            {
                char c=read<char>();
                if (c==0)
                    break;
                value+=c;
            }
            return *this;
        }

        uint8_t operator[](size_t pos)
        {
            return read<uint8_t>(pos);
        }

        size_t rpos()
        {
            return _rpos;
        };

        size_t rpos(size_t rpos_)
        {
            _rpos = rpos_;
            return _rpos;
        };

        size_t wpos()
        {
            return _wpos;
        }

        size_t wpos(size_t wpos_)
        {
            _wpos = wpos_;
            return _wpos;
        }

        template <typename T> T read()
        {
            T r=read<T>(_rpos);
            _rpos += sizeof(T);
            return r;
        };
        template <typename T> T read(size_t pos) const
        {
            ASSERT(pos + sizeof(T) <= size() || PrintPosError(false,pos,sizeof(T)));
            return *((T const*)&_storage[pos]);
        }

        void read(uint8_t *dest, size_t len)
        {
            ASSERT(_rpos  + len  <= size() || PrintPosError(false,_rpos,len));
            memcpy(dest, &_storage[_rpos], len);
            _rpos += len;
        }

        const uint8_t *contents() const { return &_storage[0]; }

        size_t size() const { return _storage.size(); }
        bool empty() const { return _storage.empty(); }

        void resize(size_t newsize)
        {
            _storage.resize(newsize);
            _rpos = 0;
            _wpos = size();
        };
        void reserve(size_t ressize)
        {
            if (ressize > size()) _storage.reserve(ressize);
        };

        void append(const std::string& str)
        {
            append((uint8_t const*)str.c_str(),str.size() + 1);
        }
        void append(const char *src, size_t cnt)
        {
            return append((const uint8_t *)src, cnt);
        }
        void append(const uint8_t *src, size_t cnt)
        {
            if (!cnt) return;

            ASSERT(size() < 10000000);

            if (_storage.size() < _wpos + cnt)
                _storage.resize(_wpos + cnt);
            memcpy(&_storage[_wpos], src, cnt);
            _wpos += cnt;
        }
        void append(const ByteBuffer& buffer)
        {
            if(buffer.size()) append(buffer.contents(),buffer.size());
        }

        void appendPackGUID(uint64_t guid)
        {
            size_t mask_position = wpos();
            *this << uint8_t(0);
            for(uint8_t i = 0; i < 8; i++)
            {
                if(guid & 0xFF)
                {
                    _storage[mask_position] |= (1<<i);
                    *this << ((uint8_t)(guid & 0xFF));
                }

                guid >>= 8;
            }
        }

        void put(size_t pos, const uint8_t *src, size_t cnt)
        {
            ASSERT(pos + cnt <= size() || PrintPosError(true,pos,cnt));
            memcpy(&_storage[pos], src, cnt);
        }

    protected:
        bool PrintPosError(bool add, size_t pos, size_t esize) const
        {
            fprintf(stderr, "ERROR: Attempt %s in ByteBuffer (pos: %u size: %u) value with size: %u",(add ? "put" : "get"),pos, size(), esize);
            return false;
        }

        size_t _rpos, _wpos;
        std::vector<uint8_t> _storage;
};

template <typename T> ByteBuffer &operator<<(ByteBuffer &b, std::vector<T> v)
{
    b << (uint32_t)v.size();
    for (typename std::vector<T>::iterator i = v.begin(); i != v.end(); i++)
    {
        b << *i;
    }
    return b;
}

template <typename T> ByteBuffer &operator>>(ByteBuffer &b, std::vector<T> &v)
{
    uint32_t vsize;
    b >> vsize;
    v.clear();
    while(vsize--)
    {
        T t;
        b >> t;
        v.push_back(t);
    }
    return b;
}

template <typename T> ByteBuffer &operator<<(ByteBuffer &b, std::list<T> v)
{
    b << (uint32_t)v.size();
    for (typename std::list<T>::iterator i = v.begin(); i != v.end(); i++)
    {
        b << *i;
    }
    return b;
}

template <typename T> ByteBuffer &operator>>(ByteBuffer &b, std::list<T> &v)
{
    uint32_t vsize;
    b >> vsize;
    v.clear();
    while(vsize--)
    {
        T t;
        b >> t;
        v.push_back(t);
    }
    return b;
}

template <typename K, typename V> ByteBuffer &operator<<(ByteBuffer &b, std::map<K, V> &m)
{
    b << (uint32_t)m.size();
    for (typename std::map<K, V>::iterator i = m.begin(); i != m.end(); i++)
    {
        b << i->first << i->second;
    }
    return b;
}

template <typename K, typename V> ByteBuffer &operator>>(ByteBuffer &b, std::map<K, V> &m)
{
    uint32_t msize;
    b >> msize;
    m.clear();
    while(msize--)
    {
        K k;
        V v;
        b >> k >> v;
        m.insert(make_pair(k, v));
    }
    return b;
}
#endif
