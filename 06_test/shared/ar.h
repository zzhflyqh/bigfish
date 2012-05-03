#ifndef __AR_H__
#define __AR_H__

#include "log.h"

struct _DOUBLE  { char doubleBits[sizeof(double)]; };
struct _float   { char floatBits[sizeof(float)]; };

#define CHECK_AR( _car ) \
{ \
	if( (_car).check_exception() ) \
	{ \
		ERR(2)( "[AR_ERR](CHECK_AR) %s:%d", __FILE__, __LINE__ ); \
	} \
}

#define safe_read_string( _ar, _buf, _buff_size ) \
{ \
	(_ar).read_string( (_buf), (_buff_size) ); \
	if((_ar).check_exception() ) \
	{ \
		ERR(2)( "[AR_ERR](AR_READ_STRING) %s:%d", __FILE__, __LINE__ ); \
	} \
}

class Ar
{
public:
	LOWER_BOUND(0);
	Ar( const void* buf = NULL, u_int buf_size = 0 );	
	~Ar();
public:
    /*! add by syd */
    void reset();

	bool is_loading() const;
	bool is_storing() const;
	
	void read( void * buf, int buf_size);
	void write(const void *buf, int buf_size);
	
	char* read_string(char* buf, int buf_size);
	void write_string(const char *buf, int buf_size);
	void write_string(const char *buf );
	
	void flush(void);
	void reel_in( unsigned int offset );
	int peek_int();
    unsigned short peek_short();
    unsigned short peek_type( const char* buf, int len );
	int check_buf(int size);					//检查buff符合要求，返回0，失败返回非0
	char* get_buffer(int* buf_size);
	u_long get_offset(void);
	void write_int_at( int num, u_long _offset );
	void write_byte_at( unsigned char num, u_long _offset );
	void recreate_buffer();
    void turnto_read();
	
	void clear_exception();
	bool check_exception();
	void set_exception();

public:
	Ar& operator<<(char by); // -127 ~ +127
	Ar& operator<<(unsigned char uc);
	Ar& operator<<(short w);
	Ar& operator<<(unsigned short us);
	Ar& operator<<(int i);
	Ar& operator<<(unsigned int i);
	Ar& operator<<(float f);
	Ar& operator<<(double d);

	Ar& operator>>(char& by);
	Ar& operator>>(unsigned char& uc);
	Ar& operator>>(short& w);
	Ar& operator>>(unsigned short& us);
	Ar& operator>>(int& i);
	Ar& operator>>(unsigned int& i);
	Ar& operator>>(float& f);
	Ar& operator>>(double& d);
	
public:
	enum { store =0, load =1 };

	Buffer* buffer_;
	int errno_;

public:
	static MemPooler<Ar>* mem_pool_;

	void * operator new( size_t size ) { (void)size; return Ar::mem_pool_->alloc(); }
	void * operator new( size_t size, char * filename, int line ) { (void)size; (void)filename; (void)line; return Ar::mem_pool_->alloc(); }
	void operator delete( void * ptr ) { Ar::mem_pool_->free( (Ar*)ptr ); }
	void operator delete( void * ptr, char * filename, int line ) { Ar::mem_pool_->free( (Ar*)ptr ); }
	

	char* buf_cur_;
	char* buf_max_;

protected:
    bool turnto_read_;
	char mode_;
	int buf_size_;
	char* buf_start_;

	UPPER_BOUND(0);
};

inline bool Ar::is_loading() const
{
	return ( mode_ == Ar::load );
}

inline bool Ar::is_storing() const
{
	return ( mode_ == Ar::store );
}

inline Ar& Ar::operator<<(unsigned short us)
{
	return Ar::operator<<((short)us);
}

inline Ar& Ar::operator<<(short w)
{
	if( check_buf( sizeof( short ) ) )
		return *this;
	*(short*)buf_cur_ = HTONS(w); 
	buf_cur_ += sizeof(short); 
	
	return *this; 
}

inline Ar& Ar::operator<<(int l)
{
	if( check_buf( sizeof(int) ) )
		return *this;
	*(int*)buf_cur_ = HTONL(l); 
	buf_cur_ += sizeof(int); 

	return *this; 
}

inline Ar& Ar::operator<<(unsigned int i)
{
	return Ar::operator<<((int)i); 
}


inline Ar& Ar::operator<<(unsigned char uc)
{
	return Ar::operator<<((char)uc); 
}

inline Ar& Ar::operator<<(char ch)
{
	if( check_buf( sizeof(char) ) )
		return *this;
	*buf_cur_ = ch; 
	buf_cur_ += sizeof(char); 
	
	return *this; 
}

inline Ar& Ar::operator<<(float f)
{
	if( check_buf( sizeof(float) ) )
		return *this;
	*(_float *)buf_cur_ = *(_float*)&f; 
	buf_cur_ += sizeof(float); 
	
	return *this; 
}

inline Ar& Ar::operator<<(double d)
{
	if( check_buf( sizeof(double) ) )
		return *this;
	*(_DOUBLE *)buf_cur_ = *(_DOUBLE*)&d; 
	buf_cur_ += sizeof(double); 
	
	return *this; 
}

inline Ar& Ar::operator>>(unsigned short& us)
{
	return Ar::operator>>((short&)us);
}

inline Ar& Ar::operator>>(short& w)
{
	if( buf_cur_ + sizeof(short) > buf_max_ ) {
		w = 0;
		buf_cur_ = buf_max_;
		set_exception();
	} else {
		w = NTOHS(*(short*)buf_cur_); 
		buf_cur_ += sizeof(short); 
	}

	return *this; 
}

inline Ar& Ar::operator>>(unsigned char& uc)
{
	return  Ar::operator>>((char&)uc);
}

inline Ar& Ar::operator>>(char& ch)
{
	if( buf_cur_ + sizeof(char) > buf_max_ ) {
		ch = 0;
		buf_cur_ = buf_max_;
		set_exception();
	} else {
		ch = *buf_cur_; 
		buf_cur_ += sizeof(char); 
	}

	return *this; 
}

inline Ar& Ar::operator>>(int& l)
{
	if( buf_cur_ + sizeof(int) > buf_max_ ) {
		l = 0;
		buf_cur_ = buf_max_;
		set_exception();
	} else {
		l = NTOHL(*(int*)buf_cur_); 
		buf_cur_ += sizeof(int); 
	}

	return *this; 
}

inline Ar& Ar::operator>>(unsigned int& i)
{
	return Ar::operator>>((int&)i);
}

inline Ar& Ar::operator>>(float& f)
{
	if( buf_cur_ + sizeof(float) > buf_max_ ) {
		f = 0.0f;
		buf_cur_ = buf_max_;
		set_exception();
	} else {
		*(_float*)&f = *(_float*)buf_cur_; 
		buf_cur_ += sizeof(float); 
	}

	return *this; 
}

inline Ar& Ar::operator>>(double& d)
{
	if( buf_cur_ + sizeof(double) > buf_max_ ) {
		d = 0.0f;
		buf_cur_ = buf_max_;
		set_exception();
	} else {
		*(_DOUBLE*)&d = *(_DOUBLE*)buf_cur_; 
		buf_cur_ += sizeof(double); 
	}

	return *this; 
}

inline u_long Ar::get_offset(void)
{
	if( !is_storing() )
	{
		LOG(2)("[AR_ERR](get offset) error occur when get offset: not in store mode.");
		set_exception();
		return 0;
	}
	
	return ( buf_cur_ - buf_start_ );
}

inline void Ar::clear_exception()
{
	errno_ = 0;
}

inline bool Ar::check_exception()
{
	return (errno_!=0);
}

inline void Ar::set_exception()
{
	errno_ = 1;
}

#endif

