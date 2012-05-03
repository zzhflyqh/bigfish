#include <assert.h>
#include "ar.h"
#include "misc.h"
#include "dbgnew.h"

MemPooler<Ar>* Ar::mem_pool_ = NULL;
Ar::Ar( const void* buf, u_int buf_size)
{
	BEFORE_CHECK(0);
	if( buf )
	{
		mode_ = load;
		buf_start_ = (char*)buf;
		buf_size_ = buf_size;
        buffer_ = NULL;
	}
	else
	{
		mode_ = store;
		buffer_ = create_buffer();
		buf_start_ = buffer_->get_writable_buffer( &buf_size_ );
	}
	
	buf_max_ = buf_start_ + buf_size_;
	buf_cur_ = buf_start_;

    turnto_read_ = false;

	clear_exception();
}

Ar::~Ar()
{
    /*! turnto_read_ 表示把write的ar改为load的ar*/
	if( is_storing() || turnto_read_ ) {
		SAFE_DELETE(buffer_);
	}
	AFTER_CHECK(0);
}

void Ar::turnto_read( )
{
    if ( mode_ == store ) {
        buf_cur_ = buf_start_;
        mode_ = load;
        turnto_read_ = true;
    }
}

void Ar::read( void *buf, int buf_size)
{
	assert( is_loading() );
	assert( buf );
	assert( buf_size >=0 );

	if( buf_cur_ + buf_size > buf_max_ )
	{
		LOG(2)("[AR_ERR](read) error occur when read :m_lpBufCur + nBufSize[%lu] > m_lpBufMax.", buf_size);
		set_exception();
		buf_cur_ = buf_max_;
		return;
	}
	
	memcpy( buf, buf_cur_, buf_size );
	buf_cur_ += buf_size;
}

char* Ar::read_string( char* buf, int buf_size )
{
	assert( is_loading() );
	assert( buf );
	assert( buf_size>0 );
	
	u_short len;
	*this >> len;

    /*
	if( (len >= buf_size) || (buf_cur_+len > buf_max_) )
	{
		LOG(2)("[AR_ERR][read string] error occur when read string: nLen [%d], nBufSize [%d]", len, buf_size);
		buf_cur_ = buf_max_;
		set_exception();
		buf[buf_size - 1] = '\0';
		return buf;
	}*/

    if ( buf_cur_+len > buf_max_) 
	{
		LOG(2)("[AR_ERR][read string] buf_cur_+len > buf_max_, error occur when read string: nLen [%d], nBufSize [%d], buf_cur_: %d, buf_max_: %d", len, buf_size, buf_cur_, buf_max_ );
		buf_cur_ = buf_max_;
		set_exception();
		buf[buf_size - 1] = '\0';
		return buf;
	}

	if(len >= buf_size) {
		LOG(2)("[AR_ERR][read string] len>=buf_size, error occur when read string: nLen [%d], nBufSize [%d]", len, buf_size);
		buf_cur_ = buf_cur_ + len;
		set_exception();
		buf[buf_size - 1] = '\0';
		return buf;
    }

    if( len>8192 ){
        ERR(2)("[AR_ERR][read string] more 8192,  error occur when read string: nLen [%d], nBufSize [%d]", len, buf_size);
    }
	
	memcpy( buf,  buf_cur_, len );
	buf[len] = '\0';
	buf_cur_ += len;
	
	return buf;	
}

void Ar::write( const void* buf, int buf_size )
{
	assert( is_storing() );
	assert( buf );
	assert( buf_size>=0 );
	
	if( check_buf( buf_size ) ) return;
	memcpy( buf_cur_, buf, buf_size );
	buf_cur_ += buf_size;	
}


void Ar::write_string(const char *buf, int buf_size)
{
	assert( buf );
	
	u_short len = strlen( buf );

	if ( len > buf_size )
		len = buf_size;

	*this << len;
	write( buf, sizeof(char) * len );
}


void Ar::write_string(const char *buf )
{
	assert( buf );
	
	u_short len = strlen( buf );

	*this << len;
	write( buf, sizeof(char) * len );
}


int Ar::check_buf( int size )
{
    assert( size>=0 );

    if( buf_cur_ + size < buf_max_ ) {
        return 0;
    }

    u_int offset = buf_cur_ - buf_start_;
    u_int extension = MAX_BUFFER * (size / MAX_BUFFER + 1 );
    u_int  tmp_size = (buf_max_ - buf_start_) + extension;
    Buffer* new_buf = create_buffer( tmp_size );
    if( new_buf == NULL ) {
		ERR(2)("[AR](ar) Ar::check_buf(), BUFFER_ERROR, packet_size = 0x%08X, dpid = 0x%08X", tmp_size, buffer_->dpid_ );
        return -1;
    }

    memcpy( new_buf->buf_start_, buffer_->buf_start_, offset );
    new_buf->dpid_ = buffer_->dpid_;

    delete buffer_;

    buffer_ = new_buf;
    buf_start_  = buffer_->buf_start_;
    buf_max_    = buffer_->buf_start_ + tmp_size;
    buf_cur_    = buffer_->buf_start_ + offset;

    buf_size_ = buf_max_ - buf_cur_;

    return 0;
}

char* Ar::get_buffer( int* buf_size )
{
	assert( is_storing() );
	assert( buf_size );
	
	*buf_size = buf_cur_ - buf_start_;
	return buf_start_;
}

void Ar::write_int_at( int _num, u_long _offset )
{	
	int bufsize;
	char* buf = get_buffer( &bufsize );
	*(int*)( buf + _offset )	= _num;
}

void Ar::write_byte_at( unsigned char _num, u_long _offset )
{
	int bufsize;
	char* buf = get_buffer( &bufsize );
	*(unsigned char*)( buf + _offset ) = _num;
}

void Ar::flush( void )
{
	assert( is_storing() );
	
	buf_cur_ = buf_start_;
}

void Ar::reel_in( unsigned int offset )
{
	assert ( is_storing() ) ;

	if( buf_start_ + offset > buf_cur_ ) 
	{
		LOG(2)("[AR_ERR](reelin) error occur when reelin: m_lpBufStart + uOffset[%lu] > m_lpBufCur.", offset);
		return;
	}

	buf_cur_	= buf_start_ + offset;
}

int Ar::peek_int()
{
	if( buf_max_ - buf_cur_ < 4 ) return -1;
	int value = *(int *)buf_cur_;
	
	return value;
}

unsigned short Ar::peek_short()
{
	if( buf_max_ - buf_cur_ < 2 ) return 0;
	unsigned short value = *(unsigned short *)buf_cur_;
	
	return value;
}

unsigned short Ar::peek_type( const char* buf, int len )
{
	if( len < 2 ) return 0;

    unsigned char tmp1 = *(unsigned char *)buf;
    unsigned char tmp2 = *(unsigned char *)(buf+1);

    return (unsigned short)((tmp1<< 8) | tmp2);        
    /*
//    if ( tmp1<ST_EXTEND ) {
    if ( tmp1<0x7f ) {
        return (unsigned short)tmp1;
    } else {
        return (unsigned short)((tmp1<< 8) | tmp2);        
    }
    */
}

void Ar::reset()
{
    if(buffer_)
        buffer_->reset();

    turnto_read_  = false;
    mode_ = store;
    buf_cur_ = buf_start_;
}

void Ar::recreate_buffer()
{
	buffer_ = create_buffer( MAX_BUFFER );
    buf_start_  = buffer_->buf_start_;
	buf_cur_    = buffer_->buf_start_;
	buf_max_    = buffer_->buf_start_ + MAX_BUFFER;

    buf_size_ = buf_max_ - buf_cur_;
}
