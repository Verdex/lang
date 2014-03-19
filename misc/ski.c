
#include<stdlib.h>
#include<stdio.h>


struct function {
    int needs;
    int has;
    void** params;
    void* (*function)(void**);
};

void* call( struct function* f, void* param ) {
    f->params[f->has] = param;
    f->has++;
    if ( f->needs == f->has ) {
        return f->function( f->params );
    } else {
        return f;
    }     
}

// k x y = x
void* k_base( void** params ) {
    return params[0];
}

// s x y z = x z ( y z )
void* s_base( void** params ) {
    struct function* x = params[0];
    struct function* y = params[1];
    void* z = params[2];

    struct function* x_res = call( x, z );
    struct function* y_res = call( y, z );
    return call( x_res, y_res );
}

struct function* k() {
    struct function* f = malloc( sizeof( struct function ) );
    f->needs = 2;
    f->has = 0;
    f->params = malloc( sizeof( void* ) * 2 );
    f->function = k_base;
    return f;
}

struct function* s() {
    struct function* f = malloc( sizeof( struct function ) );
    f->needs = 3;
    f->has = 0;
    f->params = malloc( sizeof( void* ) * 3 );
    f->function = s_base;
    return f;
}

int main( int argc, char* argv[] ) {

    int v1 = 1;
    int v2 = 2;
    void* r1 = call( call( k(), &v1 ), &v2 );
    printf( "%d\n", *((int*)r1) ); // should ignore second value

    int v3 = 3;
    void* r2 = call( call( call( s(), k() ), k() ), &v3 ); // SKK = I
    printf( "%d\n", *((int*)r2) ); // should be the same value that is put in (ie 3) 

    return 0;
}
