
#include<stdio.h> // TODO remove
// TODO remove
#define LOG( args... ) fprintf( stderr, args )

#include<stdbool.h>
#include<stdint.h>
#include<string.h>
#include<stdlib.h>

#include "vm.h"

// TODO:  resizing a stack sounds like a great way to invalidate 
// any pointers that point to it.  Try not to do that without
// thinking it through first.

static void* get_deref( vm_t* vm, param_t* p ) { 
    switch ( p->type ) {
        case loc_addr:
            return p->addr; // TODO implement, test
        case loc_sp:
            return vm->sp;  // TODO implement, test
        case loc_bp:
            return vm->bp;  // TODO implement, test
        case loc_ret:
            return &vm->ret;  // TODO implement, test
        case loc_accum:
            return &vm->accum;  // TODO implement, test
        case loc_gen:
            return &vm->gen; // TODO implement, test
        case loc_null:
            return NULL;
    }
    return NULL;
}

static void* get_direct( vm_t* vm, param_t* p ) { 
    switch ( p->type ) {
        case loc_addr:
            return &p->addr; 
        case loc_sp:
            return &vm->sp; 
        case loc_bp:
            return &vm->bp; 
        case loc_ret:
            return &vm->ret; 
        case loc_accum:
            return &vm->accum; 
        case loc_gen:
            return &vm->gen;
        case loc_null:
            return NULL;
    }
    return NULL;
}

static void* get( vm_t* vm, param_t* p ) {
    if ( p->deref ) {
        return get_deref( vm, p );
    } else {
        return get_direct( vm, p );
    }
}

static void store_deref( vm_t* vm, param_t* p, void* value, size_t size ) { 
    switch ( p->type ) {
        case loc_addr:
            // TODO implement, test
            break;
        case loc_sp:
             // TODO implement, test
            break;
        case loc_bp:
              // TODO implement, test
            break;
        case loc_ret:
              // TODO implement, test
            break;
        case loc_accum:
              // TODO implement, test
            break;
        case loc_gen:
            // TODO implement, test
            break;
        case loc_null:
            break;
    }
}

static void store_direct( vm_t* vm, param_t* p, void* value, size_t size ) { 
    switch ( p->type ) {
        case loc_addr:
            goto error;
        case loc_sp:
             if ( size != sizeof( void* ) )
                goto error;
            vm->sp = *((void**)value);
            break;
        case loc_bp:
            if ( size != sizeof( void* ) )
                goto error;
            vm->bp = *((void**)value);
            break;
        case loc_ret:
            if ( size > sizeof( uint64_t ) )
                goto error;
            memcpy( &vm->ret, value, size );
            break;
        case loc_accum:
            if ( size > sizeof( uint64_t ) )
                goto error;
            memcpy( &vm->accum, value, size );
            break;
        case loc_gen:
            if ( size > sizeof( uint64_t ) )
                goto error;
            memcpy( &vm->gen, value, size );
            break;
        case loc_null:
            break;
    }

error:
    return; 
}

static void store( vm_t* vm, param_t* p, void* value, size_t size ) {
    if ( p->deref ) {
        store_deref( vm, p, value, size );
    } else {
        store_direct( vm, p, value, size );
    }
}

void vm_run( vm_t* vm ) {
    // TODO make sure vm isnt null (or is that really going to be an issue? 
    // ... like, there's a lot of things that someone can do to make
    // input invalid)

    while ( vm->ip->opcode != op_exit ) {
        instr_t* c = vm->ip;
        switch ( c->opcode ) {

            case op_add_u32:
                { 
                    uint32_t* v1 = get( vm, &c->dest );
                    uint32_t* v2 = get( vm,  &c->src );
                    uint32_t res = *v1 + *v2;
                    store( vm, &c->dest, &res, sizeof( uint32_t ) );

                    vm->ip++;
                }                 
                break;
            case op_mov_64:
                {
                    void* s = get( vm, &c->src );
                    store( vm, &c->dest, s, 8 );
                    
                    vm->ip++;
                }
                break;
            case op_exit:
            default: 
                goto impossible;
        }

    }

impossible:
    return;
}


bool vm_init( vm_t* vm ) {
    // TODO make sure vm isnt null

    vm = malloc( sizeof( vm_t ) );
    if ( vm == NULL )
        goto error;

    // TODO initialization for other VM objects


    // TODO the assembler can only set the offsets of where
    // global objects can exist (variables, functions, etc)
    // once that file is read into the vm, something will need
    // to setup the true addresses (maybe this function)
    
    return true;

error:
    return false;
}

void vm_destroy( vm_t* vm ) {
}

void vm_destroyAndNull( vm_t** vm ) { // TODO keep?
    if ( vm == NULL ) 
        goto error;

    vm_destroy( *vm );
    *vm = NULL;

error:
    return;
}
