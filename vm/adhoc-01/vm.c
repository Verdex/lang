
#include<stdlib.h>
#include<stdint.h>
#include<stdbool.h>

#ifdef TEST

#include<stdio.h>

#endif


typedef enum {
    op_add_u32,
    op_add_s32,
    // TODO add other basic ops
    // TODO mov
    // TODO how should push/pop be structed?

    op_lea, // TODO i think i can get away with single lea, check
    op_jmp,

    op_exit
} opcode_t;
    
typedef enum {
    loc_stack,
    loc_glob,
    loc_ip,
    loc_sp,
    loc_bp,
    loc_ret,
    loc_accum,
    loc_gen,
    loc_null,
} loc_t;

typedef struct {
    loc_t type;
    bool deref;
    uint64_t offset;
} param_t;

typedef struct {
    opcode_t opcode;
    param_t dest;
    param_t src;

    // dest needs to be a register or a register dereferenced with offset of possibly zero
    // source can then be a register, a register dereferenced (with offset of possily zero),
    // dereference means two things, pull from address or put too address


    /*  TODO 

        structure of my executable will influence the structure of my opcode

        * is going to be necessary because there's no point to derefernce if you aren't using it
        offset
        label
        jump label
        jump *reg
        .glob, etc
        handling blah to reg vs blah to stack
        handling blah from reg vs blah from stack
    */
} instr_t;

typedef struct {
    uint8_t* stack;
    uint8_t* global;
    instr_t* code; 
    // TODO code length? .. maybe end program op code?
    instr_t* ip; 
    uint8_t* sp; 
    uint8_t* bp;
    // TODO decide if these are actually going to be useful
    uint64_t ret;
    uint64_t accum;
    uint64_t gen;

    // TODO:  throw a GC interface in here eventually
} vm_t;


// TODO:  resizing a stack sounds like a great way to invalidate 
// any pointers that point to it.  Try not to do that without
// thinking it through first.

static void* get( vm_t* vm, param_t* p ) {
    switch ( p->type ) {
        case loc_stack:
        case loc_glob:
            return vm->global; // TODO this isnt right ... stack will need similar treatment
        case loc_ip:
            return vm->ip;
        case loc_sp:
            return vm->sp;
        case loc_bp:
            return vm->bp;
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

void vm_run( vm_t* vm ) {
    // TODO make sure vm isnt null (or is that really going to be an issue? 
    // ... like, there's a lot of things that someone can do to make
    // input invalid)

    while ( vm->ip->opcode != op_exit ) {
        instr_t c = *vm->ip;
        switch ( c.opcode ) {

            case op_add_u32:
                { 
                    uint32_t* v1 = get( vm, &c.dest );
                    uint32_t* v2 = get( vm,  &c.src );
                    printf( "%d\n", *v1 + *v2 );
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

    return true;

error:
    return false;
}


#ifdef TEST 

int main() {

    vm_t vm;
    instr_t code[] = { 
        { op_add_u32, { loc_gen, false, 0 }, { loc_gen, false, 0 } }, 
        { op_add_u32, { loc_gen, false, 0 }, { loc_gen, false, 0 } }, 
        { op_exit, { loc_null, false, 0 }, { loc_null, false, 0 } } 
    };
    vm.code = code;
    vm.ip = vm.code;
    vm.gen = 1;
    vm_run( &vm );
    return 0;
}

#endif
