
#include<stdlib.h>
#include<stdint.h>
#include<stdbool.h>

enum op {
    op_add_u8,
    op_add_u16,
    op_add_u32,
    op_add_s8,
    op_add_s16,
    op_add_s32,
    // TODO add other basic ops
    // TODO mov
    // TODO how should push/pop be structed?

    op_lea, // TODO i think i can get away with single lea, check
    op_jmp
};
     
typedef struct opcode {
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
} opcode_t;

typedef struct vm {
    uint8_t* stack;
    uint8_t* global;
    opcode_t* ip; 
    uint8_t* sp; 
    uint8_t* bp;
    // TODO decide if these are actually going to be useful
    uint32_t g1;
    uint32_t g2;
    uint32_t g3;

    // TODO:  throw a GC interface in here eventually
} vm_t;


// TODO:  resizing a stack sounds like a great way to invalidate 
// any pointers that point to it.  Try not to do that without
// thinking it through first.


bool vm_init( vm_t* vm ) {
    vm = (vm_t*)malloc( sizeof( vm_t ) );
    if ( vm == NULL )
        goto error;

    // TODO initialization for other VM objects

    return true;

error:
    return false;
}

