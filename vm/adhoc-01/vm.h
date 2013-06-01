
#ifndef _VM_H_
#define _VM_H_

#include<stdbool.h>
#include<stdint.h>

typedef enum {
    op_add_u32,
    op_add_s32,
    // TODO add other basic ops
    op_sub_u32,
    op_sub_s32,
    op_mov_32,
    op_mov_64,
    // TODO how should push/pop be structed?

    op_lea, // TODO i think i can get away with single lea, check to make sure this is true
    op_jmp,
    // TODO need callopcode

    op_exit
} opcode_t;
    
typedef enum {
    loc_addr,
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
    void* addr;
} param_t;

typedef struct {
    opcode_t opcode;
    param_t dest;
    param_t src;
} instr_t;

typedef struct {
    uint8_t* stack;
    uint8_t* global;
    instr_t* code; 
    instr_t* ip; 
    void* sp; 
    void* bp;
    uint64_t ret;
    uint64_t accum;
    uint64_t gen;

    // TODO:  throw a GC interface in here eventually
} vm_t;


void vm_run( vm_t* vm );

bool vm_init( vm_t* vm );
void vm_destroy( vm_t* vm ); 
void vm_destroyAndNull( vm_t** vm );




#endif
