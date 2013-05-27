#include<stdint.h>
#include<stdbool.h>

#include<stdio.h>
#include<assert.h>

#include "vm.h"


static void genGetAndStoreWorks() {
    vm_t vm;
    instr_t code[] = { 
        { op_add_u32, { loc_gen, false, 0, NULL }, { loc_gen, false, 0, NULL } }, 
        { op_exit, { loc_null, false, 0, NULL }, { loc_null, false, 0, NULL } } 
    };
    vm.code = code;
    vm.ip = vm.code;
    vm.gen = 1;
    vm_run( &vm );
    assert( vm.gen == 2 );
}

static void accumGetAndStoreWorks() {
    vm_t vm;
    instr_t code[] = { 
        { op_add_u32, { loc_accum, false, 0, NULL }, { loc_accum, false, 0, NULL } }, 
        { op_exit, { loc_null, false, 0, NULL }, { loc_null, false, 0, NULL } } 
    };
    vm.code = code;
    vm.ip = vm.code;
    vm.accum = 2;
    vm_run( &vm );
    assert( vm.accum == 4 );
}

static void retGetAndStoreWorks() {
    vm_t vm;
    instr_t code[] = { 
        { op_add_u32, { loc_ret, false, 0, NULL }, { loc_ret, false, 0, NULL } }, 
        { op_exit, { loc_null, false, 0, NULL }, { loc_null, false, 0, NULL } } 
    };
    vm.code = code;
    vm.ip = vm.code;
    vm.ret = 3;
    vm_run( &vm );
    assert( vm.ret == 6 );
}

// Note:  bp probably shouldn't be used for adding, but it will need to 
// have an address directly fetched and stored, so I want to make sure this
// sort of thing will work.
static void bpGetAndStoreWorks() { 
    vm_t vm;
    instr_t code[] = { 
        { op_add_u32, { loc_ret, false, 0, NULL }, { loc_ret, false, 0, NULL } }, 
        { op_exit, { loc_null, false, 0, NULL }, { loc_null, false, 0, NULL } } 
    };
    vm.code = code;
    vm.ip = vm.code;
    vm.ret = 3;
    vm_run( &vm );
    assert( vm.ret == 6 );
}

// TODO test that bp, and sp addresses moved into them etc ... 
// basically make sure function call pre/post operations work

int main() {
    
    genGetAndStoreWorks();
    accumGetAndStoreWorks();
    retGetAndStoreWorks();

    return 0;
}

