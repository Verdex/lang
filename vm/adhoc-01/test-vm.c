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

int main() {
    
    genGetAndStoreWorks();
    accumGetAndStoreWorks();

    return 0;
}

