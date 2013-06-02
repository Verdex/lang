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

static void bpStoreWorks() { 
    vm_t vm;
    instr_t code[] = { 
        { op_mov_64, { loc_bp, false, 0, NULL }, { loc_gen, false, 0, NULL } }, 
        { op_exit, { loc_null, false, 0, NULL }, { loc_null, false, 0, NULL } } 
    };
    vm.code = code;
    vm.ip = vm.code;

    int i = 0;
    vm.gen = (uint64_t)&i;
    vm_run( &vm );
    assert( vm.bp == &i );
}

static void spStoreWorks() { 
    vm_t vm;
    instr_t code[] = { 
        { op_mov_64, { loc_sp, false, 0, NULL }, { loc_gen, false, 0, NULL } }, 
        { op_exit, { loc_null, false, 0, NULL }, { loc_null, false, 0, NULL } } 
    };
    vm.code = code;
    vm.ip = vm.code;

    int i = 0;
    vm.gen = (uint64_t)&i;
    vm_run( &vm );
    assert( vm.sp == &i );
}

static void spGetWorks() { 
    vm_t vm;
    instr_t code[] = { 
        { op_mov_64, { loc_bp, false, 0, NULL }, { loc_sp, false, 0, NULL } }, 
        { op_exit, { loc_null, false, 0, NULL }, { loc_null, false, 0, NULL } } 
    };
    vm.code = code;
    vm.ip = vm.code;

    int i = 0;
    vm.sp = &i;
    vm_run( &vm );
    assert( vm.bp == &i );
}

static void bpGetWorks() {
    vm_t vm;
    instr_t code[] = { 
        { op_mov_64, { loc_sp, false, 0, NULL }, { loc_bp, false, 0, NULL } }, 
        { op_exit, { loc_null, false, 0, NULL }, { loc_null, false, 0, NULL } } 
    };
    vm.code = code;
    vm.ip = vm.code;

    int i = 0;
    vm.bp = &i;
    vm_run( &vm );
    assert( vm.sp == &i );
}

static void addrGetWorks() {
    int i = 0;

    vm_t vm;
    instr_t code[] = { 
        { op_mov_64, { loc_gen, false, 0, NULL }, { loc_addr, false, 0, &i } }, 
        { op_exit, { loc_null, false, 0, NULL }, { loc_null, false, 0, NULL } } 
    };
    vm.code = code;
    vm.ip = vm.code;

    vm_run( &vm );
    assert( (void*)vm.gen == &i );
}

static void genGetDerefWorks() {
    vm_t vm;
    instr_t code[] = { 
        { op_mov_64, { loc_accum, false, 0, NULL }, { loc_gen, true, 0, NULL } }, 
        { op_mov_64, { loc_ret, false, 0, NULL }, { loc_gen, true, 1, NULL } }, 
        { op_exit, { loc_null, false, 0, NULL }, { loc_null, false, 0, NULL } } 
    };
    vm.code = code;
    vm.ip = vm.code;

    int i[2] = { 2, 1 };
    vm.gen = (uint64_t)&i;
    vm_run( &vm );
    printf( "%u\n", vm.accum );
    assert( vm.accum == 2 );
    assert( vm.ret == 1 );
}
// TODO test that bp, and sp addresses moved into them etc ... 
// basically make sure function call pre/post operations work

int main() {
    
    genGetAndStoreWorks();
    accumGetAndStoreWorks();
    retGetAndStoreWorks();
    bpStoreWorks();
    spStoreWorks();
    spGetWorks();
    bpGetWorks();
    addrGetWorks();
    genGetDerefWorks();

    return 0;
}

