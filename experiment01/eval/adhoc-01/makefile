
SAFE = -Wall -Wextra -ftrapv 

vm.o : vm.c
	gcc $(SAFE) -c vm.c

test : test-vm

test-vm : vm.o vm.h test-vm.c
	gcc $(SAFE) vm.o test-vm.c -o test-vm 

clean : 
	rm -rf a.out *.o test-vm
