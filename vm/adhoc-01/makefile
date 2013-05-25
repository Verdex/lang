
SAFE = -Wall -Wextra -ftrapv 

vm.o : vm.c
	gcc $(SAFE) -c vm.c

test :	test-vm

test-vm : vm.c
	gcc $(SAFE) vm.c -o test-vm -DTEST=true

clean : 
	rm -rf a.out *.o test-vm
