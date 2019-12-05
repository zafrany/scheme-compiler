%:
	touch output.s
	touch ./project/input.scm 
	touch ./project/output.s
	rm ./project/input.scm
	rm ./project/output.s
	rm output.s
	cat ./project/rt-support.scm $@.scm > ./project/input.scm
	cd project && scheme -q compiler.scm
	nasm -f elf64 ./project/output.s
	gcc -g -m64 ./project/output.o -o $@

.PHONY:
	clean all