(define start-string
"%define T_UNDEFINED 0
%define T_VOID 1
%define T_NIL 2
%define T_INTEGER 3
%define T_FRACTION 4
%define T_BOOL 5
%define T_CHAR 6
%define T_STRING 7
%define T_SYMBOL 8
%define T_CLOSURE 9
%define T_PAIR 10
%define T_VECTOR 11

%define CHAR_NUL 0
%define CHAR_TAB 9
%define CHAR_NEWLINE 10
%define CHAR_PAGE 12
%define CHAR_RETURN 13
%define CHAR_SPACE 32

%define TYPE_BITS 4
%define WORD_SIZE 64

%define An(n) qword [rbp + 8 * (n + 4)]

%macro iabs 1
	cmp %1, 0
	jge %%cont
	neg %1
	%%cont:
%endmacro

%define MAKE_LITERAL(type, lit) ((lit << TYPE_BITS) | type)

%macro TYPE 1
	and %1, ((1 << TYPE_BITS) - 1) 
%endmacro

%macro DATA 1
	shr %1, TYPE_BITS
%endmacro

%macro DATA_UPPER 1
	shr %1, (((WORD_SIZE - TYPE_BITS) >> 1) + TYPE_BITS)
%endmacro

%macro DATA_LOWER 1
	shl %1, ((WORD_SIZE - TYPE_BITS) >> 1)
	DATA_UPPER %1
%endmacro

%macro DATA_UPPER_PRINT 1
	sar %1, (((WORD_SIZE - TYPE_BITS) >> 1) + TYPE_BITS)
%endmacro

%macro DATA_LOWER_PRINT 1
	sal %1, ((WORD_SIZE - TYPE_BITS) >> 1)
	DATA_UPPER %1
%endmacro

%define MAKE_LITERAL_PAIR(car, cdr) (((((car - start_of_data) << ((WORD_SIZE - TYPE_BITS) >> 1)) | (cdr - start_of_data)) << TYPE_BITS) | T_PAIR)
%define MAKE_LITERAL_FRACTION(car, cdr) (((((car - start_of_data) << ((WORD_SIZE - TYPE_BITS) >> 1)) | (cdr - start_of_data)) << TYPE_BITS) | T_FRACTION)

%macro CAR_PRINT 1
	DATA_UPPER %1
	add %1, start_of_data
	mov %1, qword [%1]
%endmacro

%macro CDR_PRINT 1
	DATA_LOWER %1
	add %1, start_of_data
	mov %1, qword [%1]
%endmacro

%macro CAR 1
	DATA_UPPER %1
	add %1, start_of_data
	mov %1, qword [%1]
%endmacro

%macro CDR 1
	DATA_LOWER %1
	add %1, start_of_data
	mov %1, qword [%1]
%endmacro

%macro GET_CAR 1
	mov %1, qword [%1]
	DATA_UPPER %1
	add %1, start_of_data
%endmacro

%macro GET_CDR 1
	mov %1, qword [%1]
	DATA_LOWER %1
	add %1, start_of_data
%endmacro

;;; MAKE_LITERAL_CLOSURE target, env, code
%macro MAKE_LITERAL_CLOSURE 3
	push rax
	push rbx
	mov rax, %1
	sub %2, start_of_data
	mov qword [rax], %2
	shl qword [rax], ((WORD_SIZE - TYPE_BITS) >> 1)
	;lea rbx, [rax + 8 - start_of_data]
	lea rbx, [rax + 8]
	sub rbx, start_of_data
	or qword [rax], rbx
	shl qword [rax], TYPE_BITS
	or qword [rax], T_CLOSURE
	mov qword [rax + 8], %3
	pop rbx
	pop rax
%endmacro

%macro CLOSURE_ENV 1
	DATA_UPPER %1
	add %1, start_of_data
%endmacro

%macro CLOSURE_CODE 1
	DATA_LOWER %1
	add %1, start_of_data
	mov %1, qword [%1]
%endmacro

%macro MAKE_LITERAL_STRING 1+
	dq (((((%%LstrEnd - %%Lstr) << ((WORD_SIZE - TYPE_BITS) >> 1)) | (%%Lstr - start_of_data)) << TYPE_BITS) | T_STRING)
	%%Lstr:
	db %1
	%%LstrEnd:
%endmacro

;; MAKE_LITERAL_SYMBOL sobString label, dest (dest CANNOT be rax)
%macro MAKE_LITERAL_SYMBOL 2
	push qword rax
	mov qword rax, %1
	mov qword %2, rax
	shl %2, 4
	or %2, T_SYMBOL
	pop rax
%endmacro

%macro STRING_LENGTH 1
	DATA_UPPER %1
%endmacro

%macro STRING_ELEMENTS 1
	DATA_LOWER %1
	add %1, start_of_data
%endmacro

;;; STRING_REF dest, src, index
;;; dest cannot be RAX! (fix this!)
%macro STRING_REF 3
	push rax
	mov rax, %2
	STRING_ELEMENTS rax
	add rax, %3
	mov %1, byte [rax]
	pop rax
%endmacro

%macro MAKE_LITERAL_VECTOR 1+
	dq ((((((%%VecEnd - %%Vec) >> 3) << ((WORD_SIZE - TYPE_BITS) >> 1)) | (%%Vec - start_of_data)) << TYPE_BITS) | T_VECTOR)
	%%Vec:
	dq %1
	%%VecEnd:
%endmacro

%macro VECTOR_LENGTH 1
	DATA_UPPER %1
%endmacro

%macro VECTOR_ELEMENTS 1
	DATA_LOWER %1
	add %1, start_of_data
%endmacro

;;; VECTOR_REF dest, src, index
;;; dest cannot be RAX! (fix this!)
%macro VECTOR_REF 3
	mov %1, %2
	VECTOR_ELEMENTS %1
	lea %1, [%1 + %3*8]
	mov %1, qword [%1]
	mov %1, qword [%1]
%endmacro

%macro my_malloc 1
	push rbx
	mov rbx, malloc_pointer
	mov rax, qword [rbx]
	add qword [rbx], %1
	pop rbx
%endmacro

%macro sym_tab_append 1
	push rax
	push rbx
	mov rbx, symbol_table_pointer ;rbx have *symbol_table NOT [symbol_table]
	mov rbx, qword [rbx]
	mov qword [rbx], %1

	my_malloc 16
	mov qword [rax], 0
	mov qword [rbx + 8], rax
	mov qword [symbol_table_pointer], rax
	pop rbx
	pop rax
%endmacro

%define SOB_UNDEFINED MAKE_LITERAL(T_UNDEFINED, 0)
%define SOB_VOID MAKE_LITERAL(T_VOID, 0)
%define SOB_FALSE MAKE_LITERAL(T_BOOL, 0)
%define SOB_TRUE MAKE_LITERAL(T_BOOL, 1)
%define SOB_NIL MAKE_LITERAL(T_NIL, 0)


section .data
start_of_data:\n"
)

(define pre-code-string
"section .bss

extern exit, printf, scanf, malloc
global main, write_sob, write_sob_if_not_void
malloc_pointer:
resq 1
start_of_data_malloc:
resb 1073741824

symbol_table_pointer:
resq 1
symbol_table:
resb 16

section .text
main:
mov rax, malloc_pointer
mov qword [rax], start_of_data_malloc

mov rax, symbol_table
mov qword [symbol_table_pointer], rax\n\n"
)

(define end-string
"write_sob_undefined:
	push rbp
	mov rbp, rsp

	mov rax, 0
	mov rdi, .undefined
	call printf

	leave
	ret

section .data
.undefined:
	db \"#<undefined>\", 0

write_sob_integer:
	push rbp
	mov rbp, rsp

	mov rsi, qword [rbp + 8 + 1*8]
	sar rsi, TYPE_BITS
	mov rdi, .int_format_string
	mov rax, 0
	call printf

	leave
	ret

section .data
.int_format_string:
	db \"%ld\", 0

write_sob_char:
	push rbp
	mov rbp, rsp

	mov rsi, qword [rbp + 8 + 1*8]
	DATA rsi

	cmp rsi, CHAR_NUL
	je .Lnul

	cmp rsi, CHAR_TAB
	je .Ltab

	cmp rsi, CHAR_NEWLINE
	je .Lnewline

	cmp rsi, CHAR_PAGE
	je .Lpage

	cmp rsi, CHAR_RETURN
	je .Lreturn

	cmp rsi, CHAR_SPACE
	je .Lspace
	jg .Lregular

	mov rdi, .special
	jmp .done	

.Lnul:
	mov rdi, .nul
	jmp .done

.Ltab:
	mov rdi, .tab
	jmp .done

.Lnewline:
	mov rdi, .newline
	jmp .done

.Lpage:
	mov rdi, .page
	jmp .done

.Lreturn:
	mov rdi, .return
	jmp .done

.Lspace:
	mov rdi, .space
	jmp .done

.Lregular:
	mov rdi, .regular
	jmp .done

.done:
	mov rax, 0
	call printf

	leave
	ret

section .data
.space:
	db \"#\\space\", 0
.newline:
	db \"#\\newline\", 0
.return:
	db \"#\\return\", 0
.tab:
	db \"#\\tab\", 0
.page:
	db \"#\\page\", 0
.nul:
	db \"#\\nul\", 0
.special:
	db \"#\\x%02x\", 0
.regular:
	db \"#\\%c\", 0

write_sob_void:
	push rbp
	mov rbp, rsp

	mov rax, 0
	mov rdi, .void
	call printf

	leave
	ret

section .data
.void:
	db \"#<void>\", 0
	
write_sob_bool:
	push rbp
	mov rbp, rsp

	mov rax, qword [rbp + 8 + 1*8]
	cmp rax, SOB_FALSE
	je .sobFalse
	
	mov rdi, .true
	jmp .continue

.sobFalse:
	mov rdi, .false

.continue:
	mov rax, 0
	call printf	

	leave
	ret

section .data			
.false:
	db \"#f\", 0
.true:
	db \"#t\", 0

write_sob_nil:
	push rbp
	mov rbp, rsp

	mov rax, 0
	mov rdi, .nil
	call printf

	leave
	ret

section .data
.nil:
	db \"()\", 0

write_sob_string:
	push rbp
	mov rbp, rsp

	mov rax, 0
	mov rdi, .double_quote
	call printf

	mov rax, qword [rbp + 8 + 1*8]
	mov rcx, rax
	STRING_LENGTH rcx
	STRING_ELEMENTS rax

.loop:
	cmp rcx, 0
	je .done
	mov bl, byte [rax]
	and rbx, 0xff

	cmp rbx, CHAR_TAB
	je .ch_tab
	cmp rbx, CHAR_NEWLINE
	je .ch_newline
	cmp rbx, CHAR_PAGE
	je .ch_page
	cmp rbx, CHAR_RETURN
	je .ch_return
	cmp rbx, CHAR_SPACE
	jl .ch_hex
	
	mov rdi, .fs_simple_char
	mov rsi, rbx
	jmp .printf
	
.ch_hex:
	mov rdi, .fs_hex_char
	mov rsi, rbx
	jmp .printf
	
.ch_tab:
	mov rdi, .fs_tab
	mov rsi, rbx
	jmp .printf
	
.ch_page:
	mov rdi, .fs_page
	mov rsi, rbx
	jmp .printf
	
.ch_return:
	mov rdi, .fs_return
	mov rsi, rbx
	jmp .printf

.ch_newline:
	mov rdi, .fs_newline
	mov rsi, rbx

.printf:
	push rax
	push rcx
	mov rax, 0
	call printf
	pop rcx
	pop rax

	dec rcx
	inc rax
	jmp .loop

.done:
	mov rax, 0
	mov rdi, .double_quote
	call printf

	leave
	ret
section .data
.double_quote:
	db '\"', 0
.fs_simple_char:
	db \"%c\", 0
.fs_hex_char:
	db \"\\x%02x;\", 0	
.fs_tab:
	db \"\\t\", 0
.fs_page:
	db \"\\f\", 0
.fs_return:
	db \"\\r\", 0
.fs_newline:
	db \"\\n\", 0

write_sob_pair:
	push rbp
	mov rbp, rsp

	mov rax, 0
	mov rdi, .open_paren
	call printf
	mov rax, qword [rbp + 8 + 1*8]
	CAR_PRINT rax
	push rax
	call write_sob
	add rsp, 1*8
	mov rax, qword [rbp + 8 + 1*8]
	CDR_PRINT rax
	push rax
	call write_sob_pair_on_cdr
	add rsp, 1*8
	mov rdi, .close_paren
	mov rax, 0
	call printf

	leave
	ret

section .data
.open_paren:
	db \"(\", 0
.close_paren:
	db \")\", 0

write_sob_pair_on_cdr:
	push rbp
	mov rbp, rsp

	mov rbx, qword [rbp + 8 + 1*8]
	mov rax, rbx
	TYPE rbx
	cmp rbx, T_NIL
	je .done
	cmp rbx, T_PAIR
	je .cdrIsPair
	push rax
	mov rax, 0
	mov rdi, .dot
	call printf
	call write_sob
	add rsp, 1*8
	jmp .done

.cdrIsPair:
	mov rbx, rax
	CDR_PRINT rbx
	push rbx
	CAR_PRINT rax
	push rax
	mov rax, 0
	mov rdi, .space
	call printf
	call write_sob
	add rsp, 1*8
	call write_sob_pair_on_cdr
	add rsp, 1*8

.done:
	leave
	ret

section .data
.space:
	db \" \", 0
.dot:
	db \" . \", 0

write_sob_vector:
	push rbp
	mov rbp, rsp

	mov rax, 0
	mov rdi, .fs_open_vector
	call printf

	mov rax, qword [rbp + 8 + 1*8]
	mov rcx, rax
	VECTOR_LENGTH rcx
	cmp rcx, 0
	je .done
	VECTOR_ELEMENTS rax

	push rcx
	push rax
	mov rax, qword [rax]
	push qword [rax]
	call write_sob
	add rsp, 1*8
	pop rax
	pop rcx
	dec rcx
	add rax, 8

.loop:
	cmp rcx, 0
	je .done

	push rcx
	push rax
	mov rax, 0
	mov rdi, .fs_space
	call printf
	
	pop rax
	push rax
	mov rax, qword [rax]
	push qword [rax]
	call write_sob
	add rsp, 1*8
	pop rax
	pop rcx
	dec rcx
	add rax, 8
	jmp .loop

.done:
	mov rax, 0
	mov rdi, .fs_close_vector
	call printf

	leave
	ret

section	.data
.fs_open_vector:
	db \"#(\", 0
.fs_close_vector:
	db \")\", 0
.fs_space:
	db \" \", 0

write_sob_symbol:
	push rbp
	mov rbp, rsp

	mov rax, qword [rbp + 8 + 1*8]
	shr rax, 4
	mov rax, qword [rax]
	mov rcx, qword rax
	STRING_LENGTH rcx
	STRING_ELEMENTS rax

.loop:
	cmp rcx, 0
	je .done
	mov bl, byte [rax]
	and rbx, 0xff

	cmp rbx, CHAR_TAB
	je .ch_tab
	cmp rbx, CHAR_NEWLINE
	je .ch_newline
	cmp rbx, CHAR_PAGE
	je .ch_page
	cmp rbx, CHAR_RETURN
	je .ch_return
	cmp rbx, CHAR_SPACE
	jl .ch_hex
	
	mov rdi, .fs_simple_char
	mov rsi, rbx
	jmp .printf
	
.ch_hex:
	mov rdi, .fs_hex_char
	mov rsi, rbx
	jmp .printf
	
.ch_tab:
	mov rdi, .fs_tab
	mov rsi, rbx
	jmp .printf
	
.ch_page:
	mov rdi, .fs_page
	mov rsi, rbx
	jmp .printf
	
.ch_return:
	mov rdi, .fs_return
	mov rsi, rbx
	jmp .printf

.ch_newline:
	mov rdi, .fs_newline
	mov rsi, rbx

.printf:
	push rax
	push rcx
	mov rax, 0
	call printf
	pop rcx
	pop rax

	dec rcx
	inc rax
	jmp .loop

.done:
	leave
	ret
section .data
.double_quote:
	db '\"', 0
.fs_simple_char:
	db \"%c\", 0
.fs_hex_char:
	db \"\\x%02x;\", 0	
.fs_tab:
	db \"\\t\", 0
.fs_page:
	db \"\\f\", 0
.fs_return:
	db \"\\r\", 0
.fs_newline:
	db \"\\n\", 0
	
write_sob_fraction:
	push rbp
	mov rbp, rsp


	mov rax, qword [rbp + 8 + 1*8]
	CAR_PRINT rax
	push rax
	call write_sob
	add rsp, 1*8

	mov rax, 0
	mov rdi, .fraction_sign
	call printf

	mov rax, qword [rbp + 8 + 1*8]
	CDR_PRINT rax
	push rax
	call write_sob
	add rsp, 1*8
	

	leave
	ret

section .data
.fraction_sign:
	db \"/\", 0
write_sob_closure:
	push rbp
	mov rbp, rsp

	mov rsi, qword [rbp + 8 + 1*8]
	mov rdx, rsi
	CLOSURE_ENV rsi
	CLOSURE_CODE rdx
	mov rdi, .closure
	mov rax, 0
	call printf

	leave
	ret
section .data
.closure:
	db \"#<closure [env:%p, code:%p]>\", 0

write_sob:
	mov rax, qword [rsp + 1*8]
	TYPE rax
	jmp qword [.jmp_table + rax * 8]

section .data
.jmp_table:
	dq write_sob_undefined, write_sob_void, write_sob_nil
	dq write_sob_integer, write_sob_fraction, write_sob_bool
	dq write_sob_char, write_sob_string, write_sob_symbol
	dq write_sob_closure, write_sob_pair, write_sob_vector

section .text
write_sob_if_not_void:
	mov rax, qword [rsp + 1*8]
	cmp rax, SOB_VOID
	je .continue

	push rax
	call write_sob
	add rsp, 1*8
	mov rax, 0
	mov rdi, .newline
	call printf
	
.continue:
	ret
section .data
.newline:
	db CHAR_NEWLINE, 0"
)