
samples/segfault:     file format elf64-x86-64


Disassembly of section .init:

0000000000401000 <_init>:
  401000:	f3 0f 1e fa          	endbr64 
  401004:	48 83 ec 08          	sub    $0x8,%rsp
  401008:	48 8b 05 e9 2f 00 00 	mov    0x2fe9(%rip),%rax        # 403ff8 <__gmon_start__>
  40100f:	48 85 c0             	test   %rax,%rax
  401012:	74 02                	je     401016 <_init+0x16>
  401014:	ff d0                	callq  *%rax
  401016:	48 83 c4 08          	add    $0x8,%rsp
  40101a:	c3                   	retq   

Disassembly of section .plt:

0000000000401020 <.plt>:
  401020:	ff 35 e2 2f 00 00    	pushq  0x2fe2(%rip)        # 404008 <_GLOBAL_OFFSET_TABLE_+0x8>
  401026:	f2 ff 25 e3 2f 00 00 	bnd jmpq *0x2fe3(%rip)        # 404010 <_GLOBAL_OFFSET_TABLE_+0x10>
  40102d:	0f 1f 00             	nopl   (%rax)
  401030:	f3 0f 1e fa          	endbr64 
  401034:	68 00 00 00 00       	pushq  $0x0
  401039:	f2 e9 e1 ff ff ff    	bnd jmpq 401020 <.plt>
  40103f:	90                   	nop
  401040:	f3 0f 1e fa          	endbr64 
  401044:	68 01 00 00 00       	pushq  $0x1
  401049:	f2 e9 d1 ff ff ff    	bnd jmpq 401020 <.plt>
  40104f:	90                   	nop

Disassembly of section .plt.sec:

0000000000401050 <puts@plt>:
  401050:	f3 0f 1e fa          	endbr64 
  401054:	f2 ff 25 bd 2f 00 00 	bnd jmpq *0x2fbd(%rip)        # 404018 <puts@GLIBC_2.2.5>
  40105b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000401060 <printf@plt>:
  401060:	f3 0f 1e fa          	endbr64 
  401064:	f2 ff 25 b5 2f 00 00 	bnd jmpq *0x2fb5(%rip)        # 404020 <printf@GLIBC_2.2.5>
  40106b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

Disassembly of section .text:

0000000000401070 <_start>:
  401070:	f3 0f 1e fa          	endbr64 
  401074:	31 ed                	xor    %ebp,%ebp
  401076:	49 89 d1             	mov    %rdx,%r9
  401079:	5e                   	pop    %rsi
  40107a:	48 89 e2             	mov    %rsp,%rdx
  40107d:	48 83 e4 f0          	and    $0xfffffffffffffff0,%rsp
  401081:	50                   	push   %rax
  401082:	54                   	push   %rsp
  401083:	49 c7 c0 c0 12 40 00 	mov    $0x4012c0,%r8
  40108a:	48 c7 c1 50 12 40 00 	mov    $0x401250,%rcx
  401091:	48 c7 c7 0a 12 40 00 	mov    $0x40120a,%rdi
  401098:	ff 15 52 2f 00 00    	callq  *0x2f52(%rip)        # 403ff0 <__libc_start_main@GLIBC_2.2.5>
  40109e:	f4                   	hlt    
  40109f:	90                   	nop

00000000004010a0 <_dl_relocate_static_pie>:
  4010a0:	f3 0f 1e fa          	endbr64 
  4010a4:	c3                   	retq   
  4010a5:	66 2e 0f 1f 84 00 00 	nopw   %cs:0x0(%rax,%rax,1)
  4010ac:	00 00 00 
  4010af:	90                   	nop

00000000004010b0 <deregister_tm_clones>:
  4010b0:	b8 38 40 40 00       	mov    $0x404038,%eax
  4010b5:	48 3d 38 40 40 00    	cmp    $0x404038,%rax
  4010bb:	74 13                	je     4010d0 <deregister_tm_clones+0x20>
  4010bd:	b8 00 00 00 00       	mov    $0x0,%eax
  4010c2:	48 85 c0             	test   %rax,%rax
  4010c5:	74 09                	je     4010d0 <deregister_tm_clones+0x20>
  4010c7:	bf 38 40 40 00       	mov    $0x404038,%edi
  4010cc:	ff e0                	jmpq   *%rax
  4010ce:	66 90                	xchg   %ax,%ax
  4010d0:	c3                   	retq   
  4010d1:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  4010d8:	00 00 00 00 
  4010dc:	0f 1f 40 00          	nopl   0x0(%rax)

00000000004010e0 <register_tm_clones>:
  4010e0:	be 38 40 40 00       	mov    $0x404038,%esi
  4010e5:	48 81 ee 38 40 40 00 	sub    $0x404038,%rsi
  4010ec:	48 89 f0             	mov    %rsi,%rax
  4010ef:	48 c1 ee 3f          	shr    $0x3f,%rsi
  4010f3:	48 c1 f8 03          	sar    $0x3,%rax
  4010f7:	48 01 c6             	add    %rax,%rsi
  4010fa:	48 d1 fe             	sar    %rsi
  4010fd:	74 11                	je     401110 <register_tm_clones+0x30>
  4010ff:	b8 00 00 00 00       	mov    $0x0,%eax
  401104:	48 85 c0             	test   %rax,%rax
  401107:	74 07                	je     401110 <register_tm_clones+0x30>
  401109:	bf 38 40 40 00       	mov    $0x404038,%edi
  40110e:	ff e0                	jmpq   *%rax
  401110:	c3                   	retq   
  401111:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401118:	00 00 00 00 
  40111c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000401120 <__do_global_dtors_aux>:
  401120:	f3 0f 1e fa          	endbr64 
  401124:	80 3d 0d 2f 00 00 00 	cmpb   $0x0,0x2f0d(%rip)        # 404038 <__TMC_END__>
  40112b:	75 13                	jne    401140 <__do_global_dtors_aux+0x20>
  40112d:	55                   	push   %rbp
  40112e:	48 89 e5             	mov    %rsp,%rbp
  401131:	e8 7a ff ff ff       	callq  4010b0 <deregister_tm_clones>
  401136:	c6 05 fb 2e 00 00 01 	movb   $0x1,0x2efb(%rip)        # 404038 <__TMC_END__>
  40113d:	5d                   	pop    %rbp
  40113e:	c3                   	retq   
  40113f:	90                   	nop
  401140:	c3                   	retq   
  401141:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401148:	00 00 00 00 
  40114c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000401150 <frame_dummy>:
  401150:	f3 0f 1e fa          	endbr64 
  401154:	eb 8a                	jmp    4010e0 <register_tm_clones>

0000000000401156 <func2>:
#include <stdio.h>

void func2(int a)
{
  401156:	f3 0f 1e fa          	endbr64 
  40115a:	55                   	push   %rbp
  40115b:	48 89 e5             	mov    %rsp,%rbp
  40115e:	48 83 ec 10          	sub    $0x10,%rsp
  401162:	89 7d fc             	mov    %edi,-0x4(%rbp)
    printf("About to segfault... a=%d\n", a);
  401165:	8b 45 fc             	mov    -0x4(%rbp),%eax
  401168:	89 c6                	mov    %eax,%esi
  40116a:	48 8d 3d 93 0e 00 00 	lea    0xe93(%rip),%rdi        # 402004 <_IO_stdin_used+0x4>
  401171:	b8 00 00 00 00       	mov    $0x0,%eax
  401176:	e8 e5 fe ff ff       	callq  401060 <printf@plt>
    *(int *)0 = a;
  40117b:	ba 00 00 00 00       	mov    $0x0,%edx
  401180:	8b 45 fc             	mov    -0x4(%rbp),%eax
  401183:	89 02                	mov    %eax,(%rdx)
    printf("Did segfault!\n");
  401185:	48 8d 3d 93 0e 00 00 	lea    0xe93(%rip),%rdi        # 40201f <_IO_stdin_used+0x1f>
  40118c:	e8 bf fe ff ff       	callq  401050 <puts@plt>
}
  401191:	90                   	nop
  401192:	c9                   	leaveq 
  401193:	c3                   	retq   

0000000000401194 <func1>:

void func1(int a)
{
  401194:	f3 0f 1e fa          	endbr64 
  401198:	55                   	push   %rbp
  401199:	48 89 e5             	mov    %rsp,%rbp
  40119c:	48 83 ec 10          	sub    $0x10,%rsp
  4011a0:	89 7d fc             	mov    %edi,-0x4(%rbp)
    printf("Calling func2\n");
  4011a3:	48 8d 3d 83 0e 00 00 	lea    0xe83(%rip),%rdi        # 40202d <_IO_stdin_used+0x2d>
  4011aa:	e8 a1 fe ff ff       	callq  401050 <puts@plt>
    func2(a % 5);
  4011af:	8b 4d fc             	mov    -0x4(%rbp),%ecx
  4011b2:	48 63 c1             	movslq %ecx,%rax
  4011b5:	48 69 c0 67 66 66 66 	imul   $0x66666667,%rax,%rax
  4011bc:	48 c1 e8 20          	shr    $0x20,%rax
  4011c0:	89 c2                	mov    %eax,%edx
  4011c2:	d1 fa                	sar    %edx
  4011c4:	89 c8                	mov    %ecx,%eax
  4011c6:	c1 f8 1f             	sar    $0x1f,%eax
  4011c9:	29 c2                	sub    %eax,%edx
  4011cb:	89 d0                	mov    %edx,%eax
  4011cd:	89 c2                	mov    %eax,%edx
  4011cf:	c1 e2 02             	shl    $0x2,%edx
  4011d2:	01 c2                	add    %eax,%edx
  4011d4:	89 c8                	mov    %ecx,%eax
  4011d6:	29 d0                	sub    %edx,%eax
  4011d8:	89 c7                	mov    %eax,%edi
  4011da:	e8 77 ff ff ff       	callq  401156 <func2>
}
  4011df:	90                   	nop
  4011e0:	c9                   	leaveq 
  4011e1:	c3                   	retq   

00000000004011e2 <func3>:
void func3(int a)
{
  4011e2:	f3 0f 1e fa          	endbr64 
  4011e6:	55                   	push   %rbp
  4011e7:	48 89 e5             	mov    %rsp,%rbp
  4011ea:	48 83 ec 10          	sub    $0x10,%rsp
  4011ee:	89 7d fc             	mov    %edi,-0x4(%rbp)
    printf("Calling func2 %d\n", a);
  4011f1:	8b 45 fc             	mov    -0x4(%rbp),%eax
  4011f4:	89 c6                	mov    %eax,%esi
  4011f6:	48 8d 3d 3e 0e 00 00 	lea    0xe3e(%rip),%rdi        # 40203b <_IO_stdin_used+0x3b>
  4011fd:	b8 00 00 00 00       	mov    $0x0,%eax
  401202:	e8 59 fe ff ff       	callq  401060 <printf@plt>
}
  401207:	90                   	nop
  401208:	c9                   	leaveq 
  401209:	c3                   	retq   

000000000040120a <main>:

int main()
{
  40120a:	f3 0f 1e fa          	endbr64 
  40120e:	55                   	push   %rbp
  40120f:	48 89 e5             	mov    %rsp,%rbp
  401212:	48 83 ec 10          	sub    $0x10,%rsp
    for (int i = 0; i < 10; i++)
  401216:	c7 45 fc 00 00 00 00 	movl   $0x0,-0x4(%rbp)
  40121d:	eb 0e                	jmp    40122d <main+0x23>
    {
        func3(i);
  40121f:	8b 45 fc             	mov    -0x4(%rbp),%eax
  401222:	89 c7                	mov    %eax,%edi
  401224:	e8 b9 ff ff ff       	callq  4011e2 <func3>
    for (int i = 0; i < 10; i++)
  401229:	83 45 fc 01          	addl   $0x1,-0x4(%rbp)
  40122d:	83 7d fc 09          	cmpl   $0x9,-0x4(%rbp)
  401231:	7e ec                	jle    40121f <main+0x15>
    }
    func1(42);
  401233:	bf 2a 00 00 00       	mov    $0x2a,%edi
  401238:	e8 57 ff ff ff       	callq  401194 <func1>
  40123d:	b8 00 00 00 00       	mov    $0x0,%eax
}
  401242:	c9                   	leaveq 
  401243:	c3                   	retq   
  401244:	66 2e 0f 1f 84 00 00 	nopw   %cs:0x0(%rax,%rax,1)
  40124b:	00 00 00 
  40124e:	66 90                	xchg   %ax,%ax

0000000000401250 <__libc_csu_init>:
  401250:	f3 0f 1e fa          	endbr64 
  401254:	41 57                	push   %r15
  401256:	4c 8d 3d b3 2b 00 00 	lea    0x2bb3(%rip),%r15        # 403e10 <__frame_dummy_init_array_entry>
  40125d:	41 56                	push   %r14
  40125f:	49 89 d6             	mov    %rdx,%r14
  401262:	41 55                	push   %r13
  401264:	49 89 f5             	mov    %rsi,%r13
  401267:	41 54                	push   %r12
  401269:	41 89 fc             	mov    %edi,%r12d
  40126c:	55                   	push   %rbp
  40126d:	48 8d 2d a4 2b 00 00 	lea    0x2ba4(%rip),%rbp        # 403e18 <__do_global_dtors_aux_fini_array_entry>
  401274:	53                   	push   %rbx
  401275:	4c 29 fd             	sub    %r15,%rbp
  401278:	48 83 ec 08          	sub    $0x8,%rsp
  40127c:	e8 7f fd ff ff       	callq  401000 <_init>
  401281:	48 c1 fd 03          	sar    $0x3,%rbp
  401285:	74 1f                	je     4012a6 <__libc_csu_init+0x56>
  401287:	31 db                	xor    %ebx,%ebx
  401289:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)
  401290:	4c 89 f2             	mov    %r14,%rdx
  401293:	4c 89 ee             	mov    %r13,%rsi
  401296:	44 89 e7             	mov    %r12d,%edi
  401299:	41 ff 14 df          	callq  *(%r15,%rbx,8)
  40129d:	48 83 c3 01          	add    $0x1,%rbx
  4012a1:	48 39 dd             	cmp    %rbx,%rbp
  4012a4:	75 ea                	jne    401290 <__libc_csu_init+0x40>
  4012a6:	48 83 c4 08          	add    $0x8,%rsp
  4012aa:	5b                   	pop    %rbx
  4012ab:	5d                   	pop    %rbp
  4012ac:	41 5c                	pop    %r12
  4012ae:	41 5d                	pop    %r13
  4012b0:	41 5e                	pop    %r14
  4012b2:	41 5f                	pop    %r15
  4012b4:	c3                   	retq   
  4012b5:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  4012bc:	00 00 00 00 

00000000004012c0 <__libc_csu_fini>:
  4012c0:	f3 0f 1e fa          	endbr64 
  4012c4:	c3                   	retq   

Disassembly of section .fini:

00000000004012c8 <_fini>:
  4012c8:	f3 0f 1e fa          	endbr64 
  4012cc:	48 83 ec 08          	sub    $0x8,%rsp
  4012d0:	48 83 c4 08          	add    $0x8,%rsp
  4012d4:	c3                   	retq   
