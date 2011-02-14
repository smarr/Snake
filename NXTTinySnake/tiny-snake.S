// Copyright (C) 2008 - 2011 Stefan Marr <mail@stefan-marr.de>
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// copies of the Software, and to permit persons to whom the Software is
// and/or sell furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.

// This code has been written as part of a coursework assignment together
// with Jan-Arne Sobania in 2007/2008.

	.code	16
	.file	"program.c"
	.section	.text.start_game,"ax",%progbits
	.align	2
	.global	start_game
	.code 16
	.thumb_func
	.type	start_game, %function
start_game:
	push	{r4, r5, r6, r7, lr}
	sub	sp, sp, #16
	mov	r1, #231
	mov	r2, #32
	mov	r5, r0
	str	r1, [sp]
	str	r1, [sp, #4]
	str	r2, [sp, #8]
	b	.L27
.L19:
	// r = Rand()
	bl	Rand
	// r &= 0x1EF
	ldr	r4, .L28		//1x mov wegoptimiert
	and	r4, r4, r0
	mov	r1, r4
	mov	r6, #15
	lsr	r0, r4, #5
	and	r1, r1, r6
	bl	dSetSuperPixel
	ldr	r2, .L28+4
	lsl	r4, r4, #1
	ldrh	r3, [r4, r2]
	cmp	r3, #0			// while(environment[r])
	bne	.L19
	mov	r7, #16
	strh	r7, [r4, r2]
	sub	r5, r5, #1
.L27:
	// while (n_apples > 0) {
	cmp	r5, #0
	bgt	.L19
.L25:
	mov	r4, #0				// i=0 (for)
.L6:
	mov	r5, sp
	bl	dUpdateDisplay
	add	r5, r5, #15
	bl	dIOCtrlTransfer
	mov	r0, #5
	bl	busyMSleep
	add	r4, r4, #1
	mov	r0, r5
	bl	dButtonRead
	cmp	r4, #32				// for(i=0;i<32;i++)
	bne	.L6
	ldrb	r0, [r5]
	ldr	r4, .L28+8
	mov	r5, #241
	lsl	r1, r5, #4
	b	.L8
.L9:
	// Update direction of snake movement
	mov	r6, #15
	mov	r2, r4
	and	r2, r2, r6
	and	r6, r6, r1
	lsl	r3, r6, #5
	orr	r2, r2, r3
	str	r2, [sp, #8]
	lsr	r0, r0, #1
	lsr	r1, r1, #4
	lsr	r4, r4, #4
.L8:
	// while (c >= 1) {
	cmp	r0, #0
	bne	.L9
	// environment[snake_head] = snake_dir;
	mov	r3, sp
	add	r3, r3, #15
	strb	r0, [r3]
	ldr	r1, [sp, #4]
	ldr	r5, [sp, #8]
	lsl	r3, r1, #1
	add	r7, r1, r5
	mov	r2, sp
	ldr	r1, .L28
	ldr	r6, .L28+4
	ldrh	r2, [r2, #8]
	and	r1, r1, r7
	mov	r7, #15
	lsr	r0, r1, #5
	str	r1, [sp, #4]
	strh	r2, [r3, r6]
	and	r1, r1, r7
	bl	dSetSuperPixel
	//n_apples = environment[snake_head];
	ldr	r0, [sp, #4]
	lsl	r4, r0, #1
	ldrh	r0, [r4, r6]
	mov	r1, #239
	tst	r0, r1
	bne	.L14
	// n_apples = n_apples >> 4;
	asr	r5, r0, #4
	// if (n_apples == 0) {
	cmp	r5, #0
	bne	.L27
	ldr	r1, [sp]
	lsl	r2, r1, #1
	ldrh	r4, [r2, r6]
	lsr	r0, r1, #5
	strh	r5, [r2, r6]
	and	r1, r1, r7
	bl	dClrSuperPixel
	ldr	r0, [sp]
	ldr	r6, .L28
	add	r4, r0, r4
	and	r6, r6, r4
	str	r6, [sp]
	b	.L25
.L14:				// return
	add	sp, sp, #16
	@ sp needed for prologue
	pop	{r4, r5, r6, r7}
	pop	{r0}
	bx	r0
.L29:
	.align	2
.L28:
	.word	495
	.word	environment
	.word	61441
	.size	start_game, .-start_game
	.comm	environment,1024,2
	.ident	"GCC: (GNU) 4.1.2"
