#ifdef _WIN32
#include <windows.h>
#include <conio.h>
#endif

#include "d_nxtapi.h"

// The environment array. Make sure this is allocated as zero-initialized data without explicit
// zeros in the object file!
USHORT environment[16*16*2];

void start_game(int n_apples) {
	USHORT snake_tail;
	USHORT snake_head;
	USHORT snake_dir = 0x20;

	// The initial field is empty, besides a single slot occupied by the snake
	// The memset call is not needed here, as the environment array is allocated as zero
	// initialized data
	//memset(environment, 0, sizeof(environment));

	snake_tail = snake_head = 0xe7;

	// It is not necessary to draw the initial snake head here, as it will be done by the regular
	// drawing loop below once the head moves...

    while(1) {
		UBYTE c;

		// Declare this variable as a native-sized one, so regular arithmetic operations do not
		// require explicit clamping instructions
		int i;

		// Generate required number of apples.
		while (n_apples > 0) {
			// Generate random position, draw apple, and repeat until we find a slot not occupied
			// by either another apple or the snake itself.
			int r;

			n_apples--;
			do {
				r = Rand() & 0x1EF;
				dSetSuperPixel(r >> 5, r & 15);
			} while(environment[r]);

			// ASSERT(environment[r] == 0), so we can just set this value to 1
			environment[r] = 0x10;
		}

#ifndef _WIN32
        for(i=0;i<32;i++)
        {
#endif
            // update screen and read buttons
            dUpdateDisplay(); 
            // must be called every 10 ms to receive button state from AVR
            dIOCtrlTransfer();
#ifndef _WIN32
            busyMSleep(5);
#else
			busyMSleep(300);
#endif
            // read key
            dButtonRead(&c);
#ifndef _WIN32
        }
#endif

		// Update direction of snake movement
		{
			unsigned int x = 0xF1 << 4, y = 0xF001;

			while (c >= 1) {
				snake_dir = (y & 0xF) | ((x & 0xF) << 5);
				c >>= 1;
				x >>= 4;
				y >>= 4;
			}
		}

		// Mark the old snake head as having moved in the specified direction
		environment[snake_head] = snake_dir;

        // Advance snake, draw new head
		snake_head = (snake_head + snake_dir) & 0x1EF;
		dSetSuperPixel(snake_head >> 5, snake_head & 15);

		// Check for any objects the snake head may have hit. It is sufficient to check for a 
		// bitmask of 0xEF in the environment here, the highest-order bit of the movement vector
		// (0x100) can only be set if the movement is negative in that direction, so at least one
		// of the lower-order bits will be set, too.
		n_apples = environment[snake_head];
		if (n_apples & 0xEF) {
			// Snake collided with itself.
			return;
		}

		// Check for an apple occupying this location. If there is *no* apple, we have to advance
		// the snake's tail one position; otherwise, it stays just where it is, because the snake
		// has just grown by one segment...
		n_apples = n_apples >> 4;
		if (n_apples == 0) {
			// Remember movement vector of snake at this time
			USHORT tail_move = environment[snake_tail];

			// Clear snake's tail from the environment and the screen
			environment[snake_tail] = 0;
			dClrSuperPixel(snake_tail >> 5, snake_tail & 15);

			// Calculate new position of snake's tail.
			snake_tail = (snake_tail + tail_move) & 0x1EF;
		}
    }
}
