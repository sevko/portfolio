/**
 * Conway's Game of Life rendered in a full-screen window with a cell per
 * pixel. See `gameOfLife()`.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <SDL.h>
#include <unistd.h>
#include <time.h>

#include "game_of_life.h"

typedef struct {
	bool curr, next;
} Cell_t;

const int NUM_TICKS = 400,
	TICK_US = 1e6 / 30;

/**
 * Set the color of a pixel to red at specific coordinates in an SDL screen.
 *
 * @param surface The SDL surface to draw to.
 * @param x The x-coordinate of the pixel to plot. No bounds checking is
 *      performed.
 * @param y The y-coordinate of the pixel to plot. No bounds checking is
 *      performed.
 */
inline void drawPixel(SDL_Surface *surface, int x, int y){
	Uint8 *pixelAddress = (Uint8 *) surface->pixels +
		y * surface->pitch +
		x * surface->format->BytesPerPixel;
	*(Uint32 *)pixelAddress = 0xFF0000;
};

/**
 * Simulate Conway's Game of Life with the classic ruleset:
 *
 *   1. A live cell dies when the number of alive neighbors is <= 1 or >= 4.
 *   2. A dead cell comes to life if it has 3 alive neighbors.
 *
 * The simulation will be rendered on a full-screen SDL surface, with a cell
 * per pixel.
 */
int gameOfLife(void){
	if((SDL_Init(SDL_INIT_VIDEO) == -1)){
		fprintf(stderr, "Could not initialize SDL: %s.\n", SDL_GetError());
		return 1;
	}

	const SDL_VideoInfo *videoInfo = SDL_GetVideoInfo();
	int width = videoInfo->current_w,
		height = videoInfo->current_h;
	SDL_Surface *surface = SDL_SetVideoMode(width, height, 32, SDL_FULLSCREEN);

	srand(time(NULL));
	Cell_t cells[height][width];

	// Convenience macro for iterating over all cells in `cells`.
	#define forEachCell(...) \
		do {\
			for(int y = 0; y < height; y++){\
				for(int x = 0; x < width; x++){\
					__VA_ARGS__\
				}\
			}\
		} while(0)

	forEachCell(
		cells[y][x] = (Cell_t){0};
		if(rand() % 15 == 1){
			cells[y][x].curr = true;
		}
	);

	int tick = NUM_TICKS;
	while(tick--){
		for(int y = 1; y < height - 1; y++){
			for(int x = 1; x < width - 1; x++){
				int numAliveNeighbors =
					cells[y][x + 1].curr +
					cells[y][x - 1].curr +
					cells[y + 1][x].curr +
					cells[y + 1][x + 1].curr +
					cells[y + 1][x - 1].curr +
					cells[y - 1][x].curr +
					cells[y - 1][x + 1].curr +
					cells[y - 1][x - 1].curr;

				if(cells[y][x].curr){
					bool shouldDie = numAliveNeighbors >= 4 ||
						numAliveNeighbors <= 1;
					cells[y][x].next = shouldDie ? false : true;
				}
				else {
					cells[y][x].next = (numAliveNeighbors == 3) ? true : false;
				}
			}
		}

		forEachCell(
			cells[y][x].curr = cells[y][x].next;
			if(cells[y][x].curr){
				drawPixel(surface, x, y);
			}
		);

		SDL_Flip(surface);
		SDL_FillRect(surface, NULL, 0x000000);
		// usleep(TICK_US);
	}
	return 0;
}

int main(){
	return gameOfLife();
}
