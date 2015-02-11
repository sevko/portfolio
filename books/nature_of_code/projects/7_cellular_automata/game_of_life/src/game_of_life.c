#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <SDL.h>
#include <unistd.h>
#include <time.h>

#include "game_of_life.h"

typedef struct {
	bool prev, curr;
} Cell_t;

const int NUM_TICKS = 30,
	TICK_US = 1e6 / 30;

inline void drawPixel(SDL_Surface *surface, int x, int y){
	Uint8 *pixelAddress = (Uint8 *) surface->pixels +
		y * surface->pitch +
		x * surface->format->BytesPerPixel;
	*(Uint32 *)pixelAddress = 0xFF0000;
};

int gameOfLife(void){
	if((SDL_Init(SDL_INIT_VIDEO | SDL_INIT_AUDIO) == -1)){
		fprintf(stderr, "Could not initialize SDL: %s.\n", SDL_GetError());
		return 1;
	}

	const SDL_VideoInfo *videoInfo = SDL_GetVideoInfo();
	int width = videoInfo->current_w,
		height = videoInfo->current_h;
	SDL_Surface *surface = SDL_SetVideoMode(width, height, 32, SDL_FULLSCREEN);

	srand(time(NULL));
	Cell_t cells[height][width];
	for(int y = 0; y < height; y++){
		for(int x = 0; x < width; x++){
			cells[y][x] = (Cell_t){0};
			if(rand() % 2 == 1){
				cells[y][x].curr = true;
			}
		}
	}

	int tick = NUM_TICKS;
	while(tick--){
		for(int y = 0; y < height; y++){
			for(int x = 0; x < width; x++){
				if(cells[y][x].curr){
					drawPixel(surface, x, y);
				}
			}
		}
		SDL_Flip(surface);
		usleep(TICK_US);
	}
	return 0;
}

int main(){
	return gameOfLife();
}
