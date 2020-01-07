#include <iostream>
#include <vector>
#include <assert.h>
#include <algorithm>
#include <chrono>
#include <SDL2/SDL.h>

using namespace std;
using namespace std::chrono;

typedef struct {
	uint8_t b;
	uint8_t g;
	uint8_t r;
	uint8_t a;
} Pixel_t;

uint32_t pixelDist(Pixel_t *pix1, Pixel_t *pix2){
	return
		pow(max(pix1->r, pix2->r) - min(pix1->r, pix2->r), 2) +
		pow(max(pix1->g, pix2->g) - min(pix1->g, pix2->g), 2) +
		pow(max(pix1->b, pix2->b) - min(pix1->b, pix2->b), 2);
}

uint32_t pixelDistGuarded(vector<vector<Pixel_t>>& rows, Pixel_t *pix, int x, int y, int width, int height){
	return (0 <= x && x < width && 0 <= y && y < height) ?
		pixelDist(pix, &rows[y][x]) :
		0;
}

template <class T>
const T& min3(T& a, T& b, T& c){
	return min(min(a, b), c);
}

using SeamPoint = pair<int, int>;
using Seam = vector<SeamPoint>;

uint32_t getEnergyOfPoint(vector<vector<Pixel_t>>& rows, int width, int height, int x, int y){
	uint32_t energy = 0;
	Pixel_t *pix = &rows[y][x];
	energy += pixelDistGuarded(rows, pix, x - 1, y - 1, width, height);
	energy += pixelDistGuarded(rows, pix, x    , y - 1, width, height);
	energy += pixelDistGuarded(rows, pix, x + 1, y - 1, width, height);
	energy += pixelDistGuarded(rows, pix, x - 1, y    , width, height);
	energy += pixelDistGuarded(rows, pix, x + 1, y    , width, height);
	energy += pixelDistGuarded(rows, pix, x - 1, y + 1, width, height);
	energy += pixelDistGuarded(rows, pix, x    , y + 1, width, height);
	energy += pixelDistGuarded(rows, pix, x + 1, y + 1, width, height);
	return energy;
}

vector<vector<uint32_t>> computeEnergies(vector<vector<Pixel_t>>& rows, int width, int height){
	vector<vector<uint32_t>> energies(height);
	for(int y = 0; y < height; y++){
		energies[y].resize(width);
		fill(energies[y].begin(), energies[y].end(), 0);
	}

	for(int y = 0; y < height; y++){
		for(int x = 0; x < width; x++){
			energies[y][x] = getEnergyOfPoint(rows, width, height, x, y);
		}
	}

	return energies;
}

void updateEnergiesAroundSeam(
	vector<vector<Pixel_t>>& rows,
	vector<vector<uint32_t>>& energies,
	Seam& seam, int width, int height){
	const vector<pair<int, int>> dirs = {
		{-1, -1}, {-1, 0}, {-1, 1},
		{0, -1}, {0, 0}, {0, 1},
		{1, -1}, {1, 0}, {1, 1}
	};

	for(auto pt: seam){
		int y = pt.first,
			x = pt.second;
		assert(energies[y].size() == (size_t)width);

		for(auto dir: dirs){
			int newY = y + dir.first,
				newX = x + dir.second;
			if(0 <= newY && newY < height && 0 <= newX && newX < width){
				energies[newY][newX] = getEnergyOfPoint(rows, width, height, newX, newY);
			}
		}
	}
}

void computeSeams(vector<vector<uint32_t>>& energies, vector<vector<uint32_t>>& seamEnergies, int width, int height){
	copy(energies[0].begin(), energies[0].end(), seamEnergies[0].begin());

	for(int y = 1; y < height; y++){
		for(int x = 1; x < width - 1; x++){
			seamEnergies[y][x] = energies[y][x] + min3(
				seamEnergies[y - 1][x - 1],
				seamEnergies[y - 1][x    ],
				seamEnergies[y - 1][x + 1]);
		}

		int x = 0;
		seamEnergies[y][x] = energies[y][x] + min(
			seamEnergies[y - 1][x    ],
			seamEnergies[y - 1][x + 1]);

		x = width - 1;
		seamEnergies[y][x] = energies[y][x] + min(
			seamEnergies[y - 1][x - 1],
			seamEnergies[y - 1][x    ]);
	}
}

Seam findLowestEnergySeam(vector<vector<uint32_t>>& seamEnergies, int width, int height){
	auto minSeamEnd = min_element(
		seamEnergies[height - 1].begin() + 3,
		seamEnergies[height - 1].end() - 3);
	int minSeamEndInd = distance(seamEnergies[height - 1].begin(), minSeamEnd);
	int minSeamY = height - 1,
		minSeamX = minSeamEndInd;

	Seam seam;
	seam.reserve(height);
	seam.push_back(SeamPoint(minSeamY, minSeamX));

	while(minSeamY > 0){
		if(minSeamX == 0){
			if(seamEnergies[minSeamY - 1][minSeamX] > seamEnergies[minSeamY - 1][minSeamX + 1]){
				minSeamX++;
			}
		}
		else if(minSeamX == width - 1){
			if(seamEnergies[minSeamY - 1][minSeamX] > seamEnergies[minSeamY - 1][minSeamX - 1]){
				minSeamX--;
			}
		}
		else {
			uint32_t energyUp = seamEnergies[minSeamY - 1][minSeamX],
				energyUpLeft = seamEnergies[minSeamY - 1][minSeamX - 1],
				energyUpRight = seamEnergies[minSeamY - 1][minSeamX + 1];

			if(energyUpLeft <= energyUp && energyUpLeft <= energyUpRight){
				minSeamX--;
			}
			else if(energyUpRight <= energyUp && energyUpRight <= energyUpLeft){
				minSeamX++;
			}
		}
		minSeamY--;
		seam.push_back(SeamPoint(minSeamY, minSeamX));
	}

	return seam;
}

void removeSeam(vector<vector<Pixel_t>>& rows, vector<vector<uint32_t>>& energies, vector<vector<uint32_t>>& seamEnergies, Seam& seam){
	assert(seam.size() == rows.size());
	for(SeamPoint pt: seam){
		int y = pt.first,
			x = pt.second;
		rows[y].erase(rows[y].begin() + x);
		energies[y].erase(energies[y].begin() + x);
		seamEnergies[y].erase(seamEnergies[y].begin() + x);
	}
}

int main(int argc, char *argv[]){
	bool showSeams = false;
	if(argc >= 2){
		if(strcmp(argv[1], "-s") == 0){
			showSeams = true;
		}
		else {
			cout << "Error: only -s flag supported. (shows seams)" << endl;
		}
	}

	SDL_Init(SDL_INIT_VIDEO);
	SDL_Surface* bmpSurface = SDL_LoadBMP("out.bmp");

	SDL_Window *window = SDL_CreateWindow(
		"seam carving", SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, bmpSurface->w, bmpSurface->h, SDL_WINDOW_RESIZABLE);
	SDL_SetWindowMaximumSize(window, bmpSurface->w, bmpSurface->h);
	SDL_Surface* screenSurface = SDL_GetWindowSurface( window );

	int width = bmpSurface->w,
		height = bmpSurface->h;

	SDL_BlitSurface(bmpSurface, NULL, screenSurface, NULL);

	SDL_UpdateWindowSurface(window);

	vector<vector<Pixel_t>> rows(height);
	for(int y = 0; y < height; y++){
		rows[y].reserve(width);
		copy(
			&((Pixel_t *)screenSurface->pixels)[y * width],
			&((Pixel_t *)screenSurface->pixels)[(y + 1) * width],
			back_inserter(rows[y]));
	}

	bool quit = false;
	SDL_Event evt;

	int currWidth = width, currHeight = height;

	vector<vector<uint32_t>> energies = computeEnergies(rows, currWidth, currHeight);
	vector<vector<uint32_t>> seamEnergies(height);
	for(int y = 0; y < height; y++){
		seamEnergies[y].resize(width);
		fill(seamEnergies[y].begin(), seamEnergies[y].end(), 0);
	}

	while(!quit){
		while(SDL_PollEvent(&evt) != 0){
			if(evt.type == SDL_QUIT){
				quit = true;
			}
			else if(evt.type == SDL_KEYDOWN){
				if(evt.key.keysym.sym == SDLK_RETURN){
					computeSeams(energies, seamEnergies, currWidth, currHeight);
					Seam seam = findLowestEnergySeam(seamEnergies, currWidth, currHeight);
					screenSurface = SDL_GetWindowSurface(window);

					if(showSeams){
						SDL_LockSurface(screenSurface);
						for(SeamPoint pt: seam){
							int y = pt.first,
								x = pt.second;
							int ind = y * width + x;
							((Pixel_t*)screenSurface->pixels)[ind] = (Pixel_t){
								.b = 0,
								.g = 0,
								.r = 255,
								.a = 0
							};
						}
						SDL_UnlockSurface(screenSurface);
						SDL_UpdateWindowSurface(window);
						SDL_Delay(10);
					}
					SDL_LockSurface(screenSurface);

					removeSeam(rows, energies, seamEnergies, seam);
					currWidth -= 1;
					updateEnergiesAroundSeam(rows, energies, seam, currWidth, currHeight);

					for(int y = 0; y < height; y++){
						Pixel_t *pixels = (Pixel_t *)screenSurface->pixels;
						assert(rows[y].size() == (size_t)currWidth);
						copy(rows[y].begin(), rows[y].end(), pixels + y * width);
						for(int ind = currWidth; ind < width; ind++){
							pixels[y * width + ind] = (Pixel_t){
								.b = 0, .g = 0, .r = 0, .a = 0
							};
						}
					}

					SDL_UnlockSurface(screenSurface);
				}
			}
			continue;
		}

		SDL_UpdateWindowSurface(window);
	}

	SDL_DestroyWindow( window );
	SDL_Quit();
	return EXIT_SUCCESS;
}
