#include "Runtime.h"

using namespace Lang;

static void Repl() {
	char line[1024];
	while (true) {
		printf("> ");

		if (!fgets(line, sizeof(line), stdin)) {
			printf("\n");
			break;
		}

		Runtime::Interpret(line);
	}
}

static char* ReadFile(char* path) {
	FILE* file = fopen(path, "rb");
	if (file == NULL) {
		fprintf(stderr, "Could not open file `%s'\n", path);
		exit(74);
	}

	fseek(file, 0L, SEEK_END);
	size_t size = ftell(file);
	rewind(file);

	char* buffer = (char*)malloc(size + 1);
	if (buffer == NULL) {
		fprintf(stderr, "Not enough memory to read `%s'\n", path);
		exit(74);
	}

	size_t bytesRead = fread(buffer, sizeof(char), size, file);
	if (bytesRead < size) {
		fprintf(stderr, "Could not read file `%s'\n", path);
		exit(74);
	}

	buffer[bytesRead] = '\0';
	fclose(file);
	return buffer;
}

static void RunFile(char* path) {
	char* source = ReadFile(path);
	auto result = Runtime::Interpret(source);
	exit((int)result);
}

int main(int argc, char* argv[]) {
	if (argc == 1) {
		Repl();
	} else if (argc == 2) {
		RunFile(argv[1]);
	} else {
		fprintf(stderr, "Usage: %s [path]\n", argv[0]);
		exit(64);
	}

	return 0;
}
