#include <stdio.h>
#include <inttypes.h>

uint32_t puzzle3(uint32_t n, uint32_t d);

int main() {
    printf("%d\n", puzzle3(12,3));
	printf("%d\n", puzzle3(8,4));
	printf("%d\n", puzzle3(21,37));
	printf("%d\n", puzzle3(4,2));
	printf("%d\n", puzzle3(0,0));
	printf("%d\n", puzzle3(11,9));
	printf("%d\n", puzzle3(6,9));
    return 0;
}